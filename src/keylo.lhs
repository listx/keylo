\section{keylo.lhs}

\begin{code}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import Data.Word
import Safe
import System.Environment
import System.Exit
import System.IO
import System.Random.PCG
import qualified Text.Printf as TP

import KEYLO.Frequency
import KEYLO.Generate
import KEYLO.Layout
import KEYLO.Option
import KEYLO.Util

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	args' <- getArgs
	opts <- (if null args' then withArgs ["--help"] else id) $ getOpts
	(opts', argsErrNo) <- argsCheck opts
	when (argsErrNo > 0) $ do
		errMsg $ "code " ++ show argsErrNo
		exitWith $ ExitFailure argsErrNo
	keylo opts'

keylo :: Opts -> IO ()
keylo opts@Opts{..} = do
	fileList <- T.readFile corpus
	blacklistWords <- T.readFile blacklist
	src <- liftM T.concat . mapM (T.readFile . T.unpack) $ T.lines fileList
	presentLayouts opts blacklistWords src

presentLayouts :: Opts -> T.Text -> T.Text -> IO ()
presentLayouts opts@Opts{..} blacklistWords src = do
	let
		klsc = KLSearchCtx
			{ klscMPenalties = mPenaltiesDefault
			, klscCorpus = src
			, klscFreqL = (hashL, findMaxVal hashL)
			, klscFreqBW = hBW
			, klscFreqW = (thw, maxW)
			, klscFreqWTrunc = lstW
			, klscFreqLW = hLW
			, klscKLayout = nisse
			, klscKeyPlacementPenalty
				= updateKpp (hashL, findMaxVal hashL) initialKpp nisse [0..(V.length initialKpp - 1)]
			, klscCoolingChart = []
			, klscTid = 0 -- 0 is default thread ID
			, klscAlgo = algorithm
			}
		maxW = findMaxVal hashW
		thw = truncateHash hashW 3000
		lstW = truncateHashTop thw 60
		hLW = charsWeighted (lstW, maxW)
		hBW = bigramsWeighted (lstW, maxW)
		hashL = freqL src
		hashB = normalizeFreqB $ freqB hashW
		hashW = freqW
			(M.fromList . zip (concatMap T.words $ T.lines blacklistWords) $ repeat True)
			src
		keyNames = V.toList $ klLayout nisse
		keyAtoms = map (\ka -> (ka, penaltyAtom ka)) . V.toList $ klKeyboard nisse
		ks = zip keyNames keyAtoms
		initialKpp = flip V.replicate 0 (klSizeVisible nisse)
		e1 = fromIntegral $ energy klsc
	when verbose $ do
		dispFreq 100 hashL
		ruler
		dispFreq 100 hashB
		ruler
		dispFreq 100 hashW
		ruler
		putStrLn . klName $ klscKLayout klsc
		ruler
		putStrLn $ show klsc
		ruler
		mapM_ (putStrLn . show) ks
		ruler
		putStrLn . ("Top N words considered: " ++) . show . length $ lstW
		putStrLn . ("Total unique bigrams detected: "++) . show . length $ M.toList $ hashB
		putStrLn . ("Unique bigrams to be used for evaluation: "++) . show . length $ M.toList $ hBW
		ruler
	putStrLn "optimized layout(s)"
	ruler
	ls <- genLayouts opts klsc
	mapM_ (presentLayout e1) $ histogramize ls
	writeCharts opts ls

ruler :: IO ()
ruler = putStrLn $ replicate 80 '-'
\end{code}

\ct{histogramize} works as follows: sort by equality of klsc, then group by equality, then for each group: count how many there are in the group, then lastly sort again by the count.

\begin{code}
histogramize :: [KLSearchCtx] -> [(Int, KLSearchCtx)]
histogramize
	= sortBy (\(n, _) (m, _) -> compare n m)
	. map (\kgroup -> (length kgroup, headNote "histogramize" kgroup))
	. groupBy klscEq
	. sortBy klscOrd
	where
	klscOrd a b = compare (show a) (show b)
	klscEq a b = show a == show b

presentLayout :: Energy -> (Int, KLSearchCtx) -> IO ()
presentLayout e1 (cnt, k@KLSearchCtx{..}) = do
	let
		e2 = fromIntegral $ energy k
	when (klscTid == 0) $ do
		ruler
		putStrLn "Original Layout!"
	putStrLn $ "COUNT: " ++ show cnt
	putStrLn $ show k
	TP.printf "Energy loss: %.2f%%" (energyLossPerc (fromIntegral e1) e2)
	-- Original (unshuffled) layout.
	when (klscTid == 0) $ do
		putStrLn "Original Layout!"
	ruler

writeCharts :: Opts -> [KLSearchCtx] -> IO ()
writeCharts o ks = mapM_ (writeChart o) ks

writeChart :: Opts -> KLSearchCtx -> IO ()
writeChart Opts{..} KLSearchCtx{..}
	| length klscCoolingChart == 0 = return ()
	| otherwise = do
		let
			plotPoints = T.pack . unlines $ map showTupleAsColumns klscCoolingChart
		T.writeFile (fromJust chart_dir ++ "/" ++ chartFileName) plotPoints
	where
	chartFileName = let
		basename = case klscAlgo of
			ARandom -> "random"
			ASimAnneal -> "simann"
		rest = "_" ++ (show klscTid) ++ ".txt"
		in
		basename ++ rest
\end{code}

\begin{code}
genLayout' :: Opts -> GenIO -> KLSearchCtx -> IO KLSearchCtx
genLayout' Opts{..} rng klsc0 = do
	(klsc1@KLSearchCtx{..}, dataPoints) <- case algorithm of
		ARandom -> randSearch chart_dir klsc0 time rng
		ASimAnneal -> anneal chart_dir klsc0 time rng
	return $ klsc1
		{ klscKLayout = syncKLayout klscKLayout
		, klscCoolingChart = reverse dataPoints
		}

genLayouts :: Opts -> KLSearchCtx -> IO [KLSearchCtx]
genLayouts o@Opts{..} k
	| rounds < 2 = do
		rngs <- mkRngs
		let
			rng = headNote "genLayouts: rngs" rngs
		l <- genLayout' o rng k
		return [l]
	| otherwise = do
		rngs <- mkRngs
		let
			rng = headNote "genLayouts: get head of rngs" rngs
			rngsRest = tailNote "genLayouts: get tail of rngs" rngs
		layoutOrig <- genLayout' o rng k
		layoutsRest <- mapM (genLayoutScramble o k) rngsRest
		return
			$ layoutOrig
			: (map (\(l, n) -> l {klscTid = n}) $ zip layoutsRest [1..])
	where
	mkRngs = case rng_seed of
		Just sd -> mkRngs' sd
		Nothing -> do
			rng <- createSystemRandom
			s1 <- uniform rng :: IO Word64
			mkRngs' (s1, s1 + 1)
	mkRngs' (s1, s2) = mapM (initialize s1) [s2..(s2 + fromIntegral rounds)]

genLayoutScramble :: Opts -> KLSearchCtx -> GenIO -> IO KLSearchCtx
genLayoutScramble o k@KLSearchCtx{..} rng = do
	vis1 <- shuffle vis0 rng
	genLayout' o rng $ k
		{ klscKLayout = kl
			{ klLayout = (V.++) vis1 invis
			}
		}
	where
	kl@KLayout{..} = klscKLayout
	(vis0, invis) = V.splitAt klSizeVisible klLayout
\end{code}
