\section{keylo.lhs}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import Data.Word
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
	optimized <- genLayout opts klsc
	let
		e2 = fromIntegral $ energy optimized
		chart = klscCoolingChart optimized
	putStrLn . ("Top N words considered: " ++) . show . length $ lstW
	putStrLn . ("Total unique bigrams detected: "++) . show . length $ M.toList $ hashB
	putStrLn . ("Unique bigrams to be used for evaluation: "++) . show . length $ M.toList $ hBW
	ruler
	putStrLn "optimized"
	ruler
	putStrLn $ show optimized
	TP.printf "Energy loss: %.2f%%" (energyLossPerc e1 e2)
	when (length chart /= 0) $ do
		let
			plotPoints = T.pack . unlines $ map showTupleAsColumns chart
		T.writeFile (fromJust cooling_chart) plotPoints
	where
	ruler = putStrLn $ replicate 80 '-'
\end{code}

\begin{code}
genLayout :: Opts -> KLSearchCtx -> IO KLSearchCtx
genLayout o@Opts{..} k = do
	rng <- case rng_seed of
		Just (s1, s2) -> initialize s1 s2
		Nothing -> createSystemRandom
	genLayout' o rng k

genLayout' :: Opts -> GenIO -> KLSearchCtx -> IO KLSearchCtx
genLayout' Opts{..} rng klsc0 = do
	(klsc1@KLSearchCtx{..}, dataPoints) <- case algorithm of
		ARandom -> randSearch cooling_chart klsc0 time rng
		ASimAnneal -> anneal cooling_chart klsc0 time rng
	return $ klsc1
		{ klscKLayout = syncKLayout klscKLayout
		, klscCoolingChart = reverse dataPoints
		}

genLayouts :: Opts -> KLSearchCtx -> Int -> IO [KLSearchCtx]
genLayouts o@Opts{..} k n = do
	rngs <- case rng_seed of
		Just sd -> mkRngs sd
		Nothing -> do
			rng <- createSystemRandom
			s1 <- uniform rng :: IO Word64
			mkRngs (s1, s1 + 1)
	mapM (\rng -> genLayout' o rng k) rngs
	where
	mkRngs (s1, s2) = mapM (initialize s1) [s2..(s2 + fromIntegral n)]
\end{code}
