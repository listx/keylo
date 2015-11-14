\section{keylo.lhs}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment
import System.Exit
import System.IO

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
keylo Opts{..} = do
	fileList <- T.readFile corpus
	src <- liftM T.concat . mapM (T.readFile . T.unpack) $ T.lines fileList
	let
		klsc = KLSearchCtx
			{ klscConstraints = constraintDefault
			, klscCorpus = src
			, klscFreqL = (hashL, findMaxVal hashL)
			, klscFreqB = (hashB, findMaxVal hashB)
			, klscFreqW = (hashW, findMaxVal hashW)
			, klscKLayout = nisse
			}
		hashL = freqL src
		hashB = freqB hashW
		hashW = freqW src
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
	optimized <- genLayout time klsc
	putStrLn "optimized"
	ruler
	putStrLn $ show optimized
	where
	ruler = putStrLn $ replicate 80 '-'
\end{code}
