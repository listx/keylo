\section{keylo.lhs}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import KEYLO.Frequency

main :: IO ()
main = do
	fileList <- T.getContents
	src <- liftM T.concat . mapM (T.readFile . T.unpack) $ T.lines fileList
	let
		hashL = freqL src
		hashB = freqB hashW
		hashW = freqW src
	dispFreq 100 hashL
	putStrLn $ replicate 80 '-'
	dispFreq 100 hashB
	putStrLn $ replicate 80 '-'
	dispFreq 100 hashW
\end{code}
