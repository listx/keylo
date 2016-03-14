\section{KEYLO/Option.lhs}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module KEYLO.Option where

import Data.Maybe
import Data.Word
import Safe
import System.Console.CmdArgs.Implicit
import System.Directory

import KEYLO.Generate
import KEYLO.Meta
import KEYLO.Util

data Opts = Opts
	{ algorithm :: Algorithm
	, blacklist :: FilePath
	, chart_dir :: Maybe FilePath
	, corpus :: FilePath
	, rng_seed :: Maybe (Word64, Word64)
	, rounds :: Int
	, verbose :: Bool
	, threads :: Int
	, time :: Int
	} deriving (Data, Typeable, Show, Eq)

optsDefault :: Opts
optsDefault = Opts
	{ algorithm = ASimAnneal &= help
		""
	, blacklist = "" &= typFile &= help
		"location of the file containing words not considered as part of the corpus"
	, chart_dir = Nothing &= typDir &= help
		"for debugging purposes; print out data points of time vs energy (cooling rate), for every N mutations into the given directory"
	, corpus = "" &= typFile &= help
		"localtion of the file that lists other files of raw text, one on each line"
	, rng_seed = Nothing &= typ "NUM64,NUM64" &= help
		"seed of a generator; we use the PCG RNG which uses two 64-bit unsigned numbers as a seed"
	, rounds = 1 &= help
		"number of times to run the simulation; default 1"
	, verbose = True &= help
		"increase verbosity"
	, threads = 1 &= help
		"number of threads to use"
	, time = 1000 &= help
		"length of iterations to run the annealing process; the longer it is the more accurate; default 1000"
	}
\end{code}

The options are defined by the \ct{Opts} data type, and we write our version of it with \ct{optsDefault}.
We add some more customizations to how \ct{keylo} will behave (e.g., \ct{-h} and \ct{-v} flags) with \ct{getOpts}.
This is standard practice for writing command line options with the \ct{CmdArgs} library.

\begin{code}
getOpts :: IO Opts
getOpts = cmdArgs $ optsDefault
	&= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
	&= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
	&= help "search for an optimal keyboard layout using simulated annealing"
	&= helpArg [explicit, name "help", name "h"]
	&= program _PROGRAM_NAME
\end{code}

Check for errors, and return an error code (a positive number) if any errors are found.

In \ct{argsCheck'}, we have to pass in \ct{fromMaybe False ...} because otherwise that branch will always execute.

\begin{code}
argsCheck :: Opts -> IO (Opts, Int)
argsCheck opts = do
	corpus_file_not_exists <- fmap not $ doesFileExist (corpus opts)
	blacklist_file_not_exists <- fmap not $ doesFileExist (blacklist opts)
	chart_dir_exists <- if isJust (chart_dir opts)
		then fmap not $ doesDirectoryExist (fromJustNote "argsCheck" $ chart_dir opts)
		else return False
	let
		ioChecksHash =
			[ ("corpus_file_not_exists", corpus_file_not_exists)
			, ("blacklist_file_not_exists", blacklist_file_not_exists)
			, ("chart_dir_exists", chart_dir_exists)
			]
	return . (,) opts =<< argsCheck' opts ioChecksHash
	where
	argsCheck' :: Opts -> [(String, Bool)] -> IO (Int)
	argsCheck' Opts{..} ich
		| fromMaybe True (lookup "corpus_file_not_exists" ich) = do
			errMsg $ "file" ++ enclose' sQuotes corpus ++ " does not exist"
			return 1
		| fromMaybe False (lookup "blacklist_file_not_exists" ich) = do
			errMsg $ "file" ++ enclose' sQuotes blacklist ++ " does not exist"
			return 1
		| fromMaybe False (lookup "chart_dir_exists" ich) = do
			errMsg $ "directory" ++ enclose' sQuotes chart_dir' ++ " does not exist"
			return 1
		| threads < 1 = do
			errMsg "--threads cannot be less than 1"
			return 2
		| time < 1 = do
			errMsg "--time cannot be less than 1"
			return 2
		| otherwise = return 0
		where
		chart_dir' = fromJustNote "argsCheck'" chart_dir
\end{code}
