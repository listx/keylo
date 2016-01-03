\section{KEYLO/Frequency.lhs}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KEYLO.Frequency where

import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Text.Lazy as T
import Data.Word hiding (Word)
import Prelude hiding (Word)
import qualified Text.Printf as TP

type FreqMax = Word64
type Bigram = T.Text
type Word = T.Text
type HashL = M.Map Char Word64
type HashB = M.Map Bigram Word64
type HashW = M.Map Word Word64
\end{code}

\ct{freqL} finds letter frequency.
\ct{isAlpha} evaluates to true for any alphabetic \textbf{Unicode} character.
For every alphabetic or punctuation key,

\begin{code}
freqL :: T.Text -> HashL
freqL = T.foldl step M.empty
	where
	step hashL c
		| isAlpha c = M.insertWith (+) (toLower c) 1 hashL
		| elem c puncKeysAll = case lookup c puncTuples of
			Just pkey -> M.insertWith (+) pkey 1 hashL
			Nothing -> hashL
		| otherwise = hashL
	puncTuples = concatMap (\keyPair -> [(head keyPair, head keyPair), (last keyPair, head keyPair)]) puncKeys

puncKeysAll :: String
puncKeysAll = concat puncKeys

puncKeys :: [String]
puncKeys =
	[ "`~"
	, "-_"
	, "=+"
	, "[{"
	, "}]"
	, "\\|"
	, ";:"
	, "'\""
	, ",<"
	, ".>"
	, "/?"
	]
\end{code}

\ct{freqB} calculates bigram frequency.
A bigram is a 2-letter sequence found anywhere within a word.
E.g., the word ``food'' has 3 bigrams --- ``fo'', ``oo'', and ``od''.
\ct{freqB} depends on the word frequency hash, \ct{HashW}, to provide the corpus of words.

\begin{code}
freqB :: HashW -> HashB
freqB = M.foldlWithKey step M.empty
	where
	step hashB w n = case T.length w of
		0 -> error "OOPS! HashW included a zero-length word"
		1 -> hashB
		2 -> M.insertWith (+) w n hashB
		_ -> foldl (insertBigrams n) hashB $ bigrams w
	insertBigrams n hashB' bigram = M.insertWith (+) bigram n hashB'

bigrams :: T.Text -> [T.Text]
bigrams word = map (\(a, b) -> T.cons a $ T.cons b T.empty)
	. T.zip word
	$ T.drop 1 word
\end{code}

\ct{freqW} counts word frequency. A ``word'' is processed as follows:
\begin{enumerate}
	\item{Lowercase the word.}
	\item{Any character not recognized as being part of a word is removed.}
	\item{The remaining ``holes'' are removed, and the word is compacted.}
\end{enumerate}
The first step is necessary because we do not want to store multiple versions, capitalized and uncapitalized, of the same word.
It must come first before the other steps because sometimes lowercasing a word results in a longer string, according to the documentation for \ct{Data.Text.Lazy.toLower}.
The second and third steps combine into one step with \ct{Data.Text.Lazy.filter}.

\begin{code}
freqW :: T.Text -> HashW
freqW = foldl step M.empty . T.words
	where
	step hashW w
		| T.length w' == 0 = hashW
		| otherwise = M.insertWith (+) w' 1 hashW
		where
		w' = T.filter isWordLetter $ T.toLower w

isWordLetter :: Char -> Bool
isWordLetter c = or
	[ isAlpha c
	, elem c puncKeysAll
	]
\end{code}

\ct{dispFreq} takes any hash with \ct{Show}-able keys and integer values, and prints out each key's percentage.

\begin{code}
dispFreq :: (Show k, Show a, Integral a) => Int -> M.Map k a -> IO ()
dispFreq limit hash = mapM_ f
	. take limit
	. reverse
	. sortBy (comparing snd)
	$ M.toList hash
	where
	total = sum $ M.elems hash
	f (k, n) = putStrLn $ concat [m1, m2, m3]
		where
		perc :: Double
		perc
			| total == 0 = 0
			| otherwise = (fromIntegral n) / (fromIntegral total) * 100
		m1 = show k ++ " = "
		m2 = TP.printf "%.2f%%" perc
		m3 = " (" ++ show n ++ " occurrences)"
\end{code}

\ct{findMaxVal} finds the maximum value of a hash of numeric values.

\begin{code}
findMaxVal :: (Ord b, Num b) => M.Map a b -> b
findMaxVal = M.foldl' step 0
	where
	step acc v = if v > acc
		then v
		else acc
\end{code}
