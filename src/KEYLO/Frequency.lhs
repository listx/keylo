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
type HashLW = M.Map Char Double
type HashBW = M.Map Bigram Double
type WordBlacklist = M.Map T.Text Bool
\end{code}

\ct{freqL} finds letter frequency.
\ct{isAlpha} evaluates to true for any alphabetic \textbf{Unicode} character.
For every alphabetic or punctuation key,

\label{letterFreq}
\subsection{Letter Frequency}

\begin{code}
freqL :: T.Text -> HashL
freqL = T.foldl step M.empty
	where
	step hashL c
		| isAlphabet c = M.insertWith (+) (toLower c) 1 hashL
		| otherwise = hashL
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

We explicitly exclude words that are present in the blacklist, or are simply repeitions of a single letter (for words of length 2 or greater).

\begin{code}
freqW :: WordBlacklist -> T.Text -> HashW
freqW blist = foldl step M.empty . T.words
	where
	step hashW w
		| T.length w' == 0 = hashW
		| T.length w' > 1 && sameLetters (T.unpack w') = hashW
		| otherwise = if M.member w blist
			then hashW
			else M.insertWith (+) w' 1 hashW
		where
		w' = T.filter isAlphabet $ T.toLower w
\end{code}

\ct{isAlphabet} checks if the given char is in the basic, 26-letter English alphabet from ``a'' to ``z'', lower and uppercase.
For speed, instead of using \ct{elem} over a list of \ct{Char}s (\ct{String} type), it does arithmetic comparisons for a subset of the traditional ASCII character set range (0 to 127).

\begin{code}
isAlphabet :: Char -> Bool
isAlphabet c
	| 65 <= x && x <= 90 = True
	| 97 <= x && x <= 122 = True
	| otherwise = False
	where
	x = ord c

sameLetters :: String -> Bool
sameLetters xs = all (== head xs) xs
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

\ct{truncateHash} truncates the given hash structure to contain only \ct{N} keys.
It does this by selecting the top \ct{N} keys with the highest \ct{Int} values.

\begin{code}
truncateHash :: (Ord k, Ord a) => M.Map k a -> Int -> M.Map k a
truncateHash h = M.fromList . truncateHashAsList h

truncateHashAsList :: (Ord k, Ord a) => M.Map k a -> Int -> [(k, a)]
truncateHashAsList h n
	= take n
	. reverse
	. sortBy (comparing snd)
	$ M.toList h
\end{code}

\ct{truncateHashTop} takes the top \textit{p} percent of the data by sorting (biggest freq first), then taking until the subtotal's percentage matches or exceeds that of \textit{p}.

\begin{code}
truncateHashTop
	:: (Ord k, Ord a, Integral a)
	=> M.Map k a
    -> Double
    -> [(k, a)]
truncateHashTop h p = fst $ foldl' step ([], 0) lst
	where
	lst
		= reverse
		. sortBy (comparing snd)
		$ M.toList h
	total = sum $ map snd lst
	step acc@(xs, subtotal) kv@(_, a)
		| (fromIntegral subtotal / fromIntegral total) >= p = acc
		| otherwise = (kv : xs, subtotal + a)
\end{code}

\ct{HashBW} is like \ct{HashB} (a hash of bigrams), except that each bigram is also weighted by how often it shows up in \ct{HashW}.

\begin{code}
bigramsWeighted :: ([(T.Text, Word64)], FreqMax) -> HashBW
bigramsWeighted (ws, freqMax) = foldl' step M.empty ws
	where
	step h (w, n) = foldl (insertBigrams n) h $ bigrams w
	insertBigrams n h' bigram = M.insertWith (+) bigram m h'
		where
		m = weightedScale n' freqMax'
		n' = fromIntegral n
		freqMax' = fromIntegral freqMax

charsWeighted :: ([(T.Text, Word64)], FreqMax) -> HashLW
charsWeighted (ws, freqMax) = foldl' step M.empty ws
	where
	step h (w, n) = foldl (insertChars n) h $ T.unpack w
	insertChars n h' c = M.insertWith (+) c m h'
		where
		m = weightedScale n' freqMax'
		n' = fromIntegral n
		freqMax' = fromIntegral freqMax

weightedScale :: Double -> Double -> Double
weightedScale num denom = 16 ** (4 * num / denom)
\end{code}
