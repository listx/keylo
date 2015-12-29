\section{KEYLO/Generate.lhs}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KEYLO.Generate where

import Control.Monad
import Data.Data
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import System.Random.PCG
import System.Random.PCG.Class

import KEYLO.Frequency
import KEYLO.Layout

data Algorithm
	= ARandom
	| ASimAnneal
	deriving (Data, Eq, Show, Typeable)

data KLSearchCtx = KLSearchCtx
	{ klscConstraints :: Constraints
	, klscCorpus :: T.Text
	, klscFreqL :: (HashL, FreqMax)
	, klscFreqB :: (HashB, FreqMax)
	, klscFreqW :: (HashW, FreqMax)
	, klscKLayout :: KLayout
	}

instance Show KLSearchCtx where
	show klsc@KLSearchCtx{..} = unlines
		[ show klscKLayout
		, "energy (penalty): " ++ show (energy klsc)
		]
\end{code}

\ct{genLayoutFinger}'s \ct{pkeyRank} is probably the most important of all.
It ranks every \ct{KeyName} found in \ct{cgh} by \ct{hl} (letter frequency), where the term ``letter'' means any \ct{Char}.
The ranking of each \ct{KeyName} is then boosted by \ct{hw} (word frequency), according to the following formula: weight each word by its percentage, and then for each word, ``dissect'' it by letter, and assign these points to the letter.
E.g., if the word ``that'' is worth 15 points (makes up 15\% of all words), then add to the letter ``t'' 30 points because ``t'' occurs 2 times, while ``h'' and ``a'' get 15 points each.
We only use words with at least 0.05\% frequency.

After the letters are ranked, we then rank each physical key (\ct{KeyName}).
If a character requires more than one physical key, we divide up the score of the \ct{Char} evenly across all physical keys.

We then place each key on each finger --- this is the \ct{layoutFinger} structure that will be inside a \ct{Layout} type.
Each finger has a list of open positions, and each finger and position are ranked; the placement starts first with the left hand side, and alternates between the left and right finger.
The physical key placement is subject to constraints, such as ``this physical key is already on this ColRow'', etc.

After each physical key placement, we also run a bigram test.
The bigram test is as follows: select all bigrams that involve the last-positioned physical key; if we end up using the same finger for any of these bigrams, place it on the next-available-ranked finger-colrow, and run the same bigram test again.
If the second bigram test results in a lesser or equal bigram penalty score, keep this second configuration; if this second configuration is worse, then discard it and simply use the first configuration.
The last key has no rights, and will always use whatever last finger-colrow is available.
The bigram penalty score is set as the frequency of the bigram relative to the other bigrams (if it is 10.5\% then it is 105 points), and this number is set against the sorted physical key rank of each of the keys involved in the bigram, added (if ``t'' is 20 and ``h'' is 15, then 35).
The bigram score must be at least 2 times greater than the letter frequency score in order for it to be deemed important enough to make us ``skip'' the currently-considered colrow for the physical key.
This ``2 times greater'' number is custom-set.

\ct{CharNameHash} is the hash we use to find which \ct{KeyName}s need to be pressed in order to generate a given \ct{Char}.

\ct{lSorted} is the Chars sorted by frequency, and then reversed so that the most frequent letters come first.
To do this, we call \ct{reverseSortByVal} where we bake in a logic of sorting and reversing into one function by running \ct{compare} against the given two arguments in reverse order (take in ``a'' and ``b'' but evaluate ``b'' against ``a'').

\subsection{Stochastic Optimization via Simulated Annealing}

\ct{energy} is probably the most important function.
The \textbf{lower} the energy, the better.

\begin{code}
class Annealable a where
	mutate :: a -> GenIO -> IO a
	energy :: a -> Energy
\end{code}

For \ct{KLSearchCtx}, mutation depends on the search context.
But for now, we just swap two keys randomly.

For energy, we measure penalties.
The lower the penalty, the better.

\begin{code}
instance Annealable KLSearchCtx where
	mutate klsc@KLSearchCtx{..} rng = do
		let
			KLayout{..} = klscKLayout
			visibleRange = (0, klSizeVisible - 1)
		i <- uniformR visibleRange rng
		j <- uniformR' visibleRange (V.fromList [i]) rng
		return $ klsc
			{ klscKLayout = klscKLayout
				{ klLayout = swapIdx i j klLayout
				}
			}
	energy KLSearchCtx{..} = penalizeBigrams klscFreqB klscKLayout

penalizeFinger :: Finger -> Penalty
penalizeFinger f = case f of
	FPinky -> 3
	FRing -> 2
	FMiddle -> 0
	FIndex -> 1
	FThumb -> 4
\end{code}

\ct{penalizeColRow} penalizes key distance.

\begin{code}
penalizeColRow :: ColRow -> Penalty
penalizeColRow (c, r) = 2 * abs c * abs r

penalizeBigrams :: (HashB, FreqMax) -> KLayout -> Penalty
penalizeBigrams hf@(h, _) kl = M.foldlWithKey' step 0 h
	where
	step acc bigram _ = acc + penalizeBigram hf bigram kl
\end{code}

\ct{penalizeBigram}: Penalize typing a bigram --- the following are discouraged: typing with the same finger, typing hard-to-reach keys, and typing with the same hand.

\begin{code}
penalizeBigram :: (HashB, FreqMax) -> Bigram -> KLayout -> Penalty
penalizeBigram (h, freqMax) bigram kl@KLayout{..} = case M.lookup bigram h of
	Just freq -> let
		freq' = fromIntegral freq
		freqMax' = fromIntegral freqMax
		penaltyFactor = floor (freq' / freqMax' * 100 :: Double)
		in
		penaltyFactor * (penaltiesFinger * 3) + penaltyHand
	Nothing -> 0
	where
	char0 = T.index bigram 0
	char1 = T.index bigram 1
	penaltiesFinger
		= penaltyFingerBase char0
		+ penaltyFingerBase char1
		+ penaltyFingerSame
	penaltyFingerBase c = fromMaybe 0 $ do
		KeyAtom{..} <- getKeyAtom kl c
		return $ kaPenalty
			+ penalizeFinger kaFinger
			+ penalizeColRow kaColRow
	penaltyFingerSame
		| char0 == char1 = penaltyFingerBase char0
		| otherwise = 0
	penaltyHand = fromMaybe 0 $ do
		ka0 <- getKeyAtom kl char0
		ka1 <- getKeyAtom kl char1
		return $ if (kaHand ka0 == kaHand ka1)
			then 5
			else 0

getKeyAtom :: KLayout -> Char -> Maybe KeyAtom
getKeyAtom KLayout{..} c = do
	str <- M.lookup c klCtkn
	idx <- V.findIndex (==str) klLayout
	klKeyboard V.!? idx
\end{code}

\ct{penalizeSameFinger}: break up the given text into bigrams, and see if we end up using the same finger for each bigram.
Each bigram that is typed by the same finger gets a 1 point penalty.

\begin{code}
consecChars :: T.Text -> Bool
consecChars text
	| text == T.empty = False
	| otherwise = T.all (== headChar) text
	where
	headChar = T.head text

swapIdx :: Int -> Int -> V.Vector a -> V.Vector a
swapIdx i j v = V.update v (V.fromList [(i, jVal), (j, iVal)])
	where
	iVal = v V.! i
	jVal = v V.! j
\end{code}

\ct{uniformR'} is like \ct{uniformR}, but rolls again if the retrieved random number is in the given list (a blacklist of values). Note that it is dumb and ay lead to an infinite loop given an overly aggressive blacklist!

\begin{code}
uniformR'
	:: (Eq a, Variate a, Generator g m)
	=> (a, a)
    -> V.Vector a
    -> g
    -> m a
uniformR' (a, b) xs rng = do
	c <- uniformR (a, b) rng
	if V.elem c xs
		then uniformR' (a, b) xs rng
		else return c

type TimeMax = Int
type TimeCur = Int
type Temperature = Double
type Energy = Int
type Probability = Double

anneal :: (Show a, Annealable a) => a -> TimeMax -> GenIO -> IO a
anneal st tMax rng
	= foldM (annealStep rng) st
	$ map (temperature tMax) [1..tMax]

annealStep :: (Show a, Annealable a) => GenIO -> a -> Temperature -> IO a
annealStep rng st0 t = do
	r <- uniformR (0.0, 1.0) rng
	st1 <- mutate st0 rng
	let
		e2 = energy st1
		shouldMutate = probability e1 e2 t > r
	if shouldMutate
		then return st1
		else return st0
	where
	e1 = energy st0

temperature :: TimeMax -> TimeCur -> Temperature
temperature tMax tCur = 50.0 * exp (0.0 - (5.0 * currentRatio))
	where
	currentRatio = fromIntegral tCur / fromIntegral tMax

probability :: Energy -> Energy -> Temperature -> Probability
probability e1 e2 t = exp (fromIntegral (e1 - e2) / t)
\end{code}

\ct{randSearch} is like \ct{anneal} as it has the same type signature, but it is a simple random search.

\begin{code}
randSearch :: (Show a, Annealable a) => a -> TimeMax -> GenIO -> IO a
randSearch st tMax rng
	= foldM (randStep rng) st [1..tMax]

randStep :: (Show a, Annealable a) => GenIO -> a -> Int -> IO a
randStep rng st0 _ = do
	st1 <- mutate st0 rng
	let
		e2 = energy st1
		shouldMutate = e1 > e2
	if shouldMutate
		then return st1
		else return st0
	where
	e1 = energy st0
\end{code}
