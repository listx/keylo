\section{KEYLO/Generate.lhs}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KEYLO.Generate where

import Control.Monad
import Data.Data
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import Data.Word hiding (Word)
import Safe
import System.Random.PCG
import System.Random.PCG.Class

import KEYLO.Frequency
import KEYLO.Layout

data Algorithm
	= ARandom
	| ASimAnneal
	deriving (Data, Eq, Show, Typeable)

type PenaltyD = Double

type PseudoThreadID = Int

data KLSearchCtx = KLSearchCtx
	{ klscMPenalties :: MPenalties
	, klscCorpus :: T.Text
	, klscFreqL :: (HashL, FreqMax)
	, klscFreqBW :: HashBW
	, klscFreqW :: (HashW, FreqMax)
	, klscFreqWTrunc :: [(T.Text, Word64)]
	, klscFreqLW :: HashLW
	, klscKLayout :: KLayout
	, klscKeyPlacementPenalty :: V.Vector PenaltyD
	, klscCoolingChart :: [(Int, Energy)]
	, klscTid :: PseudoThreadID
	, klscAlgo :: Algorithm
	} deriving (Eq)

instance Show KLSearchCtx where
	show klsc@KLSearchCtx{..} = unlines
		[ show klscKLayout
		, "energy (penalty): " ++ show (energy klsc)
		]

instance Ord KLSearchCtx where
	compare a b = compare (energy a) (energy b)
\end{code}

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
		(i, j) <- getRandIndices klsc rng
		let
			l = swapIdx i j klLayout
			kl = klscKLayout
				{ klLayout = l
				, klLayoutIdx = foldl' step klLayoutIdx
					[ (l V.! i, i)
					, (l V.! j, j)
					]
				}
		return $ klsc
			{ klscKLayout = kl
			, klscKeyPlacementPenalty
				= updateKpp klscFreqL klscKeyPlacementPenalty kl [0..(V.length klscKeyPlacementPenalty - 1)]
			}
		where
		step h (k, v) = M.adjust (\_ -> v) k h
	energy KLSearchCtx{..}
		= penalizeFreqL klscFreqL klscFreqLW klscKLayout
		+ penalizeFreqB klscFreqBW klscKLayout
		+ penalizeHandBalance klscFreqL klscKLayout
--		+ penalizeHJKL klscKLayout
\end{code}

\ct{getRandIndices} is important in simulating the idea of natural selection.
The idea is to encourage the algorithm to keep the good keys and try to swap the bad keys (if our mutations are highly volatile, where we swap well-placed key away from its position, our search is as good as random search).

NOTE: \ct{hFreqL} is 26 elements big because its membership is guarded by \ct{isAlphabet} in Section~\ref{letterFreq}.
Still, we double-check just in case.

\ct{placementPenalty} is what really matters.
It is calculated as
\begin{equation}
\mathrm{PlacementPenalty} = \mathrm{PhysicalPenalty} * \mathrm{Frequency}
\end{equation}

.
The \textbf{frequency} term must be in terms of a percentage to keep it from becoming a runaway growing number where we use a larger corpus with more raw frequency counts.
\begin{code}
getRandIndices :: KLSearchCtx -> GenIO -> IO (Int, Int)
getRandIndices KLSearchCtx{..} rng = do
	r1 <- uniform rng :: IO Double
	r2 <- uniform rng :: IO Double
	let
		i = selectIdxByProbability r1 klscKeyPlacementPenalty
		j = selectIdxByProbability r2 klscKeyPlacementPenalty
	normalize i j
	where
	normalize i j
		| j /= i = return (i, j)
		| otherwise = do
			dir <- uniform rng :: IO Bool
			let
				k = if dir == True
					then i + 1
					else i - 1
			return (i, mod k (klSizeVisible klscKLayout))

selectIdxByProbability :: Double -> V.Vector PenaltyD -> Int
selectIdxByProbability r weights = bSearch 0 (V.length weights - 1)
	where
	r' = V.sum weights * r
	subtotals = genSums weights
	bSearch lo hi
		| length weights == 0 = 0
		| subtotalAtGuess < r' = bSearch (guess + 1) hi
		| subtotalAtGuess - (weights V.! guess) > r' = bSearch lo (guess - 1)
		| otherwise = guess
		where
		guess = div (lo + hi) 2
		subtotalAtGuess = atNote "bSearch: oops" subtotals guess

genSums :: V.Vector PenaltyD -> [PenaltyD]
genSums pens = reverse . snd $ V.foldl' step (0, []) pens
	where
	step (subtotal, acc) p = let
		p' = subtotal + p
		in
		(p', p' : acc)
\end{code}

\ct{updateKpp} updates the placement penalties for the given indices.

\begin{code}
updateKpp
	:: (HashL, FreqMax)
	-> V.Vector PenaltyD
	-> KLayout
	-> [Index]
	-> V.Vector PenaltyD
updateKpp (hFreqL, maxL) kpp KLayout{..} idxs
	= V.unsafeUpd kpp
	$ zip idxs penalties
	where
	penalties = map placementPenalty idxs
	placementPenalty idx = penPhysical * freqPerc
		where
		penPhysical = fromIntegral $ penaltyAtom ka
		freq = fromIntegral $ fromMaybe 1 $ M.lookup keyChar hFreqL
		freqPerc = (freq / (fromIntegral maxL :: Double)) * 100
		ka = klKeyboard V.! idx
		keyName = klLayout V.! idx
		keyChar = headNote "updateKpp: zero-length key name detected" keyName

penaltyAtom :: KeyAtom -> Penalty
penaltyAtom KeyAtom{..} = rowPen * (kaPenalty + penalizeFinger kaFinger)
	where
	(_, row) = kaColRow
	rowPen
		-- Home row is special!
		| row == 0 = 1
		| otherwise = abs row
\end{code}

\ct{exaggeratePenalties} checks for possible integer bounds overflow.

\begin{code}
exaggeratePenalties :: [(Penalty, Int)] -> [(Penalty, Int)]
exaggeratePenalties = map (\(p, i) -> (safeRaise p (2::Int), i))

safeRaise :: (Show a, Integral b, Num a, Ord a) => a -> b -> a
safeRaise n e
	| (n^e) < n = error $ "integer overflow! (" ++ "n was " ++ show n ++ ")"
	| otherwise = n^e

penalizeAtom :: KeyAtom -> Penalty
penalizeAtom KeyAtom{..}
	= (pf * kaPenalty * penalizeColRow kaColRow) + pf
	where
	pf = penalizeFinger kaFinger
\end{code}

\ct{penalizeColRow} penalizes key distance.

\begin{code}
penalizeColRow :: ColRow -> Penalty
penalizeColRow (c, r) = (abs c + 1) * (abs r + 1)

penalizeFreqL
  :: (HashL, FreqMax)
  -> HashLW
  -> KLayout
  -> Penalty
penalizeFreqL hl@(h, _) hw kl = M.foldlWithKey' step 0 h
	where
	step acc char _ = acc + penalizeChar hl hw char kl

penalizeFreqB
	:: HashBW
    -> KLayout
    -> Penalty
penalizeFreqB hbw kl = M.foldlWithKey' step 0 hbw
	where
	step acc bigram freq = acc + penalizeBigram bigram freq kl

penalizeHandBalance :: (HashL, FreqMax)
    -> KLayout
    -> Penalty
penalizeHandBalance (h, fMax) kl
	= floor
	. imbalance
	$ M.foldlWithKey' step (0, 0) h
	where
	step (ls, rs) char freq
		| kaHand == HandL = (n + ls, rs)
		| otherwise = (ls, n + rs)
		where
		KeyAtom{..} = getKeyAtom kl char
		n = weightedScale (fromIntegral freq) (fromIntegral fMax)
	imbalance (a, b) = abs (a - b)

penalizeHJKL :: KLayout -> Penalty
penalizeHJKL kl = penalizeHL + penalizeJK + bonusHJKL
	where
	kaH = getKeyAtom kl 'h'
	kaJ = getKeyAtom kl 'j'
	kaK = getKeyAtom kl 'k'
	kaL = getKeyAtom kl 'l'
	(_, yj) = kaColRow kaJ
	(_, yk) = kaColRow kaK
	sameHandHL = kaHand kaH == kaHand kaL
	sameHandJK = kaHand kaJ == kaHand kaK
	penalizeHL
		| sameHandHL && kaHand kaH == HandL = if kaFinger kaH > kaFinger kaL
			then 100
			else 0
		| sameHandHL && kaHand kaH == HandR = if kaFinger kaH < kaFinger kaL
			then 100
			else 0
		| kaHand kaH == HandL = 0
		| otherwise = 100
	penalizeJK
		| yj > yk = 100
		| otherwise = 0
	bonusHJKL
		| sameHandHL && sameHandJK = -10000
		| sameHandHL = -2000
		| sameHandJK = -2000
		| otherwise = 0

penalizeChar
  :: (HashL, FreqMax)
  -> HashLW
  -> Char
  -> KLayout
  -> Penalty
penalizeChar (hl, maxL) hlw char kl@KLayout{..}
	= case M.lookup char hl of
	Just freq -> let
		freq' = fromIntegral freq
		maxL' = fromIntegral maxL
		charImportance = freq' / (maxL' * 100 :: Double)
		penaltyFactor = floor (charImportance * charWordImportance)
		in
		penaltyFactor * (penalizeAtom (getKeyAtom kl char))
	Nothing -> 0
	where
	charWordImportance = case M.lookup char hlw of
		Just pen -> pen
		Nothing -> 1
\end{code}

\ct{penalizeBigram}: Penalize typing a bigram --- the following are discouraged: typing with the same finger, typing hard-to-reach keys, and typing with the same hand.
\ct{bwImportance} measures how important a bigram is with respect to a word.
For example, the bigram ``it'' is in two places in ``repitition''.
The idea is to boost a bigram's importance by the frequency in which it shows up in frequent words.
This is why we need the \ct{hw} and \ct{maxW}; the actual calculation is performed by looping through all words, and seeing how many times the bigram shows up in those words.

Both \ct{charWordImportance} and \ct{bwImportance} follow the same philosophy in emphasizing the word's frequency percentage in a nonlinear scale. so that more frequent words have a much greater impact than less frequent words.
\textbf{We heavily emphasize word frequency in judging the weight of a letter or bigram}.

\begin{code}
penalizeBigram
	:: Bigram
    -> PenaltyD
    -> KLayout
    -> Penalty
penalizeBigram bigram freq kl@KLayout{..}
	= penaltiesFinger
	+ penaltyHand
	+ penaltyVertTravel
	where
	(char0, char1) = bigram
	ka0 = getKeyAtom kl char0
	ka1 = getKeyAtom kl char1
	(x0, y0) = kaColRow ka0
	(x1, y1) = kaColRow ka1
	f0 = kaFinger ka0
	f1 = kaFinger ka1
	h0 = kaHand ka0
	h1 = kaHand ka1
	penaltiesFinger
		= (floor $ freq * (fromIntegral $ penalizeAtom ka0 + penalizeAtom ka1))
		+ (penaltyFingerSame * (floor $ freq * 10 ** 1.2))
	penaltyFingerSame
		| f0 == f1 = penalizeAtom ka0
		| otherwise = 0
	penaltyHand
		| ka0 == ka1 = 0
		| h0 == h1 = div penaltiesFinger 2
		| otherwise = 0
	penaltyVertTravel
		| ka0 == ka1 = 0
		| h0 == h1 = penaltiesFinger * dist
		| otherwise = 0
		where
		dist
			| f0 == f1 && h0 == h1 = distSameFinger
			| otherwise = distDiffFinger
		distSameFinger
			-- Horizontal same-finger movement is terrible, worse than
			-- same-finger vertical movement. This is the worst!
			| y0 == y1 = 5
			| x0 == x1 = 1 + abs (y0 - y1)
			| otherwise = 1
		-- If we're using different fingers, then key travel isn't such a big
		-- deal. If we're on the same hand though, then give a small penalty for
		-- rolling outwards (in the order index->middle->ring->pinky) instead of
		-- inwards (pinky->ring->middle->index).
		distDiffFinger
			| h0 == h1 && f1 > f0 = 2
			| otherwise = 1

getKeyAtom :: KLayout -> Char -> KeyAtom
getKeyAtom KLayout{..} c = fromJustNote "getKeyAtom" $ do
	str <- M.lookup c klCtkn
	idx <- M.lookup str klLayoutIdx
	return $ klKeyboard V.! idx
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
swapIdx i j v = V.unsafeUpd v [(i, jVal), (j, iVal)]
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

anneal
	:: (Show a, Annealable a)
	=> Maybe FilePath
	-> a
	-> TimeMax
    -> GenIO
    -> IO (a, [(Int, Energy)])
anneal cooling_dir st tMax rng = do
	ts <- getTimes
	foldM (annealStep cooling_dir rng) (st, [])
		. zip [1..tMax]
		$ map (temperature tMax) ts
	where
	getTimes :: IO [Int]
	getTimes = return . reverse =<< foldM step [1::Int] [1..tMax]
	step acc _ = do
		let
			tCur = headNote "anneal" acc
			tMax' = fromIntegral tMax :: Double
			tCur' = fromIntegral tCur :: Double
			continueUnchanged = (tCur + 1) : acc
		restartMark <- return . (1/) =<< uniformR (2.0, 10.0) rng
		if ((tCur' / tMax') >= restartMark)
			then do
				r <- uniformR (0.0, 1.0) rng :: IO Double
				if r >= 0.99
					then return (1 : acc)
					else return continueUnchanged
			else return continueUnchanged

annealStep
	:: (Show a, Annealable a)
	=> Maybe FilePath
    -> GenIO
    -> (a , [(Int, Energy)])
    -> (Int, Temperature)
	-> IO (a, [(Int, Energy)])
annealStep cooling_dir rng (st0, bs) (time, temp) = do
	r <- uniformR (0.0, 1.0) rng
	st1 <- mutate st0 rng
	let
		e1 = energy st1
		shouldMutate = probability e0 e1 temp > r
		a = if shouldMutate
			then st1
			else st0
	return $ if
		| isJust cooling_dir
			&& shouldMutate
			&& e1 < e0 -> (a, (time, e1):bs)
		| otherwise -> (a, bs)
	where
	e0 = energy st0
\end{code}

\ct{temperature} is modeled to closely tend to zero as our time percentage reaches 100\%.
The equation is
\begin{equation}
Temperature = 1 - \left(\frac{t}{t + (1 - t)^{cr}}\right)
\end{equation}

where \textit{t} is the time percentage (we go from a low number toward 1.0), and \textit{cr} is the cooling rate.
The higher the cooling rate, the quicker the temperature drops toward 0.
A cooling rate of \(\frac{1}{2}\) would mean that the temperature would reach 0.4 when were about 80 percent done, whereas a cooling rate of 4 would mean that the temperature would reeach 0.4 when we're only about 30 percent done.
In comparison, a cooling rate of 50 would get us to 0.4 temperature when we're only about 6 percent complete!

\begin{code}
temperature :: TimeMax -> TimeCur -> Temperature
temperature tMax tCur = exp (-t * cr)
	where
	t = fromIntegral tCur / fromIntegral tMax * 3
	cr = 1

probability :: Energy -> Energy -> Temperature -> Probability
probability e0 e1 t = exp (fromIntegral (e0 - e1) / t)
\end{code}

\ct{randSearch} is like \ct{anneal} as it has the same type signature, but it is a simple linear search in that it always accepts the mutation if it results in lower energy.
The theory is that this will quickly lead to the solution space becoming trapped into a local minimum..

\begin{code}
randSearch
	:: (Show a, Annealable a)
	=> Maybe FilePath
	-> a
	-> TimeMax
	-> GenIO
    -> IO (a, [(Int, Energy)])
randSearch cooling_dir st tMax rng
	= foldM (randStep cooling_dir rng) (st, []) [1..tMax]

randStep
	:: (Show a, Annealable a)
	=> Maybe FilePath
	-> GenIO
    -> (a, [(Int, Energy)])
    -> Int
    -> IO (a, [(Int, Energy)])
randStep cooling_dir rng (st0, bs) time = do
	st1 <- mutate st0 rng
	let
		e1 = energy st1
		shouldMutate = e1 < e0
		a = if shouldMutate
			then st1
			else st0
	return $ if
		| isJust cooling_dir
			&& shouldMutate
			&& e1 < e0 -> (a, (time, e1):bs)
		| otherwise -> (a, bs)
	where
	e0 = energy st0
\end{code}

\ct{shuffle} is the famous \href{https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle}{Fisher-Yates shuffle}.

\begin{code}
shuffle :: V.Vector a -> GenIO -> IO (V.Vector a)
shuffle vector rng
	= V.foldM step vector
	. V.fromList
	$ reverse [1..(V.length vector - 1)]
	where
	step :: V.Vector a -> Int -> IO (V.Vector a)
	step v i = do
		j <- uniformR (0, i) rng
		return $ V.unsafeUpd v [(i, v V.! j), (j, v V.! i)]
\end{code}
