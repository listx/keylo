\section{KEYLO/Layout.lhs}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KEYLO.Layout where

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
\end{code}

\begin{itemize}
\item{KeyName: physical keycaps; (Name, Char)}
\end{itemize}

\begin{code}

data Finger
	= FPinky
	| FRing
	| FMiddle
	| FIndex
	| FThumb
	deriving (Eq, Ord, Show)

data Hand
	= HandL
	| HandR
	deriving (Eq, Ord, Show)

type KeyName = String
type Col = Int
type Row = Int
type ColRow = (Col, Row)
\end{code}

\ct{MPenalties} are used to tie down some settings manually; for example, you might want to use a particular finger for a particular physical key, or state that some keys will be used by a finger at a particular ColRow.
Constraints are there to help weed out undesirable configurations.

We use ColRow because the syntax of (Column, Row) retains the traditional mathematical notation of (x, y).

\begin{code}
type HFinger = (Hand, Finger)
data MPenalties = MPenalties
	{ mpFavored :: Bool
	}
mPenaltiesDefault :: MPenalties
mPenaltiesDefault = MPenalties
	{ mpFavored = True
	}

nonLetters :: String
nonLetters = "`~1234567890!@#$%^&*()[]{}\\|;:'\",<.>/?"

letters :: String
letters = ['a'..'z']
\end{code}

\ct{layoutKeyNames} can be used to list the keys necessary to generate the given \ct{Char}.
For English text, this data structure is not very interesting.

\ct{layoutFinger} tells you which \ct{Finger} needs to be used against which \ct{ColRow} for the given \ct{KeyName}.

\ct{layoutFingerLiberty} tells you how many ``liberties'' a finger has, where each liberty is a \ct{ColRow}.
For example, on the Esrille NISSE keyboard, each thumb has 5 liberties.

\begin{code}
type CharToKeyName = M.Map Char KeyName

charNameHashAscii :: CharToKeyName
charNameHashAscii = M.fromList asciiKeys
	where
	lettersLowercase = map (\c -> (c, [c])) letters
	asciiKeys = lettersLowercase
		++ concatMap (uncurry genSimpleKey)
			[ ("`", ('`', '~'))
			, ("-", ('-', '_'))
			, ("=", ('=', '+'))
			, ("[", ('[', '{'))
			, ("]", (']', '}'))
			, ("\\", ('\\', '|'))
			, (";", (';', ':'))
			-- We use `succ '!'' here because Haskell mode's syntax highlighting
			-- is broken; anyway, it is the same thing as '"'.
			, ("'", ('\'', succ '!'))
			, (",", (',', '<'))
			, (".", ('.', '>'))
			, ("/", ('/', '?'))
			]

genSimpleKey :: KeyName -> (Char, Char) -> [(Char, KeyName)]
genSimpleKey name (c, cShift) =
	[ (c, name)
	, (cShift, name)
	]
\end{code}

\ct{Keyboard} is a named hash that maps a \ct{HFinger} to \ct{[ColRow]}; i.e., it describes the liberties of each finger.

\ct{KeyAtom} describes a single, keycap-less key ``unit''.
The \ct{kaColRow} is used purely for the geometric representation of the key when printed out to STDOUT and does not affect the ``value'' of the key's position.
\ct{kaPenalty} is probably the most important and defines the native ``penalty'' of this key; the higher the penalty, the less desirable it is to use the key at all.
This penalty is used later when we want to compare keys that are all assigned to the same finger, for instance.

\begin{code}
type KeyAtomRaw = (Col, Row, KeyName, Penalty)
type KeyboardAtomRawSingle = (HFinger, KeyAtomRaw)
type KeyboardAtomRaw = (HFinger, [KeyAtomRaw])
type KeyboardRaw = [KeyboardAtomRaw]
type Index = Int
data KeyAtom = KeyAtom
	{ kaHand :: Hand
	, kaFinger :: Finger
	, kaColRow :: ColRow
	, kaPenalty :: Penalty
	} deriving (Show)
data KLayout = KLayout
	{ klName :: String
	, klLayout :: V.Vector KeyName
	, klKeyboard :: V.Vector KeyAtom
	, klLayoutIdx :: M.Map KeyName Index
	, klSize :: Int
	, klSizeVisible :: Int
	, klCtkn :: CharToKeyName
	, klRaw :: KeyboardRaw
	}
\end{code}

\ct{nisse} is the ErgoDox-like keyboard from Japan.
Note the use of \ct{q} --- again, we use the \ct{succ '!'} trick to get around Emacs' Haskell mode's broken syntax highlighting.

TODO: Make \ct{nisse} be parseable from a text file.

TODO: Need to extend \ct{keyboardVerify} to cross-check the various layout definitions.
\begin{code}
keyboardVerify :: KLayout -> Maybe String
keyboardVerify KLayout{..}
	| length klName == 0 = Just "KeyboardName cannot be blank"
	| otherwise = Nothing

expandBySnd :: (a, [b]) -> [(a, b)]
expandBySnd (a, bs) = map ((,) a) bs

unzipSame :: [(a, a)] -> [a]
unzipSame = concatMap (\(l, r) -> [l, r])
type Penalty = Int

syncKLayout :: KLayout -> KLayout
syncKLayout kl@KLayout{..} = kl
	{ klRaw = layoutKeyboardToRaw klLayout klKeyboard
	}

layoutKeyboardToRaw :: V.Vector KeyName -> V.Vector KeyAtom -> KeyboardRaw
layoutKeyboardToRaw as bs
	= M.toList
	. M.fromListWith (++)
	. V.foldl' step []
	$ V.zip as bs
	where
	step acc (name, KeyAtom{..}) = ((kaHand, kaFinger), [(c, r, name, kaPenalty)]) : acc
		where
		(c, r) = kaColRow

nisse :: KLayout
nisse = KLayout
	{ klName = "nisse"
	, klLayout = V.fromList keyNames
	, klKeyboard = V.fromList keyAtoms
	, klLayoutIdx = M.fromList $ zip keyNames [0..]
	, klSize = sameSize keyNames keyAtoms
	, klSizeVisible
		= length
		. fst
		$ partition (flip elem $ map (:[]) letters) keyNames
	, klCtkn = charNameHashAscii
	, klRaw = nisseKeys
	}
	where
	sameSize xs ys
		| length xs /= length ys = error "xs/ys length mismatch"
		| otherwise = length xs - 1
	keyNames
		= map (\(_, (_, _, name, _)) -> name)
		$ sortByVisiblesAndPenalty nisseKeys
	keyAtoms
		= map (\((h, f), (c, r, _, p)) -> KeyAtom h f (c, r) p)
		$ sortByVisiblesAndPenalty nisseKeys
	nisseKeys = map primizePens
		[
			( (HandL, FPinky)
			,
				[ (-2,  1,  xxx, 7), (-1,  1,  "z", 4), ( 0,  1,   "q", 2)
				, (-2,  0,  xxx, 6), (-1,  0,  xxx, 3), ( 0,  0,   "s", 0)
				,                    (-1, -1,  xxx, 5), ( 0, -1,   "y", 1)
				]
			)
		,
			( (HandL, FRing)
			,
				[ ( 0,  1,  "r", 1)
				, ( 0,  0,  "n", 0)
				, ( 0, -1,  "w", 2)
				]
			)
		,
			( (HandL, FMiddle)
			,
				[ ( 0,  1,  "g", 1)
				, ( 0,  0,  "t", 0)
				, ( 0, -1,  "c", 2)
				]
			)
		,
			( (HandL, FIndex)
			,
				[ ( 0,  1,  "d", 2), ( 1,  1,  "b", 4), ( 2,  1,  xxx, 6)
				, ( 0,  0,  "i", 0), ( 1,  0,  "o", 3), ( 2,  0,  xxx, 7)
				, ( 0, -1,  "m", 1), ( 1, -1,  xxx, 5)
				]
			)
		,
			( (HandL, FThumb)
			,
				[ (-2, -2,  xxx, 4)
				, (-1, -2,  xxx, 1)
				, ( 0, -2,  xxx, 0)
				, ( 1, -2,  xxx, 2)
				, ( 2, -2,  xxx, 3)
				]
			)
		,	( (HandR, FPinky)
			,
				[ (-2,  1,  xxx, 7), (-1,  1,  xxx, 4), ( 0,  1,   xxx, 2)
				, (-2,  0,  xxx, 6), (-1,  0,  xxx, 3), ( 0,  0,   "u", 0)
				,                    (-1, -1,  xxx, 5), ( 0, -1,   "v", 1)
				]
			)
		,
			( (HandR, FRing)
			,
				[ ( 0,  1,  xxx, 1)
				, ( 0,  0,  "l", 0)
				, ( 0, -1,  "x", 2)
				]
			)
		,
			( (HandR, FMiddle)
			,
				[ ( 0,  1,  xxx, 1)
				, ( 0,  0,  "k", 0)
				, ( 0, -1,  "e", 2)
				]
			)
		,
			( (HandR, FIndex)
			,
				[ ( 0,  1,  "f", 2), ( 1,  1,  "p", 4), ( 2,  1,  xxx, 6)
				, ( 0,  0,  "j", 0), ( 1,  0,  "h", 3), ( 2,  0,  xxx, 7)
				, ( 0, -1,  "a", 1), ( 1, -1,  xxx, 5)
				]
			)
		,
			( (HandR, FThumb)
			,
				[ (-2, -2,  xxx, 4)
				, (-1, -2,  xxx, 1)
				, ( 0, -2,  xxx, 0)
				, ( 1, -2,  xxx, 2)
				, ( 2, -2,  xxx, 3)
				]
			)
		]
		where
		xxx = ""
		primes :: [Int]
		primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
		primizePens (h, atoms)
			= (h, map (\(a, b, c, d) -> (a, b, c, primes !! d)) atoms)
\end{code}

We want to prepare our data structure so that it is easier to mutate in a sane manner.
There are two requirements here:
\begin{enumerate}
	\item{sort the keys so that the mutatable ones (non-blacklisted) make up a contiguous span in a list, and}
	\item{within this contiguous list, sort the keys by importance (easiest to press)}
\end{enumerate}
.
This way, we don't have to do any fancy sorting or manipulation during the course of the simulated annealing process.
The first requirement allows us to easily choose the range of mutatable indices (imagine the difficulty if we had ``holes'' and had to choose from multiple ranges).
The second requirement helps us classify which mutated keys are ``good'', based on letter frequency and the ease with which we can press the key; we can use this information to encourage our future mutations to try avoiding the good keys.
Both steps are accomplished by \ct{sortVisiblesByPenalty}.

Note that there is a wrinkle in the 2\textsuperscript{nd} step.
It may be that two keys could have the same penalty for ease of use --- possibly on the same hand.
To prevent this, we alternate between the left hand and right hand; this is what \ct{a1} basically does.

\label{sortByVisiblesAndPenalty}
\subsection{hoola}
\begin{code}
sortByVisiblesAndPenalty :: KeyboardRaw -> [KeyboardAtomRawSingle]
sortByVisiblesAndPenalty kr0 = a1 ++ b
	where
	a1 = alternateKeys a1l a1r
	a1l = sortBy compPenalty a0l
	a1r = sortBy compPenalty a0r
	compPenalty ((_, f0), (_, _, _, p0)) ((_, f1), (_, _, _, p1))
		| pen0 == pen1 = compare (penalizeFinger f0) (penalizeFinger f1)
		| otherwise = compare pen0 pen1
		where
		pen0 = p0 + penalizeFinger f0
		pen1 = p1 + penalizeFinger f1
	(a0l, a0r) = partition (\kar -> handIs HandL kar) a0
	handIs :: Hand -> KeyboardAtomRawSingle -> Bool
	handIs hand ((h, _), _) = hand == h
	(a0, b) = partition (\(_, (_, _, name, _)) -> elem name letterNames) kr1
	kr1 = concatMap expandBySnd kr0
	letterNames = map (:[]) letters
\end{code}

\ct{zipKeys} is necessary because it may be the case that two hands have an unequal number of letter-based keys.
If we simply zipped them together with \ct{zip}, the extra keys on one hand would disappear!

\begin{code}

alternateKeys :: [a] -> [a] -> [a]
alternateKeys xs ys
	| length xs > length ys = alternateKeys ys xs
	| length xs == length ys = unzipSame $ zip xs ys
	| otherwise = reverse $ foldl step [] $ zip [0..] ys
	where
	step acc (idx, y)
		| idx > (length xs - 1) = y : acc
		| otherwise = y : xs!!idx : acc

penalizeFinger :: Finger -> Penalty
penalizeFinger f = case f of
	FPinky -> 4
	FRing -> 3
	FMiddle -> 1
	FIndex -> 2
	FThumb -> 5
\end{code}

We need a way of showing \ct{KLayout} in a human-friendly way.
The simplest way is to make \ct{KLayout} an instance of the \ct{Show} typeclass.
The hex value \ct{0x25a1} is the codepoint for the empty box character ``\symbol{"25A1}''.

\begin{code}
instance Show KLayout where
	show KLayout{..} = intercalate "\n"
		[ centerSpace 4 $ row 1
		, centerSpace 4 $ row 0
		, offset 2 . centerSpace 8 $ row (-1)
		, offset 6 . centerSpace 4 $ row (-2)
		]
		where
		centerSpace n str
			= (\(a, b) -> a ++ offset n b)
			$ splitAt (div (length str) 2) str
		offset n = (replicate n ' ' ++)
		row n
			= intercalate " "
			. map (\(_, _, str, _) -> if str == "" then [toEnum 0x25a1] else str)
			$ concatMap (getAtomsRawByRow klRaw n) fingers

getAtomsRawByRow :: KeyboardRaw -> Row -> HFinger -> [KeyAtomRaw]
getAtomsRawByRow kr row hfinger@(hand, _)
	= sortBy (\(c1, _, _, _) (c2, _, _, _) -> compare' c1 c2)
	. filter (\(_, r, _, _) -> r == row)
	. fromMaybe []
	$ lookup hfinger kr
	where
	compare' = if hand == HandL
		then compare
		else flip compare

fingers :: [HFinger]
fingers =
	[ (HandL, FPinky)
	, (HandL, FRing)
	, (HandL, FMiddle)
	, (HandL, FIndex)
	, (HandL, FThumb)
	, (HandR, FThumb)
	, (HandR, FIndex)
	, (HandR, FMiddle)
	, (HandR, FRing)
	, (HandR, FPinky)
	]

keyAtomsByFinger :: KeyboardRaw -> HFinger -> [KeyAtomRaw]
keyAtomsByFinger assocs = fromMaybe [] . flip lookup assocs
\end{code}
