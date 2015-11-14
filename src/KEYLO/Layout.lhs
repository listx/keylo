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
	deriving (Eq, Ord)

data Hand
	= HandL
	| HandR
	deriving (Eq, Ord, Show)

type KeyName = String
type Col = Int
type Row = Int
type ColRow = (Col, Row)
\end{code}

Constraints are used to tie down some settings manually; for example, you might want to use a particular finger for a particular physical key, or state that some keys will be used by a finger at a particular ColRow.
Constraints are there to help weed out undesirable configurations.

We use ColRow because the syntax of (Column, Row) retains the traditional mathematical notation of (x, y).

\begin{code}
type HFinger = (Hand, Finger)
data Constraints = Constraints
	{ cFixedKeys :: [Char]
	, cFingerWhitelist :: M.Map HFinger [KeyName]
	, cCharBlacklist :: [Char]
	}
constraintDefault :: Constraints
constraintDefault = Constraints
	{ cFixedKeys = []
	, cFingerWhitelist = M.fromList []
	, cCharBlacklist = nonLetters
	}

nonLetters :: String
nonLetters = "`~1234567890!@#$%^&*()[]{}\\|;:'\",<.>/?"
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
	lettersLowercase = map (\c -> (c, [c])) ['a'..'z']
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
type KeyAtomRaw = (Int, Int, KeyName, Int)
type KeyboardAtomRaw = (HFinger, [KeyAtomRaw])
type KeyboardRaw = [KeyboardAtomRaw]
data KeyAtom = KeyAtom
	{ kaHand :: Hand
	, kaFinger :: Finger
	, kaColRow :: ColRow
	, kaPenalty :: Penalty
	}
data KLayout = KLayout
	{ klName :: String
	, klLayout :: V.Vector KeyName
	, klKeyboard :: V.Vector KeyAtom
	, klSize :: Int
	, klSizeVisible :: Int
	, klKeyNameToIdx :: M.Map KeyName Int
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

expandBySnd :: (a, [b]) -> [(b, a)]
expandBySnd (a, bs) = map (flip (,) a) bs

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
\end{code}

\ct{revSortByKeyName} sorts by the name of the key, but in reverse.
The reversal is necessary because we want the non-named keys to be listed \textit{last}, not first.
This way, we can have a nice contiguous vector of ``filled'' elements at the front, allowing us to easily choose a random number somewhere along these indices later when we want to mutate them.

\begin{code}
revSortByKeyName :: [(KeyAtomRaw, HFinger)] -> [(KeyAtomRaw, HFinger)]
revSortByKeyName xs = rest ++ moveMe
	where
	ys = sortBy revCompName xs
	revCompName ((_, _, name0, _), _) ((_, _, name1, _), _)
		= compare name1 name0
	(moveMe, rest) = span moveNonLetters ys
	moveNonLetters ((_, _, name, _), _) = elem name $ map (:[]) nonLetters

nisse :: KLayout
nisse = KLayout
	{ klName = "nisse"
	, klLayout = V.fromList keyNames
	, klKeyboard = V.fromList keyAtoms
	, klSize = sameSize keyNames keyAtoms
	, klSizeVisible = length . fst $ break (flip elem $ map (:[]) nonLetters) keyNames
	, klKeyNameToIdx = M.fromList $ zip keyNames [0..(length keyNames - 1)]
	, klCtkn = charNameHashAscii
	, klRaw = nisseKeys
	}
	where
	sameSize xs ys
		| length xs /= length ys = error "xs/ys length mismatch"
		| otherwise = length xs - 1
	keyNames
		= map (\((_, _, name, _), _) -> name)
		. revSortByKeyName
		$ concatMap expandBySnd nisseKeys
	keyAtoms
		= map (\((c, r, _, p), (h, f)) -> KeyAtom h f (c, r) p)
		. revSortByKeyName
		$ concatMap expandBySnd nisseKeys
	nisseKeys =
		[
			( (HandL, FPinky)
			,
				[ (-2,  1,  "[", 7), (-1,  1,  "]", 5), ( 0,  1,     q, 2)
				, (-2,  0,  "`", 6), (-1,  0,    x, 3), ( 0,  0,   "a", 0)
				,                    (-1, -1,    x, 4), ( 0, -1,   ";", 1)
				]
			)
		,
			( (HandL, FRing)
			,
				[ ( 0,  1,  ",", 1)
				, ( 0,  0,  "o", 0)
				, ( 0, -1,  "q", 2)
				]
			)
		,
			( (HandL, FMiddle)
			,
				[ ( 0,  1,  ".", 1)
				, ( 0,  0,  "e", 0)
				, ( 0, -1,  "j", 2)
				]
			)
		,
			( (HandL, FIndex)
			,
				[ ( 0,  1,  "p", 2), ( 1,  1,  "y", 5), ( 2,  1,    x, 7)
				, ( 0,  0,  "u", 0), ( 1,  0,  "i", 3), ( 2,  0,    x, 6)
				, ( 0, -1,  "k", 1), ( 1, -1,  "x", 4)
				]
			)
		,
			( (HandL, FThumb)
			,
				[ (-2, -2,    x, 3)
				, (-1, -2,    x, 2)
				, ( 0, -2,    x, 0)
				, ( 1, -2,    x, 1)
				, ( 2, -2,    x, 4)
				]
			)
		,	( (HandR, FPinky)
			,
				[ (-2,  1, "\\", 7), (-1,  1,  "/", 5), ( 0,  1,   "l", 2)
				, (-2,  0,  "=", 6), (-1,  0,  "-", 3), ( 0,  0,   "s", 0)
				,                    (-1, -1,    x, 4), ( 0, -1,   "z", 1)
				]
			)
		,
			( (HandR, FRing)
			,
				[ ( 0,  1,  "r", 1)
				, ( 0,  0,  "n", 0)
				, ( 0, -1,  "v", 2)
				]
			)
		,
			( (HandR, FMiddle)
			,
				[ ( 0,  1,  "c", 1)
				, ( 0,  0,  "t", 0)
				, ( 0, -1,  "w", 2)
				]
			)
		,
			( (HandR, FIndex)
			,
				[ ( 0,  1,  "g", 2), ( 1,  1,  "f", 5), ( 2,  1,    x, 7)
				, ( 0,  0,  "h", 0), ( 1,  0,  "d", 3), ( 2,  0,    x, 6)
				, ( 0, -1,  "m", 1), ( 1, -1,  "b", 4)
				]
			)
		,
			( (HandR, FThumb)
			,
				[ (-2, -2,    x, 3)
				, (-1, -2,    x, 2)
				, ( 0, -2,    x, 0)
				, ( 1, -2,    x, 1)
				, ( 2, -2,    x, 4)
				]
			)
		]
		where
		x = ""
		q = succ '!' : []
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
