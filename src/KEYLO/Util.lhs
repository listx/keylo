\section{KEYLO/Util.lhs}

\begin{code}
module KEYLO.Util where

import System.Exit
import System.IO
\end{code}

These two functions make it easier to surround text with a given pair of strings.

\begin{code}
enclose :: (String, String) -> String -> String
enclose (a, b) w = a ++ w ++ b

enclose' :: (String, String) -> String -> String
enclose' pair = enclose sSpaces . enclose pair

dQuotes
	, sQuotes
	, parens
	, bracks
	, braces
	, sSpaces :: (String, String)
dQuotes = ("\"", "\"")
sQuotes = ("`", "'")
parens = ("(", ")")
bracks = ("[", "]")
braces = ("{", "}")
sSpaces = (" ", " ")

showTupleAsColumns :: (Show a, Show b) => (a, b) -> String
showTupleAsColumns (a, b) = show a ++ " " ++ show b
\end{code}

These are some basic error-logging/exiting functions.

\begin{code}
abort :: (String, Int) -> IO ()
abort (msg, eid) = do
    errMsg msg
    hPutStrLn stderr "operation aborted"
    exitWith $ ExitFailure eid

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg
\end{code}

\begin{code}
escapeShell :: String -> String
escapeShell = concatMap f
	where
	f c
		| elem c specialShellChars = '\\':c:[]
		| otherwise = [c]
	specialShellChars = "`~!@#$%^&*()[]\\|;'\"<>" :: String
\end{code}

Below are some miscellaneous math functions.

\begin{code}
energyLossPerc :: Double -> Double -> Double
energyLossPerc e1 e2 = (1.0 - (e2 / e1)) * 100
\end{code}
