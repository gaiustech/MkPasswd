-- a tool for generating random passwords (based on the Expect version by Don Libes, NIST)
--
-- 31-AUG-2010 Guy H Initial version

module Main where

import IO
import System
import System.Random
import System.Random.Shuffle (shuffle')
import System.Console.GetOpt
import Control.Monad (forM)
import Control.Monad.State
import Data.Char (isDigit)

-- default password is 9 chars long and a mix of uppercase, lowercase and digits - no special chars for Oracle
data Options = Options { minLength::Int, minDigits::Int, minLower::Int, minUpper::Int, minSpec::Int, username::String }
defOpts = Options { minLength = 9, minDigits = 2, minLower = 2, minUpper = 2, minSpec = 0, username="" }

-- Generate a random number between 0 and x, maintaining the state of the RNG
randInt:: Int -> State StdGen Int
randInt x = state $ randomR (0, x) -- changed for MTL2

-- For each string in list xss, pick one character at random and use these to construct a new string
randItem::[[a]] ->StdGen ->([a], StdGen)
randItem xss = runState $ do
    forM xss (\xs ->do
        idx <-randInt $ length xs - 1
        return (xs !! idx))

-- build a list of lists of members of each character class, pick one from each sublist, then shuffle the results
-- pad with lowercase letters if the sum of the options is too low 
mkPasswd' g l d c u s = x'
    where
        (x, g') = randItem (replicate u ['A'..'Z'] ++ replicate d ['0'..'9'] ++ replicate s (['!'..'/'] ++ [':'..'@']) ++ replicate (max c (l - (s + c + u))) ['a'..'z']) g
        x' = shuffle' x (length x) g'

-- for the purpose of Oracle the first character should not be a number
mkPasswd::StdGen ->Options ->String
mkPasswd g o = case isDigit (head p) of
    True ->mkPasswd g' o
    False ->p
    where
        (g', g'') = split g
        p = mkPasswd' g'' l d c u s
        (l, d, c, u, s) = (minLength o, minDigits o, minLower o, minUpper o, minSpec o)

-- generate a list of n passwords
mkPasswdList g o n = mkPasswdList' g o n []
mkPasswdList' g o n acc 
    |(length acc) < n = mkPasswdList' g' o n ((mkPasswd g'' o):acc)
    |otherwise = acc
    where
        (g', g'') = split g

options::[OptDescr (Options ->IO Options)]
options = [ Option "l" ["length"] (ReqArg (\arg opt ->return opt {minLength = (read arg)}) "NUMBER") "Length in chars"
          , Option "d" ["digits"] (ReqArg (\arg opt ->return opt {minDigits = (read arg)}) "NUMBER") "Number of digits"
          , Option "c" ["lower"]  (ReqArg (\arg opt ->return opt {minLower  = (read arg)}) "NUMBER") "Number of lowercase chars"
          , Option "C" ["upper"]  (ReqArg (\arg opt ->return opt {minUpper  = (read arg)}) "NUMBER") "Number of uppercase chars"
          , Option "s" ["upper"]  (ReqArg (\arg opt ->return opt {minSpec   = (read arg)}) "NUMBER") "Number of special chars"
          , Option "u" ["username"]  (ReqArg (\arg opt ->return opt {username  = arg}) "USERNAME") "Username to set password"
          , Option "v" ["version"]  (NoArg (\_ -> do 
              argv0 <-getProgName
              hPutStrLn stderr (argv0 ++ " version 0.1")
              exitWith ExitSuccess)) "Print version"
          , Option "h" ["help"]     (NoArg (\_ ->do
              argv0 <-getProgName
              hPutStrLn stderr (usageInfo argv0 options)
              exitWith ExitSuccess)) "Show help" ]

main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <-foldl (>>=) (return defOpts) actions
    g <-getStdGen

    let p = mkPasswd g opts
    case (length (username opts) > 0) of
        True ->putStrLn $ "ALTER USER " ++ (username opts) ++ " IDENTIFIED BY " ++ p ++ ";"
        False ->putStrLn p

-- end of file
