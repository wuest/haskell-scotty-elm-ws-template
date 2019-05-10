{-# LANGUAGE OverloadedStrings #-}

module Opts ( Options
            , getOpts
            , optVerbose
            , webPort
            , baseDir
            ) where
 
import Prelude
import System.Console.GetOpt
import System.Environment    (getProgName, getArgs)
import System.Exit           (exitSuccess)
import System.IO             (hPutStrLn, stderr)

import qualified System.Directory as Dir

import qualified Const

data Options = Options { optVerbose :: Bool
                       , webPort    :: Int
                       , baseDir    :: IO FilePath
                       }

defaultOptions :: Options
defaultOptions =
    Options { optVerbose = False
            , webPort    = 3000
            , baseDir    = Dir.getXdgDirectory Dir.XdgData Const.applicationName
            }

printVersion :: Options -> IO Options
printVersion _ = do
    hPutStrLn stderr $ Const.applicationName ++ " " ++ Const.version
    exitSuccess

printHelp :: Options -> IO Options
printHelp _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

baseDirLocation :: FilePath -> Options -> IO Options
baseDirLocation arg opt = return opt { baseDir = Dir.makeAbsolute arg }

verbose :: Options -> IO Options
verbose opt = return opt { optVerbose = True }

setWebPort :: String -> Options -> IO Options
setWebPort arg opt = return opt { webPort = read arg :: Int }

blank :: OptDescr (Options -> IO Options)
blank = Option [] [] (NoArg return) ""

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['v'] ["verbose"]
        (NoArg verbose) "Enable verbose messages"

    , Option ['V'] ["version"]
        (NoArg printVersion) "Print version"

    , Option ['h', '?'] ["help"]
        (NoArg printHelp) "Show help"

    , blank

    , Option ['b'] ["base"]
        (ReqArg baseDirLocation "DATABASE") "Base asset location"

    , Option ['p'] ["webport"]
        (ReqArg setWebPort "PORT") "Port for the webserver to run on"
    ]

getOpts :: IO Options
getOpts = do
    args <- getArgs
    let (actions, _nonoptions, _errors) = getOpt RequireOrder options args
    foldl (>>=) (return defaultOptions) actions
