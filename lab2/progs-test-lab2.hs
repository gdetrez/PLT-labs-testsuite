{-# OPTIONS_GHC -cpp #-}

-- GHC needs -threaded

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.IO.Unsafe
import System.Console.GetOpt
    

-- Executable name
-- TODO: make an option
--EXEC_NAME :: String
executable_name = "lab2"


{-# NOINLINE testsPath #-}
testsPath :: IORef String
testsPath = unsafePerformIO $ newIORef "."

{-# NOINLINE useColors #-}
useColors :: IORef Bool
useColors = unsafePerformIO $ newIORef True

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

debug :: String -> IO ()
debug s = do d <- readIORef doDebug
             if d then putStrLn s else return ()

listGoodProgs :: IO [String]
listGoodProgs = listCCFiles "good"

listBadProgs :: IO [String]
listBadProgs = listCCFiles "bad"

listCCFiles :: String -> IO [String]
listCCFiles dir = do
  dir' <-  readIORef testsPath >>= return . joinPath . (:[dir])
  print dir'
  liftM (map (\f -> joinPath [dir',f]) . sort . filter ((=="cc") . getExt)) $
    getDirectoryContents dir'

welcome :: IO ()
welcome = do putStrLn $ "This is the test program for Programming Languages Lab 2"

runMake :: FilePath -> IO ()
runMake dir = do checkDirectoryExists dir
                 runCommandNoFail_ ("make -C " ++ quote dir) ""

runTests :: FilePath -> IO ([Bool],[Bool])
runTests dir = 
    do let prog = joinPath [dir,executable_name]
       checkFileExists prog
       goodProgs <- listGoodProgs
       badProgs  <- listBadProgs
       good <- mapM (testBackendProg prog) goodProgs
       bad  <- mapM (testBadProgram prog) badProgs
       return (good,bad)


testBackendProg :: FilePath -> FilePath -> IO Bool
testBackendProg prog f =
    do input  <- readFileIfExists (f++".input")
       output <- readFileIfExists (f++".output")
       let c = prog ++ " " ++ f
       putStrLn $ "Running " ++ f ++ "..."
       (out,err,s) <- runCommandStrWait c input
       debug $ "Exit code: " ++ show s
       case s of
         ExitFailure n -> do
           reportError c ("Program stopped with code " ++ show n) \
             f input out err
           return False
         ExitSuccess | out == output -> return True
         ExitSuccess | otherwise -> do
           reportError c "invalid output" f input out err
           putStrLn "Expected output:"
           cPutStrLn blue output
           return False

testBadProgram :: FilePath -> FilePath -> IO Bool
testBadProgram prog f =
    do input  <- readFileIfExists (f++".input")
       output <- readFileIfExists (f++".output")
       let c = prog ++ " " ++ f
       putStrLn $ "Running " ++ f ++ "..."
       (out,err,s) <- runCommandStrWait c input
       debug $ "Exit code: " ++ show s
       case lines out of
           "TYPE ERROR":_ -> return True
           "SYNTAX ERROR":_ -> return True
           _ -> do reportError c "Passed bad program" f "" out err
                   return False

--
-- * Main
--

parseArgs :: [String] -> IO String
-- parseArgs ["-debug",cfFile] = 
--    do writeIORef doDebug True
--       return cfFile
--
-- parseArgs [cfFile] = return cfFile
-- parseArgs _ = usage
parseArgs argv = case getOpt Permute flags argv of
  (args,[pathToLab],[]) -> do
    sequence args
    return pathToLab
  (_,_,errs)      -> do
    hPutStrLn stderr $ concat errs
    usage
    exitWith (ExitFailure 1)

usage :: IO a
usage = do
  hPutStrLn stderr $
    usageInfo header flags   
  exitFailure
  where header  ="Usage: progs-test-lab3 <interpreter code directory>"

mainOpts :: FilePath -> IO ()
mainOpts dir = 
    do welcome
       runMake dir
       (good,bad) <- runTests dir
       putStrLn ""
       putStrLn "------------------------------------------------------------"
       report "Good programs: " good
       report "Bad programs:  " bad


flags =
  [Option ['d'] ["debug"]       (NoArg $ writeIORef doDebug True)
   "Print debug informations."
  ,Option ['p'] ["testspath"]       (ReqArg (writeIORef testsPath) "PATH")
   "Do not use colors in the output"
  ,Option [] ["no-colors"]       (NoArg $ writeIORef useColors False)
   "Do not use colors in the output"
  ,Option []    ["help"] (NoArg usage)
  "Print this help message"
  ]

main :: IO ()
main = getArgs >>= parseArgs >>= mainOpts

--
-- * List utilities
--

grep :: String -> String -> [String]
grep x = filter (x `isSubStringOf`) . lines
  where isSubStringOf x = any (x `isPrefixOf`) . tails

--
-- * Path name utilities
--

getExt :: FilePath -> String
getExt = reverse . takeWhile (/='.') . reverse

stripExt :: FilePath -> String
stripExt p = if '.' `elem` p then p' else p
  where p' = reverse $ drop 1 $ dropWhile (/='.') $ reverse p

basename :: FilePath -> FilePath
basename = reverse . takeWhile (not . isPathSep) . reverse

isPathSep :: Char -> Bool
isPathSep c = c == pathSep

joinPath :: [String] -> FilePath
joinPath = concat . intersperse [pathSep]

pathSep :: Char
#if defined(mingw32_HOST_OS)
pathSep = '\\'
#else
pathSep = '/'
#endif

quote :: FilePath -> FilePath
quote p = "'" ++ concatMap f p ++ "'"
  where 
    f '\'' = "\\'"
    f c = [c]

--
-- * Either utilities
--

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

fromLeft :: Either a b -> a
fromLeft =  either id (error "fromLeft: Right")

catLefts :: [Either a b] -> [a]
catLefts xs = [x | Left x <- xs]

--
-- * Terminal output colors
--

type Color = Int

highlight = "\ESC[7m"
bold      = "\ESC[1m"
underline = "\ESC[4m"
normal    = "\ESC[0m"
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"
bgcol col = "\ESC[0" ++ show (40+col) ++ "m"

red, green, blue, black :: Color
black = 0
red = 1
green = 2
blue = 6

cPutStrLn :: Color -> String -> IO ()
cPutStrLn c s = do
  d <- readIORef useColors
  putStrLn $ if d then color c s else s
  where color :: Color -> String -> String
        color c s = fgcol c ++ s ++ normal


--
-- * Various versions of runCommand
--

runCommandStr :: String -- ^ command
	      -> String -- ^ stdin data
	      -> IO (String,String,ProcessHandle) -- ^ stdout, stderr, process
runCommandStr c inStr = 
    do
    outVar <- newEmptyMVar
    errVar <- newEmptyMVar
    (pin,pout,perr,p) <- runInteractiveCommand c
    forkIO $ do debug "Writing input..."
                hPutStr pin inStr
                hClose pin
                debug "Wrote input."
    forkIO $ do debug "Reading output..."
                s <- hGetContents pout
                putMVar outVar s
                debug "Read output."
    forkIO $ do debug "Reading error..."
                s <- hGetContents perr
                putMVar errVar s
                debug "Read error."
    out <- takeMVar outVar
    err <- takeMVar errVar
    return (out,err,p)


runCommandStrWait :: String -- ^ command
		  -> String -- ^ stdin data
		  -> IO (String,String,ExitCode) -- ^ stdout, stderr, process exit status
runCommandStrWait c inStr =
    do
    debug $ "Running " ++ c
    (out,err,p) <- runCommandStr c inStr
    s <- waitForProcess p
    debug $ "Standard output:\n" ++ out
    debug $ "Standard error:\n" ++ err
    return (out,err,s)

runCommandNoFail_ :: String -- ^ Command
                  -> FilePath -- ^ Input file
                  -> IO ()
runCommandNoFail_ c f = runCommandNoFail c f >> return ()

runCommandNoFail :: String -- ^ Command
                 -> FilePath -- ^ Input file
                 -> IO (String,String) -- ^ stdout and stderr
runCommandNoFail e f = 
    do
    let c = e ++ " " ++ f
    hPutStrLn stderr $ "Running " ++ c ++ "..."
    (out,err,s) <- runCommandStrWait c ""
    case s of
	   ExitFailure x -> do
			    reportError e ("with status " ++ show x) f "" out err
			    exitFailure
	   ExitSuccess -> return (out,err)

--
-- * Checking files and directories
--

checkFileExists :: FilePath -> IO ()
checkFileExists f =
    do e <- doesFileExist f
       when (not e) $ do cPutStrLn red $ quote f ++ " is not an existing file."
		         exitFailure

checkDirectoryExists :: FilePath -> IO ()
checkDirectoryExists f =
    do e <- doesDirectoryExist f
       when (not e) $ do cPutStrLn red $ quote f ++ " is not an existing directory."
		         exitFailure

readFileIfExists :: FilePath -> IO String
readFileIfExists f = catch (readFile f) (\_ -> return "")

--
-- * Error reporting and output checking
--

reportErrorColor :: Color 
                 -> String -- ^ command that failed
	         -> String -- ^ how it failed
	         -> FilePath -- ^ source file
	         -> String -- ^ given input
	         -> String -- ^ stdout output
	         -> String -- ^ stderr output
	         -> IO ()
reportErrorColor col c m f i o e =
    do
    cPutStrLn col $ c ++ " failed: " ++ m
    when (not (null f)) $ prFile f
    when (not (null i)) $ do
			  putStrLn "Given this input:"
			  cPutStrLn blue i
    when (not (null o)) $ do
			  putStrLn "It printed this to standard output:"
			  cPutStrLn blue o
    when (not (null e)) $ do
			  putStrLn "It printed this to standard error:"
			  cPutStrLn blue e

reportError :: String -- ^ command that failed
	    -> String -- ^ how it failed
	    -> FilePath -- ^ source file
	    -> String -- ^ given input
	    -> String -- ^ stdout output
	    -> String -- ^ stderr output
	    -> IO ()
reportError = reportErrorColor red

prFile :: FilePath -> IO ()
prFile f = do
           e <- doesFileExist f           
           when e $ do putStrLn $ "For input file " ++ f ++ ":"
	               putStrLn $ "---------------- begin " ++ f ++ " ------------------" 
	               s <- readFile f
	               cPutStrLn green s
	               putStrLn $ "----------------- end " ++ f ++ " -------------------" 


-- | Report how many tests passed.
report :: String -> [Bool] -> IO ()
report n rs = 
  do let (p,t) = (length (filter id rs), length rs)
         c = if p == t then green else red
     cPutStrLn c $ 
              n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
