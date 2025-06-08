--------------------------------------------------------------------------
-- Test complete programs using input/output files
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codex.Tester.InputOutput (
  Build,
  clangBuild,
  pythonBuild,
  javaBuild,
  haskellBuild,
  stdioTester,
  ) where

import           Codex.Tester
import           Codex.Types (Language)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Data.Char (isSpace)
import           Control.Exception(handle)
import           System.Directory(copyFile)


import qualified Text.Pandoc.Builder as P

--
-- | build and run scripts for testing standalone programs;
-- the type for executables is existentially quantified (hidden);
-- this generality isn't used at the moment
--
data Build =
  forall exec.
  Build { checkLanguage :: Language -> Bool,
          makeExec :: FilePath -> Code -> IO exec,
          runExec  :: FilePath
                   -> Maybe FilePath
                   -> [Text]
                   -> Text
                   -> IO (ExitCode, Text, Text)
        }

runExec :: FilePath -> Maybe FilePath -> [Text] -> Text -> IO (ExitCode, Text, Text)
runExec exe_file workingDir args stdin = do
  let nsjailCmd = "nsjail --config /path/to/nsjail.cfg --"
  let fullCommand = nsjailCmd ++ " " ++ exe_file
  (exitCode, stdout, stderr) <- readProcessWithExitCode fullCommand (map T.unpack args) (T.unpack stdin)
  return (exitCode, T.pack stdout, T.pack stderr)

-- | builder for C programs
--
clangBuild :: Tester Build
clangBuild = do
  cc_cmd <- configured "language.c.compiler"
  limits <- configLimits "language.c.limits"
  cc:cc_args <- parseArgs cc_cmd
  let make tmpdir (Code _ code) = do
        let c_file = tmpdir </> "submit.c"
        let exe_file = tmpdir </> "submit"
        T.writeFile c_file code
        chmod (executable . readable . writeable) tmpdir
        chmod readable c_file
        runProcess (Just limits) cc (cc_args ++ [c_file, "-o", exe_file])
        return exe_file

  let run exe_file dir args stdin = do
  	let nsjailCmd = "nsjail --config /path/to/nsjail.cfg --"
  	let fullCommand = T.pack nsjailCmd <> T.pack exe_file
  	safeExec limits fullCommand dir args stdin  
  return (Build (=="c") make run)


-- | builder for Python programs
--
pythonBuild ::  Tester Build
pythonBuild = do
  python <- configured "language.python.interpreter"
  limits <- configLimits "language.python.limits"
  let make tmpdir (Code _ code)  = do
        let pyfile = tmpdir </> "submit.py"
        T.writeFile pyfile code
        chmod (readable . executable) tmpdir
        chmod readable pyfile
        return pyfile
  let run pyfile dir stdin = do
        safeExec limits python dir [pyfile] stdin
  return (Build (=="python") make run)

-- | builder for Java programs
--
javaBuild :: Tester Build
javaBuild = do
  javac_cmd <- configured "language.java.compiler"
  java_cmd <- configured "language.java.runtime"
  limits <- configLimits "language.java.limits"
  -- name for public java class with the main method
  classname <- fromMaybe "Main" <$> metadata "java-main"
  javac:javac_args <- parseArgs javac_cmd
  java:java_args <- parseArgs java_cmd
  let make tmpdir (Code _ code) = do
        let java_file = tmpdir </> classname <.> "java"
        let classfile = tmpdir </> classname <.> "class"
        T.writeFile java_file code
        chmod (executable . readable . writeable) tmpdir
        chmod readable java_file
        runProcess (Just limits) javac (javac_args ++ [java_file])
        return classfile
  let run classfile cwd stdin = do
        let classpath = takeDirectory classfile
        let args' = java_args ++ ["-cp", classpath, classname] 
        safeExec limits java cwd args' stdin
  return (Build (=="java") make run)


-- builder for Haskell programs
haskellBuild :: Tester Build
haskellBuild = do
  ghc_cmd <- configured "language.haskell.compiler"
  limits <- configLimits "language.haskell.limits"
  -- name for the Haskell main module
  modname <- fromMaybe "Main" <$> metadata "haskell-main"
  ghc:ghc_args <- parseArgs ghc_cmd
  let make tmpdir (Code _ code) = do
        let hs_file = tmpdir </> modname <.> "hs"
        let exe_file = tmpdir </> modname
        T.writeFile hs_file code
        chmod (readable . writeable . executable) tmpdir
        chmod readable hs_file
        runProcess (Just limits) ghc (ghc_args ++ [hs_file, "-o", exe_file])
        return exe_file
  let run exe_file dir stdin = do
        safeExec limits exe_file dir [] stdin
  return (Build (=="haskell") make run)



-- | parameterized I/O tester 
--
stdioTester ::  Build -> Tester Result
stdioTester Build{..} = tester "stdio" $ do
  code@(Code lang _) <- testCode
  guard (checkLanguage lang)
  dir <- takeDirectory <$> testFilePath
  -- input and output files
  pub_ins <-  globPatterns dir =<<
              metadataWithDefault "public-inputs" []
  pub_outs <- globPatterns dir =<<
              metadataWithDefault "public-outputs" [] 
  let num_ins = length pub_ins
  let num_outs = length pub_outs
  assert (pure $ num_ins == num_outs)
    "different number of public inputs and outputs"

  priv_ins <- globPatterns dir =<<
                 metadataWithDefault "private-inputs" []
  priv_outs <- globPatterns dir =<<
                  metadataWithDefault "private-outputs" []
  let num_priv_ins = length priv_ins
  let num_priv_outs = length priv_outs
  assert (pure $ num_priv_ins == num_priv_outs) 
     "different number of private inputs and outputs"
  assert (pure $ num_ins + num_priv_ins > 0)
    "no test cases defined"
  --- extra files to copy to temp directory
  files <- globPatterns dir =<< metadataWithDefault "files" []
  -- run the test and agregate results
  liftIO $
    withTempDir "codex" $ \tmpdir -> handle compileErrorHandler $ do
        -- make temp directory readable, writable and executable for user 
        chmod (readable . writeable . executable) tmpdir
        -- copy extra files
        mapM_ (\f -> copyFile f (tmpdir </> takeFileName f)) files
        -- make executable
        exe_file <- makeExec tmpdir code
        -- run public and private tests
        r1 <- runTests (runExec exe_file (Just tmpdir)) $
                          zip pub_ins pub_outs
        r2 <- runTests (runExec exe_file (Just tmpdir)) $
                          zip priv_ins priv_outs
        return (tagWith Public r1 <> tagWith Private r2)


runTests ::
  ([Text] -> Text -> IO (ExitCode, Text, Text)) ->
  [(Maybe FilePath, FilePath, FilePath)] ->
  -- ^ optional file with cmdline args, input, output
  IO Result
runTests action tests =
  loop 1 tests
  where
    total = length tests
    loop _ [] =
      return $ accepted $ "Passed " <> T.pack (show total) <> " tests"
    loop n ((opt_arg_file, in_file, out_file) : tests) = do
      in_txt <- T.readFile in_file
      out_txt <- T.readFile out_file
      arg_str <- maybe (return "") readFile opt_arg_file
      args <- map T.pack <$> parseArgs arg_str

      result <- classify in_txt out_txt <$> action args in_txt
      if resultStatus result == Accepted then
        loop (n+1) tests
      else
        return (numberResult n total arg_str result)

          
numberResult :: Int -> Int -> String -> Result -> Result
numberResult num total args Result{..}
  = Result resultStatus (test <> resultReport)
  where test
          = T.pack (printf "*** Test %d / %d ***\n\n" num total) <>
          "Command-line arguments:\n" <> T.pack args <> "\n"
          


-- run a single test case
runTest :: (Text -> IO (ExitCode, Text, Text))
        -> FilePath
        -> FilePath
        -> IO Result
runTest action in_file out_file = do
      in_txt <- T.readFile in_file
      out_txt <- T.readFile out_file
      classify in_txt out_txt <$> action in_txt
    

classify :: Text -> Text -> (ExitCode, Text, Text) -> Result
classify input expected (ExitSuccess, out, _)
  | out == expected 
      = accepted (P.plain $ P.str "OK")
  | removeSpaces out == removeSpaces expected 
      = presentationError $ textDiff expected input out
  | otherwise 
      = wrongAnswer $ textDiff expected input out
classify input _ (ExitFailure c, _, err)
  | match "Time Limit" err   
      = timeLimitExceeded $ textInput input
  | match "Memory Limit" err 
      = memoryLimitExceeded $ textInput input
  | match "Output Limit" err 
      = runtimeError (textInput input <> P.codeBlock err)
  | otherwise 
      = let msg = "Program exited with non-zero status: " <> T.pack (show c)
        in runtimeError  (textInput input <> 
                          P.plain (P.str msg) <>
                          P.codeBlock err)
                
removeSpaces :: Text -> Text
removeSpaces = T.filter (not.isSpace)

textInput :: Text -> P.Blocks
textInput input  
  = P.simpleTable []   
    [ [P.plain $ P.strong $ P.str "Input", P.codeBlock input]
    ]

textDiff :: Text -> Text -> Text -> P.Blocks
textDiff expected input out =
    P.simpleTable []
    [ [P.plain $ P.strong $ P.str "Input", P.codeBlock input]
    , [P.plain $ P.strong $ P.str "Expected Output", P.codeBlock expected]
    -- , [P.plain $ P.strong $ P.str "Obtained Output", P.codeBlock out]
    , [P.plain $ P.strong $ P.str "Obtained Output", 
       P.divWith ("", ["text-diffs"],[]) $ P.plain $ formatDiffs out expected]
    ]


