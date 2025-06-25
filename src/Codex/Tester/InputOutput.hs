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

import           Data.Char (isSpace, isControl)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Text.Printf

import           Control.Exception (catch, SomeException)
import           System.Directory (copyFile)
import           System.Process (readProcessWithExitCode)

import qualified Text.Pandoc.Builder as B



-- | build and run scripts for testing standalone programs
data Build =
  forall exec.
  Build { checkLanguage :: Language -> Bool,
          makeExec :: FilePath -> Code -> IO exec,
          runExec  :: exec
                   -> Maybe FilePath
                   -> [Text]
                   -> Text
                   -> IO (ExitCode, Text, Text)
        }

-- | NsJail wrapper for running executables securely
nsjailExec :: FilePath -> Maybe FilePath -> [Text] -> Text -> IO (ExitCode, Text, Text)
nsjailExec exe_file _ args stdin = do
  let nsjailCmd = "nsjail --config /path/to/nsjail.cfg -- " ++ exe_file
  (exitCode, stdout, stderr) <- readProcessWithExitCode "sh" ["-c", nsjailCmd ++ " " ++ unwords (map T.unpack args)] (T.unpack stdin)
  return (exitCode, T.pack stdout, T.pack stderr)

-- | builder for C programs
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
  return (Build (=="c") make nsjailExec)

-- | builder for Python programs
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
  let run pyfile dir args stdin =
        safeExec limits python dir (pyfile : map T.unpack args) stdin
  return (Build (=="python") make run)

-- | builder for Java programs
javaBuild :: Tester Build
javaBuild = do
  javac_cmd <- configured "language.java.compiler"
  java_cmd <- configured "language.java.runtime"
  limits <- configLimits "language.java.limits"
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
  let run classfile cwd args stdin = do
        let classpath = takeDirectory classfile
        let args' = map T.unpack $
              map T.pack java_args ++ ["-cp", T.pack classpath, T.pack classname] ++ args
        safeExec limits java cwd args' stdin
  return (Build (=="java") make run)

-- | builder for Haskell programs
haskellBuild :: Tester Build
haskellBuild = do
  ghc_cmd <- configured "language.haskell.compiler"
  limits <- configLimits "language.haskell.limits"
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
  return (Build (=="haskell") make nsjailExec)

-- | parametrized I/O tester 
stdioTester :: Build -> Tester Result
stdioTester Build{..} = tester "stdio" $ do
  code@(Code lang _) <- testCode
  guard (checkLanguage lang)
  dir <- takeDirectory <$> testFilePath
  inputs <- globPatterns dir =<< metadataWithDefault "inputs" []
  outputs <- globPatterns dir =<< metadataWithDefault "outputs" []
  let num_inputs = length inputs
      num_outs   = length outputs
  assert (pure $ num_inputs == num_outs) "different number of inputs and outputs"
  assert (pure $ num_inputs /= 0) "no test cases defined"
  argfiles <- globPatterns dir =<< metadataWithDefault "arguments" []
  let argfiles' = map Just argfiles ++ repeat Nothing
  files <- globPatterns dir =<< metadataWithDefault "files" []
  liftIO $
    (withTempDir "codex" $ \tmpdir -> do
        chmod (readable . writeable . executable) tmpdir
        mapM_ (\f -> copyFile f (tmpdir </> takeFileName f)) files
        exe_file <- makeExec tmpdir code
        runTests (runExec exe_file (Just tmpdir)) $ zip3 argfiles' inputs outputs)
    `catch` (\(e :: SomeException) -> return $ runtimeError $ B.para $ B.text $ T.pack (show e))


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
      return $ accepted $ B.para $
        B.text "Passed " <> B.text (T.pack $ show total) <> B.text " tests"
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
numberResult num total args Result{..} =
  Result resultStatus (Report test <> resultReport)
  where
    test =
      B.para (B.strong (B.text "*** Test "
                        <> B.text (T.pack $ show num)
                        <> B.text " / "
                        <> B.text (T.pack $ show total)
                        <> B.text " ***"))
      <> B.para (B.text "Command-line arguments:")
      <> B.para (B.text (T.pack args))
          


classify :: Text -> Text -> (ExitCode, Text, Text) -> Result
classify input expected (ExitSuccess, out, _) 
  | T.strip out == T.strip expected =
      accepted $ B.para $ B.text "OK"
  | otherwise =
      wrongAnswer $ B.para $ B.text $ textDiff expected input out
classify input _ (ExitFailure c, _, err)
  | match "Time Limit" err =
      timeLimitExceeded $ B.para $ B.text $ textInput input
  | match "Memory Limit" err =
      memoryLimitExceeded $ B.para $ B.text $ textInput input
  | match "Output Limit" err =
      runtimeError $ B.para $ B.text $ T.unlines [textInput input, err]
  | otherwise =
      runtimeError $ B.para $ B.text $ T.unlines
        [ textInput input
        , ""
        , "Program exited with non-zero status: " <> T.pack (show c)
        , err
        ]



textInput :: Text -> Text
textInput input =  "Input:\n" <> sanitize input

textDiff :: Text -> Text -> Text -> Text
textDiff expected input out =
    T.unlines
    [ "Input:", sanitize input
    , "Expected output:", sanitize expected
    , "Obtained output:", sanitize out
    ]

-- | sanitize output text from student's submission;
-- replace spaces with a visible space;
-- replace control chars with the invalid UTF symbol
sanitize :: Text -> Text
sanitize = T.map (\c -> case c of
                     ' ' -> '\x2423'
                     _ -> if isControl c && not (isSpace c)
                          then '\xfffd' else c)
