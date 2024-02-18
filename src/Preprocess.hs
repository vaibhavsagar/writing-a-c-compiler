module Preprocess where

import System.FilePath
import System.Process

preprocess :: FilePath -> IO FilePath
preprocess inputFilePath = do
    let outputFilePath = replaceFileName inputFilePath (takeBaseName inputFilePath ++ ".i")
    callProcess "gcc" ["-E", "-P", inputFilePath, "-o",  outputFilePath]
    pure outputFilePath
