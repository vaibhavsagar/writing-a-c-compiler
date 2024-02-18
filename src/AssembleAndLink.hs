module AssembleAndLink where

import System.FilePath
import System.Process

assembleAndLink :: FilePath -> IO FilePath
assembleAndLink inputFilePath = do
    let outputFilePath = replaceFileName inputFilePath (takeBaseName inputFilePath)
    callProcess "gcc" [inputFilePath, "-o", outputFilePath]
    pure outputFilePath
