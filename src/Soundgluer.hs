{-# LANGUAGE FlexibleContexts #-}

module Soundgluer where

import System.Exit
import System.Directory

import Control.Monad

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Array.IO
import Data.Array.Unboxed
import Data.Int

import Data.Audio
import Codec.Wav


wavFileError :: String
wavFileError = "wav file error: "

waveExtension :: FilePath
waveExtension = ".wav"

directorySeparator :: FilePath
directorySeparator = "/"

languagesPath :: FilePath
languagesPath = "lang" ++ directorySeparator



glueSpeech :: String -> [String] -> String-> IO ()
glueSpeech language phones fileName
        | null phones = return ()
        | otherwise   = do
            phoneAudioMap <- loadLanguageAudio language :: IO (Map.Map String (Audio Int32))
            print phoneAudioMap


loadLanguageAudio :: ( MArray IOUArray a IO
                     , IArray UArray a
                     , Audible a
                     , AudibleInWav a)
                     => String -> IO (Map.Map String (Audio a))
loadLanguageAudio language = do
    dirContents <- listDirectory languageDirectory
    phoneAudioList <- zip <$> (return . map phoneName) (filter isWave dirContents)
                          <*> forM (filter isWave dirContents) (getAudioData languageDirectory)
    return $ Map.fromList phoneAudioList
  where
    languageDirectory = getLanguageDirectory language

isWave :: FilePath -> Bool
isWave fileName = waveExtension == ( Text.unpack
                                   . Text.takeEnd (length waveExtension)
                                   . Text.pack
                                   $ fileName)

phoneName :: FilePath -> String
phoneName fileName = Text.unpack
                          . Text.dropEnd (length waveExtension)
                          . Text.pack
                          $ fileName

getLanguageDirectory :: String -> FilePath
getLanguageDirectory language = languagesPath ++ language ++ directorySeparator

getAudioData :: ( MArray IOUArray a IO
                , IArray UArray a
                , Audible a
                , AudibleInWav a)
                => FilePath -> FilePath -> IO (Audio a)
getAudioData languageDirectory fileName = do
    eitherWave <- importFile $ languageDirectory ++ fileName
    case eitherWave of
      (Left s) -> putStrLn (wavFileError ++ s) >> exitFailure
      (Right wave) -> return wave

