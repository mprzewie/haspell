module Soundgluer (module Soundgluer) where

import System.Directory

import Control.Monad

import qualified Data.Map as M
import qualified Data.Text as T -- pack, unpack, takeEnd, dropEnd
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B

import Codec.Audio.Wave
import HspTypes (Phone)


-- | Extension of audio files representing phonems.
waveExtension :: FilePath
waveExtension = ".wav"

-- | Probably a naive reimplementation of some builtin.
pathSeparator :: FilePath
pathSeparator = "/"

-- -- | Name of the root languages directory.
-- langsDirectory :: FilePath
-- langsDirectory = "lang"

-- | Name of the folder containing standard phonems (like silence), which appear accross all languages.
stdVox :: FilePath
stdVox = "std"

-- | Name of the directory containing voice files
voxDirectory :: FilePath
voxDirectory = "vox"

-- | Path of the wave header used during generation of the output.
waveHeaderPath :: FilePath
waveHeaderPath = voxDirectory ++ pathSeparator ++ stdVox ++ pathSeparator ++ "header.wav"

-- | Converts a list of phonemized words into an audio representation in some voice and writes it to a file.
glueSpeech :: String        -- ^ Name of the voice. It should match the name of the voice folder in the voxDirectory.
           -> [[Phone]]     -- ^ List of phonemized words
           -> String        -- ^ Path of the output file
           -> IO ()
glueSpeech vox words filePath
        | null words = return ()
        | otherwise = do
            phoneAudioMap <- loadVoxAudio vox
            waveHeader <- readWaveFile waveHeaderPath
            let appendWord w1 w2 = w1 `mappend` (phoneAudioMap M.! "-") `mappend` w2
            let gluedSpeech = foldr appendWord (mempty :: B.Builder)
                            $ map mconcat
                            $ map (map (phoneAudioMap M.!)) words
            let phonesWriter = flip B.hPutBuilder gluedSpeech
            writeWaveFile (filePath ++ waveExtension) waveHeader phonesWriter


-- | Loads lazily phonems of a given voice into memory.
loadVoxAudio :: String                        -- ^ Language name with a matching folder in langsDirectory
              -> IO (M.Map Phone B.Builder)    -- ^ Map from phonem name to its audio data as a lazy ByteString Builder
loadVoxAudio vox =
    M.union <$> loadVoxAudio' vox <*> loadVoxAudio' stdVox
  where
    loadVoxAudio' vox = do
        let voxDirectory = getVoxPath vox
        dirWaves <- filter isWave <$> listDirectory voxDirectory
        phoneAudioList <- zip <$> (return $ map phoneName dirWaves)
                              <*> forM dirWaves (getAudioData voxDirectory)
        return $ M.fromList phoneAudioList


-- | Checks using extension if file has the WAV format.
isWave :: FilePath      -- ^ Path of the checked file
       -> Bool          -- ^ Is it a WAV?
isWave fileName = waveExtension == ( T.unpack
                                   . T.takeEnd (length waveExtension)
                                   . T.pack
                                   $ fileName)

-- | Extracts the phone name from a file name.
phoneName :: FilePath     -- ^ File name
          -> String       -- ^ Extracted phone name
phoneName fileName = T.unpack
                   . T.dropEnd (length waveExtension)
                   . T.pack
                   $ fileName

-- | Returns path to the directory of a specified voice.
getVoxPath :: String           -- ^ Name of the voice matching its folder name
            -> FilePath         -- ^ Path to a specific voice folder
getVoxPath vox = voxDirectory ++ pathSeparator ++ vox

-- | Loads into memory raw audio data without header from a single wave file and returns a Builder of a lazy ByteString.
getAudioData :: FilePath        -- ^ Voice directory as returned by getLangPath
             -> FilePath        -- ^ Name of a wave file to read
             -> IO B.Builder    -- ^ Lazy ByteString Builder
getAudioData voxDirectory fileName = do
    let wavePath = voxDirectory ++ pathSeparator ++ fileName
    waveMetadata <- readWaveFile wavePath
    waveData <- L.readFile wavePath
    let waveHeaderLength = fromIntegral $ waveDataOffset waveMetadata
    return $ B.lazyByteString
           $ L.drop waveHeaderLength waveData

-- | Utility for generating a wave file containing only a header and no audio data.
generateHeader :: IO ()
generateHeader = do
    a <- readWaveFile $ (getVoxPath stdVox) ++ pathSeparator ++ "-.wav"
    writeWaveFile waveHeaderPath a (\h -> return ())
