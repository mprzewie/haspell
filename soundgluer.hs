module Soundgluer (module Soundgluer) where

import Data.WAVE

glueSpeech :: String -> [String] -> IO ()
glueSpeech fileName phones
        | null phones = return ()
        | otherwise   = do
            waves <- mapM phoneToWAVE phones
            putWAVEFile fileName $ foldr1 concatWAVE waves
    
phoneToWAVE :: String -> IO WAVE
phoneToWAVE p =
    getWAVEFile $ "lang/pol/" ++ p ++ ".wav"

concatWAVE :: WAVE -> WAVE -> WAVE
concatWAVE w1 w2 = WAVE
                   { waveHeader = WAVEHeader
                                  { waveNumChannels = waveNumChannels . waveHeader $ w1
                                  , waveFrameRate = waveFrameRate . waveHeader $ w1
                                  , waveBitsPerSample = waveBitsPerSample . waveHeader $ w1
                                  , waveFrames = Just (getFrames w1 + getFrames w2)
                                  }
                   , waveSamples = waveSamples w1 ++ waveSamples w2
                   }
    
getFrames :: WAVE -> Int
getFrames w =
        case waveFrames . waveHeader $ w of
            Nothing -> 0 -- Nothing means the actual value is unknown, so setting 0 for convenience.
            Just x -> x
