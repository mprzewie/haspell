module Speaker (module Speaker) where

import HspInterpreter(Phone)
import Phonemizer (phonemize)
import Soundgluer (glueSpeech, waveExtension)
import System.Process(callProcess)

speak :: String -> [[Phone]] -> IO()
speak lang (word:rest) =  do
	speakWord lang word
	playFile (emptyWave)
	speak lang rest
speak _ [] = do return ()

speakWord :: String -> [Phone] -> IO()
speakWord lang word = do
	glueSpeech lang [word] tmpFileName
	playFile tmpFileName

emptyWave :: FilePath
emptyWave = "lang/std/-"

tmpFileName :: FilePath
tmpFileName = ".tmp"

playFile :: String -> IO()
playFile file = callProcess "play" [file ++ waveExtension]

rmFile :: String -> IO()
rmFile file = callProcess "rm" [file]
