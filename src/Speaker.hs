module Speaker (module Speaker) where

import HspInterpreter(Phone)
import Phonemizer (phonemize)
import Soundgluer (glueSpeech, waveExtension)
import System.Process(callProcess)

-- | Plays the phonems in the specified language
speak 	:: String 		-- ^ Name of the language. It should match the name of the language folder in the langsDirectory.
		-> [[Phone]] 	-- ^ List of lists of Phonems to speak
		-> IO()			
speak lang (word:rest) =  do
	speakWord lang word
	playFile (emptyWave)
	speak lang rest
speak _ [] = do return ()

-- | Creates a .tmp file with given word (list of phonems) and plays it in given language
speakWord 	:: String 		-- ^ Name of the language. It should match the name of the language folder in the langsDirectory.
			-> [Phone] 		-- ^ List of phonems (word) to speak
			-> IO()
speakWord lang word = do
	glueSpeech lang [word] tmpFileName
	playFile tmpFileName

-- | Path to the "silent" waveFile
emptyWave :: FilePath
emptyWave = "lang/std/-"

-- | Name of the .tmp file which w
tmpFileName :: FilePath
tmpFileName = ".tmp"

-- | Plays the given wave file
playFile :: String -> IO()
playFile file = callProcess "play" [file ++ waveExtension]
