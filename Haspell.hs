module Haspell (module Haspell) where

--for translating texts into files!

import Phonemizer (phonemizeCMD)
import Soundgluer (glueSpeech)


spell :: String -> String -> String -> IO()
spell lang txt filename = (phonemizeCMD lang txt) >>= \phones -> (glueSpeech filename phones)




