module Haspell (module Haspell) where

--for translating texts into files!

import Phonemizer (phonemize)
import Soundgluer (glueSpeech)


spell :: String -> String -> String -> IO()
spell lang txt filename = (phonemize lang txt) >>= \phones -> (glueSpeech filename phones)

bitbox :: String -> IO()
bitbox txt = spell "pol" txt "bitbox"




