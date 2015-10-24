module Statsd.Utils where

import Data.List (nubBy)

{-
function sanitizeKeyName(key) {
  if (keyNameSanitize) {
    return key.replace(/\s+/g, '_')
              .replace(/\//g, '-')
              .replace(/[^a-zA-Z_\-0-9\.]/g, '');
  } else {
    return key;
  }
}
-}

allowedChars :: String
allowedChars = "_-." ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

sanitizeKeyName :: String -> String
sanitizeKeyName =  stripInvalidChars . replaceSpaceSlash . squashSpaces
    where squashSpaces = nubBy (\x c -> c == ' ' && x == c)
          replaceSpaceSlash = map (\c -> if c == ' ' then '_' else if c == '/' then '-' else c)
          stripInvalidChars = filter (`elem` allowedChars)
