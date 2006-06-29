module Main where

import Text.RSS

import Data.Maybe
import Network.URI
import System.IO.Unsafe
import System.Time

main = putStrLn rssTest

-- A simple static test feed
rssTest =   showXML $ rssToXML (RSS "my whiskers" 
				(fromJust (parseURI "http://www.n-heptane.com")) 
				"they are very pointy and luxurious" 
				[] 
				[[ Title "yea-haw"
				 , Link (fromJust (parseURI "http://www.n-heptane.com/"))
				 , Description "the best site ever!!!"
				 , Author "jeremy@n-heptane.com (Jeremy Shaw)"
				 , Category Nothing "meow"
				 , Enclosure (fromJust (parseURI "http://www.n-heptane.com/newpics/alice.gif")) 7333 "image/jpeg"
				 , Guid "whee babayyyy!"
				 , PubDate (unsafePerformIO (getClockTime >>= toCalendarTime))
				 , Source (fromJust (parseURI "http://www.google.com/")) "The best search engine eva!"
				]])
