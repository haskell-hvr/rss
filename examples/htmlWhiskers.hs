module Main where

import Text.RSS

import Data.Maybe
import Network.URI
import System.IO.Unsafe
import Data.Time.Clock

main = putStrLn . showXML . rssToXML . rss =<< getCurrentTime

rss :: UTCTime -> RSS
rss t = RSS "my whiskers"
            (fromJust (parseURI "http://www.n-heptane.com"))
            "they are very pointy and luxurious"
            []
            [[ Title "yea-haw"
             , Link (fromJust (parseURI "http://www.n-heptane.com/"))
             , Description "the best site ever!!!"
             , ContentEncoded "the <b>best</b> site ever!!!"
             , Author "jeremy@n-heptane.com (Jeremy Shaw)"
             , Category Nothing "meow"
             , Enclosure (fromJust (parseURI "http://www.n-heptane.com/newpics/alice.gif")) 7333 "image/jpeg"
             , Guid True "whee babayyyy!"
             , PubDate t
             , Source (fromJust (parseURI "http://www.google.com/")) "The best search engine eva!"
             ]
            ]
