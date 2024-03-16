-----------------------------------------------------------------------------
-- |
-- Module      :  Text.RSS
-- Copyright   :  Copyright 2004, Jeremy Shaw, http://www.n-heptane.com/
--                Copyright 2004-2006, Bjorn Bringert (bjorn@bringert.net)
-- License     :  This code is released to the public domain and comes
--                with no warranty.
--
-- Maintainer  :  Andreas Abel
-- Stability   :  stable
-- Portability :  portable
--
-- A libary for generating RSS 2.0 feeds.
--
--
-- Original module by Jeremy Shaw.
--
-- Changes by Bjorn Bringert:
--
-- * showXml just converts the RSS to a String, does not print it.
--
-- * Added XML escaping.
--
-- * Use RFC 2822 format for dates.
--
-- * Added all elements from RSS 2.0.1-rv-6,
--   <http://www.rssboard.org/rss-2-0-1-rv-6>
--
-- * Use HaXml.Verbatim instead of HaXml.Pretty, since
--   HaXml.Pretty seems to introduce spaces around entities.
--
-- * Removed the use of content:encoded, since the description
--   tag is the recommented way to include HTML content in RSS 2.0.
--
-- Changes by Bas van Dijk:
--
-- * Use @UTCTime@ from @time@ instead of @CalendarTime@ from @old-time@.
--
-- * Add our own @Weekday@ type instead of using the @Day@ type from @old-time@.
--
-----------------------------------------------------------------------------
module Text.RSS (RSS(..), Item, ChannelElem(..), ItemElem(..),
                 Title,Link,Description,Width,Height,
                 Email,Domain,MIME_Type,InputName,
                 Weekday(..), Hour, Minutes,
                 CloudHost, CloudPort, CloudPath,
                 CloudProcedure, CloudProtocol(..),
                 rssToXML, showXML
                ) where

import           Data.Ix                    (Ix)

import           Network.URI                (URI)

import           Data.Time.Format           (defaultTimeLocale,
                                             rfc822DateFormat)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (formatTime)

import           Text.XML.HaXml.Combinators (CFilter, cdata, literal, mkElem,
                                             mkElemAttr)
import           Text.XML.HaXml.Escape      (stdXmlEscaper, xmlEscape)
import           Text.XML.HaXml.Types       (Content (..), Element)
import           Text.XML.HaXml.Verbatim    (verbatim)


data RSS = RSS Title Link Description [ChannelElem] [Item]
         deriving Show

type Item = [ItemElem]

type Title = String
type Link = URI
type Description = String
type Width = Int
type Height = Int
type Email = String
type Domain = String
type MIME_Type = String
type InputName = String
type Hour = Int
type Minutes = Int

type CloudHost = String
type CloudPort = Int
type CloudPath = String
type CloudProcedure = String
data CloudProtocol = CloudProtocolXmlRpc | CloudProtocolSOAP
                     deriving Show

data ChannelElem = Language String
                 | Copyright String
                 | ManagingEditor Email
                 | WebMaster Email
                 | ChannelPubDate UTCTime
                 | LastBuildDate UTCTime
                 | ChannelCategory (Maybe Domain) String
                 | Generator String
                 -- no docs tag, we generate that automatically
                 | Cloud CloudHost CloudPort CloudPath CloudProcedure CloudProtocol
                 | TTL Minutes
                 | Image URI Title Link (Maybe Width) (Maybe Height) (Maybe Description)
                 | Rating String
                 | TextInput Title Description InputName Link
                 | SkipHours [Hour]
                 | SkipDays [Weekday]
                   deriving Show

data ItemElem = Title Title
              | Link Link
              | Description Description
              | Author Email
              | Category (Maybe Domain) String
              | Comments URI
              | Enclosure URI Int MIME_Type
              | Guid Bool String
              | PubDate UTCTime
              | Source URI Title
                deriving (Show)

-- | A day of the week.
data Weekday = Sunday   | Monday | Tuesday | Wednesday
             | Thursday | Friday | Saturday
               deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-- | Converts RSS to XML.
rssToXML :: RSS -> CFilter ()
rssToXML (RSS title link description celems items) =
    mkElemAttr "rss" [("version",literal "2.0")]
                     [mkElem "channel" ([mkTitle title,
                                         mkLink link,
                                         mkDescription description,
                                         mkDocs]
                                        ++ map mkChannelElem celems
                                        ++ map mkItem items)]

-- | Render XML as a string.
showXML :: CFilter () -> String
showXML = verbatim . cfilterToElem

cfilterToElem :: CFilter () -> Element ()
cfilterToElem f = case f (CString False "" ()) of
                    [CElem e _] -> xmlEscape stdXmlEscaper e
                    []          -> error "RSS produced no output"
                    _           -> error "RSS produced more than one output"

mkSimple :: String -> String -> CFilter ()
mkSimple t str = mkElem t [literal str]

mkTitle :: Title -> CFilter ()
mkTitle = mkSimple "title"

mkLink :: Link -> CFilter ()
mkLink = mkSimple "link" . show

mkDescription :: Description -> CFilter ()
mkDescription str = mkElem "description" [cdata str]

mkDocs :: CFilter ()
mkDocs = mkSimple "docs" "http://www.rssboard.org/rss-specification"

mkPubDate :: UTCTime -> CFilter ()
mkPubDate = mkSimple "pubDate" . formatDate

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale rfc822DateFormat

mkCategory :: Maybe Domain -> String -> CFilter ()
mkCategory md s = mkElemAttr "category" attrs [literal s]
    where attrs = maybe [] (\d -> [("domain", literal d)]) md

maybeElem :: (a -> CFilter ()) -> Maybe a -> [CFilter ()]
maybeElem = maybe [] . ((:[]) .)

mkChannelElem :: ChannelElem -> CFilter ()
mkChannelElem (Language str) = mkSimple "language" str
mkChannelElem (Copyright str) = mkSimple "copyright" str
mkChannelElem (ManagingEditor str) = mkSimple "managingEditor" str
mkChannelElem (WebMaster str) = mkSimple "webMaster" str
mkChannelElem (ChannelPubDate date) = mkPubDate date
mkChannelElem (LastBuildDate date) = mkSimple "lastBuildDate" $ formatDate date
mkChannelElem (ChannelCategory md str) = mkCategory md str
mkChannelElem (Generator str) = mkSimple "generator" str
mkChannelElem (Cloud host port path proc proto)
              = mkElemAttr "cloud" [("domain", literal host),
                                    ("port", literal (show port)),
                                    ("path", literal path),
                                    ("registerProcedure", literal proc),
                                    ("protocol", literal (protocolName proto))] []
mkChannelElem (TTL minutes) = mkSimple "ttl" $ show minutes
mkChannelElem (Image uri title link mw mh mdesc)
              = mkElem "image" ([mkElem "url" [literal (show uri)],
                                mkTitle title, mkLink link]
                                ++ maybeElem (mkSimple "width" . show) mw
                                ++ maybeElem (mkSimple "height" . show) mh
                                ++ maybeElem mkDescription mdesc)
mkChannelElem (Rating str) = mkSimple "rating" str
mkChannelElem (TextInput title desc name link)
              = mkElem "textInput" [mkTitle title, mkDescription desc,
                                    mkSimple "name" name, mkLink link]
mkChannelElem (SkipHours hs) = mkElem "skipHours" (map (mkSimple "hour" . show) hs)
mkChannelElem (SkipDays ds) = mkElem "skipDays" (map (mkSimple "day" . show) ds)

protocolName :: CloudProtocol -> String
protocolName CloudProtocolXmlRpc = "xml-rpc"
protocolName CloudProtocolSOAP   = "soap"

mkItem :: Item -> CFilter ()
mkItem itemElems = mkElem "item" (map mkItemElem itemElems)

mkItemElem :: ItemElem -> CFilter ()
mkItemElem (Title t) = mkTitle t
mkItemElem (Link l) = mkLink l
mkItemElem (Description d) = mkDescription d
mkItemElem (Author e) = mkElem "author" [literal e]
mkItemElem (Category md str) = mkCategory md str
mkItemElem (Comments uri) = mkSimple "comments" $ show uri
mkItemElem (Enclosure uri len mtype) =
    mkElemAttr "enclosure" [("url", literal (show uri)),
                            ("length", literal (show len)),
                            ("type", literal (mtype))]
                           []
mkItemElem (Guid perm s) = mkElemAttr "guid" attrs [ literal s ]
    where attrs = if perm then [("isPermaLink", literal "true")] else []
mkItemElem (PubDate ct) = mkElem "pubDate" [ literal (formatDate ct) ]
mkItemElem (Source uri t) =
    mkElemAttr "source" [("url", literal (show uri))] [ literal t ]
