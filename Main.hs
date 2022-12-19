#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [ frontmatter feed xml-conduit pandoc ])"
#! nix-shell --pure -i runhaskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 qualified as B8
import Data.Frontmatter (parseYamlFrontmatterMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isSuffixOf, sortOn)
import Data.Ord qualified
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601Show)
import Foreign.C (CInt)
import GHC.Generics
import GHC.IO.Device qualified as GHC.IO
import GHC.IO.Encoding qualified as GHC.IO (utf8)
import GHC.IO.FD qualified as GHC.IO (mkFD)
import GHC.IO.Handle.FD qualified as GHC.IO (mkHandleFromFD)
import GHC.IO.IOMode qualified as GHC.IO
import System.Environment (getArgs)
import System.IO qualified as IO
import Text.Atom.Feed qualified as Atom
import Text.Atom.Feed.Export qualified as Export
import Text.Pandoc qualified as Pandoc
import Text.Printf (printf)
import Text.XML qualified as XML

-- * Types

data PostFrontmatter = PostFrontmatter {title :: Text, date :: Day}
  deriving (Generic, Show)

instance ToJSON PostFrontmatter where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PostFrontmatter

data Post = Post {frontmatter :: PostFrontmatter, uri :: Text, content :: Text}
  deriving (Show)

-- * Encoding

getUri :: FilePath -> Text
getUri p =
  let path = T.pack p
   in "/"
        <> T.take (T.length path - 3) path -- drop .md
        <> ".html"

encodeDay :: Day -> Text
encodeDay = T.pack . iso8601Show

encodePost :: Post -> Text
encodePost Post {..} =
  let PostFrontmatter {..} = frontmatter
   in T.pack $ printf "- [%s](%s) (%s)" title uri (encodeDay date)

-- * RSS / Atom

toAtomEntry :: Post -> Atom.Entry
toAtomEntry Post {..} =
  let PostFrontmatter {..} = frontmatter
   in (Atom.nullEntry uri (Atom.TextString title) (encodeDay date))
        { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "add512"}],
          Atom.entryLinks = [Atom.nullLink uri],
          Atom.entryContent = Just (Atom.HTMLContent content)
        }

feed :: [Post] -> Atom.Feed
feed posts =
  Atom.nullFeed
    "https://add512.srht.site/atom.xml"
    (Atom.TextString "add512")
    ( case posts of
        Post {frontmatter = PostFrontmatter {date}} : _ -> encodeDay date
        _ -> ""
    )

renderFeed :: [Post] -> Text
renderFeed posts =
  let Right el =
        XML.fromXMLElement $
          Export.xmlFeed $
            (feed posts)
              { Atom.feedEntries = toAtomEntry <$> posts,
                Atom.feedLinks = [Atom.nullLink "https://add512.srht.site"]
              }
      doc = XML.Document (XML.Prologue [] Nothing []) el []
   in TL.toStrict $ XML.renderText XML.def doc

-- * Exe

handleFromFdNum :: CInt -> IO IO.Handle
handleFromFdNum i = do
  (fd, _) <- GHC.IO.mkFD i GHC.IO.WriteMode Nothing False False
  GHC.IO.mkHandleFromFD
    fd
    GHC.IO.Stream
    ("<file descriptor: " ++ show fd ++ ">")
    GHC.IO.WriteMode
    False
    (Just GHC.IO.utf8)

withHandleFromFdNum :: CInt -> (IO.Handle -> IO a) -> IO a
withHandleFromFdNum i = bracket (handleFromFdNum i) IO.hClose

main :: IO ()
main = withHandleFromFdNum 4 \feedIoHandle -> do
  postPaths <- getArgs

  postContents <- mapM B8.readFile postPaths
  postFrontmatters :: [PostFrontmatter] <-
    (postPaths `zip` postContents) `forM` \(path, content) ->
      parseYamlFrontmatterMaybe content
        & \case
          Just x -> pure (x :: PostFrontmatter)
          _ -> error ("Parse failure at " <> path)
  postHtmls <-
    let md2html md =
          Pandoc.runIOorExplode $
            Pandoc.readMarkdown
              (Pandoc.def {Pandoc.readerExtensions = Pandoc.pandocExtensions})
              md
              >>= Pandoc.writeHtml5String Pandoc.def
     in md2html `mapM` (T.decodeUtf8 <$> postContents)
  let posts =
        let _posts =
              zipWith3
                Post
                postFrontmatters
                (getUri <$> postPaths)
                postHtmls
         in sortOn (Data.Ord.Down . date . frontmatter) _posts

  T.hPutStrLn feedIoHandle (renderFeed posts)
  T.putStrLn . T.unlines $ encodePost <$> posts
