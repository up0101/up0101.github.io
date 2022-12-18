#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [ frontmatter ])"
#! nix-shell --pure -i runhaskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 qualified as B8
import Data.Frontmatter
import Data.Functor ((<&>))
import Data.List (isSuffixOf, sortOn)
import Data.Ord qualified
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml
import GHC.Generics
import System.Directory
import Text.Printf

data PostMeta = PostMeta {title :: String, date :: Day} deriving (Generic, Show)

instance ToJSON PostMeta where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PostMeta

encodePost :: PostMeta -> FilePath -> Text
encodePost (PostMeta title date) path =
  T.pack $ printf "- [%s](%s) (%s)" title uri (iso8601Show date)
  where
    uri =
      "/"
        <> take (length path - 3) path -- drop .md
        <> ".html"

main = do
  postPaths <-
    listDirectory "."
      <&> filter (isSuffixOf ".md")
      <&> filter (/= "index.md")
  postMetas <- forM postPaths \p ->
    B8.readFile p
      <&> parseYamlFrontmatterMaybe
      >>= \case
        Just x -> pure (x :: PostMeta)
        _ -> error ("Parse failure at " <> p)
  let sorted = sortOn (Data.Ord.Down . date . fst) (postMetas `zip` postPaths)
  T.putStrLn . T.unlines $ uncurry encodePost <$> sorted
