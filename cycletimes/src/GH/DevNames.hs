{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module GH.DevNames where

import qualified  Data.Text as T
import qualified  Data.Map.Strict as M

devMap :: M.Map T.Text T.Text
devMap = M.fromList
  [ ("Anviking", "Johannes Lund")
  , ("JaredCorduan", "Jared Corduan")
  , ("KtorZ", "Matthias Benkort")
  , ("denisshevchenko", "Denis Shevchenko")
  , ("dnadales", "Damian Nadales")
  , ("mdimjasevic", "Marko Dimjasevic")
  , ("nc6", "Nicholas Clarke")
  , ("parsonsmatt", "Matt Parsons")
  , ("paweljakubas", "Pawel Jakubas")
  , ("polinavino", "Polina Vinogradova")
  , ("uroboros", "Ryan Lemmer")
  , ("redxaxder", "Alex Bialy")
  , ("ruhatch", "Rupert Horlick")
  , ("Jimbo4350", "Jordan Millar")
  , ("kantp", "Philipp Kant")
  , ("mgudemann", "Matthias Güdemann")
  , ("ksaric", "Kristijan Šarić")
  , ("HirotoShioi", "Hiroto Shioi")
  , ("robcohen", "Rob Cohen")
  , ("coot", "Marcin Szamotulski")
  , ("akegalj", "Ante Kegalj")
  , ("CodiePP", "Alexander Diemand")
  , ("365andreas", "Andreas Tryantafyllos")
  , ("mrBliss", "Thomas Winant")
  , ("piotr-iohk", "Piotr (QA)")
  , ("karknu", "Karl Knutsson")
  , ("kderme", "Kostas Dermentzis")

  ]

toRealName :: T.Text -> T.Text
toRealName pseudo = case M.lookup pseudo devMap of
  Nothing -> pseudo
  Just uname -> uname
