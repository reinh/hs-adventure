{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
-- |

module Main where

import           Control.Applicative
import           Control.Lens                 hiding (isn't)
import           Data.Data
import           Data.Data.Lens               (uniplate)
import           Data.List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Typeable
import qualified Text.PrettyPrint.ANSI.Leijen as P

data Item = Item
  deriving (Show,Eq,Ord,Data,Typeable)

data Scenery = Scenery
  { sceneryName        :: String
  , sceneryFlags       :: Set String
  , sceneryDescription :: Scenery -> P.Doc
  , sceneryActions     :: Map String (Scenery -> Scenery)
  }

instance Show Scenery where
  show (Scenery n _ _ _) = "Scenery " ++ n

makeFields ''Scenery

data Description
  = Empty
  | Str String
  | Scen Scenery
  | Beside Description Description
  deriving Show

-- instance Plated Expr where
--   plate f (Neg e = Neg <$> f e
--   plate f (Add a b = Add <$> f a <*> f b
--   plate _ a = pure a))

instance Plated Description where
  plate f (Beside a b) = Beside <$> f a <*> f b
  plate _ a = pure a

instance Monoid Description where
  mempty = Empty
  mappend = Beside

instance P.Pretty Description where
  pretty Empty = P.empty
  pretty (Str s) = P.text s
  pretty (Scen scen) = (sceneryDescription scen) scen
  pretty (Beside a b) = P.pretty a P.<> P.pretty b

describe :: Scenery -> P.Doc
describe scen = (sceneryDescription scen) scen

instance IsString Description where
  fromString = Str

makePrisms ''Description

data Room = Room
  { roomDescription :: Description
  , roomItems       :: [Item]
  } deriving Show

makeFields ''Room

instance P.Pretty Room where
  pretty (Room desc _) = P.pretty desc

is :: (HasFlags s (Set a), Ord a)
   => s -> a -> Bool
is obj flg = flg `Set.member` (obj ^. flags)

isn't :: (HasFlags s (Set a), Ord a)
      => s -> a -> Bool
isn't obj flg = not (obj `is` flg)

apartment :: Room
apartment = Room desc []
  where
    desc = "Behold Bob's apartment. " <> Scen sink

sink = Scenery "sink" flgs sinkDesc actions
  where
    flgs = Set.fromList ["dirty"]
    sinkDesc sink
      | sink `is` "dirty" = "That smell is coming from the pile of dishes in the sink."
      | otherwise = "There is a pile of clean dishes in the sink."
    actions = (Map.fromList [("clean", \sink -> sink & flags . contains "dirty" .~ False)])
