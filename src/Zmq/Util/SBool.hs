module Zmq.Util.SBool
  ( SBool(..)
  ) where

import Zmq.Prelude


data SBool :: Bool -> Type where
  SFalse :: SBool 'False
  STrue :: SBool 'True
