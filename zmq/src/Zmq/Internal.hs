{-# LANGUAGE UndecidableInstances #-}

module Zmq.Internal
  ( CompatibleTransport,
    Endpoint (..),
    renderEndpoint,
    SocketType (..),
    Transport (..),
  )
where

import Data.Kind (Constraint)
import Data.Text (Text)
import GHC.TypeLits qualified as TypeLits

--------------------------------------------------------------------------------
-- Compatible transport
--------------------------------------------------------------------------------

type family CompatibleTransport (typ :: SocketType) (transport :: Transport) :: Constraint where
  CompatibleTransport 'Pub 'TransportEpgm = ()
  CompatibleTransport 'Pub 'TransportPgm = ()
  CompatibleTransport 'Sub 'TransportEpgm = ()
  CompatibleTransport 'Sub 'TransportPgm = ()
-- CompatibleTransport XPub TransportEpgm = ()
-- CompatibleTransport XPub TransportPgm  = ()
-- CompatibleTransport XSub TransportPgm  = ()
-- CompatibleTransport XSub TransportEpgm = ()

  CompatibleTransport typ 'TransportEpgm =
    TypeLits.TypeError
      ( 'TypeLits.ShowType typ
          'TypeLits.:<>: 'TypeLits.Text " sockets are not compatible with the `epgm` transport."
      )
  CompatibleTransport typ 'TransportPgm =
    TypeLits.TypeError
      ( 'TypeLits.ShowType typ
          'TypeLits.:<>: 'TypeLits.Text " sockets are not compatible with the `pgm` transport."
      )
  CompatibleTransport _typ _transport = ()

--------------------------------------------------------------------------------
-- Endpoint
--------------------------------------------------------------------------------

data Endpoint (transport :: Transport) where
  Epgm :: Text -> Endpoint 'TransportEpgm
  Inproc :: Text -> Endpoint 'TransportInproc
  Ipc :: Text -> Endpoint 'TransportIpc
  Pgm :: Text -> Endpoint 'TransportPgm
  Tcp :: Text -> Endpoint 'TransportTcp
  Vmci :: Text -> Endpoint 'TransportVmci

deriving stock instance Eq (Endpoint transport)

deriving stock instance Ord (Endpoint transport)

deriving stock instance Show (Endpoint transport)

renderEndpoint ::
  Endpoint transport ->
  Text
renderEndpoint = \case
  Epgm address -> "epgm://" <> address
  Inproc address -> "inproc://" <> address
  Ipc address -> "ipc://" <> address
  Pgm address -> "pgm://" <> address
  Tcp address -> "tcp://" <> address
  Vmci address -> "vmci://" <> address

--------------------------------------------------------------------------------
-- Transport
--------------------------------------------------------------------------------

data Transport where
  TransportEpgm :: Transport
  TransportInproc :: Transport
  TransportIpc :: Transport
  TransportPgm :: Transport
  TransportTcp :: Transport
  TransportVmci :: Transport

--------------------------------------------------------------------------------
-- Socket type
--------------------------------------------------------------------------------

data SocketType
  = Pub
  | Sub
