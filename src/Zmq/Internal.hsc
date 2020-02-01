{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Zmq.Internal
  ( CompatibleTransport
  , Endpoint(..)
  , endpointToString
  , SocketType(..)
  , Transport(..)
  ) where

#include <zmq.h>

import Data.Kind (Constraint)
import Data.Text (Text)
import qualified Data.Text as Text
#if defined ZMQ_HAVE_OPENPGM
import qualified GHC.TypeLits as TypeLits
#endif


--------------------------------------------------------------------------------
-- Compatible transport
--------------------------------------------------------------------------------

type family CompatibleTransport ( typ :: SocketType ) ( transport :: Transport ) :: Constraint where
#if defined ZMQ_HAVE_OPENPGM
  CompatibleTransport Pub  TransportEpgm = ()
  CompatibleTransport Pub  TransportPgm  = ()
  CompatibleTransport Sub  TransportEpgm = ()
  CompatibleTransport Sub  TransportPgm  = ()
  -- CompatibleTransport XPub TransportEpgm = ()
  -- CompatibleTransport XPub TransportPgm  = ()
  -- CompatibleTransport XSub TransportPgm  = ()
  -- CompatibleTransport XSub TransportEpgm = ()

  CompatibleTransport typ TransportEpgm =
    TypeLits.TypeError
      ( TypeLits.ShowType typ
        TypeLits.:<>:
        TypeLits.Text " sockets are not compatible with the `epgm` transport."
      )
  CompatibleTransport typ TransportPgm  =
    TypeLits.TypeError
      ( TypeLits.ShowType typ
        TypeLits.:<>:
        TypeLits.Text " sockets are not compatible with the `pgm` transport."
      )
#endif

  CompatibleTransport typ transport = ()


--------------------------------------------------------------------------------
-- Endpoint
--------------------------------------------------------------------------------

data Endpoint ( transport :: Transport ) where
#if defined ZMQ_HAVE_OPENPGM
  Epgm   :: Text -> Endpoint TransportEpgm
#endif
  Inproc :: Text -> Endpoint TransportInproc
#if !defined ZMQ_HAVE_WINDOWS && !defined ZMQ_HAVE_OPENVMS && !defined ZMQ_HAVE_VXWORKS
  Ipc    :: Text -> Endpoint TransportIpc
#endif
#if defined ZMQ_HAVE_OPENPGM
  Pgm    :: Text -> Endpoint TransportPgm
#endif
  Tcp    :: Text -> Endpoint TransportTcp
#if defined ZMQ_HAVE_VMCI
  Vmci   :: Text -> Endpoint TransportVmci
#endif
deriving stock instance Eq ( Endpoint transport )
deriving stock instance Ord ( Endpoint transport )
deriving stock instance Show ( Endpoint transport )

endpointToString
  :: Endpoint transport
  -> String
endpointToString = \case
#if defined ZMQ_HAVE_OPENPGM
  Epgm   address -> "epgm://"   ++ Text.unpack address
#endif
  Inproc address -> "inproc://" ++ Text.unpack address
#if !defined ZMQ_HAVE_WINDOWS && !defined ZMQ_HAVE_OPENVMS && !defined ZMQ_HAVE_VXWORKS
  Ipc    address -> "ipc://"    ++ Text.unpack address
#endif
#if defined ZMQ_HAVE_OPENPGM
  Pgm    address -> "pgm://"    ++ Text.unpack address
#endif
  Tcp    address -> "tcp://"    ++ Text.unpack address
#if defined ZMQ_HAVE_VMCI
  Vmci   address -> "vmci://"   ++ Text.unpack address
#endif


--------------------------------------------------------------------------------
-- Transport
--------------------------------------------------------------------------------

data Transport where
#if defined ZMQ_HAVE_OPENPGM
  TransportEpgm :: Transport
#endif
  TransportInproc :: Transport
#if !defined ZMQ_HAVE_WINDOWS && !defined ZMQ_HAVE_OPENVMS && !defined ZMQ_HAVE_VXWORKS
  TransportIpc :: Transport
#endif
#if defined ZMQ_HAVE_OPENPGM
  TransportPgm :: Transport
#endif
  TransportTcp :: Transport
#if defined ZMQ_HAVE_VMCI
  TransportVmci :: Transport
#endif


--------------------------------------------------------------------------------
-- Socket type
--------------------------------------------------------------------------------

data SocketType
  = Pub
  | Sub
