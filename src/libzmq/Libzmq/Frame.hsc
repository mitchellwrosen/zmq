{-# LANGUAGE CPP #-}

module Libzmq.Frame
  ( Frame(..)
  , initializeFrame
  , getFrameProperty
  , getFrameData
  , closeFrame
  ) where

#include <zmq.h>

import Data.Coerce (coerce)
import Foreign.C (CChar, CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))


newtype Frame
  = Frame { unFrame :: Ptr () }

instance Storable Frame where
  alignment _ = #{alignment zmq_msg_t}
  sizeOf _ = #{size zmq_msg_t}

  peek :: Ptr Frame -> IO Frame
  peek =
    coerce
      @( Ptr ( Ptr CChar ) -> IO ( Ptr CChar ) )
      #{ peek zmq_msg_t, _ }

  poke :: Ptr Frame -> Frame -> IO ()
  poke =
    coerce
      @( Ptr ( Ptr CChar ) -> Ptr CChar -> IO () )
      #{ poke zmq_msg_t, _ }


foreign import ccall unsafe "zmq_msg_init"
  initializeFrame :: Ptr Frame -> IO CInt

foreign import ccall unsafe "zmq_msg_data"
  getFrameData :: Ptr Frame -> IO ( Ptr CChar )

foreign import ccall unsafe "zmq_msg_get"
  getFrameProperty :: Ptr Frame -> CInt -> IO CInt

foreign import ccall unsafe "zmq_msg_close"
  closeFrame :: Ptr Frame -> IO CInt
