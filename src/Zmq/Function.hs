module Zmq.Function where

data Function
  = Function'Bind
  | Function'Connect
  | Function'Disconnect
  | Function'Send
  | Function'Socket
  | Function'Unbind

type family CanReturnEADDRINUSE ( function :: Function ) :: Bool where
  CanReturnEADDRINUSE 'Function'Bind = 'True
  CanReturnEADDRINUSE _ = 'False

type family CanReturnEADDRNOTAVAIL ( function :: Function ) :: Bool where
  CanReturnEADDRNOTAVAIL 'Function'Bind = 'True
  CanReturnEADDRNOTAVAIL _ = 'False

type family CanReturnEHOSTUNREACH ( function :: Function ) :: Bool where
  -- TODO only a few socket types (stream, server, router if flag set) can
  -- return EHOSTUNREACH on send
  CanReturnEHOSTUNREACH 'Function'Send = 'True
  CanReturnEHOSTUNREACH _ = 'False

type family CanReturnEINVAL ( function :: Function ) :: Bool where
  CanReturnEINVAL 'Function'Bind = 'True
  CanReturnEINVAL 'Function'Connect = 'True
  CanReturnEINVAL 'Function'Disconnect = 'True
  CanReturnEINVAL 'Function'Unbind = 'True
  CanReturnEINVAL _ = 'False

type family CanReturnEMFILE ( function :: Function ) :: Bool where
  CanReturnEMFILE 'Function'Socket = 'True
  CanReturnEMFILE _ = 'False

type family CanReturnEMTHREAD ( function :: Function ) :: Bool where
  CanReturnEMTHREAD 'Function'Bind = 'True
  CanReturnEMTHREAD 'Function'Connect = 'True
  CanReturnEMTHREAD _ = 'False

type family CanReturnENODEV ( function :: Function ) :: Bool where
  CanReturnENODEV 'Function'Bind = 'True
  CanReturnENODEV _ = 'False

-- type family CanReturnENOENT ( function :: Function ) :: Bool where
--   CanReturnENOENT _ = 'False

-- type family CanReturnEPROTONOSUPPORT ( function :: Function ) :: Bool where
--   CanReturnEPROTONOSUPPORT _ = 'False
