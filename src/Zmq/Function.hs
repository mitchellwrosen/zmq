module Zmq.Function where

data Function
  = Bind
  | Connect
  | Send

type family CanReturnEADDRINUSE ( function :: Function ) :: Bool where
  CanReturnEADDRINUSE 'Bind = 'True
  CanReturnEADDRINUSE _ = 'False

type family CanReturnEADDRNOTAVAIL ( function :: Function ) :: Bool where
  CanReturnEADDRNOTAVAIL 'Bind = 'True
  CanReturnEADDRNOTAVAIL _ = 'False

type family CanReturnEHOSTUNREACH ( function :: Function ) :: Bool where
  -- TODO only a few socket types (stream, server, router if flag set) can
  -- return EHOSTUNREACH on send
  CanReturnEHOSTUNREACH 'Send = 'True
  CanReturnEHOSTUNREACH _ = 'False

type family CanReturnEINVAL ( function :: Function ) :: Bool where
  CanReturnEINVAL 'Bind = 'True
  CanReturnEINVAL 'Connect = 'True
  CanReturnEINVAL _ = 'False

-- type family CanReturnEMFILE ( function :: Function ) :: Bool where
--   CanReturnEMFILE _ = 'False

type family CanReturnEMTHREAD ( function :: Function ) :: Bool where
  CanReturnEMTHREAD 'Bind = 'True
  CanReturnEMTHREAD 'Connect = 'True
  CanReturnEMTHREAD _ = 'False

type family CanReturnENODEV ( function :: Function ) :: Bool where
  CanReturnENODEV 'Bind = 'True
  CanReturnENODEV _ = 'False

-- type family CanReturnENOENT ( function :: Function ) :: Bool where
--   CanReturnENOENT _ = 'False

-- type family CanReturnEPROTONOSUPPORT ( function :: Function ) :: Bool where
--   CanReturnEPROTONOSUPPORT _ = 'False
