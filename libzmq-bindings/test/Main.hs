module Main (main) where

import Data.Coerce (coerce)
import Foreign (alloca, peek, sizeOf)
import Foreign.C
  ( CInt,
    Errno (..),
    eAGAIN,
    eBADF,
    eFAULT,
    eINTR,
    eINVAL,
    eMFILE,
    eNODEV,
    eNOENT,
    eNOMEM,
    peekCString,
  )
import Libzmq.Bindings
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain (testGroup "tests" tests)

tests :: [TestTree]
tests =
  [ testGroup "zmq_ctx_get" zmq_ctx_get_tests,
    testGroup "zmq_ctx_set" zmq_ctx_set_tests,
    testGroup "zmq_ctx_shutdown" zmq_ctx_shutdown_tests,
    testGroup "zmq_ctx_term" zmq_ctx_term_tests,
    testGroup "zmq_strerror" zmq_strerror_tests,
    testGroup "zmq_version" zmq_version_tests
  ]

zmq_ctx_get_tests :: [TestTree]
zmq_ctx_get_tests =
  [ testCase "gets the number of IO threads" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_IO_THREADS >>= (@?= _ZMQ_IO_THREADS_DFLT)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "gets the max number of sockets" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_MAX_SOCKETS >>= (@?= _ZMQ_MAX_SOCKETS_DFLT)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "gets the max configurable number of sockets" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_SOCKET_LIMIT >>= (@?= 65535)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "gets IPv6" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_IPV6 >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "gets blocky" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_BLOCKY >>= (@?= 1)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "gets the thread scheduling policy" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_THREAD_SCHED_POLICY >>= (@?= _ZMQ_THREAD_SCHED_POLICY_DFLT)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "gets the thread name prefix" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_THREAD_NAME_PREFIX >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "gets the size of a zmq_msg_t" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx _ZMQ_MSG_T_SIZE >>= (@?= fromIntegral @Int @CInt (sizeOf (undefined :: Zmq_msg)))
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "returns EINVAL on bogus option" do
      ctx <- zmq_ctx_new
      zmq_ctx_get ctx 12345 >>= (@?= (-1))
      zmq_errno >>= (@?= _EINVAL)
      zmq_ctx_term ctx >>= (@?= 0)
  ]

zmq_ctx_set_tests :: [TestTree]
zmq_ctx_set_tests =
  [ testCase "sets blocky" do
      ctx <- zmq_ctx_new
      zmq_ctx_set ctx _ZMQ_BLOCKY 0 >>= (@?= 0)
      zmq_ctx_get ctx _ZMQ_BLOCKY >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "sets the number of IO threads" do
      ctx <- zmq_ctx_new
      zmq_ctx_set ctx _ZMQ_IO_THREADS 0 >>= (@?= 0)
      zmq_ctx_get ctx _ZMQ_IO_THREADS >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "sets the thread scheduling policy" do
      ctx <- zmq_ctx_new
      zmq_ctx_set ctx _ZMQ_THREAD_SCHED_POLICY 0 >>= (@?= 0)
      zmq_ctx_get ctx _ZMQ_THREAD_SCHED_POLICY >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "sets the thread name prefix" do
      ctx <- zmq_ctx_new
      zmq_ctx_set ctx _ZMQ_THREAD_NAME_PREFIX 1 >>= (@?= 0)
      zmq_ctx_get ctx _ZMQ_THREAD_NAME_PREFIX >>= (@?= 1)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "sets the maximum number of sockets" do
      ctx <- zmq_ctx_new
      zmq_ctx_set ctx _ZMQ_MAX_SOCKETS 1 >>= (@?= 0)
      zmq_ctx_get ctx _ZMQ_MAX_SOCKETS >>= (@?= 1)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "sets IPv6" do
      ctx <- zmq_ctx_new
      zmq_ctx_set ctx _ZMQ_IPV6 1 >>= (@?= 0)
      zmq_ctx_get ctx _ZMQ_IPV6 >>= (@?= 1)
      zmq_ctx_term ctx >>= (@?= 0)
  ]

zmq_ctx_shutdown_tests :: [TestTree]
zmq_ctx_shutdown_tests =
  [ testCase "shuts down a context" do
      ctx <- zmq_ctx_new
      zmq_ctx_shutdown ctx >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "does nothing on shut-down context" do
      ctx <- zmq_ctx_new
      zmq_ctx_shutdown ctx >>= (@?= 0)
      zmq_ctx_shutdown ctx >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0),
    testCase "returns EFAULT on terminated context" do
      ctx <- zmq_ctx_new
      zmq_ctx_shutdown ctx >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= 0)
      zmq_ctx_shutdown ctx >>= (@?= (-1))
      zmq_errno >>= (@?= _EFAULT)
  ]

zmq_ctx_term_tests :: [TestTree]
zmq_ctx_term_tests =
  [ testCase "returns EFAULT on terminated context" do
      ctx <- zmq_ctx_new
      zmq_ctx_term ctx >>= (@?= 0)
      zmq_ctx_term ctx >>= (@?= (-1))
      zmq_errno >>= (@?= _EFAULT)
  ]

zmq_strerror_tests :: [TestTree]
zmq_strerror_tests =
  [ testCase "returns the libzmq version" do
      (x, y, z) <-
        alloca \px ->
          alloca \py ->
            alloca \pz -> do
              zmq_version px py pz
              (,,) <$> peek px <*> peek py <*> peek pz
      (x, y, z) @?= (4, 3, 4)
  ]

zmq_version_tests :: [TestTree]
zmq_version_tests =
  [ testCase "translates error codes to error strings" do
      peekCString (zmq_strerror _EADDRINUSE) >>= (@?= "Address already in use")
      peekCString (zmq_strerror _EADDRNOTAVAIL) >>= (@?= "Can't assign requested address")
      peekCString (zmq_strerror _EAFNOSUPPORT) >>= (@?= "Address family not supported by protocol family")
      peekCString (zmq_strerror _EAGAIN) >>= (@?= "Resource temporarily unavailable")
      peekCString (zmq_strerror _EBADF) >>= (@?= "Bad file descriptor")
      peekCString (zmq_strerror _ECONNABORTED) >>= (@?= "Software caused connection abort")
      peekCString (zmq_strerror _ECONNREFUSED) >>= (@?= "Connection refused")
      peekCString (zmq_strerror _ECONNRESET) >>= (@?= "Connection reset by peer")
      peekCString (zmq_strerror _EFAULT) >>= (@?= "Bad address")
      peekCString (zmq_strerror _EFSM) >>= (@?= "Operation cannot be accomplished in current state")
      peekCString (zmq_strerror _EHOSTUNREACH) >>= (@?= "Host unreachable")
      peekCString (zmq_strerror _EINPROGRESS) >>= (@?= "Operation now in progress")
      peekCString (zmq_strerror _EINTR) >>= (@?= "Interrupted system call")
      peekCString (zmq_strerror _EINVAL) >>= (@?= "Invalid argument")
      peekCString (zmq_strerror _EMFILE) >>= (@?= "Too many open files")
      peekCString (zmq_strerror _EMSGSIZE) >>= (@?= "Message too long")
      peekCString (zmq_strerror _EMTHREAD) >>= (@?= "No thread available")
      peekCString (zmq_strerror _ENETDOWN) >>= (@?= "Network is down")
      peekCString (zmq_strerror _ENETRESET) >>= (@?= "Network dropped connection on reset")
      peekCString (zmq_strerror _ENETUNREACH) >>= (@?= "Network is unreachable")
      peekCString (zmq_strerror _ENOBUFS) >>= (@?= "No buffer space available")
      peekCString (zmq_strerror _ENOCOMPATPROTO) >>= (@?= "The protocol is not compatible with the socket type")
      peekCString (zmq_strerror _ENODEV) >>= (@?= "Operation not supported by device")
      peekCString (zmq_strerror _ENOENT) >>= (@?= "No such file or directory")
      peekCString (zmq_strerror _ENOMEM) >>= (@?= "Cannot allocate memory")
      peekCString (zmq_strerror _ENOTCONN) >>= (@?= "Socket is not connected")
      peekCString (zmq_strerror _ENOTSOCK) >>= (@?= "Socket operation on non-socket")
      peekCString (zmq_strerror _ENOTSUP) >>= (@?= "Operation not supported")
      peekCString (zmq_strerror _EPROTONOSUPPORT) >>= (@?= "Protocol not supported")
      peekCString (zmq_strerror _ETERM) >>= (@?= "Context was terminated")
      peekCString (zmq_strerror _ETIMEDOUT) >>= (@?= "Operation timed out"),
    testCase "complains about unknown error codes" do
      peekCString (zmq_strerror 0) >>= (@?= "Undefined error: 0")
  ]

------------------------------------------------------------------------------------------------------------------------
-- Helpers

_EAGAIN :: CInt
_EAGAIN =
  coerce @Errno @CInt eAGAIN

_EBADF :: CInt
_EBADF =
  coerce @Errno @CInt eBADF

_EFAULT :: CInt
_EFAULT =
  coerce @Errno @CInt eFAULT

_EINTR :: CInt
_EINTR =
  coerce @Errno @CInt eINTR

_EINVAL :: CInt
_EINVAL =
  coerce @Errno @CInt eINVAL

_EMFILE :: CInt
_EMFILE =
  coerce @Errno @CInt eMFILE

_ENODEV :: CInt
_ENODEV =
  coerce @Errno @CInt eNODEV

_ENOENT :: CInt
_ENOENT =
  coerce @Errno @CInt eNOENT

_ENOMEM :: CInt
_ENOMEM =
  coerce @Errno @CInt eNOMEM
