module Zmqhs.Error
  ( Error (..),
    pattern EADDRINUSE,
    pattern EADDRNOTAVAIL,
    pattern EAGAIN,
    pattern EFAULT,
    pattern EHOSTUNREACH,
    pattern EINTR,
    pattern EINVAL,
    pattern EMFILE,
    pattern EMTHREAD,
    pattern ENODEV,
    pattern ENOENT,
    pattern ENOTSOCK,
    pattern ETERM,
  )
where

import Zmqhs.Internal.Error
