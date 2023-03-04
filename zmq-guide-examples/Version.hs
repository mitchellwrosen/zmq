import Text.Printf (printf)
import Zmq qualified

main :: IO ()
main = do
  let (major, minor, patch) = Zmq.version
  printf "Current 0MQ version is %d.%d.%d\n" major minor patch
