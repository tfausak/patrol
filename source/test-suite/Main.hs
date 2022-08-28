import qualified Patrol.Type.Event as Event

main :: IO ()
main = do
  event <- Event.new
  print event
