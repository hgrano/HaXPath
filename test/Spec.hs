--import qualified HaXPath.Schematic.Test as S
import qualified HaXPath.Test           as X
import qualified System.Exit            as E
import qualified Test.HUnit             as H

main :: IO ()
main = do
  counts <- H.runTestTT $ H.TestList [X.suite] --, S.suite]
  if H.errors counts > 0 || H.failures counts > 0 then
    E.exitFailure
  else
    E.exitSuccess
