module Main (main)  where

import Data.Char ( toLower )

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import TuringInternal
    ( TapeCell(Star, Blank, TC),
      Machine(..),
      Rule,
      Direction(MoveRight),
      headOfTape,
      createMachine,
      executeRule,
      createRule,
      executeProgram,
      createReplaceBlock,
      createHaltBlock )

executeRuleSpec :: Spec
executeRuleSpec = describe "executeRuleSpec" $ do
  it "example 1" $ do
    executeRule m r `shouldBe` Machine {left = [TC 'y'], right = [TC 'y'], headM = TC 'x', state = 1}
      where
        r = createRule 0 'x' 'y' 1 MoveRight
        m = createMachine ['x', 'x', 'y']


programResult :: Maybe Machine -> Char
programResult Nothing = '-'
programResult (Just m) = case result of
           Blank -> '#'
           TC c -> c
           Star -> '*'
     where
        result = (headOfTape $ right m)

challenge4Rules :: [Rule]
challenge4Rules = (createReplaceBlock 0 'x' 3 12) ++ 
       (createReplaceBlock 3 'y' 6 10) ++
       (createReplaceBlock 6 'z' 0 10) ++
       (createHaltBlock 10 'n') ++
       (createReplaceBlock 12 'y' 15 10) ++
       (createReplaceBlock 15 'z' 18 10) ++
       (createReplaceBlock 18 'y' 21 26) ++
       (createReplaceBlock 21 'z' 18 10) ++
       (createReplaceBlock 26 'z' 29 10) ++
       (createHaltBlock 29 'y')

executeTest :: String -> Char
executeTest str = programResult m'
    where
      m = createMachine (map toLower str)
      m' = executeProgram m challenge4Rules

challenge4Spec :: Spec
challenge4Spec = describe "challenge4Spec" $ do
  it "XYXXYYZZ" $ do
    executeTest "XYXXYYZZ" `shouldBe` 'n'
  it "ZZZYZXZY" $ do
    executeTest "ZZZYZXZY" `shouldBe` 'y'
  it "XYZZ" $ do
    executeTest "XYZZ" `shouldBe` 'n'
  it "ZYZXYZ" $ do
    executeTest "ZYZXYZ" `shouldBe` 'y'
  it "YZ" $ do
    executeTest "YZ" `shouldBe` 'n'
  it "YZZ" $ do
    executeTest "YZZ" `shouldBe` 'y'
  it "Z" $ do
    executeTest "Z" `shouldBe` 'n'
  it "ZZZYYXYYZZZ" $ do
    executeTest "ZZZYYXYYZZZ" `shouldBe` 'y'


main :: IO ()
main = hspec $ do
  executeRuleSpec
  challenge4Spec