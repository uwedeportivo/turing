module TuringInternal where

import Text.Printf

data TapeCell = TC Char | Blank | Star
    deriving (Show, Eq)

data Direction = MoveLeft | MoveRight
    deriving (Show, Eq)

type Tape = [TapeCell]

data Machine = Machine { left :: Tape
                       , right :: Tape
                       , headM :: TapeCell
                       , state :: Int
                       } deriving (Show, Eq)

data Rule = Rule {  currentState :: Int
                  , readTape :: TapeCell
                  , writeTape :: TapeCell
                  , newState :: Int
                  , direction :: Direction
                 } deriving (Show, Eq)

headOfTape :: Tape -> TapeCell
headOfTape [] = Blank
headOfTape (c:_) = c

tailOfTape :: Tape -> Tape
tailOfTape [] = []
tailOfTape (_:cs) = cs

executeLeft :: Machine -> Rule -> Machine
executeLeft m r = Machine { left = tailOfTape (left m)
                            ,right = w : (right m)
                            , headM = headOfTape (left m)
                            , state = newState r
                          }
                 where
                    w = case (writeTape r) of
                        Star -> (headM m)
                        v -> v         

executeRight :: Machine -> Rule -> Machine
executeRight m r = Machine { right = tailOfTape (right m)
                            ,left = w : (left m)
                            , headM = headOfTape (right m)
                            , state = newState r
                          }
                 where
                    w = case (writeTape r) of
                        Star -> (headM m)
                        v -> v         

executeRule :: Machine -> Rule -> Machine
executeRule m r = case (direction r) of
                MoveLeft -> executeLeft m r
                MoveRight -> executeRight m r

createMachine :: [Char] -> Machine
createMachine [] = Machine { left = []
                            ,right = []
                            , headM = Blank
                            , state = 0
                          }
createMachine (c:cs) = Machine { left = []
                            ,right = [TC v | v <- cs]
                            , headM = TC c
                            , state = 0
                          }

createRule :: Int -> Char -> Char -> Int -> Direction -> Rule
createRule s u v s' d = Rule {currentState = s
                        , readTape = TC u
                        , writeTape = TC v
                        , newState = s'
                        , direction = d
                       }

matchTapeCell :: TapeCell -> TapeCell -> Bool
matchTapeCell _ Star = True
matchTapeCell u v = u == v

matchRule :: Machine -> Rule -> Bool
matchRule m r = (currentState r) == (state m) && (matchTapeCell (headM m) (readTape r))

executeStep :: Machine -> [Rule] -> Maybe Machine
executeStep _ [] = Nothing
executeStep m (r:rs) = if (matchRule m r) then Just (executeRule m r) else executeStep m rs

executeSteps :: Int -> [Rule] -> Maybe Machine -> Maybe Machine
executeSteps _ _ Nothing = Nothing
executeSteps _ [] _ = Nothing
executeSteps 0 _ mm = mm
executeSteps n rs (Just m) = case (executeStep m rs) of
    Just m' -> executeSteps (n - 1) rs (Just m')
    Nothing -> Nothing
 
executeProgram :: Machine -> [Rule] -> Maybe Machine
executeProgram m rs = case (executeStep m rs) of
        Just m' -> if state m' == -1 then Just m' else (executeProgram m' rs)
        Nothing -> Nothing

createReplaceBlock :: Int -> Char -> Int -> Int -> [Rule]
createReplaceBlock startState c yesEndState noEndState = [
         Rule {currentState = startState
                        , readTape = TC c
                        , writeTape = TC 'd'
                        , newState = startState + 1
                        , direction = MoveLeft
                       },
         Rule {currentState = startState
                        , readTape = Blank
                        , writeTape = Blank
                        , newState = startState + 2
                        , direction = MoveLeft
                       },
         Rule {currentState = startState
                        , readTape = Star
                        , writeTape = Star
                        , newState = startState
                        , direction = MoveRight
                       },
         Rule {currentState = startState + 1
                        , readTape = Blank
                        , writeTape = Blank
                        , newState = yesOut
                        , direction = MoveRight
                       },
         Rule {currentState = startState + 1
                        , readTape = Star
                        , writeTape = Star
                        , newState = startState + 1
                        , direction = MoveLeft
                       },
        Rule {currentState = startState + 2
                        , readTape = Blank
                        , writeTape = Blank
                        , newState = noOut
                        , direction = MoveRight
                       },
        Rule {currentState = startState + 2
                        , readTape = Star
                        , writeTape = Star
                        , newState = startState + 2
                        , direction = MoveLeft
                       }
       ]
  where
    yesOut = if yesEndState == -1 then startState + 3 else yesEndState
    noOut = if noEndState == -1 then startState + 4 else noEndState

createHaltBlock :: Int -> Char -> [Rule]
createHaltBlock startState c = [
         Rule {currentState = startState
                        , readTape = Blank
                        , writeTape = TC c
                        , newState = -1
                        , direction = MoveLeft
                       },
         Rule {currentState = startState
                        , readTape = Star
                        , writeTape = Blank
                        , newState = startState
                        , direction = MoveRight
                       }
        ]

tapeCellToString :: TapeCell -> String
tapeCellToString tc = case tc of
    Blank -> "#"
    Star -> "*"
    TC c -> [c]

ruleToString :: Rule -> String
ruleToString r = printf "CurrentState=%d Read=%s Write=%s Direction=%s NewState=%d" 
                    (currentState r) (tapeCellToString $ readTape r) (tapeCellToString $ writeTape r) (show $ direction r)
                    (newState r)
