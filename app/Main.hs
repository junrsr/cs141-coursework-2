module Main where

import Hatch
import Hurtle.Parser
import Hurtle.Types

import Text.Megaparsec (parse)
import Data.Fixed (mod')
import Data.List (foldl')


-- Define your TurtleState data type
data TurtleState = TurtleState {
    position :: (Float, Float),
    angle :: Float,
    penDown :: Bool,
    linesDrawnSoFar :: [((Float, Float), (Float,Float))]
}

-- Execute a single HogoCode command and update the TurtleState
update :: TurtleState -> HogoCode -> TurtleState
-- implement forward logic
update state@TurtleState{ position = (x, y), angle = currentAngle, penDown = isPenDown, linesDrawnSoFar = drawnLines } (GoForward distance) =
    let angleInRadians = currentAngle * (pi / 180) -- convert angle to radians so can use in calculations
        newX = x + distance * cos angleInRadians
        newY = y + distance * sin angleInRadians
        newLines
            | isPenDown = drawnLines ++ [((x, y), (newX, newY))] -- add the new line to the drawn lines if the pen is down
            | otherwise = drawnLines -- otherwise set newLines to be the previous list of lines
    in state { position = (newX, newY), linesDrawnSoFar=newLines }

-- go backwards
update state@TurtleState{position = (x, y), angle = currentAngle, penDown = isPenDown, linesDrawnSoFar = drawnLines} (GoBackward distance) =
    let angleInRadians = currentAngle * (pi / 180) -- calculate the current angle in radians
        newX = x - distance * cos angleInRadians
        newY = y - distance * sin angleInRadians
        newLines
            | isPenDown = drawnLines ++ [((x, y), (newX, newY))] -- add the new line to the drawn lines if the pen is down
            | otherwise = drawnLines -- otherwise set newLines to be the previous list of lines
    in state { position = (newX, newY),  linesDrawnSoFar=newLines}


-- turn left
update state@TurtleState{angle=currentAngle} (TurnLeft degrees) = state { angle = (currentAngle + degrees) `mod'` 360 }

-- turn right
update state@TurtleState{angle=currentAngle} (TurnRight degrees) = state { angle = (currentAngle - degrees) `mod'` 360 }

-- Implement pen up logic
update state@TurtleState{position=_, angle=_, linesDrawnSoFar=_} PenUp = state { penDown = False }

-- implement pen down logic
update state@TurtleState{position=_, angle=_, linesDrawnSoFar=_} PenDown = state { penDown = True }

-- update turtleState GoHome = -- Implement go home logic
update state@TurtleState{penDown=_, linesDrawnSoFar=_} GoHome = state { position=(0, 0), angle=90 }

-- update turtleState ClearScreen = -- Implement clear screen logic
update state@TurtleState{position=_, angle=_, penDown=_} ClearScreen = state { linesDrawnSoFar = [] }

-- handles repeat statements
update state (Repeat n commands) = repeatCommands state n commands
    where
        repeatCommands :: TurtleState -> Int -> [HogoCode] -> TurtleState
        repeatCommands state n commands
            | n <= 0 = state
            | otherwise = repeatCommands (foldl' update state commands) (n - 1) commands


-- store the initial state of the turtle
initialState :: TurtleState
initialState = TurtleState { position = (0, 0), angle = 90, penDown = True, linesDrawnSoFar = [] }


showTurtleState :: TurtleState -> Image
showTurtleState TurtleState{position=(x, y), angle=currentAngle, linesDrawnSoFar=drawnLines} =
    let
        -- creates the image for the 'ant' and scales it up
        showTurtle :: Image
        showTurtle = offset (round (x)) (round (y)) (rotate angle ant)
            where angle = round (currentAngle - 90 `mod'` 360)

        -- creates the lines between each position the user has drawn and converts it into an image
        drawLines :: [Image]
        drawLines = map (\((x1, y1), (x2, y2)) -> line x1 y1 x2 y2) drawnLines
    
    in
        superimposeAll (showTurtle : drawLines)



-- Given a program, process it into a list of TurtleStates
processProgram :: [HogoCode] -> [TurtleState]
processProgram program = scanl update initialState program
  -- 'scanl' applies 'update' to each command in 'program', accumulating the results.
  -- This starts with 'initialState' and builds a list where each element is the state
  -- after executing one more command.

animation :: [TurtleState] -> Int -> Image
animation states t
    | t < length states = showTurtleState (states !! t)
    | otherwise = showTurtleState(last states)
  -- Using 'mod' to loop the animation if 't' exceeds the number of states.

main :: IO ()
main = do
    putStrLn "Enter the name of the Hogo program file:"
    filename <- getLine
    contents <- readFile filename
    let states = either (const []) processProgram (parse parseHogo filename contents)
    runAnimation (animation states)
    -- 'processProgram' turns the list of HogoCode commands into a list of TurtleStates.
    -- 'animation' is then adapted to work with this list of states.