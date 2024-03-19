module Main where

import Hatch
import Hurtle.Parser
import Hurtle.Types

import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec (parse)
import Data.Fixed (mod')
import Data.List (foldl')


{-|
    defines the turtlestate datatype which stores information such as position, angle, pendown and lines drawn. position is a pair of floats, and is used to represent
    the coordinates the turtle is currently at. the angle determines the direction they are facing, and is denoted by a float value representing angle in degrees.
    penDown stores information on if we are currently able to write or not: it returns true if so and false if we can't. finally, linesDrawnSoFar is used to represent
    the track the turtle has currently visited, tracing a line along their path. This is stored as a pair of a pair of floats, with the first pair denoting starting
    location and the second denoting the ending position. hEach turtlestate will be used to represent each individual frame, which will then be displayed to the user.
-}
data TurtleState = TurtleState {
    position :: (Float, Float),
    angle :: Float,
    penDown :: Bool,
    linesDrawnSoFar :: [((Float, Float), (Float,Float))]
}

{-|
    @RETURN the new state of the turtle after applying the given hogo code
    
    the update method enables us to take in a current state, apply some hogo code ot it and return the new state after applying the hogo code. this is important as it
    will allow us to update each individual frame to transition it from one state to the next, depending on the code. for all the given method, the state@TurtleState{}
    syntax is used to ensure that we don't need to constantly pass through all values stored within the record, even if they aren't relevant or being used. it does this
    by getting a reference to the current state (state@), and then returns that same state with updates values based on what we are attempting to do
-}
update :: TurtleState -> HogoCode -> TurtleState

{-|
    the forward and backward logic here both consider in all variables being stored in the turtle state that we take in, as well as taking in the GoForward command.
    to move forward, i want the turtle to move in the direction it is currently facing by the given distance. this can be done by converting the angle (previously
    stored in degrees) into radians so it can be then used in calculations. from here, we want to multiply the distance we want to move by the cosine of the angle
    to get the horizontal distance, and multiply it by the sine of the angle in radians to get the vertical distance. from here, we want to add/subtract these values
    from the current coordinates of the current state, and return the state with these new positions. additionally, we want to check if the pen is currently down,
    which denotes that writing is possible. if that is the case, we want to add the current position and the new calculated position as pairs into the drawnLines
    list since the turtle itself has moved, denoting a new line being drawn. if the pen is up, don't want to modify lines drawn. when returning the current state with
    updated positions, we can also update the drawnLines to match depending on the pen status.
-}
-- go forwards
update state@TurtleState{ position = (x, y), angle = currentAngle, penDown = isPenDown, linesDrawnSoFar = drawnLines } (GoForward distance) =
    let angleInRadians = currentAngle * (pi / 180) -- convert angle to radians so can use in calculations
        newX = x + distance * cos angleInRadians -- calculate horizontal direction to move in based on angle and distance
        newY = y + distance * sin angleInRadians -- calculate vertical direction to move in based on angle and distance
        newLines
            | isPenDown = drawnLines ++ [((x, y), (newX, newY))] -- add the new line to the drawn lines if the pen is down
            | otherwise = drawnLines -- otherwise set newLines to be the previous list of lines
    in state { position = (newX, newY), linesDrawnSoFar=newLines }

-- go backwards
update state@TurtleState{position = (x, y), angle = currentAngle, penDown = isPenDown, linesDrawnSoFar = drawnLines} (GoBackward distance) =
    let angleInRadians = currentAngle * (pi / 180) -- calculate the current angle in radians
        newX = x - distance * cos angleInRadians -- calcualte horizontal distance to move in based on angle and distance
        newY = y - distance * sin angleInRadians -- calculate horizontal distance to move in based on angle and distance
        newLines
            | isPenDown = drawnLines ++ [((x, y), (newX, newY))] -- add the new line to the drawn lines if the pen is down
            | otherwise = drawnLines -- otherwise set newLines to be the previous list of lines
    in state { position = (newX, newY),  linesDrawnSoFar=newLines}


{-|
    the turn left and turn right methods are both similar in what they do. they take in the current angle the turtle is facing, and then add or subtract the given
    degree to it depending on which direction we want to turn. in the event we get an angle which exceeds the current bounds (<0 or >=360), we apply the modulus
    function given by Data.Fixed. The regular mod method only works on integer values, and since we don't want to round the float in this insatnce we can instead
    use the `mod'` method with 360 to ensure that the calculated angle falls within the appropriate range. although 90 and 450 are the same, when we apply our
    calculation to convert the angle into radians (for go forward/backward), it will give a differing value, leading to misleading results. this is why the mod'
    method is essential. it then returns the current state with the angle correlating to the updated calculated angle
-}
-- turn left
update state@TurtleState{angle=currentAngle} (TurnLeft degrees) = state { angle = (currentAngle + degrees) `mod'` 360 }

-- turn right
update state@TurtleState{angle=currentAngle} (TurnRight degrees) = state { angle = (currentAngle - degrees) `mod'` 360 }

{-|
    the following pen up and pen down methods for update are relatively simple, as they take in a reference to the current turtle state and the command (which is
    constant and doesn't reqeurie any variables). from here, it simply returns the given state with the pen down variable being updated to false if the command is
    pen up, or true if the command is pen down
-}
-- pen up
update state@TurtleState{penDown=_} PenUp = state { penDown = False }

-- pen down
update state@TurtleState{penDown=_} PenDown = state { penDown = True }


{-|
    the go clear screen and home method is similar to the pen up/down methods, in a sense that they don't require any variables and the commands are constant. go
    home, however, resets the turtle state to the position (0, 0) and the angle to 90 degrees (north), and then returns this newturtle state. clearscreen, also does
    this, but also ensures that the lines drawn are removed, in effect removing the "trail" the turtle had left behind and clearing the screen
-}
-- go home
update state@TurtleState{position=_} GoHome = state { position=(0, 0), angle=90 }
-- clear screen
update state@TurtleState{linesDrawnSoFar=_} ClearScreen = state { position=(0, 0), angle=90, linesDrawnSoFar = [] }

{-|
    this method takes in a given block of hogo code comments, and applies it to the current tutle state n times, returning that new state. it contains a helper method
    repeatCommands which utilises foldl' to recursively apply the update method on a given state, and then updates the state before the next iteration so the code can
    be applied to it. once the base case is reached, we return the current state. In this case, foldl' improves the efficiency of this solution as it immediately
    applies a command to a given state, stopping us from accumulating states which haven't yet been evaluated.
-}
update state (Repeat n commands) = repeatCommands state n commands
    where
        repeatCommands :: TurtleState -> Int -> [HogoCode] -> TurtleState
        repeatCommands state n commands
            | n <= 0 = state -- base case: return the current state
            | otherwise = repeatCommands (foldl' update state commands) (n - 1) commands -- recursively call repeatCommands on the new state


-- reference to the initial state of the turtle -> at the origin facing north with no lines drawn so far
initialState :: TurtleState
initialState = TurtleState { position = (0, 0), angle = 90, penDown = True, linesDrawnSoFar = [] }


{-|
    The showTurtleState method is responsible for converting the current state to a particular frame. it first takes the turtle image we want to display, and scales it
    up. The image is then rotated to match with the angle provided, and then put in the appropriate coordinates, utilising the rotate, scale and offset methods in
    Transforms.hs.

    The Transforms.hs method also has a method called line, which takes in 4 values (the starting x and y, and the ending x and y). As the drawnLines method defined in
    TurtleState already contains these values, we can simply use the map function to call the function on each of those values. In this instance, I'm using a lambda
    function as this function isn't used elsewhere, so it isn't necessary to properly define it. Map then applies this function on the list of drawnLines to generate
    a list of images.

    With all the images we want to draw for a given frame generated, we just need to combine them into one imagine. The superimposeAll method in Layout.hs can be used
    to achieve this, as it takes in a list of images and utilises the <@> operator to fold the images into one singular image. To use this in my solution, I can add
    the turtle image onto the end of the list of drawn lines, and then superimpose that new list. This new image generated is what is returned for the function, as it
    corresponds to ther turtle state at that given frame.
-}
showTurtleState :: TurtleState -> Image
showTurtleState TurtleState{position=(x, y), angle=currentAngle, linesDrawnSoFar=drawnLines} =
    let
        -- creates the image for the turtle and scales it up
        showTurtle :: Image
        showTurtle = offset (round (x)) (round (y)) (rotate angle (scale 3 turtle)) -- round the numbers to the nearest integer as offset only takes integer values -> difference is small (<=0.5 units)
            where angle = round (90 - currentAngle `mod'` 360) -- subtracting from 90 as 'north' is 90 degrees whereas the turtle by default is facing north

        -- creates the lines between each position the user has drawn and converts it into an image
        drawLines :: [Image]
        drawLines = map (\((x1, y1), (x2, y2)) -> line x1 y1 x2 y2) drawnLines -- draws a line between each pair of coordinates in drawnLines
    
    in
        superimposeAll (drawLines ++ [showTurtle]) -- adding showTurtle at the end instead of prepending it so that it is rendered above all lines

{-|
    @PARAM a list of hogo code we want to convert into turtle states
    @RETURN a list of turtle states corresponding to the hogo commands
    
    this method takes in a list of hogocode, and converts it into a list of turtle states. scanl method is utilised to apply the update method to every single command
    within the program to update the turtle state accordingly. the turtle state is accumulated, meaning once we generate a new turtle state, we execute the next hogo
    command on this new state instead of repeatedly using the initial state of the turtle.
-}
convertProgram :: [HogoCode] -> [TurtleState]
convertProgram program = scanl update initialState program

{-|
    @PARAM a list of turtle states we want to animate
    @PARAM a value t which corresponds to the frame we currently want to show
    @RETURN a frame whose image corresponds with the current turtle state (determined by index t)

    this method is responsible for creating a frame for a given turtle state. it takes in a list of all turtle states, as well as the current state we are trying to
    display. it first checks if the current frame t is less than the length of the list of states, as we only want to play the animations once. if it exceeds the 
    current frame, we instead display the last frame consistently to denote the end of the animation. otherwise, we display the frame at the t index of the list of
    states, attained via !!. We then use the showTurtleState metod we defined earlier to take in the give state, convert it into an image and then return that image.
-}
animation :: [TurtleState] -> Int -> Image
animation states t
    | t < length states = showTurtleState (states !! t) -- show the image of the turtle state at index t
    | otherwise = showTurtleState(last states) -- show the image of the last turtle state in the list of states

{-|
    this is the main method of the program, which first asks the user for a file to run via the terminal and collects the user input using the getLine method. once we
    have obtained a filename, we store the contents of the hogo program using the readFile method on the file attained. Text.Megaparsec contains a method called parse,
    which runs a particular parser over a program. We can utilise this to parse the contents of the given file using the parseHogo method (defined in Parser.hs), to
    return a list of hogo code. if we are unable to parse the program, we want to display an error message detailing why. Text.Megaparsec.Error contains a method called
    errorBundlePretty, which takes in the error message we get when we are unable to parse it, and displays the error message in a visually appealing format, displaying
    the specific error, the line in the hogo code program which this error was encountered, the character causing the issue and possible fixes in our case.
    
    With this hogo code, we can then call convertProgram on it to the convert it into a list of turtle states. finally, after we have
    this list of states, we can call the runAnimation method outlined in Hatch.hs. This method takes in a function which converts an integer into an image, and then
    displays the image over a given fps. our animation method earlier can be used with the list of states as a constant first parameter to generate a Int->Image method
    which then displays each state in a particular frame for 1/fps seconds, allowing for animation.
-}
main :: IO ()
main = do
    putStrLn "Enter the name of the Hogo program file:" -- prints the prompt to the command line
    filename <- getLine -- gets the user input from the command line
    contents <- readFile filename -- stores the contents from the command line
    case parse parseHogo "" contents of
        Left err -> putStrLn $ "Error parsing hogo program:\n" ++ errorBundlePretty err -- displays appropriate error message if we can't parse the program
        Right hogocode -> do -- if we can parse the program
            let states = convertProgram hogocode -- convert the parsed program into a list of states
            runAnimation (animation states) -- run an animation on the list of images for each state