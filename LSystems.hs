module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

eps = 0.0000000001
degToRad = pi/180

initialState :: TurtleState
initialState = ((0, 0), 90)

testString = "FF[RF[RF][LF][FF]][LF[RF][LF][FF]][FFF[RF][LF][FF]]"

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (x, _, _) = x

-- |Returns the base string for the given system.
base :: System -> String
base (_, x, _) = x

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, x) = x

-- |Look up a character in the given set of rules.
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar key (x:xs)
   | fst(x) == key = snd(x)
   | otherwise = lookupChar key xs

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne givenRule []
   = []
expandOne givenRule (x:xs) 
   = (lookupChar x givenRule) ++ (expandOne givenRule xs) 
--Ask PPT about optimization

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand givenRule command 0
   = command
expand givenRule command n
   = expand givenRule (expandOne givenRule command) (n-1)

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move command ((x, y), angle) degree
   | command == 'R' = ((x, y), (changeAngle angle (-degree)))
   | command == 'L' = ((x, y), (changeAngle angle degree))
   | otherwise      = move1 ((x, y), angle)

changeAngle :: Float -> Float -> Float
changeAngle currentAngle change
   | newAngle >= maxAngle = newAngle - maxAngle
   | newAngle <  minAngle = maxAngle + newAngle 
   | otherwise            = newAngle
   where
      newAngle = currentAngle + change
      maxAngle = 360
      minAngle = 0

move1 :: TurtleState -> TurtleState
move1 ((x, y), angle)
   = (((x + (cos newAngle)), (y + (sin newAngle))), angle)
   where
      newAngle = angle * degToRad

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 commands angleChange colour 
   = result
   where
      (result, _) =  trace1' commands angleChange initialState colour
      trace1' :: String -> Float -> TurtleState -> Colour -> ([ColouredLine], String)
      trace1' [] _ _ _ 
         = ([], [])
      trace1' ('[' : cs) angleChange currentState colour
         = (recursionLines ++ followingLines, remainingCommands')
         where
            (recursionLines, remainingCommands) = trace1' cs angleChange currentState colour 
            (followingLines, remainingCommands') = trace1' remainingCommands angleChange currentState colour
      trace1' (']' : cs) angleChange currentState colour
         = ([], cs)
      trace1' (c:cs) angleChange currentState@((x, y), angle) colour
         | c == 'F'  = (((x, y), (x', y'), colour) : returnedLines, remainingCommands)
         | otherwise = (returnedLines, remainingCommands) 
         where
            newState@((x', y'), angle') = move c currentState angleChange
            (returnedLines, remainingCommands) = trace1' cs angleChange newState colour

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 commands angleChange colour
   = trace2' commands angleChange colour initialState [initialState]  
   where
      trace2' :: String -> Float -> Colour -> TurtleState -> [TurtleState] -> [ColouredLine]
      trace2' [] _ _ _ _
         = []
      trace2' (c : cs) angleChange colour currentState@((x, y), angle) stack@(oldState : stack')
         | c == '['  = trace2' cs angleChange colour currentState (currentState : stack)
         | c == ']'  = trace2' cs angleChange colour oldState stack'
         | c == 'F'  =  ((x, y), (x', y'), colour) : (trace2' cs angleChange colour newState stack)  
         | otherwise = trace2' cs angleChange colour newState stack
         where
            newState@((x', y'), angle') = move c currentState angleChange    

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
