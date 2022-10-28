module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.
-- Self-defined help1er functio to convert deg to rad
radians :: Floating x => x -> x
radians degrees = pi * (degrees / 180)

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (x, y, z) = x

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (x, y, z) = y

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (x, y, z) = z

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar char rules = head [snd pair | pair <- rules, fst pair == char]

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne "" _           = ""
expandOne (ax:axs) rules = lookupChar ax rules ++ expandOne axs rules

{- Another way of defining expandOne is using list comprehension:
expandOne axioms rules = concat [lookupChar i rules | i <- axioms]
-}

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand axioms 0 _ = axioms
expand axioms x rules = expand (expandOne axioms rules) (x-1) rules

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move 'L' angle (cartesian, orientation) = (cartesian, orientation + angle)
move 'R' angle (cartesian, orientation) = (cartesian, orientation - angle)
move 'F' angle ((a, b), orientation)    = ((x, y), orientation)
  where 
    x = a + cos (radians orientation)
    y = b + sin (radians orientation)
move _ angle state = error "Invalid Command: has to be L, R, or F"
 

--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--
help1 :: Commands -> Angle -> Colour -> TurtleState -> (Commands, [ColouredLine])
-- Patterm matching with help1 for '[' ']'
help1 [] angle colour state       = ("", [])
help1 ('[':cs) angle colour state = (restcommands, colouredlines ++ lines)
  where
    (commandsbetween, colouredlines) = help1 cs angle colour state
    (restcommands, lines)            = help1 commandsbetween angle colour state
help1 (']':cs) angle colour state = (cs, [])

-- Pattern matching for commands outside []
help1 (c:cs) angle colour state
  | c == 'F'  = (a, currentLine : b)
  | otherwise = help1 cs angle colour newState
  where
    (a, b)          = help1 cs angle colour newState
    newState@(e, _) = move c angle state
    currentLine     = (fst state, e, colour)


trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 (c:cs) angle colour
  = snd(help1 (c:cs) angle colour ((0, 0), 90))

-- My helper function 2
-- Pattern matching
help2 :: Commands -> Angle -> Colour -> TurtleState -> Stack -> [ColouredLine]
help2 [] _ _ _ _       = []
help2 ('F':cs) angle colour state stack = (state1, fmove1, colour) : help2new
  where
    state1   = fst state
    fmove    = move 'F' angle state
    fmove1   = fst fmove
    help2new = help2 cs angle colour fmove stack

help2 (c:cs) angle colour state stack
  | c == '['  = help2 cs angle colour state (state:stack)
  | c == ']'  = help2 cs angle colour (head stack) (tail stack)
  | otherwise = help2 cs angle colour (move c angle state) stack

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 (c:cs) angle colour
  = help2 (c:cs) angle colour ((0, 0), 90) []

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush, canopy, galaxy :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

canopy
  = (30.0,
     "M",
     [('M', "M[+MM][-MM]M[-M][+M]M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

galaxy
  = (36.0,
     "[M]++[M]++[M]++[M]++[M]",
     [('M', "+M--M---M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
