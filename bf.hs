import Char (ord,chr)
import System(getArgs)

data State = State {
    leftTape :: [Integer],
    rightTape :: [Integer],
    leftCommands :: [Char],
    rightCommands :: [Char],
    input :: [Char]
    } deriving (Eq,Show)

_jumpRight :: Integer -> State -> State
_jumpRight 0 state = state
_jumpRight level (State lts rts lcs ('[':rcs) i) = _jumpRight (level+1) (State lts rts ('[':lcs) rcs i)
_jumpRight level (State lts rts lcs (']':rcs) i) = _jumpRight (level-1) (State lts rts (']':lcs) rcs i)
_jumpRight level (State lts rts lcs (c:rcs) i) = _jumpRight level (State lts rts (c:lcs) rcs i)

_jumpLeft :: Integer -> State -> State
_jumpLeft 0 state = state
_jumpLeft level (State lts rts (']':lcs) rcs i) = _jumpLeft (level+1) (State lts rts lcs (']':rcs) i)
_jumpLeft level (State lts rts ('[':lcs) rcs i) = _jumpLeft (level-1) (State lts rts lcs ('[':rcs) i)
_jumpLeft level (State lts rts (c:lcs) rcs i) = _jumpLeft level (State lts rts lcs (c:rcs) i)

jumpRight :: State -> State
jumpRight (State lts rts lcs (c:rcs) i) = _jumpRight 1 (State lts rts (c:lcs) rcs i)

jumpLeft :: State -> State
jumpLeft (State lts rts lcs rcs i) = _jumpLeft 1 (State lts rts lcs rcs i)

_bf :: State -> String
_bf state@(State {rightCommands = []}) = "" -- Termination: out of commands.
_bf (State lt [] lc rc i) = _bf (State lt [0] lc rc i) -- needs more tape (right)!
_bf (State [] rt lc rc i) = _bf (State [0] rt lc rc i) -- needs more tape (left)!
-- actual BrainFuck command implementation
_bf (State (t:lt) rt lc ('<':rc) i) = _bf (State lt (t:rt) ('<':lc) rc i) -- <
_bf (State lt (t:rt) lc ('>':rc) i) = _bf (State (t:lt) rt ('>':lc) rc i) -- >
_bf (State lt (t:rt) lc ('+':rc) i) = _bf (State lt ((succ t):rt) ('+':lc) rc i) -- +
_bf (State lt (t:rt) lc ('-':rc) i) = _bf (State lt ((pred t):rt) ('-':lc) rc i) -- -
_bf state@(State lt (0:rt) lc ('[':rc) i) = _bf (jumpRight state) -- [
_bf state@(State lt (t:rt) lc (']':rc) i) | t /= 0 = _bf (jumpLeft state) -- ]
_bf (State lt (t:rt) lc ('.':rc) i) =  (chr (fromInteger t)) : (_bf (State lt (t:rt) ('.':lc) rc i)) -- .
_bf (State lt (_:rt) lc (',':rc) (i:is)) = _bf (State lt ((toInteger (ord i)):rt) (',':lc) rc is) -- ,
_bf (State lt rt lc (c:rc) i) = _bf (State lt rt (c:lc) rc i) -- ignore everything else

bf :: String -> String -> String
bf commands input = _bf (State [] [] "" commands input)

main = do
    args <- getArgs
    commands <- readFile (head args)
    interact (bf commands)
