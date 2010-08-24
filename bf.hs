import Char (ord,chr)

data State = State {
    leftTape :: [Integer],
    rightTape :: [Integer],
    leftCommands :: [Char],
    rightCommands :: [Char],
    input :: [Char],
    output :: [Char]
    } deriving (Eq,Show)

_jumpRight :: Integer -> State -> State
_jumpRight 0 state = state
_jumpRight level (State lts rts lcs ('[':rcs) i o) = _jumpRight (level+1) (State lts rts ('[':lcs) rcs i o)
_jumpRight level (State lts rts lcs (']':rcs) i o) = _jumpRight (level-1) (State lts rts (']':lcs) rcs i o)
_jumpRight level (State lts rts lcs (c:rcs) i o) = _jumpRight level (State lts rts (c:lcs) rcs i o)

_jumpLeft :: Integer -> State -> State
_jumpLeft 0 state = state
_jumpLeft level (State lts rts (']':lcs) rcs i o) = _jumpLeft (level+1) (State lts rts lcs (']':rcs) i o)
_jumpLeft level (State lts rts ('[':lcs) rcs i o) = _jumpLeft (level-1) (State lts rts lcs ('[':rcs) i o)
_jumpLeft level (State lts rts (c:lcs) rcs i o) = _jumpLeft level (State lts rts lcs (c:rcs) i o)

jumpRight :: State -> State
jumpRight (State lts rts lcs (c:rcs) i o) = _jumpRight 1 (State lts rts (c:lcs) rcs i o)

jumpLeft :: State -> State
jumpLeft (State lts rts (c:lcs) rcs i o) = _jumpLeft 1 (State lts rts lcs (c:rcs) i o)

_bf :: State -> State
_bf state@(State {rightCommands = []}) = state -- Termination: out of commands.
_bf (State lt [] lc rc i o) = _bf (State lt [0] lc rc i o) -- needs more tape (right)!
_bf (State [] rt lc rc i o) = _bf (State [0] rt lc rc i o) -- needs more tape (left)! 
-- actual BrainFuck command implementation
_bf (State (t:lt) rt lc ('<':rc) i o) = _bf (State lt (t:rt) ('<':lc) rc i o) -- <
_bf (State lt (t:rt) lc ('>':rc) i o) = _bf (State (t:lt) rt ('>':lc) rc i o) -- >
_bf (State lt (t:rt) lc ('+':rc) i o) = _bf (State lt ((succ t):rt) ('+':lc) rc i o) -- +
_bf (State lt (t:rt) lc ('-':rc) i o) = _bf (State lt ((pred t):rt) ('-':lc) rc i o) -- -
_bf state@(State lt (0:rt) lc ('[':rc) i o) = _bf (jumpRight state) -- [
_bf state@(State lt (t:rt) lc (']':rc) i o) | t /= 0 = _bf (jumpLeft state) -- ]
_bf (State lt (t:rt) lc ('.':rc) i o) = _bf (State lt (t:rt) ('.':lc) rc i ((chr (fromInteger t)):o)) -- .
_bf (State lt (_:rt) lc (',':rc) (i:is) o) = _bf (State lt ((toInteger (ord i)):rt) (',':lc) rc is o) -- ,
_bf (State lt rt lc (c:rc) i o) = _bf (State lt rt (c:lc) rc i o) -- ignore everything else

bf :: String -> String -> String
bf commands input = reverse (output (_bf (State [] [] "" commands input "")))
