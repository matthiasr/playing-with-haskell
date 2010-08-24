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
