data State = State {
    leftTape :: [Integer],
    rightTape :: [Integer],
    leftCommands :: [Char],
    rightCommands :: [Char],
    input :: [Char],
    output :: [Char]
    } deriving (Eq,Show)
