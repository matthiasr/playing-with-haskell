import List

data Point = Point Double Double deriving (Eq,Show)
data Direction = L | R | S deriving (Eq,Show)

cmpLowerLeft :: Point -> Point -> Ordering
cmpLowerLeft (Point x1 y1) (Point x2 y2)
    | y1 < y2 = LT
    | y2 < y1 = GT
    | x1 < x2 = LT
    | x2 < x1 = GT
    | otherwise = EQ

getFirst :: [Point] -> Point
getFirst ps = head ( sortBy cmpLowerLeft ps )

setFirst :: [Point] -> [Point]
setFirst = sortBy cmpLowerLeft

cmpAngle :: Point -> Point -> Point -> Ordering
cmpAngle (Point x0 y0) (Point x1 y1) (Point x2 y2)
    | (x1-x0)/(y1-y0) < (x2-x0)/(y2-y0) = LT
    | (x1-x0)/(y1-y0) > (x2-x0)/(y2-y0) = GT
    | otherwise = EQ

sortPoints :: [Point] -> [Point]
sortPoints ps = let p:pss = setFirst ps in
    p : (sortBy (cmpAngle p) pss)

convexHull :: [Point] -> [Point]
convexHull ps = reverse (graham (sortPoints ps) [])

leftRight :: Point -> Point -> Point -> Direction
leftRight (Point x0 y0) (Point x1 y1) (Point x2 y2)
    | (x1-x0)*(y2-y0) - (y1-y0)*(x2-x0) > 0 = L
    | (x1-x0)*(y2-y0) - (y1-y0)*(x2-x0) < 0 = R
    | otherwise = S

graham ps [] | length ps <= 3 = ps
             | otherwise = graham (tail (tail ps)) [ head (tail ps), (head ps) ]
graham [] acc = acc
graham (p0:ps) acc = let {  p1 = head acc; p2 = head (tail acc); rest = tail (tail acc) } in
    if leftRight p0 p1 p2 == L then graham ps [p0,p1,p2]++rest
        else graham ps [p0,p2]++rest
