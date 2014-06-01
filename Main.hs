import Data.Array
import Data.Maybe
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

size   = 8
sep    = 10.0
border = 50.0

type State = Array (Int, Int) (Maybe Int)
data Game = Game {state :: State, score :: Int, randomGen :: StdGen} 
            deriving (Show)
colors = ["#E9DDD2", "#E8D8BD", "#EDA16A", "#F08355", "#F16951", "#F14C33", "#E7C365"]
totalWidth = ((fromIntegral size) * border + (fromIntegral (size + 1)) * sep)

htmlColor :: String -> Color
htmlColor ['#', r1, r2, g1, g2, b1, b2] 
             = makeColor8 red green blue 255
               where [red, green, blue] = fmap toInt [[r1, r2], [g1, g2], [b1, b2]]
                     toInt str = read $ "0x" ++ str
htmlColor _  = black

square :: Float -> Picture
square x = Polygon [(0, 0), (0, x), (x, x), (x, 0)]

grid :: Int -> Picture
grid n = Pictures $ background : placeholders
       where background   = Color (htmlColor "#AC9D8F") $ square totalWidth
             block        = Color (htmlColor "#C1B3A6") $ square border
             positions    = [(x, y) | x <- [1..n], y <- [1..n]]
             placeholders = zipWith pan positions (replicate (n*n) block)
             shift n      = sep * (fromIntegral n) + border * ((fromIntegral n)-1)
             pan (x, y)   = Translate (shift x) (shift y)
             
card :: Int -> Picture
card n = Pictures [block, num]
         where block = Color color $ square border
               num   = Color black $ scaleText border border $ show n
               color = maybe red htmlColor (lookup n plate)
               plate = zip [2^i | i <- [1..]] colors

render :: Game -> Picture
render g = Translate (- totalWidth / 2) (- totalWidth / 2) $ Pictures $ grid size : cards
       where cards = [paste e | e <- assocs (state g)]
             shift n      = sep * (fromIntegral n) + border * ((fromIntegral n)-1)
             paste ((x, y), Just n) = Translate (shift y) (shift x) $ card n
             paste (_,     Nothing) = Blank


scaleText width height s = Translate xShift yShift $ Scale maxScale maxScale $ Text s
    where rawWidth  = fromIntegral (length s) * 75.0
          rawHeight = 100.0
          xScale    = width / rawWidth * 0.618
          yScale    = height / rawHeight * 0.618
          maxScale  = min xScale yScale
          xShift    = width / 2.0 - rawWidth * maxScale / 2.0
          yShift    = height / 2.0 - rawHeight * maxScale / 2.0

keyhandler :: Event -> Game -> Game
keyhandler (EventKey (SpecialKey KeyLeft)  Down _ _) game = batch moveLeft  game
keyhandler (EventKey (SpecialKey KeyRight) Down _ _) game = batch moveRight game
keyhandler (EventKey (SpecialKey KeyUp )   Down _ _) game = batch moveUp    game
keyhandler (EventKey (SpecialKey KeyDown)  Down _ _) game = batch moveDown  game
keyhandler _ game = game

batch :: (Int -> State -> State) -> Game -> Game
batch action g = if s /= s' && (length $ emptyPosition s) /= (length $ emptyPosition s')
                    then g {state = s'}
                    else generate $ g {state = s'}
               where s = state g
                     s'  = apply s $ fmap action [1..size]

apply = foldl (\ x f -> f x)

generate :: Game -> Game
generate game = game {state = s', randomGen = g''}
              where g        = randomGen game
                    s        = state game
                    ps       = emptyPosition s
                    (p, g')  = randomR (0, length ps - 1) g
                    (v, g'') = randomR (1, 2) g'
                    s'       = s // [(ps !! p, Just (v * 2))]

emptyPosition :: State -> [(Int, Int)]
emptyPosition s = [(r, c) | r <- [1..size], c <- [1..size], isNothing (s ! (r, c))]

moveLeft :: Int -> State -> State
moveLeft r s = s // [((r, c), Nothing) |  c <- [1..size]] // [((r, i), v) | (i, v) <- zip [1..size] (collapse values)]
                where values = filter (/= Nothing) [s ! (r, c) | c <- [1..size]]

moveRight :: Int -> State -> State
moveRight r s = s // [((r, c), Nothing) |  c <- [1..size]] // [((r, i), v) | (i, v) <- zip (reverse [1..size]) (collapse values)]
                where values = filter (/= Nothing) [s ! (r, c) | c <- reverse [1..size]]

moveUp :: Int -> State -> State
moveUp c s = s // [((r, c), Nothing) |  r <- [1..size]] // [((i, c), v) | (i, v) <- zip (reverse [1..size]) (collapse values)]
                where values = filter (/= Nothing) [s ! (r, c) | r <- reverse [1..size]]

moveDown :: Int -> State -> State
moveDown c s = s // [((r, c), Nothing) |  r <- [1..size]] // [((i, c), v) | (i, v) <- zip [1..size] (collapse values)]
                where values = filter (/= Nothing) [s ! (r, c) | r <- [1..size]]

collapse ::  [Maybe Int] -> [Maybe Int]
collapse [] = []
collapse (Just x: Just y:z) | x == y   = Just (2 * x) : collapse z
                            | otherwise = Just x : collapse (Just y:z)
collapse xs = xs

step :: Float -> Game -> Game
step deplay game = game

createGameOfSize :: Int -> Game
createGameOfSize n =  generate Game { state = listArray ((1, 1), (n, n)) [Nothing | x <- [1..n], y <- [1..n]], score = 0, randomGen = mkStdGen 1023}

main = play (InWindow "Game of 2048" (800, 600) (100, 100))
            (htmlColor "#F9F6EB")  0 (createGameOfSize size) render keyhandler step
