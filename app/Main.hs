module Main where

import Graphics.Gloss
import Lib
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Text.Printf

--Miguel Quinaz Pereira , up201708286
--Work based on following link and specific sintax problems in stackoverflow,etc
--http://andrew.gibiansky.com/blog/haskell/haskell-gloss/

width, height, offset,screenWidth,screenHeight :: Int
width = 300
height = 300
offset = 100

screenWidth = 600
screenHeight = 500

wallWidth,wallHeight :: Float
wallWidth = 19
wallHeight = 318

--game state, coordinates/velocity ball, player x coordinate
--pressKey to avoid double shift (key press/key release bug)
--boxesLevel to send only the game state in update function
data Game = Init
 { ballPos :: (Float, Float)
 ,ballVel :: (Float, Float) 
 ,playerx :: Float  
 ,lives::Int
 ,pressKey :: Bool 
 ,score :: Int
 ,boxesLevel :: [Level]   
 } deriving Show

data Level = InitLevel
 { boxPos :: (Float,Float)
 , level :: Int
 } deriving Show

boxWidth :: Float
boxWidth = 30

boxHeight :: Float
boxHeight = 15

--auxiliary function to fill initial boxes
appendBox:: Int -> Float -> Float -> [Level] -> [Level]
appendBox level x y lista =   
  (InitLevel{ boxPos = (x,y), level = level}) : lista 

--Since in the chosen width we can fill 11 rectangles with size
--(30,15) then this function tries to do this without hard-code
appendBoxesLevel :: Int -> Int -> Float -> Float -> [Level] -> [Level]
appendBoxesLevel cont level x y lisBoxes = 
  case cont of
    12 -> lisBoxes
    _ -> appendBoxesLevel (cont+1) level (fromIntegral ((-125)+25*cont)) y (appendBox level x y lisBoxes) 

--Same concept as before but there are 6 levels (decrementing)
appendBoxesAll :: Int -> Float -> Float -> [Level] -> [Level]
appendBoxesAll level x y lisBoxes = 
  case level of
    -1 -> lisBoxes
    _ -> appendBoxesAll (level-1) x y (appendBoxesLevel 1 level x (y-fromIntegral(15*(5-level))) lisBoxes) 


level1 :: [Level]
--level1 = [InitLevel
--  { boxPos = ( -125,125)
--  , level = 5
--  }]

--Initializing the boxes
level1 = appendBoxesAll 5 (-125) (125) []

initialState :: Game
initialState = Init
  { ballPos = ( 0, 0 )
  , ballVel = (50, -100)
  , playerx = 0
  , lives = 10
  , score = 0
  , pressKey = False
  , boxesLevel = level1
  }

window :: Display
window = InWindow "Nice Window" (screenWidth, screenHeight) (offset, offset)

background :: Color
background = black

playerHeight :: Float
playerHeight = -120

paddleHeight :: Float
paddleHeight = 10

paddleWidth :: Float
paddleWidth = 50

--mapping of int (level in record) to color
mapColor :: Int -> Color
mapColor level = 
  case level of
    0 -> blue
    1 -> green
    2 -> chartreuse
    3 -> yellow
    4 -> orange
    5 -> red
    _ -> white

--mapping of int(level in record) in points
mapPoint :: Int -> Int
mapPoint level = 
  case level of
    0 -> 1
    1 -> 1
    2 -> 4 
    3 -> 4
    4 -> 7
    5 -> 10
    _ -> 0

--draw a picture list from the record list (boxesLevel in game)
drawBoxesFromList :: [Level] -> [Picture] -> Picture
drawBoxesFromList listBoxes acc = 
  case listBoxes of
    [] -> pictures acc
    el:li -> do{
               let (aux1,aux2) = boxPos el
               ;drawBoxesFromList li ( (translate aux1 aux2 $ color (mapColor (level el)) $ rectangleSolid boxWidth boxHeight) : acc)}

--Draw a game state (convert it to a picture).
render :: Game -> Picture
render game = 
  pictures [ball, walls, boxes, scoreN,lifeN,
            mkPaddle orange (playerx game) playerHeight]
  where
    -- The ball.
    ball = uncurry translate (ballPos game) $ color ballColor $ circleSolid 10
    ballColor = dark red
    -- boxes
    boxes :: Picture
    --boxes = drawBoxesFromList level1 []
    boxes = drawBoxesFromList (boxesLevel game) []

    scoreN :: Picture
    scoreN = pictures [scale 0.5 0.5 (translate 125 325 $ color (greyN 0.5) $ text (show (score game))) ]

    lifeN :: Picture
    lifeN = pictures [scale 0.5 0.5 (translate (-325) 325 $ color (greyN 0.5) $ text (show (lives game))) ]

    --  The bottom and top walls.
    wallV :: Float -> Picture
    wallV offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid wallHeight wallWidth
    --  The left and right walls.
    wallH :: Float -> Picture
    wallH offset =
      translate offset 0 $
        color wallColor $
          rectangleSolid wallWidth wallHeight

    wallColor = greyN 0.5
    walls = pictures [wallV 150, wallV (-150), wallH 150, wallH (-150)] 
  
    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight]

    paddleColor = light (light blue)

moveBall :: Float    -- The number of seconds since last update
         -> Game -- The initial game state
         -> Game -- A new game state with an updated ball position
moveBall seconds game = game { ballPos = (x',y') }
  where
    -- Old locations and velocities.
    (x, y) = ballPos game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

fps :: Int
fps = 60

flag :: Int
flag = 0

--Function to go through all boxes and return the level in which the ball collided (second version of commented function at end)
searchCollision :: [Level] -> Position -> Float -> Level
searchCollision listBoxes (x,y) radius = 
  case listBoxes of
    [] -> InitLevel{boxPos = (0,0),level=(-1)}
    el:li -> if insideBox (boxPos el) (x,y) radius
              then el
               else searchCollision li (x,y) radius

--colision with boxes
boxBounce :: Game -> Game
boxBounce game =  game { ballVel = (vx', vy'),boxesLevel = lis,score = (score game) + auxPoint }
      where
        radius = 10

        (vx,vy) = ballVel game
        (vx',vy',lis,auxPoint) = do{
             let el = (searchCollision (boxesLevel game) (ballPos game) radius)  --we get the record Level and filter to get new list witouth 
             ;if level el /= -1
               then 
                 (vx,-vy,(filter ( (/= boxPos el ) . boxPos) (boxesLevel game)),mapPoint (level el))
               else
                (vx,vy,boxesLevel game,0)}

--check if ball is inside a certain box. (15/2 -> boxWidth / 30/2 -> boxHeight)
insideBox :: Position -> Position -> Radius -> Bool
insideBox (bx,by) (x,y) radius = upCond && downCond
  where
    upCond =((y+radius >= by-7.5) && (y+radius <= by+7.5) ) || ((y-radius >= by-7.5) && (y-radius <= by+7.5))
    downCond =((x+radius >= bx-15 ) && (x+radius <=  bx+15)) || ((x-radius >= bx-15 ) && (x-radius <=  bx+15))


type Radius = Float 
type Position = (Float, Float)

-- Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: Game -> Game
paddleBounce game = game { ballVel = (vx', vy') }
  where
    radius = 10

    (vx,vy) = ballVel game
    posx = playerx game
    (vx',vy') = if paddleCollision (ballPos game) radius posx
          then
             (vx,-vy)
           else
            (vx,vy)

paddleCollision :: Position -> Radius -> Float -> Bool 
paddleCollision ( x , y) radius posx = padCollision
  where
    padCollision    = (y - radius <=  playerHeight + paddleHeight/2) && (x - radius >= -paddleWidth/2 + posx) && (x - radius <= posx + paddleWidth/2)

-- Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: Game -> Game
wallBounce game = game { ballPos = (x,y),ballVel = (vx', vy'),playerx=px,lives=lvar,score=svar,pressKey=pvar,boxesLevel=listBoxes }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    boxesLevelAux = boxesLevel game
    pressKeyAux = pressKey game
    scoreAux = score game
    playerAux = playerx game
    (ballAuxx,ballAuxy) = ballPos game
    (vx, vy) = ballVel game
    livesAux = lives game
    (x,y,vx',vy',px,lvar,svar,pvar,listBoxes) = if wallCollisionV (ballPos game) radius
          then
             -- Update the velocity. Lost game
             if livesAux > 1
               then 
                (0,0,50,-100,0,(livesAux-1),scoreAux,pressKeyAux,boxesLevelAux)
                 else
                    (0,0,50,-100,0,10,0,False,level1)
           else
            if wallCollisionH (ballPos game) radius
          	then
                 -- Update the velocity.
                 (ballAuxx,ballAuxy,-vx,vy,playerAux,livesAux,scoreAux,pressKeyAux,boxesLevelAux)
                 else
                 -- Do nothing. Return the old velocity.
                 (ballAuxx,ballAuxy,vx,vy,playerAux,livesAux,scoreAux,pressKeyAux,boxesLevelAux)

-- Given position and radius of the ball, return whether a collision occurred.
wallCollisionV :: Position -> Radius -> Bool 
wallCollisionV ( _ , y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 + wallWidth/2
    bottomCollision = y + radius >=  fromIntegral width / 2 - wallWidth/2

wallCollisionH :: Position -> Radius -> Bool 
wallCollisionH ( x , _ ) radius = leftCollision || rightCollision
  where
    leftCollision = x - radius <= -fromIntegral width / 2 + wallWidth/2
    rightCollision = x + radius >= fromIntegral width / 2 - wallWidth/2

-- Update the game by moving the ball and bouncing off walls.
update :: Float -> Game -> Game
update seconds = wallBounce . paddleBounce . boxBounce . moveBall seconds

-- Respond to key events.
handleKeys :: Event -> Game -> Game
-- For an 'r' keypress, reset the game
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { ballPos = ( 0, 0 )
  , ballVel = (50, -100)
  , playerx = 0
  , lives = 10
  , score = 0
  , pressKey = False
  , boxesLevel = level1
  }

-- limit player movement and a boolean pressKey because sometimes if I clicked the paddle would go 2 times (keypress keyrelease I think)
handleKeys (EventKey (SpecialKey KeyLeft)  _ _ _) game =
  if pressKey game && (playerx game) >= -fromIntegral width/2 + 10 + paddleWidth/2 + wallWidth/2
  then
    game { playerx = (playerx game - 10)
    , pressKey = False
    }
  else
    game { pressKey = True
    }
--Prototype function to hold the key to make paddle go, i couldn't implement this
{-
handleInput (EventKey k ks _ _) s
  | SpecialKey KeyDown <- k
    , Down <- ks
      = s { isPositive = not (isPositive s) }
-}
handleKeys (EventKey (SpecialKey KeyRight)  _ _ _) game =
  if pressKey game && (playerx game) <= fromIntegral width/2 - 10 - paddleWidth/2 - wallWidth/2
  then 
    game { playerx = (playerx game + 10)
    , pressKey = False
    }
  else
     game { pressKey = True }
-- Do nothing for all other events.
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update


{-
--This may be ignored, previous / attempts to make some functionalities

-- 1 function wallBounce prototype to reset the game when lives go to 0.
-- function 2,3,4 were first attempts to create the list of boxes "dinamically" witouth hardcoded like the list created at end
--boxBounce game and (boxesLevel game) was a assigment/function to create another list (List.filter in Ocaml is different atleast I lost time in this)




-- Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: Game -> Game
wallBounce game = game { ballPos = (x,y),ballVel = (vx', vy'),playerx=px,lives=lvar,score=svar,pressKey=pvar,boxesLevel=listBoxes }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    boxesLevelAux = boxesLevel game
    pressKeyAux = pressKey game
    scoreAux = score game
    playerAux = playerx game
    (ballAuxx,ballAuxy) = ballPos game
    (vx, vy) = ballVel game
    livesAux = lives game
    (x,y,velx,vy',px,lvar,svar,pvar,listBoxes) = if wallCollisionV (ballPos game) radius
          then
             -- Update the velocity. Lost game
             if livesAux > 0
               then 
                (ballAuxx,ballAuxy,vx,-vy,playerAux,(livesAux-1),scoreAux,pressKeyAux,boxesLevelAux)
                 else
                    (0,0,50,-100,0,10,0,False,level1)
           else
            -- Do nothing. Return the old velocity.
            (ballAuxx,ballAuxy,velx,vy,px,lvar,svar,pvar,listBoxes)

    vx' = if wallCollisionH (ballPos game) radius
          then
             -- Update the velocity.
             -vx
           else
            -- Do nothing. Return the old velocity.
            vx




drawBoxes :: Int -> Float -> Float -> Picture -> Picture
drawBoxes level x y lista =   (translate x y $ color (mapColor level) $ rectangleSolid 30 15) : lista 
	
drawBoxesLevel :: Int -> Float -> Float -> Picture -> Picture
drawBoxesLevel level x y lisBoxes = 
  case level of
    0 -> lisBoxes
    _ -> (drawBoxesLevel (level-1) x y ( drawBoxes level x y lisBoxes ) )

searchBoxes :: [Level] -> Position
searchBoxes lisBoxes = 
  case lisBoxes of
    [] -> (0,0)
    el:li -> boxPos $ el 

--((boxesLevel game) = filter (\(x1,y1) -> x1 != removeBlockx && y1 != removeBlocky ) (boxesLevel game))
--if flag ==1 then game { boxesLevel = filter ( (== (1,1)) . boxPos) (boxesLevel game) } else game
        (vx',vy') = if insideBox coordAux (ballPos game) radius
          then 
             do {
	       let lis = filter ( (== coordAux) . boxPos) (boxesLevel game)
	       ;(vx,-vy)
		}
           else
            (vx,vy)


 boxes = pictures
      [ translate (-150) 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      , translate (-120) 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate (-90) 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate (-60) 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate (-30) 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate 0 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate 30 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight    
      ,translate 60 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate 90 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate 120 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate 150 125 $ color (mapColor 5) $ rectangleSolid boxWidth boxHeight
      ,translate (-125) 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      , translate (-100) 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate (-75) 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate (-50) 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate (-25) 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate 0 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate 25 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight    
      ,translate 50 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate 75 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate 100 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate 125 110 $ color (mapColor 4) $ rectangleSolid boxWidth boxHeight
      ,translate (-125) 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate (-100) 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate (-75) 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate (-50) 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate (-25) 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate 0 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate 25 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight  
      ,translate 50 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate 75 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate 100 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate 125 95 $ color (mapColor 3) $ rectangleSolid boxWidth boxHeight
      ,translate (-125) 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate (-100) 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate (-75) 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate (-50) 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate (-25) 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate 0 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate 25 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight  
      ,translate 50 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate 75 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate 100 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate 125 80 $ color (mapColor 2) $ rectangleSolid boxWidth boxHeight
      ,translate (-125) 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate (-100) 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate (-75) 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate (-50) 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate (-25) 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate 0 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate 25 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight 
      ,translate 50 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate 75 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate 100 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate 125 65 $ color (mapColor 1) $ rectangleSolid boxWidth boxHeight
      ,translate (-125) 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate (-100) 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate (-75) 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate (-50) 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate (-25) 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate 0 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate 25 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate 50 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate 75 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate 100 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight
      ,translate 125 50 $ color (mapColor 0) $ rectangleSolid boxWidth boxHeight]

boxBounce :: Game -> Game
boxBounce game = 
  case (boxesLevel game) of
    [] -> game
    el:li -> game { ballVel = (vx', vy'),boxesLevel = lis,score = (score game) + auxPoint }
      where
        radius = 10

        coordAux = boxPos $ el
        (vx,vy) = ballVel game
        (vx',vy',lis,auxPoint) = if insideBox coordAux (ballPos game) radius
         then 
           (vx,-vy,(filter ( (/= coordAux) . boxPos) (boxesLevel game)),mapPoint (level el))
           else
            (vx,vy,boxesLevel game,0)



wallBounce game = game { ballVel = (vx', vy'),lives = l }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.

    (vx, vy) = ballVel game
    livesAux = lives game
    (vy',l) = if wallCollisionV (ballPos game) radius
          then
             -- Update the velocity. Lost game
             (-vy,livesAux-1)
           else
            -- Do nothing. Return the old velocity.
            (vy,livesAux)

    vx' = if wallCollisionH (ballPos game) radius
          then
             -- Update the velocity.
             -vx
           else
            -- Do nothing. Return the old velocity.
            vx
-}
