module Notesack.Boundary ( 
  boundary, negativeBoundary
) where

import Notesack.Types ( Box(Box) )

-- I wrote this module previously where 
-- a (Box l r u d) was [l,r) x [u,d)
--                 not [l,r] x [u,d]
-- so instead of rewriting and redebugging,
-- well map everything to Box2 and use the 
-- previously implemented function as it was
-- except using Box2 which is what Box was... 
data Box2 = Box2 Int Int Int Int
to2 (Box l r u d) = Box2 l (r+1) u (d+1)

-- ┌──┬──┐
-- │  │  │
-- │  │  │
-- ├──┼──┤
-- │  │  │
-- │  │  │
-- └──┴──┘
--
-- In order as they appear:
--             ┌    ─     ┬      ┐   
data Bracket = RD | LR  | LRD  | LD 
            -- │    ├     ┼      ┤    
             | UD | RUD | LRUD | LUD
            -- └    ┴     ┘
             | RU | LRU | LU   | E

toChar RD   = '\9484' 
toChar LR   = '\9472' 
toChar LRD  = '\9516' 
toChar LD   = '\9488' 
toChar UD   = '\9474' 
toChar RUD  = '\9500' 
toChar LRUD = '\9532' 
toChar LUD  = '\9508' 
toChar RU   = '\9492' 
toChar LRU  = '\9524' 
toChar LU   = '\9496' 
toChar E    = ' '

instance Show Bracket where
  show b = [toChar b]

-- rotate a Bracket clockwise
rotate = fromTup . rotateTup . toTup
  where rotateTup (l,r,u,d) = (d,u,l,r)

-- flip it over the horizontal axis
reflectLR = fromTup . reflectTup . toTup
  where reflectTup (l,r,u,d) = (r,l,u,d)

-- flip it over the vertical axis
reflectUD = fromTup . reflectTup . toTup
  where reflectTup (l,r,u,d) = (l,r,d,u)

-- Conversion: does the (l,r,u,d) portion of the bracket get drawn?
toTup   RD        = (0,1,0,1) 
toTup   LR        = (1,1,0,0)
toTup   LRD       = (1,1,0,1)
toTup   LD        = (1,0,0,1)
toTup   UD        = (0,0,1,1)
toTup   RUD       = (0,1,1,1)
toTup   LRUD      = (1,1,1,1)
toTup   LUD       = (1,0,1,1)
toTup   RU        = (0,1,1,0)
toTup   LRU       = (1,1,1,0)
toTup   LU        = (1,0,1,0)
toTup   E         = (0,0,0,0)
fromTup (0,1,0,1) =        RD   
fromTup (1,1,0,0) =        LR   
fromTup (1,1,0,1) =       LRD  
fromTup (1,0,0,1) =        LD   
fromTup (0,0,1,1) =        UD   
fromTup (0,1,1,1) =       RUD  
fromTup (1,1,1,1) =      LRUD 
fromTup (1,0,1,1) =       LUD  
fromTup (0,1,1,0) =        RU   
fromTup (1,1,1,0) =       LRU  
fromTup (1,0,1,0) =        LU   
fromTup (0,0,0,0) =         E
-- ^ the inverse of toTup

-- Two brackets b1 b2 drawn on top of each other
-- would be b1 `bracketUnion` b2
bracketUnion :: Bracket -> Bracket -> Bracket
bracketUnion b1 b2 = fromTup $ maxTup (toTup b1) (toTup b2)
  where maxTup (a,b,c,d) (i,j,k,l) = (max a i, max b j, max c k, max d l)

boundary :: Box -> [Box] -> (String, [(Char, Char)], String)
boundary = boundary' True

negativeBoundary :: Box -> [Box] -> (String, [(Char, Char)], String)
negativeBoundary = boundary' False

type BBBB = ([Bracket], [Bracket], [Bracket], [Bracket])

boundary' doBoxToo box boxes = boundary2' doBoxToo (to2 box) (map to2 boxes)

boundary2' :: Bool -> Box2 -> [Box2] -> (String, [(Char, Char)], String)
boundary2' doBoxToo (Box2 l r u d) neighbors =
  let processBox :: Box2 -> BBBB
      processBox b@(Box2 l0 r0 u0 d0)  =
        case (onTL b, onBL b, onTR b, onBR b) of
          -- the four corners
          (True, False, False, False) -> (LU:tail emptyLR, emptyLR, emptyUD, emptyUD)
          (False, True, False, False) -> (tail emptyLR ++ [LD], emptyLR, emptyUD, emptyUD)
          (False, False, True, False) -> (emptyLR, RU:tail emptyLR, emptyUD, emptyUD)
          (False, False, False, True) -> (emptyLR, tail emptyLR ++ [RD], emptyUD, emptyUD)
          -- the more likely case is it is not on a corner
          (False, False, False, False) -> processBoxNotCorner b

      processBoxNotCorner b@(Box2 l0 r0 u0 d0) = 
        case (onL b, onR b, onU b, onD b) of
          (True, _, _, _) -> (intersectL u d u0 d0, emptyLR, emptyUD, emptyUD)
          (_, True, _, _) -> (emptyLR, intersectR u d u0 d0, emptyUD, emptyUD)
          (_, _, True, _) -> (emptyLR, emptyLR,              intersectU l r l0 r0, emptyUD)
          (_, _, _, True) -> (emptyLR, emptyLR,              emptyUD, intersectD l r l0 r0)
          _ -> error "invalid neighbor box given to boundary function"-- emptyBBBB
      
      -- these are on corner functions
      onTL (Box2 l0 r0 u0 d0) = (l,u)     == (r0-1,d0-1)
      onTR (Box2 l0 r0 u0 d0) = (r-1,u)   == (l0,d0-1)
      onBL (Box2 l0 r0 u0 d0) = (l,d-1)   == (r0-1,u0)
      onBR (Box2 l0 r0 u0 d0) = (r-1,d-1) == (l0,u0)

      onL (Box2 l0 r0 u0 d0) = r0-1 == l
      onR (Box2 l0 r0 u0 d0) = l0 == r-1
      onU (Box2 l0 r0 u0 d0) = d0-1 == u
      onD (Box2 l0 r0 u0 d0) = u0 == d-1

      intersectMod a b a0 b0 
        | a <= a0 && b >= b0 = replicate (a0-a) E ++ [RU] ++ replicate (b0-a0-2) LR ++ [LU] ++ replicate (b-b0) E 
        | a >  a0 && b <  b0 = replicate (b-a) LR
        | a <= a0 && b <  b0 = replicate (a0-a) E ++ [RU] ++ replicate (b-a0-1) LR
        | a >  a0 && b >= b0 = replicate (b0-a-1) LR ++ [LU] ++ replicate (b-b0) E

      intersectU a b a0 b0 =                              intersectMod a b a0 b0 
      intersectR a b a0 b0 = (map rotate)               $ intersectMod a b a0 b0 
      intersectD a b a0 b0 = (map reflectUD)            $ intersectMod a b a0 b0 
      intersectL a b a0 b0 = (map (reflectLR . rotate)) $ intersectMod a b a0 b0 

      update :: Box2 -> BBBB -> BBBB
      update box (bl,br,bu,bd) =
        let (cl,cr,cu,cd) = processBox box
            uni as bs = map (uncurry bracketUnion) $ zip as bs
         in (uni bl cl, uni br cr, uni bu cu, uni bd cd)
   
      emptyLR = replicate (d-u) E
      emptyUD = replicate (r-l) E
      emptyBBBB = (emptyLR, emptyLR, emptyUD, emptyUD)

      edgeL = RD:replicate (d-u-2) UD ++ [RU]
      edgeR = LD:replicate (d-u-2) UD ++ [LU]
      edgeU = RD:replicate (r-l-2) LR ++ [LD]
      edgeD = RU:replicate (r-l-2) LR ++ [LU]

      startBBBB = if doBoxToo
                     then (edgeL, edgeR, edgeU, edgeD)
                     else emptyBBBB

      (ol,or,ou,od) = foldr update startBBBB neighbors 

      -- the corners
      tl = (head ol) `bracketUnion` (head ou)
      tr = (head or) `bracketUnion` (last ou)
      bl = (last ol) `bracketUnion` (head od)
      br = (last or) `bracketUnion` (last od)

      middleOf [] = []
      middleOf (x:[]) = []
      middleOf (x:y:[]) = []
      middleOf (x:xs) = init xs

      top = map toChar $ tl:(middleOf ou) ++ [tr]
      bot = map toChar $ bl:(middleOf od) ++ [br]
      mid = zip (map toChar (middleOf ol)) (map toChar (middleOf or))
   in (top, mid, bot)
