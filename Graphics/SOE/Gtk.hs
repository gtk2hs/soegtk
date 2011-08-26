-- -*-haskell-*-
--  SOE implementation based on Gtk and cairo (or Gdk).
--  Some code borrowed from SOE implementation based on OpenGL and GLFW by
--  Paul Liu, http://www.haskell.org/soe/
--
--  Author : Duncan Coutts
--
--  Created: 10 October 2005
--
--  Copyright (C) 2005-2007 Duncan Coutts
--  Copyright (C) 2007 Paul Liu
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-devel@lists.sourceforge.net
-- Stability   : stable
-- Portability : portable (depends on GHC)
-- 
-- An alternative implementation of the graphics library used in The Haskell
-- School of Expression, by Paul Hudak, <http://www.haskell.org/soe/>.
--
-- It has exaclty the same interface as the original implementation
-- "Graphics.SOE". See the original for an API reference.
--
module Graphics.SOE.Gtk (
  runGraphics,
  Title,
  Size,
  Window,
  openWindow,
  getWindowSize,
  clearWindow,
  drawInWindow,
  drawInWindowNow,
  setGraphic,
  closeWindow,
  openWindowEx,
  RedrawMode,
  drawGraphic,
  drawBufferedGraphic,
  Graphic,
  emptyGraphic,
  overGraphic ,
  overGraphics,
  Color (..),
  withColor,
  text,
  Point,
  ellipse,
  shearEllipse,
  line,
  polygon,
  polyline,
  polyBezier,
  Angle,
  arc,
  Region,
  createRectangle,
  createEllipse,
  createPolygon,
  andRegion,
  orRegion,
  xorRegion,
  diffRegion,
  drawRegion,
  getKey,
  getLBP,
  getRBP,
  Event (..),
  maybeGetWindowEvent,
  getWindowEvent,
  getWindowTick,
  Word32,
  timeGetTime,
  word32ToInt
  ) where


import Data.List (foldl')
import Data.Ix (Ix)
import Data.Word (Word32)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Concurrent (forkIO, yield, rtsSupportsBoundThreads)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import qualified System.Time
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Events
import qualified Graphics.UI.Gtk.Gdk.GC as GC

import qualified Graphics.UI.Gtk.Cairo as Gtk.Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Matrix

-------------------
-- Window Functions
-------------------

runGraphics :: IO () -> IO ()
runGraphics main
  | rtsSupportsBoundThreads = do
      Gtk.unsafeInitGUIForThreadedRTS
      forkIO (main >> Gtk.postGUIAsync Gtk.mainQuit)
      Gtk.mainGUI
  | otherwise = do
      Gtk.initGUI
      quitVar <- newIORef False
      forkIO (main >> writeIORef quitVar True)
      let loop = do
            yield
            Gtk.mainIteration
            quit <- readIORef quitVar
            if quit then return ()
                    else loop
      loop
      -- give any windows a chance to close
      Gtk.flush

type Title = String
type Size = (Int, Int)

data Window = Window {
  window :: Gtk.Window,
  canvas :: Gtk.DrawingArea,
  graphicVar :: MVar Graphic,
  eventsChan :: TChan Event,
  tickVar    :: MVar Tick
}

openWindow :: Title -> Size -> IO Window
openWindow title size =
  openWindowEx title Nothing (Just size) drawBufferedGraphic Nothing

openWindowEx :: Title -> Maybe Point -> Maybe Size -> RedrawMode ->
                Maybe Time -> IO Window
openWindowEx title position size (RedrawMode useDoubleBuffer) tick =
  Gtk.postGUISync $ do
  window <- Gtk.windowNew
  Gtk.windowSetTitle window title

  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd window canvas
  Gtk.set canvas [Gtk.widgetCanFocus Gtk.:= True]
  Gtk.widgetSetRedrawOnAllocate canvas False
  Gtk.widgetSetDoubleBuffered canvas useDoubleBuffer

  case position of
    Nothing     -> return ()
    Just (x, y) -> Gtk.windowMove window x y
  case size of
    Nothing              -> return ()
    Just (width, height) -> Gtk.windowSetDefaultSize window width height

  Gtk.widgetShowAll window

  graphicVar <- newMVar emptyGraphic
  eventsChan <- atomically $ newTChan
  tickVar    <- newEmptyMVar

  -- set up the fonts
  pc <- Gtk.Cairo.cairoCreateContext Nothing
  fd <- Gtk.contextGetFontDescription pc
  Gtk.fontDescriptionSetSize fd 12
  Gtk.fontDescriptionSetFamily fd "Sans"
  Gtk.contextSetFontDescription pc fd

  win <- Gtk.widgetGetDrawWindow canvas
  Gtk.onExpose canvas $ \Events.Expose { Events.eventArea = eventArea,
                                      Events.eventRegion = exposeRegion } -> do
    Graphic graphic <- readMVar graphicVar
    win <- Gtk.widgetGetDrawWindow canvas
    Gtk.Cairo.renderWithDrawable win $ do
      -- clip to the exposed region
      Gtk.Cairo.region exposeRegion
      Cairo.clip
      Cairo.paint                 --fill backgound with black
      Cairo.setSourceRGB 1 1 1    --use white default colour
      Cairo.setLineWidth 1.5
      -- actually do the drawing
      graphic pc
    return True

  Gtk.onDelete window $ \_ -> do atomically $ writeTChan eventsChan Closed
                                 Gtk.widgetHide window
                                 return True

  case tick of
    Just frequency -> do
      let resetTick = do
           _ <- tryTakeMVar tickVar
           putMVar tickVar ()
      Gtk.timeoutAddFull (resetTick >> return True)
                         Gtk.priorityDefaultIdle frequency
      return ()
    Nothing -> return ()

  Gtk.onMotionNotify canvas True $ \Events.Motion { Events.eventX=x, Events.eventY=y} ->
    atomically $ writeTChan eventsChan MouseMove {
      pt = (round x, round y)
    } >> return True
  
  let mouseButtonHandler event@Events.Button { Events.eventX=x, Events.eventY=y } = do
        atomically $ writeTChan eventsChan Button {
            pt = (round x,round y),
            isLeft = Events.eventButton event == Gtk.LeftButton,
            isDown = case Events.eventClick event of
                       Gtk.ReleaseClick -> False
                       _                -> True
          }
        return True
  Gtk.onButtonPress canvas mouseButtonHandler
  Gtk.onButtonRelease canvas mouseButtonHandler

  let keyPressHandler Events.Key { Events.eventKeyChar = Nothing } = return True
      keyPressHandler Events.Key { Events.eventKeyChar = Just char, Events.eventRelease = release } =
        atomically $ writeTChan eventsChan Key {
                     char = char,
                     isDown = not release
        } >> return True
  Gtk.onKeyPress canvas keyPressHandler
  Gtk.onKeyRelease canvas keyPressHandler

  Gtk.onSizeAllocate canvas $ \_ -> atomically $ writeTChan eventsChan Resize

  return Window {
    window  = window,
    canvas  = canvas,
    graphicVar = graphicVar,
    eventsChan = eventsChan,
    tickVar    = tickVar
  }

getWindowSize :: Window -> IO Size
getWindowSize win = Gtk.postGUISync $ Gtk.widgetGetSize (canvas win)

clearWindow :: Window -> IO ()
clearWindow win = setGraphic win emptyGraphic

drawInWindow :: Window -> Graphic -> IO ()
drawInWindow win graphic = do
  modifyMVar_ (graphicVar win) (return . overGraphic graphic)
  Gtk.postGUIAsync $ Gtk.widgetQueueDraw (canvas win)

drawInWindowNow :: Window -> Graphic -> IO ()
drawInWindowNow = drawInWindow

setGraphic :: Window -> Graphic -> IO ()
setGraphic win graphic = do
  modifyMVar_ (graphicVar win) (\_ -> return graphic)
  Gtk.postGUIAsync $ Gtk.widgetQueueDraw (canvas win)

closeWindow :: Window -> IO ()
closeWindow win = Gtk.postGUIAsync $ Gtk.widgetHide (window win)

--------------------
-- Drawing Functions
--------------------

newtype RedrawMode = RedrawMode Bool

drawGraphic :: RedrawMode
drawGraphic = RedrawMode False

drawBufferedGraphic :: RedrawMode
drawBufferedGraphic = RedrawMode True

data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Yellow
           | White
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

type Angle = Float

--------------------------------------------------
-- implementation using the cairo API
--

newtype Graphic = Graphic (Gtk.PangoContext -> Cairo.Render ())

emptyGraphic :: Graphic
emptyGraphic = Graphic (\_ -> return ())

overGraphic :: Graphic -> Graphic -> Graphic
overGraphic (Graphic over) (Graphic base) = Graphic (\pc -> base pc >> over pc)

overGraphics :: [Graphic] -> Graphic
overGraphics = foldl1 overGraphic

colorToRGB :: Color -> (Double, Double, Double)
colorToRGB Black   = (0, 0, 0)
colorToRGB Blue    = (0, 0, 1)
colorToRGB Green   = (0, 1, 0)
colorToRGB Cyan    = (0, 1, 1)
colorToRGB Red     = (1, 0, 0)
colorToRGB Magenta = (1, 0, 1)
colorToRGB Yellow  = (1, 1, 0)
colorToRGB White   = (1, 1, 1)

withColor :: Color -> Graphic -> Graphic
withColor color (Graphic graphic) = Graphic $ \pc -> do
  Cairo.save
  case colorToRGB color of
    (r,g,b) -> Cairo.setSourceRGB r g b
  -- for some reason the SOE withColor uses a line width of 2,
  -- though the default line width outside of withColor is 1.
  Cairo.setLineWidth 2
  graphic pc
  Cairo.restore

text :: Point -> String -> Graphic
text (x,y) str = Graphic $ \pc -> do
  layout <- Cairo.liftIO $ Gtk.layoutEmpty pc
  Gtk.Cairo.updateLayout layout
  Cairo.liftIO $ Gtk.layoutSetText layout str
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  Gtk.Cairo.showLayout layout

type Point = (Int, Int)

ellipse :: Point -> Point -> Graphic
ellipse pt1 pt2 = Graphic $ \pc -> case normaliseBounds pt1 pt2 of
  Nothing -> return ()
  Just  (x,y,width,height) -> do
    Cairo.save
    Cairo.translate (x + width / 2) (y + height / 2)
    Cairo.scale (width / 2) (height / 2)
    Cairo.arc 0 0 1 0 (2*pi)
    Cairo.fill
    Cairo.restore

shearEllipse :: Point -> Point -> Point -> Graphic
shearEllipse (x1,y1) (x2,y2) (x3,y3) = Graphic $ \pc -> do
  let x = fromIntegral x1
      y = fromIntegral y1
      scalex = fromIntegral $ abs $ x1 - x3
      scaley = fromIntegral $ abs $ y1 - y2
      shearx = fromIntegral $ abs $ x1 - x2
      sheary = fromIntegral $ abs $ y1 - y3

  Cairo.save
  Cairo.transform (Matrix.Matrix scalex sheary shearx scaley x y)
  Cairo.arc 0.5 0.5 0.5 0 (2 * pi)
  Cairo.fill
  Cairo.restore

line :: Point -> Point -> Graphic
line (x1, y1) (x2, y2) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x1) (fromIntegral y1)
  Cairo.lineTo (fromIntegral x2) (fromIntegral y2)
  Cairo.stroke

polygon :: [Point] -> Graphic
polygon [] = Graphic (\_ -> return ())
polygon ((x,y):ps) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  sequence_ [ Cairo.lineTo (fromIntegral x) (fromIntegral y)
            | (x,y) <- ps ]
  Cairo.fill

polyline :: [Point] -> Graphic
polyline [] = Graphic (\_ -> return ())
polyline ((x,y):ps) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  sequence_ [ Cairo.lineTo (fromIntegral x) (fromIntegral y)
            | (x,y) <- ps ]
  Cairo.stroke

polyBezier :: [Point] -> Graphic
polyBezier [] = Graphic (\_ -> return ())
polyBezier ((x,y):ps) = Graphic $ \pc -> do
  Cairo.moveTo (fromIntegral x) (fromIntegral y)
  let loop ((x1,y1):(x2,y2):(x3,y3):ps) = do
        Cairo.curveTo (fromIntegral x1) (fromIntegral y1)
                      (fromIntegral x2) (fromIntegral y2)
                      (fromIntegral x3) (fromIntegral y3)
        loop ps
      loop _ = return ()
  loop ps
  Cairo.stroke

arc :: Point -> Point -> Angle -> Angle -> Graphic
arc pt1 pt2 start_ extent_ = Graphic $ \pc -> case normaliseBounds pt1 pt2 of
  Nothing -> return ()
  Just  (x,y,width,height) -> do
    Cairo.save
    Cairo.translate (x + width / 2) (y + height / 2)
    Cairo.scale (width / 2) (height / 2)
    Cairo.moveTo 0 0
    Cairo.arcNegative 0 0 1 (-start * pi / 180) (-(start+extent) * pi / 180)
    Cairo.fill
    Cairo.restore
  where start  = realToFrac start_
        extent = realToFrac extent_

-------------------
-- Region Functions
-------------------

data Region = Region {
  regionGraphic :: Int -> Int -> Cairo.Render (),
  regionOriginX :: !Int,
  regionOriginY :: !Int,
  regionWidth   :: !Int,
  regionHeight  :: !Int
}

createRectangle :: Point -> Point -> Region
createRectangle pt1 pt2 =
  let (x,y,width,height) = normaliseBounds' pt1 pt2 
      drawing x y = do
        Cairo.rectangle (fromIntegral x) (fromIntegral y)
                        (fromIntegral width) (fromIntegral height)
        Cairo.fill
   in Region drawing x y width height

createEllipse :: Point -> Point -> Region
createEllipse pt1 pt2 =
  let (x0,y0,width,height) = normaliseBounds' pt1 pt2
      drawing x y | width==0 || height==0 = return ()
                  | otherwise = do
        Cairo.save
        Cairo.translate (fromIntegral x + fromIntegral width / 2)
                        (fromIntegral y + fromIntegral height / 2)
        Cairo.scale (fromIntegral width / 2) (fromIntegral height / 2)
        Cairo.arc 0 0 1 0 (2*pi)
        Cairo.fill
        Cairo.restore
  in Region drawing x0 y0 width height

createPolygon :: [Point] -> Region
createPolygon [] = Region (\_ _ -> return ()) 0 0 0 0
createPolygon (p@(x0,y0):ps) =
  let minMax (minx,maxx,miny,maxy) (x,y) =
        let minx' = min minx x
            maxx' = max maxx x
            miny' = min miny y
            maxy' = max maxy y
         in seq minx' $ seq maxx' $ seq miny' $ seq maxy' $
            (minx',maxx',miny',maxy')
      (minx,maxx,miny,maxy) = foldl' minMax (x0,x0,y0,y0) (p:ps)
      drawing x y = do
        Cairo.save
        Cairo.translate (fromIntegral (x-minx)) (fromIntegral (y-miny))
        Cairo.moveTo (fromIntegral x0) (fromIntegral y0)
        sequence_ [ Cairo.lineTo (fromIntegral x) (fromIntegral y)
                  | (x,y) <- ps ]
        Cairo.fill
        Cairo.restore
   in Region drawing minx miny (maxx - minx) (maxy - miny)

andRegion, orRegion, xorRegion, diffRegion :: Region -> Region -> Region
andRegion  = combineRegion Cairo.OperatorIn
orRegion   = combineRegion Cairo.OperatorOver
xorRegion  = combineRegion Cairo.OperatorXor
diffRegion = combineRegion Cairo.OperatorDestOut

drawRegion :: Region -> Graphic
drawRegion Region { regionGraphic = graphic,
                    regionOriginX = x,
                    regionOriginY = y
                  } = Graphic $ \_ -> do
  graphic x y

combineRegion :: Cairo.Operator -> Region -> Region -> Region
combineRegion operator a b =
  let x = min (regionOriginX a) (regionOriginX b)
      y = min (regionOriginY a) (regionOriginY b)
      x' = max (regionOriginX a + regionWidth a) (regionOriginX b + regionWidth b)
      y' = max (regionOriginY a + regionHeight a) (regionOriginY b + regionHeight b)
      width  = x' - x
      height = y' - y
      drawing x'' y'' = do
        Cairo.renderWithSimilarSurface Cairo.ContentAlpha width height $
          \surface -> do
          Cairo.renderWith surface $ do
            Cairo.setSourceRGBA 0 0 0 1
            regionGraphic a (regionOriginX a - x)
                            (regionOriginY a - y)
            Cairo.setOperator operator
            regionGraphic b (regionOriginX b - x)
                            (regionOriginY b - y)
          Cairo.maskSurface surface (fromIntegral x'') (fromIntegral y'')
   in Region drawing x y width height

normaliseBounds :: Point -> Point -> Maybe (Double,Double,Double,Double)
normaliseBounds (x1,y1) (x2,y2) = 
  if x1==x2 || y1==y2 then Nothing else Just (x, y, width, height)
  where x = fromIntegral $ min x1 x2
        y = fromIntegral $ min y1 y2
        width  = fromIntegral $ abs $ x1 - x2
        height = fromIntegral $ abs $ y1 - y2

normaliseBounds' :: Point -> Point -> (Int,Int,Int,Int)
normaliseBounds' (x1,y1) (x2,y2) = (x, y, width, height)
  where x = min x1 x2
        y = min y1 y2
        width  = abs $ x1 - x2
        height = abs $ y1 - y2

---------------------------
-- Event Handling Functions
---------------------------

data Event = Key {
               char :: Char,
               isDown :: Bool
             }
           | Button {
              pt :: Point,
              isLeft :: Bool,
              isDown :: Bool
             }
           | MouseMove {
               pt :: Point
             }
           | Resize
           | Closed
  deriving Show

getWindowEvent_ :: Window -> IO Event
getWindowEvent_ win = atomically $ readTChan (eventsChan win)

getWindowEvent :: Window -> IO Event
getWindowEvent win = do
  event <- getWindowEvent_ win
  -- this says we are ready for another mouse move event
  -- (this is part of the pointer move event flood prevention system)
  case event of
    MouseMove _ -> Gtk.postGUIAsync $
                   Gtk.widgetGetDrawWindow (canvas win)
                   >>= Gtk.drawWindowGetPointer
                   >> return ()
    _ -> return ()
  return event

maybeGetWindowEvent :: Window -> IO (Maybe Event)
maybeGetWindowEvent win = do
  noEvents <- atomically $ isEmptyTChan (eventsChan win)
  if noEvents then -- Sync with the main GUI loop or we can end up spinning on
                   -- maybeGetWindowEvent and prevent the screen redrawing.
                   -- We also introduce a very short delay here of 5ms. This
                   -- prevents the program from using 100% cpu and redrawing
                   -- constantly. This actually makes animation much smoother
                   -- since the process gets treated as interactive rather than
                   -- as a CPU hog so the scheduler gives us the benefit of
                   -- latency. Even with a 5ms delay here we can animate at up
                   -- to 200fps.
                   do syncVar <- newEmptyMVar
                      Gtk.timeoutAddFull (putMVar syncVar () >> return False)
                                         Gtk.priorityDefaultIdle 10
                      takeMVar syncVar
                      return Nothing
              else do event <- atomically $ readTChan (eventsChan win)
                      case event of
                        MouseMove _ -> Gtk.postGUIAsync $
                                       Gtk.widgetGetDrawWindow (canvas win)
                                       >>= Gtk.drawWindowGetPointer
                                       >> return ()
                        _ -> return ()
                      return (Just event)


getKeyEx :: Window -> Bool -> IO Char
getKeyEx win down = loop
  where loop = do e <- getWindowEvent_ win
                  case e of
                    (Key { char = ch, isDown = d })
                      | d == down -> return ch
                    Closed -> return '\x0'
                    _ -> loop

getKey :: Window -> IO Char
getKey win = do
  ch <- getKeyEx win True
  if ch == '\x0' then return ch
    else getKeyEx win False

getButton :: Window -> Int -> Bool -> IO Point
getButton win but down = loop
  where loop = do e <- getWindowEvent_ win
                  case e of
                    (Button { pt = pt, isDown = id })
                      | id == down -> return pt
                    _ -> loop

getLBP :: Window -> IO Point
getLBP w = getButton w 1 True

getRBP :: Window -> IO Point
getRBP w = getButton w 2 True

---------------------------------
-- Window Tick Handling Functions
---------------------------------

type Time = Int

type Tick = ()

getWindowTick :: Window -> IO ()
getWindowTick w = takeMVar (tickVar w)

timeGetTime :: IO Word32
timeGetTime = do
  System.Time.TOD sec psec <- System.Time.getClockTime
  return (fromIntegral $ sec * 1000 + psec `div` 1000000000)

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral
