module Main where

import Data.Maybe
import Graphics.Canvas
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (name)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Data.Function

import Control.Monad.Eff

printCheck :: forall e. Eff (console :: CONSOLE | e) Unit
printCheck=do
  if 2>1
    then do
      log "Two Greater Than One"
    else
      log "One Greater Than Two"

state={value:5.00}

drawInnerCurves name = void $ unsafePartial do
    log name
    Just canvas <- getCanvasElementById name
    ctx <- getContext2D canvas

    void $ setFillStyle "#FFFFFF" ctx
    void $ moveTo ctx 199.00 300.00
    void $ lineTo ctx 240.00 300.00
    void $ moveTo ctx 240.00 300.00
    void $ quadraticCurveTo { cpx : 270.00,cpy :180.00,x:380.00 ,y:300.00 } ctx
    void $ moveTo ctx 401.00 300.00
    void $ lineTo ctx 360.00 300.00
    void $ moveTo ctx 360.00 300.00
    void $ quadraticCurveTo { cpx :370.00,cpy :420.00,x:220.00 ,y:300.00 } ctx
    void $ fill ctx
    void $ stroke ctx

drawOuterCircle name = void $ unsafePartial do
    log name
    Just canvas <- getCanvasElementById name
    ctx <- getContext2D canvas
    void $ setFillStyle "#5e92e5" ctx
    void $ fillPath ctx $ arc ctx{ x: 300.0, y: 300.0, r: 100.0, start : 2.0*Math.pi, end: 0.0}
    void $ stroke ctx


drawInnerCurvesPlain name = void $ unsafePartial do
    log name
    Just canvas <- getCanvasElementById name
    ctx <- getContext2D canvas

    -- void $ setFillStyle "#FFFFFF" ctx
    void $ moveTo ctx 199.00 300.00
    void $ lineTo ctx 240.00 300.00
    void $ moveTo ctx 240.00 300.00
    void $ quadraticCurveTo { cpx : 270.00,cpy :180.00,x:380.00 ,y:300.00 } ctx
    void $ moveTo ctx 401.00 300.00
    void $ lineTo ctx 360.00 300.00
    void $ moveTo ctx 360.00 300.00
    void $ quadraticCurveTo { cpx :370.00,cpy :420.00,x:220.00 ,y:300.00 } ctx
    void $ fill ctx
    void $ stroke ctx


rotateRight name=void $ unsafePartial do
  -- clearEverything name
  Just canvas <- getCanvasElementById name
  ctx <- getContext2D canvas
  void $ translate {translateX:(135.0),translateY:(-110.0)} ctx
  void $ rotate (0.523599) ctx
  drawTheLogo name
  log "Recurse Here"
  rotateRight name

clearEverything name=void $ unsafePartial do
    Just canvas <- getCanvasElementById name
    ctx <- getContext2D canvas
    void $ clearRect ctx {x:0.0,y:0.0,w:1000.0,h:1000.0}

drawTheLogo name=do
  drawOuterCircle name
  drawInnerCurves name
  -- rotateRight name

drawTheLogoStatic name=do
  drawOuterCircle name
  drawInnerCurves name


main = do
  drawTheLogoStatic "thecanvas_logo"
  rotateRight "thecanvas"
