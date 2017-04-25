{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

import Reflex.Dom
import Data.Monoid ((<>))
import Control.Monad.IO.Class ( liftIO )
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D (putImageData, fillRect, setFillStyle)
import GHCJS.DOM.Types (CanvasStyle(..), CanvasRenderingContext2D(..), toJSString, castToHTMLCanvasElement, ImageData(..))
import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Marshal (toJSVal)
import Data.Map (Map)
import Data.Text as T (Text, pack)
import Data.ByteString as BS (pack, useAsCStringLen)

-- http://lpaste.net/154691
-- https://www.snip2code.com/Snippet/1032978/Simple-Canvas-Example/

foreign import javascript unsafe 
    "(function(){                                     \
        var width  = $1;                              \
        var height = $2;                              \
        var length = $4;                              \
        var buffer = $3.u8.slice(0, length);          \
        var pixels = new Uint8ClampedArray(buffer);   \
        return new ImageData(pixels, width, height)   \
    })()" 
    jsImageData :: JSVal -> JSVal -> Ptr a -> JSVal -> IO JSVal

-- friendlier front end to jsImageData
newImageData :: Int -> Int -> (Ptr a, Int) -> IO ImageData
newImageData width height (ptr, len) = 
    ImageData <$> jsImageData (pToJSVal width) (pToJSVal height) ptr (pToJSVal len)

canvasAttrs :: Int -> Int -> Map T.Text T.Text
canvasAttrs w h =    ("width" =: (T.pack $ show w)) 
                  <> ("height" =: (T.pack $ show h))

main = mainWidget $ do
    let boxWidth = 100
        boxHeight = 30

        reds = take (boxWidth*boxHeight*4) $ concat $ repeat [0xff,0x00,0x00,0xff]
        blues = take (boxWidth*boxHeight*4) $ concat $ repeat [0x00,0xff,0x00,0xff]
        greens = take (boxWidth*boxHeight*4) $ concat $ repeat [0x00,0x00,0xff,0xff]

        image = BS.pack $ reds ++ blues ++ greens

        imageWidth = boxWidth
        imageHeight = boxHeight * 3

    -- convert image ByteString to a c style string and then to ImageData
    imageData <- liftIO $ BS.useAsCStringLen image $ newImageData imageWidth imageHeight 

    -- demonstrate the imageData is what we expect
    let canvasWidth = 300
        canvasHeight = 200

    (element, _) <- elAttr' "canvas" (canvasAttrs canvasWidth canvasHeight) $ return ()
    let canvasElement = castToHTMLCanvasElement(_element_raw element)
        elementContext =  getContext canvasElement ("2d" :: [Char])

    renderingContext <- fmap CanvasRenderingContext2D elementContext
    fillStyle <-  liftIO $ (fmap (Just . CanvasStyle) $ toJSVal $ toJSString ("grey" :: [Char]))

    setFillStyle renderingContext fillStyle
    fillRect renderingContext 0.0 0.0 (fromIntegral canvasWidth) (fromIntegral canvasHeight)
    putImageData renderingContext (Just imageData) 100 20

    return ()
