{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

import Reflex.Dom
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import GHCJS.DOM.ImageData (newImageData)
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D (putImageData)
import GHCJS.DOM.Types (CanvasRenderingContext2D(..), castToHTMLCanvasElement, Uint8ClampedArray(..))
import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal)
import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)
import Data.Map (Map)
import Data.Text as T (Text, pack)
import Data.ByteString as BS (ByteString, pack, useAsCStringLen)

-- Some code and techniques taken from these sites:
-- http://lpaste.net/154691
-- https://www.snip2code.com/Snippet/1032978/Simple-Canvas-Example/

-- import inline Javascript code as Haskell function : jsImageData
foreign import javascript unsafe 
    -- Arguments
    --     pixels : Ptr a -- Pointer to a ByteString 
    --     len    : JSVal -- Number of pixels
    "(function(){ return new Uint8ClampedArray($1.u8.slice(0, $2)); })()" 
    jsUint8ClampedArray :: Ptr a -> JSVal -> IO JSVal

-- takes pointer and length arguments as passed by useAsCStringLen
newUint8ClampedArray :: (Ptr a, Int) -> IO Uint8ClampedArray
newUint8ClampedArray (pixels, len) = 
    pFromJSVal <$> jsUint8ClampedArray pixels (pToJSVal len)

canvasAttrs :: Int -> Int -> Map T.Text T.Text
canvasAttrs w h =    ("width" =: T.pack (show w)) 
                  <> ("height" =: T.pack (show h))

main = mainWidget $ do
    -- first, generate some test pixels
    let boxWidth = 120
        boxHeight = 30
        boxDataLen = boxWidth*boxHeight*4 -- 4 bytes per pixel

        reds = take boxDataLen $ concat $ repeat [0xff,0x00,0x00,0xff]
        greens = take boxDataLen $ concat $ repeat [0x00,0xff,0x00,0xff]
        blues = take boxDataLen $ concat $ repeat [0x00,0x00,0xff,0xff]

        pixels = reds ++ greens ++ blues
        image = BS.pack pixels -- create a ByteString with the pixel data.

    -- create Uint8ClampedArray representation of pixels
    imageArray <- liftIO $ BS.useAsCStringLen image newUint8ClampedArray

    let imageWidth = boxWidth
        imageHeight = (length pixels `div` 4) `div` imageWidth

    -- use Uint8ClampedArray representation of pixels to create ImageData
    imageData <- newImageData (Just imageArray) (fromIntegral imageWidth) (fromIntegral imageHeight)

    -- demonstrate the imageData is what we expect by displaying it.
    (element, _) <- elAttr' "canvas" (canvasAttrs 300 200) $ return ()
    let canvasElement = castToHTMLCanvasElement(_element_raw element)
    elementContext <-  getContext canvasElement ("2d" :: String)

    let renderingContext = CanvasRenderingContext2D elementContext
    putImageData renderingContext (Just imageData) 80 20
