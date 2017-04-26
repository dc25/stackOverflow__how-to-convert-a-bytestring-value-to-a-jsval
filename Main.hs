{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

import Reflex.Dom
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D (putImageData)
import GHCJS.DOM.Types (CanvasRenderingContext2D(..), castToHTMLCanvasElement, ImageData(..))
import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import Data.Map (Map)
import Data.Text as T (Text, pack)
import Data.ByteString as BS (ByteString, pack, useAsCStringLen)

-- Some code and techniques taken from these sites:
-- http://lpaste.net/154691
-- https://www.snip2code.com/Snippet/1032978/Simple-Canvas-Example/

-- import inline Javascript code as Haskell function : jsImageData
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

-- friendlier front end to newImageData
makeImageData :: Int -> Int -> ByteString -> IO ImageData
makeImageData width height imageData = 
    BS.useAsCStringLen imageData $ newImageData width height

canvasAttrs :: Int -> Int -> Map T.Text T.Text
canvasAttrs w h =    ("width" =: (T.pack $ show w)) 
                  <> ("height" =: (T.pack $ show h))

main = mainWidget $ do
    let boxWidth = 100
        boxHeight = 30
        boxDataLen = boxWidth*boxHeight*4

        reds = take boxDataLen $ concat $ repeat [0xff,0x00,0x00,0xff]
        greens = take boxDataLen $ concat $ repeat [0x00,0xff,0x00,0xff]
        blues = take boxDataLen $ concat $ repeat [0x00,0x00,0xff,0xff]

        colors = reds ++ greens ++ blues
        image = BS.pack colors -- create a ByteString with the pixel data.

        imageWidth = boxWidth
        imageHeight = (length colors `div` 4) `div` imageWidth

    imageData <- liftIO $ makeImageData imageWidth imageHeight image 

    -- demonstrate the imageData is what we expect by displaying it.
    let canvasWidth = 300
        canvasHeight = 200

    (element, _) <- elAttr' "canvas" (canvasAttrs canvasWidth canvasHeight) $ return ()
    let canvasElement = castToHTMLCanvasElement(_element_raw element)
        elementContext =  getContext canvasElement ("2d" :: [Char])

    renderingContext <- fmap CanvasRenderingContext2D elementContext

    putImageData renderingContext (Just imageData) 80 20
