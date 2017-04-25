{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI, CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Reflex.Dom
import Data.Monoid ((<>))
import Control.Monad.IO.Class ( liftIO )
import GHCJS.DOM.HTMLCanvasElement 
import GHCJS.DOM.JSFFI.Generated.CanvasRenderingContext2D
import GHCJS.DOM.Types (CanvasStyle(..), CanvasRenderingContext2D(..), toJSString, castToHTMLCanvasElement, ImageData(..))
import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal)
import GHCJS.Marshal.Pure
import GHCJS.Marshal (toJSVal)
import Data.Map as M
import Data.Text as T
import Data.ByteString as BS

-- http://lpaste.net/154691
-- https://www.snip2code.com/Snippet/1032978/Simple-Canvas-Example/

canvasAttrs :: Float -> Float -> Map Text Text
canvasAttrs w h = ("width" =: (T.pack $ show w)) <> ("height" =: (T.pack $ show h))

foreign import javascript unsafe 
    -- Arguments
    --    width  : JSNumber
    --    height : JSNumber
    --    pixels : Ptr a -- Pointer to a ByteString in the format below
    "(function(){                                     \
        var width  = $1;                              \
        var height = $2;                              \
        console.log(width);                           \
        console.log(height);                           \
        console.log($3);                           \
        console.log($3.u8[0]);                           \
        var length = $4;                                         \
        var buffer = $3.u8.slice(0, length);                                         \
        console.log(length);                           \
        var pixels = new Uint8ClampedArray(buffer,0,length);    \
        return new ImageData(pixels, width, height)   \
    })()" 
    newImageData :: forall a . JSVal -> JSVal -> Ptr a -> JSVal -> IO JSVal

main = mainWidget $ do
    let width = 300.0
    let height = 200.0
    (canvasEl, _) <- elAttr' "canvas" (canvasAttrs width height) $ return ()
    let canvasElement = castToHTMLCanvasElement(_element_raw canvasEl)
    let elementContext =  getContext canvasElement ("2d" :: [Char])
    c <- fmap CanvasRenderingContext2D $ liftIO elementContext

    let imageWidth :: Float= 4.0
    let imageHeight :: Float= 3.0
    let smallImage = BS.pack [0xff,0x00,0x00,0xff,  0xff,0x00,0x00,0xff,  0xff,0x00,0x00,0xff,
                              0x00,0x00,0x00,0xff,  0x00,0xff,0x00,0xff,  0x00,0x00,0x00,0xff,
                              0x00,0x00,0xff,0xff,  0x00,0x00,0xff,0xff,  0x00,0x00,0xff,0xff,
                              0x00,0x00,0xff,0xff,  0x00,0x00,0x00,0xff,  0x00,0x00,0xff,0xff]
    smallImageData <- liftIO $ BS.useAsCStringLen smallImage $ \ (ptr,len) -> newImageData (pToJSVal imageWidth) (pToJSVal imageHeight) ptr (pToJSVal len)


    liftIO $ setFillStyle c =<< (fmap (Just . CanvasStyle) $ toJSVal $ toJSString ("red"::[Char]))
    fillRect c 0.0 0.0 width height
    liftIO $ putImageData c (Just $ ImageData smallImageData) 100 150

    return ()
