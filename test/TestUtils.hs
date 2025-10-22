module TestUtils where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (outer)
import Graphics.Svg (prettyText)

diagToSVGBS :: Diagram SVG -> ByteString
diagToSVGBS =
    encodeUtf8
        . prettyText
        . renderDia
            SVG
            ( SVGOptions
                (mkSizeSpec (V2 (Just 1000) Nothing))
                Nothing
                mempty
                []
                True
            )
