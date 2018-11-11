module Write where

import           Control.Monad.Writer
import           Data.List
import           Prelude

type Writer = WriterT Identity

type Href = String

data Style = Color String | BackgroundColor String | Width Int | Height Int deriving (Show)

data StyleTree = Div [Style] [StyleTree]
            | Title [Style] String
            | P [Style] String
            | A [Style] Href [StyleTree]

website :: StyleTree
website = Div [] [ Title [] "My interesting website"
           , P [] "Item of interest"
           , A [] "http://danieljharvey.github.io" [ P [] "Website" ]
           , Div [BackgroundColor "#FFDD00"] [ P [] "Sub-item"
                    , P []"Another sub-item"
                    ]
           ]

renderStyle :: Style -> String
renderStyle (Color s)           = "color: " ++ s
renderStyle (BackgroundColor s) = "background-color: " ++ s
renderStyle (Width w)           = "width: " ++ show w ++ "px"
renderStyle (Height h)          = "height: " ++ show h ++ "px"

renderStyles :: [Style] -> String
renderStyles [] = ""
renderStyles as = concat ["style=\"", intercalate ", " $ map renderStyle as, "\""]

showStyleTree :: StyleTree -> String
showStyleTree (Div style children) =
    concat [ "<div "
           , renderStyles style
           , ">"
           , concatMap showStyleTree children
           , "</div>"
           ]
showStyleTree (Title style text) =
    concat [ "<h1 "
           , renderStyles style
           , text
           , "</h1>"
           ]
showStyleTree (P style text) =
    concat [ "<p "
            , renderStyles style
            , text
            , "</p>"
            ]
showStyleTree (A style href children) =
    concat [ "<a href=\""
            , href
            , "\" "
            , renderStyles style
            , ">"
            , concatMap showStyleTree children
            , "</div>"
            ]

writerStyleTree :: StyleTree -> Writer [[Style]] String
writerStyleTree (Div style children) = do
    tell [style]
    pure $ concat [ "<div "
            , renderStyles style
            , ">"
            , concatMap showStyleTree children
            , "</div>"
            ]
writerStyleTree (Title style text) = do
    tell [style]
    pure $ concat [ "<h1 "
            , renderStyles style
            , text
            , "</h1>"
            ]
writerStyleTree (P style text) = do
    tell [style]
    pure $ concat [ "<p "
            , renderStyles style
            , text
            , "</p>"
            ]
writerStyleTree (A style href children) = do
    tell [style]
    pure $ concat [ "<a href=\""
            , href
            , "\" "
            , renderStyles style
            , ">"
            , concatMap showStyleTree children
            , "</div>"
            ]
