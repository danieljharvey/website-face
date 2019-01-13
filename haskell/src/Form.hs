module Form where

go :: IO ()
go = print $ ui twoNumsForm (10,12)

data UI = Div UI | Row [UI] | Col [UI] | Item String deriving (Show, Eq)

data Form i a = Form { ui :: i -> UI, result :: i -> a }

twoNumsForm :: Form (Int, Int) Int
twoNumsForm = Form { ui = uncurry twoNumsUI, result = twoNumsResult}

twoNumsResult :: (Int, Int) -> Int
twoNumsResult = uncurry (+)

twoNumsUI :: Int -> Int -> UI
twoNumsUI a b = Div $ Col [ Item $ show a
        , Item $ show b
        ]

