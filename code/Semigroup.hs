module Semigroup where


-- lists

firstList :: [Int]
firstList = [1,2,3]

secondList :: [Int]
secondList = [4,5,6]

thirdList :: [Int]
thirdList = firstList <> secondList

-- strings

firstString :: String
firstString = "Great"

secondString :: String
secondString = "Stuff"

thirdString :: String
thirdString = firstString <> secondString

-- MySum

newtype MySum a = MySum {
    getMySum :: a
}

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2
-- ten == 10

anotherTen :: Int
anotherTen = getMySum $ MySum 1 <> (MySum 7 <> MySum 2)
-- anotherTen == 10

-- MyProduct

newtype MyProduct a = MyProduct {
    getMyProduct :: a
}

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct a <> MyProduct b = MyProduct (a * b)

sixtySix :: Int
sixtySix = getMyProduct $ MyProduct 11 <> MyProduct 2 <> MyProduct 3
-- sixtySix = 66

anotherSixtySix :: Int
anotherSixtySix = getMyProduct $ MyProduct 11 <> (MyProduct 2 <> MyProduct 3)
-- anotherSixtySix = 66

-- MyString

newtype MyString = MyString {
    getMyString :: String
}

instance Semigroup MyString where
    MyString a <> MyString b = MyString (a ++ b)

allTheThings :: String
allTheThings = getMyString $ MyString "All" <> MyString "The" <> MyString "Things"
-- allTheThings = "AllTheThings"

otherAllTheThings :: String
otherAllTheThings = getMyString $ MyString "All" <> (MyString "The" <> MyString "Things")
-- otherAllTheThings = "AllTheThings"

-- non empty list

data NonEmpty a = NonEmpty a [a] deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
    (NonEmpty a as) <> (NonEmpty b bs) = NonEmpty a $ as <> [b] <> bs

first :: NonEmpty Int
first = NonEmpty 1 [2,3,4]

second :: NonEmpty Int
second = NonEmpty 5 [6,7,8]

third :: NonEmpty Int
third = first <> second
