---
title: Why The Fuck Should I Care About Lens? (Part 2)
---

```haskell
data DogFact = DogName String
             | DogAge Int deriving (Show, Eq)

spruceBruce :: DogFact
spruceBruce = DogName "Spruce Bruce"

oldDog :: DogFact
oldDog = DogAge 100

dogNamePrism :: Prism' DogFact String
dogNamePrism = prism' DogName (\e -> case e of
                            DogName a -> Just a
                            _         -> Nothing)

dogAgePrism :: Prism' DogFact Int
dogAgePrism = prism' DogAge (\e -> case e of
                            DogAge b -> Just b
                            _        -> Nothing)

getDogAge :: DogFact -> Maybe Int
getDogAge dogFact = preview dogAgePrism dogFact

getDogName :: DogFact -> Maybe String
getDogName dogFact = preview dogNamePrism dogFact

dogAge :: Maybe Int
dogAge = getDogAge oldDog
-- dogAge == Just 100

notDogAge :: Maybe Int
notDogAge = getDogAge spruceBruce
-- notDogAge == Nothing

dogName :: Maybe String
dogName = getDogName spruceBruce
-- dogName == Just "Spruce Bruce"

notDogName :: Maybe String
notDogName = getDogName oldDog
-- notDogName == Nothing

-- can change value on same side
newAge :: DogFact
newAge = set dogAgePrism 27 oldDog
-- newAge == DogAge 27

-- but not change the side
noNewAge :: DogFact
noNewAge = set dogAgePrism 27 spruceBruce
-- noNewAge == DogName "Spruce Bruce"
```
