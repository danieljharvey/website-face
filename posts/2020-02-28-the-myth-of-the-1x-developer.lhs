---
title: The Myth Of The 1x Developer
tags: thought-leadership, monoid
---

When we thing of developers and the business value that they can create, we of course all think of `Monoids`.

> import Data.Monoid
> newtype BusinessValue = Xs { getBusinessValue :: Int }

We have two developers, so of course their efforts are combined as thus.
 
> instance Semigroup BusinessValue where
>   (Xs a) <> (Xs b) = Xs (a + b) 
>
> businessValue :: Int
> businessValue = getBusinessValue $ Xs 1 <> Xs 1
> -- businessValue == 2

Two `1x` developers should give us `2 Xs` right? Seems reasonable.

It follows that a develop contributing nothing is the `identity` developer, therefore we can upgrade our `Semigroup` instance to a full `Monoid` as thus:

> instance Monoid BusinessValue where
>   mempty = Xs 0

Therefore we can save ourselves a shitload of time in Jira fucking around with story points and just calculate the teams productivity with foldMap.

> team :: [BusinessValue]
> team = [Xs 1, Xs 2, Xs 5, Xs 2]
>
> productivity :: Int
> productivity = getBusinessValue (mconcat team)
> -- productivity == 1 + 2 + 5 + 2 == 10

This is all very well, we have achieved our exhalted `10 Xs` through the power of combination, and management is happy.

But wait! I don't think it works this way. Say we have developer A and developer B.

`Developer A` is smashing along quite nicely, she knows the code, knows her stuff, and is working at a solid `8x` right now. 

> developerA :: BusinessValue
> developerA = Xs 8
 
`Developer B` is not doing so well though. Not only is he not adding any business value, but his insistence on time-wasting activities is actually a detriment to other around him.

> developerB :: BusinessValue
> developerB = Xs (-2)

Therefore our team is smashing along at a saddening `6 Xs`.

> combo :: Int
> combo = getBusinessValue (mconcat [ developerA, developerB ])
> -- combo = 8 + (-2) == 6

Management has a solution! Add more developers!

`Developer C` joins the team. They're cruising at an acceptable `4x`. It's not loads, but it's enough to cancel out `Developer B`'s contribution plus a little more.

> developerC :: BusinessValue
> developerC = Xs 4
>
> newCombo :: Int
> newCombo = getBusinessValue (mconcat [ developerA, developerB, developerC ])
> -- newCombo = 8 + (-2) + 4 == 10

Finally! The `10x` we need to start doing some real disruption!

But wait! `Developer B` is also bringing down the efforts of our new `Developer C`. Could they have less `Xs` than we thought? `-3x`? `-8x` even?

But that's not right. They are still operating at `-2x`, it's just the model that's at fault. It doesn't take into account the accumulative effect of bad stinkers.

> newtype SmashRate = XXs { getSmashRate :: Double }
>
> instance Semigroup SmashRate where
>  (XXs a) <> (XXs b) = XXs (a * b)
> 
> instance Monoid SmashRate where
>   mempty = XXs 1

Whoa. Hold on there. What's this `1x` identity function? Are you telling me that a developer doing nothing is `1x`? That feels like we're just giving `Xs` away for free? This will never scale!

Let's take a look at the team again.

> developerA' :: SmashRate
> developerA' = XXs 8
>
> developerB' :: SmashRate
> developerB' = XXs 0.75
>
> developerC' :: SmashRate
> developerC' = XXs 6

Looking at `DeveloperA` and `DeveloperB`, the team is still looking the same:

> combo' :: Double
> combo' = getSmashRate (mconcat [ developerA', developerB' ])
> -- combo' == 8 * 0.75 == 6

Seems like a fair appraisal of the situation.

> newCombo' :: Double
> newCombo' = getSmashRate (mconcat [ developerA', developerB', developerC' ])
> -- newCombo' == 8 * 0.75 * 4 == 24



