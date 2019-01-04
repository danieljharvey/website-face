---
title: What the hell is a typeclass anyway?
tags: haskell, typeclasses
---

So recently I have become one of those jerks that has sailed through *Read A Couple Of Chapters Of A Book About Haskell*, by way of *Snuck Maybe Types Into My Work Project* to arrive at full-blown *Good Morning Sir/Madam, Have You Got A Spare Moment To Talk About Monads&trade;?*. That is to say, by selfishly forcing these concepts upon my colleagues I have ended up spending a lot of my working life explaining them and why I think they are good to stop everybody hating me for making their lives unnecessary complicated. In doing so, I think I have come across one of the main conceptual stumbling blocks, which I hope to make less-stumble-able.

So the problem, as I see it, isn't grasping what a `Monad` is (it's a burrito, it's a poster tube, it's a snobby waste of time, easy!), or even what a `Functor` is (it's mappable, right?), but whether they are even "things" at all. It's one thing to see words like `Monad`, `Functor`, `Monoid` or `Traversable` around the place but it's a little unclear how to make, or god forbid, actually use one in something approaching real life code. Are they objects? Do I make a new `Monad` object just like I'd make a new `Promise`?

```javascript
const response = new Monad(ajaxUrl)
```

Is is a `Functor` a method I can run on something to increase the amount of maths?

```javascript
const makeProgramFaster = response.Functor("MAXIMISE DATA")
```

The short answer is "no", and the long answer, well, that's also "no", followed by "ok, let me tell you what typeclasses are and hopefully the other stuff will sort of get cleared up along the way."

### Maths is sadness

Reading about type theory, particularly materials written from a very mathematical perspective, can often make it feel even worse, with explanations that feel so broad and simple that they feel either a) meaningless or b) like they must contain some unseen trap we mere mortals do not understand.

![Perhaps, yes.](/images/typeclass1.png "Are monads a waste of time?")

Anyhow, I thought perhaps a better approach here is to explain what these things are not and work from there. Because the aim here is to end up with some real life knowledge we can use to write real code, instead of using examples from the world of computers, let's use them from the world of crime dramas, because I have been watching a lot of those recently.

### Time for a tenuous analogy

So we have a police officer. In this case, it's Detective Sergeant Kate Fleming from Jed Mercurio's surprisingly gripping Line Of Duty series (I too struggled with the series title's almost Clancy-ian levels of Really Must We Spell This Out, do persevere with it though, it really is OK, you probably only need to watch the first couple of seasons to follow this article)

![DS Kate Fleming, a human woman](/images/typeclass3.png "DS Kate Fleming")

**DS Kate Fleming** is a `Police Officer` in AC-12, a unit in the police force somewhere in London that investigates `Bent Coppers` and makes sure there is no *Dodgy Dealings* or *Underhand Business*. **DS Kate Fleming** was not born a police officer, but certainly displays all the characteristics of one - she *Catches Criminals*, *Likes A Pint*, and *Her Marriage Is In Tatters* because of course she is *Married To The Job*.

![DC Nigel "Nige" Morton, a human man](/images/typeclass4.png "DC Nigel "Nige" Morton, a human man")

Here is another `Police Officer` - in this case **Detective Constable Nigel "Nige" Morton**, played by 90s lad hero and renowned bounding frown Neil Morrissey. **"Nige"** was also not born a copper, but he is certainly one now as he *Likes A Pint* and also, to some extent, *Catches Criminals*. **"Nige"**, however, is a bit of a sneaky prick, and has been known from time to time to take a few backhanders in exchange for leaking information to the press and to other criminals. Therefore, to some extent, as well being a `Police Officer`, **"Nige"** is a `Bent Copper` because of his *Underhand Business* and *Dodgy Dealings*.

### How does this relate to typeclasses then?

**DS Kate Fleming** is not a typeclass.

**DC Nigel "Nige" Morton** is also not a typeclass.

*Liking A Pint* is not a typeclass.

Neither is *Underhand Business*.

### Typeclass examples

So what are?

+ `Human Person`

Both **DS Kate Fleming** and **DC Nigel "Nige" Morton** can have instances of the typeclass `Human Person`, because they *Breath*, *Eat* and *Poo*. They are 'lawful' instances of `Human Person` because I am too lazy to think of a reason that they are not.

+ `Police Officer`

**DS Kate Fleming** can be an instance of the typeclass of `Police Officer` because she *Catches Criminals* and *Likes A Pint*. She is also a 'lawful' instance, we can say, because as well as this, she does no *Underhand Business* or *Dodgy Dealings*.

**DC Nigel "Nige" Morton** is, to the innocent eye, an instance of the typeclass `Police Officer`. He also *Likes A Pint*, *Catches Criminals*, and although he is seen to have a marriage earlier in the show, later he is seen to be mowing his lawn in a solitary fashion shot through a lens tinged with regret. However, he is not a 'lawful' instance of the typeclass `Police Officer`, because of his *Underhand Business* or *Dodgy Dealings*.

+ `Bent Copper`

**DS Kate Fleming** is not in the typeclass `Bent Copper`, because (spoiler alert!) throughout the show she is seen to behave in a moral fashion befitting an officer working for AC-12, the anti-corruption unit at the centre of the whole show.

**DC Nigel "Nige" Morton** however is in the typeclass `Bent Copper`, exactly because of his *Underhand Business*, *Dodgy Dealings* and *Perhaps Not Really Needing That Walking Stick But Claiming Disability Benefits Regardless*. He is probably also a lawful instance of `Bent Copper` (although I understand at this juncture that this makes the whole analogy somewhat confusing) - simply because *Bent Coppers Don't Really Have Rules, And If They Did, Playing By Them Would Somewhat Defeat The Point.*

+ `Anti-Corruption Police Officer`

**DS Kate Fleming** is also in this typeclass because she *Nicks Bent Coppers*, whilst **DC Nigel "Nige" Morton** is not because he wouldn't *Dob In One Of Our Own*. Clear? As mud? Great.

### Back to stupid code

So how does this relate to computers and showing our colleagues how incredibly intellectual we are?

![Computer crime is also a type of crime](/images/typeclass6.png "Computer crime is also a type of crime")

Let's take an array. That's a thing you have probably used.

```javascript
const arr = [1, 2, 2, 4, 65, 7]
```

**arr** is not a typeclass, it is an object (or more broadly, a value).

A `Functor` is a typeclass though, and **arr** can be an instance of `Functor`.

The rules of a 'lawful' `Functor` is that we can run a Map function over the contents of the array and afterward *The Length Of The Array Remains The Same* and *It's Still An Array* and *Basically Nothing Really Weird Has Happened*.

```javascript
arr.map(x => x)
// arr == [1, 2, 2, 4, 65, 7]
```

As you see here, our array is the same length and the function has been applied to each item without any *Funny Business*. To tie it back to our police-based analogy - if **DS Kate Fleming** was given some important evidence to take back to the station, this result constitutes the evidence arriving at the station untampered for some forensic analysis that will pull the case together in the final nail-biting moments of the series.

However, what if **DC Nigel "Nige" Morton** was given that same evidence? That's a nice `65` value in there, surely nobody would notice if he snuck it for himself? Surely he's *Done His Bit Over The Years And Deserves It*, after all.

```javascript
var nigesFilthyPocket = []

arr.map(x => {
  if (x < 65) {
    return x
  } else {
    nigesFilthyPocket.push(x)
    return 0
  }
})
// arr == [1, 2, 2, 4, 0, 7]
```

If you can follow this code (and forgive the use of a mutable variable in an article that is, despite severely beating around the bush about the fact, written about the subject of functional programming) I think we can all agree that **"Nige"** here is a bit of a swine.

![An example of a bad crime](/images/typeclass5.png "An example of a bad crime")

**"Nige"**'s horrible law-breaking evidence-stealing `map` function does not make a 'lawful' `Functor`, as it breaks the *Basically Nothing Really Weird Has Happened* rule above by doing a massive side-effect and mutating an outside variable when it should just be doing nothing outside of the **arr**. However, if there is a `TotallyAwfulFunctor` typeclass, this would make an excellent instance of it.

### OK, I'm exhausted, please, tell me, what is a typeclass?

Sure.

![Irrelevant graph](/images/typeclass2.png "Irrelevant graph")

Objects and datatypes can be made instances of the typeclass `Functor`, provided they implement *map* without any funny business. These could be arrays, tree structure, `Maybe` and `Either`, and loads more. You can *map* over them all and expect the same behaviour.

People are instances of the typeclass `Human Being`, as in general they implement the *Breath*, *Eat* and *Poo* functions. (todo: add javascript example)

Anything that can be compared to another thing to check whether it is the same or not can be an instance of the typeclass `Eq`.

Many objects and datatypes can be made instances of the typeclass `Monad`, if they are also lawful `Functors` and `Applicative Functors` and implement *bind* and *return* functions. (What? Not now...)

Therefore...

### A typeclass is a way of categorising things based not on what they are but what they can and can't do.

(Thanks [Riku](https://okay.codes/) for helping me put that more succinctly. You may find it hard to believe that I have a tendency to ramble)

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)

[More about Typeclasses](/tags/typeclasses.html)
