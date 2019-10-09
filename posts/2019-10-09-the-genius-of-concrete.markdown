---
title: The Genius Of Concrete
tags: programming, thought leadership
---

When I was a young child I was what I would now call "quite trying". One of the ways that I achieved
this was by asking lots of questions. Once I asked an adult (I think a parent but could have as easily
been a stranger trying to do their weekly shop) the following:

*"Where do washing machines come from?"*

A reasonable, if somewhat tangental question I suppose. I can't remember why I
was interested.


The answer (which I can see now was somewhat trolling, which leads me to think this was somebody I had asked questions to before) blew my tiny mind.

*"A washing machine"*, they told me, *"is made in a washing-machine-making-machine."*

Very well, I thought. But that wasn't enough.

*"So where do the washing-machine-making-machines come from?"*

(That'll fox them)

*"Ahh, well you see"*, the reply went *"those washing-machine-making-machines are
made by washing-machine-making-machine-making-machines"*

This went on as you might predict. I won't bore you with what was obviously some sort of early lesson in the dangers of
unsupervised recursion, but as you can imagine, it was machine-making-machines
all the way down in my dreams for months after that.

![Did you really just google 'horrible washing machine' and put the first one you found in your blog?.](/images/washing-1.png "Did you really just google 'horrible washing machine' and put the first one you found in your blog?")

Anyway. There was a point to this. Or at least I thought there was when I
couldn't get to sleep and decided I needed to get up and write all this down.

### Abstractions

So when using the computers and things, we generally start with very concrete
problems, such as *"how we make a washing machine"*, and then later, *"also, how can we put another light on this machine machine that
blinks when the washing is ready?"*.

Now, being close to the metal, as such, the manual manufacturer of said washing machine
is not going to have too much trouble (crudely, perhaps) adding such a light and making it work. After all, 
they are equipped with washing machine making tools (hammer? wrench?
screwdriver? bear with me here) for the job.

### But is the true work of an engineer?

The trouble is, nobody makes washing machines by hand anymore, at least not for
long. Once the maker has gotten the hang of things they think *"this work is
very manual and perhaps somewhat beneath me now, surely we can make this easier?"*

Soon enough, their days with the hammer, the wrench and the
screwdriver are wasted, as they have constructed the **washing-machine-maker**.

![It was probably something like this.](/images/washing-2.png "It was probably
something like this.")

### We are all very pleased about this development.

It really is good stuff. We're making washing machines at a much quicker pace
now, and soon somebody hires in some consultants who point out that we could be
scaling a lot better if we abstract further. Soon enough, they're right - and
through a mixture of capitalism and boredom the  **washing-machine-making-machine-maker** is born.

![It was probably something like this.](/images/washing-3.png "It was probably
something like this.")

### Nice.

One thing we notice in making the even bigger machine is that many of the parts
in the big machine aren't as specialised as we thought, and we replace them
with a combination of more general parts that are tried and tested. This is
generally agreed to be good engineering practice, plus additionally the tried and tested stuff breaks less and is cheaper, so
everybody involved is having a positive time.

It's not long until somebody points out that with a little tweaking, we can expand and have a higher level machine called the **washing-machine-and-also-fridge-making-machine-maker-machine-maker** that is also capable of making something that may at one point end up making a fridge as well. Business is booming.

![It was probably something like this.](/images/washing-4.png "It was probably
something like this.")

### Oh shit.

But wait. A new requirement has been found.

The washing machine needs Another Light on it. This one is a new colour, not
found in washing machines or fridges. The customer bought one of our first
fridges and was impressed with how quickly they had worked for him. However now
changing the washing machine isn't a job for a hammer, a wrench or a
screwdriver anymore, it's going to require changes to the
**washing-machine-and-also-fridge-making-machine-maker-machine-maker** to
change the **washing-machine-making-machine-maker** to construct a  **washing-machine-maker** that will output the required **washing-machine**.

It'd probably be quicker to knock it all down and start over, but now we've got
a shitload of fridges to worry about too.

![Oh shit indeed.](/images/fridge.png "Oh shit indeed.")

### Get to the point, you goddamn meandering jerk.

My point here is that the recursive levels of washing machine creating
abstractions here are taking us further and further from our real aim here,
which in this already stretched allegory seems to be *"maintain an ability to
add lights to washing machines in a timely manner as and when users of said
machine take an interest in such."*

These building blocks, they're all very well, but why did we really start this?
Was it because we needed to? Or because we were bored of building fridges?

### Let's talk about concrete.

So.

Yes.

Let's.

Concrete is shit.

(not literally, I mean it has very little value. I don't know what it's made of, and looking it up now on
Wikipedia would seem somewhat insincere, but I'm pretty sure it's not made of
actual shit)

![It's basically 'big sand'.](/images/concrete.png "It's basically 'big sand'.")

However, I am pretty confident you can make things out of it.

Hole in the floor? **Pour that shit in!**

Need a wall? **Slop that shit up!**

Got it wrong? **Smash that shit to pieces!**

(People can visit the Barbican all they like, it does not change the fact that nobody in human history has been or ever will be emotionally attached to the stuff.)

Let's not be mistaken - our collection of recursive white goods factories are very nice. They may be the solution sometimes.
But I'm scared about how much emotional investment they have in them. When they're wrong, we're too attached, so we bend them to the new needs,
and they become more and more grossly deformed. They're also difficult to work
on, because changing a washing machine design via 4 layers of abstraction is
difficult, especially to a newcomer who probably somewhat oversold their interest in
washing machines during the interview process.

### Being boring.

Concrete code is boring code to write. It's easy to understand though, and it's
even easier to delete, because it's not wrapped up in 5 other levels of
abstraction. It can't be, it's just too...lumpy.

Do you really need to solve the meta-problem of your problem? Perhaps! I'm not
suggesting there aren't generalised problems worth solving, but are you solving
the generalised problem because your actual problem isn't interesting enough?

What if I told you, you could solve the easy problem, then do something else?

I have been told **"Love the problem, not the solution"** before - and I think I just realised
what that means - don't get wrapped up in cleverness. Pour the stupidest thing
you have onto the problem until it goes away, then go home and have a nice
time.

It probably doesn't get you promoted though.
