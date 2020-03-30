---
title: Validation Strategies
tags: typescript, validation, ux
---

If you've read any my posts before, you'll know I'm a pretty exciting guy, so
it will come as no surprise to you that I am very interested in validating user
input. Now there is a lot of information about elegant ways to check data (and
how to tag it with types afterwards so we know it's the Good Stuff), but I find
less about to tell the users all about it.

So, picture the scene, if you will, you are filling in a reasonably big form
(it includes, say, your name, an email, and choosing a password of some kind).

If like me, you are reasonably terrible at the pedestrian operation of
computers and websites, you're probably going to fuck up some of these fields.

```bash
me: type type type type....
computer: "WHAT THE FUCK ARE YOU DOING? THAT'S FUCKING RIDICULOUS!"
me: ...type type...?
computer: "Oh, that's fine, great job."
me: *phew*
me: *chooses next field*
me: type type type type...
computer: "NOT AGAIN FOR FUCKS SAKE DO YOU NEVER GET ANYTHING RIGHT?"
me: *logs off, never to be seen again*
```

Later, in `computer`'s managerial 1-2-1...

```bash
manager: "So, is there anything you think you could have differently...?"
computer: "But they were wrong!"
manager: "..."
computer: "..."
manager: "...but is that the point here?"
