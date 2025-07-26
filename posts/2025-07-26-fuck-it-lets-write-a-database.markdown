---
title: fuck it, let's write a database 
tags: database, fuck it, 
---

# Fuck it, let's write a database

So, confession, I love playing on the computer, but I'm fucking terrible at SQL. I know that it's Good or whatever and there's this whole very sensible looking [Relational Algebra](https://en.wikipedia.org/wiki/Relational_algebra) behind it all, but like when I have to interact with it, I get the job done and then immediately wipe whatever I learned from my brain.

Enough is enough. It's time to learn it properly. And what better way than to write an ANSI SQL database from scratch!

## Disclaimers etc 

Setting some expectations, this is very much a "fun" learning project, and we're gonna fuck a lot of things up. We'll be using Rust, because I write that a lot atm and so it's what my brain thinks in, but I would definitely read this as a Rust tutorial because I am almost as bad at Rust as SQL. I am going to describe things in a very hand-wavy way that will probably annoy people that know what they're talking about, but fuck it, it's my blog and I can do what I want.

## So, let's fucking do it?

OK. So what we're going to do in part 1 is:

- Make (steal) a SQL parser
- Do a table scan
- Filter results from that table

Because we're only starting with reads, our tables are going to be static JSON files taken from the [chinook dataset](https://github.com/marko-knoebl/chinook-database-json). We are using this because it's full of rock albums and it's nice to be reminder that Led Zeppelin are a totally sick band from time to time.

## What's next?

In part two we're going to add some JOINS. 

Make sense? If not, [get in touch](/contact.html)!

Further reading:

