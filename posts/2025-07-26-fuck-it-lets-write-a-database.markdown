---
title: Let's write a database (part 1) 
tags: database, filter, from, project 
---

So, confession, I love playing on the computer, but I'm terrible at SQL. I know that it's Good or whatever and there's this whole very sensible looking [Relational Algebra](https://en.wikipedia.org/wiki/Relational_algebra) behind it all, but like when I have to interact with it, I get the job done and then immediately wipe whatever I learned from my brain.

Enough is enough. It's time to learn it properly. And what better way than to write an ANSI SQL database from scratch!

## Disclaimers etc 

Setting some expectations, this is very much a "fun" learning project, and we're gonna get a lot of things wrong. We'll be using Rust, because I write that a lot atm and so it's what my brain thinks in, but I would definitely not read this hoping for a Rust tutorial because I am almost as bad at Rust as SQL. I am going to describe things in a very hand-wavy way that will probably annoy people that know what they're talking about, but it's my blog and I can do what I want.

## So, let's do it?

OK. So what we're going to do in part 1 is:

- Make (steal) a SQL parser
- Do a table scan (`From`)
- Filter results from that table (`Filter`)
- Choose the fields we want to look at (`Project`)

Because we're only starting with reads, our tables are going to be static JSON files taken from the [chinook dataset](https://github.com/marko-knoebl/chinook-database-json). We are using this because it's full of rock albums and it's nice to be reminder that Led Zeppelin are a totally sick band from time to time.

## Our types

We might not be implementing in Haskell but it's spirit lives on - let's start by defining the types of our query.

```rust
// the type that defines a query
enum Query {
  From(From),
  Filter(Filter),
  Project(Project)
}

// select `FROM` a table
struct From {
  table_name: TableName
}

// filter the results for a `WHERE` clause
struct Filter {
  from: Box<Query>,
  filter: Expr,
}

// boolean expression type
pub enum Expr {
    ColumnComparison {
        column: Column,
        op: Op,
        literal: serde_json::Value,
    },
}

// compare two items  
pub enum Op {
    Equals,
}

// choose which fields to return 
struct Project {
  from: Box<Query>,
  fields: ProjectFields 
}

// which fields to return
enum ProjectFields {
  Star,
  Fields(Vec<ColumnName>)
}

// name of a table
struct TableName(pub String);

// name of a column
struct ColumnName(pub String);
```

## A parser

This could quickly become a parsing tutorial and we don't want that, so we're going to use the [sqlparser](https://docs.rs/sqlparser/latest/sqlparser) crate. It takes a string input and returns either it's own AST or an error. We'll pattern match on this and extract only the things we support into a `Query` type we defined above.

Nothing about this is very interesting, so I will just [link to it](https://github.com/danieljharvey/lets-build-a-database/blob/main/crates/core/src/parser.rs#L51). Know that we parse some SQL and make the types above.



## Our `run_query` function  

Now we've worked out what the user wants, we need to run the query. Initially we'll do this by matching on the `Query` type. 

```rust
pub fn run_query(query: &Query) -> Vec<serde_json::Value> {
    match query {
        Query::From(_) => todo!("Query::From"), 
        Query::Filter(_) => todo!("Query::Filter"), 
        Query::Project(_) => todo!("Query::Project")
    }
}
```
We'll fill these `todo!` out one by one now.

## Query::From

The first thing we'll implement is a simple table scan. A table scan is "get all of the rows in the table". If you're thinking "that doesn't sound wildly performant", rest assured your Software Craftsperson spidey-sense is still working correctly. However, our tables only have ~300 items in them, so for now we'll live with it until we start thinking about indexes.

Here is some code. Forgive me, Padre.

```rust
fn table_scan(table_name: &TableName) -> Vec<serde_json::Value> {
    match table_name.0.as_str() {
        "Album" => {
            let my_str = include_str!("../static/Album.json");
            serde_json::from_str::<serde_json::Value>(my_str)
              .unwrap()
              .as_array()
              .unwrap()
              .clone()
        },
        "Artist" => {
            let my_str = include_str!("../static/Artist.json");
            serde_json::from_str::<serde_json::Value>(my_str)
              .unwrap()
              .as_array()
              .unwrap()
              .clone()
        }
        _ => panic!("table not found {table_name:?}"),
    }
}
```

Let's smash that into our `run_query` function:

```rust
pub fn run_query(query: &Query) -> Vec<serde_json::Value> {
    match query {
        Query::From(From { table_name }) => table_scan(table_name), 
        ..  
```

It is not good code, but it is code. We'd test it, but it would still fail because of the other `todo!`. Oh well. Onwards.

## Query::Filter

Call me a staunch traditionalist, but often when I am accessing a database I do not wish to download all of it's data at once. We're going to allow users to filter data using a `where` clause, which lets us define properties about rows we are interested in.

Let's recap on our `Expr` type:

```rust
pub enum Expr {
    ColumnComparison {
        column: Column,
        op: Op,
        literal: serde_json::Value,
    },
}

pub enum Op {
    Equals,
}
```

This is pretty limited, but it does let us express `select * from Album where album_id = 1`.

We start by defining a function for deciding whether we care about a row. It takes a row (which we store as a `serde_json::Value`) and an `Expr`, returning a `bool` telling us to keep the row or throw it in the bin.

```rust
fn apply_predicate(row: &serde_json::Value, where_expr: &Expr) -> bool {
    match where_expr {
        Expr::ColumnComparison {
            column,
            op,
            literal,
        } => {
            // unwrap row into a map
            let row_object = row.as_object().unwrap();

            // grab the column we care about 
            let value = row_object.get(&column.name).unwrap();

            // compare it to `value` 
            match op {
                Op::Equals => value == literal,
            }
        }
    }
}
```

Let's use it in our `run_query` function:

```rust
pub fn run_query(query: &Query) -> Vec<serde_json::Value> {
    match query {
        Query::From(From { table_name }) => table_scan(table_name),
        Query::Filter(Filter { from, filter }) => run_query(from)
            .into_iter()
            .filter(|row| apply_predicate(row, filter))
            .collect(), 
        ..
```
We'd test a query, but it'd still fail. But nearly!

## Query::Project

So far we return every single field from our table scan, so every `select` is a `select * from ...`. We can do better than that, let's implement `Project`, which is how we extract fields from rows. Eventually, we'll allowing renaming things with aliases, but that's quite boring and fiddly, so for now we're just supporting stuff like `select Title, ArtistId from Album`.

We've got two styles of projection:

```rust 
enum ProjectFields {
  // return everything
  Star,
  // return a filtered subset of fields
  Fields(Vec<ColumnName>)
}
```
First we'll make a function that works on a single row:

```rust 
fn project_fields(row: serde_json::Value, fields: &[Column]) -> serde_json::Value {
    // make set of columns to keep
    let field_set: BTreeSet<_> = 
      fields
        .iter()
        .map(|c| c.name.clone())
        .collect();

    
    if let serde_json::Value::Object(map) = row {
        // collect all the items we still want
        let new_map = map
            .into_iter()
            .filter(|(k, _)| field_set.contains(k))
            .collect();

        // wrap it back up again
        serde_json::Value::Object(new_map)
    } else {
        panic!("expected Object")
    }
}
```

Then we add it to `run_query`, completing it for now.

```rust 
pub fn run_query(query: &Query) -> Vec<serde_json::Value> {
  match query {
    Query::From(From { table_name }) => table_scan(table_name),
    Query::Filter(Filter { from, filter }) => run_query(from)
      .into_iter()
      .filter(|row| apply_predicate(row, filter))
      .collect(),
    Query::Project(Project { from, fields }) => {
      let inner = run_query(from);

      match fields {
        // just return everything
        ProjectFields::Star => inner,
        // filter the columns in each row
        ProjectFields::Fields(fields) => inner
          .into_iter()
          .map(|row| project_fields(row, fields))
          .collect(),
      }
    }
  }
}
```


## Bringing it together

We then add a basic CLI using [clap](https://docs.rs/clap/latest/clap/), that takes a single argument `--sql`. Nothing surprising or interesting here, sorry.

```rust
use clap::Parser;
use core::{parse, run_query};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// SQL query to run
    #[arg(short, long)]
    sql: String,
}

fn main() {
    let args = Args::parse();

    let query = parse(&args.sql).unwrap();
    let results = run_query(&query);

    for result in results {
        println!("{result}");
    }
}
```

This means we can run a query with `cargo run --bin cli -- --sql 'select Title from Album where AlbumId = 48' | jq ` and look what we get:

```bash 
{
  "AlbumId": 48,
  "Title": "The Essential Miles Davis (Disc 1)",
  "ArtistId": 68
}
```

Not bad, not bad at all.

## What's next?

If you've made it this far without being furious about my use of `unwrap()`, then in part two we're going to add some JOINS. 

Make sense? If not, [get in touch](/contact.html)!

