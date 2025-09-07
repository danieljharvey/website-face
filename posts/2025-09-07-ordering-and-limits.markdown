---
title: Let's write a database (part 4) 
tags: database, query-engine, rust 
---

Hello, hello, hello. Welcome to part four of this adventure where we're making a database by hand. In part three we did some joins, and today we're doing to do some limiting. 

If you'd like to look at the code, it's [right here](https://github.com/danieljharvey/lets-build-a-database).

## No no, no no no no, no no no no, no no, there's no limits

Limit is very straightforward, it's used to control the amount of data we get back from a query.

```sql
select * from albums limit 10
```

...will get 10 items from the albums table. To help decide how we choose _which_ 10, we'll need ordering which will come next. 

We add a new `Limit` to the `Query` type, containing the following:

```rust
pub struct Limit {
    pub from: Box<Query>,
    pub limit: u64,
}
```

Then we add this to our `run_query` function:

```rust
Query::Limit(Limit { limit, from }) => {
    let QueryStep {
        schema,
        mut rows,
    } = run_query(from)?;

let size: usize = (*limit).try_into().unwrap();

    rows.truncate(size);

    Ok(QueryStep { schema, rows })
}
```

That's the whole thing. What a disappointing blog post.

## What's next?

Next time we're doing ordering, which is marginally more exciting, but not that exciting tbh.

Make sense? If not, [get in touch](/contact.html)!

