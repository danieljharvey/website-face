---
title: Let's write a database (part 3) 
tags: database, query-engine, rust, joins 
---

Well, well, well, if it isn't part 3. Last time we tidied up a bit so that we're ready to tackle joins. As ever, all the code lives [right here](https://github.com/danieljharvey/lets-build-a-database).

## What is a join then?

Say we have a table of Albums that looks something like:

```json
{
  "albumId": 1,
  "title": "Best of the Beatles",
  "artistId": 1
},
{
  "albumId": 2,
  "title": "Horses",
  "artistId": 2
}
```

...and another table full of Artists like:

```json
{
  "artistId": 1,
  "name": "The Beatles"
},
{
  "artistId": 2,
  "name": "Patti Smith"
}
```

A join is what lets us look up the artist name for a given album.

```sql
select * from Albums
left join Artists on artistId
```

Will return something like:

```json
[
  {"albumId": 1, "title": "Best of the Beatles", "name": "The Beatles"},
  {"albumId": 2, "title": "Horses", "name": "Patti Smith"}
]
```
What makes it a 'join' is that for each row on the left hand side we attach the rows from the right hand table where the `join condition` (here, `Albums.artistId = Artists.artistId`). Where one row matches from each side like this, it's all very neat.

```sql
select * from Artists
left join Albums on artistId
```

Now, each artist may have zero or more albums, so our result will look like

```json
[
  {"artistId":1, "name": "The Beatles", "title": "Best of the Beatles"},
  {"artistId":1, "name": "The Beatles", "title": "Revolver"},
  {"artistId":1, "name": "The Beatles", "title": "Help"},
  {"artistId":2, "name": "Patti Smith", "title": "Horses"},
  {"artistId": 3, "name": "Johnny Noalbums"}
]
```

Because `The Beatles` have multiple albums, we make a copy of each left hand side for each right hand side. What about our old friend `Johnny Noalbums` though? He has no matching albums, but we still include the left hand side. If we didn't want to include him though, we could use an `inner join` instead:

```sql
select * from Artists
inner join Albums on artistId
```

Here we'd only include items where there is a match on both sides, so our result would look like:

```json
[
  {"artistId":1, "name": "The Beatles", "title": "Best of the Beatles"},
  {"artistId":1, "name": "The Beatles", "title": "Revolver"},
  {"artistId":1, "name": "The Beatles", "title": "Help"},
  {"artistId":2, "name": "Patti Smith", "title": "Horses"}
]
```
There are a bunch of other kinds of joins too, but we'll stick with these two for now.

## Implementing the joins

We'll be using the [classic hash join](https://en.wikipedia.org/wiki/Hash_join) to join our tables.

So in our `Artists -> Albums` join, we start by creating a `HashMap` and adding an empty array for each `artistId` found in the `Artists` table (as `artistId` is the column we are joining on). It should look like:

```json
{
  1: [],
  2: [],
  3: []
}
```

Then we go through the `Albums` table, and for every row with an `artistId`, add the entire row's values under the key in the `HashMap`. Our `HashMap` should now look like:

```json
{
  1: [
    {"artistId":1,"name":"Best of the Beatles"},
    {"artistId":1,"name":"Revolver"},
    {"artistId":1,"name":"Help"}
  ],
  2: [
    {"artistId":2,"name":"Horses"}
  ],
  3: []
}
```

The final step is to put everything together. For each row in the `Artist` table, we:

1. Look up the `artistId` in the `HashMap`.

2. For each entry we find, output a new row combining the `Artist` row with the `Album` entry from the `HashMap`.

3. if we don't find any entries, then
  - for `left outer join`, emit a new row containing the `Artist` row columns only, with `null` for any other columns in `Album`.

  - for `inner join`, don't emit a row. 

## In code 

We have a new `Query::Join` item.

```rust
pub struct Join {
    pub join_type: JoinType,
    pub left_from: Box<Query>,
    pub right_from: Box<Query>,
    pub on: JoinOn,
}

pub struct JoinOn {
    pub left: Column,
    pub right: Column,
}

pub enum JoinType {
    Inner,
    LeftOuter,
}
```

We deal with these in `run_query` as follows:

```rust
Query::Join(Join {
    left_from,
    right_from,
    join_type,
    on,
}) => {
    // get the left-hand side items
    let QueryStep {
        schema: left_schema,
        rows: left_rows,
    } = run_query(left_from)?;

    // get the right-hand side items
    let QueryStep {
        schema: right_schema,
        rows: right_rows,
    } = run_query(right_from)?;

    // join the left and right hand sides
    join::hash_join(
        left_rows,
        &left_schema,
        right_rows,
        &right_schema,
        on,
        join_type,
    )
}
```

Here is the `hash_join` function:

```rust
pub fn hash_join(
    left_rows: Vec<Row>,
    left_schema: &Schema,
    right_rows: Vec<Row>,
    right_schema: &Schema,
    on: &JoinOn,
    join_type: &JoinType,
) -> Result<QueryStep, QueryError> {
    let mut build_items = HashMap::new();

    // add all the relevant `on` values to map,
    for left_row in &left_rows {
        let value = 
          left_row
            .get_column(&on.left, left_schema)
            .ok_or_else(|| {
              QueryError::ColumnNotFoundInSchema {
                  column_name: on.left.clone(),
              }
            })?;

        build_items.insert(calculate_hash(value), vec![]);
    }

    // collect all the different right side values
    for right_row in right_rows {
        let value = right_row
            .get_column(&on.right, right_schema)
            .ok_or_else(|| QueryError::ColumnNotFoundInSchema {
                column_name: on.right.clone(),
            })?;

        // this assumes left or inner join and 
        // ignores where there's no left match
        if let Some(items) = build_items.get_mut(&calculate_hash(value)) {
            items.push(right_row.clone());
        }
    }

    let mut output_rows = vec![];

    for left_row in left_rows {
        let value = 
          left_row
            .get_column(&on.left, left_schema)
            .ok_or_else(|| {
              QueryError::ColumnNotFoundInSchema {
                  column_name: on.left.clone(),
              }
            })?;

        if let Some(rhs) = build_items.get(&calculate_hash(value)) {
            if rhs.is_empty() {
                // if left outer join
                if let JoinType::LeftOuter = join_type {
                    let mut whole_row = left_row.clone();

                    // we can't find value, so add a bunch of nulls
                    for _ in &right_schema.columns {
                        whole_row.items.push(serde_json::Value::Null);
                    }
                    output_rows.push(whole_row);
                }
            } else {
                for item in rhs {
                    let mut whole_row = left_row.clone();
                    whole_row.extend(item.clone());
                    output_rows.push(whole_row);
                }
            }
        }
    }

    let mut schema = left_schema.clone();
    schema.extend(right_schema.clone());

    Ok(QueryStep {
        rows: output_rows,
        schema,
    })
}
```

## Is that it?

Well yeah, sorta. We only handle two kinds of joins here, `left outer` and `inner`, and so there's a bunch more we can do here. Really with joins the trick is optimising them right - ideally we want the smaller table on the left hand side, for instance. We'll get to that once we start implementing some optimisations.

## What's next?

Haven't really decided yet actually. Perhaps some nice optimisations? Let's see what takes our fancy.

Make sense? If not, [get in touch](/contact.html)!

