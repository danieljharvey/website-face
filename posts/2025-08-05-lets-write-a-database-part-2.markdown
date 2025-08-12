---
title: Let's write a database (part 2) 
tags: database, query-engine, rust 
---

Hello, hello, hello. Welcome to part two of this adventure where we're making a database by hand. In part one we stole a SQL parser, did some table scans, filtered some results, and projected the ones we're interested in. I promised we'd look at joins today, but first we need to do a bit of housekeeping. 

If you'd like to look at the code, it's [right here](https://github.com/danieljharvey/lets-build-a-database).

## Storing our rows better

Previously we passed our rows around as `Vec<serde_json::Value>` so each row was a JSON value that looked like this:

```json
{ 
  "id": 1,
  "name": "Mr Horse",
  "age": 100
}
```

This is poor for two reasons:

1. Having the column names in every row is wasteful

2. If we want to rename columns (or disambiguate the "name" column from two tables that have been joined), we have to change every row

Instead we have a `Row` type that contains a `Vec` full of individual `serde_json::Value` types for each column entry:

```rust
pub struct Row {
    pub items: Vec<serde_json::Value>,
}
```

And we pass around a single `Schema` type that holds all the column names: 

```rust
pub struct Schema {
    pub columns: Vec<Column>,
}

pub struct Column {
    pub name: String,
}
```


## Changes to our query functions

Previously we just returned a big pile of rows from each query function, but now each one returns a `QueryStep` type:

```rust
pub struct QueryStep {
    pub schema: Schema,
    pub rows: Vec<Row>,
}
```

Now we have a bunch of rows, and a `Schema` so we know what's in them.

### From 

When selecting fields, we grab all the rows, and then return a schema too. These are hardcoded for now:

```rust
fn schema(table_name: &TableName) -> Vec<Column> {
    match table_name.0.as_str() {
        "Album" => vec!["AlbumId".into(), "Title".into(), "ArtistId".into()],
        "Artist" => vec!["ArtistId".into(), "Name".into()],
        "Track" => vec![
            "TrackId".into(),
            "Name".into(),
            "AlbumId".into(),
            "MediaTypeId".into(),
            "GenreId".into(),
            "Composer".into(),
            "Milliseconds".into(),
            "Bytes".into(),
            "UnitPrice".into(),
        ],
        _ => todo!("unknown schema"),
    }
}
```

### Filtering 

For instance, this means our filtering now looks like this, passing the schema through unchanged from whatever `Query` it wraps:

```rust
Query::Filter(Filter { from, filter }) => {
    let QueryStep {
        schema,
        rows,
    } = run_query(from)?;

    let mut filtered_rows = vec![];

    for row in rows {
        if filter::apply_predicate(&row, &schema, filter)? {
            filtered_rows.push(row);
        }
    }

    Ok(QueryStep {
        schema,
        rows: filtered_rows
    })
}
```

### Projections

Our projections let us drop and reorder fields, so they'll change the schemas as well as the rows:

```rust
Query::Project(Project { from, fields }) => {
    let QueryStep {
        schema,
        rows,
    } = run_query(from)?;

    let mut projected_rows = vec![];

    for row in &rows {
        projected_rows.push(project::project_fields(row, &schema, fields)?);
    }

    let schema = project::project_schema(&schema, fields)?;

    Ok(QueryStep {
        schema,
        rows: projected_rows,
    })
}
```

As well as a `project_fields` function we have a matching `project_schema` function that creates a new schema.

```rust
pub fn project_schema(schema: &Schema, fields: &[Column]) 
  -> Result<Schema, QueryError>
{
  let mut columns = vec![];

  for field in fields {
    let index = schema.get_index_for_column(field).ok_or_else(|| {
      QueryError::ColumnNotFoundInSchema {
        column_name: field.clone(),
      }
    })?;

    let column = schema
      .columns
      .get(index)
      .ok_or(QueryError::IndexNotFoundInSchema { index })?;

    columns.push(column.clone());
  }

  Ok(Schema { columns })
}
```

### Outputting everything

We still want to output everything in JSON as before, so our `QueryStep` has a `to_json` function that puts everything back as it was before.

```rust
impl QueryStep {
  // reconstruct JSON output
  pub fn to_json(&self) -> serde_json::Value {
    let mut output_rows = vec![];

    for row in &self.rows {
      let mut output_row = serde_json::Map::new();
      for column in &self.schema.columns {
        let value = row.get_column(column, &self.schema).unwrap();
        output_row.insert(column.to_string(), value.clone());
      }
      output_rows.push(serde_json::Value::Object(output_row));
    }
    serde_json::Value::Array(output_rows)
  }
}
```

After this, all the outputs look the same as before and our tests all pass again. Nice!


## What's next?

OK, we've got all our ducks in a row, next time we'll do the joins, I promise.

Make sense? If not, [get in touch](/contact.html)!

