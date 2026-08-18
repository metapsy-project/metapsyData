# Show variable description for Metapsy database

This function prints the variable description included in the metadata
of an imported Metapsy database.

## Usage

``` r
variableDescription(data)
```

## Arguments

- data:

  A database `data.frame` imported using [`getData()`](getData.md).

## Value

Returns a `data.frame` with two columns:

- `variable`. The variable name.

- `description`. The variable description, including factor level
  explanations.

## See also

[`listData`](listData.md), [`getData`](getData.md),
[`openDocs`](openDocs.md)

## Author

Mathias Harrer <mathias.h.harrer@gmail.com>

## Examples

``` r
if (FALSE) {
# List all available databases
listData()

# Get latest version of the 'depression-psyctr' database
d <- getData("depression-psyctr")

# Get version 22.2 of the 'depression-psyctr' database
d <- getData("depression-psyctr")

# Show variable description
variableDescription(d)

# Open online documentation
openDocs(d)
}
```
