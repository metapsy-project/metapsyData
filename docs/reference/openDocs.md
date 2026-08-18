# Open database documentation

This function opens the online documentation of an imported database.

## Usage

``` r
openDocs(data)
```

## Arguments

- data:

  A database `data.frame` imported using [`getData()`](getData.md).

## Value

Opens the database documentation entry on
[docs.metapsy.org](https://docs.metapsy.org/databases/).

## See also

[`listData`](listData.md), [`getData`](getData.md),
[`variableDescription`](variableDescription.md),

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
