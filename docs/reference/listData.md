# List all available databases

This function lists all publicly available Metapsy databases. It prints
all database
[shorthands](https://docs.metapsy.org/databases/#shorthand), which can
be used to import data using [`getData()`](getData.md).

## Usage

``` r
listData()
```

## Value

Returns a `data.frame` of all available databases by shorthand, along
with a URL for the database documentation entry.

## See also

[`getData`](getData.md)

## Author

Mathias Harrer <mathias.h.harrer@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
# List all available databases
listData()

# Get latest version of the 'depression-psyctr' database
d <- getData("depression-psyctr")

# Get version 22.2.0 of the 'depression-inpatients' database
d <- getData("depression-inpatients", "22.2.0")

# Show variable description
d$variableDescription()

# Open online documentation
d$openDocumentation()

# Analyze using metapsyTools
library(metapsyTools)
runMetaAnalysis(d, which.run = "combined")
} # }
```
