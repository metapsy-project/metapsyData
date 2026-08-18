# Print `metapsyDatabase` objects

Prints the dataset (`data`) if objects returned by
[`getData()`](getData.md) are of class `metapsyDatabase` (i.e. if
`include.metadata=TRUE`).

## Usage

``` r
# S3 method for class 'metapsyDatabase'
print(x, first = NULL, last = NULL, ...)
```

## Arguments

- x:

  A database of class `metapsyDatabase` imported using
  [`getData()`](getData.md).

- first:

  Number of first rows to be printed.

- last:

  Number of last rows to be printed.

- ...:

  Additional arguments.

## Author

Mathias Harrer <mathias.h.harrer@gmail.com>, Paula Kuper
<paula.r.kuper@gmail.com>, Pim Cuijpers <p.cuijpers@vu.nl>
