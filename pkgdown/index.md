<h1>
  <code style="background: white;">metapsyData</code>
</h1> 

![R CMD Check](https://img.shields.io/badge/R%20CMD%20Check-passing-brightgreen)
[![DOI](https://img.shields.io/badge/DOI-10.5281/zenodo.6566924-blue)](https://doi.org/10.5281/zenodo.6566924)

This package allows you to access the [Metapsy](https://www.metapsy.org) meta-analytic psychotherapy databases in R. To install the `metapsyData` package, use the following code:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("metapsy-project/metapsyData")
```

The `metapsyData` package allows to access the Metapsy meta-analytic psychotherapy databases directly in your R environment. Once installed, simply use the `getData` function and insert the [database shorthand](https://docs.metapsy.org/databases/#shorthand) to save the data locally. The documentation of the package is also hosted by [rdrr.io](https://rdrr.io/github/metapsy-project/metapsyData/).

To search for available databases in `metapsyData` and their shorthands, run the `listData()` function. All databases included in `metapsyData` come with an extensive documentation entry on [docs.metapsy.org](https://docs.metapsy.org/databases).

Databases loaded into the R environment can directly be analyzed using functions of the [`metapsyTools`](https://tools.metapsy.org) package. 

<br></br>

## Usage Example

```r
# List all available databases
listData()

# Get latest version of the 'inpatients' database
d <- getData("inpatients")

# Show variable description
d$variableDescription()

# Open online documentation
d$openDocumentation()

# Analyze using metapsyTools
library(metapsyTools)
runMetaAnalysis(d)
```

<br></br>

<br></br>
