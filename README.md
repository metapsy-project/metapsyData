<h1>
  <code style="background: white;">metapsyData</code>
</h1> <a href='https://www.metapsy.org'><img src='https://tools.metapsy.org/logo.png' align="right" height="139" /></a>

![R CMD Check](https://img.shields.io/badge/R%20CMD%20Check-passing-brightgreen)
[![DOI](https://img.shields.io/badge/DOI-10.5281/zenodo.6566924-blue)](https://doi.org/10.5281/zenodo.6566924)
[![Netlify Status](https://api.netlify.com/api/v1/badges/68f8ecb5-abc5-4cd3-a3e3-0d0526768abf/deploy-status)](https://app.netlify.com/sites/metapsydata/deploys)

This package allows you to access the meta-analytic psychotherapy databases in R. To install the `metapsyData` package, use the following code:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("metapsy-project/metapsyData")
```

The `metapsyData` package allows to access the Metapsy meta-analytic psychotherapy databases direct in your `R` environment. Once installed, simply use the `getData` function and insert the [database shorthand](https://docs.metapsy.org/databases/#shorthand) to save the data locally. The documentation of the package is also hosted by [rdrr.io](https://rdrr.io/github/metapsy-project/metapsyData/).

To search for available databases in `metapsyData` and their shorthands, run the `listData()` function. All databases included in `metapsyData` come with an extensive documentation entry on [docs.metapsy.org](https://docs.metapsy.org/databases).

## Usage Example

```
# List all available databases
listData()

# Get latest version of the 'inpatients' database
d <- getData("depression-inpatients")

# Show variable description
d$variableDescription()

# Open online documentation
d$openDocumentation()

# Analyze using metapsyTools
library(metapsyTools)
runMetaAnalysis(d)
```

## Repository Rules

You are allowed to use, fork and share the package. Please note that we will **not consider** pull requests or issue reports from **external users** who are not part of the Metapsy project collaboration. 
