<h1><code style="background: white; font-size: 45px !important; font-weight: 500; color: black;">metapsyData</code></h1>

<br></br>

## Overview

This package allows you to access the [Metapsy](https://www.metapsy.org) meta-analytic psychotherapy databases in R. To install the metapsyData package, use the following code:

```r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("metapsy-project/metapsyData")
```

The metapsyData package allows to access the Metapsy meta-analytic psychotherapy databases directly in your R environment. Once installed, simply use the `getData` function and insert the [database shorthand](https://docs.metapsy.org/databases/#shorthand) to save the data locally.

To search for available databases in metapsyData and their shorthands, run the `listData` function. All databases included in metapsyData come with an extensive documentation entry on [docs.metapsy.org](https://docs.metapsy.org/databases).

Databases loaded into the R environment can directly be analyzed using functions of the [metapsyTools](https://tools.metapsy.org) package. 

<br></br>

## Usage Example

```r
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


