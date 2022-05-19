# `metapsyData`: Access the Meta-Analytic Psychotherapy Databases in R

To install the `metapsyData` package, use the following code:

```r
if (!require("devtools"))
  install.packages("devtools")

devtools::install_github("metapsy-project/metapsyData")
```

The `metapsyData` package allows to access the Metapsy meta-analytic psychotherapy databases direct in your `R` environment. Once installed, simply run the `data` function (e.g. `data(depression)`) to save the data locally. The documentation of the package is also hosted by [rdrr.io](https://rdrr.io/github/metapsy-project/metapsyData/).

The interactive Metapsy web application ([metapsy.org](https://www.metapsy.org/)) uses `metapsyData` in the background. You can open the Metapsy website in `R` by running `open_app()`.

The raw data files can be accessed in the associated GitHub repository under `data`. To search for available databases in `metapsyData`, type in `metapsyData::` in your RStudio console.

## Repository Rules

You are allowed to use, fork and share the package. Please note that we will **not consider** pull requests or issue reports from **external users** who are not part of the Metapsy project collaboration. 
