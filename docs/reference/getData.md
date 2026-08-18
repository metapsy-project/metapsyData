# Import Metapsy databases into the R environment

This function allows to download all publicly available versions of
Metapsy databases into your R environment.

## Usage

``` r
getData(shorthand,
        version = NULL,
        include.metadata = TRUE)
```

## Arguments

- shorthand:

  `character`. The shorthand assigned to the database that should be
  downloaded. Shorthands are listed under "Metadata" in the [database
  documentation entry](https://docs.metapsy.org/databases).
  Alternatively, all available databases and their shortcodes can be
  accessed by running [`listData()`](listData.md).

- version:

  `character`. The version number to be downloaded. Default is `NULL`,
  which downloads the latest version.

- include.metadata:

  `logical`. If set to `TRUE`, the function will return an
  [`R6::R6Class()`](https://r6.r-lib.org/reference/R6Class.html) object
  that includes metadata associated with the database version. See
  "Value".

## Value

If `include.metadata` is set to `TRUE`, the `getData` function will
return both the requested dataset itself, as well as the metadata
associated within in it. Metadata items included by default are:

- `database.doi`: The digital object identifier of the database
  ("Concept DOI" in the [Zenodo API](https://zenodo.org/). This DOI will
  always resolve to the latest database version).

- `documentation.url`: URL of the database documentation entry on the
  [Metapsy Documentation page](https://docs.metapsy.org/databases/).

- `github.repo.url`: URL of the specific Github repository state at
  which the database was released.

- `last.search`: Date of the last search.

- `last.updated`: Date of the last database updated (release).

- `license`: License of the database.

- `repository.download.url`: Link to download the entire database from
  Zenodo.

- `title`: Title of the database (including its version).

- `variable.description`: Description of the variables included in the
  dataset.

- `version`: Version of the database.

- `version.doi`: DOI associated with the specific version of the
  database. In contrast to the `database.doi`, this identifier will also
  link to this specific database version.

The returned
[`R6::R6Class()`](https://r6.r-lib.org/reference/R6Class.html) object
also contains a few helpful functions, which can be called directly from
the object:

- `downloadZip()`: This will download the database (including metadata)
  as a .zip file from its Zenodo repository.

- `openDocumentation()`: This opens the database documentation entry on
  the [Metapsy Documentation page](https://docs.metapsy.org/databases/).

- `openGitRepo()`: This opens the Github repository of the database at
  the state of its release.

- `returnMetadata()`: Returns the entire metadata stored in the
  `metapsyDatabase` R6 object as a `list`.

- `variableDescription()`: This prints a variable description of the
  database in the R Console.

If `include.metadata` is `FALSE`, the function will return the dataset
as a simple `data.frame`.

## Details

The function can work with or without a Zenodo API access token. While a
token is not required, setting one provides several benefits:

- **Higher rate limits**: Authenticated requests can fetch up to 100
  records per page, compared to 25 records per page for unauthenticated
  requests. This significantly speeds up data retrieval for databases
  with many versions.

- **Reduced rate limiting**: Authenticated requests are less likely to
  hit Zenodo's rate limits, especially when fetching multiple pages of
  results.

- **Automatic pagination**: The function automatically handles
  pagination to retrieve all versions, but this is faster with a token
  due to larger page sizes.

If no token is set, the function will optionally prompt you to set one.
You can also set or update your token at any time using
[`updateZenodoAccessToken()`](updateZenodoAccessToken.md).

To create a Zenodo API access token:

1.  Log into your Zenodo account.

2.  Visit [this
    page](https://zenodo.org/account/settings/applications/tokens/new/).

3.  Create a new personal access token.

4.  Copy the token and use
    [`updateZenodoAccessToken()`](updateZenodoAccessToken.md) to save
    it.

The token will be saved to your `.Renviron` file for permanent storage
across R sessions. The function automatically validates tokens and will
clear invalid or expired tokens.

## See also

[`listData`](listData.md),
[`updateZenodoAccessToken`](updateZenodoAccessToken.md)

## Author

Mathias Harrer <m.harrer@vu.nl>

## Examples

``` r
if (FALSE) { # \dontrun{
# List all available databases
listData()

# Get latest version of the 'depression-psyctr' database
d <- getData("depression-psyctr")

# Get version 24.0.2 of the 'depression-psyctr' database
d <- getData("depression-psyctr", "24.0.2")

# Show variable description
d$variableDescription()

# Open online documentation
d$openDocumentation()

# Analyze using metapsyTools
library(metapsyTools)
runMetaAnalysis(d)
} # }
```
