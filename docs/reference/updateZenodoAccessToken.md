# Update Zenodo API Access Token

This function allows you to update or set your Zenodo API access token.
The token will be saved to your `.Renviron` file for permanent storage
and will also be set for the current R session.

## Usage

``` r
updateZenodoAccessToken(token = NULL)
```

## Arguments

- token:

  `character`. The Zenodo API access token. If `NULL` (default), the
  function will interactively prompt you to enter the token. You can
  create a token
  [here](https://zenodo.org/account/settings/applications/tokens/new/).

## Value

Invisibly returns `TRUE` if successful, `FALSE` otherwise.

## Details

The function performs the following actions:

- If `token` is `NULL`, prompts you to enter the token

- Strips any quotes from the token if accidentally included

- Saves the token to your `.Renviron` file (located at `~/.Renviron`)

- Updates the token in your current R session

- If a token already exists in `.Renviron`, it will be updated

After updating the token, you may need to restart R/RStudio for the
changes to take full effect, though the token will be available in the
current session immediately.

## See also

[`getData`](getData.md)

## Author

Mathias Harrer <mathias.h.harrer@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
# Update token interactively
updateZenodoAccessToken()

# Update token directly
updateZenodoAccessToken("your_token_here")
} # }
```
