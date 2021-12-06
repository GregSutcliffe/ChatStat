
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ChatStat

<!-- badges: start -->
<!-- badges: end -->

ChatStat is an R package to get data from online chat platforms, in tidy
formats. It currently only supports [Matrix](https://matrix.org), but
more backends are planned.

## Installation

You can install the development version of ChatStat from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GregSutcliffe/ChatStat")
```

## Making raw Matrix API calls

So far we only have the initial `/sync` from the Matrix Client-Server
API. You will need your `access_token` and your homeserver URL, and
optionally a port if your homeserver is non-standard:

``` r
Sys.setenv('token' = 'syt_foobarbaz',
            'host' = 'matrix.org')
```

``` r
library(ChatStat)
initial_sync <- ChatStat::sync()
length(r$rooms$join)
```

See <https://spec.matrix.org/v1.1/client-server-api/> for more details
of the Matrix Client-Server API Spec
