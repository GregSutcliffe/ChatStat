
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
# install.packages("remotes")
remotes::install_github("GregSutcliffe/ChatStat")
```

If you’re new to R, go with this:

``` bash
# install R from your package manager
mkdir chatstats && cd chatstats
Rscript -e 'install.packages("renv")'
Rscript -e 'renv::init()'
Rscript -e 'install.packages("remotes")'
Rscript -e 'remotes::install_github("GregSutcliffe/ChatStat")'
R
```

Which will set up a new `renv` environment with ChatStat installed and
ready to use.

## Making raw Matrix API calls

You will need your `access_token` and your homeserver URL, and
optionally a port if your homeserver is non-standard:

``` r
# Authentication
Sys.setenv('token' = 'syt_foobarbaz',
            'host' = 'matrix.org')
```

You can then get a JSON list of events by providing a roomID that you
access to:

``` r
library(ChatStat)
df <- room_history('!layMvdZSboJeKiyTAL:matrix.org', # rmeta:matrix.org
                   since = '2021-12-01 00:00:00')
length(df)
```

See <https://spec.matrix.org/v1.1/client-server-api/> for more details
of the Matrix Client-Server API Spec
