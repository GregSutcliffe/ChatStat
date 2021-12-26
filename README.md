
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ChatStat

<!-- badges: start -->
<!-- badges: end -->

ChatStat is an R package to get data from online chat platforms, in tidy
formats. It currently only supports [Matrix](https://matrix.org), but
more backends are planned.

## Discussion

Come talk in [#rmatrixstats:matrix.org](https://matrix.to/#/#rmatrixstats:matrix.org) to discuss ChatStat or get help using it!

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

## Usage

You will need your `access_token` and your homeserver URL, and
optionally a port if your homeserver is non-standard:

``` r
# Authentication
Sys.setenv('token' = 'syt_foobarbaz',
            'host' = 'matrix.org')
```

To retrieve room data initially, use `get_rooms()` like in the following
example.

``` r
library(ChatStat)

room_ids <- c(
  "!layMvdZSboJeKiyTAL:matrix.org", # #rmeta:matrix.org
  "!FeFZUTDOtgIlOUoYhq:matrix.org", # #rstats:matrix.org
  "!zSgZAViSMVQLIqRgOv:matrix.org"  # #rmatrixstats:matrix.org
)

rooms <- get_rooms(room_ids, since = "2022-01-01")
```

A rooms object that has been created using `get_rooms()` can be updated
later using `update_room()` which will add events that happened in the
mean time.

``` r
updated_rooms <- update_rooms(rooms)
```

See <https://spec.matrix.org/v1.1/client-server-api/> for more details
of the Matrix Client-Server API Spec
