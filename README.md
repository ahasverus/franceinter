
<!-- README.md is generated from README.Rmd. Please edit that file -->

# franceinter <img src="https://raw.githubusercontent.com/ahasverus/mystickers/master/pngs/franceinter.png" height="120px" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->

[![R CMD
Check](https://github.com/ahasverus/franceinter/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahasverus/franceinter/actions/workflows/R-CMD-check.yaml)
[![Website
deployment](https://github.com/ahasverus/franceinter/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/ahasverus/franceinter/actions/workflows/pkgdown.yaml)
[![Update
metadata](https://github.com/ahasverus/franceinter/actions/workflows/update-podcasts.yml/badge.svg)](https://github.com/ahasverus/franceinter/actions/workflows/update-podcasts.yml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The goal of the R package `franceinter` is to retrieve Radio France
(France Inter, France Culture, etc.) podcasts metadata (date, title, mp3
URL, and duration). User can easily download metadata for any podcast.
The name of the podcast must be extracted from the website URL (e.g. Le
Moment Meurice must be written as `le-moment-meurice`).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ahasverus/franceinter")
```

Then you can attach the package `franceinter`:

``` r
library("franceinter")
```

## Usage

``` r
## Create a folder to store results ----

path <- here::here("radiofrance")
dir.create(path, recursive = TRUE)


## Retrieve episodes information ----

tab <- get_metadata("la-chronique-de-waly-dia", path)


## Create a M3U playlist ----

add_m3u(tab, "la-chronique-de-waly-dia", path)
```

The `m3u` file can be open with
[VLC](https://www.videolan.org/vlc/index.en.html) to stream all
episodes. To retrieve new episodes, just re-run the functions
`get_metadata()` and `add_m3u()`.

## Citation

Please cite this package as:

> Casajus N (2022) franceinter: An R package to retrieve Radio France
> podcasts metadata. R package version 1.0.

## Code of Conduct

Please note that the `franceinter` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
