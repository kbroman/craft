## craft

[![R-CMD-check](https://github.com/kbroman/craft/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kbroman/craft/actions/workflows/R-CMD-check.yaml)
[![zenodo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20070587.svg)](https://doi.org/10.5281/zenodo.20070587)

The [miner R package](https://github.com/kbroman/miner) makes it
possible to interact with [Minecraft](https://minecraft.net) from
[R](https://www.r-project.org).

The present package,
[craft](https://github.com/kbroman/craft), includes
additional functions that illustrate and extend the use of miner.


### About this package

This package was created as part of the [ROpenSci unconference](http://unconf17.ropensci.org/)
in May, 2017 by:

* [Brooke Anderson](https://github.com/geanders)
* [Karl Broman](https://github.com/kbroman)
* [Gergely Daróczi](https://github.com/daroczig)
* [Mario Inchiosa](https://github.com/inchiosa)
* [David Smith](https://github.com/revodavid)
* [Ali Zaidi](https://github.com/akzaidi)

### Installation

Install the [miner](https://github.com/kbroman/miner) and
[craft](https://github.com/kbroman/craft) packages using the
[remotes](https://remotes.r-lib.org) package.

First install [remotes](https://remotes.r-lib.org):

```r
install.packages("remotes")
```

Then use `remotes:install_github()` to install [miner](https://github.com/kbroman/miner) and
[craft](https://github.com/kbroman/craft):

```r
library(remotes)
install_github("kbroman/miner")
install_github("kbroman/craft")
```

You'll need a Minecraft server that is running the RaspberryJuice
plugin. See the [Installation and configuration](https://kbroman.org/miner_book/installation-and-configuration.html)
section of [R Programming with Minecraft](https://kbroman.org/miner_book/).

### License

Licensed under the [MIT license](https://cran.r-project.org/web/licenses/MIT). ([More information here](https://en.wikipedia.org/wiki/MIT_License).)
