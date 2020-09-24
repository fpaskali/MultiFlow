# MultiFlowExtended
A fork of MultiFlow introducing new thresholding methods

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Installation

The package requires Bioconductor package EBImage, which should be installed
first via

```{r, eval = FALSE}
## Install package BiocManager
if(!require(BiocManager)) install.packages("BiocManager")
## Use BiocManager to install EBImage
BiocManager::install("EBImage", update = FALSE)
```

Next, one can install package MultiFlow, where all remaining dependencies will
be installed automatically.

```{r, eval = FALSE}
## Install package remotes
if(!require(remotes)) install.packages("remotes")
## Install package MultiFlow
remotes::install_github("fpaskali/MultiFlow")
```

## Start App

```{r}
MultiFlowExt::run_app()
```


## Open User's Guide

```{r}
vignette("MultiFlowExt")
```

See also: [MultiFlow User's Guide](https://stamats.github.io/MultiFlow/MultiFlow.html).


## YouTube Playlist

[MultiFlow YouTube Playlist](https://www.youtube.com/playlist?list=PLRgOZXM8LZ0gv2OJts1c62n0gsXO9VrAN).


## Description
Image analysis (cropping, segmetation, thresholding, feature engineering) 
of images taken from lateral flow assays, computation of linear calibration 
models, automatic report generation via rmarkdown by means of a shiny app.

![MultiFlow Shiny App](MultiFlowShinyApp.png)
