
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PlotFTIR

<!-- badges: start -->

[![R-CMD-check](https://github.com/pbulsink/PlotFTIR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pbulsink/PlotFTIR/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/pbulsink/PlotFTIR/graph/badge.svg?token=zrndO2tPwv)](https://codecov.io/gh/pbulsink/PlotFTIR)
<!-- badges: end -->

([Français](#introduction-et-installation))

## Introduction and Installation

The goal of `PlotFTIR` is to easily and quickly kick-start the
production of journal-quality FTIR spectral plots in R using ggplot2.
The produced plots can be published directly or further modified by
ggplot2 functions.

You can install the development version of PlotFTIR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pbulsink/PlotFTIR")
```

## Example

This is a basic example which shows you how to plot a prepared set of
FTIR spectra:

``` r
library(PlotFTIR)
plot_ftir(sample_spectra)
```

<img src="man/figures/README-basic_plot_en-1.png" width="100%" />

We can also plot spectra in a stacked/offset manner instead of overlaid:

``` r
plot_ftir_stacked(biodiesel)
```

<img src="man/figures/README-stack_plot_en-1.png" width="100%" />

## Data Sets

The package contains two datasets to provide example spectra for
plotting: \* `biodiesel` is a set of diesels with 0 to 10 % FAMES
(biodiesel) content, plus two known and one unknown diesel spectra. \*
`sample_spectra` is a set of random FTIR spectra which includes spectra
of pure toluene, isopropanol, and heptanes, as well as white printer
paper and a polystyrene film.

## Code of Conduct

Please note that the PlotFTIR project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

([English](#introduction-and-installation))

## Introduction et installation

L’objectif de PlotFTIR est de démarrer facilement et rapidement la
production de tracés spectraux FTIR de qualité journal dans R à l’aide
de ggplot2. Les tracés produits peuvent être publiés directement ou
modifiés davantage par les fonctions ggplot2.

Vous pouvez installer la version de développement de PlotFTIR depuis
[GitHub](https://github.com/) avec :

``` r
# install.packages("devtools")
devtools::install_github("pbulsink/PlotFTIR")
```

## Exemple

Ceci est un example de base qui vous montre comment tracer un ensemble
prépar de spectres FTIR:

``` r
library(PlotFTIR)
plot_ftir(sample_spectra)
```

<img src="man/figures/README-basic_plot_fr-1.png" width="100%" />

Nous pouvons également tracer les spectres de manière empilée/décalée au
lieu de les superposer :

``` r
plot_ftir_stacked(biodiesel)
```

<img src="man/figures/README-stack_plot_fr-1.png" width="100%" />

## Ensembles de données

Le package contient deux ensembles de données pour fournir des exemples
de spectres à tracer : \* `biodiesel` est un ensemble de diesels avec
une teneur en FAMES (biodiesel) de 0 à 10 %, plus deux spectres de
diesel connus et un inconnu. \* `sample_spectra` est un ensemble de
spectres FTIR aléatoires qui comprennent des spectres de toluène pur,
d’isopropanol et d’heptanes, ainsi que du papier d’imprimante blanc et
un film de polystyrène.

## Code de conduite

Veuillez noter que le projet PlotFTIR est publié avec un [Code de
conduite pour le projet](CODE_OF_CONDUCT.md). En contribuant à ce
projet, vous acceptez d’en respecter les termes.
