
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PlotFTIR <img src='man/figures/PlotFTIR_logo.png' align="right" width="25%" min-width="120px"/>

([Français](#introduction-et-installation))

<!-- badges: start -->

[![R-CMD-check](https://github.com/NRCan/PlotFTIR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NRCan/PlotFTIR/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/NRCan/PlotFTIR/graph/badge.svg?token=zrndO2tPwv)](https://codecov.io/gh/NRCan/PlotFTIR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

[![CRAN
status](https://www.r-pkg.org/badges/version/PlotFTIR)](https://CRAN.R-project.org/package=PlotFTIR)
[![CRAN
version](https://www.r-pkg.org/badges/version/PlotFTIR)](https://cran.r-project.org/package=PlotFTIR)
[![](https://img.shields.io/badge/doi--green.svg)](https://doi.org/)
<!-- badges: end -->

## Introduction and Installation

The goal of `PlotFTIR` is to easily and quickly kick-start the
production of journal-quality Fourier Transform Infra-Red (FTIR)
spectral plots in R using ggplot2. The produced plots can be published
directly or further modified by ggplot2 functions.

You can install the development version of PlotFTIR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NRCan/PlotFTIR")
```

## Example Plots

This is a basic example which shows you how to plot a prepared set of
FTIR spectra:

``` r
library(PlotFTIR)
biodiesel_plot <- plot_ftir(biodiesel)
biodiesel_plot
```

<img src="man/figures/README-basic_plot_en-1.png" width="66.67%" />

You can also plot spectra in a stacked/offset manner instead of
overlaid:

``` r
# Generate a plot
plot_ftir_stacked(biodiesel)
```

<img src="man/figures/README-stack_plot_en-1.png" width="66.67%" />

Note the default plot and legend titles are in english but can be
automatically changed to french defaults by supplying the `lang = 'fr'`
argument to plot creation functions.

Plots can be manipulated, for example, by zooming in on a range:

``` r
# Zoom to a specified range of 1850 to 1650 cm^-1
zoom_in_on_range(biodiesel_plot, c(1650, 1850))
```

<img src="man/figures/README-biodiesel_zoom_en-1.png" width="66.67%" />

Some FTIR plots have a compressed low-energy portion of the graph which
you might wish to zoom in on. You can achieve this by the following:

``` r
# compress the data with wavenumbers above 2000 (to the left of 2000 on the
# plot) by a factor of 5
compress_low_energy(biodiesel_plot, cutoff = 2000, compression_ratio = 5)
```

<img src="man/figures/README-biodiesel_compress_en-1.png" width="66.67%" />

You can also add marker lines (with labels) at specific wavenumbers on
the plots, controlling their line or text properties as needed.

``` r
biodiesel_marked <- add_wavenumber_marker(biodiesel_plot,
  wavenumber = 1742,
  text = "C=O Stretch",
  label_aesthetics = list("color" = "red")
)
add_wavenumber_marker(biodiesel_marked,
  wavenumber = 2920,
  text = "C-H Stretch",
  line_aesthetics = list("linetype" = "dashed")
)
```

<img src="man/figures/README-biodiesel_labelled_en-1.png" width="66.67%" />

If the need arises to rename samples listed in the legend, this is
possible via `rename_plot_sample_ids()`. Samples must be listed in the
rename vector with the format `"new name" = "old name"`.

``` r
new_names <- c(
  "0.0% Biodiesel" = "biodiesel_0",
  "0.25% Biodiesel" = "biodiesel_0_25",
  "0.50% Biodiesel" = "biodiesel_0_50",
  "1.0% Biodiesel" = "biodiesel_1_0",
  "2.5% Biodiesel" = "biodiesel_2_5",
  "5.0% Biodiesel" = "biodiesel_5_0",
  "7.5% Biodiesel" = "biodiesel_7_5",
  "10.0% Biodiesel" = "biodiesel_10_0",
  "Commercial B0.5" = "biodiesel_B0_5",
  "Commercial B5" = "biodiesel_B5",
  "Unknown Biodiesel" = "diesel_unknown"
)
rename_plot_sample_ids(biodiesel_plot, new_names)
```

<img src="man/figures/README-biodiesel_rename_en-1.png" width="66.67%" />

A helper function for the renaming is provided (see the documentation
for `get_plot_sample_ids()`).

Finally, plot legends are customizable (for basic changes) through a
helper function `move_plot_legend()`.

## Data Sets

The package contains two datasets to provide example spectra for
plotting: \* `biodiesel` is a set of diesels with 0 to 10 % FAMEs (fatty
acid methyl esters; biodiesel) content, plus two known and one unknown
diesel spectra. \* `sample_spectra` is a set of random FTIR spectra
which includes spectra of pure toluene, isopropanol, and heptanes, as
well as white printer paper and a polystyrene film.

An example of the `biodiesel` data set is below:

``` r
head(biodiesel)
#>   wavenumber absorbance   sample_id
#> 1   700.7395   0.072530 biodiesel_0
#> 2   702.6032   0.065398 biodiesel_0
#> 3   704.4669   0.063371 biodiesel_0
#> 4   706.3305   0.059454 biodiesel_0
#> 5   708.1942   0.058133 biodiesel_0
#> 6   710.0579   0.056636 biodiesel_0
```

## Tidy Plot Production

Note that because most functions return a data type similar to what is
provided, tidy-eval is possible (using the base R pipe `|>` or
[`magrittr` pipe
function](https://magrittr.tidyverse.org/reference/pipe) `%>%`).

``` r
library(magrittr)

new_ids <- c(
  "Toluene" = "toluene", "C7 Alkane" = "heptanes", "IPA" = "isopropanol",
  "White Paper" = "paper", "PS Film" = "polystyrene"
)

sample_spectra |>
  absorbance_to_transmittance() |>
  plot_ftir(plot_title = "Example FTIR Spectra") |>
  zoom_in_on_range(zoom_range = c(3800, 800)) |>
  compress_low_energy(compression_ratio = 4) |>
  add_wavenumber_marker(
    wavenumber = 1495,
    text = "C-C Aromatic",
    line_aesthetics = list("linetype" = "dashed"),
    label_aesthetics = list("color" = "#7e0021")
  ) |>
  add_wavenumber_marker(
    wavenumber = 3340,
    text = "O-H Alcohol",
    line_aesthetics = list("linetype" = "dotted"),
    label_aesthetics = list("color" = "#ff420e")
  ) |>
  rename_plot_sample_ids(sample_ids = new_ids) |>
  move_plot_legend(position = "bottom", direction = "horizontal")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
```

<img src="man/figures/README-tidy_en-1.png" width="66.67%" />

## Data Manipulation

FTIR spectral data can be converted between absorbance and
transmittance. Only one type of data can exist in a data.frame and be
plotted. The functions `absorbance_to_transmittance()` and
`transmittance_to_absorbance()` perform these conversions.

``` r
biodiesel_transm <- absorbance_to_transmittance(biodiesel)
head(biodiesel_transm)
#>   wavenumber transmittance   sample_id
#> 1   700.7395      84.61941 biodiesel_0
#> 2   702.6032      86.02051 biodiesel_0
#> 3   704.4669      86.42293 biodiesel_0
#> 4   706.3305      87.20593 biodiesel_0
#> 5   708.1942      87.47159 biodiesel_0
#> 6   710.0579      87.77362 biodiesel_0
```

Functions are provided for adjusting the baseline of spectra, adding or
subtracting scalar values from entire spectra, and averaging spectra,
see: \* `recalculate_baseline()` \* `add_scalar_value()` and
`subtract_scalar_value()` \* `average_spectra()`

## Reading Files

PlotFTIR can read .csv and .asp file types. The .csv file should contain
only one spectra, with columns for `wavenumber` and `absorbance` or
`transmittance.` The .asp files should be according to the file
specifications (not modified by the user).

## Code of Conduct

Please note that the PlotFTIR project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

## Citing This Package

Please cite this package in any journal articles containing images
produced by way of the package. If installed from GitHub or CRAN the
date field will be properly filled with the publishing year.

``` r
citation("PlotFTIR")
#> To cite package 'PlotFTIR' in publications use:
#> 
#>   Bulsink P (????). _PlotFTIR: Plot FTIR Spectra_. R package version
#>   0.1.0.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {PlotFTIR: Plot FTIR Spectra},
#>     author = {Philip Bulsink},
#>     note = {R package version 0.1.0},
#>   }
```

([English](#introduction-and-installation))

## Introduction et installation

Le but de `PlotFTIR` est de lancer facilement et rapidement la
production des tracés de spectres de spectroscopie infrarouge à
transformée de Fourier (IRTF) de qualité de revues scientifiques dans le
system R en utilisant ggplot2. Les tracés produits peuvent être publiés
directement ou modifiés par les fonctions ggplot2.

Vous pouvez installer la version de développement de `PlotFTIR` depuis
[GitHub](https://github.com/) avec:

``` r
# install.packages("devtools")
devtools::install_github("NRCan/PlotFTIR")
```

## Exemples des tracés

Ceci est un example de base qui vous montre comment tracer un ensemble
de spectres IRTF dejà preparé:

``` r
library(PlotFTIR)
plot_ftir(sample_spectra, lang = "fr")
```

<img src="man/figures/README-basic_plot_fr-1.png" width="66.67%" />

Vous pouvez également tracer les spectres de manière empilée/décalée au
lieu de les superposer :

``` r
plot_ftir_stacked(biodiesel, plot_title = "Spectre IRTF empilée", lang = "fr")
```

<img src="man/figures/README-stack_plot_fr-1.png" width="66.67%" />

Notez que les titres par défaut de tracé et du légende sont en anglais,
mais qu’ils peuvent être automatiquement modifiés en français en
fournissant l’argument `lang = 'fr'` aux fonctions de création de
tracés.

Les tracés peuvent être manipulés, par exemple, en zoomant sur une
plage :

``` r
# Générer un tracé
biodiesel_trace <- plot_ftir(biodiesel, lang = "fr")
# Zoom sur une plage spécifiée de 1850 à 1650 cm^-1
zoom_in_on_range(biodiesel_trace, c(1650, 1850))
```

<img src="man/figures/README-biodiesel_zoom_fr-1.png" width="66.67%" />

Certains tracés IRTF ont une partie compressée du graphique à faible
énergie qui peuvent etre agrandie de la manière suivante :

``` r
# compresser les données avec des nombres d'onde supérieurs à 2000 (à gauche de
# 2000 sur le tracé) d'un facteur 5
compress_low_energy(biodiesel_trace, cutoff = 2000, compression_ratio = 5)
```

<img src="man/figures/README-biodiesel_compress_fr-1.png" width="66.67%" />

Vous pouvez également ajouter des lignes de marqueur (avec des
étiquettes) à des numéros d’onde spécifiques sur les tracés, en
contrôlant leurs propriétés de ligne ou de texte selon vos besoins.

``` r
biodiesel_marked <- add_wavenumber_marker(biodiesel_trace,
  wavenumber = 1742,
  text = "C=O étirement",
  label_aesthetics = list("color" = "red")
)
add_wavenumber_marker(biodiesel_trace,
  wavenumber = 2920,
  text = "C-H étirement",
  line_aesthetics = list("linetype" = "dashed")
)
```

<img src="man/figures/README-biodiesel_labelled_fr-1.png" width="66.67%" />

S’il est nécessaire de renommer les échantillons répertoriés dans la
légende, cela est possible via `rename_plot_sample_ids()`. Le vecteur de
renommage doit avoir le format `"nouveau nom" = "ancien nom"`.

``` r
nouveau_noms <- c(
  "0,0% Biodiesel" = "biodiesel_0",
  "0,25% Biodiesel" = "biodiesel_0_25",
  "0,50% Biodiesel" = "biodiesel_0_50",
  "1,0% Biodiesel" = "biodiesel_1_0",
  "2,5% Biodiesel" = "biodiesel_2_5",
  "5,0% Biodiesel" = "biodiesel_5_0",
  "7,5% Biodiesel" = "biodiesel_7_5",
  "10,0% Biodiesel" = "biodiesel_10_0",
  "B0,5 Commercial" = "biodiesel_B0_5",
  "B5 Commercial" = "biodiesel_B5",
  "Biodiesel Inconnu" = "diesel_unknown"
)
rename_plot_sample_ids(biodiesel_trace, nouveau_noms)
```

<img src="man/figures/README-biodiesel_rename_fr-1.png" width="66.67%" />

Une fonction d’assistance pour le changement de nom est fournie (voir la
documentation pour `get_plot_sample_ids()`).

Enfin, les légendes des tracés sont personnalisables (pour les
modifications de base) via une fonction d’assistance
`move_plot_legend()`.

## Production de tracés “tidy”

Notez que comme la plupart des fonctions renvoient un type de données
similaire à celui qui est fourni, l’évaluation des données est possible
(en utilisant [la fonction `magrittr`
tuyau](https://magrittr.tidyverse.org/reference/pipe) `%>%`).

``` r
library(magrittr)

nouveaux_ids <- c(
  "Toluène" = "toluene", "C7 alcane" = "heptanes", "alcool isopropylique" = "isopropanol",
  "papier blanc" = "paper", "film de polystyrène" = "polystyrene"
)

sample_spectra |>
  absorbance_to_transmittance() |>
  plot_ftir(plot_title = "Exemple de spectres IRTF", lang = "fr") |>
  zoom_in_on_range(zoom_range = c(3800, 800)) |>
  compress_low_energy(compression_ratio = 4) |>
  add_wavenumber_marker(
    wavenumber = 1495,
    text = "C-C aromatique",
    line_aesthetics = list("linetype" = "dashed"),
    label_aesthetics = list("color" = "#7e0021")
  ) |>
  add_wavenumber_marker(
    wavenumber = 3340,
    text = "O-H alcool",
    line_aesthetics = list("linetype" = "dotted"),
    label_aesthetics = list("color" = "#ff420e")
  ) |>
  rename_plot_sample_ids(sample_ids = nouveaux_ids) |>
  move_plot_legend(position = "bottom", direction = "horizontal")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
```

<img src="man/figures/README-tidy_fr-1.png" width="66.67%" />

## Production de tracés “tidy”

Notez que, comme la plupart des fonctions renvoient un type de données
similaire à celui fourni, une “tidy-eval” est possible.

## Ensembles des données

Le package contient deux ensembles de données pour fournir des exemples
de spectres à tracer: \* `biodiesel` est un ensemble de diesels avec une
teneur en esters méthyliques d’acides gras (EMAGs) (ou biodiesel) de 0 à
10 %, plus deux spectres de diesel connus et un inconnu. \*
`sample_spectra` est un ensemble de spectres IRTF aléatoires qui
comprennent des spectres de toluène pur, d’isopropanol et d’heptanes,
ainsi que du papier d’imprimante blanc et un film de polystyrène.

Un exemple de l’ensemble de données `biodiesel` est ci-dessous:

``` r
head(biodiesel)
#>   wavenumber absorbance   sample_id
#> 1   700.7395   0.072530 biodiesel_0
#> 2   702.6032   0.065398 biodiesel_0
#> 3   704.4669   0.063371 biodiesel_0
#> 4   706.3305   0.059454 biodiesel_0
#> 5   708.1942   0.058133 biodiesel_0
#> 6   710.0579   0.056636 biodiesel_0
```

## Manipulation de données

Les données spectrales IRTF peuvent être converties entre l’absorbance
et la transmission. Un seul type de données peut exister dans un
data.frame et être tracé. Les fonctions `absorbance_to_transmittance()`
et `transmittance_to_absorbance()` effectuent ces conversions.

``` r
biodiesel_transm <- absorbance_to_transmittance(biodiesel)
head(biodiesel_transm)
#>   wavenumber transmittance   sample_id
#> 1   700.7395      84.61941 biodiesel_0
#> 2   702.6032      86.02051 biodiesel_0
#> 3   704.4669      86.42293 biodiesel_0
#> 4   706.3305      87.20593 biodiesel_0
#> 5   708.1942      87.47159 biodiesel_0
#> 6   710.0579      87.77362 biodiesel_0
```

Des fonctions sont fournies pour ajuster la ligne de base des spectres,
ajouter ou soustraire des valeurs scalaires de spectres entiers, et
calculer la moyenne des spectres, voir : \* `recalculate_baseline()` \*
`add_scalar_value()` et `subtract_scalar_value()` \* `average_spectra()`

## Lecture des fichiers

PlotFTIR peut lire les fichiers de type .csv et .asp. Le fichier .csv ne
doit contenir qu’un seul spectre, avec des colonnes pour le `wavenumber`
et `absorbance` ou `transmittance`. Les fichiers .asp doivent être
conformes aux spécifications du fichier (non modifiées par
l’utilisateur).

## Code de conduite

Veuillez noter que le projet PlotFTIR est publié avec un [Code de
conduite pour le projet](CODE_OF_CONDUCT.md). En contribuant à ce
projet, vous acceptez de respecter ces conditions.

## Citer ce paquet

Veuillez citer ce paquet dans tout article de journal contenant des
images produites à l’aide de ce paquet. Si le paquet est installé à
partir de GitHub ou de CRAN, le texte de la date sera correctement
rempli avec l’année de publication.

``` r
citation("PlotFTIR")
#> To cite package 'PlotFTIR' in publications use:
#> 
#>   Bulsink P (????). _PlotFTIR: Plot FTIR Spectra_. R package version
#>   0.1.0.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {PlotFTIR: Plot FTIR Spectra},
#>     author = {Philip Bulsink},
#>     note = {R package version 0.1.0},
#>   }
```
