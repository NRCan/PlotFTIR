# Normalize FTIR spectra

Normalizing spectra restricts the range of absorbance values from 0 to 1
inclusive. This function shifts and scales spectra to achieve this
absorbance range. It can be applied to a whole spectral set or just one
sample, and across the entire spectra or by normalizing within a
wavenumber region. This function does not operate on transmittance data,
it will return an error.

La normalisation des spectres restreint la gamme des valeurs
d'absorbance de 0 à 1 inclus. Cette fonction décale et met à l'échelle
les spectres pour atteindre cette gamme d'absorbance. Elle peut être
appliquée à un ensemble de spectres ou à un seul échantillon, et sur
l'ensemble des spectres ou en normalisant dans une région de nombre
d'ondes. Cette fonction ne fonctionne pas sur les données de
transmittance, elle renverra une erreur.

## Usage

``` r
normalize_spectra(ftir, sample_ids = NA, wavenumber_range = NA)
```

## Arguments

- ftir:

  A data.frame in long format with columns \`sample_id\`,
  \`wavenumber\`, and \`absorbance\`/\`transmittance\`. Un data.frame au
  format long avec les colonnes \`sample_id\`, \`wavenumber\`, et
  \`absorbance\`/\`transmittance\`.

- sample_ids:

  A vector of sample IDs to be adjusted. All sample IDs must be present
  in the `ftir` data.frame. If adjusting all spectra, provide NA or
  NULL. Unlisted `sample_id` from `ftir` will be left alone.

  Un vecteur d'ID d'échantillons à ajuster Tous les ID d'échantillons
  doivent être présents dans la base de données `ftir` data.frame. Si
  l'ajustement concerne tous les spectres, fournir NA ou NULL. Les
  `sample_id` non listés de `ftir` seront laissés seuls.

- wavenumber_range:

  If specifying a single point wavenumber; a single numeric value. If
  specifying a wavenumber range, then a vector of two numeric values.

  Si l'on spécifie un nombre d'ondes ponctuel, une seule valeur
  numérique. Si l'on spécifie un nombre d'ondes, alors un vecteur de
  deux valeurs numériques.

## Value

A data.frame containing the adjusted FTIR spectra.

Un data.frame contenant les spectres IRTF ajustee.

## Examples

``` r
# Normalize all samples in `biodiesel`
normalize_spectra(biodiesel)
#> PlotFTIR data:
#>   Spectral range: 700.7395 - 3999.434 cm⁻¹ 
#>   Resolution: variable
#>   Intensity type: normalized absorbance 
#>   Number of samples: 11 
#>   Sample IDs: biodiesel_0, biodiesel_0_25, biodiesel_0_50, biodiesel_1_0, biodiesel_2_5 ...

# Normalize just `paper` and `isopropanol` spectra from 4000 to 3100 cm^-1^
normalize_spectra(sample_spectra,
  sample_ids = c("paper", "isopropanol"),
  wavenumber_range = c(4000, 3100)
)
#> PlotFTIR data:
#>   Spectral range: 650.4205 - 3999.434 cm⁻¹ 
#>   Resolution: variable
#>   Intensity type: normalized absorbance 
#>   Number of samples: 5 
#>   Sample IDs: toluene, heptanes, isopropanol, paper, polystyrene 
```
