# Add or Subtract Scalar Value

Add or subtract a constant (scalar) value to each data point in a FTIR
spectra. Shifts the plot up or down on the y axis by the specified
amount without any other change.

Ajoute ou soustrait une valeur constante (scalaire) à chaque point de
données d'un spectre IRTF. Décale le tracé vers le haut ou vers le bas
sur l'axe des y de la valeur spécifiée sans aucune autre modification.

## Usage

``` r
add_scalar_value(ftir, value, sample_ids = NA)

subtract_scalar_value(ftir, value, sample_ids = NA)
```

## Arguments

- ftir:

  A data.frame in long format with columns \`sample_id\`,
  \`wavenumber\`, and \`absorbance\`/\`transmittance\`. Un data.frame au
  format long avec les colonnes \`sample_id\`, \`wavenumber\`, et
  \`absorbance\`/\`transmittance\`.

- value:

  The numeric value to add or subtract.

  Le valeur numerique d'ajute ou soustrait.

- sample_ids:

  A vector of sample IDs to be shifted. All sample IDs must be present
  in the \`ftir\` data.frame. If modifying all spectra, provide NA or
  NULL.

  Un vecteur d'identifiants d'échantillons dont la moyenne doit être
  décalée. Tous les identifiants des échantillons doivent être présents
  dans le data.frame \`ftir\`. Si modifiez tous les spectres, indiquez
  NA ou NULL.

## Value

A data.frame containing the adjusted FTIR spectra.

Un data.frame contenant les spectres IRTF ajustee.

## Examples

``` r
# Add 0.1 to each spectra in biodiesel
add_scalar_value(biodiesel, 0.1)
#> PlotFTIR data:
#>   Spectral range: 700.7395 - 3999.434 cm⁻¹ 
#>   Resolution: variable
#>   Intensity type: absorbance 
#>   Number of samples: 11 
#>   Sample IDs: biodiesel_0, biodiesel_0_25, biodiesel_0_50, biodiesel_1_0, biodiesel_2_5 ...

# Subtract 0.05 from biodiesel_0 and biodiesel_0_25
subtract_scalar_value(biodiesel, 0.05, sample_ids = c("biodiesel_0", "biodiesel_0_25"))
#> PlotFTIR data:
#>   Spectral range: 700.7395 - 3999.434 cm⁻¹ 
#>   Resolution: variable
#>   Intensity type: absorbance 
#>   Number of samples: 11 
#>   Sample IDs: biodiesel_0, biodiesel_0_25, biodiesel_0_50, biodiesel_1_0, biodiesel_2_5 ...
```
