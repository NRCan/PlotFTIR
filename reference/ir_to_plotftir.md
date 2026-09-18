# Convert \`ir\` to \`PlotFTIR\` data format

convert data from the \`ir\` package to a structure that will work with
\`PlotFTIR\`.

convertir les données du paquet \`ir\` en une structure qui fonctionnera
avec \`PlotFTIR\`.

## Usage

``` r
ir_to_plotftir(ir_data, what = NA)
```

## Arguments

- ir_data:

  data of class \`ir\` from \`ir\` package

  données de la classe \`ir\` du paquet \`ir\`.

- what:

  which samples to convert to \`PlotFTIR\` format. Defaults to all
  available spectra.

  les échantillons à convertir au format \`PlotFTIR\`. Par défaut, tous
  les spectres disponibles

## Value

a data.frame compatible with \`PlotFTIR\` functions

un data.frame compatible avec les fonctions \`PlotFTIR\`.

## See also

\[ir::ir_get_spectrum()\] for information on how ir passes out data.

## Examples

``` r
if (requireNamespace("ir", quietly = TRUE)) {
  # Convert samples 1 & 4 to PlotFTIR format
  ir_to_plotftir(ir::ir_sample_data, c(1, 4))
}
#> PlotFTIR data:
#>   Spectral range: 650 - 4000 cm⁻¹ 
#>   Resolution: 1 cm⁻¹ 
#>   Intensity type: absorbance 
#>   Number of samples: 2 
#>   Sample IDs: GN.11.389, GN.11.411 
```
