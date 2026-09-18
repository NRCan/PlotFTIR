# \`ChemoSec\` to \`PlotFTIR\` conversions

Converts \`ChemoSpec\` data to that ready to use by \`PlotFTIR\`.

Convertit les données \`ChemoSpec\` en données prêtes à être utilisées
par \`PlotFTIR\`.

## Usage

``` r
chemospec_to_plotftir(csdata)
```

## Arguments

- csdata:

  \`ChemoSpec\` data to convert to \`PlotFTIR.\` Données \`ChemoSpec\` a
  convertir à \`PlotFTIR.\`

## Value

a data.frame compatible with \`PlotFTIR\` functions

un data.frame compatible avec les fonctions \`PlotFTIR\`.

## See also

\[ChemoSpec::files2SpectraObject()\] for import requirements, and
\[chemospec_to_plotftir()\] for converting to \`PlotFTIR\` format.

\[ChemoSpec::files2SpectraObject()\] pour les conditions d'importation,
et \[chemospec_to_plotftir()\] pour la conversion au format
\`PlotFTIR\`.

## Examples

``` r
if (requireNamespace("ChemoSpec", quietly = TRUE)) {
  # convert `chemospec` to PlotFTIR data
  data("SrE.IR", package = "ChemoSpec", envir = environment())
  chemospec_to_plotftir(SrE.IR)
}
#> Registered S3 method overwritten by 'patchwork':
#>   method   from    
#>   -.ggplot PlotFTIR
#> PlotFTIR data:
#>   Spectral range: 399.2123 - 3999.837 cm⁻¹ 
#>   Resolution: variable
#>   Intensity type: absorbance 
#>   Number of samples: 16 
#>   Sample IDs: CVS_adSrE, ET_pSrE, GNC_adSrE, LF_adSrE, MDB_pSrE ...
```
