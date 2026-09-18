# Intensity Type

Determines if the provided data has intensity type of absorbance or
transmittance.

## Usage

``` r
intensity_type(ftir)
```

## Arguments

- ftir:

  A data.frame in long format with columns \`sample_id\`,
  \`wavenumber\`, and \`absorbance\`/\`transmittance\`. Un data.frame au
  format long avec les colonnes \`sample_id\`, \`wavenumber\`, et
  \`absorbance\`/\`transmittance\`.

## Value

a character value 'absorbance' or 'transmittance'
