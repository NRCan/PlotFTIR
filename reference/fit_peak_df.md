# Fitted Peaks Data.Frame

Reformat the \[fit_peaks()\] object to a data.frame of peak
specifications.

Reformater l'objet \[fit_peaks()\] en un data.frame de spécifications de
pics.

## Usage

``` r
fit_peak_df(fitted_peaks)
```

## Arguments

- fitted_peaks:

  An object from \[fit_peaks()\].

  Un objet de \[fit_peaks()\].

## Value

A data.frame of peak properties.

Un data.frame des propriétés des pics.

## Examples

``` r
# Load the isopropanol sample spectrum from the PlotFTIR package
ftir_data <- PlotFTIR::sample_spectra[
  PlotFTIR::sample_spectra$sample_id == "isopropanol",
]

# Choose a subset of the data (reducing run time)
ftir_data <- ftir_data[
  ftir_data$wavenumber < 1500 & ftir_data$wavenumber > 1000,
]
if(requireNamespace('signal')){
  # First, fit the peaks (using the default 'voigt' method)
  fitted_voigt <- fit_peaks(ftir_data, method = "voigt")

  # Now, convert the fitted model object to a data frame
  peak_df_voigt <- fit_peak_df(fitted_voigt)

  print("Peak Data Frame from Voigt Fit:")
  print(peak_df_voigt)
}
#> [1] "Peak Data Frame from Voigt Fit:"
#>      sample_id peak wavenumber     sigma          eta  amplitude   mix_ratio
#> 1  isopropanol    1   1076.832 10.542637 1.763596e-01  0.8287992 0.013355539
#> 2  isopropanol    2   1105.636 11.366414 1.742152e-01 10.7205025 0.172753666
#> 3  isopropanol    3   1129.572  8.451949 3.581103e-01 10.4940375 0.169104334
#> 4  isopropanol    4   1158.515  8.106154 2.207360e-01  8.1748947 0.131732913
#> 5  isopropanol    5   1160.432 14.784173 9.445630e-01  0.9591707 0.015456389
#> 6  isopropanol    6   1234.130 75.722559 5.749176e-11  0.6204128 0.009997534
#> 7  isopropanol    7   1273.855 16.319172 4.374586e-01  1.7666885 0.028468992
#> 8  isopropanol    8   1304.639 13.326750 1.586410e-01  6.6373828 0.106956945
#> 9  isopropanol    9   1338.172 13.656896 5.354268e-01  4.3672592 0.070375436
#> 10 isopropanol   10   1374.722  9.270935 2.921512e-01  7.4929757 0.120744248
#> 11 isopropanol   11   1412.516 16.479990 2.433954e-01  5.1966501 0.083740511
#> 12 isopropanol   12   1464.550 11.319198 6.359799e-01  4.7978114 0.077313494
#>    peak_shape
#> 1       voigt
#> 2       voigt
#> 3       voigt
#> 4       voigt
#> 5       voigt
#> 6       voigt
#> 7       voigt
#> 8       voigt
#> 9       voigt
#> 10      voigt
#> 11      voigt
#> 12      voigt
```
