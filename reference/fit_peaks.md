# Fit Peaks

Once peaks are found by
[`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md),
they can be fitted by adjusting intensity (area), standard deviation
(width), and shape parameters (gam, eta, and/or alpha). This can be done
by Expectation-Maximization methods, implemented here by the `EMpeaksR`
package's technique. Note that the spectra provided is shifted to
baseline to reduce the work of the peak fitter in producing background
noise.

Automatic peak discovery in
[`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md)
is a heuristic initialization step based on smoothing, thresholding, and
merging nearby candidate peaks. It is useful for proposing starting
values, but it is not a validated deconvolution standard. In crowded
regions, shoulder peaks, or broad overlapping bands, users should
inspect residuals carefully and may need to supply `peaklist` directly
or tune the peak-finding arguments.

Une fois les pics trouvés par
[`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md),
ils peuvent être ajustés en ajustant l'intensité (surface), l'écart-type
(largeur) et les paramètres de forme (gam, eta, et/ou alpha). Ceci peut
être fait par des méthodes d'espérance-maximisation, implémentées ici
par la technique du paquet `EMpeaksR`. Notez que le spectre fourni est
décalé par rapport à la ligne de base afin de réduire le travail de
l'ajusteur de pics en produisant un bruit de fond.

La recherche automatique des pics dans
[`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md)
est une étape d'initialisation heuristique basée sur le lissage, le
seuillage et la fusion de pics candidats voisins. Elle est utile pour
proposer des valeurs de départ, mais ne constitue pas une méthode de
déconvolution validée. Dans les régions encombrées, pour les pics en
épaule ou pour les bandes larges qui se chevauchent, il faut examiner
les résidus avec soin et il peut être nécessaire de fournir `peaklist`
directement ou d'ajuster les arguments de recherche de pics.

## Usage

``` r
fit_peaks(
  ftir,
  peaklist = NA,
  method = "voigt",
  fixed_peaks = FALSE,
  sigma = NULL,
  gam = NULL,
  mix_ratio = NULL,
  eta = NULL,
  alpha = NULL,
  conv_cri = 0.01,
  maxit = 1000,
  call = rlang::caller_env(),
  ...
)
```

## Arguments

- ftir:

  A data.frame in long format with a single FTIR spectra in columns
  `sample_id`, `wavenumber`, and `absorbance`.

  Un data.frame au format long avec un seul spectre IRTF dans les
  colonnes `sample_id`, `wavenumber`, et `absorbance`.

- peaklist:

  (`numeric`) The locations of peaks from `[find_ftir_peaks()]`. If none
  provided, will search for peaks using the default parameters of that
  function. Note that you could provide a common list of peaks for
  fitting multiple different spectra to compare results between samples.

  Les emplacements des pics de `[find_ftir_peaks()]`. Si aucune valeur
  n'est fournie, les pics seront recherchés en utilisant les paramètres
  par défaut de cette fonction. Notez que vous pouvez fournir une liste
  commune de pics pour l'ajustement de plusieurs spectres différents
  afin de comparer les résultats entre les échantillons.

- method:

  (`character`) The peak style / fitting method. Theoretically FTIR
  peaks are Lorentz shaped, but with Gaussian broadening the
  pseudo-Voigt shape matches best. Some success is seen using
  Doniach-Šunjić-Gauss peak shapes since these can adopt undetected
  shoulder peaks in an asymmetric measure for each peak. Options are:

  - `voigt` (default): Fit Voigt shaped peaks
    [`EMpeaksR::spect_em_pvmm()`](https://rdrr.io/pkg/EMpeaksR/man/PVMM.html)

  - `gauss` Fit Gauss shaped peaks
    [`EMpeaksR::spect_em_gmm()`](https://rdrr.io/pkg/EMpeaksR/man/GMM.html)

  - `lorentz` Fit Lorentz shaped peaks
    [`EMpeaksR::spect_em_lmm()`](https://rdrr.io/pkg/EMpeaksR/man/LMM.html)

  - `dsg` Fit Doniach-Šunjić-Gauss shaped peaks
    [`EMpeaksR::spect_em_dsgmm()`](https://rdrr.io/pkg/EMpeaksR/man/DSGMM.html)

    Le style des pics / la méthode d'ajustement. En théorie, les pics
    IRTF ont une forme de Lorentz, mais avec un élargissement Gaussien,
    c'est la forme pseudo-Voigt qui convient le mieux. Les formes de
    pics de Doniach-Šunjić-Gauss donnent de bons résultats, car elles
    permettent d'adopter des pics d'épaulement non détectés dans le
    cadre d'une mesure asymétrique pour chaque pic. Les options sont les
    suivantes :

  - `voigt` (par défaut) : Ajuster les pics en forme de Voigt
    [`EMpeaksR::spect_em_pvmm()`](https://rdrr.io/pkg/EMpeaksR/man/PVMM.html)

  - `gauss` Ajuster les pics en forme de Gauss
    [`EMpeaksR::spect_em_gmm()`](https://rdrr.io/pkg/EMpeaksR/man/GMM.html)

  - `lorentz` Ajuster les pics en forme de Lorentz
    [`EMpeaksR::spect_em_lmm()`](https://rdrr.io/pkg/EMpeaksR/man/LMM.html)

  - `dsg` Ajuster les pics en forme de Doniach-Šunjić-Gauss
    [`EMpeaksR::spect_em_dsgmm()`](https://rdrr.io/pkg/EMpeaksR/man/DSGMM.html)

- fixed_peaks:

  (`logical`) Boolean, whether to fix the peak locations to the provided
  values or allow the optimizer to move peaks as needed.

  Booléen, pour savoir s'il faut fixer l'emplacement des pics aux
  valeurs fournies ou permettre à l'optimiseur de déplacer les pics
  selon les besoins.

- sigma:

  (`numeric`) Optional starting standard deviation values for Gauss,
  Voigt, and Doniach-Šunjić-Gauss fits. When supplied, this should match
  the number of peaks being fitted. Defaults to `rep(10, n_peaks)`.

  Valeurs initiales optionnelles d'écart-type pour les ajustements
  Gauss, Voigt et Doniach-Šunjić-Gauss. Lorsqu'elles sont fournies,
  elles doivent correspondre au nombre de pics ajustés. Par défaut
  `rep(10, n_peaks)`.

- gam:

  (`numeric`) Optional starting gamma width values for Lorentz fits.
  When supplied, this should match the number of peaks being fitted.
  Defaults to `rep(10, n_peaks)`.

  Valeurs initiales optionnelles de largeur gamma pour les ajustements
  Lorentz. Lorsqu'elles sont fournies, elles doivent correspondre au
  nombre de pics ajustés. Par défaut `rep(10, n_peaks)`.

- mix_ratio:

  (`numeric`) Optional starting component mixing ratios. When supplied,
  this should match the number of peaks being fitted. Defaults to
  `rep(1 / n_peaks, n_peaks)`.

  Rapports de mélange initiaux optionnels des composantes. Lorsqu'ils
  sont fournis, ils doivent correspondre au nombre de pics ajustés. Par
  défaut `rep(1 / n_peaks, n_peaks)`.

- eta:

  (`numeric`) Optional starting Gauss/Lorentz mixing values for Voigt
  and Doniach-Šunjić-Gauss fits. When supplied, this should match the
  number of peaks being fitted. Defaults to `rep(0.5, n_peaks)`.

  Valeurs initiales optionnelles de mélange Gauss/Lorentz pour les
  ajustements Voigt et Doniach-Šunjić-Gauss. Lorsqu'elles sont fournies,
  elles doivent correspondre au nombre de pics ajustés. Par défaut
  `rep(0.5, n_peaks)`.

- alpha:

  (`numeric`) Optional starting asymmetry values for
  Doniach-Šunjić-Gauss fits. When supplied, this should match the number
  of peaks being fitted. Defaults to `rep(1e-4, n_peaks)`.

  Valeurs initiales optionnelles d'asymétrie pour les ajustements
  Doniach-Šunjić-Gauss. Lorsqu'elles sont fournies, elles doivent
  correspondre au nombre de pics ajustés. Par défaut
  `rep(1e-4, n_peaks)`.

- conv_cri:

  (`numeric`) Optional convergence threshold passed to the underlying
  `EMpeaksR` optimizer. Smaller values request tighter convergence at
  higher computational cost. Defaults to `1e-2`.

  Seuil de convergence optionnel transmis à l'optimiseur `EMpeaksR`
  sous-jacent. Des valeurs plus petites demandent une convergence plus
  serrée à un coût de calcul plus élevé. Par défaut `1e-2`.

- maxit:

  (`numeric`) Optional maximum number of optimization iterations passed
  to the underlying `EMpeaksR` optimizer. Defaults to `1000`.

  Nombre maximal optionnel d'itérations d'optimisation transmis à
  l'optimiseur `EMpeaksR` sous-jacent. Par défaut `1000`.

- call:

  (\`environment\`) The caller environment for error messages.
  L'environnement de l'appelant pour les messages d'erreur.

- ...:

  Additional parameters passed to
  [`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md)
  only when `peaklist` is not supplied.

  Paramètres supplémentaires transmis à
  [`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md)
  seulement lorsque `peaklist` n'est pas fourni.

## Value

An `EMpeaksR` style fitted model. See the documentation for each peak
shape.

Un modèle ajusté de type `EMpeaksR`. Voir la documentation pour chaque
forme de pic.

## References

Matsumura, T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2019)
"Spectrum adapted expectation-maximization algorithm for high-throughput
peak shift analysis". Science and technology of advanced materials,
20(1), pp 733-745. doi:10.1080/14686996.2019.1620123 Matsumura, T.,
Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2021) "Spectrum adapted
expectation-conditional maximization algorithm for extending
high–throughput peak separation method in XPS analysis". Science and
Technology of Advanced Materials: Methods, 1(1), pp 45-55.
doi:10.1080/27660400.2021.1899449

## See also

[`spect_em_gmm()`](https://rdrr.io/pkg/EMpeaksR/man/GMM.html),
[`spect_em_lmm()`](https://rdrr.io/pkg/EMpeaksR/man/LMM.html),
[`spect_em_pvmm()`](https://rdrr.io/pkg/EMpeaksR/man/PVMM.html),
[`spect_em_dsgmm()`](https://rdrr.io/pkg/EMpeaksR/man/DSGMM.html)

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

# Example 1: Fit peaks using the default 'voigt' method
if(requireNamespace('signal')){
  # Peaks will be found automatically using find_ftir_peaks defaults
  fitted_voigt_default <- fit_peaks(ftir_data)
  print("Fitted Voigt Peaks (Default):")

  # Show key results like final parameters and convergence status
  print(fit_peak_df(fitted_voigt_default))
  print(paste("Convergence:", fitted_voigt_default$convergence))
}
#> [1] "Fitted Voigt Peaks (Default):"
#>      sample_id peak wavenumber     sigma          eta  amplitude   mix_ratio
#> 1  isopropanol    1   1076.832 10.542634 1.763596e-01  0.8287992 0.013355539
#> 2  isopropanol    2   1105.636 11.366413 1.742152e-01 10.7205025 0.172753665
#> 3  isopropanol    3   1129.572  8.451948 3.581104e-01 10.4940375 0.169104335
#> 4  isopropanol    4   1158.515  8.106154 2.207359e-01  8.1748946 0.131732912
#> 5  isopropanol    5   1160.432 14.784172 9.445631e-01  0.9591707 0.015456388
#> 6  isopropanol    6   1234.130 75.722565 5.749176e-11  0.6204128 0.009997533
#> 7  isopropanol    7   1273.855 16.319168 4.374589e-01  1.7666884 0.028468992
#> 8  isopropanol    8   1304.639 13.326749 1.586411e-01  6.6373827 0.106956945
#> 9  isopropanol    9   1338.172 13.656896 5.354270e-01  4.3672594 0.070375439
#> 10 isopropanol   10   1374.722  9.270935 2.921512e-01  7.4929757 0.120744249
#> 11 isopropanol   11   1412.516 16.479991 2.433953e-01  5.1966500 0.083740509
#> 12 isopropanol   12   1464.550 11.319198 6.359799e-01  4.7978113 0.077313492
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
#> [1] "Convergence: converged"

if (FALSE) { # \dontrun{
# Example 2: Fit peaks using the 'gauss' method
fitted_gauss <- fit_peaks(ftir_data, method = "gauss")
print("Fitted Gaussian Peaks:")
print(fit_peak_df(fitted_gauss))

# Example 3: Provide a pre-defined list of peaks
# First, find some peaks (maybe with custom settings)
initial_peaks <- find_ftir_peaks(ftir_data, window_norm = 20)
print("Initial peaks found:")
print(initial_peaks)
# Now fit using this specific list
fitted_voigt_custom_peaks <- fit_peaks(ftir_data, peaklist = initial_peaks)
print("Fitted Voigt Peaks (Custom Initial List):")
print(fit_peak_df(fitted_voigt_custom_peaks))

# Example 4: Fit peaks but keep their locations fixed
# Use a smaller subset of peaks for demonstration
fixed_peak_locations <- c(1130, 1375, 1460)
fitted_voigt_fixed <- fit_peaks(
  ftir_data,
  peaklist = fixed_peak_locations,
  fixed_peaks = TRUE
)
print("Fitted Voigt Peaks (Fixed Locations):")
print(fit_peak_df(fitted_voigt_fixed))

# Example 5: Pass explicit fitting control parameters
# Note: This might take longer or behave differently
selected_peaks <- c(1130, 1375, 1460)
fitted_voigt_tight_conv <- fit_peaks(
  ftir_data,
  peaklist = selected_peaks,
  conv_cri = 1e-4,
  maxit = 2000,
  sigma = rep(8, length(selected_peaks))
)
print("Fitted Voigt Peaks (Tighter Convergence):")
print(paste("Iterations:", fitted_voigt_tight_conv$it))
print(paste("Convergence:", fitted_voigt_tight_conv$convergence))
} # }
```
