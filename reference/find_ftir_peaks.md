# Find FTIR Peaks

This function finds peaks in FTIR spectra by identifying minima of the
double derivative, then re-scanning for maxima of peaks missed by the
derivative method. It also uses first derivative zero-crossings to find
broad asymmetric peaks that might be missed by the second derivative.
Peaks detected by different methods within a specified wavenumber window
are merged into a single representative peak location. The spectra is
smoothed by a Savitzky-Golay filter prior to analysis and as such there
are a number of optional tuning parameters that can be provided (the
defaults work well for typical spectra).

This procedure is heuristic and is intended to generate starting peak
locations for fitting, not to certify a unique physical deconvolution.
In crowded regions or spectra with broad shoulders, users may need to
adjust smoothing and window parameters or provide peak centers manually.

Cette fonction permet de trouver des pics dans les spectres IRTF en
identifiant les minima de la double dérivée, puis en recherchant à
nouveau les maxima des pics manqués par la méthode de la dérivée. Elle
utilise également les zéro-crossings de la première dérivée pour trouver
les pics larges et asymétriques qui pourraient être manqués par la
dérivée seconde. Les pics détectés par différentes méthodes dans une
fenêtre spécifique de nombres d'ondes sont fusionnés en un seul pic
représentatif. Le spectre est lissé par un filtre de Savitzky-Golay
avant l'analyse et, à ce titre, un certain nombre de paramètres de
réglage facultatifs peuvent être fournis (les valeurs par défaut
fonctionnent bien pour les spectres typiques).

Cette procédure est heuristique et vise à générer des positions
initiales de pics pour l'ajustement, et non à certifier une
déconvolution physique unique. Dans les régions encombrées ou pour les
spectres présentant de larges épaules, il peut être nécessaire d'ajuster
les paramètres de lissage et de fenêtre ou de fournir les centres des
pics manuellement.

## Usage

``` r
find_ftir_peaks(ftir, call = rlang::caller_env(), ...)
```

## Arguments

- ftir:

  (`data.frame`) A data.frame in long format with a single FTIR spectra
  in columns `sample_id`, `wavenumber`, and `absorbance`. The
  `absorbance` column may be replaced by a `transmittance` column for
  transmittance plots.

  Un data.frame au format long avec un seul spectre IRTF dans les
  colonnes `sample_id`, `wavenumber`, et `absorbance`. La colonne
  `absorbance` peut être remplacée par une colonne `transmittance` pour
  les tracés de transmittance.

- call:

  (\`environment\`) The caller environment for error messages.
  L'environnement de l'appelant pour les messages d'erreur.

- ...:

  Additional optional parameters to pass to peak finding algorithm.

  - `sg_p_norm` The polynomial degree used in smoothing the spectra for
    finding peaks by signal maxima. Default `3`.

  - `sg_p_deriv` The polynomial degree used in smoothing the derivative
    for finding peaks by minima. Default `3`.

  - `sg_n_norm` The number of points used in smoothing the spectra for
    finding peaks by signal maxima. Default `13`.

  - `sg_n_deriv` The number of points used in smoothing the derivative
    for finding peaks by minima. Default `15`.

  - `window_norm` The width of the window (in wavenumbers) to ensure
    that a peak is a true maxima and not just noise. Default `10`. Works
    best on data with consistent resolution, and will round up to the
    next data point.

  - `window_deriv` The width of the window (in wavenumbers) to ensure
    that a derivative minima is a true minima and not just noise.
    Default `5`. Works best on data with consistent resolution, and will
    round up to the next data point.

  - `zero_norm` Spectra have baseline noise removed before searching for
    peaks by setting signal value below the zero threshold to 0. Default
    `1e-2`.

  - `zero_deriv` Derivative have baseline noise removed before searching
    for peaks by setting values below the zero threshold to 0. Default
    `1e-4`.

  - `window_merge` The width of the window (in wavenumbers) within which
    peaks detected by different methods are merged into a single
    representative peak. Default `5`. Works best on data with consistent
    resolution.

    Paramètres optionnels supplémentaires à transmettre à l'algorithme
    de recherche de pics. \#' \* `sg_p_norm` Le degré polynomial utilisé
    pour lisser les spectres afin de trouver les pics par les maxima du
    signal. Valeur par défaut `3`.

  - `sg_p_deriv` Le degré polynomial utilisé dans le lissage de la
    dérivée pour trouver les pics par les minima. Par défaut `3`.

  - `sg_n_norm` Le nombre de points utilisés pour lisser les spectres
    afin de trouver les pics par maxima du signal. Valeur par défaut
    `13`.

  - `sg_n_deriv` Le nombre de points utilisés dans le lissage de la
    dérivée pour trouver les pics par minima. Par défaut `15`.

  - `window_norm` La largeur de la fenêtre (en wavenumbers) pour
    s'assurer qu'un pic est un vrai maxima et pas seulement du bruit.
    Valeur par défaut `10`. Fonctionne mieux sur des données avec une
    résolution cohérente, et arrondit au point de données suivant.

  - `window_deriv` La largeur de la fenêtre (en wavenumbers) pour
    s'assurer qu'un minima de dérivée est un vrai minima et pas
    seulement du bruit. Valeur par défaut `5`. Fonctionne mieux sur des
    données avec une résolution cohérente, et arrondira au point de
    données suivant.

  - `zero_norm` Les spectres sont débarrassés du bruit de base avant de
    rechercher les pics en fixant à 0 la valeur du signal en dessous du
    seuil zéro. Valeur par défaut `1e-2`.

  - `zero_deriv` La dérivée est débarrassée du bruit de base avant la
    recherche des pics en fixant à 0 les valeurs inférieures au seuil
    zéro. Valeur par défaut `1e-4`.

  - `window_merge` La largeur de la fenêtre (en wavenumbers) dans
    laquelle les pics détectés par différentes méthodes sont fusionnés
    en un seul pic représentatif. Par défaut `5`. Fonctionne mieux sur
    des données avec une résolution cohérente.

## Value

A vector of wavenumbers corresponding to peaks found in the provided
FTIR spectra.

Un vecteur de nombres d'ondes correspondant aux pics trouvés dans les
spectres IRTF fournis.

## References

Savitzky, A.; Golay, M.J.E. (1964). "Smoothing and Differentiation of
Data by Simplified Least Squares Procedures". Analytical Chemistry 36.
pp. 1627–1639. doi:10.1021/ac60214a047

## See also

[`signal::sgolayfilt()`](https://rdrr.io/pkg/signal/man/sgolayfilt.html)

## Examples

``` r
if(requireNamespace('signal')){
  # Load the isopropanol sample spectrum from the PlotFTIR package
  ftir_data <- PlotFTIR::sample_spectra[
    PlotFTIR::sample_spectra$sample_id == "isopropanol",
  ]

  # Find peaks using default settings
  peaks_default <- find_ftir_peaks(ftir_data)
  print("Peaks found with default settings:")
  print(peaks_default)

  # Find peaks with adjusted smoothing and window parameters
  # Example: Less smoothing on derivative, wider window for normal peaks
  peaks_adjusted <- find_ftir_peaks(
    ftir_data,
    sg_n_deriv = 11, # Fewer points for derivative smoothing
    window_norm = 15 # Wider window (wavenumbers) for normal peak check
  )
  print("Peaks found with adjusted settings:")
  print(peaks_adjusted)
}
#> Loading required namespace: signal
#> [1] "Peaks found with default settings:"
#>  [1]  659.7388  685.8302  710.0579  816.2870  857.2877  866.6060  877.7881
#>  [8]  950.4711 1107.0193 1129.3834 1159.2021 1194.6118 1216.9758 1250.5218
#> [15] 1308.2956 1338.1143 1375.3877 1408.9337 1464.8438 1628.8466 1647.4833
#> [22] 1766.7581 1902.8060 1975.4891 2070.5362 2077.9908 2195.4020 2297.9038
#> [29] 2314.6768 2407.8602 2521.5440 2590.4998 2657.5919 2722.8203 2884.9595
#> [36] 2933.4149 2970.6883 3248.3749 3268.8753 3296.8303 3328.5127 3335.9674
#> [43] 3341.5584 3365.7861 3388.1501 3417.9688 3457.1058 3608.0630 3636.0181
#> [50] 3663.9731 3680.7461 3716.1558 3742.2472 3775.7932 3798.1573 3824.2486
#> [57] 3887.6134 3906.2501 3932.3414 3969.6148
#> [1] "Peaks found with adjusted settings:"
#>  [1]  663.4661  682.1028  715.6489  728.6945  745.4676  775.2863  816.2870
#>  [8]  888.9701  916.9251  950.4711  985.8809 1045.5183 1107.0193 1129.3834
#> [15] 1159.2021 1190.8844 1230.0215 1308.2956 1313.8866 1341.8416 1366.0693
#> [22] 1379.1150 1408.9337 1464.8438 1507.7082 1632.5740 1647.4833 1671.7110
#> [29] 1684.7567 1722.0301 1766.7581 1902.8060 1975.4891 2077.9908 2195.4020
#> [36] 2314.6768 2407.8602 2517.8167 2571.8631 2594.2271 2657.5919 2700.4563
#> [43] 2720.9566 2728.4113 2780.5940 2799.2307 2814.1401 2834.6404 2884.9595
#> [50] 2933.4149 2970.6883 3212.9652 3227.8746 3250.2386 3268.8753 3289.3756
#> [57] 3302.4213 3321.0580 3335.9674 3341.5584 3347.1494 3362.0587 3386.2864
#> [64] 3414.2415 3423.5598 3432.8781 3442.1965 3451.5148 3460.8332 3479.4699
#> [71] 3494.3792 3509.2886 3520.4706 3537.2436 3554.0166 3574.5170 3589.4263
#> [78] 3609.9267 3637.8817 3663.9731 3682.6098 3697.5191 3716.1558 3740.3835
#> [85] 3775.7932 3792.5662 3824.2486 3846.6126 3859.6583 3876.4314 3887.6134
#> [92] 3902.5227 3932.3414 3954.7054 3965.8875 3988.2515
```
