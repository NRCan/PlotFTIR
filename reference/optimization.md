# Peak Optimization

Perform peak optimization (component location/wavenumber, component
width, proportional area, and/or shape parameters) for all provided
component peaks against an absorbance intensity. Uses expectation
maximization algorithms from Matsumura *et. al.*. The specific function
called results in different peak types and has different input
parameters to optimize:

- `.spect_em_gmm()` optimizes Gauss shaped component peaks with the
  parameters:

  - `sigma` - standard deviation (sigma) of the component peak

- `.spect_em_lmm()` optimizes Lorentz shaped component peaks with the
  parameters:

  - `gam` - width (gamma) of the peak(s). Can be thought of as standard
    deviation.

- `.spect_em_pvmm()` optimizes pseudo-Voigt shaped component peaks (a
  blending of Gauss and Lorentz) with the following parameters:

  - `sigma` - standard deviation (sigma) of the component peak

  - `eta` - mixing of Gauss and Lorentz distribution for the component
    (proportion of Lorentz from 0-1)

- `.spect_em_dsgmm()` optimizes Doniach-Šunjić-Gauss shaped component
  peaks (pseudo-Voigt but can be skew/asymmetrical) with the following
  parameters:

  - `sigma` - standard deviation (sigma) of the component peak

  - `alpha` - proportion asymmetric (0-1) of the component peak

  - `eta` - mixing of Gauss and Lorentz distribution for the component
    (proportion of Lorentz from 0-1)

  Optimisation des pics (emplacement des composants/nombre d'ondes,
  largeur des composants, surface proportionnelle et/ou paramètres de
  forme) pour tous les pics de composants fournis par rapport à une
  intensité d'absorption. Utilise les algorithmes de maximisation de
  l'espérance de Matsumura *et. al.*. La fonction spécifique appelée
  produit différents types de pics et a différents paramètres d'entrée à
  optimiser :

- `.spect_em_gmm()` optimise les pics des composants en forme de Gauss
  avec les paramètres :

  - `sigma` - écart-type (sigma) du pic de la composante

- `.spect_em_lmm()` optimise les pics des composants en forme de Lorentz
  avec les paramètres :

  - `gam` - largeur (gamma) du (des) pic(s). On peut l'assimiler à un
    écart-type.

- `.spect_em_pvmm()` optimise les pics des composantes en forme de
  pseudo-Voigt (un mélange de Gauss et de Lorentz) avec les paramètres
  suivants :

  - `sigma` - écart-type (sigma) du pic de la composante

  - `eta` - mélange des distributions de Gauss et de Lorentz pour le
    composant (proportion de Lorentz de 0 à 1)

- `.spect_em_dsgmm()` optimise les pics des composantes en forme de
  Doniach-Šunjić-Gauss (pseudo-Voigt mais peut être
  asymétrique/asymétrique) avec les paramètres suivants :

  - `sigma` - écart-type (sigma) du pic de la composante.

  - `alpha` - proportion asymétrique (0-1) du pic de la composante

  - `eta` - mélange des distributions de Gauss et de Lorentz pour le
    composant (proportion de Lorentz de 0 à 1)

## Usage

``` r
.spect_em_dsgmm(
  x,
  y,
  mu,
  sigma = rep(10, length(mu)),
  alpha = rep(0.5, length(mu)),
  eta = rep(0.5, length(mu)),
  mix_ratio = rep(1/length(mu), length(mu)),
  conv_cri = 0.01,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
)

.spect_em_gmm(
  x,
  y,
  mu,
  sigma = rep(10, length(mu)),
  mix_ratio = rep(1/length(mu), length(mu)),
  conv_cri = 0.01,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
)

.spect_em_lmm(
  x,
  y,
  mu,
  gam = rep(10, length(mu)),
  mix_ratio = rep(1/length(mu), length(mu)),
  conv_cri = 0.01,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
)

.spect_em_pvmm(
  x,
  y,
  mu,
  sigma = rep(10, length(mu)),
  eta = rep(0.5, length(mu)),
  mix_ratio = rep(1/length(mu), length(mu)),
  conv_cri = 0.01,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
)
```

## Arguments

- x:

  A numeric vector of x values (wavenumbers) of the spectra against
  which the components are being optimized.

  Un tableau numérique des valeurs x (nombres d'ondes) des spectres par
  rapport auxquels les composants sont optimisés.

- y:

  (`numeric`) A numeric vector of absorbance values (of same length as
  `x`) of the spectra against which the components are being optimized.

  Un tableau numérique des valeurs d'absorption (de la même longueur que
  `x`) des spectres par rapport auxquels les composants sont optimisés.

- mu:

  A numeric vector of component peak centers.

  Un tableau numérique des centres de pics des composants.

- sigma:

  A numeric vector of component peak standard deviation (sigma) values.
  Must be the same length as `mu`.

  Un tableau numérique des valeurs d'écart-type (sigma) des pics des
  composants. Doit être de la même longueur que `mu`.

- alpha:

  (`numeric`) A numeric vector of component proportion asymmetric
  (alpha) values. Must all be between 0 and 1. Must be the same length
  as `mu`.

  Un tableau numérique des valeurs de proportion asymétrique (alpha) des
  composants. Doit être compris entre 0 et 1. Doit être de la même
  longueur que `mu`.

- eta:

  A numeric vector of component mixing of Gauss and Lorentz
  characteristics. Must all be between 0 and 1. Must be the same length
  as `mu`.

  Un tableau numérique du mélange des composantes des caractéristiques
  de Gauss et de Lorentz. Doit être compris entre 0 et 1. Doit être de
  la même longueur que `mu`.

- mix_ratio:

  A numeric vector of mix ratios (e.g. proportionate area under the
  curve) for each component peak. Must be the same length as `mu`.

  Un tableau numérique des rapports de mélange (par exemple, l'aire
  proportionnelle sous la courbe) pour chaque pic de composant. Doit
  être de la même longueur que `mu`.

- conv_cri:

  The value (in absolute units) to which convergence is measured. If
  sequential optimization cycles differ by less than this convergence
  amount, then the resulting parameters are returned. Smaller values of
  `conv_cri` result in tighter convergence at higher computational cost.

  La valeur (en unités absolues) à laquelle la convergence est est
  mesurée. Si les cycles d'optimisation séquentiels diffèrent de moins
  de cette valeur de convergence, les paramètres résultants sont
  renvoyés. Des valeurs plus petites de `conv_cri` résultent en une
  convergence plus serrée à un coût de calcul plus élevé.

- maxit:

  The maximum number of optimization cycles permitted. If `conv_cri` has
  not been met by the completion of `maxit` cycles an error is returned.

  Le nombre maximum de cycles d'optimisation autorisés. Si `conv_cri`
  n'a pas été atteint à la fin des cycles `maxit`, une erreur est
  renvoyée.

- fixed_mu:

  Whether to allow peak centers to be moved in optimization (TRUE) or
  fixed to the provided values (FALSE).

  Permet de déplacer les centres de pic lors de l'optimisation (VRAI) ou
  de les fixer aux valeurs fournies (FAUX).

- verbose:

  Whether to output a status message at the conclusion of each
  optimization cycle.

  Indique si un message d'état doit être émis à la fin de chaque cycle
  d'optimisation.

- gam:

  A numeric vector of component peak widths (gamma) values. Must be the
  same length as `mu`.

  Un tableau numérique des valeurs de largeur des pics des composants
  (gamma). Doit être de la même longueur que `mu`.

## Value

A named list object, with values corresponding to the optimal peak
location, width, area, and shape parameters (function dependent), as
well as the optimization record of each optimized parameter, count of
optimization iterations, convergence status, and the type of
optimization.

Un objet liste nommé, avec des valeurs correspondant aux paramètres
optimaux d'emplacement, de largeur, de surface et de forme du pic
(dépendant de la fonction), ainsi que l'enregistrement d'optimisation de
chaque paramètre optimisé, le nombre d'itérations d'optimisation, l'état
de convergence et le type d'optimisation.

## References

- Matsumura, T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2019)
  "Spectrum adapted expectation-maximization algorithm for
  high-throughput peak shift analysis". Science and technology of
  advanced materials, 20(1), pp 733-745.
  doi:10.1080/14686996.2019.1620123

- Matsumura, T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2021)
  "Spectrum adapted expectation-conditional maximization algorithm for
  extending high–throughput peak separation method in XPS analysis".
  Science and Technology of Advanced Materials: Methods, 1(1), pp 45-55.
  doi:10.1080/27660400.2021.1899449

## See also

[`fit_peaks()`](https://nrcan.github.io/PlotFTIR/reference/fit_peaks.md)
