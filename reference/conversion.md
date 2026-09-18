# Convert Between Absorbance and Transmittance

These functions allow for the convenient conversion between
%Transmittance and Absorbance units for the Y axis.

Converting between %Transmittance and absorbance units for the Y axis is
not a simple flipping of axis or inversion. Instead, the two are related
by the following formulas:

\$\$ A=-log\_{10}(\tfrac{\\T}{100}) \$\$ and \$\$ \\T=10^{-A}\cdot 100
\$\$.

Ces fonctions permettent une conversion pratique entre les unités
%Transmittance et Absorbance pour l'axe Y. La conversion entre les
unités %Transmittance et Absorbance pour l'axe Y n'est pas un simple
retournement d'axe ou une inversion. Au lieu de cela, les deux sont liés
par les formules suivantes :

\$\$ A=-log\_{10}(\tfrac{\\T}{100}) \$\$ and \$\$ \\T=10^{-A}\cdot 100
\$\$

## Usage

``` r
absorbance_to_transmittance(ftir)

transmittance_to_absorbance(ftir)
```

## Arguments

- ftir:

  A data.frame in long format with columns \`sample_id\`,
  \`wavenumber\`, and \`absorbance\`/\`transmittance\`. Un data.frame au
  format long avec les colonnes \`sample_id\`, \`wavenumber\`, et
  \`absorbance\`/\`transmittance\`.

## Value

a data.frame of FTIR spectral data with conversion between absorbance or
transmittance as requested. Note the original data column is removed
since FTIR spectral data frames can't be fed into plotting functions
with both transmittance and absorbance data included.

un data.frame de données spectrales IRTF avec conversion entre
l'absorbance ou la transmittance comme demandé. Notez que la colonne de
données d'origine est supprimée car les trames de données spectrales
IRTF ne peuvent pas être introduites dans les fonctions de tracé avec
les données de transmittance et d'absorbance incluses.

## Examples

``` r
# Convert from absorbance to transmittance
sample_spectra_transmittance <- absorbance_to_transmittance(sample_spectra)

# Convert back to absorbance
sample_spectra_absorbance <- transmittance_to_absorbance(sample_spectra_transmittance)
```
