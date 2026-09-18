# Check FTIR Data

Check the provided FTIR data.frame is appropriate for manipulation or
plotting. Not typically called directly, but as a function in data
integrity check process before further calculation or plotting happens.
Sets data.frame attribute \`intensity\` to \`transmittance\` or
\`absorbance\` if not previously set.

Vérifie que le data.frame IRTF fourni est approprié pour la manipulation
ou le tracé. Cette fonction n'est généralement pas appelée directement,
mais elle est utilisée dans le cadre du processus de vérification de
l'intégrité des données avant tout autre calcul ou tracé. Définit
l'attribut data.frame \`intensity\` à \`transmittance\` ou
\`absorbance\` s'il n'a pas été défini auparavant.

## Usage

``` r
check_ftir_data(ftir)
```

## Arguments

- ftir:

  A data.frame of FTIR spectral data.

  Un data.frame de données spectrales IRTF.

## Value

Invisibly returns FTIR data if ok, or raises an error.

Renvoie de manière invisible les données IRTF si elles sont correctes,
ou soulève une erreur.

## Examples

``` r
# This returns (invisibly) the biodiesel data. If instead there was an issue
# with the data structure it would raise an error.
check_ftir_data(biodiesel)
```
