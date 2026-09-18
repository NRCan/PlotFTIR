# Read FTIR file

Reads provided files and returns a data.frame in the proper format for
PlotFTIR functions.

Lit les fichiers fournis et renvoie un data.frame au format approprié
pour les fonctions PlotFTIR.

## Usage

``` r
read_ftir_directory(path, files, sample_names = NA_character_, ...)
```

## Arguments

- path:

  Path to the file. Default is the current working directory, as `"."`.

  Chemin d'accès au fichier. Par défaut, il s'agit du répertoire de
  travail actuel, sous la forme `"."`.

- files:

  File names, required.

  Noms de fichiers, obligatoires.

- sample_names:

  Name for sample_id column in the returned data.frame. If not provided,
  the file names are used without the extension.

  Nom de la colonne sample_id dans le data.frame renvoyé. S'il n'est pas
  fourni, les noms de fichiers sont utilisés sans l'extension.

- ...:

  Additional parameters to pass to the file reading function. For CSV
  files, see
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html), it may
  be wise to pass `col.names` to disambiguate the input data.

  Paramètres supplémentaires à transmettre à la fonction de lecture de
  fichier. Pour les fichiers CSV, voir
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html), il
  peut être judicieux de passer `col.names` pour désambiguïser les
  données d'entrée.

## Value

a data.frame containing the spectral data from the files.

un data.frame contenant les données spectrales des fichiers.

## See also

[`read_ftir()`](https://nrcan.github.io/PlotFTIR/reference/read_ftir.md)

## Examples

``` r
# Putting some files in a temp dir to read back into PlotFTIR:
td <- tempdir()
write.csv(sample_spectra[sample_spectra$sample_id == "paper", c("wavenumber", "absorbance")],
  file = file.path(td, "ftir_sample_1.csv"), row.names = FALSE
)
write.csv(sample_spectra[sample_spectra$sample_id == "toluene", c("wavenumber", "absorbance")],
  file = file.path(td, "ftir_sample_2.csv"), row.names = FALSE
)

# Read .csv files from the temp directory and call them `sample-1` and `sample-2`
read_ftir_directory(td, c("ftir_sample_1.csv", "ftir_sample_2.csv"), c("sample-1", "sample-2"))
#> PlotFTIR data:
#>   Spectral range: 650.4205 - 3999.434 cm⁻¹ 
#>   Resolution: variable
#>   Intensity type: absorbance 
#>   Number of samples: 2 
#>   Sample IDs: sample-1, sample-2 
```
