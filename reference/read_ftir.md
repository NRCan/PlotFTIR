# Read FTIR file

Reads a provided file and returns a data.frame in the proper format for
PlotFTIR functions.

Lit un fichier fourni et renvoie un data.frame dans le format approprié
pour les fonctions PlotFTIR.

## Usage

``` r
read_ftir(path = ".", file = NA_character_, sample_name = NA_character_, ...)
```

## Arguments

- path:

  Path to the file. Default is the current working directory, as `"."`.
  Can include the filename, in which case provide `NA` as the filename.

  Chemin d'accès au fichier. Par défaut, il s'agit du répertoire de
  travail actuel, sous la forme `"."`. Peut inclure le nom du fichier,
  auquel cas il faut fournir `NA` comme nom de fichier.

- file:

  File name, required. If the file and path are provided together as
  `path`, then `NA` is accepted.

  Nom du fichier, obligatoire. Si le fichier et le chemin sont fournis
  ensemble en tant que `chemin`, alors `NA` est accepté.

- sample_name:

  Name for sample_id column in the returned data.frame. If not provided,
  the file name is used without the extension.

  Nom de la colonne sample_id dans le data.frame renvoyé. S'il n'est pas
  fourni, le nom du fichier est utilisé sans l'extension.

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

a data.frame containing the spectral data from the file.

un data.frame contenant les données spectrales du fichier.

## See also

[`read_ftir_directory()`](https://nrcan.github.io/PlotFTIR/reference/read_ftir_directory.md)

## Examples

``` r
# Writing a temporary file to read later
tf <- tempfile(fileext = ".csv")
write.csv(sample_spectra[sample_spectra$sample_id == "paper", c("wavenumber", "absorbance")],
  file = tf, row.names = FALSE
)

# Read the .csv file and call the sample `sample1`
read_ftir(tf, sample_name = "sample1")
#> PlotFTIR data:
#>   Spectral range: 650.4205 - 3999.434 cm⁻¹ 
#>   Resolution: variable
#>   Intensity type: absorbance 
#>   Number of samples: 1 
#>   Sample IDs: sample1 
```
