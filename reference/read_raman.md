# Read Raman file

Reads a Raman spectra file and returns a data.frame in the proper format
for PlotFTIR functions.

Lit un fichier de spectres Raman et renvoie un data.frame dans le format
approprié pour les fonctions PlotFTIR.

## Usage

``` r
read_raman(path = ".", file = NA_character_, sample_name = NA_character_, ...)
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

  Additional parameters to pass to the file reading function.

  Paramètres supplémentaires à transmettre à la fonction de lecture de
  fichier.

## Value

a data.frame containing the Raman spectral data from the file, with
attributes: `intensity` set to `"raman"`, and `raman_metadata`
containing the `KEY=VALUE` header lines (introduced by one or more `#`)
parsed as a named list.

un data.frame contenant les données spectrales Raman du fichier, avec
les attributs : `intensity` défini sur `"raman"`, et `raman_metadata`
contenant les lignes d'en-tête `KEY=VALUE` (introduites par un ou
plusieurs `#`) analysées sous forme de liste nommée.

## Examples

``` r
# Reading a Raman file:
# raman_data <- read_raman("./data/Graphite_Raman.txt")
```
