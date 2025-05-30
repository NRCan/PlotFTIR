% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{read_ftir}
\alias{read_ftir}
\title{Read FTIR file}
\usage{
read_ftir(path = ".", file = NA, sample_name = NA, ...)
}
\arguments{
\item{path}{Path to the file. Default is the current working directory, as \code{"."}. Can include the filename, in which case provide \code{NA} as the filename.

Chemin d'accès au fichier. Par défaut, il s'agit du répertoire de travail actuel, sous la forme \code{"."}. Peut inclure le nom du fichier, auquel cas il faut fournir \code{NA} comme nom de fichier.}

\item{file}{File name, required. If the file and path are provided together as \code{path}, then \code{NA} is accepted.

Nom du fichier, obligatoire. Si le fichier et le chemin sont fournis ensemble en tant que \code{chemin}, alors \code{NA} est accepté.}

\item{sample_name}{Name for sample_id column in the returned data.frame. If not provided, the file name is used without the extension.

Nom de la colonne sample_id dans le data.frame renvoyé. S'il n'est pas fourni, le nom du fichier est utilisé sans l'extension.}

\item{...}{Additional parameters to pass to the file reading function. For CSV files, see \code{\link[utils:read.table]{utils::read.csv()}}, it may be wise to pass \code{col.names} to disambiguate the input data.

Paramètres supplémentaires à transmettre à la fonction de lecture de fichier. Pour les fichiers CSV, voir \code{\link[utils:read.table]{utils::read.csv()}}, il peut être judicieux de passer \code{col.names} pour désambiguïser les données d'entrée.}
}
\value{
a data.frame containing the spectral data from the file.

un data.frame contenant les données spectrales du fichier.
}
\description{
Reads a provided file and returns a data.frame in the proper format for PlotFTIR functions.

Lit un fichier fourni et renvoie un data.frame dans le format approprié pour les fonctions PlotFTIR.
}
\examples{
# Writing a temporary file to read later
tf <- tempfile(fileext = ".csv")
write.csv(sample_spectra[sample_spectra$sample_id == "paper", c("wavenumber", "absorbance")],
  file = tf, row.names = FALSE
)

# Read the .csv file and call the sample `sample1`
read_ftir(tf, sample_name = "sample1")
}
\seealso{
\code{\link[=read_ftir_directory]{read_ftir_directory()}}
}
