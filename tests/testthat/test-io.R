withr::local_options(list(
  cli.num_colors = 1,
  cli.unicode = FALSE
))

test_that("reading csv works", {
  # Create a temporary CSV with wavenumber and absorbance columns
  data <- data.frame(
    wavenumber = 1000:1500,
    absorbance = biodiesel$absorbance[1:501]
  )
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  result <- read_ftir(path = tmppath, file = tmpfile)

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$absorbance, 4), round(data$absorbance, 4))

  # Make sure single file paths work
  expect_equal(result, read_ftir(path = file.path(tmppath, tmpfile), file = NA))

  # Create a temporary CSV with misnamed wavenumber column
  data <- data.frame(
    "row" = 1000:1500,
    absorbance = biodiesel$absorbance[1:501]
  )
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  expect_message_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "has deduced that input data",
    fr = "a déduit que la colonne de données d'entrée"
  )
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$row)
  expect_equal(round(result$absorbance, 2), round(data$absorbance, 2))

  # do it backwards
  data <- data.frame("absorbance" = data$absorbance, "row" = 1000:1500)
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_equal(
    result$wavenumber,
    read_ftir(path = tmppath, file = tmpfile)$wavenumber
  )

  # do it with a name match
  data <- data.frame("energy" = 1000:1500, "absorbance" = data$absorbance)
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_equal(
    result$wavenumber,
    read_ftir(path = tmppath, file = tmpfile)$wavenumber
  )

  # Create a temporary CSV with misnamed energy column (absorbance)
  data <- data.frame(
    "wavenumber" = 1000:1500,
    "energy" = biodiesel$absorbance[1:501]
  )
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  expect_message_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "has deduced that input data",
    fr = "a déduit que la colonne de données d'entrée"
  )
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$absorbance, 4), round(data$energy, 4))

  # Create a temporary CSV with misnamed energy column (transmittance)
  data <- data.frame(
    "wavenumber" = 1000:1500,
    "energy" = 100 - (biodiesel$absorbance[1:501] * 20)
  )
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  expect_message_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "has deduced that input data",
    fr = "a déduit que la colonne de données d'entrée"
  )
  suppressMessages(
    result <- read_ftir(path = tmppath, file = tmpfile, sample_name = "test")
  )

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "transmittance", "sample_id"))
  expect_equal(result$sample_id[1], "test")
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$transmittance, 2), round(data$energy, 2))

  data <- data.frame(
    "wavenumber" = 1000:1500,
    "absorbance" = biodiesel$absorbance[1:501],
    sample_id = "test"
  )
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_error_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "Input file has too many columns",
    fr = "Le fichier d'entrée contient trop de colonnes"
  )

  data <- data.frame("row" = 1000:1500, "col" = 2000:2500)
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_error_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "Could not confidently determine which column contains wavenumber",
    fr = "Impossible de déterminer avec certitude quelle colonne contient les données de nombre d'ondes"
  )
})

test_that("read_ftir handles invalid arguments", {
  expect_error_bilingual(
    read_ftir(path = NULL, file = "file.csv"),
    en = "must be a single string value",
    fr = "doit être une valeur de chaîne unique"
  )
  expect_error_bilingual(
    read_ftir(path = "path", file = NULL),
    en = "must be a single string value",
    fr = "doit être une valeur de chaîne unique"
  )
  expect_error_bilingual(
    read_ftir(path = c("path1", "path2"), file = "file.csv"),
    en = "must be a single string value",
    fr = "doit être une valeur de chaîne unique"
  )
  expect_error_bilingual(
    read_ftir(path = "path", file = c("file1.csv", "file2.csv")),
    en = "must be a single string value",
    fr = "doit être une valeur de chaîne unique"
  )
  expect_error_bilingual(
    read_ftir(path = ".", file = "file.csv", sample_name = c("name1", "name2")),
    en = "must be a single string value or single",
    fr = "doit être une valeur de chaîne unique ou un seul"
  )
  expect_error_bilingual(
    read_ftir(path = ".", file = "file.csv", sample_name = 123),
    en = "must be a string value",
    fr = "doit être une valeur de chaîne"
  )
  expect_error_bilingual(
    read_ftir(path = ".", file = "nonexistent_file.csv"),
    en = 'nonexistent_file.csv" does not appear to exist',
    fr = 'nonexistent_file.csv" ne semble pas exister'
  )
  tempfile <- withr::local_tempfile(fileext = ".docx")
  file.create(tempfile)
  expect_error_bilingual(
    read_ftir(path = dirname(tempfile), file = basename(tempfile)),
    en = "could not be processed",
    fr = "n'a pas pu être traité"
  )
  tempfile <- withr::local_tempfile(fileext = ".a2r")
  file.create(tempfile)
  expect_error_bilingual(
    read_ftir(path = dirname(tempfile), file = basename(tempfile)),
    en = "PlotFTIR is not (yet) able to read .a2r files",
    fr = "PlotFTIR ne peut pas encore lire les fichiers .a2r"
  )
  tempfile <- withr::local_tempfile(fileext = ".spc")
  file.create(tempfile)
  expect_error_bilingual(
    read_ftir(path = dirname(tempfile), file = basename(tempfile)),
    en = "PlotFTIR is not (yet) able to read .spc files",
    fr = "PlotFTIR ne peut pas encore lire les fichiers .spc"
  )
})

test_that("reading asp works", {
  # create data and write file
  data <- data.frame(
    wavenumber = 1000:1500,
    absorbance = biodiesel$absorbance[1:501]
  )
  temp_file <- withr::local_tempfile(fileext = ".asp")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  write(
    c(
      nrow(data),
      max(data$wavenumber),
      min(data$wavenumber),
      1,
      2,
      4,
      rev(data$absorbance)
    ),
    temp_file,
    ncolumns = 1
  )

  # Read the data using read_ftir
  expect_message_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "has deduced that input data",
    fr = "a déduit que les données d'entrée"
  )
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$absorbance, 2), round(data$absorbance, 2))

  # Create a temporary CSV with misnamed energy column (transmittance)
  data <- data.frame(
    "wavenumber" = 1000:1500,
    "transmittance" = 100 - (biodiesel$absorbance[1:501] * 20)
  )
  write(
    c(
      nrow(data),
      max(data$wavenumber),
      min(data$wavenumber),
      1,
      2,
      4,
      rev(data$transmittance)
    ),
    temp_file,
    ncolumns = 1
  )

  # Read the data using read_ftir
  expect_message_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "has deduced that input data",
    fr = "a déduit que les données d'entrée"
  )
  suppressMessages(
    result <- read_ftir(path = tmppath, file = tmpfile, sample_name = "test")
  )

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "transmittance", "sample_id"))
  expect_equal(result$sample_id[1], "test")
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$transmittance, 2), round(data$transmittance, 2))
})

test_that("reading .jdx works", {
  if (!requireNamespace("readJDX", quietly = TRUE)) {
    expect_error_bilingual(
      read_ftir_jdx(data.frame("testdata" = LETTERS)),
      en = "requires readJDX package installation for this function.",
      fr = "nécessite l'installation du paquet readJDX pour cette fonction."
    )
    testthat::skip("readJDX not available for testing interface")
  }

  jdx_ftir <- read_ftir(system.file("extdata", "SBO.jdx", package = "readJDX"))

  # read the .jdx file using readJDX
  jdx_jdx <- readJDX::readJDX(system.file(
    "extdata",
    "SBO.jdx",
    package = "readJDX"
  ))

  expect_equal(names(jdx_jdx)[4], unique(jdx_ftir$sample_id))
  expect_equal(nrow(jdx_ftir), nrow(jdx_jdx[[4]]))
  expect_true('transmittance' %in% colnames(jdx_ftir))
  expect_message_bilingual(
    read_ftir(
      path = system.file("extdata", "SBO.jdx", package = "readJDX"),
      sample_name = 'test_sample'
    ),
    en = "does not match that contained in the .jdx file",
    fr = "ne correspond pas à celui contenu dans le fichier .jdx",
    fixed = FALSE
  )

  #More IR Data
  jdx_ir2 <- read_ftir(
    path = system.file("extdata", "MiniDIFDUP.JDX", package = "readJDX")
  )
  expect_equal(unique(jdx_ir2$sample_id), "Demo IR Spectrum")

  # NMR Data
  expect_error_bilingual(
    read_ftir(
      path = system.file("extdata", "PCRF.jdx", package = "readJDX")
    ),
    en = "Could not confirm `infrared` or `raman` data file.",
    fr = "Impossible de confirmer le fichier de données",
    fixed = FALSE
  )
  # 2D NMR Data
  expect_error_bilingual(
    suppressWarnings(
      read_ftir(
        path = system.file("extdata", "isasspc1.dx", package = "readJDX")
      )
    ),
    en = "Could not confirm `infrared` or `raman` data file.",
    fr = "Impossible de confirmer le fichier de données",
    fixed = FALSE
  )
})

# Check reading multiple files
test_that("Reading multiple files works", {
  # Prep some files
  data <- data.frame(
    wavenumber = 1000:1500,
    absorbance = biodiesel$absorbance[1:501]
  )
  tmppath <- withr::local_tempdir()
  temp_file1 <- withr::local_tempfile(tmpdir = tmppath, fileext = ".csv")
  temp_file2 <- withr::local_tempfile(tmpdir = tmppath, fileext = ".csv")
  tmpfile1 <- basename(temp_file1)
  tmpfile2 <- basename(temp_file2)
  write.csv(data, file = file.path(tmppath, tmpfile1), row.names = FALSE)
  write.csv(data, file = file.path(tmppath, tmpfile2), row.names = FALSE)

  # Read the data using read_ftir
  result <- read_ftir_directory(
    path = tmppath,
    files = c(tmpfile1, tmpfile2),
    sample_names = c("one", "two")
  )

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], "one")
  expect_equal(result$sample_id[nrow(result)], "two")
  expect_equal(nrow(result), nrow(data) * 2)
  expect_equal(result$wavenumber, rep(data$wavenumber, 2))
  expect_equal(round(result$absorbance, 4), rep(round(data$absorbance, 4), 2))

  # Read the data using read_ftir
  result <- read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2))

  # Check the result (no sample names)
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile1))
  expect_equal(
    result$sample_id[nrow(result)],
    tools::file_path_sans_ext(tmpfile2)
  )
  expect_equal(nrow(result), nrow(data) * 2)
  expect_equal(result$wavenumber, rep(data$wavenumber, 2))
  expect_equal(round(result$absorbance, 4), rep(round(data$absorbance, 4), 2))

  # Checking for issues
  expect_error_bilingual(
    suppressWarnings(read_ftir_directory(
      path = tmppath,
      files = c("fake.csv", "fake2.csv")
    )),
    en = "No spectral data was read from files",
    fr = "Aucune donnée spectrale n'a été lue à partir des fichiers"
  )
  expect_warning_bilingual(
    read_ftir_directory(
      path = tmppath,
      files = c(tmpfile1, tmpfile2, "fake.csv")
    ),
    en = 'fake.csv" does not appear to exist',
    fr = 'fake.csv" ne semble pas exister'
  )
  suppressWarnings(
    result2 <- read_ftir_directory(
      path = tmppath,
      files = c(tmpfile1, tmpfile2, "fake.csv")
    )
  )

  expect_equal(result, result2)

  expect_error_bilingual(
    read_ftir_directory(
      path = tmppath,
      files = c(tmpfile1, tmpfile2),
      sample_names = c("One", "Two", "Extra")
    ),
    en = "You provided 3 `sample_names` and 2 `files`",
    fr = "Vous avez fourni 3 `sample_names` et 2 `files`"
  )

  expect_error_bilingual(
    read_ftir_directory(
      path = c(tmppath, tmppath),
      files = c(tmpfile1, tmpfile2)
    ),
    en = "must be a single string value",
    fr = "doit être une valeur de chaîne unique"
  )
  expect_error_bilingual(
    read_ftir_directory(
      path = tmppath,
      files = c(tmpfile1, as.data.frame(tmpfile2))
    ),
    en = "must be a vector of string values",
    fr = "doit être un vecteur de valeurs de chaîne"
  )
})

test_that("plot saves", {
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error_bilingual(
      save_plot(123),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )
    testthat::skip("ggplot2 not available for testing file saving")
  }

  temp_file <- withr::local_tempfile(fileext = ".png")

  expect_false(file.exists(temp_file))
  save_plot(plot_ftir(biodiesel), filename = temp_file)
  expect_true(file.exists(temp_file))

  # test arg checks.
  expect_error_bilingual(
    save_plot("abc", filename = temp_file),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni"
  )
})

test_that("interface to ir is ok", {
  withr::local_options(list(
    cli.num_colors = 1,
    cli.unicode = FALSE
  ))

  if (!requireNamespace("ir", quietly = TRUE)) {
    expect_error_bilingual(
      ir_to_plotftir(data.frame("testdata" = LETTERS)),
      en = "requires ir package installation for this function.",
      fr = "nécessite l'installation du paquet ir pour cette fonction."
    )
    expect_error_bilingual(
      ir_to_df(data.frame("testdata" = LETTERS)),
      en = "requires ir package installation for this function.",
      fr = "nécessite l'installation du paquet ir pour cette fonction."
    )
    expect_error_bilingual(
      plotftir_to_ir(biodiesel),
      en = "requires ir package installation for this function.",
      fr = "nécessite l'installation du paquet ir pour cette fonction."
    )
    testthat::skip("ir not available for testing interface")
  }

  irdata <- ir::ir_sample_data
  # Param checks
  expect_error_bilingual(
    ir_to_plotftir(biodiesel),
    en = "must be of class <ir>, produced by the ir package.",
    fr = "doit être de la classe <ir>, produit par le paquet ir."
  )
  expect_error_bilingual(
    ir_to_df(biodiesel),
    en = "must be of class <ir>, produced by the ir package.",
    fr = "doit être de la classe <ir>, produit par le paquet ir."
  )
  expect_error_bilingual(
    ir_to_plotftir(irdata, what = c(1, "two")),
    en = "must contain the row numbers of sample spectra to extract, or exact names matching what is in `ir_data$id_sample`",
    fr = "doit contenir les numéros de lignes des spectres d'échantillon à extraire, ou les noms exacts correspondant à ceux dans `ir_data$id_sample`"
  )
  expect_error_bilingual(
    ir_to_plotftir(irdata, what = c(1, 1e6)),
    en = "must contain the row numbers of sample spectra to extract, or exact names matching what is in `ir_data$id_sample`",
    fr = "doit contenir les numéros de lignes des spectres d'échantillon à extraire, ou les noms exacts correspondant à ceux dans `ir_data$id_sample`"
  )

  expect_error_bilingual(
    plotftir_to_ir(biodiesel, metadata = "bob"),
    en = "must be either `NA` or a <data.frame>",
    fr = "doit être soit `NA` soit un <data.frame>",
    fixed = FALSE
  )

  allir <- ir_to_plotftir(irdata)
  expect_equal(length(unique(allir$sample_id)), nrow(irdata))
  expect_equal(colnames(allir), c("wavenumber", "absorbance", "sample_id"))

  irnum <- ir_to_plotftir(irdata, what = c(1:5))
  expect_equal(length(unique(irnum$sample_id)), 5)

  irname <- ir_to_plotftir(
    irdata,
    what = c("GN 11-389", "GN 11-400", "GN 11-407")
  )
  expect_true("PlotFTIR_data" %in% class(irname))
  expect_equal(length(unique(irname$sample_id)), 3)

  plotir <- plotftir_to_ir(biodiesel)

  expect_equal(nrow(plotir), length(unique(biodiesel$sample_id)))

  plotirmeta <- plotftir_to_ir(
    biodiesel,
    data.frame(
      "biodiesel_content" = c(0, 0.25, 0.5, 1, 2.5, 5, 7.5, 10, 0.5, 5, NA)
    )
  )

  expect_equal(nrow(plotirmeta), length(unique(biodiesel$sample_id)))
  expect_true("biodiesel_content" %in% colnames(plotirmeta))

  expect_true("ggplot" %in% suppressWarnings(class(plot_ftir(irdata))))
})

test_that("interface to ir is ok for PlotFTIR data (#35)", {
  if (!requireNamespace("ir", quietly = TRUE)) {
    testthat::skip("ir not available for testing interface")
  }

  adjusted_bd <- plotftir_to_ir(biodiesel, metadata = NA) |>
    ir::ir_bc(method = "rubberband", return_bl = FALSE) |>
    ir_to_plotftir(what = NA)

  expect_true("PlotFTIR_data" %in% class(adjusted_bd))
  expect_setequal(unique(adjusted_bd$sample_id), unique(biodiesel$sample_id))

  adjusted_2 <- plotftir_to_ir(biodiesel, metadata = NA) |>
    ir::ir_bc(method = "rubberband", return_bl = FALSE)

  adjusted_2$id_sample <- NULL

  expect_warning_bilingual(
    ir_to_plotftir(ir_data = adjusted_2, what = c(1, 2, 4)),
    en = "Could not find sample spectra ids from",
    fr = "Impossible de trouver les identifiants de spectres d'échantillon"
  )

  suppressWarnings(
    adjusted_2 <- ir_to_plotftir(ir_data = adjusted_2, what = c(1, 2, 4))
  )

  expect_equal(unique(adjusted_2$sample_id), as.character(c(1, 2, 4)))
})

test_that("Interface to ChemoSpec is ok", {
  withr::local_options(list(
    cli.num_colors = 1,
    cli.unicode = FALSE
  ))

  if (!requireNamespace("R.utils", quietly = TRUE)) {
    expect_error_bilingual(
      plotftir_to_chemospec(biodiesel),
      en = "R.utils",
      fr = "R.utils"
    )
    testthat::skip("R.utils not available for testing interface")
  }

  if (!requireNamespace("ChemoSpec", quietly = TRUE)) {
    expect_error_bilingual(
      chemospec_to_plotftir(data.frame("testdata" = LETTERS)),
      en = "ChemoSpec",
      fr = "ChemoSpec"
    )
    expect_error_bilingual(
      plotftir_to_chemospec(biodiesel),
      en = "requires ChemoSpec package installation for this function.",
      fr = "nécessite l'installation du paquet ChemoSpec pour cette fonction."
    )
    testthat::skip("ChemoSpec not available for testing interface")
  }

  data("SrE.IR", package = "ChemoSpec", envir = environment())
  data("SrE.NMR", package = "ChemoSpec", envir = environment())

  expect_error_bilingual(
    chemospec_to_plotftir(SrE.NMR),
    en = "must be of IR spectra, this data appears to be from another instrument.",
    fr = "doit être des spectres IR, ces données semblent provenir d'un autre instrument."
  )
  expect_error_bilingual(
    chemospec_to_plotftir(data.frame("A" = LETTERS)),
    en = "must be of class <Spectra>, produced by the ChemoSpec package. You provided ",
    fr = "doit être de la classe <Spectra>, produite par le package ChemoSpec. Vous avez fourni"
  )

  csftir <- chemospec_to_plotftir(SrE.IR)

  expect_true("PlotFTIR_data" %in% class(csftir))
  expect_equal(colnames(csftir), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(length(unique(csftir$sample_id)), length(SrE.IR$names))

  expect_error_bilingual(
    plotftir_to_chemospec(biodiesel, group_colours = "blue"),
    en = ", or a vector of the same length as group_crit",
    fr = ", ou un vecteur de la même longueur que group_crit"
  )
  expect_error_bilingual(
    plotftir_to_chemospec(
      biodiesel,
      group_crit = c("biodiesel", "unknown"),
      group_colours = c("orange", "green", "blue")
    ),
    en = ", or a vector of the same length as group_crit",
    fr = ", ou un vecteur de la même longueur que group_crit"
  )
  expect_message_bilingual(
    plotftir_to_chemospec(
      biodiesel,
      group_crit = c("biodiesel", "unknown"),
      group_colours = c("red", "blue"),
      description = "This is a very long description with 57 characters in it."
    ),
    en = "ChemoSpec advises that description is 40 characters or less. Your description is 57 characters",
    fr = "ChemoSpec conseille que description fasse 40 caractères ou moins. Votre description fait 57 caractères"
  )

  expect_message_bilingual(
    plotftir_to_chemospec(biodiesel),
    en = " to ensure enough colours available for groups.",
    fr = " pour garantir suffisamment de couleurs disponibles pour les groupes."
  )
  expect_error_bilingual(
    plotftir_to_chemospec(rbind(biodiesel, sample_spectra)),
    en = " has to make 12 or less groups for ChemoSpec to be happy",
    fr = " doit créer 12 groupes ou moins pour que ChemoSpec soit satisfait"
  )

  csdata <- plotftir_to_chemospec(
    biodiesel,
    group_crit = c("biodiesel", "unknown")
  )

  expect_equal(class(csdata), "Spectra")
  expect_type(csdata$data, "double")

  csdata2 <- chemospec_to_plotftir(csdata)

  expect_equal(
    csdata2[csdata2$sample_id == "biodiesel_0", ],
    biodiesel[biodiesel$sample_id == "biodiesel_0", ],
    ignore_attr = TRUE
  )

  expect_true("ggplot" %in% suppressWarnings(class(plot_ftir(SrE.IR))))
})

test_that("parses raman header metadata into attributes", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "##FILETYPE=Raman",
    "##RAMAN WAVELENGTH=532.0",
    "##TITLE=Graphite spectrum",
    "100, 1000",
    "200, 800",
    "300, 600"
  )

  writeLines(raman_content, temp_file)

  result <- read_raman(path = tmppath, file = tmpfile)

  expect_equal(colnames(result), c("wavenumber", "intensity", "sample_id"))
  expect_true("PlotFTIR_data" %in% class(result))
  expect_equal(attr(result, "intensity"), "raman")
  expect_equal(length(unique(result$sample_id)), 1)
})

test_that("reads two-column wavenumber-intensity data correctly", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  wn <- seq(100, 2000, by = 10)
  intensity <- 100 * exp(-(wn - 500)^2 / 5000) + 50
  data_lines <- paste(wn, ",", intensity, sep = "")

  raman_content <- c(
    "##FILETYPE=Raman",
    data_lines
  )

  writeLines(raman_content, temp_file)

  result <- read_raman(path = tmppath, file = tmpfile)

  expect_equal(nrow(result), length(wn))
  expect_equal(result$wavenumber, wn)
  expect_equal(round(result$intensity, 4), round(intensity, 4))
})

test_that("sample_name parameter overrides filename-derived name", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "##FILETYPE=Raman",
    "100, 1000"
  )

  writeLines(raman_content, temp_file)

  result <- read_raman(
    path = tmppath,
    file = tmpfile,
    sample_name = "custom_sample"
  )

  expect_equal(result$sample_id[1], "custom_sample")
})

test_that("errors on non-Raman file without FILETYPE=Raman header", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "100, 1000",
    "200, 800"
  )

  writeLines(raman_content, temp_file)

  expect_error_bilingual(
    read_raman(path = tmppath, file = tmpfile),
    en = "File does not appear to be Raman data",
    fr = "Le fichier ne semble pas être des données Raman."
  )
})

test_that("handles sample_id column in input gracefully", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "##FILETYPE=Raman",
    "100, 1000"
  )

  writeLines(raman_content, temp_file)

  result <- read_raman(path = tmppath, file = tmpfile)

  expect_true("sample_id" %in% colnames(result))
  expect_equal(attr(result, "intensity"), "raman")
})

test_that("read_raman handles invalid arguments", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "##FILETYPE=Raman",
    "100, 1000"
  )

  writeLines(raman_content, temp_file)

  expect_error_bilingual(
    read_raman(path = NULL, file = tmpfile),
    en = "`path` must be a single string value.",
    fr = "`path` doit être une valeur de chaîne unique."
  )

  expect_error_bilingual(
    read_raman(path = tmppath, file = NULL),
    en = "`file` must be a single string value.",
    fr = "`file` doit être une valeur de chaîne unique."
  )

  expect_error_bilingual(
    read_raman(path = tmppath, file = tmpfile, sample_name = c("a", "b")),
    en = "`sample_name` must be a single string value or single ",
    fr = "`sample_name` doit être une valeur de chaîne unique ou un seul "
  )
})

test_that("errors on non-existent file", {
  expect_error_bilingual(
    read_raman(path = ".", file = "nonexistent.csv"),
    en = "does not appear to exist",
    fr = "ne semble pas exister"
  )
})

test_that("WiRE-format CSV with single-hash Raman header is accepted", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "#type = Raman 785nm",
    "#license = CC-BY-NC-ND",
    "#units:x = Raman shift (1/cm)",
    "#units:y = Intensity (Arbitrary Units)",
    '"1800.92","0"',
    '"1799.96","2008.25"',
    '"1798.99","6014.03"'
  )

  writeLines(raman_content, temp_file)

  result <- read_raman(path = tmppath, file = tmpfile)

  expect_equal(colnames(result), c("wavenumber", "intensity", "sample_id"))
  expect_true("PlotFTIR_data" %in% class(result))
  expect_equal(attr(result, "intensity"), "raman")
  expect_equal(nrow(result), 3L)
  expect_equal(round(result$wavenumber, 2), c(1800.92, 1799.96, 1798.99))
  expect_equal(round(result$intensity, 2), c(0, 2008.25, 6014.03))
})

test_that("flexible Raman header regex accepts various formats", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  # Test "#FILETYPE=RAMAN" (uppercase, no double hash)
  raman_content <- c(
    "#FILETYPE=RAMAN",
    "100, 500",
    "200, 600"
  )
  writeLines(raman_content, temp_file)
  result1 <- read_raman(path = tmppath, file = tmpfile)
  expect_equal(attr(result1, "intensity"), "raman")

  # Test "#  FILETYPE  =  raman" (extra whitespace)
  raman_content2 <- c(
    "#  FILETYPE  =  raman",
    "100, 500"
  )
  writeLines(raman_content2, temp_file)
  result2 <- read_raman(path = tmppath, file = tmpfile)
  expect_equal(attr(result2, "intensity"), "raman")

  # Test "#type=RAMAN" (no spaces around equals)
  raman_content3 <- c(
    "#type=RAMAN",
    "100, 500"
  )
  writeLines(raman_content3, temp_file)
  result3 <- read_raman(path = tmppath, file = tmpfile)
  expect_equal(attr(result3, "intensity"), "raman")
})

test_that("WiRE-format CSV reads correctly from temp file", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "#type = Raman 785nm",
    "#license = CC-BY-NC-ND",
    "#units:x = Raman shift (1/cm)",
    "#units:y = Intensity (Arbitrary Units)",
    '"1800.92","0"',
    '"1799.96","2008.25"',
    '"1798.99","6014.03"'
  )

  writeLines(raman_content, temp_file)

  result <- read_raman(path = tmppath, file = tmpfile)

  expect_equal(colnames(result), c("wavenumber", "intensity", "sample_id"))
  expect_true("PlotFTIR_data" %in% class(result))
  expect_equal(attr(result, "intensity"), "raman")
  expect_equal(nrow(result), 3L)
  expect_equal(round(result$wavenumber, 2), c(1800.92, 1799.96, 1798.99))
  expect_equal(round(result$intensity, 2), c(0, 2008.25, 6014.03))
})

test_that("read_ftir_jdx DATATYPE missing error path", {
  if (!requireNamespace("readJDX", quietly = TRUE)) {
    testthat::skip("readJDX not available for testing")
  }

  temp_file <- withr::local_tempfile(fileext = ".jdx")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  # JCAMP-DX file without DATATYPE metadata (proper XYY format)
  jdx_content <- c(
    "##TITLE=Test Spectrum",
    "##JCAMP-DX=5.01",
    "##XUNITS=1/CM",
    "##YUNITS=TRANSMITTANCE",
    "##NPOINTS=10",
    "##FIRSTX=900",
    "##LASTX=4000",
    "##FIRSTY=0.5",
    "##LASTY=1.0",
    "##XFACTOR=1.000000",
    "##YFACTOR=1.000000",
    "##XYDATA=(X++(Y..Y))",
    "900 80 70 60",
    "1000 75 65 55",
    "1100 70 60 50",
    "1200 45",
    "##END="   
  )

  writeLines(jdx_content, file.path(tmppath, tmpfile))

  expect_error_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "Could not find `datatype` in file metadata",
    fr = "Impossible de trouver `datatype` dans les m\u00e9tadonn\u00e9es"
  )
})

test_that("read_ftir_jdx Raman branch is covered", {
  if (!requireNamespace("readJDX", quietly = TRUE)) {
    testthat::skip("readJDX not available for testing")
  }

  temp_file <- withr::local_tempfile(fileext = ".jdx")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  # Synthetic JCAMP-DX Raman spectrum (proper XYY format)
  jdx_content <- c(
    "##TITLE=Raman Test",
    "##JCAMP-DX=5.01",
    "##DATATYPE=RAMAN SPECTRUM",
    "##XUNITS=1/CM",
    "##YUNITS=INTENSITY",
    "##FIRSTX=100",
    "##LASTX=400",
    "##XFACTOR=1.000",
    "##YFACTOR=1.000",
    "##FIRSTY=0",
    "##LASTY=1000",
    "##NPOINTS=10",
    "##XYDATA=(X++(Y..Y))",
    "100 50 48 52",
    "200 800 790 810",
    "300 600 590 610",
    "400 500",
    "##END="
  )

  writeLines(jdx_content, file.path(tmppath, tmpfile))

  expect_message_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "has deduced that input data is Raman spectra",
    fr = "a d\u00e9duit que les donn\u00e9es d'entr\u00e9e sont des spectres Raman"
  )

  result <- suppressMessages(read_ftir(path = tmppath, file = tmpfile))

  expect_equal(colnames(result), c("wavenumber", "intensity", "sample_id"))
  expect_equal(attr(result, "intensity"), "raman")
  expect_equal(nrow(result), 10L)
})

test_that("read_ftir_jdx intensity mismatch warning path", {
  if (!requireNamespace("readJDX", quietly = TRUE)) {
    testthat::skip("readJDX not available for testing")
  }

  temp_file <- withr::local_tempfile(fileext = ".jdx")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  # JCAMP-DX with TRANSMITTANCE in YUNITS but absorbance-like y values (max > 1.2)
  jdx_content <- c(
    "##TITLE=Mismatch Test",
    "##JCAMP-DX=5.01",
    "##DATATYPE=INFRARED SPECTRUM",
    "##YUNITS=TRANSMITTANCE",
    "##XUNITS=1/CM",
    "##FIRSTX=900",
    "##LASTX=1200",
    "##FIRSTY=0.8",
    "##LASTY=2.10",
    "##NPOINTS=10",
    "##XFACTOR=1.000",
    "##YFACTOR=1.000",
    "##XYDATA=(X++(Y..Y))",
    "900 0.8 0.7 0.6",
    "1000 1.50 1.40 1.30",
    "1100 1.80 1.70 1.60",
    "1200 2.10",
    "##END="
  )

  writeLines(jdx_content, file.path(tmppath, tmpfile))

  expect_warning_bilingual(
    read_ftir(path = tmppath, file = tmpfile),
    en = "does not match detected intensity",
    fr = "ne correspond pas \u00e0 l'intensit\u00e9 d\u00e9tect\u00e9e"
  )

  result <- suppressWarnings(read_ftir(path = tmppath, file = tmpfile))

  expect_true("absorbance" %in% colnames(result))
})

test_that("read_raman unsupported extension error", {
  temp_file <- withr::local_tempfile(fileext = ".spc")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)
  file.create(file.path(tmppath, tmpfile), overwrite = TRUE)

  expect_error_bilingual(
    read_raman(path = tmppath, file = tmpfile),
    en = "Input file of type spc could not be processed",
    fr = "Le fichier d'entr\u00e9e de type spc n'a pas pu \u00eatre trait\u00e9"
  )
})

test_that("read_raman_csv no valid data rows error", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  # Raman header but only comment lines, no actual data
  raman_content <- c(
    "##FILETYPE=Raman",
    "##RAMAN WAVELENGTH=532.0",
    "# This is a comment",
    "# Another comment"
  )

  writeLines(raman_content, file.path(tmppath, tmpfile))

  expect_error_bilingual(
    read_raman(path = tmppath, file = tmpfile),
    en = "No valid data rows found",
    fr = "Aucune ligne de donn\u00e9es valide trouv\u00e9e"
  )
})

test_that("read_raman_csv invalid numeric data error", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  # Raman CSV with non-numeric values in data rows
  raman_content <- c(
    "##FILETYPE=Raman",
    "abc, def",
    "ghi, jkl"
  )

  writeLines(raman_content, file.path(tmppath, tmpfile))

  expect_error_bilingual(
    read_raman(path = tmppath, file = tmpfile),
    en = "Invalid numeric data found at row 1",
    fr = "Donn\u00e9es num\u00e9riques invalides trouv\u00e9es \u00e0 la ligne 1"
  )
})

test_that("read_raman path auto-extraction when file is NA and extension matches txt", {
  temp_file <- withr::local_tempfile(fileext = ".txt")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "##FILETYPE=Raman",
    "100, 500"
  )

  writeLines(raman_content, file.path(tmppath, tmpfile))

  result <- read_raman(path = temp_file, file = NA)

  expect_equal(nrow(result), 1L)
  expect_equal(attr(result, "intensity"), "raman")
})

test_that("read_raman sample_name must be string or NA validation", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  raman_content <- c(
    "##FILETYPE=Raman",
    "100, 500"
  )

  writeLines(raman_content, file.path(tmppath, tmpfile))

  expect_error_bilingual(
    read_raman(path = tmppath, file = tmpfile, sample_name = 123),
    en = "`sample_name` must be a string value or",
    fr = "`sample_name` doit \u00eatre une valeur de cha\u00eene ou"
  )

  expect_error_bilingual(
    read_raman(path = tmppath, file = tmpfile, sample_name = c("a", "b")),
    en = "`sample_name` must be a single string value or single ",
    fr = "`sample_name` doit \u00eatre une valeur de cha\u00eene unique ou un seul "
  )
})

test_that("read_ftir_jdx else branch for is.na(intensity) falls back to intensity_type", {
  if (!requireNamespace("readJDX", quietly = TRUE)) {
    testthat::skip("readJDX not available for testing")
  }

  temp_file <- withr::local_tempfile(fileext = ".jdx")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  # JCAMP-DX without absorbance or transmittance in metadata, so intensity stays NA
  jdx_content <- c(
    "##TITLE=No Intensity Type",
    "##JCAMP-DX=5.01",
    "##DATATYPE=INFRARED SPECTRUM",
    "##XUNITS=1/CM",
    "##YUNITS=COUNTS",
    "##FIRSTX=400",
    "##LASTX=4000",
    "##FIRSTY=0.8",
    "##LASTY=2.1",
    "##XFACTOR=1.000",
    "##YFACTOR=1.000",
    "##NPOINTS=10",
    "##XYDATA=(X++(Y..Y))",
    "900 0.80 0.75 0.70",
    "1000 1.50 1.40 1.30",
    "1100 1.80 1.70 1.60",
    "1200 2.10",
    "##END="
  )

  writeLines(jdx_content, file.path(tmppath, tmpfile))

  result <- read_ftir(path = tmppath, file = tmpfile)

  expect_true("absorbance" %in% colnames(result))
})
