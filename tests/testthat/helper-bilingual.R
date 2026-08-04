# Helpers to validate that conditions (errors/warnings/messages) are raised
# correctly in both supported languages (English and French), per the
# `PlotFTIR.lang` option.
#
# Aides pour valider que les conditions (erreurs/avertissements/messages) sont
# levées correctement dans les deux langues prises en charge (anglais et
# français), selon l'option `PlotFTIR.lang`.

expect_error_bilingual <- function(expr, en, fr, ..., fixed = TRUE) {
  expr <- rlang::enquo(expr)
  withr::with_options(
    list(PlotFTIR.lang = "en"),
    testthat::expect_error(rlang::eval_tidy(expr), en, ..., fixed = fixed)
  )
  withr::with_options(
    list(PlotFTIR.lang = "fr"),
    testthat::expect_error(rlang::eval_tidy(expr), fr, ..., fixed = fixed)
  )
}

expect_warning_bilingual <- function(expr, en, fr, ..., fixed = TRUE) {
  expr <- rlang::enquo(expr)
  withr::with_options(
    list(PlotFTIR.lang = "en"),
    testthat::expect_warning(rlang::eval_tidy(expr), en, ..., fixed = fixed)
  )
  withr::with_options(
    list(PlotFTIR.lang = "fr"),
    testthat::expect_warning(rlang::eval_tidy(expr), fr, ..., fixed = fixed)
  )
}

expect_message_bilingual <- function(expr, en, fr, ..., fixed = TRUE) {
  expr <- rlang::enquo(expr)
  withr::with_options(
    list(PlotFTIR.lang = "en"),
    testthat::expect_message(rlang::eval_tidy(expr), en, ..., fixed = fixed)
  )
  withr::with_options(
    list(PlotFTIR.lang = "fr"),
    testthat::expect_message(rlang::eval_tidy(expr), fr, ..., fixed = fixed)
  )
}
