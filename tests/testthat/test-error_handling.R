# .pkg_abort() ----------------------------------------------------------

test_that(".pkg_abort() raises an error using the English message by default", {
  withr::local_options(PlotFTIR.lang = "en")

  expect_error(
    .pkg_abort(list(en = "an english error", fr = "une erreur fran\u00e7aise")),
    "an english error"
  )
})

test_that(".pkg_abort() raises an error using the French message when set", {
  withr::local_options(PlotFTIR.lang = "fr")

  expect_error(
    .pkg_abort(list(en = "an english error", fr = "une erreur fran\u00e7aise")),
    "une erreur fran\u00e7aise"
  )
})

test_that(".pkg_abort() falls back to English when the requested language is missing", {
  withr::local_options(PlotFTIR.lang = "fr")

  expect_error(
    .pkg_abort(list(en = "an english error")),
    "an english error"
  )
})

test_that(".pkg_abort() works with a named character vector as well as a list", {
  expect_error(
    .pkg_abort(c(en = "vector error", fr = "erreur vectorielle")),
    "vector error"
  )
})

test_that(".pkg_abort() itself errors when messages is not named", {
  expect_error(
    .pkg_abort(list("unnamed error")),
    "must be a named character vector or a named list"
  )
})

test_that(".pkg_abort() itself errors when messages is not character or list", {
  # A non-character, non-list input (e.g. NULL) should trip the guard clause
  expect_error(
    .pkg_abort(NULL),
    "must be a named character vector or a named list"
  )
})

test_that(".pkg_abort() attaches the supplied call for reporting", {
  f <- function() {
    .pkg_abort(list(en = "boom", fr = "boum"), call = rlang::caller_env())
  }

  err <- tryCatch(f(), error = function(e) e)
  expect_s3_class(err, "rlang_error")
  expect_true(!is.null(err$call))
})


# .pkg_warn() -------------------------------------------------------------

test_that(".pkg_warn() raises a warning using the English message by default", {
  withr::local_options(PlotFTIR.lang = "en")

  expect_warning(
    .pkg_warn(list(
      en = "an english warning",
      fr = "un avertissement fran\u00e7ais"
    )),
    "an english warning"
  )
})

test_that(".pkg_warn() raises a warning using the French message when set", {
  withr::local_options(PlotFTIR.lang = "fr")

  expect_warning(
    .pkg_warn(list(
      en = "an english warning",
      fr = "un avertissement fran\u00e7ais"
    )),
    "un avertissement fran\u00e7ais"
  )
})

test_that(".pkg_warn() falls back to English when the requested language is missing", {
  withr::local_options(PlotFTIR.lang = "fr")

  expect_warning(
    .pkg_warn(list(en = "an english warning")),
    "an english warning"
  )
})

test_that(".pkg_warn() itself errors when messages is not named", {
  expect_error(
    .pkg_warn(list("unnamed warning")),
    "must be a named character vector or a named list"
  )
})

test_that(".pkg_warn() itself errors when messages is not character or list", {
  expect_error(
    .pkg_warn(NULL),
    "must be a named character vector or a named list"
  )
})


# .pkg_inform() -----------------------------------------------------------

test_that(".pkg_inform() emits a message using the English message by default", {
  withr::local_options(PlotFTIR.lang = "en")

  expect_message(
    .pkg_inform(list(
      en = "an english message",
      fr = "un message fran\u00e7ais"
    )),
    "an english message"
  )
})

test_that(".pkg_inform() emits a message using the French message when set", {
  withr::local_options(PlotFTIR.lang = "fr")

  expect_message(
    .pkg_inform(list(
      en = "an english message",
      fr = "un message fran\u00e7ais"
    )),
    "un message fran\u00e7ais"
  )
})

test_that(".pkg_inform() falls back to English when the requested language is missing", {
  withr::local_options(PlotFTIR.lang = "fr")

  expect_message(
    .pkg_inform(list(en = "an english message")),
    "an english message"
  )
})

test_that(".pkg_inform() itself errors when messages is not named", {
  expect_error(
    .pkg_inform(list("unnamed message")),
    "must be a named character vector or a named list"
  )
})

test_that(".pkg_inform() itself errors when messages is not character or list", {
  expect_error(
    .pkg_inform(NULL),
    "must be a named character vector or a named list"
  )
})


# .get_language() ----------------------------------------------------------

test_that(".get_language() recognizes English aliases", {
  withr::local_options(PlotFTIR.lang = "en")
  expect_equal(.get_language(), "en")

  withr::local_options(PlotFTIR.lang = "English")
  expect_equal(.get_language(), "en")
})

test_that(".get_language() recognizes French aliases", {
  withr::local_options(PlotFTIR.lang = "fr")
  expect_equal(.get_language(), "fr")

  withr::local_options(PlotFTIR.lang = "francais")
  expect_equal(.get_language(), "fr")
})

test_that(".get_language() defaults to English when unset", {
  withr::local_options(PlotFTIR.lang = NULL)
  expect_equal(.get_language(), "en")
})

test_that(".get_language() defaults to English for an unrecognized value", {
  withr::local_options(PlotFTIR.lang = "klingon")
  expect_equal(.get_language(), "en")
})


# User-facing bilingual errors, end to end ---------------------------------

test_that("exported functions raise bilingual errors on malformed input", {
  withr::local_options(PlotFTIR.lang = "en")
  expect_error(plot_ftir("not a data frame"), "must be")

  withr::local_options(PlotFTIR.lang = "fr")
  expect_error(plot_ftir("not a data frame"), "doit")
})
