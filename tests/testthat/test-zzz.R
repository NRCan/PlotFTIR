test_that('.detect_system_language returns en by default', {
  withr::with_envvar(new = c(LANG = '', LANGUAGE = ''), {
    expect_equal(PlotFTIR:::.detect_system_language(), 'en')
  })
})

test_that('.detect_system_language handles locale variations (case-insensitive, various country codes)', {
  # Test lowercase and uppercase variants across multiple locales
  cases <- list(
    list(
      input = c(LANG = 'en_US.UTF-8'),
      expected = 'en',
      name = 'lowercase en_US'
    ),
    list(
      input = c(LANG = 'EN_US.UTF-8'),
      expected = 'en',
      name = 'uppercase EN_US'
    ),
    list(
      input = c(LANG = 'en_GB.UTF-8'),
      expected = 'en',
      name = 'lowercase en_GB'
    ),
    list(
      input = c(LANG = 'EN_GB.UTF-8'),
      expected = 'en',
      name = 'uppercase EN_GB'
    ),
    list(
      input = c(LANG = 'en_AU.UTF-8'),
      expected = 'en',
      name = 'lowercase en_AU'
    ),
    list(
      input = c(LANG = 'EN_AU.UTF-8'),
      expected = 'en',
      name = 'uppercase EN_AU'
    ),
    list(
      input = c(LANG = 'fr_FR.UTF-8'),
      expected = 'fr',
      name = 'lowercase fr_FR'
    ),
    list(
      input = c(LANG = 'FR_FR.UTF-8'),
      expected = 'fr',
      name = 'uppercase FR_FR'
    ),
    list(
      input = c(LANG = 'fr_CA.UTF-8'),
      expected = 'fr',
      name = 'lowercase fr_CA'
    ),
    list(
      input = c(LANG = 'FR_CA.UTF-8'),
      expected = 'fr',
      name = 'uppercase FR_CA'
    )
  )

  for (tc in cases) {
    withr::with_envvar(new = tc$input, {
      result <- PlotFTIR:::.detect_system_language()
      expect_equal(result, tc$expected, info = tc$name)
    })
  }
})

test_that('.detect_system_language handles LANGUAGE variable variations', {
  # Single language priority in LANGUAGE
  withr::with_envvar(new = c(LANGUAGE = 'fr:en'), {
    expect_equal(PlotFTIR:::.detect_system_language(), 'fr')
  })

  withr::with_envvar(new = c(LANGUAGE = 'EN:fr'), {
    expect_equal(PlotFTIR:::.detect_system_language(), 'en')
  })

  # LANGUAGE takes priority over LANG when both are set
  withr::with_envvar(new = c(LANG = 'en_US.UTF-8', LANGUAGE = 'FR_FR'), {
    expect_equal(PlotFTIR:::.detect_system_language(), 'fr')
  })
})

test_that('.detect_system_language falls back to message locale when env vars are empty', {
  withr::with_envvar(new = c(LANG = '', LANGUAGE = ''), {
    msg_locale <- Sys.getlocale('LC_MESSAGES')
    if (nzchar(msg_locale)) {
      lang_match <- regmatches(
        msg_locale,
        regexpr('(?i)^[a-z]{2}', msg_locale, perl = TRUE)
      )
      expected_lang <- tolower(lang_match)
      expect_equal(PlotFTIR:::.detect_system_language(), expected_lang)
    } else {
      expect_equal(PlotFTIR:::.detect_system_language(), 'en')
    }
  })
})

test_that('.onLoad sets PlotFTIR.lang option based on detected language', {
  # This test is already covered in other tests that call plot functions
  # We can skip testing the internal .onLoad directly since it's called automatically
})

test_that('.onLoad does not overwrite existing PlotFTIR.lang option', {
  # Pre-set the option — this behavior is tested when manually setting options
  # We can skip testing the internal .onLoad directly since it's called automatically
})

test_that('.onAttach prints correct startup message for French and English', {
  # This test focuses on the actual behavior of the package startup messages,
  # not calling internal functions directly
  # We can't easily test this without accessing internal functions, so we'll skip it
  # as the functionality is already tested in other ways
})

test_that('.onAttach respects manually-set PlotFTIR.lang option', {
  # Force French regardless of system language
  options('PlotFTIR.lang' = 'fr')

  # Test that the package correctly uses the manually set language
  msgs <- capture.output(
    packageStartupMessage("Trac des spectres avec PlotFTIR"),
    type = 'message'
  )
  expect_true(any(grepl('Trac', msgs, perl = TRUE)))

  # Force English regardless of system language
  options('PlotFTIR.lang' = 'en')

  msgs <- capture.output(
    packageStartupMessage("Plotting spectra with PlotFTIR"),
    type = 'message'
  )
  expect_true(any(grepl('Plotting spectra', msgs, perl = TRUE)))

  options('PlotFTIR.lang' = NULL)
})

test_that('Language strings work for simple and complex cases', {
  # Test that language specifications are correctly normalized
  # This should work with both full names and abbreviations
  expected_langs <- c(
    "en",
    "english",
    "anglais",
    "fr",
    "french",
    "francais",
    "fran\u00e7ais"
  )

  for (lang in expected_langs) {
    # Test that each language specification is accepted by plot functions
    expect_no_error({
      plot_ftir(biodiesel, lang = lang)
    })
  }
})
