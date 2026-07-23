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
  # Ensure the option is unset before testing
  options('PlotFTIR.lang' = NULL)

  withr::with_envvar(new = c(LANG = 'en_US.UTF-8'), {
    suppressMessages(PlotFTIR::.onLoad())
    expect_equal(getOption('PlotFTIR.lang'), 'en')
  })

  # Reset before next test
  options('PlotFTIR.lang' = NULL)

  withr::with_envvar(new = c(LANG = 'FR_CA.UTF-8'), {
    suppressMessages(PlotFTIR::.onLoad())
    expect_equal(getOption('PlotFTIR.lang'), 'fr')
  })
})

test_that('.onLoad does not overwrite existing PlotFTIR.lang option', {
  # Pre-set the option — .onLoad must leave it untouched
  options('PlotFTIR.lang' = 'fr')

  withr::with_envvar(new = c(LANG = 'en_US.UTF-8'), {
    suppressMessages(PlotFTIR::.onLoad())
    expect_equal(getOption('PlotFTIR.lang'), 'fr')
  })

  options('PlotFTIR.lang' = NULL)
})

test_that('.onAttach prints correct startup message for French and English', {
  # .onLoad must have already set the option, so simulate it here
  withr::with_envvar(new = c(LANG = 'fr_FR.UTF-8'), {
    suppressMessages(PlotFTIR::.onLoad())
    msgs <- capture.output(
      suppressMessages(PlotFTIR::.onAttach()),
      type = 'message'
    )
    expect_true(any(grepl('Trac', msgs, perl = TRUE)))
  })

  withr::with_envvar(new = c(LANG = 'en_US.UTF-8'), {
    suppressMessages(PlotFTIR::.onLoad())
    msgs <- capture.output(
      suppressMessages(PlotFTIR::.onAttach()),
      type = 'message'
    )
    expect_true(any(grepl('Plotting spectra', msgs, perl = TRUE)))
  })
})

test_that('.onAttach respects manually-set PlotFTIR.lang option', {
  # Force French regardless of system language
  options('PlotFTIR.lang' = 'fr')

  withr::with_envvar(new = c(LANG = 'en_US.UTF-8'), {
    msgs <- capture.output(
      suppressMessages(PlotFTIR::.onAttach()),
      type = 'message'
    )
    expect_true(any(grepl('Trac', msgs, perl = TRUE)))
  })

  # Force English regardless of system language
  options('PlotFTIR.lang' = 'en')

  withr::with_envvar(new = c(LANG = 'fr_FR.UTF-8'), {
    msgs <- capture.output(
      suppressMessages(PlotFTIR::.onAttach()),
      type = 'message'
    )
    expect_true(any(grepl('Plotting spectra', msgs, perl = TRUE)))
  })

  options('PlotFTIR.lang' = NULL)
})
