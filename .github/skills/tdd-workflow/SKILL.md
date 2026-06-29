---
name: tdd-workflow
trigger: writing or reviewing tests
description: Test-driven development workflow. Use when writing any R code (writing new features, fixing bugs, refactoring, or reviewing tests).
---

# TDD workflow

## Core principle

Write a failing test first, then implement the minimal code to make it pass, then refactor. Never write implementation code without a failing test driving it.

## File naming

Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`. Within the correct test file for the source file under test, place new tests adjacent to existing tests for the same function being tested, or at the end of the file if no tests for that function exist yet.

## Running tests

```r
# Full suite
devtools::test(reporter = "check")

# Single file
devtools::test(filter = "name", reporter = "check")
```

Testing functions load code automatically. You do not need to call `library()` or `devtools::load_all()` separately.

## Coverage

Goal: **100%** for every edited file. After editing `R/{name}.R`, replace `{name}` with the actual filename and verify:

```r
# Replace {name} with the actual filename (e.g., "plot_ftir")
covr_res <- devtools:::test_coverage_active_file("R/{name}.R")
which(purrr::map_int(covr_res, "value") == 0)
```

If a specific line or branch cannot be covered without unreasonable test contortion (e.g., a defensive check against an impossible internal state), add a `# nocov start` / `# nocov end` comment pair around those lines and leave a comment explaining why coverage is excluded. Do not write tests purely to hit a coverage number.

Files excluded from the coverage requirement:
- `R/*-package.R`
- `R/aaa-shared_params.R`
- Files matching `R/import-standalone-*.R`

## Test types

### Unit tests

Test individual functions in isolation:

```r
test_that("fetch_records() returns a tibble (#2)", {
  result <- fetch_records(sample_input)
  expect_s3_class(result, "tbl_df")
})
```

### Integration tests

Test end-to-end pipelines through multiple functions:

```r
test_that("build_report() produces expected output (#15)", {
  input <- data.frame(value = c(1.123, 2.456, NA))
  result <- build_report(input, tempfile())
  expect_equal(nrow(result), 2L)
})
```

### Snapshot tests

For complex outputs that are hard to specify with equality assertions:

```r
test_that("build_summary print method is stable (#123)", {
  expect_snapshot(print(build_summary(sample_data)))
})
```

When snapshots change intentionally, read the full diff of the `.md` snapshot file to confirm the change matches the expected behavioral change described in the issue. Only call `testthat::snapshot_accept()` after confirming the new snapshot output is correct. Never accept a snapshot diff without reviewing it:

```r
testthat::snapshot_accept("test_name")
```

Snapshots are stored in `tests/testthat/_snaps/`. The filename corresponds to the R file being tested, ending with `.md`.

## Test design principles

- **Self-sufficient:** each test contains its own setup, execution, and assertion. Tests must be runnable in isolation.
- **Self-contained setup:** keep each test's setup code with the test (do not split across helpers). When setup is repeated, this is acceptable if it keeps each test readable in isolation; extract only if the setup becomes genuinely hard to understand or maintain.
- **One concept per test:** a failing test should tell you exactly what broke.
- **Minimal with few comments:** keep tests lean. Avoid over-commenting.
- **Issue reference in description:** the `desc` of every new `test_that()` call should end with one or more parenthetical issue references for the issue(s) *verified by those tests* — typically the issue currently being solved. **Never guess or invent issue numbers.** Determine the number from the user's prompt, the branch name (`git branch --show-current`), or `gh issue list`. Before writing a number, verify you can trace it to one of these sources. If none of these sources yields a verifiable issue number (e.g., `gh` is unavailable or returns an error, the branch name contains no number, and the user's prompt does not mention one), use `#noissue` and inform the user that an issue reference could not be determined automatically. If no tracked issue applies, do not add a reference number. The numbers in the examples below are illustrative placeholders — do not copy them:
  ```r
  test_that("fetch_records() returns correct columns (#1)", { ... })
  test_that("build_summary() returns correct columns (#2, #3)", { ... })
  test_that(".check_record() errors on empty input", { ... })
  ```

## testthat Edition 3 — deprecated patterns

```r
# Deprecated → Modern
context("Data validation")        # Remove — filename serves this purpose
expect_equivalent(x, y)          # expect_equal(x, y, ignore_attr = TRUE)
with_mock(...)                    # local_mocked_bindings(...)
expect_is(x, "data.frame")       # expect_s3_class(x, "data.frame")
```

Detailed sub-sections and examples follow below.

## Essential expectations

### Equality & identity

```r
expect_equal(x, y)                        # with numeric tolerance
expect_equal(x, y, tolerance = 0.001)
expect_equal(x, y, ignore_attr = TRUE)
expect_identical(x, y)                    # exact match
```

### Conditions

**Errors thrown by this package** (via `.cli_abort()`) should always be tested
with `expect_error()`

```r
test_that("process_data() errors on empty input (#42)", {
  expect_error(
    process_data(data.frame()),
    "PlotFTIR",
    "empty_input"
  )
})
```

**Errors from other packages** can be tested with `expect_error()`, optionally
wrapped in `expect_snapshot()` to lock down the message text:

```r
expect_error(code, "pattern")
expect_error(code, class = "some-error-class")

# Lock down both class and message text:
test_that("fetch_records errors on invalid input (#456)", {
  expect_snapshot(
    (expect_error(
      fetch_records("not valid input"),
      class = "pkg-error"
    ))
  )
})
```

```r
expect_warning(code)
expect_no_warning(code)
expect_message(code)
expect_no_message(code)
```

### Collections

```r
expect_setequal(x, y)           # same elements, any order
expect_in(element, set)
expect_named(x, c("a", "b"))
```

### Type & structure

```r
expect_type(x, "double")
expect_s3_class(x, "tbl_df")
expect_length(x, 10)
expect_null(x)
```

### Logical

These expectations are a last resort when more-specific checks aren't available.

```r
expect_true(x)
expect_false(x)
```

## `withr` patterns for temporary state

```r
withr::local_options(list(PlotFTIR.verbose = TRUE))
withr::local_envvar(MY_VAR = "value")
withr::local_tempfile(lines = c("a", "b"))
```

## Fixtures

Store static test data in `tests/testthat/fixtures/` and access via:

```r
test_path("fixtures", "sample.rds")
```

## Mocking

Mock functions that return non-deterministic output (e.g. random numbers, current timestamps, UUIDs) or that depend on external state (file system, environment variables, system time). Do not mock stable, pure functions within this package.

## Common mistakes

- **Do not modify tests to make them pass.** Fix the implementation.
- **Do not write tests that depend on other tests' state.** Each test must be independently runnable.
- **Ask for help if test is bad.** If you think a test might be invalid, do not loop through trying to make impossible tests pass. Ask for help if possible.
- **Do not write tests that just check that a function exists** — this is not a useful test. Test the function's behavior instead.
