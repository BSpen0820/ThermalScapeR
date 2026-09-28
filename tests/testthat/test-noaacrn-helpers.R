test_that(".crn_period_label keeps exact days (no month snapping)", {
  expect_identical(.crn_period_label(as.Date("2024-01-01"), as.Date("2024-03-31")),
                   "20240101_to_20240331")
  expect_identical(.crn_period_label(as.Date("2024-01-02"), as.Date("2024-01-02")),
                   "20240102_to_20240102")
})

test_that(".crn_years_needed pads the following year for a Dec 31 end west of Greenwich", {
  y <- .crn_years_needed(as.Date("2024-01-01"), as.Date("2024-03-31"), lon = -150.44)
  expect_identical(y$required, 2024L)
  expect_identical(y$padding, integer())

  y <- .crn_years_needed(as.Date("2023-12-30"), as.Date("2023-12-31"), lon = -150.44)
  expect_identical(y$required, 2023L)
  expect_identical(y$padding, 2024L)

  y <- .crn_years_needed(as.Date("2023-11-01"), as.Date("2025-02-01"), lon = -150.44)
  expect_identical(y$required, 2023:2025)
  expect_identical(y$padding, integer())
})

test_that(".crn_years_needed pads the previous year for a Jan 1 start east of Greenwich", {
  y <- .crn_years_needed(as.Date("2024-01-01"), as.Date("2024-01-10"), lon = 128.9)
  expect_identical(y$required, 2024L)
  expect_identical(y$padding, 2023L)

  y <- .crn_years_needed(as.Date("2024-03-01"), as.Date("2024-12-31"), lon = 128.9)
  expect_identical(y$padding, integer())

  y <- .crn_years_needed(as.Date("2024-01-01"), as.Date("2024-01-10"), lon = -150.44)
  expect_identical(y$padding, integer())
})
