test_that("Corner cases work.", {
  expect_identical(
    .do_function(
      do_f = NULL,
      default_f = mean,
      main_args = list(1:10)
    ),
    mean(1:10)
  )
})
