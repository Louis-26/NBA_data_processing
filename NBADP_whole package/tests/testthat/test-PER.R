test_that("PER is a number of player efficiency rate", {
  expect_equal(PER(lgdata = lg, regulardata = X2021to2022data, yrs = "2021-22", "LeBron James","LAL"),26.25731)
  expect_equal(PER(lgdata = lg, regulardata = X2021to2022data, yrs = "2021-22", "Deandre Ayton","PHX"),21.981326)
})
