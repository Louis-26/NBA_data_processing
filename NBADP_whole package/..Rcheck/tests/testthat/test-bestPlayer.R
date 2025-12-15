test_that("best_nba_player works", {
  expect_equal(best_nba_player(top = 4, year = 2021), c("Mfiondu Kabengele","Mfiondu Kabengele","DaQuan Jeffries","DaQuan Jeffries" ))
})
