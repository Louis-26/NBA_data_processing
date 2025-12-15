test_that("predSalary works", {
  expect_equal(pred_salary(0.6, 3.9, 2.1, 0.4, 1.4, 6, 0, 0.8, 13.4, 1.4, 13.4, -2.9, 0.6), 2400420.64)
  expect_equal(pred_salary(1, 10.8, 6, 1.2, 2, 2, -0.2, 2.1, 19.3, 3.6, 25, -1.5, 1.2),2051165.6)
})
