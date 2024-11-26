library(testthat)
library(Math4753Hari)



test_that("Calculates correct area", {
  expect_message(myncurve(mu = 0, sigma = 1, a = 0), "0.4987")
})

test_that("Calculates correct area", {
  expect_message(myncurve(mu = 0, sigma = 1, a = 1), "0.84")
})

test_that("Calculates correct area", {
  expect_message(myncurve(mu = 0, sigma = 1, a = 2), "0.9759")
})

