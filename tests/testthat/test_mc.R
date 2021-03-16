#' ---
#' title: "Test: mc"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: mc}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ include=FALSE, cache=FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#+ setup
library(testthat)
context("Test mc.")
#'
#+ load libraries
library(semmcci)
library(lavaan)
library(MASS)
theta_hit <- function(lo,
                      theta,
                      up) {
  lo < theta & theta < up
}
#'
#+ Generate Data
mu <- c(
  x = 100,
  m = 100,
  y = 100
)
Sigma <- matrix(
  data = c(
    225,
    114.7279,
    173.2279,
    114.7279,
    225,
    173.2279,
    173.2279,
    173.2279,
    225
  ),
  ncol = 3
)
data <- mvrnorm(
  n = 100,
  mu = mu,
  Sigma = Sigma,
  empirical = TRUE
)
#'
#+ Model Fit
model <- "
  y ~ cp * x + b * m
  m ~ a * x
  ab := a * b
"
object <- sem(
  data = data,
  model = model
)
#'
#+ Monte Carlo
out <- mc(
  object = object,
  R = 20000L,
  alpha = c(0.001, 0.01, 0.05),
  plot = TRUE
)
#'
#+ testthat, echo=TRUE
test_that("99.9% CI", {
  expect_true(
    theta_hit(
      lo = out[, "ci_0.05"],
      theta = 0.26,
      up = out[, "ci_99.95"]
    )
  )
})
test_that("99% CI", {
  expect_true(
    theta_hit(
      lo = out[, "ci_0.5"],
      theta = 0.26,
      up = out[, "ci_99.5"]
    )
  )
})
test_that("95% CI", {
  expect_true(
    theta_hit(
      lo = out[, "ci_2.5"],
      theta = 0.26,
      up = out[, "ci_97.5"]
    )
  )
})
test_that("object is not of class lavaan", {
  expect_error(
    mc(
      object = mu
    )
  )
})
test_that("no defined parameter", {
  model <- "
    y ~ cp * x + b * m
    m ~ a * x
  "
  object <- sem(
    data = data,
    model = model
  )
  expect_error(
    mc(
      object = object
    )
  )
})
