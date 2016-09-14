context("Evaluation: extract")

test_that("outputs are extracted correctly", {
  testeval <- "n <- 100
    warning('this is a warning!')
    x <- rnorm(n)
    plot(x)
    mean(x)
    message('this is a message!')
    y <- rnorm(n)
    z <- x+y
    hist(z)
    stop('this is an error!')"
  
  output <- evaluate(testeval)
  
  expect_that(length(extract(output, "source")), equals(10))
  expect_that(length(extract(output, "graphics")), equals(2))
  expect_that(length(extract(output, "text")), equals(1))
  expect_that(length(extract(output, "warning")), equals(1))
  expect_that(length(extract(output, "message")), equals(1))
  expect_that(length(extract(output, "error")), equals(1))
})
  

