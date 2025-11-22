library(testthat)

# Validity
test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("valid sparse_numeric object", {
  x <- new("sparse_numeric", value = c(1, 2, 3), pos = c(1L, 2L, 3L), length = 5L)
  expect_true(validObject(x))
})

test_that("invalid sparse_numeric object (wrong length)", {
  expect_error({
    x <- new("sparse_numeric", value = c(1, 2), pos = c(1L, 2L), length = 1L)
    validObject(x)
  })
})

# Coercions
test_that("numeric -> sparse_numeric coercion", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
  expect_equal(x@value, c(1,2))
  expect_equal(x@pos, c(2L, 4L))
})

test_that("sparse_numeric -> numeric coercion", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  y <- as(x, "numeric")
  expect_equal(y, c(0,1,0,2))
})

# Generics
test_that("check generics exist", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_crossprod"))
})

# Methods
x <- as(c(0, 0, 3, 0, 2), "sparse_numeric")
y <- as(c(1, 0, 0, 4, 2), "sparse_numeric")

test_that("addition (+ and sparse_add)", {
  sum1 <- sparse_add(x, y)
  sum2 <- x + y
  expect_s4_class(sum1, "sparse_numeric")
  expect_equal(as(sum1, "numeric"), as(sum2, "numeric"))
  expect_equal(as(sum1, "numeric"), c(1,0,3,4,4))
})

test_that("subtraction (- and sparse_sub)", {
  diff1 <- sparse_sub(x, y)
  diff2 <- x - y
  expect_s4_class(diff1, "sparse_numeric")
  expect_equal(as(diff1, "numeric"), as(diff2, "numeric"))
  expect_equal(as(diff1, "numeric"), c(-1,0,3,-4,0))
})

test_that("multiplication (* and sparse_mult)", {
  prod1 <- sparse_mult(x, y)
  prod2 <- x * y
  expect_s4_class(prod1, "sparse_numeric")
  expect_equal(as(prod1, "numeric"), as(prod2, "numeric"))
  expect_equal(as(prod1, "numeric"), c(0,0,0,0,4))
})

test_that("crossprod", {
  cp <- sparse_crossprod(x, y)
  expect_equal(cp, sum(c(3*0, 2*2))) # 4
})

# HW6: Mean, Norm, Standardize
test_that("mean method", {
  m <- mean(x)
  expect_equal(m, sum(x@value)/x@length)
})

test_that("norm method", {
  n <- norm(x)
  expect_equal(n, sqrt(sum(x@value^2)))
})

test_that("standardize method", {
  z <- standardize(x)
  expect_s4_class(z, "sparse_numeric")
  mu <- sum(x@value) / x@length
  var <- sum(x@value^2) / x@length - mu^2
  vals <- (x@value - mu) / sqrt(var)
  expect_equal(as(z, "numeric")[x@pos], vals)
})


test_that("standardize constant vector throws error", {
  expect_error({
    z <- standardize(as(c(0,0,0), "sparse_numeric"))
  })
})

# HW5: Show, plot
test_that("show method", {
  expect_output(show(x), "sparse_numeric object")
})

test_that("plot method with overlap", {
  expect_silent(plot(x, y))
})

test_that("plot method with no overlap", {
  a <- as(c(1,0,0), "sparse_numeric")
  b <- as(c(0,2,0), "sparse_numeric")
  expect_silent(plot(a, b))
})

# Other
test_that("addition with zero-length sparse vectors", {
  a <- as(c(0,0,0), "sparse_numeric")
  b <- as(c(0,0,0), "sparse_numeric")
  res <- sparse_add(a, b)
  expect_equal(as(res,"numeric"), c(0,0,0))
})

test_that("arithmetic with mismatched lengths throws error", {
  a <- as(c(1,0), "sparse_numeric")
  b <- as(c(1,0,0), "sparse_numeric")
  expect_error(sparse_add(a,b))
  expect_error(sparse_sub(a,b))
  expect_error(sparse_mult(a,b))
})

