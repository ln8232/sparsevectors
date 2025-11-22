#' @import methods
#' @importFrom stats setNames
NULL

## HW5 Class/Methods

#--- Class ---#

#' Sparse Numeric Vector Class
#'
#' An S4 class to represent numeric vectors that are mostly zero.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions corresponding to `value`.
#' @slot length Integer scalar specifying the total length of the vector.
#'
#' @exportClass sparse_numeric
setClass(
    Class = 'sparse_numeric',
    slots = c(
        value = 'numeric',
        pos = 'integer',
        length = 'integer'
    )
)

#--- Validity ---#
setValidity(
    Class = 'sparse_numeric',
    method = function(object) {
        # Check for NA in value and pos vectors
        if (anyNA(object@value)) return('Elements in value cannot be NA')
        if (anyNA(object@pos)) return('Elements in pos cannot be NA')
        # Check if pos only contain positive integers
        if (any(object@pos <= 0)) return('Elements in pos must be positive integers')
        # Check if length is a single positive integer
        if (length(object@length) != 1 || object@length <= 0) return('Length must be a single positive integer')
        # Check if length of value and pos are the same
        if (length(object@value) != length(object@pos)) return('val and pos must contain the same number of elements')
        # Check if elements in pos are unique (indices)
        if (anyDuplicated(object@pos)) return('pos must not contain duplicate elements')
        # Check if element in pos is within length
        if (any(object@pos > object@length)) return('Elements in pos cannot be greater than length')
        TRUE
    }
)

#--- Generic functions ---#

#' Addition of Sparse Numeric Vectors
#'
#' Computes element-wise addition of two `sparse_numeric` vectors.
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object of the same length as `x`.
#' @param ... Additional arguments.
#' @return A `sparse_numeric` object representing the element-wise sum of `x` and `y`.
#' @examples
#' x <- new("sparse_numeric", value = c(1, 2), pos = c(2L, 5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3, 4), pos = c(1L, 5L), length = 6L)
#' sparse_add(x, y)
#' @export
setGeneric(
    name = 'sparse_add',
    def = function(x, y, ...) {
        standardGeneric('sparse_add')
    }
)

#' Subtraction of Sparse Numeric Vectors
#'
#' Computes element-wise subtraction of two `sparse_numeric` vectors.
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object of the same length as `x`.
#' @param ... Additional arguments.
#' @return A `sparse_numeric` object representing the element-wise difference of `x` and `y`.
#' @examples
#' x <- new("sparse_numeric", value = c(1,2), pos = c(2L,5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3,4), pos = c(1L,5L), length = 6L)
#' sparse_sub(x, y)
#' @export
setGeneric(
    name = 'sparse_sub',
    def = function(x, y, ...) {
        standardGeneric('sparse_sub')
    }
)

#' Multiplication of Sparse Numeric Vectors
#'
#' Computes element-wise multiplication of two `sparse_numeric` vectors.
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object of the same length as `x`.
#' @param ... Additional arguments.
#' @return A `sparse_numeric` object representing the element-wise product of `x` and `y`.
#' @examples
#' x <- new("sparse_numeric", value = c(1,2), pos = c(2L,5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3,4), pos = c(1L,5L), length = 6L)
#' sparse_mult(x, y)
#' @export
setGeneric(
    name = 'sparse_mult',
    def = function(x, y, ...) {
        standardGeneric('sparse_mult')
    }
)

#' Cross Product of Sparse Numeric Vectors
#'
#' Computes the cross product of two `sparse_numeric` vectors.
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object of the same length as `x`.
#' @param ... Additional arguments (not used).
#' @return A numeric scalar representing the cross product of `x` and `y`.
#' @examples
#' x <- new("sparse_numeric", value = c(1,2), pos = c(2L,5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3,4), pos = c(1L,5L), length = 6L)
#' sparse_crossprod(x, y)
#' @export
setGeneric(
    name = 'sparse_crossprod',
    def = function(x, y, ...) {
        standardGeneric('sparse_crossprod')
    }
)

#--- Set Methods ---#

#' @rdname sparse_add
#' @export
setMethod(
    f = 'sparse_add',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        pos <- sort(unique(c(x@pos, y@pos)))
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        res <- numeric(length(pos))
        names(res) <- pos
        inX <- names(res) %in% names(xVals)
        inY <- names(res) %in% names(yVals)
        res[inX] <- xVals[names(res)[inX]]
        res[inY] <- res[inY] + yVals[names(res)[inY]]
        temp <- res != 0
        new(
            Class = 'sparse_numeric',
            value = as.numeric(res[temp]),
            pos = as.integer(names(res)[temp]),
            length = x@length
        )
    }
)

#' Addition of Sparse Numeric Vectors (Operator)
#'
#' Computes element-wise addition of two `sparse_numeric` objects using the `+` operator.
#'
#' @param e1 A `sparse_numeric` object.
#' @param e2 A `sparse_numeric` object of the same length as `e1`.
#' @return A `sparse_numeric` object representing the element-wise sum.
#' @examples
#' x <- new("sparse_numeric", value = c(1, 2), pos = c(2L, 5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3, 4), pos = c(1L, 5L), length = 6L)
#' x + y
#' @exportMethod +
setMethod(
    f = '+',
    signature = c(e1 = 'sparse_numeric', e2 = 'sparse_numeric'),
    definition = function(e1, e2) {
        sparse_add(e1, e2)
    }
)

#' @rdname sparse_sub
#' @export
setMethod(
    f = 'sparse_sub',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        pos <- sort(unique(c(x@pos, y@pos)))
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        res <- numeric(length(pos))
        names(res) <- pos
        inX <- names(res) %in% names(xVals)
        inY <- names(res) %in% names(yVals)
        res[inX] <- xVals[names(res)[inX]]
        res[inY] <- res[inY] - yVals[names(res)[inY]]
        temp <- res != 0
        new(
            Class = 'sparse_numeric',
            value = as.numeric(res[temp]),
            pos = as.integer(names(res)[temp]),
            length = x@length
        )
    }
)

#' Subtraction of Sparse Numeric Vectors (Operator)
#'
#' Computes element-wise subtraction of two `sparse_numeric` objects using the `-` operator.
#'
#' @param e1 A `sparse_numeric` object.
#' @param e2 A `sparse_numeric` object of the same length as `e1`.
#' @return A `sparse_numeric` object representing the element-wise difference.
#' @examples
#' x <- new("sparse_numeric", value = c(1,2), pos = c(2L,5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3,4), pos = c(1L,5L), length = 6L)
#' x - y
#' @exportMethod -
setMethod(
    f = '-',
    signature = c(e1 = 'sparse_numeric', e2 = 'sparse_numeric'),
    definition = function(e1, e2) {
        sparse_sub(e1, e2)
    }
)

#' @rdname sparse_mult
#' @export
setMethod(
    f = 'sparse_mult',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        pos <- intersect(x@pos, y@pos)
        if (length(pos) == 0) {
            return(new('sparse_numeric', value = numeric(0), pos = integer(0), length = x@length))
        }
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        res <- xVals[as.character(pos)] * yVals[as.character(pos)]
        temp <- res != 0
        new(
            Class = 'sparse_numeric',
            value = as.numeric(res[temp]),
            pos = as.integer(names(res)[temp]),
            length = x@length
        )
    }
)

#' Multiplication of Sparse Numeric Vectors (Operator)
#'
#' Computes element-wise multiplication of two `sparse_numeric` objects using the `*` operator.
#'
#' @param e1 A `sparse_numeric` object.
#' @param e2 A `sparse_numeric` object of the same length as `e1`.
#' @return A `sparse_numeric` object representing the element-wise product.
#' @examples
#' x <- new("sparse_numeric", value = c(1,2), pos = c(2L,5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3,4), pos = c(1L,5L), length = 6L)
#' x * y
#' @exportMethod *
setMethod(
    f = '*',
    signature = c(e1 = 'sparse_numeric', e2 = 'sparse_numeric'),
    definition = function(e1, e2) {
        sparse_mult(e1, e2)
    }
)

#' @rdname sparse_crossprod
#' @export
setMethod(
    f = 'sparse_crossprod',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        # Check if length of vectors are the same
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        # Setup
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        # Compute cross product
        sum(xVals[intersect(names(xVals), names(yVals))] * yVals[intersect(names(xVals), names(yVals))])
    }
)

#' Show a Sparse Numeric Vector
#'
#' Displays a `sparse_numeric` object in a readable form, showing its length
#' and all non-zero elements with their positions.
#'
#' @param object A `sparse_numeric` object.
#' @return `NULL`. Prints information to the console.
#' @examples
#' x <- new("sparse_numeric", value = c(1,2), pos = c(2L,5L), length = 6L)
#' x  # Calls show()
#' @exportMethod show
setMethod(
    f = 'show',
    signature = 'sparse_numeric',
    definition = function(object) {
        cat('sparse_numeric object of length', object@length, '\n')
        cat('Non-zero elements:\n')
        print(data.frame(pos = object@pos, value = object@value))
    }
)

#' Plot Two Sparse Numeric Vectors
#'
#' Plots the overlapping non-zero elements of two `sparse_numeric` vectors.
#'
#' @param x A `sparse_numeric` object.
#' @param y A `sparse_numeric` object.
#' @param ... Additional arguments.
#' @return `NULL`. Produces a scatter plot of overlapping non-zero elements.
#' @examples
#' x <- new("sparse_numeric", value = c(1,2), pos = c(2L,5L), length = 6L)
#' y <- new("sparse_numeric", value = c(3,4), pos = c(1L,5L), length = 6L)
#' plot(x, y)
#' @exportMethod plot
setMethod(
    f = 'plot',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        pos <- intersect(x@pos, y@pos)
        if (length(pos) == 0) {
            plot(0, 0, type = 'n',
                 xlab = 'Sparse Vector 1',
                 ylab = 'Sparse Vector 2',
                 main = 'Overlapping non-zero elements')
            return(invisible(NULL))
        }
        xvals <- x@value[match(pos, x@pos)]
        yvals <- y@value[match(pos, y@pos)]
        plot(
            xvals, yvals,
            xlab = 'Sparse Vector 1',
            ylab = 'Sparse Vector 2',
            main = 'Overlapping non-zero elements',
            ...
        )
    }
)

#--- Coercions ---#

setAs('numeric', 'sparse_numeric', function(from) {
    new(
        'sparse_numeric',
        value = from[from != 0],
        pos   = as.integer(which(from != 0)),
        length = as.integer(length(from))
    )
})

setAs('sparse_numeric', 'numeric', function(from) {
    out <- numeric(from@length)
    out[from@pos] <- from@value
    out
})

#--- Homework 6 ---#

#' Mean of a Sparse Numeric Vector
#'
#' Computes the mean of a `sparse_numeric` vector, including all the zeros.
#'
#' @param x A `sparse_numeric` object.
#' @param ... Additional arguments.
#' @return A numeric scalar representing the mean.
#' @examples
#' x <- new("sparse_numeric", value = c(1, 2), pos = c(2L, 5L), length = 6L)
#' mean(x)
#' @export
setMethod(
    f = 'mean',
    signature = 'sparse_numeric',
    definition = function(x, ...) {
        sum(x@value) / x@length
    }
)

#' Norm of a Sparse Numeric Vector
#'
#' Computes the squared norm of a `sparse_numeric` vector which is the square root of
#' the sum of the squared individual elements of the vector.
#'
#' @param x A `sparse_numeric` object.
#' @param ... Additional arguments.
#' @return A numeric scalar representing the squared norm.
#' @examples
#' x <- new("sparse_numeric", value = c(1, 2), pos = c(2L, 5L), length = 6L)
#' norm(x)
#' @export
setGeneric(
    name = 'norm',
    def = function(x, ...) {
        standardGeneric('norm')
    }
)

#' @rdname norm
#' @export
setMethod(
    f = 'norm',
    signature = 'sparse_numeric',
    definition = function(x, ...) {
        sqrt(sum(x@value^2))
    }
)

#' Standardization of a Sparse Numeric Vector
#'
#' Computes the standardization of a `sparse_numeric` vector which is done by taking
#' taking each element of the vector and subtracting off the vector mean and dividing
#' by the vector standard deviation.
#'
#' @param x A `sparse_numeric` object.
#' @param ... Additional arguments.
#' @return A `sparse_numeric` object with standardized values.
#' @examples
#' x <- new("sparse_numeric", value = c(1, 2), pos = c(2L, 5L), length = 6L)
#' z <- standardize(x)
#' as(z, "numeric")
#' @export
# Generic/method standardize()
setGeneric(
    name = 'standardize',
    def = function(x, ...) {
        standardGeneric('standardize')
    }
)

#' @rdname standardize
#' @export
setMethod(
    f = 'standardize',
    signature = 'sparse_numeric',
    definition = function(x, ...) {
        mu <- sum(x@value) / x@length
        sumsq <- sum(x@value^2)
        var <- sumsq / x@length - mu^2
        sd <- sqrt(var)
        if (sd == 0) stop('Cannot standardize a constant sparse vector')
        newVals <- (x@value - mu) / sd
        vals <- newVals != 0
        new(
            'sparse_numeric',
            value = newVals[vals],
            pos = x@pos[vals],
            length = x@length
        )
    }
)

