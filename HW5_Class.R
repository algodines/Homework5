## HW5 Class/Methods

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

# Validity Test return TRUE or a character vector of errors
setValidity("sparse_numeric", function(object) {
  errs <- character()
  
  if (length(object@length) != 1L)
    errs <- c(errs, "'length' must be a single integer scalar.")
  if (!is.numeric(object@value))
    errs <- c(errs, "'value' must be numeric.")
  if (!is.integer(object@pos))
    errs <- c(errs, "'pos' must be integer.")
  if (length(object@value) != length(object@pos))
    errs <- c(errs, "'value' and 'pos' must have the same length.")
  
  # len constraints
  if (length(object@length) == 1L) {
    if (is.na(object@length) || object@length < 0L)
      errs <- c(errs, "'length' must be a non-negative integer.")
  }
  
  n <- if (length(object@length) == 1L) as.integer(object@length) else NA_integer_
  
  #position
  if (length(object@pos)) {
    if (any(is.na(object@pos)))
      errs <- c(errs, "'pos' cannot contain NA.")
    if (!is.na(n) && (any(object@pos < 1L) || any(object@pos > n)))
      errs <- c(errs, "'pos' must be within [1, length].")
    if (!isTRUE(all(diff(object@pos) > 0L)))
      errs <- c(errs, "'pos' must be strictly increasing (sorted, unique).")
  }
  
  # values
  if (length(object@value)) {
    if (any(!is.finite(object@value)))
      errs <- c(errs, "'value' must be finite (no NA/Inf).")
    if (any(object@value == 0))
      errs <- c(errs, "'value' must not contain zeros; omit zero entries.")
  }
  
  if (length(errs)) errs else TRUE
})

# leng
.check_same_len <- function(x, y) {
  if (x@length != y@length)
    stop("Operands must have the same 'length'.")
}

# merge add/sub
.merge_sum <- function(px, vx, py, vy, subtract = FALSE) {
  i <- 1L; j <- 1L
  nx <- length(px); ny <- length(py)
  out_pos <- integer(0L); out_val <- numeric(0L)
  
  while (i <= nx || j <= ny) {
    if (i <= nx && (j > ny || px[i] < py[j])) {
      pos <- px[i]; val <- vx[i]
      i <- i + 1L
    } else if (j <= ny && (i > nx || py[j] < px[i])) {
      pos <- py[j]; val <- if (subtract) -vy[j] else vy[j]
      j <- j + 1L
    } else { ## px[i] == py[j]
      pos <- px[i]
      val <- vx[i] + if (subtract) -vy[j] else vy[j]
      i <- i + 1L; j <- j + 1L
    }
    if (val != 0) { # keep  nonzeros
      out_pos <- c(out_pos, pos)
      out_val <- c(out_val, val)
    }
  }
  
  list(pos = out_pos, val = out_val)
}

#  product on overlap
.merge_prod <- function(px, vx, py, vy) {
  i <- 1L; j <- 1L
  nx <- length(px); ny <- length(py)
  out_pos <- integer(0L); out_val <- numeric(0L)
  
  while (i <= nx && j <= ny) {
    if (px[i] == py[j]) {
      val <- vx[i] * vy[j]
      if (val != 0) {
        out_pos <- c(out_pos, px[i])
        out_val <- c(out_val, val)
      }
      i <- i + 1L; j <- j + 1L
    } else if (px[i] < py[j]) {
      i <- i + 1L
    } else {
      j <- j + 1L
    }
  }
  list(pos = out_pos, val = out_val)
}

# overlap
.merge_dot <- function(px, vx, py, vy) {
  i <- 1L; j <- 1L
  nx <- length(px); ny <- length(py)
  s <- 0.0
  while (i <= nx && j <= ny) {
    if (px[i] == py[j]) {
      s <- s + vx[i] * vy[j]
      i <- i + 1L; j <- j + 1L
    } else if (px[i] < py[j]) {
      i <- i + 1L
    } else {
      j <- j + 1L
    }
  }
  s
}

.make_sparse <- function(pos, val, n) {
  if (length(pos)) {
    o <- order(pos)
    pos <- as.integer(pos[o])
    val <- val[o]
  } else {
    pos <- integer()
    val <- numeric()
  }
  new("sparse_numeric", value = val, pos = pos, length = as.integer(n))
}

# Coercions

#  to sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  if (length(nz)) {
    .make_sparse(pos = nz, val = from[nz], n = length(from))
  } else {
    new("sparse_numeric", value = numeric(), pos = integer(), length = as.integer(length(from)))
  }
})

##  to numeric
setAs("sparse_numeric", "numeric", function(from) {
  n <- as.integer(from@length)
  out <- numeric(n)
  if (length(from@pos))
    out[from@pos] <- from@value
  out
})



setGeneric("sparse_add",  function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub",  function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# methods

setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            m <- .merge_sum(x@pos, x@value, y@pos, y@value, subtract = FALSE)
            .make_sparse(m$pos, m$val, x@length)
          }
)

setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            m <- .merge_sum(x@pos, x@value, y@pos, y@value, subtract = TRUE)
            .make_sparse(m$pos, m$val, x@length)
          }
)

setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            m <- .merge_prod(x@pos, x@value, y@pos, y@value)
            .make_sparse(m$pos, m$val, x@length)
          }
)

setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            .merge_dot(x@pos, x@value, y@pos, y@value)
          }
)


#  methods (+, -, *)
setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2)
)

setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2)
)

setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2)
)

# show() method
setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("An object of class 'sparse_numeric'\n")
  cat("  length:", as.integer(object@length), "  nnz:", nnz, "\n", sep = "")
  if (nnz) {
    k <- min(nnz, 10L)
    cat("  first nonzeros (pos:value):\n")
    for (i in seq_len(k)) {
      cat("   ", object@pos[i], ":", format(object@value[i]), "\n", sep = "")
    }
    if (nnz > k) cat("   ...\n")
  }
})

# plot(x, y)
setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            overlap <- intersect(x@pos, y@pos)
            n <- as.integer(x@length)
            if (length(overlap) == 0L) {
              plot(NA, xlim = c(1, n), ylim = c(0, 1), xlab = "index", ylab = "overlap",
                   main = "No overlapping nonzero elements")
              return(invisible(NULL))
            }
            plot(overlap, rep(1, length(overlap)),
                 xlim = c(1, n), ylim = c(0, 1.1),
                 xlab = "index", ylab = "overlap (y=1)",
                 main = "Overlapping nonzero indices")
            abline(h = 1, lty = 3)
            invisible(NULL)
          }
)

setMethod("length", "sparse_numeric", function(x) as.integer(x@length))

