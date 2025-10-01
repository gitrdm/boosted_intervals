.resolve_length <- function(len_a, len_b, zero_ok = FALSE) {
  if (!zero_ok && (len_a == 0L || len_b == 0L)) {
    stop("Intervals cannot be recycled against zero-length vectors", call. = FALSE)
  }
  if (len_a == len_b) {
    return(len_a)
  }
  if (len_a == 1L) {
    return(len_b)
  }
  if (len_b == 1L) {
    return(len_a)
  }
  stop(sprintf("Length mismatch: %d vs %d", len_a, len_b), call. = FALSE)
}
# Internal constructor --------------------------------------------------------

.new_units_interval <- function(lower, upper, validate = TRUE) {
  interval <- structure(
    list(lower = lower, upper = upper),
    class = c("units_interval", "list")
  )
  if (validate) {
    interval <- .validate_units_interval(interval)
  }
  interval
}

.validate_units_interval <- function(x) {
  stopifnot(inherits(x, "units_interval"))
  if (!inherits(x$lower, "units") || !inherits(x$upper, "units")) {
    stop("Interval bounds must carry units. Use units::set_units before construction.", call. = FALSE)
  }
  lower_len <- length(x$lower)
  upper_len <- length(x$upper)
  if (lower_len != upper_len) {
    stop(sprintf("Lower and upper bounds must have the same length (%d vs %d)", lower_len, upper_len), call. = FALSE)
  }
  base_unit <- units(x$lower)
  comparable_upper <- tryCatch(
    units::set_units(x$upper, base_unit, mode = "standard"),
    error = function(e) stop("Lower and upper bounds must use compatible units.", call. = FALSE)
  )
  comparable_lower <- units::set_units(x$lower, base_unit, mode = "standard")
  too_small <- comparable_lower > comparable_upper
  if (any(too_small, na.rm = TRUE)) {
    bad <- which(too_small)[1]
    stop(sprintf("Lower bound exceeds upper bound for element %d", bad), call. = FALSE)
  }
  x
}

.ensure_units <- function(x, unit, arg_name) {
  if (inherits(x, "units")) {
    return(units::set_units(x, unit, mode = "standard"))
  }
  if (is.numeric(x)) {
    if (is.null(unit)) {
      stop(sprintf("Argument '%s' is numeric but no unit was supplied", arg_name), call. = FALSE)
    }
    return(units::set_units(x, unit, mode = "standard"))
  }
  stop(sprintf("Argument '%s' must be numeric or a units object", arg_name), call. = FALSE)
}

.interval_unit <- function(x) {
  units::deparse_unit(x$lower)
}

.resolve_length <- function(len_a, len_b) {
  if (len_a == len_b) {
    return(len_a)
  }
  if (len_a == 1L) {
    return(len_b)
  }
  if (len_b == 1L) {
    return(len_a)
  }
  stop(sprintf("Length mismatch: %d vs %d", len_a, len_b), call. = FALSE)
}

.drop_units <- function(x, unit = NULL) {
  if (!is.null(unit)) {
    lower <- units::set_units(x$lower, unit, mode = "standard")
    upper <- units::set_units(x$upper, unit, mode = "standard")
  } else {
    lower <- x$lower
    upper <- x$upper
  }
  list(
    lower = units::drop_units(lower),
    upper = units::drop_units(upper)
  )
}

.recycle_bounds <- function(x, size) {
  lower <- rep(x$lower, length.out = size)
  upper <- rep(x$upper, length.out = size)
  .new_units_interval(lower, upper)
}

.resolve_interval_spec <- function(unit = NULL, length = NULL, like = NULL, allow_zero = TRUE) {
  if (!is.null(like)) {
    if (!inherits(like, "units_interval")) {
      stop("Argument 'like' must be a units_interval when provided.", call. = FALSE)
    }
    base_unit <- units(like$lower)
    target_length <- length(like$lower)
    return(list(unit = base_unit, length = target_length))
  }

  if (is.null(unit)) {
    stop("Supply either 'unit' or an interval via 'like'.", call. = FALSE)
  }

  base_unit <- if (inherits(unit, "units")) unit else units::as_units(unit)

  if (is.null(length)) {
    target_length <- 1L
  } else {
    if (!is.numeric(length) || length < 0 || (length %% 1) != 0) {
      stop("Argument 'length' must be a non-negative integer.", call. = FALSE)
    }
    if (!allow_zero && length == 0) {
      stop("Argument 'length' must be positive.", call. = FALSE)
    }
    target_length <- as.integer(length)
  }

  list(unit = base_unit, length = target_length)
}

#' Create a unit-aware interval
#'
#' Construct a vector of closed intervals with explicit units. Inputs may be
#' numeric or `units` objects. When supplying numeric bounds a `unit` argument
#' must be provided so the interval can be expressed in physical units.
#'
#' @param lower,upper Vectors containing the lower and upper bounds. These may
#'   be numeric or `units` objects. When numeric, both must share the same
#'   length and a `unit` must be provided.
#' @param unit Optional unit specification (character string or `units` object).
#'   Required whenever either bound is numeric. When both bounds supply units
#'   the `unit` argument is ignored but can still be used to force conversion.
#' @return A `units_interval` vector.
#' @section Usage Patterns:
#' 
#' **From numeric bounds:**
#' ```
#' units_interval(0, 10, unit = "meters")
#' units_interval(c(0, 5), c(10, 15), unit = "seconds")
#' ```
#' 
#' **From units objects:**
#' ```
#' units_interval(set_units(0, "m"), set_units(10, "m"))
#' units_interval(set_units(c(0, 5), "s"), set_units(c(10, 15), "s"))
#' ```
#' 
#' **Unit conversion:**
#' ```
#' units_interval(set_units(0, "m"), set_units(1000, "mm"), unit = "m")
#' ```
#' @section Error Handling:
#' - Bounds must have compatible units
#' - Lower bound cannot exceed upper bound
#' - Numeric inputs require explicit units
#' @importFrom units set_units drop_units as_units deparse_unit
#' @examples
#' library(units)
#' i1 <- units_interval(set_units(c(0, 1), "m"), set_units(c(1, 2), "m"))
#' i2 <- units_interval(0, 5, unit = "s")
#' i3 <- units_interval(set_units(0, "m"), set_units(100, "cm"))
#' @export
units_interval <- function(lower, upper, unit = NULL) {
  if (!is.null(unit) && !inherits(unit, "units")) {
    unit <- units::as_units(unit)
  }

  if (!inherits(lower, "units") && !inherits(upper, "units") && is.null(unit)) {
    stop("A unit must be supplied when both bounds are numeric.", call. = FALSE)
  }

  if (inherits(lower, "units") && inherits(upper, "units")) {
    base_unit <- units(lower)
    upper <- tryCatch(
      units::set_units(upper, base_unit, mode = "standard"),
      error = function(e) stop("Lower and upper bounds use incompatible units.", call. = FALSE)
    )
    if (!is.null(unit)) {
      lower <- units::set_units(lower, unit, mode = "standard")
      upper <- units::set_units(upper, unit, mode = "standard")
    }
  } else {
    lower <- .ensure_units(lower, unit, "lower")
    upper <- .ensure_units(upper, units(lower), "upper")
  }

  .new_units_interval(lower, upper)
}

#' Coerce objects to unit intervals
#'
#' @param x Object to coerce. Supported inputs are numeric scalars/vectors,
#'   `units` objects, and existing `units_interval` vectors.
#' @param unit Optional fallback unit used when `x` is numeric. Ignored for
#'   existing intervals or `units` objects.
#' @return A `units_interval` object.
#' @examples
#' library(units)
#' as_units_interval(5, unit = "m")
#' as_units_interval(set_units(1:3, "s"))
#' @export
as_units_interval <- function(x, unit = NULL) {
  if (inherits(x, "units_interval")) {
    return(x)
  }
  if (inherits(x, "units")) {
    return(units_interval(x, x))
  }
  if (is.numeric(x)) {
    return(units_interval(x, x, unit = unit))
  }
  stop("Cannot coerce object of class '", paste(class(x), collapse = ","), "' to a units_interval.", call. = FALSE)
}

#' Create a whole (unbounded) interval
#'
#' Constructs a `units_interval` vector where all elements span the entire real line (i.e., `[-Inf, Inf]`).
#' This is useful for representing maximal uncertainty or as a starting point for algorithms that iteratively contract intervals.
#'
#' @param unit Unit to attach to the interval. Accepts a character string or a `units` object. Ignored when `like` is supplied.
#' @param length Desired length of the resulting interval vector. Defaults to 1 when `like` is not provided.
#' @param like Optional `units_interval` whose units (and length) should be reused. When supplied, `unit` and `length` are ignored.
#' @return A `units_interval` vector containing only whole intervals (all bounds infinite).
#' @seealso [empty_interval()]
#' @export
whole_interval <- function(unit = NULL, length = NULL, like = NULL) {
  spec <- .resolve_interval_spec(unit = unit, length = length, like = like)
  lower <- units::set_units(rep(-Inf, spec$length), spec$unit, mode = "standard")
  upper <- units::set_units(rep(Inf, spec$length), spec$unit, mode = "standard")
  .new_units_interval(lower, upper)
}

#' Length of an interval vector
#'
#' @param x A `units_interval` object.
#' @return Integer length.
#' @export
length.units_interval <- function(x) {
  length(x$lower)
}

#' Subset an interval vector
#'
#' @param x A `units_interval` object.
#' @param i,j,drop Subsetting arguments.
#' @param ... Ignored.
#' @export
`[.units_interval` <- function(x, i, j, ..., drop = FALSE) {
  if (!missing(j)) {
    stop("Two-dimensional subsetting is not supported for intervals.", call. = FALSE)
  }
  if (drop) {
    stop("Dropping dimensions is not supported for intervals.", call. = FALSE)
  }
  .new_units_interval(x$lower[i], x$upper[i])
}

#' @export
`$.units_interval` <- function(x, name) {
  if (missing(name)) {
    stop("Must supply a component name", call. = FALSE)
  }
  if (!name %in% names(x)) {
    stop(sprintf("Component '%s' does not exist", name), call. = FALSE)
  }
  x[[name]]
}

#' Combine interval vectors
#'
#' @param ... One or more `units_interval` objects.
#' @export
c.units_interval <- function(...) {
  intervals <- list(...)
  if (length(intervals) == 1) {
    return(intervals[[1]])
  }
  units_list <- lapply(intervals, function(x) units::deparse_unit(x$lower))
  base_unit <- units_list[[1]]
  converted <- lapply(intervals, convert_units, unit = base_unit)
  lower <- do.call(c, lapply(converted, `[[`, "lower"))
  upper <- do.call(c, lapply(converted, `[[`, "upper"))
  .new_units_interval(lower, upper)
}

#' Repeat units_interval vectors
#'
#' Repeats the elements of a `units_interval` vector, similar to [base::rep()].
#'
#' @param x A `units_interval` object.
#' @param ... Arguments passed to [base::rep()].
#' @return A repeated `units_interval` vector.
#' @export
rep.units_interval <- function(x, ...) {
  .new_units_interval(rep(x$lower, ...), rep(x$upper, ...))
}

#' Format intervals for display
#'
#' @param x A `units_interval` object.
#' @param ... Additional arguments (unused).
#' @return A character vector.
#' @export
format.units_interval <- function(x, ...) {
  paste0("[", format(x$lower, ...), ", ", format(x$upper, ...), "]")
}

#' Print intervals
#'
#' @param x A `units_interval` object.
#' @param ... Additional arguments passed to [format.units_interval()].
#' @export
print.units_interval <- function(x, ...) {
  out <- format(x, ...)
  cat(out, sep = "\n")
  invisible(x)
}

#' Convert interval units
#'
#' @param x A `units_interval` object.
#' @param unit Target unit (character string or `units` object).
#' @param mode Passed to [units::set_units()].
#' @return A `units_interval` expressed in `unit`.
#' @examples
#' library(units)
#' x <- units_interval(set_units(0, "m"), set_units(2, "m"))
#' convert_units(x, "cm")
#' @export
convert_units <- function(x, unit, mode = c("standard", "mixed")) {
  UseMethod("convert_units")
}

#' @rdname convert_units
#' @export
convert_units.units_interval <- function(x, unit, mode = c("standard", "mixed")) {
  mode <- match.arg(mode)
  if (!inherits(unit, "units")) {
    unit <- units::as_units(unit)
  }
  lower <- units::set_units(x$lower, unit, mode = mode)
  upper <- units::set_units(x$upper, unit, mode = mode)
  .new_units_interval(lower, upper)
}

#' Extract lower bounds
#'
#' @param x A `units_interval` object.
#' @return A `units` vector of lower bounds.
#' @export
lower_bounds <- function(x) {
  stopifnot(inherits(x, "units_interval"))
  x$lower
}

#' Extract upper bounds
#'
#' @param x A `units_interval` object.
#' @return A `units` vector of upper bounds.
#' @export
upper_bounds <- function(x) {
  stopifnot(inherits(x, "units_interval"))
  x$upper
}

#' Numeric interval bounds
#'
#' Emit the lower and upper endpoints of an interval as a numeric matrix while
#' optionally converting to a requested unit. This helper is convenient when
#' interfacing with APIs that expect plain doubles instead of unit-aware
#' vectors.
#'
#' @param x A `units_interval` object.
#' @param unit Optional target unit (character string or `units` object). When
#'   omitted the interval's native unit is preserved before dropping metadata.
#' @return A numeric matrix with columns `lower` and `upper`.
#' @examples
#' library(units)
#' rng <- units_interval(set_units(0, "m"), set_units(1, "m"))
#' as_numeric_bounds(rng)
#' as_numeric_bounds(rng, unit = "cm")
#' @export
as_numeric_bounds <- function(x, unit = NULL) {
  stopifnot(inherits(x, "units_interval"))
  target_unit <- if (is.null(unit)) {
    units(x$lower)
  } else if (inherits(unit, "units")) {
    unit
  } else {
    units::as_units(unit)
  }

  lower <- units::set_units(x$lower, target_unit, mode = "standard")
  upper <- units::set_units(x$upper, target_unit, mode = "standard")
  cbind(
    lower = units::drop_units(lower),
    upper = units::drop_units(upper)
  )
}

#' Create an empty interval
#'
#' Constructs a `units_interval` vector where all elements are empty (i.e., both bounds are `NA`).
#' This is useful for representing missing or undefined measurements, or as a seed for algorithms that refine intervals iteratively.
#'
#' @param unit Unit to attach to the interval. Accepts a character string or a `units` object. Ignored when `like` is supplied.
#' @param length Desired length of the resulting interval vector. Defaults to 1 when `like` is not provided.
#' @param like Optional `units_interval` whose units (and length) should be reused. When supplied, `unit` and `length` are ignored.
#' @return A `units_interval` vector containing only empty intervals (all `NA`).
#' @seealso [whole_interval()]
#' @export
empty_interval <- function(unit = NULL, length = NULL, like = NULL) {
  spec <- .resolve_interval_spec(unit = unit, length = length, like = like)
  lower <- units::set_units(rep(NA_real_, spec$length), spec$unit, mode = "standard")
  upper <- units::set_units(rep(NA_real_, spec$length), spec$unit, mode = "standard")
  .new_units_interval(lower, upper)
}