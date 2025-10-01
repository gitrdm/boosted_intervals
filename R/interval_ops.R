#' Arithmetic, power, and comparison for unit-aware intervals
#'
#' Operations reuse Boost interval arithmetic under the hood while ensuring
#' units remain consistent. Arithmetic results preserve or combine units as
#' appropriate. Comparison operations return logical vectors after aligning
#' operands to shared units. Integer powers are supported via the `^`
#' operator, provided the base does not span zero for negative exponents.
#'
#' @param e1,e2 Operands supplied by the group generic.
#' @return For arithmetic operators, a `units_interval`. For comparison
#'   operators, a logical vector.
#' @importFrom units set_units as_units deparse_unit drop_units
#' @export
Ops.units_interval <- function(e1, e2) {
  op <- tryCatch(.Generic, error = function(e) NULL)
  if (is.null(op)) {
    op_call <- sys.call(sys.parent())
    if (!is.null(op_call) && length(op_call) >= 1L) {
      op <- as.character(op_call[[1L]])
    }
  }
  if (is.null(op) || length(op) != 1L) {
    stop("Operator context unavailable for units_interval objects.", call. = FALSE)
  }

  # Unary operations ---------------------------------------------------------
  if (missing(e2)) {
    if (op == "+") {
      return(as_units_interval(e1))
    }
    if (op == "-") {
      interval <- as_units_interval(e1)
      numerics <- .drop_units(interval)
      res <- interval_negate(numerics$lower, numerics$upper)
      unit <- units(interval$lower)
      return(.new_units_interval(
        units::set_units(res$lower, unit, mode = "standard"),
        units::set_units(res$upper, unit, mode = "standard")
      ))
    }
    stop(sprintf("Unary operator '%s' is not supported for units_interval.", op), call. = FALSE)
  }

  # Binary arithmetic --------------------------------------------------------
  if (op %in% c("+", "-", "*", "/")) {
    return(.interval_arith(op, e1, e2))
  }

  if (op == "^") {
    if (!inherits(e1, "units_interval")) {
      stop("The left-hand side of '^' must be a units_interval", call. = FALSE)
    }
    if (inherits(e2, "units_interval")) {
      stop("Exponentiation by an interval is not supported", call. = FALSE)
    }
    return(.interval_pow(e1, e2))
  }

  # Binary comparison --------------------------------------------------------
  if (op %in% c("==", "!=", "<", "<=", ">", ">=")) {
    return(.interval_compare(op, e1, e2))
  }

  stop(sprintf("Operator '%s' is not supported for units_interval objects.", op), call. = FALSE)
}

.interval_arith <- function(op, e1, e2) {
  pair <- .coerce_pair_for_arith(op, e1, e2)
  x_unit <- units(pair$x$lower)
  y_unit <- units(pair$y$lower)

  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)

  res <- switch(
    op,
    "+" = interval_add(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    "-" = interval_subtract(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    "*" = interval_multiply(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    "/" = interval_divide(x_num$lower, x_num$upper, y_num$lower, y_num$upper)
  )

  result_unit <- switch(
    op,
    "+" = x_unit,
    "-" = x_unit,
    "*" = units(units::set_units(1, x_unit, mode = "standard") * units::set_units(1, y_unit, mode = "standard")),
    "/" = units(units::set_units(1, x_unit, mode = "standard") / units::set_units(1, y_unit, mode = "standard"))
  )

  .new_units_interval(
    units::set_units(res$lower, result_unit, mode = "standard"),
    units::set_units(res$upper, result_unit, mode = "standard")
  )
}

.interval_pow <- function(base, exponent) {
  unitless <- units::as_units(1)
  if (inherits(exponent, "units")) {
    exponent <- units::drop_units(units::set_units(exponent, unitless, mode = "standard"))
  }
  if (!is.numeric(exponent)) {
    stop("Exponent must be numeric or a dimensionless units vector", call. = FALSE)
  }
  if (length(exponent) != 1L) {
    if (!all(exponent == exponent[1])) {
      stop("Exponent recycling would lead to inconsistent units", call. = FALSE)
    }
    exponent <- exponent[1]
  }

  exponent_value <- exponent
  if (is.na(exponent_value)) {
    empty <- rep(NA_real_, length(base))
    base_unit <- units(base$lower)
    return(.new_units_interval(
      units::set_units(empty, base_unit, mode = "standard"),
      units::set_units(empty, base_unit, mode = "standard")
    ))
  }

  numerics <- .drop_units(base)
  base_unit_sample <- units::set_units(1, units(base$lower), mode = "standard")
  tol <- sqrt(.Machine$double.eps)
  is_integer_exp <- abs(exponent_value - round(exponent_value)) < tol

  if (is_integer_exp) {
    exponent_int <- as.integer(round(exponent_value))
    res <- interval_pow(numerics$lower, numerics$upper, exponent_int)
    result_unit_symbol <- units::deparse_unit(base_unit_sample^exponent_int)
  } else {
    if (exponent_value < 0) {
      zero_mask <- zero_in(base)
      if (any(zero_mask == TRUE, na.rm = TRUE)) {
        stop("Negative exponents are not defined for intervals spanning zero", call. = FALSE)
      }
    }
    if (any(numerics$lower < 0, na.rm = TRUE)) {
      stop("Fractional exponents require non-negative base intervals", call. = FALSE)
    }
    res <- interval_pow_numeric(numerics$lower, numerics$upper, exponent_value)
    result_unit_symbol <- tryCatch(
      units::deparse_unit(base_unit_sample^exponent_value),
      error = function(e) {
        stop(
          "Cannot raise units '", units::deparse_unit(base_unit_sample),
          "' to exponent ", exponent_value, "; integer exponents only for these units",
          call. = FALSE
        )
      }
    )
  }

  .new_units_interval(
    units::set_units(res$lower, result_unit_symbol, mode = "standard"),
    units::set_units(res$upper, result_unit_symbol, mode = "standard")
  )
}

.interval_compare <- function(op, e1, e2) {
  pair <- .coerce_pair_same_units(e1, e2)
  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)

  res <- switch(
    op,
    "==" = interval_equals(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    "!=" = !interval_equals(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    "<" = interval_less(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    "<=" = interval_less_equal(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    ">" = interval_greater(x_num$lower, x_num$upper, y_num$lower, y_num$upper),
    ">=" = interval_greater_equal(x_num$lower, x_num$upper, y_num$lower, y_num$upper)
  )

  as.vector(res)
}

.coerce_pair_for_arith <- function(op, e1, e2) {
  if (op %in% c("+", "-")) {
    base <- .base_unit_from_operands(e1, e2)
    x <- if (inherits(e1, "units_interval")) convert_units(e1, base) else convert_units(as_units_interval(e1, unit = base), base)
    y <- if (inherits(e2, "units_interval")) convert_units(e2, base) else as_units_interval(e2, unit = base)
  } else {
    dimless <- units::as_units(1)
    x <- if (inherits(e1, "units_interval")) e1 else as_units_interval(e1, unit = dimless)
    y <- if (inherits(e2, "units_interval")) e2 else as_units_interval(e2, unit = dimless)
  }

  len <- .resolve_length(length(x), length(y))
  if (length(x) != len) {
    x <- rep(x, length.out = len)
  }
  if (length(y) != len) {
    y <- rep(y, length.out = len)
  }
  list(x = x, y = y, len = len)
}

.coerce_pair_same_units <- function(e1, e2) {
  base <- .base_unit_from_operands(e1, e2)
  x <- if (inherits(e1, "units_interval")) convert_units(e1, base) else convert_units(as_units_interval(e1, unit = base), base)
  y <- if (inherits(e2, "units_interval")) convert_units(e2, base) else convert_units(as_units_interval(e2, unit = base), base)

  len <- .resolve_length(length(x), length(y))
  if (length(x) != len) {
    x <- rep(x, length.out = len)
  }
  if (length(y) != len) {
    y <- rep(y, length.out = len)
  }
  list(x = x, y = y, len = len)
}

.base_unit_from_operands <- function(e1, e2) {
  if (inherits(e1, "units_interval")) {
    units(e1$lower)
  } else if (inherits(e2, "units_interval")) {
    units(e2$lower)
  } else if (inherits(e1, "units")) {
    units(e1)
  } else if (inherits(e2, "units")) {
    units(e2)
  } else {
    stop("At least one operand must carry units information.", call. = FALSE)
  }
}

# Width and midpoint ---------------------------------------------------------

#' Interval width
#'
#' @param x A `units_interval` object.
#' @return A `units` vector containing interval widths.
#' @export
#' @keywords internal
width <- function(x) {
  UseMethod("width")
}

#' @rdname width
#' @export
width.units_interval <- function(x) {
  numerics <- .drop_units(x)
  res <- interval_width(numerics$lower, numerics$upper)
  units::set_units(res, units(x$lower), mode = "standard")
}

#' Interval midpoint
#'
#' @param x A `units_interval` object.
#' @return A `units` vector containing midpoints. Midpoints are `NA` where the
#'   interval is unbounded.
#' @export
#' @keywords internal
midpoint <- function(x) {
  UseMethod("midpoint")
}

#' @rdname midpoint
#' @export
midpoint.units_interval <- function(x) {
  numerics <- .drop_units(x)
  res <- interval_midpoint(numerics$lower, numerics$upper)
  units::set_units(res, units(x$lower), mode = "standard")
}

#' Interval median
#'
#' The median provides a deterministic representative point from each
#' interval, mirroring Boost's `median()` helper. Compared to
#' [midpoint()] it avoids overflow by applying directed rounding, making it
#' suitable for intervals with very large magnitudes.
#'
#' @inheritParams midpoint
#' @param na.rm Logical. Medians of interval vectors are computed element-wise,
#'   so `na.rm = TRUE` is not supported.
#' @param ... Ignored; present for compatibility with [stats::median()].
#' @return A `units` vector containing medians for each interval element.
#' @importFrom stats median
#' @export
median.units_interval <- function(x, na.rm = FALSE, ...) {
  if (!identical(na.rm, FALSE)) {
    stop("na.rm is not supported for units_interval medians", call. = FALSE)
  }
  numerics <- .drop_units(x)
  res <- interval_median(numerics$lower, numerics$upper)
  units::set_units(res, units(x$lower), mode = "standard")
}

#' Interval radius (semi-width)
#'
#' The radius (also called semi-width) of an interval is half of its width.
#' This is useful when propagating uncertainties or computing relative error
#' bands. Results retain the same units as the input interval.
#'
#' @param x A `units_interval` object.
#' @return A `units` vector containing interval radii.
#' @export
radius <- function(x) {
  UseMethod("radius")
}

#' @rdname radius
#' @export
radius.units_interval <- function(x) {
  numerics <- .drop_units(x)
  res <- interval_radius(numerics$lower, numerics$upper)
  units::set_units(res, units(x$lower), mode = "standard")
}

#' Interval magnitude and mignitude
#'
#' These helpers surface Boost's `mag()` and `mig()` utilities. `mag()` returns
#' the maximum absolute value attainable within the interval, while `mig()`
#' returns the minimum absolute value (zero if the interval spans zero).
#'
#' @inheritParams radius
#' @return A `units` vector.
#' @name magnitude-helpers
NULL

#' Interval norm
#'
#' Compute the supremum norm of a `units_interval`. For other objects this
#' function defers to [base::norm()] so existing matrix workflows continue to
#' operate as usual.
#'
#' @param x Object whose norm should be computed.
#' @param type Norm type (mirroring [base::norm()]). All supported options map
#'   to the interval sup norm when `x` is a `units_interval`.
#' @param ... Additional arguments passed along to [base::norm()].
#' @return For interval inputs, a `units` vector containing the element-wise
#'   norms. Otherwise, the result of [base::norm()].
#' @seealso [base::norm()]
#' @export
norm <- function(x, type = c("O", "I", "F", "M", "2"), ...) {
  if (inherits(x, "units_interval")) {
    return(.norm_units_interval(x, type))
  }
  base::norm(x, type = type, ...)
}

.norm_units_interval <- function(x, type) {
  if (length(type) > 1L) {
    type <- type[1L]
  }
  type <- match.arg(type, c("O", "I", "F", "M", "2", "max"))
  numerics <- .drop_units(x)
  res <- interval_norm(numerics$lower, numerics$upper)
  units::set_units(res, units(x$lower), mode = "standard")
}

#' @rdname magnitude-helpers
#' @export
mag <- function(x) {
  UseMethod("mag")
}

#' @rdname magnitude-helpers
#' @export
mag.units_interval <- function(x) {
  numerics <- .drop_units(x)
  res <- interval_mag(numerics$lower, numerics$upper)
  units::set_units(res, units(x$lower), mode = "standard")
}

#' @rdname magnitude-helpers
#' @export
mig <- function(x) {
  UseMethod("mig")
}

#' @rdname magnitude-helpers
#' @export
mig.units_interval <- function(x) {
  numerics <- .drop_units(x)
  res <- interval_mig(numerics$lower, numerics$upper)
  units::set_units(res, units(x$lower), mode = "standard")
}

#' Interval distance
#'
#' Computes the smallest distance between two intervals. Overlapping intervals
#' yield zero, while disjoint intervals return the gap between their hulls.
#'
#' @param x,y `units_interval` objects. Recycling rules match those of
#'   arithmetic operations.
#' @return A `units` vector describing the element-wise distance.
#' @export
distance <- function(x, y) {
  pair <- .coerce_pair_same_units(x, y)
  base_unit <- units(pair$x$lower)
  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)
  res <- interval_distance(x_num$lower, x_num$upper, y_num$lower, y_num$upper)
  units::set_units(res, base_unit, mode = "standard")
}

#' Interval subdivision and inflation helpers
#'
#' `bisect()` splits each interval at its median into left and right halves.
#' `inflate()` expands intervals symmetrically by an absolute margin and/or a
#' relative amount scaled by the interval magnitude. Both helpers preserve unit
#' metadata while delegating enclosure guarantees to Boost.
#'
#' @inheritParams midpoint
#' @param absolute Absolute widening applied symmetrically to each bound. Accepts
#'   numerics or `units` vectors convertible to the interval's unit. Defaults to
#'   zero, meaning no absolute expansion.
#' @param relative Relative widening factor scaled by the interval magnitude.
#'   Must be dimensionless and non-negative. Defaults to zero.
#' @return `bisect()` returns a list with `left` and `right` elements, each a
#'   `units_interval`. `inflate()` returns a `units_interval` whose bounds have
#'   been expanded outward.
#' @name interval-transformations
NULL

#' @rdname interval-transformations
#' @export
bisect <- function(x) {
  stopifnot(inherits(x, "units_interval"))
  numerics <- .drop_units(x)
  halves <- interval_bisect(numerics$lower, numerics$upper)
  unit <- units(x$lower)
  left <- .new_units_interval(
    units::set_units(halves$left_lower, unit, mode = "standard"),
    units::set_units(halves$left_upper, unit, mode = "standard")
  )
  right <- .new_units_interval(
    units::set_units(halves$right_lower, unit, mode = "standard"),
    units::set_units(halves$right_upper, unit, mode = "standard")
  )
  list(left = left, right = right)
}

#' @rdname interval-transformations
#' @export
inflate <- function(x, absolute = 0, relative = 0) {
  stopifnot(inherits(x, "units_interval"))

  len <- length(x)
  unit <- units(x$lower)

  absolute_units <- if (inherits(absolute, "units")) {
    units::set_units(absolute, unit, mode = "standard")
  } else {
    units::set_units(absolute, unit, mode = "standard")
  }
  absolute_numeric <- units::drop_units(rep(absolute_units, length.out = len))
  if (any(absolute_numeric < 0, na.rm = TRUE)) {
    stop("absolute inflation must be non-negative", call. = FALSE)
  }

  if (inherits(relative, "units")) {
    stop("relative inflation must be dimensionless", call. = FALSE)
  }
  relative_numeric <- rep(relative, length.out = len)
  if (any(relative_numeric < 0, na.rm = TRUE)) {
    stop("relative inflation must be non-negative", call. = FALSE)
  }

  magnitudes <- units::drop_units(mag(x))
  expansion <- absolute_numeric + relative_numeric * magnitudes

  numerics <- .drop_units(x)
  widened <- interval_widen(numerics$lower, numerics$upper, expansion)
  .new_units_interval(
    units::set_units(widened$lower, unit, mode = "standard"),
    units::set_units(widened$upper, unit, mode = "standard")
  )
}

#' Interval power helpers
#'
#' These helpers surface Boost's generalised power routines. `pow_interval()`
#' raises an interval to a (potentially non-integer) scalar exponent while
#' preserving enclosure guarantees. `pow_scalar_interval()` evaluates
#' dimensionless scalars raised to interval-valued exponents. `nth_root()`
#' extracts integer roots and `pow1p()` expands small offsets via
#' `pow(1 + x, p) - 1` with interval rounding.
#'
#' @inheritParams midpoint
#' @param exponent Numeric or dimensionless units exponent.
#' @param base Numeric or dimensionless units vector forming the scalar base in
#'   `pow_scalar_interval()`.
#' @param k Positive integer root degree.
#' @return Functions return `units_interval` vectors with appropriate units.
#' @name interval-powers
#'
#' @details
#' Note on fractional exponents: when supplying a fractional exponent using
#' the `^` infix operator, R's operator precedence can cause unexpected
#' results. For example, writing `x^1/3` is parsed as `(x^1) / 3`. Always
#' wrap fractional exponents in parentheses, e.g. `x^(1/3)`, or use a
#' numeric literal such as `0.3333333` to avoid this class of user error.
NULL

.dimensionless_numeric <- function(x, arg_name) {
  dimless <- units::as_units(1)
  if (inherits(x, "units")) {
    return(units::drop_units(units::set_units(x, dimless, mode = "standard")))
  }
  if (is.numeric(x)) {
    return(x)
  }
  stop(sprintf("Argument '%s' must be numeric or a dimensionless units vector", arg_name), call. = FALSE)
}

#' @rdname interval-powers
#' @export
pow_interval <- function(x, exponent) {
  stopifnot(inherits(x, "units_interval"))
  .interval_pow(x, exponent)
}

#' @rdname interval-powers
#' @export
pow_scalar_interval <- function(base, exponent) {
  stopifnot(inherits(exponent, "units_interval"))
  dimless <- units::as_units(1)
  exponent_dimless <- convert_units(exponent, dimless)
  exp_num <- .drop_units(exponent_dimless)

  if (inherits(base, "units")) {
    base_numeric <- units::drop_units(units::set_units(base, dimless, mode = "standard"))
  } else if (is.numeric(base)) {
    base_numeric <- base
  } else {
    stop("'base' must be numeric or a dimensionless units vector", call. = FALSE)
  }

  if (any(base_numeric <= 0, na.rm = TRUE)) {
    stop("'base' must be strictly positive when raised to an interval exponent", call. = FALSE)
  }

  numerics <- interval_scalar_pow(base_numeric, exp_num$lower, exp_num$upper)
  unit_symbol <- "1"
  .new_units_interval(
    units::set_units(numerics$lower, unit_symbol, mode = "standard"),
    units::set_units(numerics$upper, unit_symbol, mode = "standard")
  )
}

#' @rdname interval-powers
#' @export
nth_root <- function(x, k) {
  stopifnot(inherits(x, "units_interval"))
  if (inherits(k, "units")) {
    k <- .dimensionless_numeric(k, "k")
  }
  if (!is.numeric(k)) {
    stop("'k' must be numeric", call. = FALSE)
  }
  if (length(k) != 1L) {
    if (!all(k == k[1])) {
      stop("All root degrees must agree", call. = FALSE)
    }
    k <- k[1]
  }
  if (!isTRUE(all.equal(k, round(k)))) {
    stop("Root degree must be an integer", call. = FALSE)
  }
  k_int <- as.integer(round(k))
  if (k_int <= 0) {
    stop("Root degree must be positive", call. = FALSE)
  }

  numerics <- .drop_units(x)
  degree_vec <- rep(k_int, length.out = length(x))
  res <- interval_nth_root(numerics$lower, numerics$upper, degree_vec)
  # Derive the resulting unit by dividing integer exponents of the base unit
  base_unit_template <- units(lower_bounds(x))
  # symbolic_units have numerator/denominator vectors we can use
  sym <- base_unit_template
  if (!inherits(sym, "symbolic_units")) {
    # fallback: try to coerce to symbolic representation
    sym <- tryCatch(units::as_units(units::deparse_unit(sym)), error = function(e) NULL)
  }
  if (is.null(sym) || !inherits(sym, "symbolic_units")) {
    stop("Cannot determine base unit structure for nth_root", call. = FALSE)
  }

  num <- sym$numerator
  den <- sym$denominator
  all_syms <- unique(c(num, den))
  net_counts <- vapply(all_syms, function(s) {
    sum(num == s) - sum(den == s)
  }, integer(1))

  if (any(net_counts %% k_int != 0)) {
    stop("Cannot take the ", k_int, "-th root of unit '", units::deparse_unit(base_unit_template), "'", call. = FALSE)
  }

  new_counts <- net_counts / k_int
  # Build unit string
  parts <- character(0)
  for (i in seq_along(all_syms)) {
    s <- all_syms[i]
    expn <- new_counts[i]
    if (expn == 0) next
    if (expn == 1) parts <- c(parts, s) else parts <- c(parts, paste0(s, "^", expn))
  }
  unit_str <- if (length(parts) == 0) "1" else paste(parts, collapse = " ")
  result_unit <- tryCatch(units::as_units(unit_str), error = function(e) NULL)
  if (is.null(result_unit)) {
    stop("Cannot construct root unit from '", units::deparse_unit(base_unit_template), "'", call. = FALSE)
  }

  .new_units_interval(
    units::set_units(res$lower, result_unit, mode = "standard"),
    units::set_units(res$upper, result_unit, mode = "standard")
  )
}

#' @rdname interval-powers
#' @export
root <- function(x, k) {
  nth_root(x, k)
}

#' @rdname interval-powers
#' @export
pow1p <- function(x, exponent) {
  stopifnot(inherits(x, "units_interval"))
  dimless <- units::as_units(1)
  x_dimless <- convert_units(x, dimless)
  numerics <- .drop_units(x_dimless)

  exponent_numeric <- .dimensionless_numeric(exponent, "exponent")
  if (length(exponent_numeric) != 1L) {
    if (!all(exponent_numeric == exponent_numeric[1])) {
      stop("Exponent recycling would lead to inconsistent units", call. = FALSE)
    }
    exponent_numeric <- exponent_numeric[1]
  }

  res <- interval_pow1p(numerics$lower, numerics$upper, exponent_numeric)
  .new_units_interval(
    units::set_units(res$lower, dimless, mode = "standard"),
    units::set_units(res$upper, dimless, mode = "standard")
  )
}

#' Interval hull spanning multiple inputs
#'
#' Construct the minimal interval (element-wise) that contains every supplied
#' interval or point. Inputs are recycled to a common length following standard
#' R recycling rules. When `na.rm = TRUE`, empty intervals (all `NA` bounds) are
#' ignored. Otherwise, any empty input yields an empty result.
#'
#' @param ... Intervals, numeric vectors, or `units` vectors to enclose.
#' @param unit Optional unit specification used when the first argument lacks
#'   units. Must be supplied if the first argument is numeric.
#' @param na.rm Whether to ignore empty intervals.
#' @return A `units_interval` representing the combined hull.
#' @export
hull <- function(..., unit = NULL, na.rm = FALSE) {
  args <- list(...)
  if (length(args) == 0) {
    stop("Supply at least one interval to hull()", call. = FALSE)
  }

  determine_base_unit <- function(x) {
    if (inherits(x, "units_interval")) {
      return(units(x$lower))
    }
    if (inherits(x, "units")) {
      return(units(x))
    }
    NULL
  }

  base_unit <- if (!is.null(unit)) {
    if (inherits(unit, "units")) unit else units::as_units(unit)
  } else {
    first_unit <- determine_base_unit(args[[1]])
    if (is.null(first_unit)) {
      stop("Provide 'unit' when the first argument lacks units", call. = FALSE)
    }
    first_unit
  }

  to_interval <- function(value) {
    if (inherits(value, "units_interval")) {
      convert_units(value, base_unit)
    } else if (inherits(value, "units")) {
      units_interval(value, value, unit = base_unit)
    } else if (is.numeric(value)) {
      units_interval(value, value, unit = base_unit)
    } else {
      stop("Unsupported input type supplied to hull()", call. = FALSE)
    }
  }

  intervals <- lapply(args, to_interval)

  is_empty_interval <- function(intv) {
    all(is.na(intv$lower)) || all(is.na(intv$upper))
  }

  if (na.rm) {
    intervals <- Filter(function(intv) !is_empty_interval(intv), intervals)
    if (length(intervals) == 0) {
      return(empty_interval(unit = base_unit, length = 0))
    }
  } else {
    if (any(vapply(intervals, is_empty_interval, logical(1)))) {
      len <- length(intervals[[1]])
      na_vec <- rep(NA_real_, len)
      return(.new_units_interval(
        units::set_units(na_vec, base_unit, mode = "standard"),
        units::set_units(na_vec, base_unit, mode = "standard")
      ))
    }
  }

  target_length <- length(intervals[[1]])
  if (length(intervals) > 1) {
    for (intv in intervals[-1]) {
      target_length <- .resolve_length(target_length, length(intv))
    }
  }

  align_interval <- function(intv) {
    if (length(intv) == target_length) {
      return(convert_units(intv, base_unit))
    }
    convert_units(.recycle_bounds(intv, target_length), base_unit)
  }

  aligned <- lapply(intervals, align_interval)

  lower_list <- lapply(aligned, function(intv) units::drop_units(intv$lower))
  upper_list <- lapply(aligned, function(intv) units::drop_units(intv$upper))

  reduce_min <- function(a, b) pmin(a, b, na.rm = FALSE)
  reduce_max <- function(a, b) pmax(a, b, na.rm = FALSE)

  lower <- Reduce(reduce_min, lower_list)
  upper <- Reduce(reduce_max, upper_list)

  .new_units_interval(
    units::set_units(lower, base_unit, mode = "standard"),
    units::set_units(upper, base_unit, mode = "standard")
  )
}

# Diagnostics --------------------------------------------------------------

#' Interval diagnostics
#'
#' `zero_in()` reports whether an interval contains zero. `is_empty()` checks
#' for empty intervals (represented as `NA` bounds in this package).
#' `is_subset()` / `is_proper_subset()` and their superset counterparts surface
#' Boost's containment predicates. `is_whole()` identifies intervals that span
#' the entire real line (i.e., `[-Inf, Inf]`).
#'
#' @param x,y `units_interval` objects.
#' @return For `zero_in()` and `is_empty()`, a logical vector. For
#'   `is_subset()` / `is_proper_subset()`, a logical vector indicating whether
#'   the condition holds element-wise. `is_superset()` and
#'   `is_proper_superset()` return analogous results with arguments swapped.
#'   `is_whole()` returns `TRUE` where intervals are unbounded above and below.
#' @name interval-diagnostics
NULL

#' @rdname interval-diagnostics
#' @export
zero_in <- function(x) {
  UseMethod("zero_in")
}

#' @rdname interval-diagnostics
#' @export
zero_in.units_interval <- function(x) {
  numerics <- .drop_units(x)
  as.vector(interval_zero_in(numerics$lower, numerics$upper))
}

#' @rdname interval-diagnostics
#' @export
is_empty <- function(x) {
  UseMethod("is_empty")
}

#' @rdname interval-diagnostics
#' @export
is_empty.units_interval <- function(x) {
  numerics <- .drop_units(x)
  as.vector(interval_is_empty(numerics$lower, numerics$upper))
}

#' @rdname interval-diagnostics
#' @export
is_subset <- function(x, y) {
  pair <- .coerce_pair_same_units(x, y)
  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)
  as.vector(interval_subset(x_num$lower, x_num$upper, y_num$lower, y_num$upper))
}

#' @rdname interval-diagnostics
#' @export
is_proper_subset <- function(x, y) {
  pair <- .coerce_pair_same_units(x, y)
  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)
  as.vector(interval_proper_subset(x_num$lower, x_num$upper, y_num$lower, y_num$upper))
}

#' @rdname interval-diagnostics
#' @export
is_superset <- function(x, y) {
  UseMethod("is_superset")
}

#' @rdname interval-diagnostics
#' @export
is_superset.units_interval <- function(x, y) {
  is_subset(x, y)
}

#' @rdname interval-diagnostics
#' @export
is_proper_superset <- function(x, y) {
  UseMethod("is_proper_superset")
}

#' @rdname interval-diagnostics
#' @export
is_proper_superset.units_interval <- function(x, y) {
  is_proper_subset(x, y)
}

#' @rdname interval-diagnostics
#' @export
is_whole <- function(x) {
  UseMethod("is_whole")
}

#' @rdname interval-diagnostics
#' @export
is_whole.units_interval <- function(x) {
  numerics <- .drop_units(x)
  lower_inf <- is.infinite(numerics$lower) & numerics$lower < 0
  upper_inf <- is.infinite(numerics$upper) & numerics$upper > 0
  lower_inf[is.na(lower_inf)] <- FALSE
  upper_inf[is.na(upper_inf)] <- FALSE
  lower_inf & upper_inf
}

# Containment ---------------------------------------------------------------

#' Interval containment
#'
#' Determine whether intervals contain given points or other intervals.
#'
#' @param x A `units_interval` object.
#' @param value Points (numeric or `units`) or another `units_interval`.
#' @return Logical vector indicating containment for each element.
#' @export
contains <- function(x, value) {
  UseMethod("contains")
}

#' @rdname contains
#' @export
contains.units_interval <- function(x, value) {
  if (inherits(value, "units_interval")) {
    pair <- .coerce_pair_same_units(x, value)
    x_num <- .drop_units(pair$x)
    y_num <- .drop_units(pair$y)
    return(as.vector(interval_contains_interval(x_num$lower, x_num$upper, y_num$lower, y_num$upper)))
  }

  base <- units(x$lower)
  points <- if (inherits(value, "units")) {
    units::set_units(value, base, mode = "standard")
  } else {
    units::set_units(value, base, mode = "standard")
  }

  len <- .resolve_length(length(x), length(points))
  if (length(x) != len) {
    x <- rep(x, length.out = len)
  }
  if (length(points) != len) {
    points <- rep(points, length.out = len)
  }

  numerics <- .drop_units(x)
  pts <- units::drop_units(points)
  interval_contains_point(numerics$lower, numerics$upper, pts)
}

# Overlap -------------------------------------------------------------------

#' Determine interval overlap
#'
#' @param x,y Interval operands.
#' @return Logical vector indicating whether intervals overlap.
#' @export
overlaps <- function(x, y) {
  pair <- .coerce_pair_same_units(x, y)
  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)
  as.vector(interval_overlaps(x_num$lower, x_num$upper, y_num$lower, y_num$upper))
}

# Intersection & union ------------------------------------------------------

interval_intersection_cpp <- interval_intersection
interval_union_cpp <- interval_union

#' Interval intersection
#'
#' Compute the elementwise overlap between two `units_interval` vectors. When
#' intervals do not overlap the corresponding result is an `NA` interval.
#'
#' @param x,y `units_interval` objects.
#' @return A `units_interval` containing the intersections.
#' @export
interval_intersection <- function(x, y) {
  pair <- .coerce_pair_same_units(x, y)
  base_unit <- units(pair$x$lower)
  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)
  res <- interval_intersection_cpp(x_num$lower, x_num$upper, y_num$lower, y_num$upper)
  lower <- units::set_units(res$lower, base_unit, mode = "standard")
  upper <- units::set_units(res$upper, base_unit, mode = "standard")
  .new_units_interval(lower, upper)
}

#' Interval union
#'
#' Compute the elementwise hull that spans the input intervals.
#'
#' @inheritParams interval_intersection
#' @return A `units_interval` containing the hull of each elementwise pair.
#' @export
interval_union <- function(x, y) {
  pair <- .coerce_pair_same_units(x, y)
  base_unit <- units(pair$x$lower)
  x_num <- .drop_units(pair$x)
  y_num <- .drop_units(pair$y)
  res <- interval_union_cpp(x_num$lower, x_num$upper, y_num$lower, y_num$upper)
  lower <- units::set_units(res$lower, base_unit, mode = "standard")
  upper <- units::set_units(res$upper, base_unit, mode = "standard")
  .new_units_interval(lower, upper)
}

#' Mathematical transformations for unit-aware intervals
#'
#' The `Math` group generic supports a subset of unary mathematical
#' transformations. `abs()` preserves units while `sqrt()` expects squared
#' quantities and returns the principal square root with adjusted units.
#' Dimensionless intervals additionally support `exp()`, `expm1()`, `log()`,
#' `log1p()`, `log2()`, `log10()`, `sinh()`, `cosh()`, `tanh()`,
#' `asinh()`, `acosh()`, and `atanh()`. Trigonometric
#' functions `sin()`, `cos()`, and `tan()` accept intervals expressed in radians
#' (or any units convertible to radians) and return dimensionless results.
#' Inverse trigonometric functions `asin()`, `acos()`, and `atan()` operate on
#' dimensionless inputs and return angles in radians.
#'
#' @param x A `units_interval` vector.
#' @param ... Unused.
#' @return A `units_interval` after applying the requested transformation.
#' @export
Math.units_interval <- function(x, ...) {
  fun <- .Generic
  base_unit_symbol <- units::deparse_unit(x$lower)[1]
  base_unit <- units::as_units(base_unit_symbol)
  unitless <- units::as_units(1)
  numerics <- .drop_units(x)

  if (fun == "abs") {
    res <- interval_abs(numerics$lower, numerics$upper)
    return(.new_units_interval(
      units::set_units(res$lower, base_unit, mode = "standard"),
      units::set_units(res$upper, base_unit, mode = "standard")
    ))
  }

  if (fun == "sqrt") {
    unit_sample <- units::set_units(1, base_unit, mode = "standard")
    target_unit_symbol <- units::deparse_unit(sqrt(unit_sample))
    res <- interval_sqrt(numerics$lower, numerics$upper)
    return(.new_units_interval(
      units::set_units(res$lower, target_unit_symbol, mode = "standard"),
      units::set_units(res$upper, target_unit_symbol, mode = "standard")
    ))
  }

  if (fun %in% c("exp", "expm1", "log", "log1p", "log2", "log10",
                 "sinh", "cosh", "tanh", "asinh", "acosh", "atanh")) {
    if (base_unit_symbol != "1") {
      stop(sprintf("Function '%s' requires dimensionless intervals", fun), call. = FALSE)
    }
    res <- switch(
      fun,
  "exp" = interval_exp(numerics$lower, numerics$upper),
      "expm1" = interval_expm1(numerics$lower, numerics$upper),
      "log" = interval_log(numerics$lower, numerics$upper),
      "log1p" = interval_log1p(numerics$lower, numerics$upper),
      "log2" = interval_log2(numerics$lower, numerics$upper),
      "log10" = interval_log10(numerics$lower, numerics$upper),
      "sinh" = interval_sinh(numerics$lower, numerics$upper),
      "cosh" = interval_cosh(numerics$lower, numerics$upper),
      "tanh" = interval_tanh(numerics$lower, numerics$upper),
      "asinh" = interval_asinh(numerics$lower, numerics$upper),
      "acosh" = interval_acosh(numerics$lower, numerics$upper),
      "atanh" = interval_atanh(numerics$lower, numerics$upper)
    )
    return(.new_units_interval(
      units::set_units(res$lower, unitless, mode = "standard"),
      units::set_units(res$upper, unitless, mode = "standard")
    ))
  }

  if (fun %in% c("sin", "cos", "tan")) {
    target_unit <- units::as_units("rad")
    radians <- tryCatch(
      convert_units(x, target_unit),
      error = function(e) {
        stop(sprintf("Function '%s' requires angles convertible to radians", fun), call. = FALSE)
      }
    )
    rad_numerics <- .drop_units(radians)
    res <- switch(
      fun,
      "sin" = interval_sin(rad_numerics$lower, rad_numerics$upper),
      "cos" = interval_cos(rad_numerics$lower, rad_numerics$upper),
      "tan" = interval_tan(rad_numerics$lower, rad_numerics$upper)
    )
    return(.new_units_interval(
      units::set_units(res$lower, unitless, mode = "standard"),
      units::set_units(res$upper, unitless, mode = "standard")
    ))
  }

  if (fun %in% c("asin", "acos", "atan")) {
    if (base_unit_symbol != "1") {
      stop(sprintf("Function '%s' requires dimensionless intervals", fun), call. = FALSE)
    }
    res <- switch(
      fun,
      "asin" = interval_asin(numerics$lower, numerics$upper),
      "acos" = interval_acos(numerics$lower, numerics$upper),
      "atan" = interval_atan(numerics$lower, numerics$upper)
    )
    radians <- units::as_units("rad")
    return(.new_units_interval(
      units::set_units(res$lower, radians, mode = "standard"),
      units::set_units(res$upper, radians, mode = "standard")
    ))
  }

  stop(sprintf("Function '%s' is not implemented for units_interval", fun), call. = FALSE)
}