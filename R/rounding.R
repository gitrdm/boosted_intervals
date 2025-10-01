#' Rounding controls and successor helpers
#'
#' These helpers expose Boost's control features for navigating the floating
#' point lattice and nudging interval bounds. `successor()` and
#' `predecessor()` shift individual bounds by the next representable double,
#' `next_value()` and `prior_value()` advance scalar measurements while
#' preserving units, and `next_interval()` and `prior_interval()` move both
#' bounds outward or inward. Building on those primitives, `round_outward()` expands an interval
#' conservatively and `round_inward()` contracts it. `median_rounding()`
#' collapses an interval to its Boost median, returning a point value that lies
#' inside the original enclosure.
#'
#' @param x Either a `units_interval`, a `units` vector, or a numeric vector.
#' @param steps Non-negative integer step counts recycled along `x`. Each step
#'   advances one representable floating-point value.
#' @return `successor()`, `predecessor()`, `next_value()`, and `prior_value()`
#'   return objects of the same class as `x` (numeric inputs yield doubles,
#'   `units` inputs retain their units). Interval-specific helpers
#'   (`next_interval()`, `prior_interval()`, `round_outward()`, and
#'   `round_inward()`) always return `units_interval` vectors.
#'   `median_rounding()` returns a `units` vector representing the rounded
#'   median.
#' @examples
#' library(units)
#' interval <- units_interval(set_units(1, "m"), set_units(2, "m"))
#' successor(interval)
#' predecessor(interval)
#' round_outward(interval)
#' round_inward(interval)
#'
#' dimless <- units_interval(-0.5, 0.5, unit = "1")
#' median_rounding(dimless)
#' @name rounding-helpers
NULL

.validate_rounding_steps <- function(steps, n, arg = "steps") {
  if (!is.numeric(steps)) {
    stop(sprintf("'%s' must be numeric", arg), call. = FALSE)
  }
  if (length(steps) == 0) {
    return(integer(n))
  }
  steps_int <- suppressWarnings(as.integer(steps))
  if (any(is.na(steps_int) & !is.na(steps))) {
    stop(sprintf("'%s' must contain whole numbers", arg), call. = FALSE)
  }
  if (any(steps_int < 0, na.rm = TRUE)) {
    stop(sprintf("'%s' must be non-negative", arg), call. = FALSE)
  }
  rep_len(steps_int, n)
}

.successor_numeric <- function(x, steps) {
  interval_successor_scalar(x, steps)
}

.predecessor_numeric <- function(x, steps) {
  interval_predecessor_scalar(x, steps)
}

.successor_units <- function(x, steps) {
  base_unit <- units(x)
  numeric <- units::drop_units(units::set_units(x, base_unit, mode = "standard"))
  shifted <- .successor_numeric(numeric, steps)
  units::set_units(shifted, base_unit, mode = "standard")
}

.predecessor_units <- function(x, steps) {
  base_unit <- units(x)
  numeric <- units::drop_units(units::set_units(x, base_unit, mode = "standard"))
  shifted <- .predecessor_numeric(numeric, steps)
  units::set_units(shifted, base_unit, mode = "standard")
}

.next_value_numeric <- function(x, steps) {
  interval_next_scalar(x, steps)
}

.prior_value_numeric <- function(x, steps) {
  interval_prior_scalar(x, steps)
}

.next_value_units <- function(x, steps) {
  base_unit <- units(x)
  numeric <- units::drop_units(units::set_units(x, base_unit, mode = "standard"))
  shifted <- .next_value_numeric(numeric, steps)
  units::set_units(shifted, base_unit, mode = "standard")
}

.prior_value_units <- function(x, steps) {
  base_unit <- units(x)
  numeric <- units::drop_units(units::set_units(x, base_unit, mode = "standard"))
  shifted <- .prior_value_numeric(numeric, steps)
  units::set_units(shifted, base_unit, mode = "standard")
}

.successor_interval <- function(x, steps) {
  numerics <- .drop_units(x)
  res <- interval_successor_interval(numerics$lower, numerics$upper, steps)
  unit <- units(x$lower)
  .new_units_interval(
    units::set_units(res$lower, unit, mode = "standard"),
    units::set_units(res$upper, unit, mode = "standard")
  )
}

.predecessor_interval <- function(x, steps) {
  numerics <- .drop_units(x)
  res <- interval_predecessor_interval(numerics$lower, numerics$upper, steps)
  unit <- units(x$lower)
  .new_units_interval(
    units::set_units(res$lower, unit, mode = "standard"),
    units::set_units(res$upper, unit, mode = "standard")
  )
}

.next_interval_impl <- function(x, steps) {
  numerics <- .drop_units(x)
  res <- interval_next_interval(numerics$lower, numerics$upper, steps)
  unit <- units(x$lower)
  .new_units_interval(
    units::set_units(res$lower, unit, mode = "standard"),
    units::set_units(res$upper, unit, mode = "standard")
  )
}

.prior_interval_impl <- function(x, steps) {
  numerics <- .drop_units(x)
  res <- interval_prior_interval(numerics$lower, numerics$upper, steps)
  unit <- units(x$lower)
  .new_units_interval(
    units::set_units(res$lower, unit, mode = "standard"),
    units::set_units(res$upper, unit, mode = "standard")
  )
}

.round_outward_interval_impl <- function(x, steps) {
  numerics <- .drop_units(x)
  res <- interval_round_outward(numerics$lower, numerics$upper, steps)
  unit <- units(x$lower)
  .new_units_interval(
    units::set_units(res$lower, unit, mode = "standard"),
    units::set_units(res$upper, unit, mode = "standard")
  )
}

.round_inward_interval_impl <- function(x, steps) {
  numerics <- .drop_units(x)
  res <- interval_round_inward(numerics$lower, numerics$upper, steps)
  unit <- units(x$lower)
  .new_units_interval(
    units::set_units(res$lower, unit, mode = "standard"),
    units::set_units(res$upper, unit, mode = "standard")
  )
}

#' @rdname rounding-helpers
#' @export
successor <- function(x, steps = 1L) {
  n <- length(x)
  steps <- .validate_rounding_steps(steps, n)
  if (inherits(x, "units_interval")) {
    return(.successor_interval(x, steps))
  }
  if (inherits(x, "units")) {
    return(.successor_units(x, steps))
  }
  if (is.numeric(x)) {
    return(.successor_numeric(x, steps))
  }
  stop("Unsupported input for successor()", call. = FALSE)
}

#' @rdname rounding-helpers
#' @export
predecessor <- function(x, steps = 1L) {
  n <- length(x)
  steps <- .validate_rounding_steps(steps, n)
  if (inherits(x, "units_interval")) {
    return(.predecessor_interval(x, steps))
  }
  if (inherits(x, "units")) {
    return(.predecessor_units(x, steps))
  }
  if (is.numeric(x)) {
    return(.predecessor_numeric(x, steps))
  }
  stop("Unsupported input for predecessor()", call. = FALSE)
}

#' @rdname rounding-helpers
#' @export
next_value <- function(x, steps = 1L) {
  n <- length(x)
  steps <- .validate_rounding_steps(steps, n)
  if (inherits(x, "units_interval")) {
    stop("next_value() expects numeric or units input; use next_interval() for intervals.", call. = FALSE)
  }
  if (inherits(x, "units")) {
    return(.next_value_units(x, steps))
  }
  if (is.numeric(x)) {
    return(.next_value_numeric(x, steps))
  }
  stop("Unsupported input for next_value()", call. = FALSE)
}

#' @rdname rounding-helpers
#' @export
prior_value <- function(x, steps = 1L) {
  n <- length(x)
  steps <- .validate_rounding_steps(steps, n)
  if (inherits(x, "units_interval")) {
    stop("prior_value() expects numeric or units input; use prior_interval() for intervals.", call. = FALSE)
  }
  if (inherits(x, "units")) {
    return(.prior_value_units(x, steps))
  }
  if (is.numeric(x)) {
    return(.prior_value_numeric(x, steps))
  }
  stop("Unsupported input for prior_value()", call. = FALSE)
}

#' @rdname rounding-helpers
#' @export
next_interval <- function(x, steps = 1L) {
  if (!inherits(x, "units_interval")) {
    stop("next_interval() expects a units_interval", call. = FALSE)
  }
  steps <- .validate_rounding_steps(steps, length(x))
  .next_interval_impl(x, steps)
}

#' @rdname rounding-helpers
#' @export
prior_interval <- function(x, steps = 1L) {
  if (!inherits(x, "units_interval")) {
    stop("prior_interval() expects a units_interval", call. = FALSE)
  }
  steps <- .validate_rounding_steps(steps, length(x))
  .prior_interval_impl(x, steps)
}

#' @rdname rounding-helpers
#' @export
round_outward <- function(x, steps = 1L) {
  if (!inherits(x, "units_interval")) {
    stop("round_outward() expects a units_interval", call. = FALSE)
  }
  steps <- .validate_rounding_steps(steps, length(x))
  .round_outward_interval_impl(x, steps)
}

#' @rdname rounding-helpers
#' @export
round_inward <- function(x, steps = 1L) {
  if (!inherits(x, "units_interval")) {
    stop("round_inward() expects a units_interval", call. = FALSE)
  }
  steps <- .validate_rounding_steps(steps, length(x))
  .round_inward_interval_impl(x, steps)
}

#' @rdname rounding-helpers
#' @export
median_rounding <- function(x) {
  if (!inherits(x, "units_interval")) {
    stop("median_rounding() expects a units_interval", call. = FALSE)
  }
  numerics <- .drop_units(x)
  med <- interval_median(numerics$lower, numerics$upper)
  units::set_units(med, units(x$lower), mode = "standard")
}

#' Control Boost interval rounding and checking policies
#'
#' These helpers expose Boost interval's policy controls so callers can opt into
#' directed rounding or relaxed checking semantics for targeted operations. The
#' setters return the previous mode, making it straightforward to restore the
#' prior configuration manually or via the corresponding `with_*` helpers.
#'
#' @param mode A string identifying the desired policy. For rounding modes the
#'   allowed values are `"nearest"`, `"upward"`, `"downward"`, and
#'   `"toward_zero"`. For checking modes choose among `"strict"`, `"warning"`,
#'   and `"permissive"`.
#' @param expr Code to evaluate while a temporary policy is active.
#' @return `set_rounding_mode()` and `set_checking_mode()` return the previous
#'   mode invisibly (as a string). The `with_*()` helpers return the result of
#'   evaluating `expr`. The `*_modes()` accessors return the available options
#'   as character vectors.
#' @details In `"warning"` checking mode, domain violations emit a warning and
#'   yield `NA` intervals (or scalars) instead of terminating the computation.
#'   `"permissive"` mode behaves the same but suppresses the warning, enabling
#'   exploratory workflows where wide or undefined operations are expected.
#' @examples
#' old <- set_rounding_mode("upward")
#' on.exit(set_rounding_mode(old))
#' with_rounding_mode("downward", successor(1))
#' checking_modes()
#' @name policy-controls
NULL

.rounding_modes <- c("nearest", "upward", "downward", "toward_zero")
.checking_modes <- c("strict", "warning", "permissive")

.checking_mode_index <- function(mode) {
  match(mode, .checking_modes) - 1L
}

.checking_mode_name <- function(index) {
  .checking_modes[index + 1L]
}

#' @rdname policy-controls
#' @export
rounding_modes <- function() {
  .rounding_modes
}

#' @rdname policy-controls
#' @export
checking_modes <- function() {
  .checking_modes
}

#' @rdname policy-controls
#' @export
get_rounding_mode <- function() {
  interval_rounding_name(interval_get_rounding_mode())
}

#' @rdname policy-controls
#' @export
set_rounding_mode <- function(mode = c("nearest", "upward", "downward", "toward_zero")) {
  mode <- match.arg(mode)
  previous <- interval_set_rounding_mode(interval_rounding_constant(mode))
  invisible(interval_rounding_name(previous))
}

#' @rdname policy-controls
#' @export
with_rounding_mode <- function(mode, expr) {
  old <- get_rounding_mode()
  set_rounding_mode(mode)
  on.exit(set_rounding_mode(old), add = TRUE)
  eval(substitute(expr), envir = parent.frame())
}

#' @rdname policy-controls
#' @export
get_checking_mode <- function() {
  .checking_mode_name(interval_get_checking_mode())
}

#' @rdname policy-controls
#' @export
set_checking_mode <- function(mode = c("strict", "warning", "permissive")) {
  mode <- match.arg(mode)
  previous <- interval_get_checking_mode()
  interval_set_checking_mode(.checking_mode_index(mode))
  invisible(.checking_mode_name(previous))
}

#' @rdname policy-controls
#' @export
with_checking_mode <- function(mode, expr) {
  old <- get_checking_mode()
  set_checking_mode(mode)
  on.exit(set_checking_mode(old), add = TRUE)
  eval(substitute(expr), envir = parent.frame())
}