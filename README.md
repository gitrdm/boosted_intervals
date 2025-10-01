# boostedintervals

`boostedintervals` is a proof-of-concept R package that combines rigorous
interval arithmetic (via the Boost.Interval C++ library) with the
unit-awareness of the `units` R package. It offers a light-weight wrapper
around Boost routines, exposing idiomatic R helpers for constructing,
manipulating, and diagnosing interval-valued quantities.

## Why this package exists

- **Interval arithmetic with units:** R users can model measurement
  uncertainty, tolerances, and physical constraints while preserving unit
  metadata throughout calculations.
- **Boost accuracy with R ergonomics:** Boost.Interval handles interval math
  correctly; the package brings that capability into R with S3 classes,
  vectorization, and tidy-ish workflows.
- **Diagnostics & utilities:** The API includes relation diagnostics (possible
  vs. certain), contraction helpers, rounding controls, and conversion tools to
  bridge interval computations with downstream pipelines.

> ⚠️ **Proof of concept** — this package is an experiment, not a polished
> production library. Expect sharp edges and evolving behaviour.

## Installation

This package isn't on CRAN. Install the development snapshot directly from
GitHub using `remotes` (or `devtools`, which re-exports the same helper):

```r
# install.packages("remotes")
remotes::install_github("rdmerrio/boosted_intervals")
```

Because compilation is required, ensure you have a working R toolchain (Rtools
on Windows, Xcode Command Line Tools on macOS, or build-essential + headers on
Linux) along with system Boost headers. The package links against Boost via the
`BH` R package, so manual Boost installation is usually not necessary.

## Underlying assumptions

- **Parentheses around complex powers:** Fractional exponents and negative
  bases require explicit parentheses, mirroring `pow(interval, double)` in
  Boost. For example, `(x^0.5)` is safe, while `x^0.5` without parentheses can
  trigger unintended operator precedence.
- **Unit compatibility required:** Inputs must carry compatible units (or be
  coercible) before interval operations occur. The package expects you to align
  units explicitly when mixing heterogeneous quantities.
- **Closed intervals only:** All helpers assume closed intervals. Open or
  half-open behaviours are out of scope.
- **Deterministic double precision:** Computations rely on IEEE-754 double
  precision and Boost's directed rounding controls. Alternative numeric types
  aren't supported.
- **Real (double) endpoints assumed:** While Boost.Interval supports both
  integer and real interval types, this R wrapper assumes real (double)
  endpoints everywhere because unit handling and conversions are defined for
  reals in `units`. Integer-specific semantics (e.g., modular wrapping,
  integer overflow behaviour, or discrete-only operations) are not modelled
  by the R helpers.
- **Finite, real-valued bounds:** Complex numbers, symbolic representations, or
  infinite precision arithmetic aren't part of this design.

## Quick start

```r
library(units)
library(boostedintervals)

# Create a unit-aware interval
width <- units_interval(set_units(5, "cm"), set_units(5.5, "cm"))
height <- units_interval(set_units(7, "cm"), set_units(7.2, "cm"))

# Perform interval arithmetic (units combine automatically)
area <- width * height
area
#> [25[cm^2], 39.6[cm^2]]  (example output)

# Diagnostics: is an overlap possible or certain?
tolerance <- units_interval(set_units(38, "cm^2"), set_units(40, "cm^2"))
possible(area, tolerance, "<=")
#> TRUE  TRUE
certain(area, tolerance, "<=")
#> FALSE TRUE

# Convert to bare numeric bounds for external code
as_numeric_bounds(area, unit = "cm^2")
```

## API Overview

### Core Concepts

**Interval Arithmetic**: Operations propagate uncertainty through calculations, producing conservative bounds that guarantee containment of the true result.

**Unit Compatibility**: All operations require compatible units. Use `convert_units()` to align units before operations.

**Vectorization**: Most functions work element-wise on vectors of intervals. Single intervals are recycled to match vector lengths.

**NA Handling**: Invalid operations (like division by zero-spanning intervals) return `NA` intervals. Check with `is_empty()` before use.

### Common Patterns

**Measurement Uncertainty**:
```r
# Model sensor precision
temp <- units_interval(20, 25, unit = "celsius") 
pressure <- units_interval(1000, 1010, unit = "hPa")

# Calculate derived quantities with propagated uncertainty
density <- pressure / (287.05 * (temp + 273.15))
```

**Tolerance Checking**:
```r
measured <- units_interval(9.5, 10.5, unit = "mm")
spec <- units_interval(9.8, 10.2, unit = "mm")

possible(measured, spec, "==")  # Could be in spec?
certain(measured, spec, "==")   # Guaranteed in spec?
```

**Example Calculations**:
```r
# Rectangle with uncertain dimensions
w <- units_interval(2, 3, unit = "m")
h <- units_interval(1, 1.5, unit = "m")
area <- w * h  # Units: m²

# Check if area meets requirements
min_area <- set_units(2.5, "m^2")
certain(area, min_area, ">=")
```

## Function Reference

### Construction & Conversion
- `units_interval()` - Create unit-aware intervals from bounds
- `as_units_interval()` - Convert objects to units_interval
- `as_numeric_bounds()` - Extract numeric bounds without units
- `convert_units()` - Change units of interval bounds
- `convert_units()` - Change units of interval bounds

### Arithmetic Operations
- `+`, `-`, `*`, `/` - Standard arithmetic with unit propagation
- `^` - Integer powers (use `pow_interval()` for fractional)
- `sqrt()`, `exp()`, `log()` - Transcendental functions
- `pow_interval()`, `nth_root()` - Advanced power operations

### Set Operations
- `interval_union()`, `interval_intersection()` - Combine intervals
- `hull()` - Convex hull of multiple intervals
- `contract()` - Shrink intervals toward midpoint

### Diagnostics & Relations
- `possible()`, `certain()`, `verify()` - Check if relations might/must hold
- `is_subset()`, `is_superset()` - Containment checks
- `overlaps()` - Detect interval overlap
- `zero_in()` - Check if interval contains zero
- `is_empty()` - Detect invalid/empty intervals

### Bounds & Measures
- `lower_bounds()`, `upper_bounds()` - Extract bounds
- `midpoint()`, `width()`, `radius()` - Interval properties
- `median()` - Median of interval bounds

### Rounding & Precision
- `round_inward()`, `round_outward()` - Directed rounding
- `predecessor()`, `successor()` - Adjacent representable values
- `next_interval()`, `prior_interval()` - Interval stepping

### Policy Controls
- `checking_modes()`, `set_checking_mode()` - Error handling behavior
- `rounding_modes()`, `set_rounding_mode()` - Rounding precision control

## Quick Reference Cheatsheet

### Creating Intervals
```r
# Numeric bounds with units
units_interval(0, 10, unit = "m")
units_interval(c(0, 5), c(10, 15), unit = "s")

# From units objects  
units_interval(set_units(0, "m"), set_units(10, "m"))
units_interval(set_units(c(0, 5), "s"), set_units(c(10, 15), "s"))

# Unit conversion during creation
units_interval(set_units(0, "m"), set_units(1000, "mm"), unit = "m")
```

### Arithmetic Operations
```r
x <- units_interval(1, 3, unit = "m")
y <- units_interval(2, 4, unit = "m")

x + y    # Addition
x * y    # Multiplication  
x / y    # Division
x ^ 2    # Integer powers
sqrt(x)  # Square root
```

### Checking Relations
```r
a <- units_interval(1, 5, unit = "m")
b <- units_interval(3, 7, unit = "m")

possible(a, b, "<")   # TRUE - some a < some b
certain(a, b, "<")    # FALSE - not all a < all b  
verify(a, b, "<")     # NA - cannot determine

is_subset(a, b)       # TRUE - a is subset of b
overlaps(a, b)        # TRUE - intervals overlap
```

### Extracting Values
```r
interval <- units_interval(1, 5, unit = "m")

lower_bounds(interval)  # c(1) with units
upper_bounds(interval)  # c(5) with units
midpoint(interval)      # c(3) with units
width(interval)         # c(4) with units

as_numeric_bounds(interval)  # list(lower = 1, upper = 5)
```

### Common Error Patterns
```r
# ❌ Wrong: incompatible units
units_interval(set_units(1, "m"), set_units(5, "s"))

# ❌ Wrong: lower > upper  
units_interval(5, 1, unit = "m")

# ❌ Wrong: numeric without units
units_interval(1, 5)  # Error!

# ✅ Correct: specify units
units_interval(1, 5, unit = "m")
```


## Known limitations & gotchas

- **Loss of performance:** The R layering and unit conversions add overhead
  compared to pure C++ Boost usage. Heavy workloads may be slow.
- **Boost syntax constraints:** Some Boost algorithms are surfaced largely as-is;
  you'll still need to respect Boost-specific domain restrictions (e.g., nth
  roots require odd degrees for negative bases).
- **Checking modes aren't bulletproof:** Switching to warning or permissive mode
  suppresses hard stops but may return `NA` intervals — verify results before
  trusting them.
- **No symbolic simplification:** The library executes numerically; it will not
  attempt analytical simplifications or unit algebra beyond what `units`
  provides.
- **R session responsibility:** The package doesn't manage global rounding or
  checking modes outside its helpers. If you alter those states manually,
  you're responsible for restoring them.

## Architecture notes

- **Light Boost wrapper:** Almost every exported R helper maps directly to a
  Boost-Interval-backed routine via Rcpp. That keeps the implementation thin but
  also means Boost limitations and bugs surface directly in R.
  Note: the wrapper is focused on real/double interval semantics. If you need
  integer-typed interval behaviour you should use Boost directly or extend the
  C++ layer — the R layer does not attempt to emulate integer-specific rules.
- **Minimal state:** Aside from optional rounding/checking modes, the package is
  largely stateless. Inputs in, outputs out.
- **Regressions only:** Tests focus on correctness/regression scenarios, not on
  performance benchmarking or fuzzing.

## Disclaimer

This software is provided "as is". It is a light wrapper over Boost.Interval
and inherits any defects present in Boost in addition to bugs in the R glue. Use
it entirely at your own risk, especially in safety-critical or
regulatory-sensitive settings.