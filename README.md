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
- **Minimal state:** Aside from optional rounding/checking modes, the package is
  largely stateless. Inputs in, outputs out.
- **Regressions only:** Tests focus on correctness/regression scenarios, not on
  performance benchmarking or fuzzing.

## Disclaimer

This software is provided "as is". It is a light wrapper over Boost.Interval
and inherits any defects present in Boost in addition to bugs in the R glue. Use
it entirely at your own risk, especially in safety-critical or
regulatory-sensitive settings.
