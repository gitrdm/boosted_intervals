# Boost Interval Parity Checklist

Use this checklist to track parity between the Boost Interval library and the `boostedintervals` R wrapper. Update the status column as work progresses, add notes when behaviour differs intentionally, and reference related issues/commits.

## Legend

- `[x]` Complete
- `[~]` In progress / partial
- `[ ]` Not started

## Core Construction & Arithmetic

- [x] Interval creation with unit awareness (`units_interval`, `convert_units`)
- [x] Basic arithmetic (`+`, `-`, `*`, `/`, unary `-`)
- [x] Integer powers (`^` via `interval_pow`)
- [x] General real powers (`pow(interval, double)`, `pow(double, interval)`) — fractional exponents routed through Boost `pow`
- [x] nth/root helpers (`nth_root`, `root`, `pow1p`, etc.) — wrappers added for `nth_root`, `pow1p`, and related utilities
- [x] Absolute value (`abs`)
- [x] Hull/union helpers beyond binary union (`hull` spanning >2 inputs) — implemented `hull()` aggregator with unit alignment
- [x] Intersection (`interval_intersection`)

## Comparison & Predicates

- [x] Relational comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`)
- [x] Overlap detection (`overlaps`)
- [x] Interval/point containment (`contains`)
- [x] Interval subset / superset predicates (`subset`, `proper_subset`, `superset`)
- [x] Empty/whole interval constructors & checks (`empty`, `whole`, `is_empty`, `is_whole`)
- [x] Zero-in / sign checks (`zero_in`, `in_zero`, etc.)

## Metrics & Summaries

- [x] Width / diameter (`width`)
- [x] Midpoint (`midpoint`)
- [x] Radius / semi-width (`rad`)
- [x] Median (`median` / `median_point`) — added via Boost median helper
- [x] Norm / magnitude / mig helpers (`norm`, `mag`, `mig`)
- [x] Distance metrics (`distance`)
- [x] Bisect / inflate utilities (`bisect`, `inflate`) — implemented with Boost bisect and widening helpers

## Transcendental & Special Functions

- [x] Square root (`sqrt`)
- [x] Exponential (`exp`)
- [x] Exponential minus one (`expm1`)
- [x] Natural logarithm (`log`)
- [x] Logarithm base 10 (`log10`)
- [x] Logarithm base 2 (`log2`)
- [x] Logarithm of one plus (`log1p`)
- [x] Trigonometric (`sin`, `cos`, `tan`) with radian handling
- [x] Inverse trigonometric (`asin`, `acos`, `atan`) returning radians
- [x] Hyperbolic (`sinh`, `cosh`, `tanh`)
- [x] Inverse hyperbolic (`asinh`, `acosh`, `atanh`)
- [x] Base-2 exponential (`exp2`, `pow2`) – exported interval-aware helpers (see commit for details)
- [x] Other Boost-provided helpers (`sqrt1pm1`, `hypot`, etc.) – wrappers implemented with tests

## Rounding & Policy Controls

- [x] Successor / predecessor (`successor`, `predecessor`, `next_interval`, `prior_interval`) — scalar and interval helpers with unit preservation
- [x] Directed rounding utilities (e.g., `median_rounding`, `round_outward`, `round_inward`) — wrapping Boost outward/inward adjustments
- [x] Policy customization surface (rounding/checking getters, setters, and `with_*()` guards)

## Diagnostics & Utilities

- [x] Possibility/necessity helpers (`possible`, `certain`, `verify`) — wired to Boost comparators with regression tests
- [x] Interval inflation / contraction helpers (`inflate`, `contract`) — inward contraction counterpart added
- [x] Conversion to other representations (e.g., `as.numeric` hull endpoints) — numeric bounds helper implemented

## Documentation & Testing Tasks

- [x] Checklist created (`inst/checklists/boost_parity_checklist.md`)
- [x] Document outstanding gaps in vignette / developer notes as they close — vignette section added for rounding & policies
- [x] Add regression tests when each unchecked item is implemented — rounding test suite covers new helpers

## Usage Notes

1. Before starting a parity effort, duplicate the relevant section into your task notes and mark the planned scope.
2. When you implement a new capability:
   - Update the corresponding checkbox to `[x]` and add a brief note (e.g., "Added in PR #42").
   - Ensure tests and documentation are updated before marking complete.
3. If you decide a Boost feature is intentionally out of scope, annotate the checkbox with the rationale instead of checking it off.
