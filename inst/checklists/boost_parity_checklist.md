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
- [ ] General real powers (`pow(interval, double)`, `pow(double, interval)`)
- [ ] nth/root helpers (`nth_root`, `root`, `pow1p`, etc.)
- [x] Absolute value (`abs`)
- [ ] Hull/union helpers beyond binary union (`hull` spanning >2 inputs)
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
- [ ] Median (`median` / `median_point`)
- [x] Norm / magnitude / mig helpers (`norm`, `mag`, `mig`)
- [x] Distance metrics (`distance`)
- [ ] Bisect / inflate utilities (`bisect`, `inflate`)

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
- [ ] Base-2 exponential (`exp2`, `pow2`) – decide on exposure strategy
- [ ] Other Boost-provided helpers (`sqrt1pm1`, `hypot`, etc.)

## Rounding & Policy Controls

- [ ] Successor / predecessor (`successor`, `predecessor`, `next`, `prior`)
- [ ] Directed rounding utilities (e.g., `median_rounding`, `round_outward`)
- [ ] Policy customization surface (exposing `save_state`, custom checking policies)

## Diagnostics & Utilities

- [ ] Possibility/necessity helpers (`possible`, `certain`, `verify`)
- [ ] Interval inflation / contraction helpers (`inflate`, `contract`)
- [ ] Conversion to other representations (e.g., `as.numeric` hull endpoints)

## Documentation & Testing Tasks

- [x] Checklist created (`inst/checklists/boost_parity_checklist.md`)
- [ ] Document outstanding gaps in vignette / developer notes as they close
- [ ] Add regression tests when each unchecked item is implemented

## Usage Notes

1. Before starting a parity effort, duplicate the relevant section into your task notes and mark the planned scope.
2. When you implement a new capability:
   - Update the corresponding checkbox to `[x]` and add a brief note (e.g., "Added in PR #42").
   - Ensure tests and documentation are updated before marking complete.
3. If you decide a Boost feature is intentionally out of scope, annotate the checkbox with the rationale instead of checking it off.
