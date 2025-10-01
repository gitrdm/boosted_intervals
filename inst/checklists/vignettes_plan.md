# Vignettes Plan for boostedintervals

This document records the planned vignettes, their purpose, and minimal
implementation notes so the context survives across sessions and context
window shifts.

## Goal
Keep a stable, discoverable plan for adding narrative examples that exercise
both the R wrapper and the underlying Boost.Interval features. For each
vignette we list filename, title, purpose, key APIs to demonstrate, short
example snippets, test suggestions, and priority.

---

## Core / Priority vignettes

1. boosting-the-basics.Rmd
   - Title: "Boost-backed interval basics"
   - Purpose: Show how the R wrappers map to Boost operations and exercise
     core arithmetic, comparisons, and numeric conversion helpers.
   - Key APIs: `units_interval`, arithmetic Ops, `possible`, `certain`, `verify`, `as_numeric_bounds`
   - Snippet:
     ```r
     a <- units_interval(set_units(0, "m"), set_units(1, "m"))
     b <- units_interval(set_units(0.5, "m"), set_units(2, "m"))
     a + b
     possible(a, b, "<")
     ```
   - Tests: small happy-path test checking outputs are `units_interval` and
     `possible()` returns expected logicals.
   - Priority: High

2. rounding-and-policies.Rmd
   - Title: "Rounding, policies, and the floating-point lattice"
   - Purpose: Demonstrate `successor`/`predecessor`, scalar helpers,
     `next_interval`/`prior_interval`, `round_outward`/`round_inward`, and
     the `with_rounding_mode()`/`with_checking_mode()` guards.
   - Key APIs: `successor`, `predecessor`, `next_value`, `prior_value`,
     `next_interval`, `prior_interval`, `round_outward`, `round_inward`,
     `median_rounding`, `with_rounding_mode`, `with_checking_mode`
   - Snippet:
     ```r
     with_rounding_mode("upward", {
       successor(set_units(1, "m"))
     })
     ```
   - Tests: verify `with_*()` restores previous state and inward/outward
     rounding collapse-case returns `NA` when it should.
   - Priority: High

3. diagnostics-and-decision.Rmd
   - Title: "Possibility, necessity, and verification"
   - Purpose: Walk through tri-state diagnostics and how to use these in data
     pipelines to flag ambiguous or unsafe values.
   - Key APIs: `possible`, `certain`, `verify`, `contract`, `inflate`
   - Tests: small data-frame-based filtering examples.
   - Priority: High

---

## Supporting vignettes

4. integer-vs-real-semantics.Rmd
   - Title: "Integer vs real interval semantics (what the R wrapper assumes)"
   - Purpose: Explicitly show where Boost integer semantics differ and why the
     R wrapper uses real (double) endpoints.
   - Key point: R wrapper assumes real/double endpoints; integer-specific
     behaviours (overflow, discrete-only semantics) are not modelled.
   - Priority: Medium

5. performance-and-benchmarks.Rmd
   - Title: "Performance, vectorization, and when to push to C++"
   - Purpose: Practical advice and small benchmarks showing overhead of
     unit conversions and strategies to minimize it.
   - Priority: Medium

---

## Discipline vignettes (one per discipline; iteration plan)

- physics-uncertainties.Rmd
  - Example: propagate measurement uncertainty through kinetic energy,
    E = 1/2 * m * v^2 (units: J). Show `as_numeric_bounds()` for external use.
  - Priority: Medium-High

- mechanical-engineering-tolerancing.Rmd
  - Example: shaft/hole fits; compute clearance interval and check `> 0`.
  - Priority: Medium-High

- geoscience-uncertainty.Rmd
  - Example: sensor elevation intervals, distance calculations, mapping
    integration with spatial plotting.
  - Priority: Medium

- finance-intervals.Rmd
  - Example: uncertain interest rates and compound outcomes; show numeric
    conversion and threshold checks.
  - Priority: Medium

- pharmacokinetics.Rmd
  - Example: concentration-time profiles from uncertain doses and volumes.
  - Priority: Medium

---

## Implementation guidance

- Keep vignettes self-contained and synthetic (no large external datasets).
- Each vignette should include:
  - Short motivation paragraph
  - Minimal reproducible examples (happy path)
  - One small plot when useful (ggplot2 ribbon using `lower`/`upper`)
  - A short "takeaways" bullet list and any warnings about pitfalls
- Add small `tests/testthat/test-vignette-<topic>.R` files that run the core
  snippet(s) (happy-path assertions). These tests should be lightweight and
  only verify that the core functions return reasonable types/values.

## Suggested next actionable steps

1. Scaffold `vignettes/boosting-the-basics.Rmd` and
   `vignettes/rounding-and-policies.Rmd` and add `tests/testthat/test-vignette-*.R` for both.
2. Render the vignettes locally with `devtools::build_vignettes()` to ensure
   they knit in the build environment.
3. Iterate on discipline vignettes starting with `physics-uncertainties.Rmd`.

---

_Last updated: 2025-10-01_
