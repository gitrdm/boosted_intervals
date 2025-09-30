# Boosted Intervals — Rcpp Integration Checklist

_Last updated: 2025-09-30_

This document captures the current state of the `boostedintervals` package scaffold and outlines the detailed steps required to finish the Boost.Interval + Rcpp integration. It is designed so any coding agent (human or LLM) can quickly rehydrate context after a context-window reset.

---

## 1. Current Scaffold Snapshot

- **Package metadata**
  - `DESCRIPTION`: Package name `boostedintervals`, imports `units`, `Rcpp`, links to `Rcpp`. No external Boost system requirement.
  - `NAMESPACE`: Currently only `export(units_interval)`; no `useDynLib` or Rcpp imports yet.
- **R-level code**
  - `R/units_interval.R`: Provides `units_interval()` constructor validation and simple print method. No arithmetic, conversions, or helper APIs yet.
- **C++ layer**
  - `src/interval_arithmetic.cpp`: Implements only `interval_add()` via Boost.Interval, returning lower/upper bounds as `Rcpp::List`.
  - `src/Makevars`: Includes headers from `../inst/include` (no compiled libraries required).
- **Bundled Boost subset**
  - `inst/include/boost/`: Contains hand-curated headers (numeric, config, mpl, type_traits, utility, detail, and core helper headers) sufficient for current compilation.
- **Testing & docs**
  - `man/`, `tests/`, `vignettes/`: Present but empty placeholders.
  - No README or vignette content yet; roxygen not yet run.
- **Build artifacts**
  - `.gitignore` excludes tarballs, Rproj artefacts, compiled objects.
  - `boostedintervals_0.1.0.tar.gz` exists from local build (ignore for commits).

---

## 2. Implementation Checklist

### 2.1 Build & Infrastructure
- [ ] Confirm the pruned Boost headers cover transitive includes needed for future operations. If a new compile error surfaces, add the missing header subtree to `inst/include/boost/`.
- [ ] Update `NAMESPACE` to include `useDynLib(boostedintervals, .registration = TRUE)` and `importFrom(Rcpp, evalCpp)` (via roxygen tags once new functions are documented).
- [ ] After adding new C++ functions, run `Rcpp::compileAttributes()` to regenerate `src/RcppExports.cpp`/`RcppExports.R` (these files do not exist yet).

### 2.2 C++ Interval Core
Implement remaining Boost interval routines in `src/` (one export per feature).
- [ ] Add an internal helper to construct intervals safely (validate bounds, handle NaNs/Inf, throw descriptive errors).
- [ ] Arithmetic: `interval_subtract`, `interval_multiply`, `interval_divide` (reject intervals spanning zero), `interval_negate`.
- [ ] Comparison and predicates: equality/inequality, relational comparisons, `contains_point`, `overlaps`, `is_subset`, etc.
- [ ] Interval-specific ops: `interval_width`, `interval_midpoint`, `interval_intersection`, `interval_union` (with empty-set handling).
- [ ] Wrap Boost exceptions in `Rcpp::stop` to surface meaningful R errors.
- [ ] Ensure all functions operate on vectorised inputs (recycle lengths or enforce matching lengths as per design decision).

### 2.3 R Wrappers & S3 Methods
Bridge R and C++ while respecting units.
- [ ] Add R wrapper functions (e.g., `interval_add_cpp <- function(...)`) that:
  - Validate `units_interval` objects.
  - Align/conform units via `units::set_units()`.
  - Convert to numeric (`units::drop_units`), call the Rcpp function, and rebuild a `units_interval` with original unit metadata.
- [ ] Define arithmetic operators (`+.units_interval`, `-.units_interval`, etc.) delegating to wrappers.
- [ ] Implement comparison generics (`==`, `<`, `<=`, etc.) and predicates (`contains`, `intersect`, `union`, `width`, `midpoint`).
- [ ] Add constructors/helpers: `as_units_interval`, `units_interval_from_numeric`, `rep`, `c`, list coercions if needed.
- [ ] Provide `convert_units.units_interval` to leverage the `units` package while maintaining interval integrity.

### 2.4 Validation & Edge Cases
- [ ] Enforce `lower <= upper` after unit normalization; emit errors with clear guidance when violated.
- [ ] Handle scalar vs. vector inputs explicitly; document recycling rules or prohibit implicit recycling.
- [ ] Decide representation for empty intersections (e.g., `NA` bounds vs. explicit flag) and keep behaviour consistent across R and C++ layers.
- [ ] Document and test behaviour for unbounded intervals (`Inf`, `-Inf`) and ensure midpoint/width behave sensibly (possibly returning `Inf`/`NA`).

### 2.5 Documentation
- [ ] Add roxygen headers for every exported function and the `units_interval` class (include examples, parameter docs, unit-handling notes).
- [ ] Generate Rd files via `roxygen2::roxygenise()`.
- [ ] Create at least one vignette under `vignettes/` (e.g., "Working with unit-aware intervals"), demonstrating construction, arithmetic, unit conversion, and error handling.
- [ ] Prepare a `README.md` summarising goals, installation, and basic examples.

### 2.6 Testing
- [ ] Set up `tests/testthat/` (if not already) with `testthat` edition 3.
- [ ] Cover constructor validation (unit compatibility, reversed bounds, NA handling).
- [ ] Test each arithmetic operation against manually computed results (with units).
- [ ] Add tests for comparison predicates, intersections/unions, and edge cases (empty results, division by zero interval).
- [ ] Include tests confirming unit conversions round-trip correctly and preserve interval semantics.
- [ ] (Optional) Add C++ unit tests using `catch` or similar via `testthat::test_path("src")` helper if needed.

### 2.7 Continuous Validation / QA
- [ ] Add a helper script (e.g., `scripts/run-check.R`) running `devtools::check()` for local validation.
- [ ] Plan for CI (GitHub Actions) once repo is remote: R CMD check on Linux, macOS, Windows.

### 2.8 Package Polish
- [ ] Update `DESCRIPTION` `Suggests:` with `testthat`, `roxygen2`, `knitr`, `rmarkdown`.
- [ ] Run `devtools::document()` to sync `NAMESPACE` and Rd files.
- [ ] Verify `.gitignore` excludes build artefacts (`.Rcheck`, `src/*.o`, tarballs).
- [ ] Remove local `boostedintervals_0.1.0.tar.gz` (ignored in git, but keep clean working tree before commits).
- [ ] Craft CHANGELOG or `NEWS.md` once ready for releases.

### 2.9 Future Enhancements (Post-MVP)
- [ ] Evaluate S4 or vctrs integration for richer type safety.
- [ ] Explore matrix/list intervals and broadcasting rules.
- [ ] Investigate performance tweaks (e.g., batching operations, parallelism, caching unit conversions).
- [ ] Consider UDUNITS/units thread-safety guidance for downstream users.

---

Keep this document updated as tasks progress—checking boxes and noting design decisions will help future sessions quickly rehydrate context.
