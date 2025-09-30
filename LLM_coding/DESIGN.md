# R Package Design Document: `unitsinterval`  
## Purpose  
Create an R package that combines interval arithmetic (via Boost.Interval and Rcpp) with unit management (via the units R package) for engineering, scientific, and statistical workflows. This document is a comprehensive design discussion to guide development, collaboration, and LLM coding agents.

---

## 1. **Overview**

- **Goal:** Allow R users to represent, compute, and manipulate intervals with associated units using simple, idiomatic R code.
- **Core Technologies:**
  - **Interval arithmetic:** [Boost.Interval](https://www.boost.org/doc/libs/release/libs/numeric/interval/doc/interval.htm) via Rcpp.
  - **Units management:** [units](https://github.com/r-quantities/units) R package.
- **Motivation:**  
  - R lacks a package combining rigorous interval arithmetic and flexible, extensible unit management.
  - Boost.Interval provides C++ rigor and performance; units provides R-native interactivity and extensibility.

---

## 2. **User-Facing API**

### 2.1. Core Class

- S3 class: `units_interval`
  - Represents a closed interval with units.
  - Structure:  
    ```r
    structure(list(
      lower = units::set_units(...),
      upper = units::set_units(...),
      unit = ... # string or units object
    ), class = "units_interval")
    ```
  - Optionally, can use S4 for future extensibility (see section 7).

### 2.2. Construction

- `units_interval(lower, upper, unit)`
  - Accepts numeric or units objects for bounds.
  - Validates that `lower` and `upper` share the same unit.

### 2.3. Methods

- Arithmetic operators: `+`, `-`, `*`, `/`, unary `-`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=` (where meaningful)
- Interval-specific: `width()`, `midpoint()`, `contains()`, `intersect()`, `union()`
- Unit conversion: `convert_units(x, new_unit)` (uses units package)
- Print/format: Custom printing for easy inspection

### 2.4. Vectorization

- Support for vectorized intervals (e.g., vectors of intervals)
- Optionally, support for list of intervals or interval matrices

---

## 3. **Internal Structure & Workflow**

### 3.1. Interval Arithmetic

- **Delegated to C++ via Rcpp:**  
  - All interval arithmetic (addition, multiplication, etc.) performed in C++ using Boost.Interval.
  - Rcpp wrappers expose C++ interval operations to R.

### 3.2. Unit Management

- **Remain in R:**  
  - Units assigned, validated, and converted in R using the units package.
  - C++ code receives only the numeric bounds (converted to matching units in R before passing to C++).

### 3.3. Data Flow Example

1. User constructs a `units_interval` object in R.
2. Arithmetic operation is requested.
3. R method:
   - Validates unit compatibility using units package.
   - Converts bounds to numeric values with common units (if needed).
   - Calls C++ via Rcpp for interval arithmetic.
   - Returns new `units_interval` object with result bounds and unit metadata.

---

## 4. **Error Checking, Validation, and Edge Cases**

- Ensure lower ≤ upper for intervals.
- Units must match (or be convertible) for interval ops.
- Division by intervals containing zero is forbidden (Boost.Interval convention).
- Unit conversion errors handled gracefully (using units package exceptions).

---

## 5. **Parallelism and Performance**

- Interval arithmetic in C++ is thread-safe (Boost.Interval).
- All unit conversion and management occurs in R; avoid threading issues with UDUNITS2.
- Vectorized operations for performance.

---

## 6. **Extensibility & Future Features**

- S4 class support if stricter validation or inheritance needed.
- Support for uncertainty quantification (e.g., attach uncertainty to intervals).
- Support for multi-dimensional intervals (boxes, regions).
- Integration with other units-based R packages (quantities, errors).
- Advanced printing, plotting, and summary statistics.

---

## 7. **S3 vs S4 Discussion**

- **S3 chosen for initial implementation:**  
  - Simple, flexible, aligns with units package.
- **S4 migration possible:**  
  - If package grows in complexity, migration to S4 for formal validation and inheritance.

---

## 8. **Dependencies & Installation**

- **Boost headers:**  
  - Include required Boost.Interval headers in `inst/include`.
- **Rcpp:**  
  - For C++/R interface.
- **units:**  
  - For unit management in R. Add to `Imports` in DESCRIPTION.
- **Other dependencies:**  
  - Optional: testthat (testing), vctrs (vectorization), methods (for S4).

---

## 9. **Testing**

- Unit tests for interval arithmetic (C++ and R).
- Tests for unit conversion, compatibility, and error handling.
- Edge cases (empty intervals, invalid units, etc.).

---

## 10. **Documentation & Examples**

- Vignettes demonstrating typical engineering/scientific/statistical workflows.
- Example: tolerance intervals for physical measurements, propagation of uncertainty, etc.

---

## 11. **Open Questions & Review Points**

- S3 vs S4 class for future extensibility?
- How to best represent/print intervals with units (formatting)?
- Should interval endpoints be restricted to numeric only, or support units objects directly?
- Support for vectorized intervals or matrices?
- Error handling best practices (conversion errors, arithmetic exceptions)?

---

## 12. **Contributing & LLM Coding Agent Guidance**

- This document provides a roadmap for LLM coding agents:
  - When asked for code, refer to API structure and workflow above.
  - Use Boost.Interval for all interval arithmetic.
  - Use units R package for units handling.
  - Implement S3 class initially, but keep extensibility in mind.
  - Follow error checking and validation rules.
  - Provide code examples and vignettes as needed.

---

## 13. **References**
- [Boost.Interval](https://www.boost.org/doc/libs/release/libs/numeric/interval/doc/interval.htm)
- [units R package](https://github.com/r-quantities/units)
- [Rcpp](https://cran.r-project.org/package=Rcpp)

---

## 14. **Revision History**

- v0.1 - Initial design document (2025-09-30)