# API Design Guide for boostedintervals

This document explains the design decisions behind the boostedintervals API to help developers and AI assistants understand the package's structure and usage patterns.

## Core Design Principles

### 1. Unit-Awareness by Default
- All intervals carry explicit units from the `units` package
- Operations automatically propagate and combine units
- No "unitless" intervals - dimensionless quantities use explicit unit `1` or custom-units via install_unit()
- Prevents accidental mixing of incompatible physical quantities

### 2. Conservative Interval Arithmetic
- Uses Boost.Interval's directed rounding for guaranteed containment
- Operations produce bounds that are guaranteed to contain the true result
- Handles edge cases (division by zero-spanning intervals, etc.) gracefully
- Returns `NA` intervals for invalid operations rather than crashing

### 3. Vectorized Operations
- All functions work element-wise on vectors
- Single intervals automatically recycle to match vector lengths
- Maintains R's vectorized programming paradigm
- Efficient for batch operations on measurement data

### 4. S3 Class System
- `units_interval` objects are lists with `lower` and `upper` bounds
- Inherits from base R classes for method dispatch
- Compatible with existing R generic functions (`print`, `length`, etc.)
- Extensible for future enhancements

## Function Naming Conventions

### Consistent Patterns
- `is_*()` - Boolean predicates (is_empty, is_subset, is_whole)
- `as_*()` - Type conversion (as_units_interval, as_numeric_bounds)
- `get_*()` / `set_*()` - Accessors for global state (get_checking_mode, set_rounding_mode)
- `*bounds()` - Extract specific bounds (lower_bounds, upper_bounds)

### Relation Operators
- `possible()` - "Might this relation hold?" (exists some combination)
- `certain()` - "Must this relation hold?" (holds for all combinations)
- `verify()` - Tri-state: TRUE/certain, FALSE/impossible, NA/undecided

## Error Handling Strategy

### Graceful Degradation
- Invalid operations return `NA` intervals instead of stopping execution
- Use `is_empty()` to check for invalid results
- Checking modes allow permissive operation when needed
- Clear error messages for programming mistakes

### Checking Modes
- `"strict"` - Stop on any invalid operation (default)
- `"warn"` - Warn but continue with NA results
- `"permissive"` - Silent operation with NA results

## Integration Patterns

### With units Package
```r
# Convert existing units objects
existing_length <- set_units(5, "m")
interval <- units_interval(existing_length - set_units(0.1, "m"),
                          existing_length + set_units(0.1, "m"))

# Extract for external use
bounds <- as_numeric_bounds(interval, unit = "mm")

# Install custom units
install_unit("people")
install_unit("pizza", "1")  # pizza is dimensionless (count)
```

### With dplyr/tidyverse
```r
library(dplyr)
measurements <- data.frame(
  length = c(10, 20, 30),
  tolerance = c(0.1, 0.2, 0.15)
) %>%
  mutate(
    length_interval = units_interval(length - tolerance,
                                   length + tolerance,
                                   unit = "mm"),
    area = length_interval ^ 2
  )
```

### With Statistical Modeling
```r
# Propagate uncertainty through model predictions
predict_with_uncertainty <- function(model, newdata, se_fit) {
  pred <- predict(model, newdata, se.fit = TRUE)
  units_interval(pred$fit - 2*pred$se.fit,
                pred$fit + 2*pred$se.fit,
                unit = "response_units")
}
```

## Performance Considerations

### When to Use
- Measurement uncertainty quantification
- Tolerance stack-up analysis
- Safety-critical calculations requiring guaranteed bounds
- Educational demonstrations of interval arithmetic

### When to Avoid
- High-performance computing (C++ overhead)
- Exact arithmetic (intervals are conservative)
- Symbolic computation (only numeric evaluation)
- Complex numbers or non-real intervals

### Optimization Tips
- Vectorize operations rather than looping
- Use appropriate checking modes for production code
- Cache converted units to avoid repeated conversions
- Consider pre-allocating result vectors for large datasets

## Extension Points

### Adding New Operations
- Implement C++ functions in `src/` following Boost.Interval patterns
- Add R wrappers in `R/` files with proper roxygen documentation
- Register S3 methods for new generics
- Update NAMESPACE exports

### Custom Units
- Define custom units using `units::as_units()`
- Use dimensionless units for counting quantities
- Ensure unit compatibility before operations

This design enables reliable uncertainty propagation while maintaining R's usability and performance characteristics.