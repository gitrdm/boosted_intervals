#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <boost/numeric/interval.hpp>

using Interval = boost::numeric::interval<double>;

namespace {

inline Interval make_interval(double lower, double upper) {
  if (std::isnan(lower) || std::isnan(upper)) {
    Rcpp::stop("Interval bounds cannot be NA");
  }
  if (lower > upper) {
    Rcpp::stop("Lower bound (%f) exceeds upper bound (%f)", lower, upper);
  }
  return Interval(lower, upper);
}

inline std::size_t resolve_length(std::size_t len_a, std::size_t len_b) {
  if (len_a == len_b) {
    return len_a;
  }
  if (len_a == 1) {
    return len_b;
  }
  if (len_b == 1) {
    return len_a;
  }
  Rcpp::stop("Length mismatch: %d vs %d", len_a, len_b);
}

inline void validate_pair_lengths(std::size_t lower, std::size_t upper) {
  if (lower != upper) {
    Rcpp::stop("Lower and upper vectors must have identical lengths (%d vs %d)",
               lower, upper);
  }
}

inline bool interval_contains_zero(const Interval& interval) {
  return interval.lower() <= 0.0 && interval.upper() >= 0.0;
}

inline double midpoint_or_na(const Interval& interval) {
  const double lower = interval.lower();
  const double upper = interval.upper();
  if (!std::isfinite(lower) || !std::isfinite(upper)) {
    return NA_REAL;
  }
  return (lower + upper) / 2.0;
}

inline double get_numeric_with_recycle(const Rcpp::NumericVector& values,
                                       std::size_t idx) {
  if (values.size() == 1) {
    return values[0];
  }
  return values[idx];
}

} // namespace

// [[Rcpp::export]]
Rcpp::List interval_add(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                        Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());

  Rcpp::NumericVector res_lower(n), res_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    Interval res = int1 + int2;
    res_lower[i] = res.lower();
    res_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = res_lower,
                            Rcpp::Named("upper") = res_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_subtract(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                             Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());

  Rcpp::NumericVector res_lower(n), res_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    Interval res = int1 - int2;
    res_lower[i] = res.lower();
    res_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = res_lower,
                            Rcpp::Named("upper") = res_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_multiply(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                             Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());

  Rcpp::NumericVector res_lower(n), res_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    Interval res = int1 * int2;
    res_lower[i] = res.lower();
    res_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = res_lower,
                            Rcpp::Named("upper") = res_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_divide(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                           Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());

  Rcpp::NumericVector res_lower(n), res_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval numerator = make_interval(get_numeric_with_recycle(lower1, i),
                                       get_numeric_with_recycle(upper1, i));
    Interval denominator = make_interval(get_numeric_with_recycle(lower2, i),
                                         get_numeric_with_recycle(upper2, i));
    if (interval_contains_zero(denominator)) {
      Rcpp::stop("Cannot divide by an interval that spans zero");
    }
    Interval res = numerator / denominator;
    res_lower[i] = res.lower();
    res_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = res_lower,
                            Rcpp::Named("upper") = res_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_negate(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector res_lower(n), res_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = -intv;
    res_lower[i] = res.lower();
    res_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = res_lower,
                            Rcpp::Named("upper") = res_upper);
}

// [[Rcpp::export]]
Rcpp::NumericVector interval_width(Rcpp::NumericVector lower,
                                   Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector width(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    width[i] = intv.upper() - intv.lower();
  }
  return width;
}

// [[Rcpp::export]]
Rcpp::NumericVector interval_midpoint(Rcpp::NumericVector lower,
                                      Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector mid(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    mid[i] = midpoint_or_na(intv);
  }
  return mid;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_equals(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                    Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    out[i] = (int1.lower() == int2.lower()) && (int1.upper() == int2.upper());
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_less(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                  Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    out[i] = int1.upper() < int2.lower();
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_less_equal(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                        Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    out[i] = int1.upper() <= int2.lower();
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_greater(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                     Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  return interval_less(lower2, upper2, lower1, upper1);
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_greater_equal(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                           Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  return interval_less_equal(lower2, upper2, lower1, upper1);
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_overlaps(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                      Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    out[i] = (int1.lower() <= int2.upper()) && (int2.lower() <= int1.upper());
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_contains_interval(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                               Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval outer = make_interval(get_numeric_with_recycle(lower1, i),
                                   get_numeric_with_recycle(upper1, i));
    Interval inner = make_interval(get_numeric_with_recycle(lower2, i),
                                   get_numeric_with_recycle(upper2, i));
    out[i] = (outer.lower() <= inner.lower()) && (outer.upper() >= inner.upper());
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_contains_point(Rcpp::NumericVector lower,
                                            Rcpp::NumericVector upper,
                                            Rcpp::NumericVector point) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = resolve_length(lower.size(), point.size());
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(get_numeric_with_recycle(lower, i),
                                  get_numeric_with_recycle(upper, i));
    double value = get_numeric_with_recycle(point, i);
    out[i] = (intv.lower() <= value) && (value <= intv.upper());
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::List interval_intersection(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                 Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    double lower_bound = std::max(int1.lower(), int2.lower());
    double upper_bound = std::min(int1.upper(), int2.upper());
    if (lower_bound > upper_bound) {
      out_lower[i] = NA_REAL;
      out_upper[i] = NA_REAL;
    } else {
      out_lower[i] = lower_bound;
      out_upper[i] = upper_bound;
    }
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_union(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                          Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval int1 = make_interval(get_numeric_with_recycle(lower1, i),
                                  get_numeric_with_recycle(upper1, i));
    Interval int2 = make_interval(get_numeric_with_recycle(lower2, i),
                                  get_numeric_with_recycle(upper2, i));
    out_lower[i] = std::min(int1.lower(), int2.lower());
    out_upper[i] = std::max(int1.upper(), int2.upper());
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}