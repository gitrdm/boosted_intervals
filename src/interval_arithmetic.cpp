#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <boost/numeric/interval.hpp>
#include <boost/numeric/interval/transc.hpp>
#include <boost/numeric/interval/utility.hpp>

using IntervalRounding = boost::numeric::interval_lib::save_state<
  boost::numeric::interval_lib::rounded_transc_std<double>>;
using Interval = boost::numeric::interval<
  double,
  boost::numeric::interval_lib::policies<
    IntervalRounding,
    boost::numeric::interval_lib::checking_strict<double>>>;

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

inline int get_integer_with_recycle(const Rcpp::IntegerVector& values,
                                    std::size_t idx) {
  if (values.size() == 1) {
    return values[0];
  }
  return values[idx];
}

inline Interval pow_integer(const Interval& base, int exponent) {
  if (exponent == 0) {
    return Interval(1.0, 1.0);
  }

  const bool negative_power = exponent < 0;
  std::uint32_t power = negative_power ? static_cast<std::uint32_t>(-static_cast<long long>(exponent))
                                       : static_cast<std::uint32_t>(exponent);

  Interval result(1.0, 1.0);
  Interval factor = base;
  while (power > 0) {
    if (power & 1U) {
      result *= factor;
    }
    power >>= 1U;
    if (power > 0) {
      factor *= factor;
    }
  }

  if (negative_power) {
    return Interval(1.0, 1.0) / result;
  }
  return result;
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
Rcpp::List interval_exp(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::exp(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_expm1(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  const Interval one(1.0, 1.0);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::exp(intv) - one;
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_log(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() <= 0.0) {
      Rcpp::stop("log is undefined for intervals at or below zero");
    }
    Interval res = boost::numeric::log(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_log10(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() <= 0.0) {
      Rcpp::stop("log10 is undefined for intervals at or below zero");
    }
    Interval res = boost::numeric::log(intv) / std::log(10.0);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_log1p(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  const Interval one(1.0, 1.0);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() <= -1.0) {
      Rcpp::stop("log1p is undefined for intervals at or below -1");
    }
    Interval res = boost::numeric::log(intv + one);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_log2(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() <= 0.0) {
      Rcpp::stop("log2 is undefined for intervals at or below zero");
    }
    Interval res = boost::numeric::log(intv) / std::log(2.0);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_sin(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::sin(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_cos(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
  Interval intv = make_interval(lower[i], upper[i]);
  Interval res = boost::numeric::cos(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
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

// [[Rcpp::export]]
Rcpp::List interval_abs(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::abs(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }
  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_sqrt(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() < 0.0) {
      Rcpp::stop("Cannot take square root of an interval with negative lower bound");
    }
    Interval res = boost::numeric::sqrt(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }
  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_pow(Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                        Rcpp::IntegerVector exponent) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = resolve_length(lower.size(), exponent.size());

  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(get_numeric_with_recycle(lower, i),
                                  get_numeric_with_recycle(upper, i));
    const int power = get_integer_with_recycle(exponent, i);
    if (power == 0 && interval_contains_zero(intv)) {
      Rcpp::stop("0^0 is undefined for interval exponentiation");
    }
    if (power < 0 && interval_contains_zero(intv)) {
      Rcpp::stop("Negative exponents are not defined for intervals spanning zero");
    }
    Interval res = pow_integer(intv, power);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_zero_in(Rcpp::NumericVector lower,
                                     Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    if (Rcpp::NumericVector::is_na(lower[i]) || Rcpp::NumericVector::is_na(upper[i])) {
      out[i] = NA_LOGICAL;
      continue;
    }
    Interval intv = make_interval(lower[i], upper[i]);
    out[i] = interval_contains_zero(intv);
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_is_empty(Rcpp::NumericVector lower,
                                      Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    out[i] = Rcpp::NumericVector::is_na(lower[i]) || Rcpp::NumericVector::is_na(upper[i]);
  }
  return out;
}

namespace {

inline bool is_na_interval(double lower, double upper) {
  return Rcpp::NumericVector::is_na(lower) || Rcpp::NumericVector::is_na(upper);
}

} // namespace

// [[Rcpp::export]]
Rcpp::LogicalVector interval_subset(Rcpp::NumericVector outer_lower, Rcpp::NumericVector outer_upper,
                                    Rcpp::NumericVector inner_lower, Rcpp::NumericVector inner_upper) {
  validate_pair_lengths(outer_lower.size(), outer_upper.size());
  validate_pair_lengths(inner_lower.size(), inner_upper.size());
  const std::size_t n = resolve_length(outer_lower.size(), inner_lower.size());

  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double o_lower = get_numeric_with_recycle(outer_lower, i);
    const double o_upper = get_numeric_with_recycle(outer_upper, i);
    const double i_lower = get_numeric_with_recycle(inner_lower, i);
    const double i_upper = get_numeric_with_recycle(inner_upper, i);

    const bool outer_na = is_na_interval(o_lower, o_upper);
    const bool inner_na = is_na_interval(i_lower, i_upper);

    if (inner_na) {
      out[i] = true;
      continue;
    }
    if (outer_na) {
      out[i] = false;
      continue;
    }

    Interval outer = make_interval(o_lower, o_upper);
    Interval inner = make_interval(i_lower, i_upper);
    out[i] = (outer.lower() <= inner.lower()) && (outer.upper() >= inner.upper());
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector interval_proper_subset(Rcpp::NumericVector outer_lower, Rcpp::NumericVector outer_upper,
                                           Rcpp::NumericVector inner_lower, Rcpp::NumericVector inner_upper) {
  validate_pair_lengths(outer_lower.size(), outer_upper.size());
  validate_pair_lengths(inner_lower.size(), inner_upper.size());
  const std::size_t n = resolve_length(outer_lower.size(), inner_lower.size());

  Rcpp::LogicalVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double o_lower = get_numeric_with_recycle(outer_lower, i);
    const double o_upper = get_numeric_with_recycle(outer_upper, i);
    const double i_lower = get_numeric_with_recycle(inner_lower, i);
    const double i_upper = get_numeric_with_recycle(inner_upper, i);

    const bool outer_na = is_na_interval(o_lower, o_upper);
    const bool inner_na = is_na_interval(i_lower, i_upper);

    if (inner_na) {
      out[i] = !outer_na;
      continue;
    }
    if (outer_na) {
      out[i] = false;
      continue;
    }

    Interval outer = make_interval(o_lower, o_upper);
    Interval inner = make_interval(i_lower, i_upper);
    const bool contains = (outer.lower() <= inner.lower()) && (outer.upper() >= inner.upper());
    const bool equal_bounds = (outer.lower() == inner.lower()) && (outer.upper() == inner.upper());
    out[i] = contains && !equal_bounds;
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector interval_radius(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    if (Rcpp::NumericVector::is_na(lower[i]) || Rcpp::NumericVector::is_na(upper[i])) {
      out[i] = NA_REAL;
      continue;
    }
    Interval intv = make_interval(lower[i], upper[i]);
    out[i] = (intv.upper() - intv.lower()) / 2.0;
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector interval_norm(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    if (Rcpp::NumericVector::is_na(lower[i]) || Rcpp::NumericVector::is_na(upper[i])) {
      out[i] = NA_REAL;
      continue;
    }
    Interval intv = make_interval(lower[i], upper[i]);
    out[i] = boost::numeric::norm(intv);
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector interval_mag(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    if (Rcpp::NumericVector::is_na(lower[i]) || Rcpp::NumericVector::is_na(upper[i])) {
      out[i] = NA_REAL;
      continue;
    }
    Interval intv = make_interval(lower[i], upper[i]);
    Interval abs_intv = boost::numeric::abs(intv);
    out[i] = abs_intv.upper();
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector interval_mig(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    if (Rcpp::NumericVector::is_na(lower[i]) || Rcpp::NumericVector::is_na(upper[i])) {
      out[i] = NA_REAL;
      continue;
    }
    Interval intv = make_interval(lower[i], upper[i]);
    Interval abs_intv = boost::numeric::abs(intv);
    out[i] = abs_intv.lower();
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector interval_distance(Rcpp::NumericVector lower1, Rcpp::NumericVector upper1,
                                      Rcpp::NumericVector lower2, Rcpp::NumericVector upper2) {
  validate_pair_lengths(lower1.size(), upper1.size());
  validate_pair_lengths(lower2.size(), upper2.size());
  const std::size_t n = resolve_length(lower1.size(), lower2.size());

  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double a_lower = get_numeric_with_recycle(lower1, i);
    const double a_upper = get_numeric_with_recycle(upper1, i);
    const double b_lower = get_numeric_with_recycle(lower2, i);
    const double b_upper = get_numeric_with_recycle(upper2, i);

    if (is_na_interval(a_lower, a_upper) || is_na_interval(b_lower, b_upper)) {
      out[i] = NA_REAL;
      continue;
    }

    Interval a = make_interval(a_lower, a_upper);
    Interval b = make_interval(b_lower, b_upper);

    if (a.upper() < b.lower()) {
      out[i] = b.lower() - a.upper();
    } else if (b.upper() < a.lower()) {
      out[i] = a.lower() - b.upper();
    } else {
      out[i] = 0.0;
    }
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::List interval_tan(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::tan(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_asin(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() < -1.0 || intv.upper() > 1.0) {
      Rcpp::stop("asin is defined only for intervals within [-1, 1]");
    }
    Interval res = boost::numeric::asin(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_acos(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() < -1.0 || intv.upper() > 1.0) {
      Rcpp::stop("acos is defined only for intervals within [-1, 1]");
    }
    Interval res = boost::numeric::acos(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_atan(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::atan(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_sinh(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::sinh(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_cosh(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::cosh(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_tanh(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::tanh(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_asinh(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    Interval res = boost::numeric::asinh(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_acosh(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() < 1.0) {
      Rcpp::stop("acosh is defined only for intervals with lower bound >= 1");
    }
    Interval res = boost::numeric::acosh(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}

// [[Rcpp::export]]
Rcpp::List interval_atanh(Rcpp::NumericVector lower, Rcpp::NumericVector upper) {
  validate_pair_lengths(lower.size(), upper.size());
  const std::size_t n = lower.size();
  Rcpp::NumericVector out_lower(n), out_upper(n);
  for (std::size_t i = 0; i < n; ++i) {
    Interval intv = make_interval(lower[i], upper[i]);
    if (intv.lower() <= -1.0 || intv.upper() >= 1.0) {
      Rcpp::stop("atanh is defined only for intervals within (-1, 1)");
    }
    Interval res = boost::numeric::atanh(intv);
    out_lower[i] = res.lower();
    out_upper[i] = res.upper();
  }

  return Rcpp::List::create(Rcpp::Named("lower") = out_lower,
                            Rcpp::Named("upper") = out_upper);
}
