#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <numeric>

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(
      Rcpp::NumericVector x,
      int width,
      int by,
      Rcpp::String const& align,
      Rcpp::LogicalVector na_rm,
      Rcpp::Nullable<Rcpp::NumericVector> weights
  ) {

    if (width < 1) {
      Rcpp::stop("Window 'width' must be 1 or larger");
    }
    if (width > x.size()) {
      Rcpp::stop("Window 'width' cannot be larger than 'x'");
    }
    if (by < 1) {
      Rcpp::stop("Increment 'by' must be 1 or larger");
    }
    if (by > x.size()) {
      Rcpp::stop("Increment 'by' cannot be larger than 'x'");
    }

    // Initialize private vars
    x_ = x;
    width_ = width;
    by_ = by;

    if (na_rm.size() != 1 || na_rm[0] == NA_LOGICAL) {
      Rcpp::stop("'na_rm' must be a single TRUE or FALSE value");
    }
    na_rm_ = static_cast<bool>(na_rm[0]);

    weights_ = Rcpp::rep(1.0, width_);

    // Default weights
    if (!weights.isNull()) {
      // See:  https://stackoverflow.com/questions/43388698/rcpp-how-can-i-get-the-size-of-a-rcppnullable-numericvector
      Rcpp::NumericVector w(weights.get());
      if (w.size() != width_) {
        Rcpp::stop("'weights' must be either NULL or a vector of the same length as the window 'width'");
      }
      for (int i = 0; i < width_; ++i) {
        if (w[i] < 0) {
          Rcpp::stop("All 'weights' must be positive values or zero. Negative weights are not supported.");
        };
      }
      // See:  https://en.cppreference.com/w/cpp/algorithm/accumulate
      double weights_sum = std::accumulate(w.begin(), w.end(), (double)0);
      if ( weights_sum == 0 ) {
        Rcpp::stop("All 'weights' are zero. Please include non-zero values in 'weights'.");
      }
      double normalization = (double)width_ / weights_sum;
      for (int i = 0; i < width_; ++i) {
        weights_[i] = w[i] * normalization;
      }
    }

    // Additional private vars
    length_ = x.size();
    half_width_ = width / 2;   // truncated division rounds down

    // Initialize start and end
    if (align == "left") {
      align_code_ = -1;
      start_ = 0;
      end_ = length_ - (width - 1);
    } else if (align == "center") {
      align_code_ = 0;
      start_ = half_width_;
      end_ = length_ - half_width_;
    } else if (align == "right") {
      align_code_ = 1;
      start_ = width - 1;
      end_ = length_;
    } else {
      Rcpp::stop("Window alignment 'align' must be either 'left', 'center' or 'right'");
    }

  }

  // Rolling Hampel filter
  Rcpp::NumericVector hampel() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowHampel(i);
    }
    return out;
  }

  // Rolling Median Absolute Deviation
  Rcpp::NumericVector MAD() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMAD(i);
    }
    return out;
  }

  // Rolling Maximum
  Rcpp::NumericVector max() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMax(i);
    }
    return out;
  }

  // Rolling Mean
  Rcpp::NumericVector mean() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMean(i);
    }
    return out;
  }

  // Rolling Median
  Rcpp::NumericVector median() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMedian(i);
    }
    return out;
  }

  // Rolling Minimum
  Rcpp::NumericVector min() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMin(i);
    }
    return out;
  }

  // Rolling Product
  Rcpp::NumericVector prod() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowProd(i);
    }
    return out;
  }

  // Rolling Standard Deviation
  Rcpp::NumericVector sd() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = sqrt(windowVar(i));
    }
    return out;
  }

  // Rolling Sum
  Rcpp::NumericVector sum() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowSum(i);
    }
    return out;
  }

  // Rolling Variance
  Rcpp::NumericVector var() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowVar(i);
    }
    return out;
  }

private:

  Rcpp::NumericVector x_;        // data
  int width_;                    // window width
  int by_;                       // increment
  int align_code_;               // alignment
  bool na_rm_;                   // NA removal
  Rcpp::NumericVector weights_;  // window weights
  int length_;                   // data length
  int half_width_;               // window half-width
  int start_;                    // start index
  int end_;                      // end index

  int windowIndex(int index, int i) const {
    switch (align_code_) {
    case -1:
      return index + i;
    case 0:
      return index - half_width_ + i;
    case 1:
      return index - (width_ - 1) + i;
    default:
      Rcpp::stop("Invalid internal alignment code.");
    }
  }

  bool collectWindowValues(const int& index,
                           Rcpp::NumericVector& tmp,
                           int& valid_count) {

    valid_count = 0;

    for (int i = 0; i < width_; ++i) {
      int s = windowIndex(index, i);

      if (s < 0 || s >= length_) {
        if (!na_rm_) return false;
      } else if (ISNAN(x_[s])) {
        if (!na_rm_) return false;
      } else {
        tmp[valid_count++] = x_[s];
      }
    }

    return true;
  }

  // Window Hampel filter
  double windowHampel(const int &index) {
    const double kappa = 1.4826;

    double median = windowMedian(index);
    if (ISNAN(median)) {
      return NA_REAL;
    }

    double MAD = windowMAD(index);
    if (ISNAN(MAD)) {
      return NA_REAL;
    }

    double deviation = std::fabs(x_[index] - median);

    if (MAD == 0) {
      if (deviation == 0) {
        return 0.0;
      } else {
        return R_PosInf;
      }
    }

    return deviation / (kappa * MAD);
  }

  // Window Median Absolute Deviation
  double windowMAD(const int &index) {
    double median = windowMedian(index);
    if (ISNAN(median)) {
      return NA_REAL;
    }

    int valid_count = 0;
    Rcpp::NumericVector values(width_);

    if (!collectWindowValues(index, values, valid_count)) {
      return NA_REAL;
    }

    if (valid_count == 0) {
      return NA_REAL;
    }

    for (int i = 0; i < valid_count; ++i) {
      values[i] = std::fabs(values[i] - median);
    }

    int mid = valid_count / 2;

    if (valid_count % 2 == 1) {
      std::nth_element(values.begin(), values.begin() + mid, values.begin() + valid_count);
      return values[mid];
    } else {
      std::nth_element(values.begin(), values.begin() + mid, values.begin() + valid_count);
      double upper = values[mid];

      std::nth_element(values.begin(), values.begin() + (mid - 1), values.begin() + valid_count);
      double lower = values[mid - 1];

      return (lower + upper) / 2.0;
    }
  }

  // Window Maximum
  double windowMax(const int &index) {
    Rcpp::NumericVector values(width_);
    int valid_count = 0;

    if (!collectWindowValues(index, values, valid_count)) {
      return NA_REAL;
    }

    if (valid_count == 0) {
      return NA_REAL;
    }

    double current_max = values[0];
    for (int i = 1; i < valid_count; ++i) {
      if (values[i] > current_max) {
        current_max = values[i];
      }
    }

    return current_max;
  }

  // Window Mean
  double windowMean(const int &index) {
    int na_count = 0;
    double weighted_sum = 0.0;
    double used_weight_sum = 0.0;

    // Don't use collectWindowValues() because weights must stay aligned
    // with the original window index 'i'.
    for (int i = 0; i < width_; ++i) {
      int s = windowIndex(index, i);

      if (s < 0 || s >= length_) {
        if (!na_rm_) {
          return NA_REAL;
        }
        na_count += 1;
      } else if (ISNAN(x_[s])) {
        if (!na_rm_) {
          return NA_REAL;
        }
        na_count += 1;
      } else {
        weighted_sum += x_[s] * weights_[i];
        used_weight_sum += weights_[i];
      }
    }

    if (na_count == width_) {
      return NA_REAL;
    }

    if (used_weight_sum == 0.0) {
      return NA_REAL;
    }

    return weighted_sum / used_weight_sum;
  }

  // Window Median
  double windowMedian(const int &index) {
    int valid_count = 0;
    Rcpp::NumericVector values(width_);

    if (!collectWindowValues(index, values, valid_count)) {
      return NA_REAL;
    }

    if (valid_count == 0) {
      return NA_REAL;
    }

    int mid = valid_count / 2;

    if (valid_count % 2 == 1) {
      std::nth_element(values.begin(), values.begin() + mid, values.begin() + valid_count);
      return values[mid];
    } else {
      std::nth_element(values.begin(), values.begin() + mid, values.begin() + valid_count);
      double upper = values[mid];

      std::nth_element(values.begin(), values.begin() + (mid - 1), values.begin() + valid_count);
      double lower = values[mid - 1];

      return (lower + upper) / 2.0;
    }
  }

  // Window Minimum
  double windowMin(const int &index) {
    Rcpp::NumericVector values(width_);
    int valid_count = 0;

    if (!collectWindowValues(index, values, valid_count)) {
      return NA_REAL;
    }

    if (valid_count == 0) {
      return NA_REAL;
    }

    double current_min = values[0];
    for (int i = 1; i < valid_count; ++i) {
      if (values[i] < current_min) {
        current_min = values[i];
      }
    }

    return current_min;
  }

  // Window Product
  double windowProd(const int &index) {
    Rcpp::NumericVector values(width_);
    int valid_count = 0;

    if (!collectWindowValues(index, values, valid_count)) {
      return NA_REAL;
    }

    if (valid_count == 0) {
      return NA_REAL;
    }

    double product = 1.0;
    for (int i = 0; i < valid_count; ++i) {
      product *= values[i];
    }

    return product;
  }

  // Window Sum
  double windowSum(const int &index) {
    Rcpp::NumericVector values(width_);
    int valid_count = 0;

    if (!collectWindowValues(index, values, valid_count)) {
      return NA_REAL;
    }

    if (valid_count == 0) {
      return NA_REAL;
    }

    double total = 0.0;
    for (int i = 0; i < valid_count; ++i) {
      total += values[i];
    }

    return total;
  }

  // Window Variance
  double windowVar(const int &index) {
    Rcpp::NumericVector values(width_);
    int valid_count = 0;

    if (!collectWindowValues(index, values, valid_count)) {
      return NA_REAL;
    }

    if (valid_count < 2) {
      return NA_REAL;
    }

    double window_mean = 0.0;
    for (int i = 0; i < valid_count; ++i) {
      window_mean += values[i];
    }
    window_mean /= valid_count;

    double variance = 0.0;
    for (int i = 0; i < valid_count; ++i) {
      double deviation = values[i] - window_mean;
      variance += deviation * deviation;
    }

    return variance / (valid_count - 1);
  }

};

// [[Rcpp::export(".roll_hampel_cpp")]]
Rcpp::NumericVector roll_hampel_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.hampel();
}

// [[Rcpp::export(".roll_MAD_cpp")]]
Rcpp::NumericVector roll_MAD_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.MAD();
}

// [[Rcpp::export(".roll_max_cpp")]]
Rcpp::NumericVector roll_max_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.max();
}

// [[Rcpp::export(".roll_mean_cpp")]]
Rcpp::NumericVector roll_mean_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0),
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue
) {
  Roll roll;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.mean();
}

// [[Rcpp::export(".roll_median_cpp")]]
Rcpp::NumericVector roll_median_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.median();
}

// [[Rcpp::export(".roll_min_cpp")]]
Rcpp::NumericVector roll_min_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.min();
}

// [[Rcpp::export(".roll_prod_cpp")]]
Rcpp::NumericVector roll_prod_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.prod();
}

// [[Rcpp::export(".roll_sd_cpp")]]
Rcpp::NumericVector roll_sd_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.sd();
}

// [[Rcpp::export(".roll_sum_cpp")]]
Rcpp::NumericVector roll_sum_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.sum();
}

// [[Rcpp::export(".roll_var_cpp")]]
Rcpp::NumericVector roll_var_cpp(
    Rcpp::NumericVector x,
    int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.var();
}






