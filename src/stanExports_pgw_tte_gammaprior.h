// Generated by rstantools.  Do not edit by hand.

/*
    BWSPsignal is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BWSPsignal is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BWSPsignal.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.32.2
#include <stan/model/model_header.hpp>
namespace model_pgw_tte_gammaprior_namespace {
using stan::model::model_base_crtp;
using namespace stan::math;
stan::math::profile_map profiles__;
static constexpr std::array<const char*, 38> locations_array__ =
  {" (found before start of program)",
  " (in 'string', line 49, column 2 to column 22)",
  " (in 'string', line 50, column 2 to column 19)",
  " (in 'string', line 51, column 2 to column 22)",
  " (in 'string', line 54, column 2 to column 64)",
  " (in 'string', line 55, column 2 to column 62)",
  " (in 'string', line 56, column 2 to column 64)",
  " (in 'string', line 57, column 2 to column 62)",
  " (in 'string', line 58, column 2 to column 64)",
  " (in 'string', line 59, column 2 to column 62)",
  " (in 'string', line 60, column 2 to column 45)",
  " (in 'string', line 61, column 2 to column 48)",
  " (in 'string', line 63, column 4 to column 49)",
  " (in 'string', line 62, column 24 to line 64, column 3)",
  " (in 'string', line 62, column 2 to line 64, column 3)",
  " (in 'string', line 66, column 4 to column 50)",
  " (in 'string', line 65, column 24 to line 67, column 3)",
  " (in 'string', line 65, column 2 to line 67, column 3)",
  " (in 'string', line 35, column 2 to column 28)",
  " (in 'string', line 36, column 2 to column 28)",
  " (in 'string', line 37, column 20 to column 30)",
  " (in 'string', line 37, column 2 to column 35)",
  " (in 'string', line 38, column 20 to column 30)",
  " (in 'string', line 38, column 2 to column 35)",
  " (in 'string', line 39, column 2 to column 27)",
  " (in 'string', line 40, column 2 to column 26)",
  " (in 'string', line 41, column 2 to column 27)",
  " (in 'string', line 42, column 2 to column 26)",
  " (in 'string', line 43, column 2 to column 27)",
  " (in 'string', line 44, column 2 to column 26)",
  " (in 'string', line 9, column 2 to column 136)",
  " (in 'string', line 8, column 56 to line 10, column 3)",
  " (in 'string', line 15, column 2 to column 42)",
  " (in 'string', line 14, column 57 to line 16, column 3)",
  " (in 'string', line 23, column 2 to column 26)",
  " (in 'string', line 22, column 59 to line 24, column 3)",
  " (in 'string', line 28, column 2 to column 24)",
  " (in 'string', line 27, column 58 to line 29, column 3)"};
template <bool propto__, typename T0__, typename T1__, typename T2__,
          typename T3__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>,
                              stan::is_stan_scalar<T2__>,
                              stan::is_stan_scalar<T3__>>* = nullptr>
stan::promote_args_t<T0__, T1__, T2__, T3__>
pgw_lpdf(const T0__& t, const T1__& theta, const T2__& nu, const T3__& gamma,
         std::ostream* pstream__);
template <typename T0__, typename T1__, typename T2__, typename T3__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>,
                              stan::is_stan_scalar<T2__>,
                              stan::is_stan_scalar<T3__>>* = nullptr>
stan::promote_args_t<T0__, T1__, T2__, T3__>
pgW_lccdf(const T0__& t, const T1__& theta, const T2__& nu, const T3__&
          gamma, std::ostream* pstream__);
template <typename T0__, typename T1__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>>* = nullptr>
stan::promote_args_t<T0__, T1__>
gamma_expect_stdev_to_alpha(const T0__& expect, const T1__& stdev,
                            std::ostream* pstream__);
template <typename T0__, typename T1__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>>* = nullptr>
stan::promote_args_t<T0__, T1__>
gamma_expect_stdev_to_beta(const T0__& expect, const T1__& stdev,
                           std::ostream* pstream__);
template <bool propto__, typename T0__, typename T1__, typename T2__,
          typename T3__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>,
                              stan::is_stan_scalar<T2__>,
                              stan::is_stan_scalar<T3__>>*>
stan::promote_args_t<T0__, T1__, T2__, T3__>
pgw_lpdf(const T0__& t, const T1__& theta, const T2__& nu, const T3__& gamma,
         std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__, T1__, T2__, T3__>;
  int current_statement__ = 0;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 30;
    return ((((((stan::math::log(nu) - stan::math::log(gamma)) - (nu *
           stan::math::log(theta))) + ((nu - 1) * stan::math::log(t))) + (((1
           / gamma) - 1) *
           stan::math::log((1 + stan::math::pow((t / theta), nu))))) + 1) -
           stan::math::pow((1 + stan::math::pow((t / theta), nu)), (1 /
             gamma)));
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
template <typename T0__, typename T1__, typename T2__, typename T3__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>,
                              stan::is_stan_scalar<T2__>,
                              stan::is_stan_scalar<T3__>>*>
stan::promote_args_t<T0__, T1__, T2__, T3__>
pgW_lccdf(const T0__& t, const T1__& theta, const T2__& nu, const T3__&
          gamma, std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__, T1__, T2__, T3__>;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 32;
    return (1 -
           stan::math::pow((1 + stan::math::pow((t / theta), nu)), (1 /
             gamma)));
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
template <typename T0__, typename T1__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>>*>
stan::promote_args_t<T0__, T1__>
gamma_expect_stdev_to_alpha(const T0__& expect, const T1__& stdev,
                            std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__, T1__>;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 34;
    return (stan::math::pow(expect, 2) / stan::math::pow(stdev, 2));
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
template <typename T0__, typename T1__,
          stan::require_all_t<stan::is_stan_scalar<T0__>,
                              stan::is_stan_scalar<T1__>>*>
stan::promote_args_t<T0__, T1__>
gamma_expect_stdev_to_beta(const T0__& expect, const T1__& stdev,
                           std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__, T1__>;
  int current_statement__ = 0;
  static constexpr bool propto__ = true;
  // suppress unused var warning
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  // suppress unused var warning
  (void) DUMMY_VAR__;
  try {
    current_statement__ = 36;
    return (expect / stan::math::pow(stdev, 2));
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
  }
}
#include <stan_meta_header.hpp>
class model_pgw_tte_gammaprior final : public model_base_crtp<model_pgw_tte_gammaprior> {
private:
  int N_status_e;
  int N_status_c;
  Eigen::Matrix<double,-1,1> te_data__;
  Eigen::Matrix<double,-1,1> tc_data__;
  double t_expect;
  double t_stdev;
  double n_expect;
  double n_stdev;
  double g_expect;
  double g_stdev;
  Eigen::Map<Eigen::Matrix<double,-1,1>> te{nullptr, 0};
  Eigen::Map<Eigen::Matrix<double,-1,1>> tc{nullptr, 0};
public:
  ~model_pgw_tte_gammaprior() {}
  model_pgw_tte_gammaprior(stan::io::var_context& context__, unsigned int
                           random_seed__ = 0, std::ostream*
                           pstream__ = nullptr) : model_base_crtp(0) {
    int current_statement__ = 0;
    using local_scalar_t__ = double;
    boost::ecuyer1988 base_rng__ =
      stan::services::util::create_rng(random_seed__, 0);
    // suppress unused var warning
    (void) base_rng__;
    static constexpr const char* function__ =
      "model_pgw_tte_gammaprior_namespace::model_pgw_tte_gammaprior";
    // suppress unused var warning
    (void) function__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      current_statement__ = 18;
      context__.validate_dims("data initialization", "N_status_e", "int",
        std::vector<size_t>{});
      N_status_e = std::numeric_limits<int>::min();
      current_statement__ = 18;
      N_status_e = context__.vals_i("N_status_e")[(1 - 1)];
      current_statement__ = 18;
      stan::math::check_greater_or_equal(function__, "N_status_e",
        N_status_e, 1);
      current_statement__ = 19;
      context__.validate_dims("data initialization", "N_status_c", "int",
        std::vector<size_t>{});
      N_status_c = std::numeric_limits<int>::min();
      current_statement__ = 19;
      N_status_c = context__.vals_i("N_status_c")[(1 - 1)];
      current_statement__ = 19;
      stan::math::check_greater_or_equal(function__, "N_status_c",
        N_status_c, 0);
      current_statement__ = 20;
      stan::math::validate_non_negative_index("te", "N_status_e", N_status_e);
      current_statement__ = 21;
      context__.validate_dims("data initialization", "te", "double",
        std::vector<size_t>{static_cast<size_t>(N_status_e)});
      te_data__ = Eigen::Matrix<double,-1,1>::Constant(N_status_e,
                    std::numeric_limits<double>::quiet_NaN());
      new (&te) Eigen::Map<Eigen::Matrix<double,-1,1>>(te_data__.data(),
        N_status_e);
      {
        std::vector<local_scalar_t__> te_flat__;
        current_statement__ = 21;
        te_flat__ = context__.vals_r("te");
        current_statement__ = 21;
        pos__ = 1;
        current_statement__ = 21;
        for (int sym1__ = 1; sym1__ <= N_status_e; ++sym1__) {
          current_statement__ = 21;
          stan::model::assign(te, te_flat__[(pos__ - 1)],
            "assigning variable te", stan::model::index_uni(sym1__));
          current_statement__ = 21;
          pos__ = (pos__ + 1);
        }
      }
      current_statement__ = 21;
      stan::math::check_greater_or_equal(function__, "te", te, 0);
      current_statement__ = 22;
      stan::math::validate_non_negative_index("tc", "N_status_c", N_status_c);
      current_statement__ = 23;
      context__.validate_dims("data initialization", "tc", "double",
        std::vector<size_t>{static_cast<size_t>(N_status_c)});
      tc_data__ = Eigen::Matrix<double,-1,1>::Constant(N_status_c,
                    std::numeric_limits<double>::quiet_NaN());
      new (&tc) Eigen::Map<Eigen::Matrix<double,-1,1>>(tc_data__.data(),
        N_status_c);
      {
        std::vector<local_scalar_t__> tc_flat__;
        current_statement__ = 23;
        tc_flat__ = context__.vals_r("tc");
        current_statement__ = 23;
        pos__ = 1;
        current_statement__ = 23;
        for (int sym1__ = 1; sym1__ <= N_status_c; ++sym1__) {
          current_statement__ = 23;
          stan::model::assign(tc, tc_flat__[(pos__ - 1)],
            "assigning variable tc", stan::model::index_uni(sym1__));
          current_statement__ = 23;
          pos__ = (pos__ + 1);
        }
      }
      current_statement__ = 23;
      stan::math::check_greater_or_equal(function__, "tc", tc, 0);
      current_statement__ = 24;
      context__.validate_dims("data initialization", "t_expect", "double",
        std::vector<size_t>{});
      t_expect = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 24;
      t_expect = context__.vals_r("t_expect")[(1 - 1)];
      current_statement__ = 24;
      stan::math::check_greater_or_equal(function__, "t_expect", t_expect, 0);
      current_statement__ = 25;
      context__.validate_dims("data initialization", "t_stdev", "double",
        std::vector<size_t>{});
      t_stdev = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 25;
      t_stdev = context__.vals_r("t_stdev")[(1 - 1)];
      current_statement__ = 25;
      stan::math::check_greater_or_equal(function__, "t_stdev", t_stdev, 0);
      current_statement__ = 26;
      context__.validate_dims("data initialization", "n_expect", "double",
        std::vector<size_t>{});
      n_expect = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 26;
      n_expect = context__.vals_r("n_expect")[(1 - 1)];
      current_statement__ = 26;
      stan::math::check_greater_or_equal(function__, "n_expect", n_expect, 0);
      current_statement__ = 27;
      context__.validate_dims("data initialization", "n_stdev", "double",
        std::vector<size_t>{});
      n_stdev = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 27;
      n_stdev = context__.vals_r("n_stdev")[(1 - 1)];
      current_statement__ = 27;
      stan::math::check_greater_or_equal(function__, "n_stdev", n_stdev, 0);
      current_statement__ = 28;
      context__.validate_dims("data initialization", "g_expect", "double",
        std::vector<size_t>{});
      g_expect = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 28;
      g_expect = context__.vals_r("g_expect")[(1 - 1)];
      current_statement__ = 28;
      stan::math::check_greater_or_equal(function__, "g_expect", g_expect, 0);
      current_statement__ = 29;
      context__.validate_dims("data initialization", "g_stdev", "double",
        std::vector<size_t>{});
      g_stdev = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 29;
      g_stdev = context__.vals_r("g_stdev")[(1 - 1)];
      current_statement__ = 29;
      stan::math::check_greater_or_equal(function__, "g_stdev", g_stdev, 0);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    num_params_r__ = 1 + 1 + 1;
  }
  inline std::string model_name() const final {
    return "model_pgw_tte_gammaprior";
  }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.32.2",
             "stancflags = --allow-undefined"};
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI,
            stan::require_vector_like_t<VecR>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR>
  log_prob_impl(VecR& params_r__, VecI& params_i__, std::ostream*
                pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    static constexpr const char* function__ =
      "model_pgw_tte_gammaprior_namespace::log_prob";
    // suppress unused var warning
    (void) function__;
    try {
      local_scalar_t__ theta = DUMMY_VAR__;
      current_statement__ = 1;
      theta = in__.template read_constrain_lb<local_scalar_t__,
                jacobian__>(0, lp__);
      local_scalar_t__ nu = DUMMY_VAR__;
      current_statement__ = 2;
      nu = in__.template read_constrain_lb<local_scalar_t__, jacobian__>(0,
             lp__);
      local_scalar_t__ gamma = DUMMY_VAR__;
      current_statement__ = 3;
      gamma = in__.template read_constrain_lb<local_scalar_t__,
                jacobian__>(0, lp__);
      {
        local_scalar_t__ t_alpha = DUMMY_VAR__;
        current_statement__ = 4;
        t_alpha = gamma_expect_stdev_to_alpha(t_expect, t_stdev, pstream__);
        local_scalar_t__ t_beta = DUMMY_VAR__;
        current_statement__ = 5;
        t_beta = gamma_expect_stdev_to_beta(t_expect, t_stdev, pstream__);
        local_scalar_t__ n_alpha = DUMMY_VAR__;
        current_statement__ = 6;
        n_alpha = gamma_expect_stdev_to_alpha(n_expect, n_stdev, pstream__);
        local_scalar_t__ n_beta = DUMMY_VAR__;
        current_statement__ = 7;
        n_beta = gamma_expect_stdev_to_beta(n_expect, n_stdev, pstream__);
        local_scalar_t__ g_alpha = DUMMY_VAR__;
        current_statement__ = 8;
        g_alpha = gamma_expect_stdev_to_alpha(g_expect, g_stdev, pstream__);
        local_scalar_t__ g_beta = DUMMY_VAR__;
        current_statement__ = 9;
        g_beta = gamma_expect_stdev_to_beta(g_expect, g_stdev, pstream__);
        current_statement__ = 10;
        lp_accum__.add(stan::math::gamma_lpdf<false>(nu, n_alpha, n_beta));
        current_statement__ = 11;
        lp_accum__.add(stan::math::gamma_lpdf<false>(gamma, g_alpha, g_beta));
        current_statement__ = 14;
        for (int i = 1; i <= N_status_e; ++i) {
          current_statement__ = 12;
          lp_accum__.add(pgw_lpdf<false>(
                           stan::model::rvalue(te, "te",
                             stan::model::index_uni(i)), theta, nu, gamma,
                           pstream__));
        }
        current_statement__ = 17;
        for (int j = 1; j <= N_status_c; ++j) {
          current_statement__ = 15;
          lp_accum__.add(pgW_lccdf(
                           stan::model::rvalue(tc, "tc",
                             stan::model::index_uni(j)), theta, nu, gamma,
                           pstream__));
        }
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
  }
  template <typename RNG, typename VecR, typename VecI, typename VecVar,
            stan::require_vector_like_vt<std::is_floating_point,
            VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral,
            VecI>* = nullptr, stan::require_vector_vt<std::is_floating_point,
            VecVar>* = nullptr>
  inline void
  write_array_impl(RNG& base_rng__, VecR& params_r__, VecI& params_i__,
                   VecVar& vars__, const bool
                   emit_transformed_parameters__ = true, const bool
                   emit_generated_quantities__ = true, std::ostream*
                   pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    static constexpr bool propto__ = true;
    // suppress unused var warning
    (void) propto__;
    double lp__ = 0.0;
    // suppress unused var warning
    (void) lp__;
    int current_statement__ = 0;
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    constexpr bool jacobian__ = false;
    static constexpr const char* function__ =
      "model_pgw_tte_gammaprior_namespace::write_array";
    // suppress unused var warning
    (void) function__;
    try {
      double theta = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 1;
      theta = in__.template read_constrain_lb<local_scalar_t__,
                jacobian__>(0, lp__);
      double nu = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 2;
      nu = in__.template read_constrain_lb<local_scalar_t__, jacobian__>(0,
             lp__);
      double gamma = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 3;
      gamma = in__.template read_constrain_lb<local_scalar_t__,
                jacobian__>(0, lp__);
      out__.write(theta);
      out__.write(nu);
      out__.write(gamma);
      if (stan::math::logical_negation(
            (stan::math::primitive_value(emit_transformed_parameters__) ||
            stan::math::primitive_value(emit_generated_quantities__)))) {
        return ;
      }
      if (stan::math::logical_negation(emit_generated_quantities__)) {
        return ;
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, typename VecI,
            stan::require_vector_t<VecVar>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void
  unconstrain_array_impl(const VecVar& params_r__, const VecI& params_i__,
                         VecVar& vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ theta = DUMMY_VAR__;
      current_statement__ = 1;
      theta = in__.read<local_scalar_t__>();
      out__.write_free_lb(0, theta);
      local_scalar_t__ nu = DUMMY_VAR__;
      current_statement__ = 2;
      nu = in__.read<local_scalar_t__>();
      out__.write_free_lb(0, nu);
      local_scalar_t__ gamma = DUMMY_VAR__;
      current_statement__ = 3;
      gamma = in__.read<local_scalar_t__>();
      out__.write_free_lb(0, gamma);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, stan::require_vector_t<VecVar>* = nullptr>
  inline void
  transform_inits_impl(const stan::io::var_context& context__, VecVar&
                       vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      current_statement__ = 1;
      context__.validate_dims("parameter initialization", "theta", "double",
        std::vector<size_t>{});
      current_statement__ = 2;
      context__.validate_dims("parameter initialization", "nu", "double",
        std::vector<size_t>{});
      current_statement__ = 3;
      context__.validate_dims("parameter initialization", "gamma", "double",
        std::vector<size_t>{});
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ theta = DUMMY_VAR__;
      current_statement__ = 1;
      theta = context__.vals_r("theta")[(1 - 1)];
      out__.write_free_lb(0, theta);
      local_scalar_t__ nu = DUMMY_VAR__;
      current_statement__ = 2;
      nu = context__.vals_r("nu")[(1 - 1)];
      out__.write_free_lb(0, nu);
      local_scalar_t__ gamma = DUMMY_VAR__;
      current_statement__ = 3;
      gamma = context__.vals_r("gamma")[(1 - 1)];
      out__.write_free_lb(0, gamma);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  inline void
  get_param_names(std::vector<std::string>& names__, const bool
                  emit_transformed_parameters__ = true, const bool
                  emit_generated_quantities__ = true) const {
    names__ = std::vector<std::string>{"theta", "nu", "gamma"};
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline void
  get_dims(std::vector<std::vector<size_t>>& dimss__, const bool
           emit_transformed_parameters__ = true, const bool
           emit_generated_quantities__ = true) const {
    dimss__ = std::vector<std::vector<size_t>>{std::vector<size_t>{},
                std::vector<size_t>{}, std::vector<size_t>{}};
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline void
  constrained_param_names(std::vector<std::string>& param_names__, bool
                          emit_transformed_parameters__ = true, bool
                          emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "theta");
    param_names__.emplace_back(std::string() + "nu");
    param_names__.emplace_back(std::string() + "gamma");
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline void
  unconstrained_param_names(std::vector<std::string>& param_names__, bool
                            emit_transformed_parameters__ = true, bool
                            emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "theta");
    param_names__.emplace_back(std::string() + "nu");
    param_names__.emplace_back(std::string() + "gamma");
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline std::string get_constrained_sizedtypes() const {
    return std::string("[{\"name\":\"theta\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"nu\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"gamma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"}]");
  }
  inline std::string get_unconstrained_sizedtypes() const {
    return std::string("[{\"name\":\"theta\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"nu\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"gamma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"}]");
  }
  // Begin method overload boilerplate
  template <typename RNG> inline void
  write_array(RNG& base_rng, Eigen::Matrix<double,-1,1>& params_r,
              Eigen::Matrix<double,-1,1>& vars, const bool
              emit_transformed_parameters = true, const bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + 1);
    const size_t num_transformed = emit_transformed_parameters * (0);
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    std::vector<int> params_i;
    vars = Eigen::Matrix<double,-1,1>::Constant(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <typename RNG> inline void
  write_array(RNG& base_rng, std::vector<double>& params_r, std::vector<int>&
              params_i, std::vector<double>& vars, bool
              emit_transformed_parameters = true, bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + 1);
    const size_t num_transformed = emit_transformed_parameters * (0);
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    vars = std::vector<double>(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(Eigen::Matrix<T_,-1,1>& params_r, std::ostream* pstream = nullptr) const {
    Eigen::Matrix<int,-1,1> params_i;
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(std::vector<T_>& params_r, std::vector<int>& params_i,
           std::ostream* pstream = nullptr) const {
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  inline void
  transform_inits(const stan::io::var_context& context,
                  Eigen::Matrix<double,-1,1>& params_r, std::ostream*
                  pstream = nullptr) const final {
    std::vector<double> params_r_vec(params_r.size());
    std::vector<int> params_i;
    transform_inits(context, params_i, params_r_vec, pstream);
    params_r = Eigen::Map<Eigen::Matrix<double,-1,1>>(params_r_vec.data(),
                 params_r_vec.size());
  }
  inline void
  transform_inits(const stan::io::var_context& context, std::vector<int>&
                  params_i, std::vector<double>& vars, std::ostream*
                  pstream__ = nullptr) const {
    vars.resize(num_params_r__);
    transform_inits_impl(context, vars, pstream__);
  }
  inline void
  unconstrain_array(const std::vector<double>& params_constrained,
                    std::vector<double>& params_unconstrained, std::ostream*
                    pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = std::vector<double>(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
  inline void
  unconstrain_array(const Eigen::Matrix<double,-1,1>& params_constrained,
                    Eigen::Matrix<double,-1,1>& params_unconstrained,
                    std::ostream* pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = Eigen::Matrix<double,-1,1>::Constant(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
};
}
using stan_model = model_pgw_tte_gammaprior_namespace::model_pgw_tte_gammaprior;
#ifndef USING_R
// Boilerplate
stan::model::model_base&
new_model(stan::io::var_context& data_context, unsigned int seed,
          std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_pgw_tte_gammaprior_namespace::profiles__;
}
#endif
#endif