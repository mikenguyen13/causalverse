# Global variables declaration to avoid R CMD check NOTEs
# These are column names used in aes(), data.table, or NSE contexts
utils::globalVariables(c(
  # Original globals
  ".", "nas",
  "x", "y", "lower_ci", "upper_ci",
  "potential_taus", "TE_lower", "TE_upper",
  "sd",

  # New function globals -- ggplot2 aes() column references
  # balance_table / love_plot / covariate_summary
  "covariate", "smd", "adjustment",
  "x_start", "x_end",

  # blp_analysis
  "term", "estimate", "ci_lower", "ci_upper",

  # qini_curve
  "fraction", "uplift",

  # heterogeneity_plot
  "group", "cate", "mod", "tau",

  # did_power_analysis / mde_calc
  "effect", "power", "mde",

  # did_event_study
  "period", "model",

  # sc_gap_plot
  "gap", "donor", "lo", "hi", "time",

  # treatment_calendar
  "unit_f", "fill_var",

  # staggered_summary
  "first_treat_period", "n_units",

  # iv_sensitivity
  "delta", "adjusted",

  # rd_assumption_tests
  "cutoff_type", "cutoff",

  # mediation_analysis
  "effect",

  # spec_curve / sensitivity_plot
  "r2dz", "r2yz", "significant",

  # causal_forest_summary
  "importance", "feature",

  # event study
  "aar", "caar", "event_time", "car",

  # dr_ate / overlap_weights
  "d",

  # dose_response
  "dose",

  # general
  "ci_hi", "ci_lo", "bandwidth",
  "estimator", "type", "where",
  "na.omit",

  # bandwidth sensitivity
  "bw_multiplier",

  # placebo_test
  "stat",

  # causal_table
  "n",

  # tidy_causal
  "std_error", "t_stat", "p_value",

  # love_plot
  "weight",

  # multiverse_analysis
  "rank", "significant", "ci_lo", "ci_hi",

  # balance_plot
  "label", "smd_unadj", "smd_adj",

  # attrition_analysis
  "attrition_rate",

  # power_sim
  "power", "effect_size",

  # coef_plot
  "conf.low", "conf.high", "term_display",
  "sig", "model_nm",

  # panel_diagnostics
  "adf_t", "lag_val", "acf_val",
  "unit_i", "unit_j", "rho",
  "fitted_vals", "resid_vals",

  # pretrend_sensitivity
  "ci_lo_min", "ci_hi_max", "ci_lo_M0", "ci_hi_M0",

  # rd_plot / regression_discontinuity_plot
  "x_mid", "y_mean", "ci_l", "ci_r",
  "x_fit", "y_fit", "ci_low", "ci_high",
  "side",

  # dag_utils (ggdag column names)
  "name", "status",

  # multiverse_analysis (pivot_longer output)
  "dimension", "choice",

  # attrition / balance_plot extras
  "bound", "smd_treated_arm", "smd_control_arm",

  # attrition_analysis lee bounds
  "est",

  # parallel_trends_plot
  "time_pt", "mean_y", "ci_lo_pt", "ci_hi_pt", "grp",

  # iv_diagnostics
  "pass_label", "ratio", "threshold",

  # dose_response_curve
  "dose_mid",

  # stacked_did
  "rel_period",

  # causal_summary  (no new aes() vars)

  # heterogeneity_forest_plot
  # (ci_lower, ci_upper already declared above)

  # propensity_diagnostics
  "ps", "pred_mean", "obs_rate",

  # synthetic_control_gaps
  "series", "gap_d",

  # rd_binscatter
  "x_bin", "ci_low_f", "ci_hi_f",
  # (x_fit, y_fit, y_mean, side already declared above)

  # confounding_strength
  "adj_est", "bias_adj",
  # (r2dz, r2yz already declared above)

  # heterogeneity_forest_plot
  "sig_label",

  # iv_diagnostics
  "test", "statistic"
))
