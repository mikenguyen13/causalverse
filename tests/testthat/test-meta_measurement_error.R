sim_pairs <- function(J = 3000, theta = 0.4, seed = 1) {
  set.seed(seed)
  paper <- rep(seq_len(J / 10), each = 10)
  beta <- rnorm(J, 0.2, 0.5)
  gamma <- rnorm(J, 0.02, 0.05)
  se_iv <- rlnorm(J, log(0.15), 0.4)
  se_ols <- se_iv / 3
  fs <- abs(rnorm(J, 0.4, 0.1))
  data.frame(paper = paper,
             b_ols = theta * beta + gamma + rnorm(J, 0, se_ols), se_ols = se_ols,
             b_iv = beta + rnorm(J, 0, se_iv), se_iv = se_iv,
             b_fs = fs, se_fs = fs / 8)
}

prep_sim <- function(d) {
  meta_me_prepare(d, "paper", "b_ols", "se_ols", "b_iv", "se_iv",
                  b_fs = "b_fs", se_fs = "se_fs", normalize = FALSE, winsor = 0)
}

test_that("meta-OLS recovers average proxy quality", {
  d <- prep_sim(sim_pairs(theta = 0.4))
  for (w in c("none", "average", "fe", "weighted")) {
    f <- meta_me_ols(d, within = w)
    expect_s3_class(f, "meta_me_ols")
    expect_lt(abs(f$theta - 0.4), 4 * f$theta_se + 0.02)
  }
  f <- meta_me_ols(d)
  expect_true(f$rho2 > 0 && f$rho2 < 1)
  expect_lt(abs(f$ovb - 0.02), 0.03)
})

test_that("meta-IV recovers proxy quality and returns an AR set", {
  d <- prep_sim(sim_pairs(theta = 0.5, seed = 2))
  f <- meta_me_iv(d)
  expect_s3_class(f, "meta_me_iv")
  expect_lt(abs(f$theta - 0.5), 4 * f$theta_se + 0.02)
  expect_length(f$theta_ar, 2)
  expect_true(f$theta_ar[1] < f$theta && f$theta < f$theta_ar[2])
})

test_that("normalization and winsorization behave", {
  d <- sim_pairs(J = 200)
  d$sdx <- 2; d$sdy <- 4
  p <- meta_me_prepare(d, "paper", "b_ols", "se_ols", "b_iv", "se_iv",
                       sd_x = "sdx", sd_y = "sdy", winsor = 0)
  expect_equal(p$b_iv, d$b_iv * 0.5)
  w <- meta_me_prepare(d, "paper", "b_ols", "se_ols", "b_iv", "se_iv",
                       normalize = FALSE, winsor = 0.05)
  expect_lte(max(w$b_iv), max(d$b_iv))
  expect_error(meta_me_prepare(d, "paper", "b_ols", "se_ols", "b_iv", "se_iv"),
               "requires sd_x")
})

test_that("Proposition 4 equals baseline at R = 0 and moves with sign of R", {
  f <- meta_me_ols(prep_sim(sim_pairs()))
  g <- meta_me_corr_bias(f, R = c(-0.3, 0, 0.3), var_theta = 0)
  expect_equal(g$theta[g$R == 0], f$theta)
  expect_lt(g$theta[g$R == 0.3], f$theta)
  expect_gt(g$theta[g$R == -0.3], f$theta)
})

test_that("invalidity bounds bracket the estimate", {
  f <- meta_me_ols(prep_sim(sim_pairs()))
  b <- meta_me_invalidity_bounds(f)
  expect_equal(b[["lower"]], f$theta)
  expect_gte(b[["upper"]], b[["lower"]])
})

test_that("U-statistic is near zero without selection and detects selection", {
  set.seed(3)
  J <- 800
  se <- rlnorm(J, log(0.2), 0.5)
  b <- rnorm(J, 0.1, 0.3) + rnorm(J, 0, se)
  expect_lt(abs(meta_me_ustat(b, se)$U_1), 0.05)
  keep <- abs(b / se) > 1.96 | runif(J) < 0.2
  u <- meta_me_ustat(b[keep], se[keep])
  expect_lt(u$U_1, 0)
  expect_lt(u$q, 0.6)
})

test_that("bias categories follow the stated directions", {
  cat <- meta_me_bias_category(sim_sign = c(1, 0, 0, 1), ovb_sign = c(0, -1, 0, -1),
                               me_sign = c(0, 0, 0, 0), b_iv = c(1, 1, 1, 1))
  expect_equal(cat, c("Affirmative", "Corrective", "Unclear", "Unclear"))
  orig <- meta_me_bias_category(0, 0, 0, 1, replicate_original = TRUE)
  expect_equal(orig, "Corrective")
})

test_that("descriptives and bins run", {
  d <- prep_sim(sim_pairs())
  d$fstat <- rexp(nrow(d), 1 / 30)
  expect_equal(nrow(meta_me_bins(d, "t_iv")), 4)
  expect_equal(nrow(meta_me_bins(d, "fstat")), 4)
  expect_equal(nrow(meta_me_sorting(d)), 5)
  cmp <- meta_me_compare(d$b_ols, d$b_iv)
  expect_true(cmp$pct_iv_bigger_all > 50)
  mv <- meta_me_motivation(c(1, 0, 1), c(0, 1, 1), c(0, 0, 1))
  expect_equal(sum(mv$n), 3)
  nb <- meta_me_nobias_benchmark(d$b_iv[1:200], d$se_ols[1:200], d$se_iv[1:200])
  expect_true(nb[["prob"]] > 45 && nb[["prob"]] < 65)
})

test_that("heterogeneity solver returns theta in (0, 1)", {
  f <- meta_me_ols(prep_sim(sim_pairs()))
  h <- meta_me_heterogeneity(f, R = c(-0.5, 0.5), var_theta = 0.01)
  expect_true(all(h$theta[is.finite(h$theta)] > 0 & h$theta[is.finite(h$theta)] < 1))
})

test_that("structural model recovers theta without publication bias", {
  skip_on_cran()
  d <- prep_sim(sim_pairs(J = 2000, theta = 0.4, seed = 5))
  pb <- meta_me_pubbias(d, fix_q = 1, n_sim = 5e4, n_rep = 2)
  expect_true(pb$converged)
  expect_lt(abs(pb$params[["theta"]] - 0.4), 0.15)
  expect_s3_class(meta_me_plot_tdist(pb, d), "ggplot")
})

test_that("plots build", {
  f <- meta_me_ols(prep_sim(sim_pairs()))
  expect_s3_class(meta_me_plot_scatter(f), "ggplot")
  expect_s3_class(meta_me_plot_corr_bias(f), "ggplot")
  expect_s3_class(meta_me_plot_heterogeneity(f), "ggplot")
  expect_s3_class(meta_me_plot_compare(meta_me_compare(f$data$b_ols, f$data$b_iv,
                                                       rep(c("A", "B"), length.out = f$n))), "ggplot")
})
