# ==========================================================
# code1.R  -  R translation of code1.py (code1.ipynb)
#
# Reproduces the Python/scikit-learn/statsmodels/scipy analysis
# with the same numerical results:
#   * train_test_split(test_size=0.20, random_state=42) and
#     KFold(n_splits=5, shuffle=True, random_state=42) are reproduced
#     exactly by re-implementing numpy's RandomState(42) (MT19937)
#     so the SAME rows land in the same train/test sets and folds.
#   * StandardScaler uses the population SD (ddof = 0), like sklearn.
#   * LinearRegression is solved by minimum-norm least squares
#     (SVD), like sklearn/scipy lstsq, so rank-deficient polynomial
#     designs give the same predictions.
#   * PolynomialFeatures column order is reproduced.
#   * VIF follows statsmodels' variance_inflation_factor.
#   * SD of CV RMSE uses ddof = 0, like numpy .std().
#
# Required package: readxl   (install.packages("readxl"))
# All plots use base R graphics.
# ==========================================================

# ================================
# IMPORT LIBRARIES
# ================================

library(readxl)

# ================================
# HELPER FUNCTIONS (Python equivalents)
# ================================

# ---- numpy RandomState(seed) re-implementation (MT19937) -------------
# Unsigned 32-bit helpers working on doubles
u32_xor <- function(a, b) {
  ah <- a %/% 65536; al <- a %% 65536
  bh <- b %/% 65536; bl <- b %% 65536
  bitwXor(ah, bh) * 65536 + bitwXor(al, bl)
}
u32_and <- function(a, b) {
  ah <- a %/% 65536; al <- a %% 65536
  bh <- b %/% 65536; bl <- b %% 65536
  bitwAnd(ah, bh) * 65536 + bitwAnd(al, bl)
}
u32_mul <- function(a, b) {         # (a * b) mod 2^32, exact
  al <- a %% 65536; ah <- a %/% 65536
  bl <- b %% 65536; bh <- b %/% 65536
  ((((ah * bl + al * bh) %% 65536) * 65536) + al * bl) %% 4294967296
}

# Set R's own Mersenne-Twister to the exact state of
# numpy.random.RandomState(seed); runif() * 2^32 then returns
# exactly the same 32-bit integers numpy draws.
np_random_state <- function(seed) {
  RNGkind("Mersenne-Twister")
  mt <- numeric(624)
  mt[1] <- seed
  for (i in 2:624) {
    prev <- mt[i - 1]
    mt[i] <- (u32_mul(1812433253, u32_xor(prev, prev %/% 2^30)) + (i - 1)) %% 4294967296
  }
  signed <- ifelse(mt >= 2^31, mt - 2^32, mt)
  assign(".Random.seed", c(10403L, 624L, as.integer(signed)), envir = .GlobalEnv)
  invisible(NULL)
}
next_uint32 <- function() runif(1) * 4294967296

# numpy legacy random_interval(max)
random_interval <- function(mx) {
  if (mx == 0) return(0)
  # smallest all-ones bit mask >= mx  (numpy: mask |= mask >> 1, 2, 4, 8, 16)
  mask <- 1
  while (mask < mx) mask <- mask * 2 + 1
  repeat {
    v <- u32_and(next_uint32(), mask)
    if (v <= mx) return(v)
  }
}

# numpy RandomState(seed).shuffle(x) / .permutation(n)  (0-based values)
np_shuffle <- function(x, seed) {
  np_random_state(seed)
  n <- length(x)
  if (n > 1) {
    for (i in (n - 1):1) {
      j <- random_interval(i)
      tmp <- x[i + 1]; x[i + 1] <- x[j + 1]; x[j + 1] <- tmp
    }
  }
  x
}

# sklearn.model_selection.train_test_split (returns 1-based row indices)
train_test_split_idx <- function(n, test_size, random_state) {
  n_test  <- ceiling(test_size * n)
  n_train <- n - n_test
  perm <- np_shuffle(0:(n - 1), random_state)
  list(train = perm[(n_test + 1):(n_test + n_train)] + 1,
       test  = perm[1:n_test] + 1)
}

# sklearn KFold(n_splits, shuffle=TRUE, random_state) -> list of test indices (1-based)
kfold_test_idx <- function(n, n_splits, random_state) {
  idx <- np_shuffle(0:(n - 1), random_state) + 1
  fold_sizes <- rep(n %/% n_splits, n_splits)
  extra <- n %% n_splits
  if (extra > 0) fold_sizes[1:extra] <- fold_sizes[1:extra] + 1
  folds <- list(); current <- 0
  for (k in seq_len(n_splits)) {
    folds[[k]] <- idx[(current + 1):(current + fold_sizes[k])]
    current <- current + fold_sizes[k]
  }
  folds
}

# ---- sklearn LinearRegression (fit_intercept=TRUE, min-norm lstsq) ----
# sklearn >= 1.7 calls scipy.linalg.lstsq(X, y, cond=tol) with tol = 1e-6:
# singular values below tol * largest singular value are discarded.
# (sklearn <= 1.6 used machine epsilon; set LR_TOL <- .Machine$double.eps
#  to mimic that, but with exactly collinear polynomial columns the result
#  then depends on floating-point noise in any language.)
LR_TOL <- 1e-6
lr_fit <- function(X, y, tol = LR_TOL) {
  X <- as.matrix(X)
  x_mean <- colMeans(X)
  y_mean <- mean(y)
  Xc <- sweep(X, 2, x_mean)
  yc <- y - y_mean
  s <- svd(Xc)
  d_inv <- ifelse(s$d > tol * s$d[1], 1 / s$d, 0)
  coef <- as.vector(s$v %*% (d_inv * crossprod(s$u, yc)))
  list(coef = coef, intercept = y_mean - sum(x_mean * coef))
}
lr_predict <- function(model, X) as.vector(as.matrix(X) %*% model$coef + model$intercept)

# ---- sklearn StandardScaler (population SD, ddof = 0) ----
scaler_fit <- function(X) {
  X <- as.matrix(X)
  mu <- colMeans(X)
  sdv <- sqrt(colMeans(sweep(X, 2, mu)^2))
  sdv[sdv == 0] <- 1
  list(mean = mu, scale = sdv)
}
scaler_transform <- function(sc, X) {
  sweep(sweep(as.matrix(X), 2, sc$mean), 2, sc$scale, "/")
}

# ---- sklearn PolynomialFeatures(degree, include_bias=FALSE) ----
poly_features <- function(X, degree) {
  X <- as.matrix(X)
  p <- ncol(X)
  out <- list(); nm <- c()
  # combinations_with_replacement order, degree 1..degree
  rec <- function(start, d, combo) {
    if (d == 0) {
      out[[length(out) + 1]] <<- apply(X[, combo, drop = FALSE], 1, prod)
      nm <<- c(nm, paste(colnames(X)[combo], collapse = " "))
      return(invisible())
    }
    for (j in start:p) rec(j, d - 1, c(combo, j))
  }
  for (deg in 1:degree) rec(1, deg, integer(0))
  res <- do.call(cbind, out)
  colnames(res) <- nm
  res
}

# ---- metrics ----
mean_squared_error  <- function(y, yhat) mean((y - yhat)^2)
mean_absolute_error <- function(y, yhat) mean(abs(y - yhat))
r2_score <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

# ---- cross_val_score(LinearRegression(), X, y, cv=kf, 'neg_mean_squared_error') ----
cv_rmse_lr <- function(X, y, folds) {
  X <- as.matrix(X)
  sapply(folds, function(test) {
    m <- lr_fit(X[-test, , drop = FALSE], y[-test])
    sqrt(mean_squared_error(y[test], lr_predict(m, X[test, , drop = FALSE])))
  })
}

# numpy .std() (ddof = 0)
np_std <- function(x) sqrt(mean((x - mean(x))^2))

# ---- statsmodels variance_inflation_factor ----
# statsmodels <= 0.14 (classic formula, default here): VIF of "const" is
# computed from an uncentered R^2.  statsmodels >= 0.15 standardizes the
# columns first (standardize=True), which makes the "const" VIF 1.0; all
# other VIFs are identical in both versions.  Set VIF_STANDARDIZE <- TRUE
# to mimic statsmodels >= 0.15.
VIF_STANDARDIZE <- FALSE
sm_rsquared <- function(y, X) {
  fit <- lm.fit(X, y)
  ssr <- sum(fit$residuals^2)
  has_const <- any(apply(X, 2, function(col) diff(range(col)) == 0 && col[1] != 0))
  if (!has_const) {   # implicit constant check, as statsmodels does
    rank_orig <- qr(X)$rank
    rank_aug  <- qr(cbind(1, X))$rank
    has_const <- rank_orig == rank_aug
  }
  if (has_const) 1 - ssr / sum((y - mean(y))^2) else 1 - ssr / sum(y^2)
}
variance_inflation_factor <- function(exog, i, standardize = VIF_STANDARDIZE) {
  if (standardize) {
    mu <- colMeans(exog)
    sdv <- sqrt(colMeans(sweep(exog, 2, mu)^2))
    ok <- sdv > 1e-10
    exog[, ok] <- sweep(sweep(exog[, ok, drop = FALSE], 2, mu[ok]), 2, sdv[ok], "/")
  }
  r2 <- sm_rsquared(exog[, i], exog[, -i, drop = FALSE])
  if (standardize) r2 <- min(max(r2, 0), 1 - 1e-15)
  1 / (1 - r2)
}

# Python-like printing of a full-precision number
pyprint <- function(label, x) cat(label, format(x, digits = 16), "\n")

# ================================
# LOAD DATA
# ================================

df <- as.data.frame(read_excel("gym_members_exercise_tracking.xlsx"),
                    check.names = FALSE)

cat("\nDATA SHAPE\n")
print(dim(df))

cat("\nFIRST 5 ROWS\n")
print(head(df, 5))

# ================================
# BASIC INSPECTION
# ================================

cat("\nINFO\n")
str(df)

cat("\nMISSING VALUES\n")
print(colSums(is.na(df)))

cat("\nDESCRIPTIVE STATISTICS\n")
num_cols <- names(df)[sapply(df, is.numeric)]
describe <- sapply(df[num_cols], function(x) {
  x <- x[!is.na(x)]
  c(count = length(x), mean = mean(x), std = sd(x), min = min(x),
    `25%` = unname(quantile(x, 0.25, type = 7)),
    `50%` = unname(quantile(x, 0.50, type = 7)),
    `75%` = unname(quantile(x, 0.75, type = 7)),
    max = max(x))
})
print(describe)

# ================================
# PREPROCESSING
# ================================

if ("BMI" %in% names(df)) {
  df$BMI <- NULL
}

df$Gender <- as.integer(df$Gender == "Male")

# pd.get_dummies(columns=['Workout_Type'], drop_first=True, dtype=int)
# (levels sorted, first dropped, dummy columns appended at the end)
wt <- as.character(df$Workout_Type)
wt_levels <- sort(unique(wt[!is.na(wt)]))
df$Workout_Type <- NULL
for (lv in wt_levels[-1]) {
  df[[paste0("Workout_Type_", lv)]] <- as.integer(!is.na(wt) & wt == lv)
}

cat("\nDATA TYPES AFTER PREPROCESSING\n")
print(sapply(df, class))

# ================================
# EDA
# ================================

cb <- df$Calories_Burned
hist(cb,
     breaks = seq(min(cb), max(cb), length.out = 31),
     main = "Distribution of Calories Burned",
     xlab = "Calories Burned", ylab = "Frequency")

num_df <- df[sapply(df, is.numeric)]
corr <- cor(num_df)

# Correlation heatmap (annotated, coolwarm-like palette)
local({
  k <- ncol(corr)
  pal <- colorRampPalette(c("#3B4CC0", "#DDDDDD", "#B40426"))(200)
  op <- par(mar = c(10, 10, 3, 2))
  image(1:k, 1:k, t(corr[k:1, ]), col = pal, zlim = c(-1, 1),
        axes = FALSE, xlab = "", ylab = "", main = "Correlation Heatmap")
  axis(1, at = 1:k, labels = colnames(corr), las = 2, cex.axis = 0.7)
  axis(2, at = 1:k, labels = rev(rownames(corr)), las = 1, cex.axis = 0.7)
  for (i in 1:k) for (j in 1:k)
    text(j, k - i + 1, sprintf("%.2f", corr[i, j]), cex = 0.55)
  par(op)
})

cat("\nCORRELATION WITH TARGET\n")
print(sort(corr[, "Calories_Burned"], decreasing = TRUE))

plot(df$`Session_Duration (hours)`, df$Calories_Burned,
     xlab = "Session Duration", ylab = "Calories Burned",
     main = "Session Duration vs Calories Burned")

# ================================
# DEFINE X AND y
# ================================

X <- df[, setdiff(names(df), "Calories_Burned"), drop = FALSE]
y <- df$Calories_Burned

# ================================
# VIF ANALYSIS
# ================================

X_vif <- cbind(const = 1, as.matrix(X))

vif_df <- data.frame(
  Variable = colnames(X_vif),
  VIF = sapply(seq_len(ncol(X_vif)),
               function(i) variance_inflation_factor(X_vif, i)),
  check.names = FALSE
)

cat("\nVIF TABLE\n")
print(vif_df[order(vif_df$VIF, decreasing = TRUE), ])

# ================================
# TRAIN TEST SPLIT
# ================================

split <- train_test_split_idx(nrow(X), test_size = 0.20, random_state = 42)

X_train <- X[split$train, , drop = FALSE]
X_test  <- X[split$test, , drop = FALSE]
y_train <- y[split$train]
y_test  <- y[split$test]

# ================================
# STANDARDIZATION
# ================================

scaler <- scaler_fit(X_train)

X_train_sc <- scaler_transform(scaler, X_train)
X_test_sc  <- scaler_transform(scaler, X_test)

# ================================
# MULTIPLE LINEAR REGRESSION
# ================================

mlr <- lr_fit(X_train_sc, y_train)

y_pred_mlr <- lr_predict(mlr, X_test_sc)

# ================================
# MLR EVALUATION
# ================================

rmse_mlr <- sqrt(mean_squared_error(y_test, y_pred_mlr))
mae_mlr  <- mean_absolute_error(y_test, y_pred_mlr)
r2_mlr   <- r2_score(y_test, y_pred_mlr)

n <- length(y_test)
p <- ncol(X_test)

adj_r2_mlr <- 1 - ((1 - r2_mlr) * (n - 1) / (n - p - 1))

cat("\nMLR RESULTS\n")
pyprint("RMSE =", rmse_mlr)
pyprint("MAE =", mae_mlr)
pyprint("R2 =", r2_mlr)
pyprint("Adjusted R2 =", adj_r2_mlr)

# ================================
# COEFFICIENT TABLE
# ================================

coef_df <- data.frame(
  Variable = names(X),
  Coefficient = mlr$coef
)

coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE), ]

cat("\nCOEFFICIENT TABLE\n")
print(coef_df)

# ================================
# RESIDUAL ANALYSIS
# ================================

residuals_mlr <- y_test - y_pred_mlr

plot(y_pred_mlr, residuals_mlr,
     xlab = "Predicted", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)

hist(residuals_mlr, freq = FALSE,
     main = "Distribution of Residuals", xlab = "Residuals")
lines(density(residuals_mlr), lwd = 2)

# statsmodels qqplot(line='45'): theoretical quantiles at i/(n+1)
local({
  r <- sort(residuals_mlr)
  theo <- qnorm(seq_along(r) / (length(r) + 1))
  plot(theo, r, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  abline(0, 1, col = "red")
})

sw <- shapiro.test(residuals_mlr)
w_stat <- unname(sw$statistic)
p_shapiro <- sw$p.value

cat("\nSHAPIRO-WILK TEST\n")
pyprint("W =", w_stat)
pyprint("p-value =", p_shapiro)

# ================================
# DEGREE SELECTION
# ================================

kf <- kfold_test_idx(length(y_train), n_splits = 5, random_state = 42)

degree_results <- list()

for (degree in c(1, 2, 3)) {

  X_poly <- poly_features(X_train_sc, degree)

  cv_rmse <- cv_rmse_lr(X_poly, y_train, kf)

  degree_results[[length(degree_results) + 1]] <-
    c(degree, mean(cv_rmse), np_std(cv_rmse))
}

degree_df <- as.data.frame(do.call(rbind, degree_results))
names(degree_df) <- c("Degree", "Mean RMSE", "SD")

cat("\nDEGREE SELECTION\n")
print(degree_df, digits = 16)

plot(degree_df$Degree, degree_df$`Mean RMSE`, type = "o", pch = 16,
     xlab = "Degree", ylab = "Mean RMSE", main = "Polynomial Degree Selection")

# ================================
# POLYNOMIAL REGRESSION
# ================================

X_train_poly <- poly_features(X_train_sc, 2)
X_test_poly  <- poly_features(X_test_sc, 2)

pr <- lr_fit(X_train_poly, y_train)

y_pred_pr <- lr_predict(pr, X_test_poly)

# ================================
# POLYNOMIAL EVALUATION
# ================================

rmse_pr <- sqrt(mean_squared_error(y_test, y_pred_pr))
mae_pr  <- mean_absolute_error(y_test, y_pred_pr)
r2_pr   <- r2_score(y_test, y_pred_pr)

p_poly <- ncol(X_test_poly)

adj_r2_pr <- 1 - ((1 - r2_pr) * (n - 1) / (n - p_poly - 1))

cat("\nPOLYNOMIAL RESULTS\n")
pyprint("RMSE =", rmse_pr)
pyprint("MAE =", mae_pr)
pyprint("R2 =", r2_pr)
pyprint("Adjusted R2 =", adj_r2_pr)

# ================================
# ACTUAL VS PREDICTED
# ================================

plot(y_test, y_pred_mlr, xlab = "Actual", ylab = "Predicted",
     main = "MLR: Actual vs Predicted")
lines(c(min(y_test), max(y_test)), c(min(y_test), max(y_test)),
      col = "red", lty = 2)

plot(y_test, y_pred_pr, xlab = "Actual", ylab = "Predicted",
     main = "Polynomial Regression: Actual vs Predicted")
lines(c(min(y_test), max(y_test)), c(min(y_test), max(y_test)),
      col = "red", lty = 2)

# ================================
# CROSS VALIDATION COMPARISON
# ================================

rmse_mlr_folds <- cv_rmse_lr(X_train_sc, y_train, kf)
rmse_pr_folds  <- cv_rmse_lr(X_train_poly, y_train, kf)

cv_results <- data.frame(
  Fold = 1:5,
  MLR_RMSE = rmse_mlr_folds,
  PR_RMSE = rmse_pr_folds
)

cat("\nCROSS VALIDATION RESULTS\n")
print(cv_results, digits = 16)

# ================================
# NORMALITY OF PAIRED DIFFERENCES
# (assumption of the paired t-test)
# ================================

diff_folds <- rmse_mlr_folds - rmse_pr_folds

sw_diff <- shapiro.test(diff_folds)

cat("\nNORMALITY OF PAIRED DIFFERENCES (SHAPIRO-WILK)\n")
print(diff_folds)
pyprint("W =", unname(sw_diff$statistic))
pyprint("p-value =", sw_diff$p.value)

local({
  theo <- qnorm(seq_along(diff_folds) / (length(diff_folds) + 1))
  plot(theo, sort(diff_folds), xlab = "Theoretical Quantiles",
       ylab = "Paired Difference (MLR - PR RMSE)",
       main = "Q-Q Plot of Paired Differences")
  abline(mean(diff_folds), sd(diff_folds), col = "red")
})

# Distribution-free check: exact Wilcoxon signed-rank test
wx <- suppressWarnings(wilcox.test(rmse_mlr_folds, rmse_pr_folds, paired = TRUE))
cat("\nWILCOXON SIGNED-RANK TEST\n")
pyprint("V =", unname(wx$statistic))
pyprint("p-value =", wx$p.value)

# ================================
# PAIRED T TEST
# ================================

tt <- t.test(rmse_mlr_folds, rmse_pr_folds, paired = TRUE)
t_stat <- unname(tt$statistic)
p_value <- tt$p.value

cat("\nPAIRED T TEST\n")
pyprint("T Statistic =", t_stat)
pyprint("P Value =", p_value)
cat("95% CI of mean difference =", format(tt$conf.int, digits = 6), "\n")
pyprint("Effect size d_z =", mean(diff_folds) / sd(diff_folds))

# ================================
# FINAL COMPARISON
# ================================

comparison <- data.frame(
  Metric = c("RMSE", "MAE", "R2", "Adjusted R2"),
  MLR = c(rmse_mlr, mae_mlr, r2_mlr, adj_r2_mlr),
  Polynomial = c(rmse_pr, mae_pr, r2_pr, adj_r2_pr)
)

cat("\nFINAL MODEL COMPARISON\n")
print(comparison, digits = 16)
