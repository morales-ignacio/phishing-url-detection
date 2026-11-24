# 04_selection.R
# Feature selection and extraction: Spearman with the label, contingency tables
# for binary variables, Gini importance, VIF, logistic regression for
# interpretation and PCA. Everything on train.


# spearman_association(X, y)
# Spearman correlation of each variable with the label. Spearman is used
# instead of Pearson because it works on ranks and does not need a linear
# relationship.
# PRECONDITION: X numeric matrix without NA, y in {0,1}, same number of rows.
# POSTCONDITION: one value between -1 and 1 per column of X (0 if the column is constant).
spearman_association <- function(X, y) {
  stopifnot(is.matrix(X), !anyNA(X), nrow(X) == length(y), all(y %in% c(0, 1)))
  rho <- suppressWarnings(apply(X, 2, function(x) cor(x, y, method = "spearman")))
  rho[is.na(rho)] <- 0
  stopifnot(all(abs(rho) <= 1 + 1e-12), identical(names(rho), colnames(X)))
  rho
}


# binary_contingency_table(df, variables)
# For each binary variable, builds the 2x2 table against the label and computes:
#   chi^2 = sum of (observed - expected)^2 / expected
#   Cramer's V = sqrt(chi^2 / (n * (min(K, L) - 1)))
#   corrected C = sqrt(chi^2 / (chi^2 + n)) / sqrt((min(K, L) - 1) / min(K, L))
#   odds ratio = (A * D) / (B * C)
# with A = has the feature and is phishing, B = has it and is legitimate,
# C = does not have it and is phishing, D = does not have it and is legitimate.
# If any cell is 0, 0.5 is added to every cell to avoid dividing by zero.
# Variables with a single value (like is_ip) are skipped.
# PRECONDITION: df has label in {0,1} and the requested variables are 0/1.
# POSTCONDITION: data.frame with chi2 >= 0, Cramer's V and corrected C between 0 and 1, and odds > 0.
# INVARIANT: each pass of the loop adds at most one row, the one for the
# current variable.
binary_contingency_table <- function(df, variables) {
  stopifnot(all(c(variables, "label") %in% names(df)))
  rows <- list()
  for (v in variables) {
    x <- df[[v]]
    stopifnot(all(x %in% c(0, 1)))
    if (length(unique(x)) < 2) next
    obs <- table(factor(x, levels = c(1, 0)), factor(df$label, levels = c(1, 0)))
    n   <- sum(obs)
    expected <- outer(rowSums(obs), colSums(obs)) / n
    chi2 <- sum((obs - expected)^2 / expected)
    cramer <- sqrt(chi2 / (n * (min(dim(obs)) - 1)))
    m      <- min(dim(obs))
    ccorr  <- sqrt(chi2 / (chi2 + n)) / sqrt((m - 1) / m)
    o <- obs + if (any(obs == 0)) 0.5 else 0
    odds <- (o[1, 1] * o[2, 2]) / (o[1, 2] * o[2, 1])
    rows[[v]] <- data.frame(variable = v, share_leg = mean(x[df$label == 0]),
                            share_phi = mean(x[df$label == 1]),
                            chi2 = chi2, cramer = cramer, ccorr = ccorr, odds = odds,
                            stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, rows)
  if (!is.null(res)) stopifnot(all(res$chi2 >= 0), all(res$cramer >= 0 & res$cramer <= 1 + 1e-9),
                               all(res$ccorr >= 0 & res$ccorr <= 1 + 1e-9), all(res$odds > 0))
  res
}


# gini_importance(X, y, n_max, seed)
# Trains a 300-tree Random Forest and takes the Gini importance of each variable
# (how much, on average, it lowers Gini impurity in the nodes that use it).
# It is normalized by dividing by the maximum. A sample of n_max URLs is used
# so it does not take too long.
# PRECONDITION: X numeric matrix without NA with column names, y in {0,1}.
# POSTCONDITION: one value between 0 and 1 per column, and the most important equals 1.
gini_importance <- function(X, y, n_max = 20000, seed = 2026) {
  stopifnot(is.matrix(X), !anyNA(X), nrow(X) == length(y), !is.null(colnames(X)))
  idx <- sample_for_plot(nrow(X), n_max, seed)
  set.seed(seed)
  rf  <- randomForest::randomForest(x = X[idx, , drop = FALSE],
                                    y = factor(y[idx], levels = c(0, 1)), ntree = 300)
  g <- randomForest::importance(rf, type = 2)[, 1]
  g <- g / max(g)
  stopifnot(all(g >= 0 & g <= 1), abs(max(g) - 1) < 1e-12)
  g[colnames(X)]
}


# select_features(rho, gini, rho_threshold, gini_threshold)
# A variable is kept if |Spearman| >= rho_threshold or if its normalized Gini
# >= gini_threshold. "Or" is used instead of "and" because Spearman only sees
# monotonic relationships and the forest can exploit others, so those
# variables should not be discarded.
# PRECONDITION: rho and gini have the same names.
# POSTCONDITION: at least 2 variables remain (otherwise the 2 with the highest Gini are taken).
select_features <- function(rho, gini, rho_threshold = 0.1, gini_threshold = 0.1) {
  stopifnot(setequal(names(rho), names(gini)))
  gini <- gini[names(rho)]
  sel <- names(rho)[abs(rho) >= rho_threshold | gini >= gini_threshold]
  if (length(sel) < 2) sel <- names(sort(gini, decreasing = TRUE))[1:2]
  stopifnot(length(sel) >= 2, all(sel %in% names(rho)))
  sel
}


# compute_vif(X)
# VIF_j = 1 / (1 - R^2_j), where R^2_j comes from regressing (OLS) variable j
# on all the others. It detects multicollinearity among 3 or more variables,
# which pairwise correlation does not show.
# PRECONDITION: X numeric, without NA, with at least 2 columns.
# POSTCONDITION: VIF >= 1 for every variable (Inf if collinearity is exact).
compute_vif <- function(X) {
  stopifnot(is.matrix(X), ncol(X) >= 2, !anyNA(X))
  vif <- sapply(seq_len(ncol(X)), function(j) {
    r2 <- summary(lm(X[, j] ~ X[, -j]))$r.squared
    if (r2 >= 1 - 1e-12) Inf else 1 / (1 - r2)
  })
  names(vif) <- colnames(X)
  stopifnot(all(vif >= 1 - 1e-9))
  vif
}


# vif_filter(X, threshold, n_max, seed)
# While some variable has a VIF above the threshold (10), the one with the
# highest VIF is dropped and VIF is recomputed.
# PRECONDITION: X numeric, without NA, with column names.
# POSTCONDITION: the remaining variables have VIF <= threshold (or only 2 remain).
# INVARIANT: each pass of the repeat loop drops exactly one variable.
vif_filter <- function(X, threshold = 10, n_max = 20000, seed = 2026) {
  stopifnot(is.matrix(X), !anyNA(X), !is.null(colnames(X)))
  Xm <- X[sample_for_plot(nrow(X), n_max, seed), , drop = FALSE]
  dropped <- character(0); values <- numeric(0)
  repeat {
    if (ncol(Xm) <= 2) break
    vif <- compute_vif(Xm)
    if (max(vif) <= threshold) break
    worst <- names(which.max(vif))
    dropped <- c(dropped, worst); values <- c(values, max(vif))
    Xm <- Xm[, colnames(Xm) != worst, drop = FALSE]
  }
  vif_final <- if (ncol(Xm) >= 2) compute_vif(Xm) else setNames(1, colnames(Xm))
  stopifnot(ncol(Xm) <= 2 || all(vif_final <= threshold))
  list(kept = colnames(Xm), dropped = dropped,
       vif_dropped = values, vif_final = vif_final)
}


# fit_pca(X, target_var)
# PCA on the selected variables. The first k components that together explain
# at least target_var of the variance are kept.
# PRECONDITION: X numeric without NA, with names; 0 < target_var <= 1.
# POSTCONDITION: 1 <= k <= ncol(X) and the cumulative variance at k is >= target_var.
fit_pca <- function(X, target_var = 0.90) {
  stopifnot(is.matrix(X), !anyNA(X), !is.null(colnames(X)),
            target_var > 0, target_var <= 1)
  model   <- prcomp(X, center = TRUE, scale. = TRUE)
  var_exp <- model$sdev^2 / sum(model$sdev^2)
  cum_var <- cumsum(var_exp)
  k       <- which(cum_var >= target_var - 1e-12)[1]
  stopifnot(k >= 1, k <= ncol(X))
  list(model = model, k = k, var_exp = var_exp, cum_var = cum_var)
}


# plot_pca(pca, y, fig_dir)
# Scatter of the first two components (pca_scatter.pdf) and a plot of the
# variance explained per component (pca_variance.pdf).
# PRECONDITION: pca comes from fit_pca() and y has the same length as its rows.
# POSTCONDITION: both files are created.
plot_pca <- function(pca, y, fig_dir) {
  Z <- pca$model$x
  stopifnot(length(y) == nrow(Z), ncol(Z) >= 2)
  d <- data.frame(PC1 = Z[, 1], PC2 = Z[, 2], Class = as_class(y))
  d <- d[sample_for_plot(nrow(d)), ]
  d <- d[order(d$Class), ]
  p1 <- ggplot(d, aes(PC1, PC2, colour = Class)) +
    geom_point(alpha = 0.35, size = 0.8) +
    scale_colour_manual(values = CLASS_COLORS, name = NULL) +
    labs(x = sprintf("PC1 (%.1f%% of the variance)", 100 * pca$var_exp[1]),
         y = sprintf("PC2 (%.1f%% of the variance)", 100 * pca$var_exp[2]),
         title = "Training URLs on the first two principal components") +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
  ggsave(file.path(fig_dir, "pca_scatter.pdf"), p1, width = 6.5, height = 4.5)

  dv <- data.frame(pc = seq_along(pca$var_exp), ind = pca$var_exp, cum = pca$cum_var)
  p2 <- ggplot(dv, aes(x = pc)) +
    geom_col(aes(y = ind), fill = "#2E86AB", alpha = 0.8) +
    geom_line(aes(y = cum), colour = "#D1495B") +
    geom_point(aes(y = cum), colour = "#D1495B", size = 1.5) +
    geom_vline(xintercept = pca$k, linetype = "dashed", colour = "grey50") +
    scale_y_continuous(labels = function(v) paste0(round(100 * v), "%")) +
    labs(x = "Principal component", y = "Explained variance",
         title = "Individual (bars) and cumulative (line) variance") +
    theme_minimal(base_size = 11)
  ggsave(file.path(fig_dir, "pca_variance.pdf"), p2, width = 6.5, height = 3.8)
  stopifnot(file.exists(file.path(fig_dir, c("pca_scatter.pdf", "pca_variance.pdf"))))
  invisible(NULL)
}


# plot_gini(gini, rho, selected, fig_dir)
# Bars with the Gini importance of each variable, marking which ones were kept
# (feature_selection.pdf).
# PRECONDITION: gini and rho have the same names; fig_dir exists.
# POSTCONDITION: the file is created.
plot_gini <- function(gini, rho, selected, fig_dir) {
  d <- data.frame(variable = names(gini), gini = as.numeric(gini),
                  rho = abs(as.numeric(rho[names(gini)])),
                  Status = ifelse(names(gini) %in% selected, "Selected", "Dropped"))
  d <- d[order(d$gini), ]
  d$variable <- factor(d$variable, levels = d$variable)
  p <- ggplot(d, aes(x = gini, y = variable, fill = Status)) +
    geom_col() +
    geom_vline(xintercept = 0.1, linetype = "dashed", colour = "grey40") +
    scale_fill_manual(values = c("Selected" = "#2E86AB", "Dropped" = "#C9CED6"), name = NULL) +
    labs(x = "Normalized Gini importance (Random Forest)", y = NULL,
         title = "Feature relevance") +
    theme_minimal(base_size = 10) + theme(legend.position = "top")
  path <- file.path(fig_dir, "feature_selection.pdf")
  ggsave(path, p, width = 6.5, height = 5.5)
  stopifnot(file.exists(path))
  invisible(NULL)
}


# interpret_logistic(X, y)
# Fits a binomial glm on the selected variables and returns what summary()
# gives: coefficient, standard error, z and p-value, plus the odds ratio
# exp(beta). Since the variables are standardized, exp(beta) says by how much
# the odds of phishing are multiplied when the variable increases by one
# standard deviation and the rest stay fixed.
# This is for interpretation; the final model is chosen later with caret.
# PRECONDITION: X numeric without NA with names, y in {0,1}.
# POSTCONDITION: one row per variable (without the intercept), with odds > 0
# and p-values between 0 and 1; the AIC is returned too.
interpret_logistic <- function(X, y) {
  stopifnot(is.matrix(X), !anyNA(X), !is.null(colnames(X)), all(y %in% c(0, 1)),
            nrow(X) == length(y))
  model <- suppressWarnings(glm(y ~ ., data = data.frame(X, y = y),
                                family = binomial(link = "logit")))
  co <- summary(model)$coefficients
  co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
  tab <- data.frame(variable = rownames(co), beta = co[, 1], se = co[, 2],
                    z = co[, 3], p = co[, 4], or = exp(co[, 1]),
                    stringsAsFactors = FALSE)
  tab <- tab[order(-abs(tab$z)), ]
  rownames(tab) <- NULL
  stopifnot(all(tab$or > 0), all(tab$p >= 0 & tab$p <= 1))
  list(table = tab, aic = AIC(model))
}
