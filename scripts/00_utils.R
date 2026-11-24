# 00_utils.R
# Small helpers used in several places: class labels, samples for plotting,
# metrics and threshold selection.

# plot colors: grey for legitimate, red for phishing
CLASS_COLORS <- c("Legitimate" = "#8A96A3", "Phishing" = "#D1495B")


# as_class(y)
# Turns the 0/1 label into a named factor, so plots read better.
# PRECONDITION: y contains only 0 and 1.
# POSTCONDITION: factor with levels "Legitimate" and "Phishing", same length as y.
as_class <- function(y) {
  stopifnot(all(y %in% c(0, 1)))
  factor(y, levels = c(0, 1), labels = names(CLASS_COLORS))
}


# caret_factor(y)
# caret needs the class as a factor with valid R names. "phishing" goes first
# because caret takes the first level as the positive class (for sensitivity
# and the ROC curve).
# PRECONDITION: y contains only 0 and 1.
# POSTCONDITION: factor with levels c("phishing", "legitimate"), same length as y.
caret_factor <- function(y) {
  stopifnot(all(y %in% c(0, 1)))
  f <- factor(ifelse(y == 1, "phishing", "legitimate"), levels = c("phishing", "legitimate"))
  stopifnot(length(f) == length(y), !anyNA(f))
  f
}


# sample_for_plot(n, n_max, seed)
# With this many URLs scatter plots get saturated and PDFs get heavy, so a
# random sample is plotted. It is also used for the caret sample.
# PRECONDITION: n >= 1 and n_max >= 1.
# POSTCONDITION: indices without repetition between 1 and n, at most n_max.
sample_for_plot <- function(n, n_max = 20000, seed = 2026) {
  stopifnot(n >= 1, n_max >= 1)
  set.seed(seed)
  idx <- if (n <= n_max) seq_len(n) else sort(sample.int(n, n_max))
  stopifnot(!anyDuplicated(idx), all(idx >= 1 & idx <= n))
  idx
}


# compute_metrics(y, prob, threshold)
# Builds the confusion matrix with the given threshold and computes the
# classification metrics. The positive class is phishing.
#   sensitivity = TP / (TP + FN)
#   specificity = TN / (TN + FP)
#   precision   = TP / (TP + FP)
#   F1          = 2 * precision * sensitivity / (precision + sensitivity)
#   accuracy    = (TP + TN) / n
#   AUC with pROC
# PRECONDITION: y in {0,1} with both classes present, prob in [0,1], same
# length, and 0 < threshold < 1.
# POSTCONDITION: one-row data.frame, TP+TN+FP+FN equals length(y) and every
# metric is between 0 and 1.
compute_metrics <- function(y, prob, threshold) {
  stopifnot(length(y) == length(prob), all(y %in% c(0, 1)),
            all(prob >= 0 & prob <= 1), threshold > 0, threshold < 1,
            length(unique(y)) == 2)
  pred <- as.integer(prob >= threshold)
  TP <- sum(pred == 1 & y == 1); TN <- sum(pred == 0 & y == 0)
  FP <- sum(pred == 1 & y == 0); FN <- sum(pred == 0 & y == 1)
  sens <- TP / (TP + FN)
  spec <- TN / (TN + FP)
  prec <- if (TP + FP > 0) TP / (TP + FP) else 0
  f1 <- if (prec + sens > 0) 2 * prec * sens / (prec + sens) else 0
  acc <- (TP + TN) / length(y)
  auc <- as.numeric(pROC::auc(pROC::roc(response = y, predictor = prob,
                                        levels = c(0, 1), direction = "<",
                                        quiet = TRUE)))
  res <- data.frame(threshold = threshold, TP = TP, FN = FN, FP = FP, TN = TN,
                    accuracy = acc, sensitivity = sens, specificity = spec,
                    precision = prec, f1 = f1, auc = auc)
  vals <- unlist(res[, c("accuracy", "sensitivity", "specificity",
                         "precision", "f1", "auc")])
  stopifnot(TP + TN + FP + FN == length(y), all(vals >= 0 & vals <= 1))
  res
}


# best_f1_threshold(y, prob, grid)
# Since phishing URLs are few (about 14%), 0.5 is not the best threshold.
# Every threshold in the grid is tried and the one with the highest F1 is kept.
# It is called with cross-validation predictions on train, never with test.
# PRECONDITION: y in {0,1}, prob in [0,1], grid inside (0,1).
# POSTCONDITION: returns a value from the grid.
# INVARIANT: best_f1 is the highest F1 found up to the current threshold.
best_f1_threshold <- function(y, prob, grid = seq(0.05, 0.95, by = 0.01)) {
  stopifnot(length(y) == length(prob), all(grid > 0 & grid < 1))
  best_t <- grid[1]
  best_f1 <- -Inf
  for (t in grid) {
    pred <- prob >= t
    tp <- sum(pred & y == 1); fp <- sum(pred & y == 0); fn <- sum(!pred & y == 1)
    f1 <- if (2 * tp + fp + fn > 0) 2 * tp / (2 * tp + fp + fn) else 0
    if (f1 > best_f1) { best_f1 <- f1; best_t <- t }
  }
  stopifnot(best_t %in% grid)
  best_t
}
