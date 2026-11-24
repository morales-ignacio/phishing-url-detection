# 05_modeling.R
# Model comparison with caret and training of the best model.
# Models from two families are tested:
#   geometric: logistic regression, KNN and linear SVM
#   logical: decision tree (rpart) and Random Forest
# All of them are trained with the same call (an lapply over the list of
# models), the same 5-fold cross-validation grouped by domain and the same
# selection metric (ROC AUC). Only the best one is kept afterwards.

MODEL_SPECS <- list(
  "Logistic regression" = list(method = "glm",         preproc = NULL),
  "Decision tree"       = list(method = "rpart",       preproc = NULL),
  "KNN"                 = list(method = "knn",         preproc = NULL),
  "Linear SVM"          = list(method = "svmLinear",   preproc = NULL),
  "Random Forest"       = list(method = "rf",          preproc = NULL)
)

# KNN and SVM are slow with many rows, so their final model is trained on a
# sample of at most MAX_FINAL_SLOW URLs (see train_final()).
SLOW_METHODS   <- c("knn", "svmLinear")
MAX_FINAL_SLOW <- 30000

# caret_summary(data, lev, model)
# Summary caret computes in each fold: ROC AUC, sensitivity and specificity
# (same as twoClassSummary) plus F1.
# PRECONDITION: lev[1] is the positive class ("phishing"); data has obs, pred
# and one probability column per class.
# POSTCONDITION: vector named ROC, Sens, Spec and F1.
caret_summary <- function(data, lev = NULL, model = NULL) {
  base <- caret::twoClassSummary(data, lev, model)
  pos <- lev[1]
  tp <- sum(data$pred == pos & data$obs == pos)
  fp <- sum(data$pred == pos & data$obs != pos)
  fn <- sum(data$pred != pos & data$obs == pos)
  f1 <- if (2 * tp + fp + fn > 0) 2 * tp / (2 * tp + fp + fn) else 0
  c(base, F1 = f1)
}

# caret_control(group, k, pca_var, seed)
# k-fold cross-validation grouped by domain (groupKFold). All URLs from the
# same domain fall in the same fold; otherwise the model would be validated on
# domains it already saw during training and the result would look better than
# it is.
# PRECONDITION: group has at least k distinct values.
# POSTCONDITION: a trainControl with k training indices.
caret_control <- function(group, k = 5, pca_var = 0.90, seed = 2026) {
  stopifnot(length(unique(group)) >= k)
  set.seed(seed)
  indices <- caret::groupKFold(group, k = k)
  ctrl <- caret::trainControl(method = "cv", index = indices, classProbs = TRUE,
                              summaryFunction = caret_summary,
                              savePredictions = "final",
                              preProcOptions = list(thresh = pca_var))
  stopifnot(length(ctrl$index) == k)
  ctrl
}

# train_caret(spec, X, y, control, seed)
# Trains one model with caret::train, trying 3 values per hyperparameter
# (tuneLength = 3), and keeps the one with the highest ROC AUC.
# If a model fails, NULL is returned with a warning, so the comparison of the
# others is not interrupted.
# PRECONDITION: spec has $method and $preproc; X numeric matrix without NA;
# y factor with levels c("phishing", "legitimate").
# POSTCONDITION: a 'train' object with the cross-validation results, or NULL.
train_caret <- function(spec, X, y, control, seed = 2026) {
  stopifnot(is.matrix(X), !anyNA(X), is.factor(y),
            identical(levels(y), c("phishing", "legitimate")), nrow(X) == length(y))
  set.seed(seed)
  fit <- tryCatch(suppressWarnings(caret::train(
    x = as.data.frame(X), y = y, method = spec$method, preProcess = spec$preproc,
    metric = "ROC", trControl = control, tuneLength = 3)),
    error = function(e) { warning("Could not train ", spec$method, ": ",
                                  conditionMessage(e)); NULL })
  stopifnot(is.null(fit) || (inherits(fit, "train") && "ROC" %in% names(fit$results)))
  fit
}

# comparison_table(fits)
# Mean and standard deviation across folds of AUC, sensitivity, specificity and
# F1 (with a 0.5 threshold) for the best configuration of each model.
# PRECONDITION: fits is a named list of 'train' objects.
# POSTCONDITION: data.frame with one row per model, sorted by AUC.
comparison_table <- function(fits) {
  rows <- lapply(names(fits), function(nm) {
    r <- fits[[nm]]$resample
    data.frame(model = nm, auc = mean(r$ROC), auc_sd = sd(r$ROC),
               sens = mean(r$Sens), spec = mean(r$Spec), f1 = mean(r$F1),
               stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  tab <- tab[order(-tab$auc), ]
  rownames(tab) <- NULL
  stopifnot(nrow(tab) == length(fits), !is.unsorted(rev(tab$auc)))
  tab
}

# plot_comparison(fits, fig_dir)
# Boxplot of each model's AUC over the 5 folds (model_comparison.pdf, plus
# model_comparison.png for the README).
# PRECONDITION: fits is named; fig_dir exists.
# POSTCONDITION: the file is created.
plot_comparison <- function(fits, fig_dir) {
  d <- do.call(rbind, lapply(names(fits), function(nm)
    data.frame(model = nm, auc = fits[[nm]]$resample$ROC)))
  ord <- names(sort(tapply(d$auc, d$model, mean)))
  d$model <- factor(d$model, levels = ord)
  p <- ggplot(d, aes(x = auc, y = model)) +
    geom_boxplot(fill = "#2E86AB", alpha = 0.5) +
    geom_point(size = 1.2, alpha = 0.7) +
    labs(x = "ROC AUC in each fold (cross-validation grouped by domain)", y = NULL,
         title = "Model comparison with caret") +
    theme_minimal(base_size = 11)
  path <- file.path(fig_dir, "model_comparison.pdf")
  ggsave(path, p, width = 6.5, height = 4)
  ggsave(file.path(fig_dir, "model_comparison.png"), p, width = 6.5, height = 4, dpi = 120)   # for the README
  stopifnot(file.exists(path))
  invisible(NULL)
}

# threshold_from_cv(fit)
# Finds the threshold that maximizes F1 using the cross-validation predictions
# caret keeps (savePredictions) for the best configuration.
# This way the threshold is not chosen by looking at the test set.
# PRECONDITION: fit trained with savePredictions = "final" and classProbs = TRUE.
# POSTCONDITION: threshold between 0 and 1.
threshold_from_cv <- function(fit) {
  p <- fit$pred
  stopifnot(!is.null(p), "phishing" %in% names(p))
  t <- best_f1_threshold(as.integer(p$obs == "phishing"), p$phishing)
  stopifnot(t > 0, t < 1)
  t
}

# train_final(spec, best_tune, X, y, seed)
# Retrains the best model with the hyperparameters chosen by caret, now on the
# whole training set and without validation (trainControl "none").
# For KNN and SVM, which become very slow with many rows, a sample of at most
# MAX_FINAL_SLOW URLs is used.
# PRECONDITION: best_tune is caret's bestTune; X and y as in train_caret().
# POSTCONDITION: a 'train' object ready to predict probabilities.
train_final <- function(spec, best_tune, X, y, seed = 2026) {
  stopifnot(is.data.frame(best_tune), nrow(X) == length(y))
  idx <- if (spec$method %in% SLOW_METHODS)
    sample_for_plot(nrow(X), MAX_FINAL_SLOW, seed) else seq_len(nrow(X))
  set.seed(seed)
  model <- suppressWarnings(caret::train(
    x = as.data.frame(X[idx, , drop = FALSE]), y = y[idx], method = spec$method,
    preProcess = spec$preproc, tuneGrid = best_tune,
    trControl = caret::trainControl(method = "none", classProbs = TRUE,
                                    preProcOptions = list(thresh = 0.90))))
  stopifnot(inherits(model, "train"))
  model
}

# predict_prob(model, X)
# Probability of phishing for each row.
# PRECONDITION: X has the same columns used for training.
# POSTCONDITION: one value between 0 and 1 per row.
predict_prob <- function(model, X) {
  p <- predict(model, newdata = as.data.frame(X), type = "prob")[, "phishing"]
  stopifnot(length(p) == nrow(X), !anyNA(p), all(p >= 0 & p <= 1))
  as.numeric(p)
}
