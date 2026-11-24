# Phishing URL detection from features of the URL itself
#
# Dataset: URL-Phish (Linh and Hung, 2025), downloaded from Mendeley Data
# (doi 10.17632/65z9twcx3r). It contains legitimate and phishing URLs with 22
# precomputed numeric features. The label column is 1 if the URL is phishing
# and 0 if it is legitimate.
#
# How to run: open phishing-url-detection.Rproj in RStudio (so the working
# directory is the project folder) and run this file with Source.
# It takes a few minutes, mostly because of the Random Forest.
# Figures are saved to figures/ and tables to results/.
#
# Functions live in the scripts/ folder, one stage per file.
# Every function documents its precondition, postcondition and invariant
# (when it applies), and the important ones are checked with stopifnot().
#
# Warning: the CSV contains real phishing URLs, do not open them in a browser.
#
# Libraries used (missing ones are installed by the block below):
#   caret        training and comparing the models
#   ggplot2      plots
#   pROC         ROC curves and AUC
#   randomForest Random Forest and Gini importance
#   kernlab      SVM (used by caret)
#   rpart        decision tree (used by caret, ships with R)

if(!require('caret')) {
  install.packages('caret')
  library('caret')}
if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')}
if(!require('pROC')) {
  install.packages('pROC')
  library('pROC')}
if(!require('randomForest')) {
  install.packages('randomForest')
  library('randomForest')}
if(!require('kernlab')) {
  install.packages('kernlab')
  library('kernlab')}
if(!require('rpart')) {
  install.packages('rpart')
  library('rpart')}

# ---- Parameters ----
SEED <- 2026             # so everything is reproducible
TRAIN_SHARE <- 0.70      # share of URLs used for training
VAR_THRESHOLD <- 0.995   # if one value appears in more than 99.5% of rows, the variable is near-constant
COR_THRESHOLD <- 0.90    # from |r| = 0.9 on, two variables are considered redundant
RHO_THRESHOLD <- 0.10    # minimum |Spearman| with the label to keep a variable
GINI_THRESHOLD <- 0.10   # or, otherwise, minimum (normalized) Gini importance
VIF_THRESHOLD <- 10
PCA_VARIANCE <- 0.90     # cumulative variance the components must explain
K_FOLDS <- 5
N_COMPARE <- 10000       # size of the sample used to compare models with caret
DATA_DIR <- "data"
FIG_DIR <- "figures"
OUT_DIR <- "results"

# PRECONDITION: the working directory must be the project folder
if (!dir.exists(DATA_DIR) || !dir.exists("scripts")) {
  stop("The data/ and scripts/ folders were not found. Open phishing-url-detection.Rproj in ",
       "RStudio (or use setwd() on the project folder).")
}
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# load all functions
for (script in sort(list.files("scripts", pattern = "\\.R$", full.names = TRUE))) {
  source(script, encoding = "UTF-8")
}


# ---- 1. Loading and cleaning ----
csv_path <- find_csv(DATA_DIR)
raw_df <- load_data(csv_path)
clean <- clean_data(raw_df)
urls <- clean$data
cat("Unique URLs after cleaning:", nrow(urls), "\n")


# ---- 2. Train/test split by domain ----
# Done first so the test set is not looked at until the end.
partition <- split_by_domain(urls$label, urls$group, TRAIN_SHARE, SEED)
train_df <- urls[partition$train, ]
test_df <- urls[partition$test, ]


# ---- 3. Preprocessing ----
# Everything is fitted on train and then applied unchanged to test:
# imputation, feature engineering, log, variance filter, correlation filter and standardization
recipe <- fit_recipe(train_df, VAR_THRESHOLD, COR_THRESHOLD)
tr <- apply_recipe(train_df, recipe)
te <- apply_recipe(test_df, recipe)
stopifnot(identical(colnames(tr$X), colnames(te$X)))   # both matrices must have the same columns
cat("Predictors after filters:", ncol(tr$X), "\n")

# for the descriptive statistics I use untransformed values (only imputed + new features)
train_desc <- create_features(impute(train_df, recipe$medians))


# ---- 4. Descriptive statistics (train only) ----
exploration <- plot_exploration(train_desc, FIG_DIR)
descriptive <- descriptive_table(train_desc, CANDIDATE_COLS)
concentration <- lorenz_gini(urls$group, FIG_DIR)   # how many URLs per domain, on the whole dataset


# ---- 5. Feature selection and extraction ----
rho <- spearman_association(tr$X, tr$y)
contingency <- binary_contingency_table(
  train_desc, c("is_https", "is_ip", "has_at", "has_query"))
gini <- gini_importance(tr$X, tr$y, n_max = 20000, seed = SEED)
relevant <- select_features(rho, gini, RHO_THRESHOLD, GINI_THRESHOLD)
vif <- vif_filter(tr$X[, relevant, drop = FALSE], VIF_THRESHOLD, seed = SEED)
selected <- vif$kept
cat("Selected features:", paste(selected, collapse = ", "), "\n")

plot_gini(gini, rho, selected, FIG_DIR)
top_boxplot <- head(names(sort(abs(rho[selected]), decreasing = TRUE)), 9)
plot_boxplots(train_desc, top_boxplot, FIG_DIR)

X_tr <- tr$X[, selected, drop = FALSE]
logistic <- interpret_logistic(X_tr, tr$y)   # glm summary(), to read odds ratios and p-values
X_te <- te$X[, selected, drop = FALSE]

pca <- fit_pca(X_tr, PCA_VARIANCE)
plot_pca(pca, tr$y, FIG_DIR)


# ---- 6. Model comparison with caret ----
# All models in MODEL_SPECS are trained in a single line, with the same folds
# (grouped by domain). A sample of N_COMPARE URLs is used because the full
# training set takes too long.
idx_comp <- sample_for_plot(nrow(X_tr), N_COMPARE, SEED)
y_tr_f <- caret_factor(tr$y)
control <- caret_control(train_df$group[idx_comp], K_FOLDS, PCA_VARIANCE, SEED)

fits <- lapply(MODEL_SPECS, function(spec) train_caret(spec, X_tr[idx_comp, ], y_tr_f[idx_comp], control))
fits <- Filter(Negate(is.null), fits)   # drop any model that failed
stopifnot(length(fits) >= 2)

comparison <- comparison_table(fits)
print(comparison, digits = 3)
plot_comparison(fits, FIG_DIR)

# keep the model with the highest mean AUC
best_name <- comparison$model[1]
best_spec <- MODEL_SPECS[[best_name]]
best_fit <- fits[[best_name]]
threshold <- threshold_from_cv(best_fit)   # the threshold comes from cross-validation, not from test
cat("Best model:", best_name, "| threshold:", threshold, "\n")


# ---- 7. Final model and evaluation on test ----
final_model <- train_final(best_spec, best_fit$bestTune, X_tr, y_tr_f, SEED)
prob_test <- predict_prob(final_model, X_te)
metrics <- compute_metrics(te$y, prob_test, threshold)
roc_test <- pROC::roc(response = te$y, predictor = prob_test, levels = c(0, 1),
                      direction = "<", quiet = TRUE)
print(metrics, digits = 3)

plot_roc(roc_test, best_name, threshold, FIG_DIR)
plot_confusion_matrix(te$y, prob_test, threshold, FIG_DIR)
top_var <- names(sort(gini[selected], decreasing = TRUE))[1]
plot_probability_scatter(X_te[, top_var], top_var, te$y, prob_test, threshold, FIG_DIR)


# ---- 8. Save tables and values to CSV ----
export_results(
  out_dir = OUT_DIR, clean = clean, exploration = exploration, partition = partition,
  urls = urls, tr = tr, recipe = recipe, descriptive = descriptive, rho = rho,
  contingency = contingency, gini = gini, relevant = relevant, vif = vif,
  selected = selected, pca = pca, comparison = comparison,
  best_name = best_name, best_fit = best_fit, threshold = threshold,
  metrics = metrics, n_compare = length(idx_comp),
  concentration = concentration, logistic = logistic
)

export_session(OUT_DIR)
cat("\nDone. Figures in", FIG_DIR, "and tables in", OUT_DIR, "\n")
