# 07_export.R
# Saves the numbers and tables of the run to CSV files in results/, so they can
# be reviewed without running everything again. Numbers are saved unrounded.


# save_csv(df, path)
# Writes a data.frame to CSV.
# PRECONDITION: df is a data.frame and the folder of path exists.
# POSTCONDITION: the file exists at path, with one row per row of df.
save_csv <- function(df, path) {
  stopifnot(is.data.frame(df), dir.exists(dirname(path)))
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  stopifnot(file.exists(path))
  invisible(path)
}


# join_names(v)
# Joins several variable names into a single comma-separated string.
# PRECONDITION: v is character, possibly empty.
# POSTCONDITION: a single string ("none" if v is empty).
join_names <- function(v) {
  if (length(v) == 0) "none" else paste(v, collapse = ", ")
}


# values_table(lst)
# Turns a list of standalone results (key = value) into a table, with one
# column for numeric values and another for text values.
# PRECONDITION: lst has unique names and each element is a number or a string.
# POSTCONDITION: data.frame with one row per element and columns key, value and text.
# INVARIANT: each pass of the loop adds exactly one row.
values_table <- function(lst) {
  stopifnot(!is.null(names(lst)), !anyDuplicated(names(lst)))
  rows <- vector("list", length(lst))
  for (i in seq_along(lst)) {
    x <- lst[[i]]
    is_num <- is.numeric(x)
    rows[[i]] <- data.frame(key = names(lst)[i],
                            value = if (is_num) as.numeric(x) else NA_real_,
                            text = if (is_num) NA_character_ else as.character(x),
                            stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, rows)
  stopifnot(nrow(res) == length(lst))
  res
}


# filters_table(recipe, tr, relevant, rho, gini, vif)
# Lists the dropped variables, with the criterion that dropped them and the
# value that justifies it (mode share, correlation, Spearman or VIF).
# PRECONDITION: the arguments come from fit_recipe(), apply_recipe(),
# select_features() and vif_filter().
# POSTCONDITION: data.frame with one row per dropped variable, in the order the
# criteria were applied.
# INVARIANT: each loop adds exactly one row per variable it visits.
filters_table <- function(recipe, tr, relevant, rho, gini, vif) {
  # row(...): one row of the table. PRECONDITION: variable and criterion are strings.
  # POSTCONDITION: one-row data.frame with the five columns.
  row <- function(variable, criterion, value, other = NA_character_, g = NA_real_) {
    data.frame(variable = variable, criterion = criterion, value = value,
               other_variable = other, gini = g, stringsAsFactors = FALSE)
  }
  rows <- list()
  for (vv in recipe$dropped_variance)
    rows[[length(rows) + 1]] <- row(vv, "Near-zero variance", recipe$mode_share[[vv]])
  pr <- recipe$pairs
  for (i in seq_len(nrow(pr))) {
    other <- if (pr$dropped[i] == pr$var1[i]) pr$var2[i] else pr$var1[i]
    rows[[length(rows) + 1]] <- row(pr$dropped[i], "Pearson correlation", pr$r[i], other)
  }
  for (vv in setdiff(colnames(tr$X), relevant))
    rows[[length(rows) + 1]] <- row(vv, "Relevance (Spearman)", abs(rho[[vv]]), g = gini[[vv]])
  for (i in seq_along(vif$dropped))
    rows[[length(rows) + 1]] <- row(vif$dropped[i], "VIF", vif$vif_dropped[i])
  if (length(rows) == 0) return(row("none", NA_character_, NA_real_))
  do.call(rbind, rows)
}


# export_results(...)
# Saves the standalone values of the run (values.csv) and every table
# (table_*.csv) to out_dir.
# PRECONDITION: the arguments come from main.R, after the evaluation, and
# out_dir exists.
# POSTCONDITION: values.csv and the table_*.csv files exist in out_dir.
export_results <- function(out_dir, clean, exploration, partition, urls, tr, recipe,
                           descriptive, rho, contingency, gini, relevant, vif,
                           selected, pca, comparison, best_name, best_fit,
                           threshold, metrics, n_compare, concentration, logistic) {
  stopifnot(dir.exists(out_dir), best_name %in% comparison$model)
  s <- clean$summary
  y_tr <- urls$label[partition$train]; y_te <- urls$label[partition$test]
  rho_order <- names(sort(abs(rho), decreasing = TRUE))
  gini_order <- names(sort(gini, decreasing = TRUE))
  hp <- best_fit$bestTune
  hp_text <- if (identical(names(hp), "parameter")) "no hyperparameters" else
    paste0(names(hp), " = ", sapply(hp, function(v) format(v, digits = 3)), collapse = ", ")

  v <- list(
    "n_initial"          = s$n_initial,
    "n_invalid_label"    = s$n_invalid_label,
    "n_conflict"         = s$n_conflict,
    "n_duplicates"       = s$n_duplicates,
    "n_impossible"       = s$n_impossible,
    "n_missing"          = s$n_missing,
    "n_final"            = s$n_final,
    "n_phishing"         = s$n_phishing,
    "n_legitimate"       = s$n_legitimate,
    "share_phishing"     = s$n_phishing / s$n_final,
    "n_domains"          = s$n_domains,
    "max_per_domain"     = s$max_per_domain,
    "top_domain"         = s$top_domain,
    "n_train"            = length(partition$train),
    "n_test"             = length(partition$test),
    "share_phishing_train" = mean(y_tr),
    "share_phishing_test"  = mean(y_te),
    "domains_train"      = length(unique(urls$group[partition$train])),
    "domains_test"       = length(unique(urls$group[partition$test])),
    "https_legitimate"   = exploration$https_legitimate,
    "https_phishing"     = exploration$https_phishing,
    "n_sample_plots"     = exploration$n_sample,
    "n_candidates"       = length(CANDIDATE_COLS),
    "n_dropped_variance" = length(recipe$dropped_variance),
    "dropped_variance"   = join_names(recipe$dropped_variance),
    "n_dropped_cor"      = length(recipe$dropped_correlation),
    "dropped_cor"        = join_names(recipe$dropped_correlation),
    "n_after_filters"    = ncol(tr$X),
    "rho_top"            = join_names(head(rho_order, 3)),
    "rho_max"            = rho[[rho_order[1]]],
    "gini_top"           = join_names(head(gini_order, 3)),
    "n_relevant"         = length(relevant),
    "not_relevant"       = join_names(setdiff(colnames(tr$X), relevant)),
    "n_dropped_vif"      = length(vif$dropped),
    "dropped_vif"        = join_names(vif$dropped),
    "n_selected"         = length(selected),
    "selected"           = join_names(selected),
    "pca_k"              = pca$k,
    "pca_cum_k"          = pca$cum_var[pca$k],
    "pca_var_1"          = pca$var_exp[1],
    "pca_var_2"          = pca$var_exp[2],
    "n_compare"          = n_compare,
    "n_models"           = nrow(comparison),
    "best_model"         = best_name,
    "best_cv_auc"        = comparison$auc[1],
    "second_model"       = comparison$model[2],
    "second_cv_auc"      = comparison$auc[2],
    "worst_model"        = comparison$model[nrow(comparison)],
    "worst_cv_auc"       = comparison$auc[nrow(comparison)],
    "best_hyperparams"   = hp_text,
    "threshold"          = threshold,
    "accuracy" = metrics$accuracy, "sensitivity" = metrics$sensitivity,
    "specificity" = metrics$specificity, "precision" = metrics$precision,
    "f1" = metrics$f1, "auc" = metrics$auc,
    "tp" = metrics$TP, "fn" = metrics$FN,
    "fp" = metrics$FP, "tn" = metrics$TN,
    "gini_domains"       = concentration$G,
    "gini_domains_norm"  = concentration$Gn,
    "share_top1_domains" = concentration$share_top1,
    "share_single_url_domains" = concentration$share_single_url,
    "logit_aic"          = logistic$aic,
    "logit_top"          = logistic$table$variable[1],
    "logit_top_or"       = logistic$table$or[1]
  )

  sel_order <- selected[order(-gini[selected])]
  k <- seq_len(pca$k)
  files <- c(
    save_csv(values_table(v), file.path(out_dir, "values.csv")),
    save_csv(descriptive, file.path(out_dir, "table_descriptive.csv")),
    save_csv(filters_table(recipe, tr, relevant, rho, gini, vif),
             file.path(out_dir, "table_filters.csv")),
    save_csv(data.frame(variable = sel_order, spearman = unname(rho[sel_order]),
                        gini = unname(gini[sel_order]),
                        vif = unname(vif$vif_final[sel_order])),
             file.path(out_dir, "table_selection.csv")),
    save_csv(logistic$table, file.path(out_dir, "table_logistic.csv")),
    save_csv(data.frame(component = paste0("PC", k), variance = pca$var_exp[k],
                        cumulative = pca$cum_var[k]),
             file.path(out_dir, "table_pca.csv")),
    save_csv(comparison, file.path(out_dir, "table_comparison.csv")),
    save_csv(metrics, file.path(out_dir, "table_metrics.csv"))
  )
  if (!is.null(contingency) && nrow(contingency) > 0)
    files <- c(files, save_csv(contingency, file.path(out_dir, "table_contingency.csv")))
  stopifnot(all(file.exists(files)))
  invisible(files)
}


# export_session(out_dir)
# Saves the version of R and of every loaded library, to know what the results
# were obtained with.
# PRECONDITION: out_dir exists.
# POSTCONDITION: session.csv exists, with one row for R and one per library.
export_session <- function(out_dir) {
  stopifnot(dir.exists(out_dir))
  si <- sessionInfo()
  pkgs <- c(si$otherPkgs, si$loadedOnly)
  tab <- data.frame(component = c("R", names(pkgs)),
                    version = c(paste(R.version$major, R.version$minor, sep = "."),
                                vapply(pkgs, function(p) p$Version, character(1))),
                    stringsAsFactors = FALSE)
  save_csv(tab, file.path(out_dir, "session.csv"))
}
