# 06_evaluation.R
# Plots of the best model on the test set: ROC curve, confusion matrix and a
# scatter of the predicted probabilities.

# plot_roc(roc_obj, name, threshold, fig_dir)
# ROC curve of the best model with its AUC, marking the point that corresponds
# to the chosen threshold (roc.pdf, plus roc.png for the README).
# PRECONDITION: roc_obj is a pROC object; 0 < threshold < 1.
# POSTCONDITION: the file is created.
plot_roc <- function(roc_obj, name, threshold, fig_dir) {
  stopifnot(inherits(roc_obj, "roc"), threshold > 0, threshold < 1)
  d <- data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities)
  point <- pROC::coords(roc_obj, x = threshold, input = "threshold",
                        ret = c("specificity", "sensitivity"), transpose = FALSE)
  p <- ggplot(d, aes(x = fpr, y = tpr)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey65") +
    geom_path(colour = "#2E86AB", linewidth = 0.9) +
    geom_point(data = data.frame(fpr = 1 - point$specificity, tpr = point$sensitivity),
               colour = "#D1495B", size = 3) +
    coord_equal() +
    labs(x = "1 - Specificity (false positive rate)",
         y = "Sensitivity (true positive rate)",
         title = sprintf("Test ROC curve: %s (AUC = %.3f)", name,
                         as.numeric(pROC::auc(roc_obj))),
         subtitle = sprintf("Red point: chosen threshold = %.2f", threshold)) +
    theme_minimal(base_size = 11)
  path <- file.path(fig_dir, "roc.pdf")
  ggsave(path, p, width = 6, height = 5.5)
  ggsave(file.path(fig_dir, "roc.png"), p, width = 6, height = 5.5, dpi = 120)   # for the README
  stopifnot(file.exists(path))
  invisible(NULL)
}

# plot_confusion_matrix(y, prob, threshold, fig_dir)
# Confusion matrix with counts and the share within each true class, so the
# phishing row shows sensitivity and the legitimate row shows specificity
# (confusion_matrix.pdf, plus confusion_matrix.png for the README).
# PRECONDITION: y in {0,1}; prob of the same length as y; 0 < threshold < 1.
# POSTCONDITION: the file is created and the cells add up to length(y).
plot_confusion_matrix <- function(y, prob, threshold, fig_dir) {
  stopifnot(length(y) == length(prob), all(y %in% c(0, 1)))
  pred <- as.integer(prob >= threshold)
  tab  <- table(Actual = factor(y, levels = c(0, 1)), Predicted = factor(pred, levels = c(0, 1)))
  stopifnot(sum(tab) == length(y))
  d <- as.data.frame(tab, responseName = "n")
  d$share <- d$n / ave(d$n, d$Actual, FUN = sum)
  labels <- c("0" = names(CLASS_COLORS)[1], "1" = names(CLASS_COLORS)[2])
  d$Actual    <- factor(labels[as.character(d$Actual)], levels = rev(labels))
  d$Predicted <- factor(labels[as.character(d$Predicted)], levels = labels)
  p <- ggplot(d, aes(x = Predicted, y = Actual, fill = share)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, 100 * share)), size = 4) +
    scale_fill_gradient(low = "#F4F6F8", high = "#2E86AB", limits = c(0, 1),
                        name = "Share of\nthe row") +
    labs(x = "Predicted class", y = "Actual class",
         title = sprintf("Test confusion matrix (threshold %.2f)", threshold)) +
    theme_minimal(base_size = 11) + theme(panel.grid = element_blank())
  path <- file.path(fig_dir, "confusion_matrix.pdf")
  ggsave(path, p, width = 5.5, height = 4.2)
  ggsave(file.path(fig_dir, "confusion_matrix.png"), p, width = 5.5, height = 4.2, dpi = 120)   # for the README
  stopifnot(file.exists(path))
  invisible(NULL)
}

# plot_probability_scatter(x, x_name, y, prob, threshold, fig_dir)
# Predicted phishing probability against the most relevant variable, for a
# sample of test URLs colored by their true class (scatter_probability.pdf).
# The dotted line is the threshold: red points below it are missed phishing and
# grey points above it are false alarms.
# PRECONDITION: x, y and prob of the same length; 0 < threshold < 1.
# POSTCONDITION: the file is created.
plot_probability_scatter <- function(x, x_name, y, prob, threshold, fig_dir) {
  stopifnot(length(x) == length(y), length(y) == length(prob))
  d <- data.frame(x = x, prob = prob, Class = as_class(y))
  d <- d[sample_for_plot(nrow(d)), ]
  d <- d[order(d$Class), ]
  p <- ggplot(d, aes(x = x, y = prob, colour = Class)) +
    geom_jitter(alpha = 0.4, size = 0.8, width = 0.02, height = 0) +
    geom_hline(yintercept = threshold, linetype = "dotted") +
    scale_colour_manual(values = CLASS_COLORS, name = NULL) +
    labs(x = paste0(x_name, " (standardized)"), y = "Estimated P(phishing)",
         title = "Estimated probability on the test set") +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
  path <- file.path(fig_dir, "scatter_probability.pdf")
  ggsave(path, p, width = 6.5, height = 4.5)
  stopifnot(file.exists(path))
  invisible(NULL)
}
