# 02_descriptive.R
# Descriptive statistics and exploratory plots. Everything uses train only;
# the test set is not touched until the end.


# descriptive_table(df, columns)
# For each variable, split into legitimate and phishing: mean, median, standard
# deviation, interquartile range (Q75 - Q25) and coefficient of variation (sd / mean).
# PRECONDITION: df has label in {0,1} and the requested numeric columns.
# POSTCONDITION: data.frame with one row per variable, with IQR and sd >= 0.
descriptive_table <- function(df, columns) {
  stopifnot(all(columns %in% names(df)), all(df$label %in% c(0, 1)))
  # cv(x): coefficient of variation. PRECONDITION: x numeric.
  # POSTCONDITION: sd / mean, or NA if the mean is 0.
  cv <- function(x) { m <- mean(x, na.rm = TRUE); if (abs(m) > 0) sd(x, na.rm = TRUE) / m else NA }
  # iqr(x): interquartile range. PRECONDITION: x numeric.
  # POSTCONDITION: Q75 - Q25, greater than or equal to 0.
  iqr <- function(x) unname(diff(quantile(x, c(0.25, 0.75), na.rm = TRUE)))
  rows <- lapply(columns, function(col) {
    l <- df[[col]][df$label == 0]; p <- df[[col]][df$label == 1]
    data.frame(variable = col,
               mean_leg = mean(l, na.rm = TRUE), mean_phi = mean(p, na.rm = TRUE),
               median_leg = median(l, na.rm = TRUE), median_phi = median(p, na.rm = TRUE),
               sd_leg = sd(l, na.rm = TRUE), sd_phi = sd(p, na.rm = TRUE),
               iqr_leg = iqr(l), iqr_phi = iqr(p),
               cv_leg = cv(l), cv_phi = cv(p), stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, rows)
  stopifnot(nrow(res) == length(columns),
            all(res$iqr_leg >= 0 & res$iqr_phi >= 0, na.rm = TRUE),
            all(res$sd_leg >= 0 & res$sd_phi >= 0, na.rm = TRUE))
  res
}


# plot_boxplots(df, columns, fig_dir)
# Boxplots by class of the requested columns (boxplots.pdf). A log(1 + x) scale
# is used because otherwise the long tails flatten the boxes.
# PRECONDITION: the columns exist in df, are >= 0, and fig_dir exists.
# POSTCONDITION: the file was created.
plot_boxplots <- function(df, columns, fig_dir) {
  stopifnot(all(columns %in% names(df)), dir.exists(fig_dir))
  d <- df[sample_for_plot(nrow(df)), ]
  long <- do.call(rbind, lapply(columns, function(col)
    data.frame(variable = col, value = log1p(pmax(d[[col]], 0)),
               Class = as_class(d$label))))
  long$variable <- factor(long$variable, levels = columns)
  p <- ggplot(long, aes(x = Class, y = value, fill = Class)) +
    geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3, na.rm = TRUE) +
    facet_wrap(~ variable, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = CLASS_COLORS, guide = "none") +
    labs(x = NULL, y = "log(1 + value)", title = "Boxplots by class") +
    theme_minimal(base_size = 10)
  path <- file.path(fig_dir, "boxplots.pdf")
  ggsave(path, p, width = 7.5, height = 6)
  stopifnot(file.exists(path))
  invisible(NULL)
}


# plot_exploration(df, fig_dir)
# Two scatter plots (length vs. entropy and domain length vs. path length) and
# the Pearson correlation heatmap.
# PRECONDITION: df has NUMERIC_COLS and label; fig_dir exists.
# POSTCONDITION: the three PDFs are created; returns the share of HTTPS per class
# and the size of the plotted sample.
plot_exploration <- function(df, fig_dir) {
  stopifnot(is.data.frame(df), dir.exists(fig_dir), "label" %in% names(df))
  d <- df[sample_for_plot(nrow(df)), ]
  d$Class <- as_class(d$label)
  d <- d[order(d$label), ]   # sorted so phishing is drawn on top

  p1 <- ggplot(d, aes(x = log1p(url_len), y = entropy, colour = Class)) +
    geom_point(alpha = 0.3, size = 0.7, na.rm = TRUE) +
    scale_colour_manual(values = CLASS_COLORS, name = NULL) +
    labs(x = "log(1 + URL length)", y = "Shannon entropy of the URL",
         title = "URL length and entropy") +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
  ggsave(file.path(fig_dir, "scatter_length_entropy.pdf"), p1, width = 6.5, height = 4.2)

  p2 <- ggplot(d, aes(x = log1p(dom_len), y = log1p(path_len), colour = Class)) +
    geom_jitter(alpha = 0.3, size = 0.7, width = 0.02, height = 0.02, na.rm = TRUE) +
    scale_colour_manual(values = CLASS_COLORS, name = NULL) +
    labs(x = "log(1 + domain length)", y = "log(1 + path length)",
         title = "URL domain and path") +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
  ggsave(file.path(fig_dir, "scatter_domain_path.pdf"), p2, width = 6.5, height = 4.2)

  m <- suppressWarnings(cor(df[, c(NUMERIC_COLS, "label")], use = "pairwise.complete.obs"))
  dm <- data.frame(v1 = rep(rownames(m), times = ncol(m)),
                   v2 = rep(colnames(m), each = nrow(m)), r = as.vector(m))
  dm$v1 <- factor(dm$v1, levels = rownames(m))
  dm$v2 <- factor(dm$v2, levels = rev(colnames(m)))
  p3 <- ggplot(dm, aes(x = v1, y = v2, fill = r)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = ifelse(is.na(r), "", sprintf("%.2f", r))), size = 1.8) +
    scale_fill_gradient2(low = "#2E86AB", mid = "white", high = "#D1495B",
                         midpoint = 0, limits = c(-1, 1), name = "r", na.value = "grey90") +
    labs(x = NULL, y = NULL, title = "Pearson correlation") +
    theme_minimal(base_size = 8) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(fig_dir, "correlation.pdf"), p3, width = 7.5, height = 6.5)

  stopifnot(all(file.exists(file.path(fig_dir, c("scatter_length_entropy.pdf",
                                                  "scatter_domain_path.pdf",
                                                  "correlation.pdf")))))
  list(https_legitimate = mean(df$is_https[df$label == 0], na.rm = TRUE),
       https_phishing = mean(df$is_https[df$label == 1], na.rm = TRUE),
       n_sample = nrow(d))
}


# lorenz_gini(group, fig_dir)
# Lorenz curve and Gini coefficient of the number of URLs per domain, to see
# whether a few domains concentrate many URLs.
# With the counts sorted from smallest to largest:
#   u_i = i/n,  v_i = sum of the first i / total
#   G = 1 - (1/n) * sum(v_{i-1} + v_i),  G+ = n/(n-1) * G
# PRECONDITION: group has at least 2 distinct values.
# POSTCONDITION: 0 <= G <= (n-1)/n and G+ <= 1; lorenz.pdf is created.
# INVARIANT: v is non-decreasing and ends at 1.
lorenz_gini <- function(group, fig_dir) {
  x <- sort(as.numeric(table(group)))
  n <- length(x)
  stopifnot(n >= 2, all(x > 0))
  u <- seq_len(n) / n
  v <- cumsum(x) / sum(x)
  stopifnot(!is.unsorted(v), abs(v[n] - 1) < 1e-9)   # invariant
  G <- 1 - sum(c(0, v[-n]) + v) / n
  Gn <- n / (n - 1) * G
  stopifnot(G >= -1e-9, G <= (n - 1) / n + 1e-9, Gn <= 1 + 1e-9)
  idx <- unique(round(seq(1, n, length.out = 2000)))   # no need to plot all 90 thousand points
  d <- data.frame(u = c(0, u[idx]), v = c(0, v[idx]))
  p <- ggplot(d, aes(u, v)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_ribbon(aes(ymin = v, ymax = u), fill = "#2E86AB", alpha = 0.15) +
    geom_line(colour = "#2E86AB", linewidth = 0.9) +
    coord_equal() +
    labs(x = "Cumulative share of domains (u)",
         y = "Cumulative share of URLs (v)",
         title = sprintf("Lorenz curve of URLs per domain (G = %.3f)", G)) +
    theme_minimal(base_size = 11)
  path <- file.path(fig_dir, "lorenz.pdf")
  ggsave(path, p, width = 5.5, height = 5.2)
  stopifnot(file.exists(path))
  top1 <- sum(tail(x, ceiling(0.01 * n))) / sum(x)   # share of URLs held by the largest 1% of domains
  list(G = G, Gn = Gn, n_domains = n, share_top1 = top1,
       share_single_url = mean(x == 1))
}
