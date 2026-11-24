# 03_preprocessing.R
# Split by domain, imputation, feature engineering, transformations and filters.
#
# The idea is to work in two steps: fit_recipe() computes everything that
# depends on the data (medians, which variables to drop, means and standard
# deviations) using train only, and apply_recipe() applies exactly that to any
# set. This way the test set has no influence on preprocessing.

# words that often appear in phishing URLs (logins, accounts, payments)
SUSPICIOUS_WORDS <- c("login", "signin", "sign-in", "verif", "secure",
                      "account", "update", "bank", "confirm", "password",
                      "webscr", "wallet", "billing", "suspend", "unlock",
                      "support")

ENGINEERED_COLS <- c("n_suspicious", "has_at", "has_query",
                     "path_ratio", "domain_ratio")

# variables transformed with log(1 + x) because they are very skewed
LOG_COLS <- c(COUNT_COLS, "n_suspicious")

# all candidates: the 22 original features plus the 5 engineered ones
CANDIDATE_COLS <- c(NUMERIC_COLS, ENGINEERED_COLS)


# split_by_domain(y, group, train_share, seed)
# Splits train and test so that no domain ends up in both. With a random split
# by row, URLs from the same site fall on both sides and the model can learn to
# recognize the site instead of phishing (as shown by Abood, 2026).
#
# The catch is that some domains have thousands of URLs, and sampling domains
# at random can leave very different phishing rates in train and test. So:
#  1. each domain is assigned the majority class of its URLs
#  2. within each class, domains are visited from most to fewest URLs (ties in
#     random order) and each one goes to the set that is furthest from its
#     target (70% of the URLs to train, 30% to test)
# This way each set ends up with roughly 70/30 of each class.
# PRECONDITION: y in {0,1}, group of the same length as y, 0 < train_share < 1.
# POSTCONDITION: train and test do not overlap, together they cover every row
# and they share no domains. If the phishing rate differs by more than 2
# points, a warning is raised.
# INVARIANT: within each class, a and b are the URLs assigned so far to train
# and to test.
split_by_domain <- function(y, group, train_share = 0.7, seed = 2026) {
  stopifnot(all(y %in% c(0, 1)), length(group) == length(y),
            train_share > 0, train_share < 1)
  set.seed(seed)
  group_class <- tapply(y, group, function(v) as.integer(mean(v) >= 0.5))
  group_size  <- table(group)
  train_groups <- character(0)
  for (cls in c(0, 1)) {
    g    <- names(group_class)[group_class == cls]
    g    <- g[sample.int(length(g))]   # random order for ties
    size <- as.numeric(group_size[g])
    ord  <- order(-size)   # order() is stable, so ties keep the random order
    g <- g[ord]; size <- size[ord]
    total <- sum(size); a <- 0; b <- 0
    in_train <- logical(length(g))
    for (i in seq_along(g)) {
      if (a / (train_share * total) <= b / ((1 - train_share) * total)) {
        in_train[i] <- TRUE; a <- a + size[i]
      } else {
        b <- b + size[i]
      }
    }
    stopifnot(a + b == total)   # invariant check
    train_groups <- c(train_groups, g[in_train])
  }
  in_train  <- group %in% train_groups
  idx_train <- which(in_train)
  idx_test  <- which(!in_train)
  stopifnot(length(intersect(idx_train, idx_test)) == 0,
            length(idx_train) + length(idx_test) == length(y),
            length(intersect(unique(group[idx_train]), unique(group[idx_test]))) == 0)
  diff_rate <- abs(mean(y[idx_train]) - mean(y[idx_test]))
  if (diff_rate > 0.02) warning(sprintf("The phishing rate differs by %.1f points between train and test.", 100 * diff_rate))
  list(train = idx_train, test = idx_test)
}


# impute(df, medians)
# Replaces NA with the median (computed on train). The median is used because
# lengths have very long tails.
# PRECONDITION: the names of medians are columns of df.
# POSTCONDITION: those columns have no NA and the rest is unchanged.
# INVARIANT: each pass of the loop fills a single column, and columns already
# filled do not change again.
impute <- function(df, medians) {
  stopifnot(all(names(medians) %in% names(df)))
  for (col in names(medians)) df[[col]][is.na(df[[col]])] <- medians[[col]]
  stopifnot(!anyNA(df[, names(medians)]))
  df
}


# create_features(df)
# New features, computed row by row:
#   n_suspicious: how many of the SUSPICIOUS_WORDS appear in the URL
#   has_at: whether the URL contains "@" (the browser ignores what comes before
#           it, a known trick to disguise the real destination)
#   has_query: whether the URL has query parameters
#   path_ratio: path_len / url_len
#   domain_ratio: dom_len / url_len
# PRECONDITION: url, path_len, dom_len, url_len and query_len are present, with no NA in the numeric ones.
# POSTCONDITION: the 5 columns are added; ratios lie in [0,1] and indicators
# in {0,1}.
# INVARIANT: after each pass of the loop, n_suspicious counts how many of the
# words visited so far appear in the URL.
create_features <- function(df) {
  stopifnot(all(c("url", "path_len", "dom_len", "url_len", "query_len") %in% names(df)),
            !anyNA(df[, c("path_len", "dom_len", "url_len", "query_len")]))
  u <- tolower(ifelse(is.na(df$url), "", df$url))
  df$n_suspicious <- 0
  for (word in SUSPICIOUS_WORDS) {
    df$n_suspicious <- df$n_suspicious + grepl(word, u, fixed = TRUE)
  }
  df$has_at    <- as.integer(grepl("@", u, fixed = TRUE))
  df$has_query <- as.integer(df$query_len > 0)
  len <- pmax(df$url_len, 1)
  df$path_ratio   <- pmin(pmax(df$path_len / len, 0), 1)
  df$domain_ratio <- pmin(pmax(df$dom_len / len, 0), 1)
  stopifnot(all(df$n_suspicious >= 0),
            all(df$has_at %in% c(0, 1)), all(df$has_query %in% c(0, 1)),
            all(df$path_ratio >= 0 & df$path_ratio <= 1),
            all(df$domain_ratio >= 0 & df$domain_ratio <= 1))
  df
}


# log_transform(df)
# log(1 + x) on the LOG_COLS columns, to reduce skewness (some URLs are
# hundreds of characters long).
# PRECONDITION: the columns exist and are >= 0.
# POSTCONDITION: finite values >= 0.
# INVARIANT: each pass of the loop transforms a single column, exactly once.
log_transform <- function(df) {
  stopifnot(all(LOG_COLS %in% names(df)), all(df[, LOG_COLS] >= 0))
  for (col in LOG_COLS) df[[col]] <- log1p(df[[col]])
  stopifnot(all(is.finite(as.matrix(df[, LOG_COLS]))))
  df
}


# variance_filter(df_num, threshold)
# Drops near-constant variables: if the most frequent value appears in more
# than threshold of the rows, the variable has almost no spread and adds nothing.
# PRECONDITION: df_num numeric without NA, threshold between 0.5 and 1.
# POSTCONDITION: returns which variables to drop and the share of the most
# frequent value of each one.
variance_filter <- function(df_num, threshold = 0.995) {
  stopifnot(!anyNA(df_num), threshold > 0.5, threshold < 1)
  mode_share <- sapply(df_num, function(x) max(table(x)) / length(x))
  drop <- names(mode_share)[mode_share > threshold]
  stopifnot(all(drop %in% names(df_num)))
  list(dropped = drop, mode_share = mode_share)
}


# correlation_filter(df_num, threshold, protected)
# While some pair has |r| > threshold, the variable of the pair with the
# highest mean correlation with the rest is dropped. "protected" variables are
# never dropped (none are used here, but the option is kept).
# PRECONDITION: df_num numeric, without NA and with at least 2 columns.
# POSTCONDITION: no remaining pair has |r| > threshold.
# INVARIANT: m holds only the variables still in play, and no protected
# variable is in dropped.
correlation_filter <- function(df_num, threshold = 0.90, protected = character(0)) {
  stopifnot(ncol(df_num) >= 2, !anyNA(df_num))
  r_signed <- suppressWarnings(cor(df_num))
  r_signed[is.na(r_signed)] <- 0   # happens with constant columns
  m <- abs(r_signed); diag(m) <- 0
  dropped <- character(0)
  pairs <- data.frame(var1 = character(0), var2 = character(0),
                      r = numeric(0), dropped = character(0),
                      stringsAsFactors = FALSE)
  repeat {
    if (ncol(m) < 2 || max(m) <= threshold) break
    pos <- which(m == max(m), arr.ind = TRUE)[1, ]
    a <- rownames(m)[pos[1]]; b <- colnames(m)[pos[2]]
    if (a %in% protected && b %in% protected) { m[a, b] <- 0; m[b, a] <- 0; next }
    remove <- if (a %in% protected) b else if (b %in% protected) a else
              if (mean(m[a, ]) >= mean(m[b, ])) a else b
    pairs <- rbind(pairs, data.frame(var1 = a, var2 = b, r = r_signed[a, b],
                                     dropped = remove, stringsAsFactors = FALSE))
    dropped <- c(dropped, remove)
    keep <- setdiff(rownames(m), remove)
    m <- m[keep, keep, drop = FALSE]
    stopifnot(!any(protected %in% dropped))   # invariant
  }
  list(dropped = dropped, pairs = pairs)
}


# fit_recipe(train, var_threshold, cor_threshold)
# Computes on train everything needed for preprocessing, in this order:
# imputation, new features, log, variance filter, correlation filter and
# standardization (z-score).
# PRECONDITION: train comes from clean_data() (it may contain NA).
# POSTCONDITION: list with everything apply_recipe() uses; all stored standard
# deviations are > 0.
fit_recipe <- function(train, var_threshold = 0.995, cor_threshold = 0.90) {
  stopifnot(is.data.frame(train), nrow(train) > 0, "label" %in% names(train))

  medians <- lapply(train[NUMERIC_COLS], median, na.rm = TRUE)
  medians <- lapply(medians, function(v) if (is.na(v)) 0 else v)   # in case a column is all NA
  tr <- impute(train, medians)
  tr <- create_features(tr)
  tr <- log_transform(tr)

  fv <- variance_filter(tr[, CANDIDATE_COLS], var_threshold)
  after_variance <- setdiff(CANDIDATE_COLS, fv$dropped)
  fc <- correlation_filter(tr[, after_variance], cor_threshold)
  numeric_vars <- setdiff(after_variance, fc$dropped)

  means <- sapply(tr[, numeric_vars], mean)
  sds   <- sapply(tr[, numeric_vars], sd)
  sds[is.na(sds) | sds == 0] <- 1

  recipe <- list(medians = medians, numeric_vars = numeric_vars,
                 dropped_variance = fv$dropped, mode_share = fv$mode_share,
                 dropped_correlation = fc$dropped, pairs = fc$pairs,
                 means = means, sds = sds)
  stopifnot(all(recipe$sds > 0), length(numeric_vars) >= 2)
  recipe
}


# apply_recipe(df, recipe)
# Applies the recipe to a set (train or test). url, dom and tld are not used as
# predictors.
# PRECONDITION: df has every column of the CSV and recipe comes from fit_recipe().
# POSTCONDITION: list with X (standardized matrix, without NA, with the same
# columns whatever df is) and y (0/1 vector).
# INVARIANT: each pass of the loop standardizes a single column, with the train
# mean and standard deviation.
apply_recipe <- function(df, recipe) {
  stopifnot(all(EXPECTED_COLS %in% names(df)))
  d <- impute(df, recipe$medians)
  d <- create_features(d)
  d <- log_transform(d)
  for (col in recipe$numeric_vars) {
    d[[col]] <- (d[[col]] - recipe$means[[col]]) / recipe$sds[[col]]
  }
  X <- as.matrix(d[, recipe$numeric_vars])
  rownames(X) <- NULL
  stopifnot(!anyNA(X), nrow(X) == nrow(df), identical(colnames(X), recipe$numeric_vars))
  list(X = X, y = df$label)
}
