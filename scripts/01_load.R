# 01_load.R
# Reading the CSV, column checks and cleaning.

# The 22 features that come with the dataset. All of them are derived from the URL text.
NUMERIC_COLS <- c("url_len", "dom_len", "is_ip", "tld_len", "subdom_cnt",
                  "letter_cnt", "digit_cnt", "special_cnt", "eq_cnt", "qm_cnt",
                  "amp_cnt", "dot_cnt", "dash_cnt", "under_cnt", "letter_ratio",
                  "digit_ratio", "spec_ratio", "is_https", "slash_cnt",
                  "entropy", "path_len", "query_len")
# url, dom and tld are not used as predictors (the authors keep them as reference).
# dom is used to group URLs in the split.
REFERENCE_COLS <- c("url", "dom", "tld")
EXPECTED_COLS <- c(NUMERIC_COLS, REFERENCE_COLS, "label")

# counts and lengths: they cannot be negative and are very skewed
COUNT_COLS <- c("url_len", "dom_len", "tld_len", "subdom_cnt", "letter_cnt",
                "digit_cnt", "special_cnt", "eq_cnt", "qm_cnt", "amp_cnt",
                "dot_cnt", "dash_cnt", "under_cnt", "slash_cnt", "path_len",
                "query_len")


# find_csv(data_dir)
# Depending on the version, the file is called Dataset.csv or URL-Phish.csv, so
# the name is not hardcoded: the only .csv in the folder is used.
# PRECONDITION: data_dir exists.
# POSTCONDITION: returns the path of a single .csv file.
find_csv <- function(data_dir) {
  stopifnot(dir.exists(data_dir))
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE,
                      ignore.case = TRUE)
  if (length(files) == 0) {
    stop("There is no .csv in ", data_dir, ". Download URL-Phish from ",
         "https://data.mendeley.com/datasets/65z9twcx3r and copy it there.")
  }
  if (length(files) > 1) {
    stop("There is more than one .csv in ", data_dir, ": keep only the URL-Phish one.")
  }
  files
}


# load_data(path)
# Reads the CSV and checks that every column is there.
# PRECONDITION: path points to an existing file.
# POSTCONDITION: data.frame with the EXPECTED_COLS columns, in that order, and
# at least one row. If a column is missing, the error lists the ones in the file.
load_data <- function(path) {
  stopifnot(is.character(path), length(path) == 1, file.exists(path))
  df <- read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                 encoding = "UTF-8")
  names(df) <- trimws(tolower(names(df)))
  missing_cols <- setdiff(EXPECTED_COLS, names(df))
  if (length(missing_cols) > 0) {
    stop("The CSV is missing columns: ", paste(missing_cols, collapse = ", "),
         "\nColumns found: ", paste(names(df), collapse = ", "))
  }
  df <- df[, EXPECTED_COLS]
  stopifnot(nrow(df) > 0, identical(names(df), EXPECTED_COLS))
  df
}


# normalize_domain(dom)
# Lowercase and without "www.", so www.example.com and example.com are the same group.
# PRECONDITION: dom is character.
# POSTCONDITION: character of the same length.
normalize_domain <- function(dom) {
  stopifnot(is.character(dom))
  d <- tolower(trimws(dom))
  sub("^www\\.", "", d)
}


# clean_data(df)
# Steps:
#  - label to integer 0/1 and the 22 features to numeric
#  - negative counts or ratios outside [0,1] become NA (they are imputed
#    later, with the training medians)
#  - if a URL appears several times with the same label, one row is kept; if
#    it appears with different labels the right one is unknown, so all are dropped
#  - a group column (normalized domain) is added for the split; if there is no
#    domain, the URL is used
# Nothing is imputed here; that is done later, on train only.
# PRECONDITION: df comes from load_data().
# POSTCONDITION: list with data (unique URLs, label 0/1 without NA, group without NA)
# and summary with the counts of each step.
# INVARIANT: final rows = initial - invalid labels - conflicts - duplicates.
clean_data <- function(df) {
  stopifnot(identical(names(df), EXPECTED_COLS))
  n_initial <- nrow(df)

  df$label <- suppressWarnings(as.integer(as.character(df$label)))
  n_invalid_label <- sum(is.na(df$label) | !(df$label %in% c(0, 1)))
  df <- df[!is.na(df$label) & df$label %in% c(0, 1), ]

  for (col in NUMERIC_COLS) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  n_impossible <- 0
  for (col in COUNT_COLS) {
    bad <- !is.na(df[[col]]) & df[[col]] < 0
    n_impossible <- n_impossible + sum(bad); df[[col]][bad] <- NA
  }
  for (col in c("letter_ratio", "digit_ratio", "spec_ratio")) {
    bad <- !is.na(df[[col]]) & (df[[col]] < 0 | df[[col]] > 1)
    n_impossible <- n_impossible + sum(bad); df[[col]][bad] <- NA
  }

  df$url <- trimws(as.character(df$url))
  # URLs with contradictory labels: all their rows are dropped
  labels_per_url <- tapply(df$label, df$url, function(v) length(unique(v)))
  conflict_urls <- names(labels_per_url)[labels_per_url > 1]
  in_conflict <- df$url %in% conflict_urls
  n_conflict <- sum(in_conflict)
  df <- df[!in_conflict, ]
  # repeated with the same label: keep the first one
  repeated <- duplicated(df$url)
  n_duplicates <- sum(repeated)
  df <- df[!repeated, ]

  df$group <- normalize_domain(ifelse(is.na(df$dom), "", as.character(df$dom)))
  df$group[df$group == ""] <- df$url[df$group == ""]
  rownames(df) <- NULL

  n_per_group <- table(df$group)
  summary_list <- list(n_initial = n_initial,
                       n_invalid_label = n_invalid_label,
                       n_conflict = n_conflict,
                       n_duplicates = n_duplicates,
                       n_impossible = n_impossible,
                       n_missing = sum(is.na(df[, NUMERIC_COLS])),
                       n_final = nrow(df),
                       n_phishing = sum(df$label == 1),
                       n_legitimate = sum(df$label == 0),
                       n_domains = length(n_per_group),
                       max_per_domain = max(n_per_group),
                       top_domain = names(n_per_group)[which.max(n_per_group)])

  stopifnot(nrow(df) == n_initial - n_invalid_label - n_conflict - n_duplicates,
            all(df$label %in% c(0, 1)), !anyDuplicated(df$url), !anyNA(df$group))
  list(data = df, summary = summary_list)
}
