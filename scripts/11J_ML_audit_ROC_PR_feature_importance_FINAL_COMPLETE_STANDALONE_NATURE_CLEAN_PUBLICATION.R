
cat("\n[11J FINAL] Starting ML audit / ROC-PR / feature importance review...\n")
cat("[11J FINAL] Mode: complete standalone 11J rebuild; no previous 11J dependency; no internet; no 00-10P rerun.\n")
cat("[11J FINAL] Inputs allowed: locked upstream 09C / 11H / 11I outputs.\n")
cat("[11J FINAL] Claim boundary: marker-rule-derived prioritization model audit only; no clinical prediction or clinical biomarker claim.\n")
cat("[11J FINAL] Figure style: Nature-style clean publication layout; no long explanatory text inside panels.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "11J_ML_audit_ROC_PR_feature_importance_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "11J_ML_audit_ROC_PR_feature_importance_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "11J_ML_audit_ROC_PR_feature_importance_FINAL_COMPLETE_STANDALONE"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

safe_chr <- function(value_obj) {
  out <- as.character(value_obj)
  out[is.na(out)] <- ""
  out
}

safe_num <- function(value_obj) {
  suppressWarnings(as.numeric(value_obj))
}

clean_space <- function(value_obj) {
  out <- safe_chr(value_obj)
  out <- gsub("^\\s+|\\s+$", "", out)
  out <- gsub("[\r\n\t]+", " ", out)
  out <- gsub("\\s+", " ", out)
  out
}

clean_gene_symbol <- function(value_obj) {
  out <- toupper(clean_space(value_obj))
  out <- gsub("[^A-Z0-9.-]", "", out)
  out[out %in% c("", "NA", "NAN", "NULL", "NONE", "GENE", "SYMBOL", "FEATURE")] <- ""
  out
}

clean_label <- function(value_obj) {
  out <- clean_space(value_obj)
  out[out %in% c("", "NA", "NaN", "NULL", "None")] <- ""
  out
}

safe_bind_rows <- function(list_value) {
  if (length(list_value) < 1) return(data.frame(stringsAsFactors = FALSE))
  keep_vec <- rep(FALSE, length(list_value))
  for (idx_value in seq_along(list_value)) {
    keep_vec[idx_value] <- is.data.frame(list_value[[idx_value]]) && nrow(list_value[[idx_value]]) > 0
  }
  list_value <- list_value[keep_vec]
  if (length(list_value) < 1) return(data.frame(stringsAsFactors = FALSE))
  all_cols <- unique(unlist(lapply(list_value, colnames), use.names = FALSE))
  fixed_list <- list()
  for (idx_value in seq_along(list_value)) {
    data_value <- list_value[[idx_value]]
    missing_cols <- setdiff(all_cols, colnames(data_value))
    if (length(missing_cols) > 0) {
      for (col_value in missing_cols) data_value[[col_value]] <- NA
    }
    fixed_list[[idx_value]] <- data_value[, all_cols, drop = FALSE]
  }
  do.call(base::rbind, fixed_list)
}

write_csv_safe <- function(data_value, file_value) {
  utils::write.csv(data_value, file_value, row.names = FALSE, na = "")
  cat("[11J FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[11J FINAL] Wrote:", file_value, "\n")
}

read_table_safe <- function(file_value) {
  if (!file.exists(file_value)) return(data.frame(stringsAsFactors = FALSE))
  ext_value <- tolower(tools::file_ext(file_value))
  out <- data.frame(stringsAsFactors = FALSE)
  tryCatch({
    if (ext_value %in% c("tsv", "txt")) {
      out <- utils::read.table(
        file_value,
        sep = "\t",
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        quote = "",
        comment.char = "",
        fill = TRUE
      )
    } else {
      out <- utils::read.csv(file_value, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(err_obj) {
    out <<- data.frame(stringsAsFactors = FALSE)
  })
  if (!is.data.frame(out)) out <- data.frame(stringsAsFactors = FALSE)
  out
}

open_pdf_safe <- function(filename, width_value = 10, height_value = 6) {
  file_primary <- file.path(out_fig_dir, filename)
  if (file.exists(file_primary)) suppressWarnings(try(file.remove(file_primary), silent = TRUE))
  ok_value <- TRUE
  tryCatch({
    grDevices::pdf(
      file_primary,
      width = width_value,
      height = height_value,
      onefile = FALSE,
      useDingbats = FALSE,
      paper = "special"
    )
  }, error = function(err_obj) {
    ok_value <<- FALSE
  })
  if (!ok_value) {
    alt_name <- paste0(sub("\\.pdf$", "", filename), "_ALT_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    file_alt <- file.path(out_fig_dir, alt_name)
    grDevices::pdf(
      file_alt,
      width = width_value,
      height = height_value,
      onefile = FALSE,
      useDingbats = FALSE,
      paper = "special"
    )
    cat("[11J FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

nature_palette <- list(
  ink = "#1D1D1F",
  muted = "#5F6368",
  grid = "#E6E8EB",
  border = "#2F3A45",
  navy = "#3B4992",
  blue = "#4DBBD5",
  teal = "#00A087",
  orange = "#E64B35",
  red = "#B2182B",
  purple = "#7E6148",
  gold = "#F39B7F",
  pale_blue = "#EAF2F8",
  pale_orange = "#FDE9DF",
  pale_green = "#E8F3EF",
  white = "#FFFFFF"
)

blend_color <- function(color_low, color_high, fraction_value) {
  fraction_value <- safe_num(fraction_value)
  fraction_value[!is.finite(fraction_value)] <- 0
  fraction_value[fraction_value < 0] <- 0
  fraction_value[fraction_value > 1] <- 1
  low_rgb <- grDevices::col2rgb(color_low) / 255
  high_rgb <- grDevices::col2rgb(color_high) / 255
  out_colors <- character(length(fraction_value))
  for (idx_color in seq_along(fraction_value)) {
    mixed_rgb <- low_rgb[, 1] * (1 - fraction_value[idx_color]) + high_rgb[, 1] * fraction_value[idx_color]
    out_colors[idx_color] <- grDevices::rgb(mixed_rgb[1], mixed_rgb[2], mixed_rgb[3])
  }
  out_colors
}

nature_continuous_color <- function(value_obj, max_obj, low_color = nature_palette$pale_blue, high_color = nature_palette$navy) {
  value_num <- safe_num(value_obj)
  max_num <- max(safe_num(max_obj), na.rm = TRUE)
  if (!is.finite(max_num) || max_num <= 0) max_num <- 1
  fraction_value <- value_num / max_num
  fraction_value[!is.finite(fraction_value)] <- 0
  fraction_value[fraction_value < 0] <- 0
  fraction_value[fraction_value > 1] <- 1
  blend_color(low_color, high_color, fraction_value)
}

feature_type_color <- function(feature_type_value) {
  feature_type_value <- safe_chr(feature_type_value)
  out_colors <- rep(nature_palette$navy, length(feature_type_value))
  out_colors[grepl("marker|signature", feature_type_value, ignore.case = TRUE)] <- nature_palette$teal
  out_colors[grepl("risk|stress|apoptosis|p53|inflammatory|off_target|cell_cycle", feature_type_value, ignore.case = TRUE)] <- nature_palette$orange
  out_colors[grepl("pd|genetic", feature_type_value, ignore.case = TRUE)] <- nature_palette$purple
  out_colors
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 0.98, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

if (!dir.exists(table_root)) stop("[11J FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
file_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(file_info$size) & file_info$size > 0 & file_info$size < 100 * 1024 * 1024]

all_table_files <- all_table_files[!grepl("11J_ML_audit_ROC_PR_feature_importance", all_table_files, ignore.case = TRUE)]

find_files_all_terms <- function(term_values, max_n = 20) {
  if (length(all_table_files) < 1) return(character(0))
  term_values <- tolower(safe_chr(term_values))
  path_lower <- tolower(all_table_files)
  keep_vec <- rep(TRUE, length(all_table_files))
  for (term_value in term_values) keep_vec <- keep_vec & grepl(term_value, path_lower, fixed = TRUE)
  hits <- all_table_files[keep_vec]
  if (length(hits) < 1) return(character(0))
  hit_info <- file.info(hits)
  hits <- hits[order(hit_info$mtime, decreasing = TRUE)]
  unique(hits)[seq_len(min(max_n, length(unique(hits))))]
}

find_files_any_terms <- function(term_values, max_n = 20) {
  if (length(all_table_files) < 1) return(character(0))
  term_values <- tolower(safe_chr(term_values))
  path_lower <- tolower(all_table_files)
  keep_vec <- rep(FALSE, length(all_table_files))
  for (term_value in term_values) keep_vec <- keep_vec | grepl(term_value, path_lower, fixed = TRUE)
  hits <- all_table_files[keep_vec]
  if (length(hits) < 1) return(character(0))
  hit_info <- file.info(hits)
  hits <- hits[order(hit_info$mtime, decreasing = TRUE)]
  unique(hits)[seq_len(min(max_n, length(unique(hits))))]
}

first_existing_file <- function(file_values) {
  file_values <- safe_chr(file_values)
  file_values <- file_values[file.exists(file_values)]
  if (length(file_values) < 1) return("")
  file_values[1]
}

detect_numeric_column <- function(data_value, column_terms, require_many = TRUE) {
  if (!is.data.frame(data_value) || nrow(data_value) < 1) return("")
  col_names <- colnames(data_value)
  col_lower <- tolower(col_names)
  for (term_value in column_terms) {
    hits <- col_names[grepl(term_value, col_lower, fixed = TRUE)]
    if (length(hits) > 0) {
      for (hit_col in hits) {
        vals <- safe_num(data_value[[hit_col]])
        if (!require_many || sum(is.finite(vals)) >= max(3, floor(0.20 * length(vals)))) return(hit_col)
      }
    }
  }
  ""
}

detect_text_column <- function(data_value, column_terms) {
  if (!is.data.frame(data_value) || nrow(data_value) < 1) return("")
  col_names <- colnames(data_value)
  col_lower <- tolower(col_names)
  for (term_value in column_terms) {
    hits <- col_names[grepl(term_value, col_lower, fixed = TRUE)]
    if (length(hits) > 0) return(hits[1])
  }
  ""
}

binary_label_from_vector <- function(label_obj) {
  if (length(label_obj) < 1) return(integer(0))
  num_values <- safe_num(label_obj)
  if (sum(is.finite(num_values)) >= max(3, floor(0.50 * length(label_obj)))) {
    unique_values <- sort(unique(num_values[is.finite(num_values)]))
    if (length(unique_values) <= 2) {
      return(as.integer(num_values == max(unique_values)))
    }
  }

  label_text <- tolower(clean_space(label_obj))
  positive_terms <- c("1", "true", "yes", "positive", "high", "priority", "favorable", "ideal", "good", "case", "da", "target")
  negative_terms <- c("0", "false", "no", "negative", "low", "risk", "non", "bad", "control", "background")
  out <- rep(NA_integer_, length(label_text))
  for (idx_label in seq_along(label_text)) {
    text_now <- label_text[idx_label]
    if (text_now == "") next
    if (any(vapply(positive_terms, function(term_value) grepl(term_value, text_now, fixed = TRUE), logical(1)))) out[idx_label] <- 1L
    if (any(vapply(negative_terms, function(term_value) grepl(term_value, text_now, fixed = TRUE), logical(1)))) {
      if (!grepl("priority|favorable|ideal", text_now)) out[idx_label] <- 0L
    }
  }
  out
}

compute_roc_pr <- function(score_values, label_values) {
  score_values <- safe_num(score_values)
  label_values <- as.integer(label_values)
  ok_vec <- is.finite(score_values) & !is.na(label_values)
  score_values <- score_values[ok_vec]
  label_values <- label_values[ok_vec]
  if (length(score_values) < 5 || length(unique(label_values)) < 2) {
    return(list(
      roc = data.frame(stringsAsFactors = FALSE),
      pr = data.frame(stringsAsFactors = FALSE),
      metrics = data.frame(
        n = length(score_values),
        positives = sum(label_values == 1, na.rm = TRUE),
        negatives = sum(label_values == 0, na.rm = TRUE),
        AUROC = NA_real_,
        AUPRC = NA_real_,
        baseline_positive_rate = ifelse(length(label_values) > 0, mean(label_values == 1, na.rm = TRUE), NA_real_),
        stringsAsFactors = FALSE
      )
    ))
  }

  ord <- order(score_values, decreasing = TRUE)
  score_sorted <- score_values[ord]
  label_sorted <- label_values[ord]
  pos_total <- sum(label_sorted == 1)
  neg_total <- sum(label_sorted == 0)

  thresholds <- unique(score_sorted)
  roc_list <- list()
  pr_list <- list()

  roc_list[[1]] <- data.frame(threshold = Inf, FPR = 0, TPR = 0, stringsAsFactors = FALSE)
  pr_list[[1]] <- data.frame(threshold = Inf, recall = 0, precision = 1, stringsAsFactors = FALSE)

  for (idx_thr in seq_along(thresholds)) {
    thr_now <- thresholds[idx_thr]
    pred_pos <- score_sorted >= thr_now
    tp <- sum(pred_pos & label_sorted == 1)
    fp <- sum(pred_pos & label_sorted == 0)
    fn <- sum(!pred_pos & label_sorted == 1)
    tpr <- ifelse(pos_total > 0, tp / pos_total, NA_real_)
    fpr <- ifelse(neg_total > 0, fp / neg_total, NA_real_)
    precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 1)
    recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    roc_list[[length(roc_list) + 1]] <- data.frame(threshold = thr_now, FPR = fpr, TPR = tpr, stringsAsFactors = FALSE)
    pr_list[[length(pr_list) + 1]] <- data.frame(threshold = thr_now, recall = recall, precision = precision, stringsAsFactors = FALSE)
  }

  roc_df <- safe_bind_rows(roc_list)
  roc_df <- roc_df[order(roc_df$FPR, roc_df$TPR), , drop = FALSE]
  if (tail(roc_df$FPR, 1) < 1 || tail(roc_df$TPR, 1) < 1) {
    roc_df <- safe_bind_rows(list(roc_df, data.frame(threshold = -Inf, FPR = 1, TPR = 1, stringsAsFactors = FALSE)))
  }

  pr_df <- safe_bind_rows(pr_list)
  pr_df <- pr_df[order(pr_df$recall, decreasing = FALSE), , drop = FALSE]
  if (tail(pr_df$recall, 1) < 1) {
    last_precision <- tail(pr_df$precision, 1)
    pr_df <- safe_bind_rows(list(pr_df, data.frame(threshold = -Inf, recall = 1, precision = last_precision, stringsAsFactors = FALSE)))
  }

  trapz_auc <- function(x_values, y_values) {
    x_values <- safe_num(x_values)
    y_values <- safe_num(y_values)
    ok_inner <- is.finite(x_values) & is.finite(y_values)
    x_values <- x_values[ok_inner]
    y_values <- y_values[ok_inner]
    if (length(x_values) < 2) return(NA_real_)
    ord_inner <- order(x_values)
    x_values <- x_values[ord_inner]
    y_values <- y_values[ord_inner]
    sum(diff(x_values) * (head(y_values, -1) + tail(y_values, -1)) / 2)
  }

  auroc <- trapz_auc(roc_df$FPR, roc_df$TPR)
  auprc <- trapz_auc(pr_df$recall, pr_df$precision)

  metrics_df <- data.frame(
    n = length(score_sorted),
    positives = pos_total,
    negatives = neg_total,
    AUROC = auroc,
    AUPRC = auprc,
    baseline_positive_rate = mean(label_sorted == 1),
    stringsAsFactors = FALSE
  )

  list(roc = roc_df, pr = pr_df, metrics = metrics_df)
}

cat("[11J FINAL] Discovering locked upstream ML/evidence tables...\n")

file_09c_candidates <- unique(c(
  find_files_all_terms(c("09c", "prediction"), max_n = 30),
  find_files_all_terms(c("09c", "prob"), max_n = 30),
  find_files_all_terms(c("09c", "auc"), max_n = 30),
  find_files_all_terms(c("09c", "roc"), max_n = 30),
  find_files_all_terms(c("09c", "feature"), max_n = 30),
  find_files_all_terms(c("09c", "importance"), max_n = 30),
  find_files_all_terms(c("09c", "priority"), max_n = 30)
))
file_09c_candidates <- file_09c_candidates[file.exists(file_09c_candidates)]

file_11h_marker <- first_existing_file(c(
  file.path(table_root, "11H_integrated_evidence_tier_and_candidate_marker_signature_FINAL_COMPLETE_STANDALONE", "11H_FINAL_candidate_transcriptomic_marker_signature_table.csv"),
  find_files_all_terms(c("11h", "candidate_transcriptomic_marker_signature_table"), max_n = 10)
))

file_11h_umbrella <- first_existing_file(c(
  file.path(table_root, "11H_integrated_evidence_tier_and_candidate_marker_signature_FINAL_COMPLETE_STANDALONE", "11H_FINAL_integrated_umbrella_evidence_tier_table.csv"),
  find_files_all_terms(c("11h", "integrated_umbrella_evidence_tier_table"), max_n = 10)
))

file_11i_pair <- first_existing_file(c(
  file.path(table_root, "11I_module_score_correlation_FINAL_COMPLETE_STANDALONE", "11I_FINAL_pairwise_module_correlation_table.csv"),
  find_files_all_terms(c("11i", "pairwise_module_correlation_table"), max_n = 10)
))

input_audit_df <- data.frame(
  input_layer = c("09C_ML_candidate_tables", "11H_candidate_marker_signature", "11H_umbrella_evidence_tier", "11I_pairwise_module_correlation"),
  files_detected = c(length(file_09c_candidates), ifelse(file_11h_marker != "", 1, 0), ifelse(file_11h_umbrella != "", 1, 0), ifelse(file_11i_pair != "", 1, 0)),
  representative_file = c(
    ifelse(length(file_09c_candidates) > 0, file_09c_candidates[1], ""),
    file_11h_marker,
    file_11h_umbrella,
    file_11i_pair
  ),
  allowed_as_input = c(TRUE, TRUE, TRUE, TRUE),
  note = c(
    "locked upstream 09C ML outputs; no previous 11J tables",
    "locked upstream 11H marker table",
    "locked upstream 11H umbrella evidence table",
    "locked upstream 11I correlation table"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "11J_FINAL_locked_upstream_input_audit.csv"))

prediction_task_list <- list()
roc_list_all <- list()
pr_list_all <- list()
metrics_list <- list()

if (length(file_09c_candidates) > 0) {
  for (idx_file in seq_along(file_09c_candidates)) {
    file_value <- file_09c_candidates[idx_file]
    data_value <- read_table_safe(file_value)
    if (!is.data.frame(data_value) || nrow(data_value) < 5) next

    score_col <- detect_numeric_column(
      data_value,
      c("predicted_probability", "prediction_probability", "probability", "prob", "score", "priority", "ml_score", "prediction"),
      require_many = TRUE
    )
    label_col <- detect_text_column(
      data_value,
      c("true_label", "label", "class", "target", "group", "ground_truth", "weak_label", "response")
    )

    has_feature_col <- detect_text_column(data_value, c("feature", "gene", "module"))
    has_importance_col <- detect_numeric_column(data_value, c("importance", "gain", "gini", "coef", "weight"), require_many = TRUE)
    if (score_col != "" && label_col != "" && !(has_feature_col != "" && has_importance_col != "" && nrow(data_value) < 200 && !grepl("prediction|prob|roc|auc", basename(file_value), ignore.case = TRUE))) {
      labels_bin <- binary_label_from_vector(data_value[[label_col]])
      scores_num <- safe_num(data_value[[score_col]])
      ok_vec <- is.finite(scores_num) & !is.na(labels_bin)
      if (sum(ok_vec) >= 8 && length(unique(labels_bin[ok_vec])) == 2) {
        task_name <- paste0("task_", length(metrics_list) + 1)
        rocpr <- compute_roc_pr(scores_num[ok_vec], labels_bin[ok_vec])
        if (nrow(rocpr$metrics) > 0) {
          roc_df <- rocpr$roc
          pr_df <- rocpr$pr
          metrics_df <- rocpr$metrics
          roc_df$task_id <- task_name
          pr_df$task_id <- task_name
          metrics_df$task_id <- task_name
          metrics_df$source_file <- file_value
          metrics_df$score_column <- score_col
          metrics_df$label_column <- label_col

          roc_list_all[[length(roc_list_all) + 1]] <- roc_df
          pr_list_all[[length(pr_list_all) + 1]] <- pr_df
          metrics_list[[length(metrics_list) + 1]] <- metrics_df
        }
      }
    }
  }
}

roc_all_df <- safe_bind_rows(roc_list_all)
pr_all_df <- safe_bind_rows(pr_list_all)
metrics_all_df <- safe_bind_rows(metrics_list)

if (nrow(metrics_all_df) < 1) {
  metrics_all_df <- data.frame(
    task_id = "no_valid_prediction_score_label_table_detected",
    n = 0,
    positives = 0,
    negatives = 0,
    AUROC = NA_real_,
    AUPRC = NA_real_,
    baseline_positive_rate = NA_real_,
    source_file = "",
    score_column = "",
    label_column = "",
    stringsAsFactors = FALSE
  )
}

write_csv_safe(metrics_all_df, file.path(out_table_dir, "11J_FINAL_ROC_PR_metric_summary.csv"))
if (nrow(roc_all_df) > 0) write_csv_safe(roc_all_df, file.path(out_table_dir, "11J_FINAL_ROC_curve_points.csv"))
if (nrow(pr_all_df) > 0) write_csv_safe(pr_all_df, file.path(out_table_dir, "11J_FINAL_PR_curve_points.csv"))

feature_list <- list()
for (idx_file in seq_along(file_09c_candidates)) {
  file_value <- file_09c_candidates[idx_file]
  data_value <- read_table_safe(file_value)
  if (!is.data.frame(data_value) || nrow(data_value) < 1) next

  feature_col <- detect_text_column(data_value, c("feature", "gene_symbol", "gene", "module", "variable", "marker"))
  importance_col <- detect_numeric_column(data_value, c("importance", "gain", "gini", "coefficient", "coef", "weight", "rank_score", "score"), require_many = FALSE)

  if (feature_col != "" && importance_col != "") {
    feature_values <- clean_label(data_value[[feature_col]])
    importance_values <- safe_num(data_value[[importance_col]])
    ok_vec <- feature_values != "" & is.finite(importance_values)
    if (sum(ok_vec) > 0) {
      tmp_feature <- data.frame(
        feature_name = feature_values[ok_vec],
        feature_gene_symbol = clean_gene_symbol(feature_values[ok_vec]),
        raw_importance = importance_values[ok_vec],
        source_file = file_value,
        feature_column = feature_col,
        importance_column = importance_col,
        stringsAsFactors = FALSE
      )
      feature_list[[length(feature_list) + 1]] <- tmp_feature
    }
  }
}

feature_raw_df <- safe_bind_rows(feature_list)
if (nrow(feature_raw_df) > 0) {

  feature_raw_df$abs_importance <- abs(safe_num(feature_raw_df$raw_importance))
  feature_names <- sort(unique(feature_raw_df$feature_name))
  feature_summary_list <- list()
  for (idx_feature in seq_along(feature_names)) {
    feature_value <- feature_names[idx_feature]
    sub_feature <- feature_raw_df[feature_raw_df$feature_name == feature_value, , drop = FALSE]
    best_idx <- which.max(sub_feature$abs_importance)
    gene_value <- clean_gene_symbol(sub_feature$feature_gene_symbol[best_idx])
    feature_summary_list[[length(feature_summary_list) + 1]] <- data.frame(
      feature_name = feature_value,
      feature_gene_symbol = gene_value,
      max_abs_importance = max(sub_feature$abs_importance, na.rm = TRUE),
      signed_importance_at_max_abs = sub_feature$raw_importance[best_idx],
      n_source_files = length(unique(sub_feature$source_file)),
      source_files = paste(unique(sub_feature$source_file), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }
  feature_summary_df <- safe_bind_rows(feature_summary_list)
  feature_summary_df <- feature_summary_df[order(feature_summary_df$max_abs_importance, decreasing = TRUE), , drop = FALSE]
} else {

  feature_summary_df <- data.frame(
    feature_name = c("DA_core", "A9_like", "projection_competence", "neuronal_maturation", "stress_p53_apoptosis", "cell_cycle_proliferation_risk"),
    feature_gene_symbol = "",
    max_abs_importance = NA_real_,
    signed_importance_at_max_abs = NA_real_,
    n_source_files = 0,
    source_files = "",
    stringsAsFactors = FALSE
  )
}

write_csv_safe(feature_raw_df, file.path(out_table_dir, "11J_FINAL_raw_detected_feature_importance_rows.csv"))
write_csv_safe(feature_summary_df, file.path(out_table_dir, "11J_FINAL_feature_importance_summary.csv"))
write_tsv_safe(feature_summary_df, file.path(out_table_dir, "11J_FINAL_feature_importance_summary.tsv"))

marker_df <- read_table_safe(file_11h_marker)
umbrella_df <- read_table_safe(file_11h_umbrella)
pair11i_df <- read_table_safe(file_11i_pair)

marker_genes <- character(0)
if (nrow(marker_df) > 0) {
  gene_col <- detect_text_column(marker_df, c("gene_symbol", "gene", "symbol"))
  if (gene_col != "") marker_genes <- unique(clean_gene_symbol(marker_df[[gene_col]]))
}
marker_genes <- marker_genes[marker_genes != ""]

risk_marker_genes <- character(0)
if (nrow(marker_df) > 0) {
  gene_col <- detect_text_column(marker_df, c("gene_symbol", "gene", "symbol"))
  tier_col <- detect_text_column(marker_df, c("tier", "direction", "summary", "context"))
  if (gene_col != "" && tier_col != "") {
    tmp_genes <- clean_gene_symbol(marker_df[[gene_col]])
    tmp_text <- tolower(safe_chr(marker_df[[tier_col]]))
    risk_marker_genes <- unique(tmp_genes[grepl("risk|stress|apoptosis|p53", tmp_text)])
  }
}
risk_marker_genes <- risk_marker_genes[risk_marker_genes != ""]

feature_summary_df$is_11H_candidate_marker_signature <- feature_summary_df$feature_gene_symbol %in% marker_genes
feature_summary_df$is_11H_risk_context_marker <- feature_summary_df$feature_gene_symbol %in% risk_marker_genes
feature_summary_df$feature_evidence_type <- "ML_feature_or_detected_predictor"
feature_summary_df$feature_evidence_type[feature_summary_df$is_11H_candidate_marker_signature] <- "ML_feature_overlaps_11H_candidate_marker"
feature_summary_df$feature_evidence_type[feature_summary_df$is_11H_risk_context_marker] <- "ML_feature_overlaps_11H_risk_marker"

feature_summary_df$feature_family <- "other_or_unclassified"
feature_summary_df$feature_family[grepl("DA_core|A9|A10|projection|axon|synaptic|maturation", feature_summary_df$feature_name, ignore.case = TRUE)] <- "identity_projection_maturation"
feature_summary_df$feature_family[grepl("risk|stress|p53|apoptosis|inflammatory|off_target|cell_cycle", feature_summary_df$feature_name, ignore.case = TRUE)] <- "risk_safety_context"
feature_summary_df$feature_family[feature_summary_df$is_11H_candidate_marker_signature] <- "candidate_marker_signature"
feature_summary_df$feature_family[feature_summary_df$is_11H_risk_context_marker] <- "risk_candidate_marker_signature"

real_feature_importance_available <- nrow(feature_raw_df) > 0 && any(is.finite(safe_num(feature_summary_df$max_abs_importance)))
feature_summary_df$feature_importance_mode <- ifelse(
  real_feature_importance_available,
  "detected_numeric_feature_importance",
  "module_level_transparency_fallback_no_numeric_feature_importance"
)

write_csv_safe(feature_summary_df, file.path(out_table_dir, "11J_FINAL_feature_importance_with_11H_marker_overlap.csv"))

calibration_list <- list()
if (length(file_09c_candidates) > 0) {
  for (idx_metric in seq_len(nrow(metrics_all_df))) {
    task_id_value <- metrics_all_df$task_id[idx_metric]
    source_file_value <- metrics_all_df$source_file[idx_metric]
    score_col_value <- metrics_all_df$score_column[idx_metric]
    label_col_value <- metrics_all_df$label_column[idx_metric]
    if (source_file_value == "" || !file.exists(source_file_value) || score_col_value == "" || label_col_value == "") next
    data_value <- read_table_safe(source_file_value)
    if (!is.data.frame(data_value) || nrow(data_value) < 5) next
    score_values <- safe_num(data_value[[score_col_value]])
    label_values <- binary_label_from_vector(data_value[[label_col_value]])
    ok_vec <- is.finite(score_values) & !is.na(label_values)
    if (sum(ok_vec) < 8 || length(unique(label_values[ok_vec])) < 2) next
    score_values <- score_values[ok_vec]
    label_values <- label_values[ok_vec]

    min_score <- min(score_values, na.rm = TRUE)
    max_score <- max(score_values, na.rm = TRUE)
    if (is.finite(min_score) && is.finite(max_score) && (min_score < 0 || max_score > 1) && abs(max_score - min_score) > 1e-12) {
      score_values <- (score_values - min_score) / (max_score - min_score)
    }
    bins <- cut(score_values, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE, labels = FALSE)
    for (bin_value in sort(unique(bins))) {
      idx_bin <- which(bins == bin_value)
      if (length(idx_bin) < 1) next
      calibration_list[[length(calibration_list) + 1]] <- data.frame(
        task_id = task_id_value,
        bin = bin_value,
        mean_predicted_score = mean(score_values[idx_bin], na.rm = TRUE),
        observed_positive_rate = mean(label_values[idx_bin] == 1, na.rm = TRUE),
        n = length(idx_bin),
        stringsAsFactors = FALSE
      )
    }
  }
}
calibration_df <- safe_bind_rows(calibration_list)
write_csv_safe(calibration_df, file.path(out_table_dir, "11J_FINAL_calibration_bin_table.csv"))

n_valid_metric_tasks <- sum(is.finite(safe_num(metrics_all_df$AUROC)))
n_feature_rows <- nrow(feature_summary_df)
n_marker_overlap <- sum(feature_summary_df$is_11H_candidate_marker_signature, na.rm = TRUE)
n_risk_marker_overlap <- sum(feature_summary_df$is_11H_risk_context_marker, na.rm = TRUE)
median_auroc <- ifelse(n_valid_metric_tasks > 0, median(safe_num(metrics_all_df$AUROC), na.rm = TRUE), NA_real_)
median_auprc <- ifelse(n_valid_metric_tasks > 0, median(safe_num(metrics_all_df$AUPRC), na.rm = TRUE), NA_real_)

audit_summary_df <- data.frame(
  item = c(
    "09C_candidate_files_detected",
    "valid_ROC_PR_tasks_detected",
    "median_AUROC",
    "median_AUPRC",
    "feature_importance_rows_detected_or_fallback",
    "real_numeric_feature_importance_available",
    "features_overlapping_11H_candidate_marker_signatures",
    "features_overlapping_11H_risk_context_markers",
    "11H_marker_table_detected",
    "11H_umbrella_table_detected",
    "11I_pairwise_correlation_table_detected",
    "claim_boundary"
  ),
  value = c(
    as.character(length(file_09c_candidates)),
    as.character(n_valid_metric_tasks),
    as.character(round(median_auroc, 6)),
    as.character(round(median_auprc, 6)),
    as.character(n_feature_rows),
    as.character(real_feature_importance_available),
    as.character(n_marker_overlap),
    as.character(n_risk_marker_overlap),
    as.character(file_11h_marker != ""),
    as.character(file_11h_umbrella != ""),
    as.character(file_11i_pair != ""),
    "marker-rule-derived prioritization model audit only; not clinical prediction or biomarker validation"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(audit_summary_df, file.path(out_table_dir, "11J_FINAL_execution_summary.csv"))
write_tsv_safe(audit_summary_df, file.path(out_table_dir, "11J_FINAL_execution_summary.tsv"))

fig_a <- open_pdf_safe("11J_FINAL_FigA_ML_audit_input_and_claim_boundary.pdf", 10.8, 5.8)
new_canvas()
draw_title("11J ML audit input summary", "Standalone rebuild from locked upstream inputs; no previous 11J output dependency.")

audit_plot <- data.frame(
  label = c(
    "09C candidate files",
    "Valid ROC/PR tasks",
    "Feature audit rows",
    "Numeric feature importance",
    "11H marker overlap",
    "11H risk-marker overlap"
  ),
  value = c(
    length(file_09c_candidates),
    n_valid_metric_tasks,
    n_feature_rows,
    ifelse(real_feature_importance_available, 1, 0),
    n_marker_overlap,
    n_risk_marker_overlap
  ),
  display_value = c(
    as.character(length(file_09c_candidates)),
    as.character(n_valid_metric_tasks),
    as.character(n_feature_rows),
    ifelse(real_feature_importance_available, "yes", "no"),
    as.character(n_marker_overlap),
    as.character(n_risk_marker_overlap)
  ),
  color_family = c("input", "performance", "feature", "feature", "marker", "risk"),
  stringsAsFactors = FALSE
)
max_audit <- max(safe_num(audit_plot$value), na.rm = TRUE)
if (!is.finite(max_audit) || max_audit <= 0) max_audit <- 1
bar_x0 <- 0.35
bar_x1 <- 0.78
y_values <- seq(0.78, 0.30, length.out = nrow(audit_plot))
for (idx_row in seq_len(nrow(audit_plot))) {
  yy <- y_values[idx_row]
  audit_value <- safe_num(audit_plot$value[idx_row])
  width_value <- audit_value / max_audit
  if (audit_plot$label[idx_row] == "Numeric feature importance" && audit_value <= 0) width_value <- 0.035
  color_high <- nature_palette$navy
  if (audit_plot$color_family[idx_row] == "performance") color_high <- nature_palette$blue
  if (audit_plot$color_family[idx_row] == "feature") color_high <- nature_palette$teal
  if (audit_plot$color_family[idx_row] == "marker") color_high <- nature_palette$purple
  if (audit_plot$color_family[idx_row] == "risk") color_high <- nature_palette$orange
  if (audit_plot$label[idx_row] == "Numeric feature importance" && audit_value <= 0) color_high <- nature_palette$muted
  text(bar_x0 - 0.018, yy, audit_plot$label[idx_row], cex = 0.60, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.026, bar_x0 + width_value * (bar_x1 - bar_x0), yy + 0.026,
       col = nature_continuous_color(max(audit_value, 0.2), max_audit, nature_palette$pale_blue, color_high),
       border = nature_palette$border, lwd = 0.45)
  text(bar_x0 + width_value * (bar_x1 - bar_x0) + 0.012, yy, audit_plot$display_value[idx_row], cex = 0.54, adj = c(0, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[11J FINAL] Wrote figure:", fig_a, "
")

fig_b <- open_pdf_safe("11J_FINAL_FigB_ROC_PR_performance_audit.pdf", 13.2, 7.0)
new_canvas()
draw_title("ROC/PR performance audit", "Marker-rule-derived prioritisation performance; not a clinical-use model.")
if (n_valid_metric_tasks > 0 && nrow(roc_all_df) > 0 && nrow(pr_all_df) > 0) {
  task_values <- unique(roc_all_df$task_id)
  task_cols <- c(nature_palette$navy, nature_palette$teal, nature_palette$orange, nature_palette$purple, nature_palette$blue, nature_palette$red)
  metrics_use <- metrics_all_df[match(task_values, metrics_all_df$task_id), , drop = FALSE]
  median_baseline <- median(safe_num(metrics_use$baseline_positive_rate), na.rm = TRUE)
  if (!is.finite(median_baseline)) median_baseline <- 0.5

  map_unit_x <- function(values_vec, x0, x1) x0 + safe_num(values_vec) * (x1 - x0)
  map_unit_y <- function(values_vec, y0, y1) y0 + safe_num(values_vec) * (y1 - y0)

  panel_y0 <- 0.19
  panel_y1 <- 0.77
  roc_x0 <- 0.08
  roc_x1 <- 0.39
  pr_x0 <- 0.48
  pr_x1 <- 0.79

  rect(roc_x0, panel_y0, roc_x1, panel_y1, border = nature_palette$border, col = NA, lwd = 0.65)
  for (tick_value in seq(0, 1, by = 0.25)) {
    xx <- map_unit_x(tick_value, roc_x0, roc_x1)
    yy <- map_unit_y(tick_value, panel_y0, panel_y1)
    segments(xx, panel_y0, xx, panel_y1, col = nature_palette$grid, lwd = 0.5)
    segments(roc_x0, yy, roc_x1, yy, col = nature_palette$grid, lwd = 0.5)
    text(xx, panel_y0 - 0.030, sprintf("%.2f", tick_value), cex = 0.38, adj = c(0.5, 1), col = nature_palette$muted)
    text(roc_x0 - 0.018, yy, sprintf("%.2f", tick_value), cex = 0.38, adj = c(1, 0.5), col = nature_palette$muted)
  }
  segments(roc_x0, panel_y0, roc_x1, panel_y1, col = nature_palette$muted, lwd = 0.8, lty = 2)
  for (idx_task in seq_along(task_values)) {
    task_now <- task_values[idx_task]
    sub_roc <- roc_all_df[roc_all_df$task_id == task_now, , drop = FALSE]
    sub_roc <- sub_roc[order(sub_roc$FPR, sub_roc$TPR), , drop = FALSE]
    if (nrow(sub_roc) > 1) {
      line_x <- map_unit_x(sub_roc$FPR, roc_x0, roc_x1)
      line_y <- map_unit_y(sub_roc$TPR, panel_y0, panel_y1)
      lines(line_x, line_y, col = task_cols[(idx_task - 1) %% length(task_cols) + 1], lwd = 1.55)
    }
  }
  text((roc_x0 + roc_x1) / 2, 0.115, "False positive rate", cex = 0.50, col = nature_palette$ink)
  text(0.030, (panel_y0 + panel_y1) / 2, "True positive rate", cex = 0.50, srt = 90, col = nature_palette$ink)
  text((roc_x0 + roc_x1) / 2, 0.825, paste0("ROC; median AUROC = ", sprintf("%.3f", median_auroc)), cex = 0.58, font = 2, col = nature_palette$ink)

  rect(pr_x0, panel_y0, pr_x1, panel_y1, border = nature_palette$border, col = NA, lwd = 0.65)
  for (tick_value in seq(0, 1, by = 0.25)) {
    xx <- map_unit_x(tick_value, pr_x0, pr_x1)
    yy <- map_unit_y(tick_value, panel_y0, panel_y1)
    segments(xx, panel_y0, xx, panel_y1, col = nature_palette$grid, lwd = 0.5)
    segments(pr_x0, yy, pr_x1, yy, col = nature_palette$grid, lwd = 0.5)
    text(xx, panel_y0 - 0.030, sprintf("%.2f", tick_value), cex = 0.38, adj = c(0.5, 1), col = nature_palette$muted)
    text(pr_x0 - 0.018, yy, sprintf("%.2f", tick_value), cex = 0.38, adj = c(1, 0.5), col = nature_palette$muted)
  }
  baseline_y <- map_unit_y(median_baseline, panel_y0, panel_y1)
  segments(pr_x0, baseline_y, pr_x1, baseline_y, col = nature_palette$muted, lwd = 0.8, lty = 2)
  text(pr_x1 + 0.008, baseline_y, paste0("baseline ", sprintf("%.2f", median_baseline)), cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)

  for (idx_task in seq_along(task_values)) {
    task_now <- task_values[idx_task]
    sub_pr <- pr_all_df[pr_all_df$task_id == task_now, , drop = FALSE]
    sub_pr <- sub_pr[order(sub_pr$recall, sub_pr$precision), , drop = FALSE]
    if (nrow(sub_pr) > 1) {
      line_x <- map_unit_x(sub_pr$recall, pr_x0, pr_x1)
      line_y <- map_unit_y(sub_pr$precision, panel_y0, panel_y1)
      lines(line_x, line_y, type = "s", col = task_cols[(idx_task - 1) %% length(task_cols) + 1], lwd = 1.55)
    }
  }
  text((pr_x0 + pr_x1) / 2, 0.115, "Recall", cex = 0.50, col = nature_palette$ink)
  text(0.432, (panel_y0 + panel_y1) / 2, "Precision", cex = 0.50, srt = 90, col = nature_palette$ink)
  text((pr_x0 + pr_x1) / 2, 0.825, paste0("PR; median AUPRC = ", sprintf("%.3f", median_auprc)), cex = 0.58, font = 2, col = nature_palette$ink)

  legend_x <- 0.825
  legend_y <- 0.75
  text(legend_x, legend_y + 0.048, "Detected tasks", cex = 0.48, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  for (idx_task in seq_along(task_values)) {
    yy <- legend_y - (idx_task - 1) * 0.052
    col_now <- task_cols[(idx_task - 1) %% length(task_cols) + 1]
    segments(legend_x, yy, legend_x + 0.028, yy, col = col_now, lwd = 2.0)
    auroc_now <- safe_num(metrics_use$AUROC[idx_task])
    auprc_now <- safe_num(metrics_use$AUPRC[idx_task])
    label_now <- paste0("T", idx_task, "  AUROC ", sprintf("%.2f", auroc_now), "  AUPRC ", sprintf("%.2f", auprc_now))
    text(legend_x + 0.036, yy, label_now, cex = 0.34, adj = c(0, 0.5), col = nature_palette$ink)
  }
  segments(legend_x, 0.34, legend_x + 0.028, 0.34, col = nature_palette$muted, lwd = 1.2, lty = 2)
  text(legend_x + 0.036, 0.34, "PR baseline", cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)
} else {
  text(0.5, 0.56, "No valid score-label prediction table was detected.", cex = 0.76, font = 2, col = nature_palette$ink)
  text(0.5, 0.48, "11J reports feature/evidence audit only.", cex = 0.56, col = nature_palette$muted)
}
dev.off()
cat("[11J FINAL] Wrote figure:", fig_b, "
")

fig_c <- open_pdf_safe("11J_FINAL_FigC_feature_importance_marker_overlap.pdf", 10.8, 6.4)
new_canvas()
draw_title("Feature-transparency and candidate-marker overlap audit", "Detected feature audit rows against locked 11H marker signatures.")

plot_features <- feature_summary_df
plot_features <- plot_features[seq_len(min(18, nrow(plot_features))), , drop = FALSE]
if (nrow(plot_features) > 0) {
  y_values <- seq(0.78, 0.24, length.out = nrow(plot_features))
  bar_x0 <- 0.35
  bar_x1 <- 0.76

  if (real_feature_importance_available) {
    display_scores <- safe_num(plot_features$max_abs_importance)
    display_scores[!is.finite(display_scores)] <- 0
    max_display <- max(display_scores, na.rm = TRUE)
    if (!is.finite(max_display) || max_display <= 0) max_display <- 1
  } else {
    display_scores <- rep(1, nrow(plot_features))
    max_display <- 1
  }

  for (idx_row in seq_len(nrow(plot_features))) {
    yy <- y_values[idx_row]
    width_value <- max(0.05, display_scores[idx_row] / max_display)
    color_now <- feature_type_color(plot_features$feature_evidence_type[idx_row])
    if (!real_feature_importance_available) {
      if (plot_features$feature_family[idx_row] == "risk_safety_context") color_now <- nature_palette$orange
      if (plot_features$feature_family[idx_row] == "identity_projection_maturation") color_now <- nature_palette$teal
      if (plot_features$feature_family[idx_row] == "other_or_unclassified") color_now <- nature_palette$muted
    }
    text(bar_x0 - 0.018, yy, plot_features$feature_name[idx_row], cex = 0.48, adj = c(1, 0.5), col = nature_palette$ink)
    rect(bar_x0, yy - 0.022, bar_x0 + width_value * (bar_x1 - bar_x0), yy + 0.022,
         col = color_now, border = nature_palette$border, lwd = 0.35)
    label_now <- ""
    if (plot_features$is_11H_candidate_marker_signature[idx_row]) label_now <- "11H marker"
    if (plot_features$is_11H_risk_context_marker[idx_row]) label_now <- "risk marker"
    if (!real_feature_importance_available) label_now <- "module audit"
    text(bar_x1 + 0.014, yy, label_now, cex = 0.40, adj = c(0, 0.5), col = nature_palette$muted)
  }
} else {
  text(0.5, 0.5, "No feature rows detected.", cex = 0.75, col = nature_palette$ink)
}
dev.off()
cat("[11J FINAL] Wrote figure:", fig_c, "
")

fig_d <- open_pdf_safe("11J_FINAL_FigD_integrated_ML_audit_summary.pdf", 10.8, 6.2)
new_canvas()
draw_title("Integrated ML audit summary", "Performance transparency and feature-availability summary.")

summary_plot <- data.frame(
  item = c(
    "Valid ROC/PR tasks",
    "Median AUROC",
    "Median AUPRC",
    "Feature audit rows",
    "Numeric feature importance",
    "11H marker-overlap features",
    "Risk-marker-overlap features",
    "Strong 11I correlation pairs"
  ),
  value = c(
    n_valid_metric_tasks,
    ifelse(is.finite(median_auroc), round(100 * median_auroc, 1), 0),
    ifelse(is.finite(median_auprc), round(100 * median_auprc, 1), 0),
    n_feature_rows,
    ifelse(real_feature_importance_available, 1, 0),
    n_marker_overlap,
    n_risk_marker_overlap,
    ifelse(nrow(pair11i_df) > 0 && "abs_rho" %in% colnames(pair11i_df), sum(safe_num(pair11i_df$abs_rho) >= 0.5, na.rm = TRUE), NA_real_)
  ),
  display_value = c(
    as.character(n_valid_metric_tasks),
    ifelse(is.finite(median_auroc), sprintf("%.3f", median_auroc), "NA"),
    ifelse(is.finite(median_auprc), sprintf("%.3f", median_auprc), "NA"),
    as.character(n_feature_rows),
    ifelse(real_feature_importance_available, "yes", "no"),
    as.character(n_marker_overlap),
    as.character(n_risk_marker_overlap),
    ifelse(nrow(pair11i_df) > 0 && "abs_rho" %in% colnames(pair11i_df), as.character(sum(safe_num(pair11i_df$abs_rho) >= 0.5, na.rm = TRUE)), "0")
  ),
  family = c("performance", "performance", "performance", "feature", "feature", "marker", "risk", "correlation"),
  stringsAsFactors = FALSE
)
summary_plot$value[!is.finite(summary_plot$value)] <- 0
max_summary <- max(summary_plot$value, na.rm = TRUE)
if (!is.finite(max_summary) || max_summary <= 0) max_summary <- 1
bar_x0 <- 0.39
bar_x1 <- 0.78
y_values <- seq(0.80, 0.20, length.out = nrow(summary_plot))
for (idx_row in seq_len(nrow(summary_plot))) {
  yy <- y_values[idx_row]
  summary_value <- safe_num(summary_plot$value[idx_row])
  width_value <- summary_value / max_summary
  if (summary_plot$item[idx_row] == "Numeric feature importance" && summary_value <= 0) width_value <- 0.035
  color_high <- nature_palette$navy
  if (summary_plot$family[idx_row] == "performance") color_high <- nature_palette$blue
  if (summary_plot$family[idx_row] == "feature") color_high <- nature_palette$teal
  if (summary_plot$family[idx_row] == "marker") color_high <- nature_palette$purple
  if (summary_plot$family[idx_row] == "risk") color_high <- nature_palette$orange
  if (summary_plot$family[idx_row] == "correlation") color_high <- nature_palette$red
  if (summary_plot$item[idx_row] == "Numeric feature importance" && summary_value <= 0) color_high <- nature_palette$muted
  text(bar_x0 - 0.018, yy, summary_plot$item[idx_row], cex = 0.47, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.020, bar_x0 + width_value * (bar_x1 - bar_x0), yy + 0.020,
       col = nature_continuous_color(max(summary_value, 0.2), max_summary, nature_palette$pale_blue, color_high),
       border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_value * (bar_x1 - bar_x0) + 0.012, yy, summary_plot$display_value[idx_row], cex = 0.45, adj = c(0, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[11J FINAL] Wrote figure:", fig_d, "
")

claim_boundary_df <- data.frame(
  category = c(
    "allowed",
    "allowed",
    "allowed",
    "allowed",
    "prohibited",
    "prohibited",
    "prohibited",
    "prohibited",
    "prohibited"
  ),
  statement = c(
    "marker-rule-derived prioritization model audit",
    "ROC/PR evaluation only when score-label tables are detected",
    "Feature importance and candidate marker-signature overlap",
    "Model transparency support for transcriptomic prioritisation",
    "Clinical prediction",
    "Diagnostic biomarker validation",
    "Prognostic biomarker validation",
    "Therapeutic response biomarker validation",
    "Causal graft efficacy or safety proof"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_boundary_df, file.path(out_table_dir, "11J_FINAL_claim_boundary.csv"))

report_lines <- c(
  "11J FINAL report",
  "================",
  "Module: ML audit / ROC-PR / feature importance review",
  "Mode: complete standalone 11J rebuild; no previous 11J output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 09C, locked 11H, locked 11I outputs.",
  "",
  paste0("09C candidate files detected: ", length(file_09c_candidates)),
  paste0("Valid ROC/PR tasks detected: ", n_valid_metric_tasks),
  paste0("Median AUROC: ", round(median_auroc, 6)),
  paste0("Median AUPRC: ", round(median_auprc, 6)),
  paste0("Feature importance rows detected or fallback: ", n_feature_rows),
  paste0("Features overlapping 11H candidate marker signatures: ", n_marker_overlap),
  paste0("Features overlapping 11H risk-context markers: ", n_risk_marker_overlap),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_table_dir, "11J_FINAL_ROC_PR_metric_summary.csv")),
  paste0("- ", file.path(out_table_dir, "11J_FINAL_feature_importance_with_11H_marker_overlap.csv")),
  paste0("- ", file.path(out_table_dir, "11J_FINAL_feature_importance_summary.csv")),
  paste0("- ", file.path(out_table_dir, "11J_FINAL_calibration_bin_table.csv")),
  paste0("- ", file.path(out_table_dir, "11J_FINAL_locked_upstream_input_audit.csv")),
  "",
  "Allowed interpretation:",
  "- marker-rule-derived prioritization model audit and model transparency.",
  "- Candidate feature/marker overlap as transcriptomic prioritisation support.",
  "- ROC/PR only if raw score-label tables are detected.",
  "",
  "Prohibited interpretation:",
  "- No clinical prediction.",
  "- No validated biomarker claim.",
  "- No causal graft efficacy/safety claim.",
  "",
  "Decision: INPUT_READY_FOR_12A_FINAL_STORYLINE_REFRESH_AND_12B_FINAL_FIGURE_PLAN"
)
report_file <- file.path(out_text_dir, "11J_FINAL_ML_audit_ROC_PR_feature_importance_report.txt")
writeLines(report_lines, report_file)
cat("[11J FINAL] Wrote:", report_file, "\n")

cat("\n[11J FINAL] Completed ML audit / ROC-PR / feature importance review.\n")
cat("[11J FINAL] 09C candidate files detected:", length(file_09c_candidates), "\n")
cat("[11J FINAL] Valid ROC/PR tasks detected:", n_valid_metric_tasks, "\n")
cat("[11J FINAL] Median AUROC:", round(median_auroc, 6), "\n")
cat("[11J FINAL] Median AUPRC:", round(median_auprc, 6), "\n")
cat("[11J FINAL] Feature importance rows detected or fallback:", n_feature_rows, "\n")
cat("[11J FINAL] Features overlapping 11H candidate markers:", n_marker_overlap, "\n")
cat("[11J FINAL] Features overlapping 11H risk-context markers:", n_risk_marker_overlap, "\n")
cat("[11J FINAL] Figures written: 4\n")
cat("[11J FINAL] Decision: INPUT_READY_FOR_12A_FINAL_STORYLINE_REFRESH_AND_12B_FINAL_FIGURE_PLAN\n")
cat("[11J FINAL] Output tables:", out_table_dir, "\n")
cat("[11J FINAL] Output figs  :", out_fig_dir, "\n")
cat("[11J FINAL] Output text  :", out_text_dir, "\n")
cat("[11J FINAL] Next         : review 11J FINAL PDFs; if accepted, proceed to 12A final storyline refresh.\n")
