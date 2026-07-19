
PROJECT_ROOT <- "D:/PD_Graft_Project"
N_WORKERS <- 2L

options(stringsAsFactors = FALSE)
options(timeout = 1800)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("缺少data.table包。请先运行 install.packages('data.table')")
}

library(data.table)

PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = TRUE)
plain_dir <- file.path(PROJECT_ROOT, "00_raw_data", "GSE200610", "01_extracted_plain_csv")
metadata_dir <- file.path(PROJECT_ROOT, "01_metadata")
reports_dir <- file.path(PROJECT_ROOT, "06_reports")
tmp_dir <- file.path(PROJECT_ROOT, "tools", "fread_tmp_GSE200610_plain")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(...), "\n", sep = "")
}

human_size <- function(bytes) {
  if (length(bytes) == 0L || is.na(bytes)) return(NA_character_)
  units <- c("B", "KB", "MB", "GB", "TB")
  x <- as.numeric(bytes)
  i <- 1L
  while (x >= 1024 && i < length(units)) {
    x <- x / 1024
    i <- i + 1L
  }
  sprintf("%.2f %s", x, units[i])
}

shannon_entropy <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  if (length(chars) == 0L) return(NA_real_)
  p <- table(chars) / length(chars)
  -sum(p * log2(p))
}

max_homopolymer <- function(x) {
  runs <- rle(strsplit(x, "", fixed = TRUE)[[1L]])$lengths
  if (length(runs) == 0L) return(NA_integer_)
  max(runs)
}

extract_dna_tokens <- function(x, min_len = 10L, max_len = 40L) {
  if (length(x) == 0L) return(character())
  hits <- regmatches(
    toupper(x),
    gregexpr(paste0("[ACGTN]{", min_len, ",", max_len, "}"), toupper(x), perl = TRUE)
  )
  unique(unlist(hits, use.names = FALSE))
}

is_ordinary_10x_name <- function(x) {
  grepl("(^|[_:.\\-])[ACGT]{16}-[0-9]+$|[ACGT]{16}-[0-9]+$|^[ACGT]{16}$", toupper(x), perl = TRUE)
}

classify_sequence <- function(dna_seq, source_type, recurrence_n) {
  dna_seq_upper <- toupper(as.character(dna_seq))
  n <- nchar(dna_seq_upper)
  ent <- shannon_entropy(dna_seq_upper)
  hp <- max_homopolymer(dna_seq_upper)

  if (source_type == "column_name" && n == 16L) return("LIKELY_10X_CELL_BARCODE")
  if (n < 12L || n > 32L) return("IMPLAUSIBLE_LENGTH")
  if (is.na(ent) || ent < 1.35 || (!is.na(hp) && hp >= 7L)) return("LOW_COMPLEXITY_DNA_LIKE_TOKEN")
  if (source_type == "feature_name" && recurrence_n >= 2L) return("RECURRENT_POTENTIAL_BARCODE_FEATURE")
  if (source_type == "feature_name") return("ISOLATED_POTENTIAL_BARCODE_FEATURE")
  "UNRESOLVED_DNA_LIKE_TOKEN"
}

audit_one_file <- function(path, tmp_dir) {
  suppressPackageStartupMessages(library(data.table))
  file_name <- basename(path)

  header <- fread(
    path,
    nrows = 0L,
    showProgress = FALSE,
    tmpdir = tmp_dir,
    data.table = TRUE,
    check.names = FALSE
  )

  column_names <- names(header)
  if (length(column_names) == 0L) stop("文件没有可识别列：", file_name)

  first_col <- fread(
    path,
    select = 1L,
    colClasses = "character",
    showProgress = FALSE,
    tmpdir = tmp_dir,
    data.table = TRUE,
    check.names = FALSE
  )[[1L]]

  explicit_pattern <- paste(
    c("clone", "clonotype", "lineage", "molecular.?barcode", "viral.?barcode", "barcode.?id", "clone.?id", "bc.?id"),
    collapse = "|"
  )

  explicit_columns <- column_names[
    grepl(explicit_pattern, column_names, ignore.case = TRUE, perl = TRUE)
  ]

  data_columns <- if (length(column_names) > 1L) column_names[-1L] else character()
  ordinary_10x_n <- sum(is_ordinary_10x_name(data_columns))
  column_tokens <- extract_dna_tokens(data_columns)
  feature_exact <- unique(
    toupper(first_col[grepl("^[ACGTN]{10,40}$", toupper(first_col), perl = TRUE)])
  )

  candidates <- rbindlist(
    list(
      data.table(dataset_file = file_name, source_type = "column_name", sequence = column_tokens),
      data.table(dataset_file = file_name, source_type = "feature_name", sequence = feature_exact)
    ),
    fill = TRUE
  )

  if (nrow(candidates) > 0L) candidates <- unique(candidates)

  summary <- data.table(
    file_name = file_name,
    file_size_bytes = file.info(path)$size,
    file_size = human_size(file.info(path)$size),
    n_columns = length(column_names),
    n_data_columns = max(0L, length(column_names) - 1L),
    n_features = length(first_col),
    explicit_mapping_column_n = length(explicit_columns),
    explicit_mapping_columns = paste(explicit_columns, collapse = " | "),
    ordinary_10x_column_n = ordinary_10x_n,
    dna_like_column_token_n = length(column_tokens),
    exact_dna_feature_n = length(feature_exact)
  )

  list(summary = summary, candidates = candidates)
}

if (!dir.exists(plain_dir)) {
  stop("目录不存在：", plain_dir, "\n请先建立目录并把14个.csv.gz完全解压成普通.csv。")
}

all_files <- list.files(plain_dir, full.names = TRUE, recursive = FALSE)
plain_files <- all_files[
  grepl("\\.csv$", all_files, ignore.case = TRUE) &
    !grepl("\\.csv\\.gz$", all_files, ignore.case = TRUE)
]
gz_files <- all_files[grepl("\\.csv\\.gz$", all_files, ignore.case = TRUE)]

stamp("普通.csv数量：", length(plain_files))
stamp(".csv.gz数量：", length(gz_files))

if (length(plain_files) == 0L && length(gz_files) > 0L) {
  stop(
    "当前目录里仍然只有.csv.gz压缩文件，并没有普通.csv。\n",
    "请在资源管理器打开‘查看 → 显示 → 文件扩展名’，然后选中14个.csv.gz，",
    "右键WinRAR → 解压到当前文件夹。"
  )
}

if (length(plain_files) != 14L) {
  stop("应检测到14个普通.csv，实际检测到：", length(plain_files), "\n请确认没有漏解压、重复文件或子文件夹。")
}

file_inventory <- data.table(
  file_name = basename(plain_files),
  file_path = normalizePath(plain_files, winslash = "/", mustWork = TRUE),
  size_bytes = file.info(plain_files)$size,
  size_human = vapply(file.info(plain_files)$size, human_size, character(1L))
)

fwrite(file_inventory, file.path(metadata_dir, "00E4_GSE200610_plain_csv_inventory.csv"))
stamp("14个普通CSV检查通过。")
stamp("使用worker数量：", N_WORKERS)

if (.Platform$OS.type == "windows" && N_WORKERS > 1L) {
  cl <- parallel::makeCluster(N_WORKERS)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterExport(
    cl,
    varlist = c("audit_one_file", "human_size", "extract_dna_tokens", "is_ordinary_10x_name", "tmp_dir"),
    envir = environment()
  )

  results <- parallel::parLapply(cl, plain_files, audit_one_file, tmp_dir = tmp_dir)
} else {
  results <- lapply(plain_files, audit_one_file, tmp_dir = tmp_dir)
}

summary_table <- rbindlist(lapply(results, `[[`, "summary"), fill = TRUE)
candidate_table <- rbindlist(lapply(results, `[[`, "candidates"), fill = TRUE)

if (nrow(candidate_table) > 0L) {
  recurrence <- candidate_table[, .(recurrence_n = uniqueN(dataset_file), source_file_n = .N), by = .(sequence, source_type)]
  candidate_table <- merge(candidate_table, recurrence, by = c("sequence", "source_type"), all.x = TRUE)

  candidate_table[, `:=`(
    sequence_length = nchar(sequence),
    shannon_entropy = vapply(sequence, shannon_entropy, numeric(1L)),
    max_homopolymer = vapply(sequence, max_homopolymer, integer(1L))
  )]

  candidate_table$classification <- mapply(
    FUN = classify_sequence,
    dna_seq = candidate_table$sequence,
    source_type = candidate_table$source_type,
    recurrence_n = candidate_table$recurrence_n,
    USE.NAMES = FALSE
  )

  candidate_table <- unique(candidate_table)
} else {
  candidate_table <- data.table(
    sequence = character(), source_type = character(), dataset_file = character(),
    recurrence_n = integer(), source_file_n = integer(), sequence_length = integer(),
    shannon_entropy = numeric(), max_homopolymer = integer(), classification = character()
  )
}

explicit_mapping_found <- any(summary_table$explicit_mapping_column_n > 0L)
potential_feature_found <- any(candidate_table$classification %in% c(
  "RECURRENT_POTENTIAL_BARCODE_FEATURE",
  "ISOLATED_POTENTIAL_BARCODE_FEATURE"
))

if (explicit_mapping_found) {
  final_status <- "EXPLICIT_CLONE_MAPPING_CANDIDATE_COLUMN_FOUND"
  interpretation <- "发现显式clone/barcode相关字段，需要人工确认是否能建立cell-to-clone映射。"
} else if (potential_feature_found) {
  final_status <- "POTENTIAL_BARCODE_FEATURES_FOUND_MAPPING_STILL_REQUIRED"
  interpretation <- paste(
    "发现DNA样feature候选，但没有显式cell-to-clone字段。",
    "这些候选不能直接当作clone ID；仍需作者映射表或原始建库说明。"
  )
} else {
  final_status <- "NO_RECOVERABLE_CLONE_MAPPING_IN_PUBLIC_COUNT_MATRICES"
  interpretation <- paste(
    "公开14个计数矩阵中没有发现可直接恢复的cell-to-clone映射。",
    "正式项目应启用命运倾向与安全风险替代模块。"
  )
}

summary_csv <- file.path(metadata_dir, "00E4_GSE200610_plain_csv_summary.csv")
candidate_csv <- file.path(metadata_dir, "00E4_GSE200610_barcode_candidate_classification.csv")
report_txt <- file.path(reports_dir, "00E4_GSE200610_plain_csv_candidate_validation_report.txt")

fwrite(summary_table, summary_csv)
fwrite(candidate_table, candidate_csv)

classification_counts <- if (nrow(candidate_table) > 0L) {
  candidate_table[, .N, by = classification][order(-N)]
} else {
  data.table(classification = "NONE", N = 0L)
}

report_lines <- c(
  "GSE200610普通CSV候选barcode审计报告",
  paste0("生成时间：", Sys.time()),
  "",
  paste0("普通CSV数量：", length(plain_files)),
  paste0("并行worker：", N_WORKERS),
  paste0("显式clone/barcode字段存在：", explicit_mapping_found),
  paste0("最终状态：", final_status),
  "",
  interpretation,
  "",
  "分类统计：",
  capture.output(print(classification_counts)),
  "",
  "重要限制：",
  "DNA样字符串不等于clone barcode。",
  "没有cell-to-clone映射时，不允许把任何候选直接写成真实clone。"
)

writeLines(report_lines, report_txt, useBytes = TRUE)

if (requireNamespace("openxlsx", quietly = TRUE)) {
  xlsx_file <- file.path(metadata_dir, "00E4_GSE200610_plain_csv_candidate_validation.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "file_summary")
  openxlsx::writeData(wb, "file_summary", summary_table)
  openxlsx::addWorksheet(wb, "candidate_classification")
  openxlsx::writeData(wb, "candidate_classification", candidate_table)
  openxlsx::addWorksheet(wb, "classification_counts")
  openxlsx::writeData(wb, "classification_counts", classification_counts)
  openxlsx::addWorksheet(wb, "decision")
  openxlsx::writeData(wb, "decision", data.frame(final_status = final_status, interpretation = interpretation))
  openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
}

cat("\n============================================================\n")
cat("00E4 GSE200610普通CSV候选验证完成\n")
cat("============================================================\n")
cat("普通CSV：", length(plain_files), "/14\n")
cat("显式clone/barcode字段：", explicit_mapping_found, "\n")
cat("最终状态：", final_status, "\n\n")
cat(interpretation, "\n\n")
cat("候选分类统计：\n")
print(classification_counts)
cat("\n主要输出：\n")
cat(summary_csv, "\n")
cat(candidate_csv, "\n")
cat(report_txt, "\n")

if (final_status == "NO_RECOVERABLE_CLONE_MAPPING_IN_PUBLIC_COUNT_MATRICES") {
  cat("\n下一步：进入正式01数据导入与对象构建，并启用命运倾向/安全风险替代模块。\n")
} else {
  cat("\n下一步：先人工核查候选；若没有cell-to-clone映射，仍启用替代模块。\n")
}

gc(verbose = FALSE)
