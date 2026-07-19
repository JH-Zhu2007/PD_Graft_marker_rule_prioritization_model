# ==============================================================================
# 00C_GSE178265_barcode_match_v2.R
# 项目：PD干细胞治疗“功能身份—疾病脆弱性悖论”
#
# 目的：
#   1. 读取 Single Cell Portal 的 METADATA_PD.tsv（优先使用未压缩版）
#   2. 读取 da_UMAP.tsv 中的人类DA亚型标签
#   3. 与 GEO 的 GSE178265_Homo_bcd.tsv.gz 做精确barcode匹配
#   4. 生成可直接用于后续DA易损性分析的合并metadata
#
# 注意：
#   - SCP文件第2行是 TYPE / numeric / group 等字段说明，不是细胞数据
#   - 脚本会自动删除这行
#   - 不读取4.74 GB表达矩阵，不会造成内存爆炸
# ==============================================================================


# ----------------------------- 0. 只改这里 -------------------------------------
PROJECT_ROOT <- "D:/PD_Graft_Project"

AUTO_INSTALL_CRAN <- TRUE
# ==============================================================================


# ----------------------------- 1. 环境与包 -------------------------------------
options(stringsAsFactors = FALSE)
options(timeout = 600)
set.seed(20260713)

required_cran <- c(
  "data.table",
  "openxlsx"
)

missing_cran <- required_cran[
  !vapply(required_cran, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_cran) > 0L) {
  if (AUTO_INSTALL_CRAN) {
    install.packages(missing_cran, dependencies = TRUE)
  } else {
    stop(
      "缺少CRAN包：",
      paste(missing_cran, collapse = ", ")
    )
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

if (!dir.exists(PROJECT_ROOT)) {
  stop("项目目录不存在：", PROJECT_ROOT)
}

PROJECT_ROOT <- normalizePath(
  PROJECT_ROOT,
  winslash = "/",
  mustWork = TRUE
)

external_dir <- file.path(
  PROJECT_ROOT,
  "00_raw_data",
  "GSE178265",
  "02_external_metadata"
)

geo_barcode_file <- file.path(
  PROJECT_ROOT,
  "00_raw_data",
  "GSE178265",
  "00_downloaded",
  "GSE178265_Homo_bcd.tsv.gz"
)

metadata_dir <- file.path(PROJECT_ROOT, "01_metadata")
objects_dir <- file.path(PROJECT_ROOT, "02_objects")
reports_dir <- file.path(PROJECT_ROOT, "06_reports")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(
  reports_dir,
  "00C_GSE178265_barcode_match_log.txt"
)

log_message <- function(...) {
  msg <- paste0(...)
  line <- paste0(
    "[",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] ",
    msg
  )
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

safe_pct <- function(num, den) {
  if (is.na(den) || den == 0L) return(NA_real_)
  100 * num / den
}

normalise_exact <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub('^"|"$', "", x)
  x
}

normalise_no_10x_suffix <- function(x) {
  x <- normalise_exact(x)
  sub("-[0-9]+$", "", x)
}

clean_column_names <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub('^"+|"+$', "", x)
  x <- gsub("^'+|'+$", "", x)
  x
}


# ----------------------------- 2. 文件定位 -------------------------------------
metadata_plain <- file.path(
  external_dir,
  "METADATA_PD.tsv"
)

metadata_gz <- file.path(
  external_dir,
  "METADATA_PD.tsv.gz"
)

da_file <- file.path(
  external_dir,
  "da_UMAP.tsv"
)

if (!file.exists(da_file)) {
  stop(
    "没有找到 da_UMAP.tsv：\n",
    da_file
  )
}

if (!file.exists(geo_barcode_file)) {
  stop(
    "没有找到GEO barcode文件：\n",
    geo_barcode_file
  )
}

# 优先使用已经解压且能被WPS打开的纯TSV文件
if (file.exists(metadata_plain)) {
  metadata_file <- metadata_plain
  metadata_source_type <- "plain_tsv"
} else if (file.exists(metadata_gz)) {
  metadata_file <- metadata_gz
  metadata_source_type <- "gzip_tsv"
} else {
  stop(
    "没有找到METADATA_PD.tsv或METADATA_PD.tsv.gz。\n",
    "请把文件放到：\n",
    external_dir
  )
}

log_message("00C开始。")
log_message("metadata文件：", metadata_file)
log_message("metadata类型：", metadata_source_type)
log_message("DA UMAP文件：", da_file)
log_message("GEO barcode文件：", geo_barcode_file)


# ----------------------------- 3. 读取SCP metadata ------------------------------
metadata_raw <- tryCatch(
  fread(
    metadata_file,
    sep = "\t",
    header = TRUE,
    colClasses = "character",
    quote = "",
    showProgress = TRUE,
    data.table = TRUE
  ),
  error = function(e) e
)

if (inherits(metadata_raw, "error")) {
  stop(
    "METADATA_PD读取失败：\n",
    conditionMessage(metadata_raw),
    "\n\n如果当前读取的是.gz文件，请使用你已经解压出的METADATA_PD.tsv，",
    "并放到02_external_metadata目录。"
  )
}

# SCP1768导出的TSV表头可能保留英文双引号，例如 "NAME"。
# 统一去掉列名两端引号后再检查。
setnames(
  metadata_raw,
  old = names(metadata_raw),
  new = clean_column_names(names(metadata_raw))
)

if (!"NAME" %in% names(metadata_raw)) {
  stop(
    "METADATA_PD中没有找到NAME列。\n",
    "当前列名：",
    paste(names(metadata_raw), collapse = " | ")
  )
}

# SCP第二行是TYPE/group/numeric等格式说明，必须删除
metadata_raw[, NAME := normalise_exact(NAME)]

n_before_type_filter <- nrow(metadata_raw)

metadata <- metadata_raw[
  !is.na(NAME) &
  nzchar(NAME) &
  toupper(NAME) != "TYPE"
]

n_type_rows_removed <- n_before_type_filter - nrow(metadata)

log_message(
  "METADATA_PD读取完成：",
  nrow(metadata),
  "个数据行；删除SCP TYPE说明行：",
  n_type_rows_removed
)

log_message(
  "METADATA_PD列名已标准化：",
  paste(names(metadata), collapse = " | ")
)


# ----------------------------- 4. 读取DA UMAP ----------------------------------
da_raw <- fread(
  da_file,
  sep = "\t",
  header = TRUE,
  colClasses = "character",
  quote = "",
  showProgress = FALSE,
  data.table = TRUE
)

setnames(
  da_raw,
  old = names(da_raw),
  new = clean_column_names(names(da_raw))
)

required_da_cols <- c(
  "NAME",
  "X",
  "Y",
  "Cell_Type"
)

missing_da_cols <- setdiff(
  required_da_cols,
  names(da_raw)
)

if (length(missing_da_cols) > 0L) {
  stop(
    "da_UMAP.tsv缺少列：",
    paste(missing_da_cols, collapse = ", "),
    "\n当前列名：",
    paste(names(da_raw), collapse = " | ")
  )
}

da_raw[, NAME := normalise_exact(NAME)]

da <- da_raw[
  !is.na(NAME) &
  nzchar(NAME) &
  toupper(NAME) != "TYPE"
]

da[, X := suppressWarnings(as.numeric(X))]
da[, Y := suppressWarnings(as.numeric(Y))]
da[, Cell_Type := trimws(Cell_Type)]

log_message(
  "da_UMAP读取完成：",
  nrow(da),
  "个DA细胞记录；亚型数：",
  uniqueN(da$Cell_Type)
)


# ----------------------------- 5. 读取GEO barcode -------------------------------
geo_bcd <- fread(
  geo_barcode_file,
  sep = "\t",
  header = FALSE,
  colClasses = "character",
  showProgress = FALSE,
  data.table = TRUE
)

if (ncol(geo_bcd) < 1L) {
  stop("GEO barcode文件为空。")
}

setnames(geo_bcd, 1L, "NAME")
geo_bcd <- geo_bcd[, .(NAME)]
geo_bcd[, NAME := normalise_exact(NAME)]
geo_bcd <- geo_bcd[
  !is.na(NAME) &
  nzchar(NAME)
]

log_message(
  "GEO barcode读取完成：",
  nrow(geo_bcd),
  "个barcode。"
)


# ----------------------------- 6. 匹配策略比较 ---------------------------------
metadata_names_exact <- unique(metadata$NAME)
da_names_exact <- unique(da$NAME)
geo_names_exact <- unique(geo_bcd$NAME)

exact_meta_geo_n <- length(
  intersect(metadata_names_exact, geo_names_exact)
)

exact_da_meta_n <- length(
  intersect(da_names_exact, metadata_names_exact)
)

exact_da_geo_n <- length(
  intersect(da_names_exact, geo_names_exact)
)

metadata_names_nosuffix <- unique(
  normalise_no_10x_suffix(metadata$NAME)
)

da_names_nosuffix <- unique(
  normalise_no_10x_suffix(da$NAME)
)

geo_names_nosuffix <- unique(
  normalise_no_10x_suffix(geo_bcd$NAME)
)

nosuffix_meta_geo_n <- length(
  intersect(metadata_names_nosuffix, geo_names_nosuffix)
)

nosuffix_da_meta_n <- length(
  intersect(da_names_nosuffix, metadata_names_nosuffix)
)

nosuffix_da_geo_n <- length(
  intersect(da_names_nosuffix, geo_names_nosuffix)
)

strategy_table <- data.table(
  strategy = c(
    "exact",
    "remove_trailing_10x_suffix"
  ),
  metadata_geo_overlap_n = c(
    exact_meta_geo_n,
    nosuffix_meta_geo_n
  ),
  metadata_geo_overlap_pct_of_geo = c(
    safe_pct(exact_meta_geo_n, length(geo_names_exact)),
    safe_pct(nosuffix_meta_geo_n, length(geo_names_nosuffix))
  ),
  da_metadata_overlap_n = c(
    exact_da_meta_n,
    nosuffix_da_meta_n
  ),
  da_metadata_overlap_pct_of_da = c(
    safe_pct(exact_da_meta_n, length(da_names_exact)),
    safe_pct(nosuffix_da_meta_n, length(da_names_nosuffix))
  ),
  da_geo_overlap_n = c(
    exact_da_geo_n,
    nosuffix_da_geo_n
  ),
  da_geo_overlap_pct_of_da = c(
    safe_pct(exact_da_geo_n, length(da_names_exact)),
    safe_pct(nosuffix_da_geo_n, length(da_names_nosuffix))
  )
)

best_strategy <- strategy_table[
  order(
    -metadata_geo_overlap_pct_of_geo,
    -da_metadata_overlap_pct_of_da
  ),
  strategy
][1L]

if (best_strategy == "exact") {
  metadata[, barcode_key := normalise_exact(NAME)]
  da[, barcode_key := normalise_exact(NAME)]
  geo_bcd[, barcode_key := normalise_exact(NAME)]
} else {
  metadata[, barcode_key := normalise_no_10x_suffix(NAME)]
  da[, barcode_key := normalise_no_10x_suffix(NAME)]
  geo_bcd[, barcode_key := normalise_no_10x_suffix(NAME)]
}

log_message("自动选择barcode匹配策略：", best_strategy)


# ----------------------------- 7. 去重与合并 -----------------------------------
metadata_duplicate_n <- sum(duplicated(metadata$barcode_key))
da_duplicate_n <- sum(duplicated(da$barcode_key))
geo_duplicate_n <- sum(duplicated(geo_bcd$barcode_key))

metadata_unique <- metadata[
  !duplicated(barcode_key)
]

da_unique <- da[
  !duplicated(barcode_key)
]

geo_unique <- geo_bcd[
  !duplicated(barcode_key)
]

# DA标签与全metadata合并
da_metadata_merged <- merge(
  da_unique,
  metadata_unique,
  by = "barcode_key",
  all.x = TRUE,
  suffixes = c("_DA", "_META"),
  sort = FALSE
)

# 再标记是否属于GEO过滤表达矩阵
da_metadata_merged[
  ,
  in_GEO_filtered_matrix :=
    barcode_key %in% geo_unique$barcode_key
]

# metadata是否属于GEO过滤矩阵
metadata_unique[
  ,
  in_GEO_filtered_matrix :=
    barcode_key %in% geo_unique$barcode_key
]


# ----------------------------- 8. 关键字段识别 ---------------------------------
find_first_column <- function(columns, patterns) {
  for (pat in patterns) {
    hit <- columns[
      grepl(
        pat,
        columns,
        ignore.case = TRUE,
        perl = TRUE
      )
    ]
    if (length(hit) > 0L) return(hit[1L])
  }
  NA_character_
}

metadata_columns <- names(metadata_unique)

donor_col <- find_first_column(
  metadata_columns,
  c(
    "^donor_id$",
    "^donor$",
    "subject",
    "individual"
  )
)

disease_col <- find_first_column(
  metadata_columns,
  c(
    "^disease$",
    "diagnosis",
    "^status$",
    "condition"
  )
)

organ_col <- find_first_column(
  metadata_columns,
  c(
    "^organ$",
    "region",
    "brain"
  )
)

sex_col <- find_first_column(
  metadata_columns,
  c(
    "^sex$",
    "gender"
  )
)

age_col <- find_first_column(
  metadata_columns,
  c(
    "donor_age",
    "^age$"
  )
)

key_fields <- data.table(
  semantic_field = c(
    "barcode",
    "donor",
    "disease",
    "organ_or_region",
    "sex",
    "age",
    "DA_subtype"
  ),
  detected_column = c(
    "NAME",
    donor_col,
    disease_col,
    organ_col,
    sex_col,
    age_col,
    "Cell_Type"
  ),
  detected = c(
    TRUE,
    !is.na(donor_col),
    !is.na(disease_col),
    !is.na(organ_col),
    !is.na(sex_col),
    !is.na(age_col),
    TRUE
  )
)


# ----------------------------- 9. 统计与判定 -----------------------------------
best_row <- strategy_table[
  strategy == best_strategy
]

n_da_merged_with_metadata <- sum(
  !is.na(da_metadata_merged$NAME_META)
)

n_da_in_geo <- sum(
  da_metadata_merged$in_GEO_filtered_matrix,
  na.rm = TRUE
)

subtype_counts <- da_unique[
  ,
  .N,
  by = Cell_Type
][order(-N)]

agtr1_n <- da_unique[
  Cell_Type == "SOX6_AGTR1",
  .N
]

readiness <- if (
  best_row$metadata_geo_overlap_pct_of_geo >= 95 &&
  best_row$da_metadata_overlap_pct_of_da >= 95 &&
  n_da_in_geo / nrow(da_unique) >= 0.95 &&
  !is.na(donor_col) &&
  !is.na(disease_col)
) {
  "READY_FOR_FORMAL_GSE178265_ANALYSIS"
} else if (
  best_row$metadata_geo_overlap_pct_of_geo >= 80 &&
  best_row$da_metadata_overlap_pct_of_da >= 80
) {
  "PARTIAL_MATCH_REQUIRES_MANUAL_REVIEW"
} else {
  "BARCODE_MATCH_FAILED"
}

summary_table <- data.table(
  metric = c(
    "metadata_source_file",
    "metadata_rows_after_TYPE_removal",
    "metadata_unique_barcodes",
    "metadata_duplicate_barcodes",
    "GEO_barcode_rows",
    "GEO_unique_barcodes",
    "GEO_duplicate_barcodes",
    "DA_rows_after_TYPE_removal",
    "DA_unique_barcodes",
    "DA_duplicate_barcodes",
    "DA_subtype_count",
    "SOX6_AGTR1_cells",
    "best_barcode_strategy",
    "metadata_to_GEO_overlap_pct",
    "DA_to_metadata_overlap_pct",
    "DA_to_GEO_overlap_pct",
    "DA_rows_merged_with_metadata",
    "DA_rows_present_in_GEO_matrix",
    "donor_column",
    "disease_column",
    "organ_or_region_column",
    "readiness"
  ),
  value = as.character(c(
    metadata_file,
    nrow(metadata),
    uniqueN(metadata$barcode_key),
    metadata_duplicate_n,
    nrow(geo_bcd),
    uniqueN(geo_bcd$barcode_key),
    geo_duplicate_n,
    nrow(da),
    uniqueN(da$barcode_key),
    da_duplicate_n,
    uniqueN(da$Cell_Type),
    agtr1_n,
    best_strategy,
    round(best_row$metadata_geo_overlap_pct_of_geo, 4),
    round(best_row$da_metadata_overlap_pct_of_da, 4),
    round(best_row$da_geo_overlap_pct_of_da, 4),
    n_da_merged_with_metadata,
    n_da_in_geo,
    donor_col,
    disease_col,
    organ_col,
    readiness
  ))
)


# ----------------------------- 10. 保存对象与表格 ------------------------------
saveRDS(
  metadata_unique,
  file.path(
    objects_dir,
    "00C_GSE178265_all_cell_metadata_clean.rds"
  ),
  compress = TRUE
)

saveRDS(
  da_unique,
  file.path(
    objects_dir,
    "00C_GSE178265_DA_UMAP_annotation_clean.rds"
  ),
  compress = TRUE
)

saveRDS(
  da_metadata_merged,
  file.path(
    objects_dir,
    "00C_GSE178265_DA_metadata_merged.rds"
  ),
  compress = TRUE
)

fwrite(
  da_metadata_merged,
  file.path(
    metadata_dir,
    "00C_GSE178265_DA_metadata_merged.csv"
  ),
  bom = TRUE
)

fwrite(
  strategy_table,
  file.path(
    metadata_dir,
    "00C_GSE178265_barcode_strategy_comparison.csv"
  ),
  bom = TRUE
)

fwrite(
  subtype_counts,
  file.path(
    metadata_dir,
    "00C_GSE178265_DA_subtype_counts.csv"
  ),
  bom = TRUE
)

fwrite(
  summary_table,
  file.path(
    metadata_dir,
    "00C_GSE178265_barcode_match_summary.csv"
  ),
  bom = TRUE
)

wb <- createWorkbook()

addWorksheet(wb, "summary")
writeDataTable(wb, "summary", summary_table)

addWorksheet(wb, "strategy_comparison")
writeDataTable(wb, "strategy_comparison", strategy_table)

addWorksheet(wb, "key_fields")
writeDataTable(wb, "key_fields", key_fields)

addWorksheet(wb, "DA_subtype_counts")
writeDataTable(wb, "DA_subtype_counts", subtype_counts)

addWorksheet(wb, "DA_merged_preview")
writeDataTable(
  wb,
  "DA_merged_preview",
  head(da_metadata_merged, 1000L)
)

for (sheet in names(wb)) {
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(
    wb,
    sheet,
    cols = 1:50,
    widths = "auto"
  )
}

saveWorkbook(
  wb,
  file.path(
    metadata_dir,
    "00C_GSE178265_barcode_match.xlsx"
  ),
  overwrite = TRUE
)


# ----------------------------- 11. 最终报告 ------------------------------------
report_lines <- c(
  "GSE178265｜SCP1768 barcode匹配报告",
  paste0("生成时间：", Sys.time()),
  "",
  paste0("Metadata文件：", metadata_file),
  paste0("DA UMAP文件：", da_file),
  paste0("GEO barcode文件：", geo_barcode_file),
  "",
  paste0("最佳匹配策略：", best_strategy),
  paste0(
    "Metadata→GEO匹配率：",
    round(best_row$metadata_geo_overlap_pct_of_geo, 4),
    "%"
  ),
  paste0(
    "DA→Metadata匹配率：",
    round(best_row$da_metadata_overlap_pct_of_da, 4),
    "%"
  ),
  paste0(
    "DA→GEO匹配率：",
    round(best_row$da_geo_overlap_pct_of_da, 4),
    "%"
  ),
  paste0("DA亚型数：", uniqueN(da$Cell_Type)),
  paste0("SOX6_AGTR1细胞数：", agtr1_n),
  paste0("Donor字段：", donor_col),
  paste0("Disease字段：", disease_col),
  paste0("最终状态：", readiness),
  "",
  "输出对象：",
  file.path(
    objects_dir,
    "00C_GSE178265_all_cell_metadata_clean.rds"
  ),
  file.path(
    objects_dir,
    "00C_GSE178265_DA_metadata_merged.rds"
  )
)

writeLines(
  report_lines,
  file.path(
    reports_dir,
    "00C_GSE178265_barcode_match_report.txt"
  ),
  useBytes = TRUE
)


# ----------------------------- 12. 控制台输出 ----------------------------------
cat("\n")
cat("============================================================\n")
cat("00C GSE178265 barcode匹配完成\n")
cat("============================================================\n")

print(summary_table)

cat("\nDA亚型数量：\n")
print(subtype_counts)

cat("\n最终状态：", readiness, "\n")

cat("\n主要输出：\n")
cat(
  file.path(
    metadata_dir,
    "00C_GSE178265_barcode_match.xlsx"
  ),
  "\n"
)
cat(
  file.path(
    metadata_dir,
    "00C_GSE178265_DA_metadata_merged.csv"
  ),
  "\n"
)
cat(
  file.path(
    objects_dir,
    "00C_GSE178265_DA_metadata_merged.rds"
  ),
  "\n"
)

if (readiness == "READY_FOR_FORMAL_GSE178265_ANALYSIS") {
  cat(
    "\nGSE178265的metadata、DA亚型和GEO表达barcode已经通过匹配，",
    "可以进入正式分析。\n",
    sep = ""
  )
} else {
  cat(
    "\n匹配率未达到正式分析阈值，请把控制台summary和strategy表发来检查。\n"
  )
}

gc(verbose = FALSE)
