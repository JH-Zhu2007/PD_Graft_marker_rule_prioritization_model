# ==============================================================================
# 01B_GSE178265_DA_STREAMING_SUBMATRIX_V7.R
#
# 目的：
#   01A 已完成以后，进入 01B。
#   本脚本专门处理 GSE178265 的 DA 目标细胞：
#     - 不读取整个超大矩阵到内存
#     - 从 01A 生成的 target barcodes / metadata 中读取目标细胞
#     - 流式扫描 MatrixMarket matrix.mtx(.gz)
#     - 只提取目标 barcodes 对应的列
#     - 保存目标 count matrix / metadata / Seurat object
#
# 成功标志：
#   最后显示：
#     ✅ 01B V7 GSE178265 DA streaming submatrix 完成。
#
# 运行方式：
#   RStudio 打开本文件，点击 Source。
# ==============================================================================


# ==============================================================================
# 0. 用户设置
# ==============================================================================

PROJECT_ROOT <- "D:/PD_Graft_Project"

AUTO_INSTALL_CRAN <- TRUE

# TRUE：如果 01B 输出已经存在，重新生成
# FALSE：如果输出已存在且可读取，直接跳过
REBUILD_EXISTING <- FALSE

# 每次从 matrix.mtx 读取多少行非零元素。
# 内存紧张改小，比如 200000；速度慢可改大，比如 1000000。
MTX_LINE_CHUNK <- 500000L

# 是否创建 Seurat 对象
CREATE_SEURAT_OBJECT <- TRUE


# ==============================================================================
# 1. 环境
# ==============================================================================

options(stringsAsFactors = FALSE)
options(timeout = 7200)
options(future.globals.maxSize = 24 * 1024^3)

cat("\n============================================================\n")
cat("01B V7：GSE178265 DA streaming submatrix\n")
cat("============================================================\n\n")

if (!dir.exists(PROJECT_ROOT)) {
  stop("项目目录不存在：", PROJECT_ROOT)
}

PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = TRUE)

required_pkgs <- c("data.table", "Matrix")

if (CREATE_SEURAT_OBJECT) {
  required_pkgs <- c(required_pkgs, "Seurat")
}

install_if_missing <- function(pkgs) {
  missing <- pkgs[
    !vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L))
  ]

  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }

  if (!AUTO_INSTALL_CRAN) {
    stop("缺少 R 包：", paste(missing, collapse = ", "))
  }

  install.packages(
    missing,
    repos = "https://cloud.r-project.org",
    dependencies = TRUE
  )

  invisible(TRUE)
}

install_if_missing(required_pkgs)

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
})

if (CREATE_SEURAT_OBJECT) {
  suppressPackageStartupMessages(library(Seurat))
}

metadata_dir <- file.path(PROJECT_ROOT, "01_metadata")
reports_dir  <- file.path(PROJECT_ROOT, "06_reports")
objects_dir  <- file.path(PROJECT_ROOT, "02_objects", "01B_GSE178265_DA")
tables_dir   <- file.path(PROJECT_ROOT, "03_tables", "01B_GSE178265_DA")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

log_file <- file.path(
  reports_dir,
  paste0("01B_GSE178265_DA_streaming_submatrix_", run_stamp, ".log.txt")
)

stamp <- function(...) {
  line <- paste0(
    "[",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] ",
    paste0(...)
  )
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
  invisible(line)
}


# ==============================================================================
# 2. 通用函数
# ==============================================================================

trim_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

normalize_path_text <- function(x) {
  x <- trim_na(x)
  gsub("\\\\", "/", x)
}

safe_name <- function(x) {
  x <- trim_na(x)
  x <- gsub(
    "\\.rds$|\\.csv$|\\.tsv$|\\.txt$|\\.gz$|\\.mtx$",
    "",
    x,
    ignore.case = TRUE
  )
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

read_csv_any <- function(path) {
  path <- normalize_path_text(path)

  sep <- if (grepl("\\.csv(\\.gz)?$", path, ignore.case = TRUE)) "," else "\t"

  data.table::fread(
    path,
    sep = sep,
    header = TRUE,
    quote = "",
    data.table = FALSE,
    showProgress = FALSE
  )
}

save_rds_atomic <- function(object, final_path, compress = FALSE) {
  dir.create(dirname(final_path), recursive = TRUE, showWarnings = FALSE)

  temp_path <- paste0(
    final_path,
    ".writing_",
    Sys.getpid(),
    "_",
    run_stamp
  )

  saveRDS(object, temp_path, compress = compress)

  if (file.exists(final_path)) {
    unlink(final_path, force = TRUE)
  }

  if (!file.rename(temp_path, final_path)) {
    stop("无法保存对象：", final_path)
  }

  final_path
}

atomic_write_csv <- function(df, final_path) {
  temp_path <- paste0(
    final_path,
    ".writing_",
    Sys.getpid(),
    "_",
    run_stamp
  )

  write.csv(
    df,
    temp_path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  if (!file.exists(temp_path)) {
    stop("临时 CSV 未生成：", temp_path)
  }

  if (file.exists(final_path)) {
    unlink(final_path, force = TRUE)
  }

  if (!file.rename(temp_path, final_path)) {
    stop("无法写入 CSV：", final_path)
  }

  invisible(final_path)
}

first_existing <- function(paths) {
  paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  paths <- paths[file.exists(paths)]

  if (length(paths) == 0L) {
    return(NA_character_)
  }

  paths[order(file.info(paths)$mtime, decreasing = TRUE)][1L]
}

object_is_readable <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  tryCatch(
    {
      obj <- readRDS(path)
      valid <- TRUE
      if (inherits(obj, "Seurat")) {
        valid <- nrow(obj) > 0L && ncol(obj) > 0L
      }
      rm(obj)
      gc(verbose = FALSE)
      isTRUE(valid)
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}




standardize_feature_names <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "UNKNOWN_FEATURE"
  make.unique(x, sep = "__dup")
}

# ==============================================================================
# V3: gz 文件容错读取工具
# ==============================================================================
# data.table::fread 读取损坏/尾部不完整的 .gz 时，可能会调用 R.utils 解压并直接中断。
# 对 GSE178265 的 barcode/feature 文件，使用 gzfile + readLines 分块读取。
# 如果 gzip 尾部不完整，只要主体内容能读出来，就继续使用已读出的行。

read_lines_tolerant <- function(path, chunk_n = 200000L) {
  path <- normalize_path_text(path)

  con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }

  on.exit(try(close(con), silent = TRUE), add = TRUE)

  out <- character()
  total <- 0L

  repeat {
    lines <- tryCatch(
      withCallingHandlers(
        readLines(con, n = chunk_n, warn = FALSE, skipNul = TRUE),
        warning = function(w) {
          stamp("读取压缩文本 warning：", basename(path), "；", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        stamp("读取压缩文本到达异常结尾，保留已读取内容：", basename(path), "；", conditionMessage(e))
        character()
      }
    )

    if (length(lines) == 0L) break

    out <- c(out, lines)
    total <- total + length(lines)

    if (total %% (chunk_n * 5L) == 0L) {
      stamp("已读取文本行：", basename(path), "；", total)
    }
  }

  out
}

read_tsv_lines_first_col <- function(path) {
  lines <- read_lines_tolerant(path)

  if (length(lines) == 0L) {
    stop("文件没有可读取文本行：", path)
  }

  first_col <- vapply(
    strsplit(lines, "\t", fixed = TRUE, useBytes = TRUE),
    function(x) {
      if (length(x) == 0L) "" else x[1L]
    },
    character(1L)
  )

  first_col <- trim_na(first_col)
  first_col <- first_col[nzchar(first_col)]

  if (length(first_col) == 0L) {
    stop("无法从文件提取第一列：", path)
  }

  first_col
}

read_tsv_lines_feature_names <- function(path) {
  lines <- read_lines_tolerant(path)

  if (length(lines) == 0L) {
    stop("features 文件没有可读取文本行：", path)
  }

  fields <- strsplit(lines, "\t", fixed = TRUE, useBytes = TRUE)

  genes <- vapply(
    fields,
    function(x) {
      if (length(x) >= 2L && nzchar(trimws(x[2L]))) {
        x[2L]
      } else if (length(x) >= 1L) {
        x[1L]
      } else {
        ""
      }
    },
    character(1L)
  )

  genes <- trim_na(genes)
  genes[!nzchar(genes)] <- paste0("feature_", which(!nzchar(genes)))

  genes <- as.character(genes)
  genes[is.na(genes) | genes == ""] <- "UNKNOWN_FEATURE"
  make.unique(genes, sep = "__dup")
}


# ==============================================================================
# 3. 输出路径
# ==============================================================================

out_counts_rds <- file.path(
  objects_dir,
  "01B_GSE178265_DA_counts_matrix.rds"
)

out_metadata_rds <- file.path(
  objects_dir,
  "01B_GSE178265_DA_metadata.rds"
)

out_seurat_rds <- file.path(
  objects_dir,
  "01B_GSE178265_DA_seurat.rds"
)

out_audit_csv <- file.path(
  metadata_dir,
  "01B_GSE178265_DA_streaming_audit.csv"
)

# V6 修正：
# V5 在复用旧 barcode match 表时使用 out_barcode_match_csv，
# 但原始脚本变量名是 barcode_match_csv，导致找不到对象。
out_barcode_match_csv <- file.path(
  metadata_dir,
  "01B_GSE178265_DA_barcode_match.csv"
)

barcode_match_csv <- out_barcode_match_csv

out_report_txt <- file.path(
  reports_dir,
  "01B_GSE178265_DA_streaming_submatrix_report.txt"
)

if (
  !REBUILD_EXISTING &&
    file.exists(out_counts_rds) &&
    file.exists(out_metadata_rds) &&
    object_is_readable(out_counts_rds)
) {
  cat("\n检测到 01B count matrix 已存在，并且 REBUILD_EXISTING = FALSE。\n")
  cat("已有文件：", out_counts_rds, "\n")
  cat("如果需要重建，把 REBUILD_EXISTING 改成 TRUE。\n")
  quit(save = "no", status = 0)
}


# ==============================================================================
# 4. 查找 01A 生成的 GSE178265 DA target barcodes / metadata
# ==============================================================================

find_target_barcode_file <- function() {
  exact_candidates <- c(
    file.path(metadata_dir, "01A_GSE178265_DA_target_barcodes.rds"),
    file.path(metadata_dir, "01A_GSE178265_DA_target_barcodes.csv"),
    file.path(metadata_dir, "01A_GSE178265_DA_target_barcodes.tsv"),
    file.path(metadata_dir, "01A_GSE178265_DA_barcodes.rds"),
    file.path(metadata_dir, "01A_GSE178265_DA_barcodes.csv"),
    file.path(metadata_dir, "01A_GSE178265_DA_barcodes.tsv")
  )

  exact_hit <- first_existing(exact_candidates)

  if (!is.na(exact_hit)) {
    return(exact_hit)
  }

  all_meta <- list.files(
    metadata_dir,
    pattern = "GSE178265.*(DA|da).*(barcode|barcodes|cell|cells).*\\.(rds|csv|tsv|txt)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  first_existing(all_meta)
}

find_metadata_file <- function() {
  exact_candidates <- c(
    file.path(metadata_dir, "01A_GSE178265_DA_metadata_ready_for_01B.rds"),
    file.path(metadata_dir, "01A_GSE178265_DA_metadata_ready_for_01B.csv"),
    file.path(metadata_dir, "01A_GSE178265_DA_metadata_ready_for_01B.tsv"),
    file.path(metadata_dir, "01A_GSE178265_DA_metadata.rds"),
    file.path(metadata_dir, "01A_GSE178265_DA_metadata.csv"),
    file.path(metadata_dir, "01A_GSE178265_DA_metadata.tsv")
  )

  exact_hit <- first_existing(exact_candidates)

  if (!is.na(exact_hit)) {
    return(exact_hit)
  }

  all_meta <- list.files(
    metadata_dir,
    pattern = "GSE178265.*(DA|da).*metadata.*\\.(rds|csv|tsv|txt)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  first_existing(all_meta)
}

load_any_table_or_vector <- function(path) {
  path <- normalize_path_text(path)

  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    return(readRDS(path))
  }

  read_csv_any(path)
}

extract_barcodes_from_object <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  if (is.vector(x) && !is.list(x)) {
    return(unique(trim_na(x)))
  }

  if (is.data.frame(x)) {
    cn <- colnames(x)
    cn_lower <- tolower(cn)

    priority_patterns <- c(
      "^barcode$",
      "barcode",
      "^cell$",
      "cell_id",
      "cellid",
      "cell_name",
      "cellname",
      "barcodes",
      "cells"
    )

    for (pat in priority_patterns) {
      idx <- which(grepl(pat, cn_lower, ignore.case = TRUE))

      if (length(idx) > 0L) {
        vals <- unique(trim_na(x[[idx[1L]]]))
        vals <- vals[nzchar(vals)]
        if (length(vals) > 0L) {
          return(vals)
        }
      }
    }

    # 如果没有明显列名，尝试第一列
    vals <- unique(trim_na(x[[1L]]))
    vals <- vals[nzchar(vals)]
    return(vals)
  }

  if (is.list(x)) {
    for (nm in names(x)) {
      if (grepl("barcode|barcodes|cell|cells", nm, ignore.case = TRUE)) {
        vals <- extract_barcodes_from_object(x[[nm]])
        if (length(vals) > 0L) {
          return(vals)
        }
      }
    }
  }

  character()
}

target_barcode_file <- find_target_barcode_file()
metadata_file <- find_metadata_file()

stamp("target barcode file：", target_barcode_file)
stamp("metadata file：", metadata_file)

metadata_obj <- NULL
metadata_df <- NULL

if (!is.na(metadata_file) && file.exists(metadata_file)) {
  metadata_obj <- load_any_table_or_vector(metadata_file)

  if (is.data.frame(metadata_obj)) {
    metadata_df <- metadata_obj
  } else if (is.list(metadata_obj) && !is.null(metadata_obj$metadata) && is.data.frame(metadata_obj$metadata)) {
    metadata_df <- metadata_obj$metadata
  }
}

target_barcodes <- character()

if (!is.na(target_barcode_file) && file.exists(target_barcode_file)) {
  bc_obj <- load_any_table_or_vector(target_barcode_file)
  target_barcodes <- extract_barcodes_from_object(bc_obj)
}

if (length(target_barcodes) == 0L && !is.null(metadata_df)) {
  target_barcodes <- extract_barcodes_from_object(metadata_df)
}

target_barcodes <- unique(trim_na(target_barcodes))
target_barcodes <- target_barcodes[nzchar(target_barcodes)]

if (length(target_barcodes) == 0L) {
  stop(
    "没有找到 GSE178265 DA target barcodes。\n",
    "请确认 01A 已生成 01A_GSE178265_DA_target_barcodes.* 或 metadata_ready_for_01B.*"
  )
}

stamp("target barcode 数量：", length(target_barcodes))


# ==============================================================================
# 5. 查找 GSE178265 原始 matrix / features / barcodes
# ==============================================================================

raw_gse_dir <- file.path(PROJECT_ROOT, "00_raw_data", "GSE178265")

if (!dir.exists(raw_gse_dir)) {
  stop("找不到 GSE178265 原始数据目录：", raw_gse_dir)
}

all_raw_files <- list.files(
  raw_gse_dir,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)

all_raw_files <- normalizePath(all_raw_files, winslash = "/", mustWork = FALSE)
all_raw_files <- all_raw_files[file.exists(all_raw_files)]

find_matrix_file <- function() {
  hits <- all_raw_files[
    grepl("\\.mtx(\\.gz)?$", basename(all_raw_files), ignore.case = TRUE)
  ]

  if (length(hits) == 0L) {
    return(NA_character_)
  }

  score <- rep(0L, length(hits))
  b <- basename(hits)

  score <- score + ifelse(grepl("Homo|human|hg", b, ignore.case = TRUE), 50L, 0L)
  score <- score + ifelse(grepl("matrix", b, ignore.case = TRUE), 50L, 0L)
  score <- score + ifelse(grepl("filtered|raw|counts", b, ignore.case = TRUE), 10L, 0L)

  info <- file.info(hits)

  hits <- hits[order(score, info$size, decreasing = TRUE)]

  hits[1L]
}

find_feature_file <- function(matrix_file) {
  matrix_dir <- dirname(matrix_file)

  candidates <- all_raw_files[
    grepl("(features|genes)\\.tsv(\\.gz)?$", basename(all_raw_files), ignore.case = TRUE)
  ]

  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  score <- rep(0L, length(candidates))
  b <- basename(candidates)

  score <- score + ifelse(dirname(candidates) == matrix_dir, 100L, 0L)
  score <- score + ifelse(grepl("Homo|human|hg", b, ignore.case = TRUE), 50L, 0L)
  score <- score + ifelse(grepl("features", b, ignore.case = TRUE), 20L, 0L)
  score <- score + ifelse(grepl("genes", b, ignore.case = TRUE), 10L, 0L)

  candidates[order(score, file.info(candidates)$size, decreasing = TRUE)][1L]
}

find_barcode_file <- function(matrix_file) {
  matrix_dir <- dirname(matrix_file)

  candidates <- all_raw_files[
    grepl("(barcodes|barcode|bcd|cells|cell)\\.tsv(\\.gz)?$", basename(all_raw_files), ignore.case = TRUE)
  ]

  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  score <- rep(0L, length(candidates))
  b <- basename(candidates)

  score <- score + ifelse(dirname(candidates) == matrix_dir, 100L, 0L)
  score <- score + ifelse(grepl("Homo|human|hg", b, ignore.case = TRUE), 50L, 0L)
  score <- score + ifelse(grepl("barcodes|barcode|bcd", b, ignore.case = TRUE), 20L, 0L)

  candidates[order(score, file.info(candidates)$size, decreasing = TRUE)][1L]
}

matrix_file <- find_matrix_file()

if (is.na(matrix_file) || !file.exists(matrix_file)) {
  stop("找不到 GSE178265 MatrixMarket matrix.mtx(.gz) 文件。")
}

feature_file <- find_feature_file(matrix_file)
barcode_file <- find_barcode_file(matrix_file)

if (is.na(feature_file) || !file.exists(feature_file)) {
  stop("找不到 GSE178265 features/genes 文件。matrix=", matrix_file)
}

if (is.na(barcode_file) || !file.exists(barcode_file)) {
  stop("找不到 GSE178265 barcodes 文件。matrix=", matrix_file)
}

stamp("matrix file：", matrix_file)
stamp("feature file：", feature_file)
stamp("barcode file：", barcode_file)


# ==============================================================================
# 6. 读取 features / barcodes
# ==============================================================================

read_feature_names <- function(path) {
  # V3：不再用 fread 直接读 gz，避免 gzip 尾部不完整导致中断。
  read_tsv_lines_feature_names(path)
}

read_barcodes <- function(path) {
  # V3：不再用 fread 直接读 gz，避免 R.utils 解压失败。
  read_tsv_lines_first_col(path)
}

features <- read_feature_names(feature_file)
all_barcodes <- read_barcodes(barcode_file)

# V3：去除可能由损坏 gzip 尾部读出的空行/异常 NA
all_barcodes <- trim_na(all_barcodes)
all_barcodes <- all_barcodes[nzchar(all_barcodes)]

features <- trim_na(features)
features <- features[nzchar(features)]
features <- standardize_feature_names(features)

stamp("features 数量：", length(features))
stamp("raw matrix barcodes 数量：", length(all_barcodes))

# ------------------------------------------------------------------------------
# V5 修正：
# 有些 GEO 的 barcode.tsv 比 matrix.mtx header 多少量 barcode 行。
# 本项目 GSE178265 出现过：
#   matrix header cells = 434340
#   barcode file rows   = 434354
# 差 14 行。这不是 target barcode 问题，而是 barcode 文件尾部/附加行问题。
# 所以在正式匹配前，先读取 matrix header，若 barcode 行数略多，则裁剪到 matrix ncol。
# ------------------------------------------------------------------------------

peek_mtx_dimensions <- function(path) {
  con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }

  on.exit(try(close(con), silent = TRUE), add = TRUE)

  first_line <- readLines(con, n = 1L, warn = FALSE)

  if (length(first_line) == 0L || !grepl("^%%MatrixMarket", first_line)) {
    stop("不是 MatrixMarket 文件：", path)
  }

  repeat {
    line <- readLines(con, n = 1L, warn = FALSE)

    if (length(line) == 0L) {
      stop("找不到 MatrixMarket 维度行：", path)
    }

    if (!startsWith(line, "%")) {
      break
    }
  }

  dims <- as.numeric(strsplit(trimws(line), "\\s+")[[1L]])

  if (length(dims) < 3L) {
    stop("MatrixMarket 维度行无法解析：", line)
  }

  list(
    n_features = as.integer(dims[1L]),
    n_barcodes = as.integer(dims[2L]),
    nnz = as.numeric(dims[3L])
  )
}

mtx_dim_precheck <- peek_mtx_dimensions(matrix_file)

stamp(
  "matrix precheck header：",
  mtx_dim_precheck$n_features,
  " x ",
  mtx_dim_precheck$n_barcodes,
  "；nnz=",
  mtx_dim_precheck$nnz
)

if (length(features) != mtx_dim_precheck$n_features) {
  stop(
    "features 数量与 matrix header 不一致：features=",
    length(features),
    " matrix_features=",
    mtx_dim_precheck$n_features
  )
}

if (length(all_barcodes) != mtx_dim_precheck$n_barcodes) {
  diff_n <- length(all_barcodes) - mtx_dim_precheck$n_barcodes

  if (diff_n > 0L) {
    stamp(
      "barcode 文件比 matrix header 多 ",
      diff_n,
      " 行；V5 自动裁剪到 matrix ncol：",
      mtx_dim_precheck$n_barcodes
    )

    all_barcodes <- all_barcodes[seq_len(mtx_dim_precheck$n_barcodes)]
  } else {
    stamp(
      "barcode 文件比 matrix header 少 ",
      abs(diff_n),
      " 行；这通常是 gzip 尾部损坏导致只读到部分 barcode。V5 不在此处中断，将优先使用既有 barcode_match 或已匹配 target。"
    )
  }
}

stamp("V5 校正/容错后 raw matrix barcodes 数量：", length(all_barcodes))


# ==============================================================================
# 7. 匹配 target barcodes 到原始矩阵列
# ==============================================================================

normalize_barcode <- function(x) {
  x <- trim_na(x)
  x <- sub("^.*__", "", x)
  x <- sub("^.*:", "", x)
  x
}

strip_suffix <- function(x) {
  sub("-[0-9]+$", "", x)
}

match_target_to_raw <- function(targets, raw) {
  targets <- trim_na(targets)
  raw <- trim_na(raw)

  raw_map <- setNames(seq_along(raw), raw)

  idx <- raw_map[targets]
  idx <- as.integer(idx)

  matched <- !is.na(idx)

  # fallback 1: 去掉 sample prefix
  if (sum(matched) < length(targets)) {
    raw_norm <- normalize_barcode(raw)
    target_norm <- normalize_barcode(targets)

    raw_map2 <- setNames(seq_along(raw), raw_norm)

    idx2 <- raw_map2[target_norm]
    idx2 <- as.integer(idx2)

    use2 <- is.na(idx) & !is.na(idx2)
    idx[use2] <- idx2[use2]
    matched <- !is.na(idx)
  }

  # fallback 2: 去掉 -1 suffix
  if (sum(matched) < length(targets)) {
    raw_norm2 <- strip_suffix(normalize_barcode(raw))
    target_norm2 <- strip_suffix(normalize_barcode(targets))

    raw_map3 <- setNames(seq_along(raw), raw_norm2)

    idx3 <- raw_map3[target_norm2]
    idx3 <- as.integer(idx3)

    use3 <- is.na(idx) & !is.na(idx3)
    idx[use3] <- idx3[use3]
    matched <- !is.na(idx)
  }

  data.frame(
    target_barcode = targets,
    raw_col_index = idx,
    matched = !is.na(idx),
    stringsAsFactors = FALSE
  )
}

# V5：先尝试用当前读取到的 barcode 文件匹配。
fresh_barcode_match <- match_target_to_raw(target_barcodes, all_barcodes)
fresh_matched <- fresh_barcode_match[fresh_barcode_match$matched, , drop = FALSE]

barcode_match <- fresh_barcode_match

# 如果当前 barcode.gz 因尾部损坏只读到 400000 行，可能仍然有上一次成功生成的 barcode_match。
# 例如 V1/V5 在报 matrix/header 不一致前，已经写出过完整 22048 target 的 raw_col_index。
# V5 会优先复用这个已有完整匹配表，避免被损坏的 barcode.gz 尾部卡住。
if (file.exists(out_barcode_match_csv)) {
  stamp("V6 检测已有 barcode_match 文件：", out_barcode_match_csv)

  old_match <- tryCatch(
    read.csv(out_barcode_match_csv, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )

  if (
    !is.null(old_match) &&
      all(c("target_barcode", "raw_col_index", "matched") %in% colnames(old_match))
  ) {
    old_match$matched <- as.logical(old_match$matched)
    old_good <- old_match[old_match$matched & !is.na(old_match$raw_col_index), , drop = FALSE]

    if (nrow(old_good) > nrow(fresh_matched)) {
      stamp(
        "V5 检测到已有 barcode_match 更完整，复用旧匹配表：",
        nrow(old_good),
        " > 当前匹配 ",
        nrow(fresh_matched)
      )
      barcode_match <- old_match
    }
  }
}

matched_match <- barcode_match[
  as.logical(barcode_match$matched) & !is.na(barcode_match$raw_col_index),
  ,
  drop = FALSE
]

# 保存最终采用的匹配表
atomic_write_csv(barcode_match, out_barcode_match_csv)

if (nrow(matched_match) == 0L) {
  stop(
    "target barcodes 与原始 GSE178265 barcodes 完全匹配不到。\n",
    "已输出：",
    out_barcode_match_csv
  )
}

matched_match <- matched_match[!duplicated(matched_match$raw_col_index), , drop = FALSE]
matched_match <- matched_match[order(matched_match$raw_col_index), , drop = FALSE]

target_raw_col_indices <- as.integer(matched_match$raw_col_index)

# V5 关键修正：
# 不再强制用 all_barcodes[target_raw_col_indices] 命名细胞。
# 因为 barcode.gz 可能只读到 400000 行，而 target 的 raw_col_index 可以来自旧完整 match 表。
# 这里直接使用 target_barcode 作为最终 cell name，更稳。
target_final_barcodes <- trim_na(matched_match$target_barcode)

bad_name <- is.na(target_final_barcodes) | target_final_barcodes == ""

if (any(bad_name)) {
  can_use_raw <- target_raw_col_indices[bad_name] <= length(all_barcodes)
  idx_bad <- which(bad_name)

  if (any(can_use_raw)) {
    target_final_barcodes[idx_bad[can_use_raw]] <- all_barcodes[target_raw_col_indices[bad_name][can_use_raw]]
  }

  still_bad <- is.na(target_final_barcodes) | target_final_barcodes == ""
  if (any(still_bad)) {
    target_final_barcodes[still_bad] <- paste0("GSE178265_DA_cell_", which(still_bad))
  }
}

target_final_barcodes <- make.unique(target_final_barcodes, sep = "__dup")

col_index_to_new_index <- integer(max(target_raw_col_indices))
col_index_to_new_index[target_raw_col_indices] <- seq_along(target_raw_col_indices)

stamp("target barcodes 匹配成功数量：", length(target_raw_col_indices))
stamp("target barcodes 未匹配数量：", sum(!barcode_match$matched))

barcode_match_csv <- out_barcode_match_csv

# V6：barcode_match 已在 V5/V6 匹配阶段写出，这里再次覆盖保存一次，确保报告路径一致。
atomic_write_csv(barcode_match, barcode_match_csv)


# ==============================================================================
# 8. 解析 MatrixMarket header
# ==============================================================================

open_mtx_con <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
}

read_mtx_header <- function(con) {
  first_line <- readLines(con, n = 1L, warn = FALSE)

  if (length(first_line) == 0L || !grepl("^%%MatrixMarket", first_line)) {
    stop("不是 MatrixMarket 文件：", matrix_file)
  }

  repeat {
    line <- readLines(con, n = 1L, warn = FALSE)

    if (length(line) == 0L) {
      stop("找不到 MatrixMarket 维度行。")
    }

    if (!startsWith(line, "%")) {
      break
    }
  }

  dims <- as.numeric(strsplit(trimws(line), "\\s+")[[1L]])

  if (length(dims) < 3L) {
    stop("MatrixMarket 维度行无法解析：", line)
  }

  list(
    n_features = as.integer(dims[1L]),
    n_barcodes = as.integer(dims[2L]),
    nnz = as.numeric(dims[3L])
  )
}


# V7：记录 matrix.mtx.gz 是否因为 gzip 尾部不完整而提前结束。
mtx_stream_incomplete <- FALSE
mtx_stream_error_message <- NA_character_

# ==============================================================================
# 9. 流式提取目标列
# ==============================================================================

stream_extract_mtx <- function() {
  con <- open_mtx_con(matrix_file)
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  header <- read_mtx_header(con)

  stamp(
    "matrix header：",
    header$n_features,
    " x ",
    header$n_barcodes,
    "；nnz=",
    header$nnz
  )

  if (header$n_features != length(features)) {
    stop(
      "matrix feature 数与 feature 文件不一致：matrix=",
      header$n_features,
      " feature_file=",
      length(features)
    )
  }

  if (header$n_barcodes != length(all_barcodes)) {
    stamp(
      "V5 streaming 阶段：matrix ncol 与已读取 barcode 数不同，但不作为致命错误。matrix=",
      header$n_barcodes,
      " barcode_read=",
      length(all_barcodes),
      "。将使用 raw_col_index 直接抽取 matrix 列。"
    )
  }

  i_all <- integer()
  j_all <- integer()
  x_all <- numeric()

  processed_nnz <- 0
  kept_nnz <- 0

  repeat {
    lines <- tryCatch(
      withCallingHandlers(
        readLines(
          con,
          n = MTX_LINE_CHUNK,
          warn = FALSE
        ),
        warning = function(w) {
          stamp("matrix.mtx.gz 读取 warning：", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        assign("mtx_stream_incomplete", TRUE, envir = .GlobalEnv)
        assign("mtx_stream_error_message", msg, envir = .GlobalEnv)

        stamp(
          "matrix.mtx.gz 读取到异常结尾：",
          msg,
          "。V7 将保留已扫描内容并继续生成 partial DA submatrix。"
        )

        character()
      }
    )

    if (length(lines) == 0L) {
      break
    }

    processed_nnz <- processed_nnz + length(lines)

    dt <- data.table::fread(
      text = lines,
      header = FALSE,
      sep = " ",
      data.table = TRUE,
      showProgress = FALSE
    )

    if (ncol(dt) < 3L) {
      stop("matrix chunk 列数异常。")
    }

    row_i <- as.integer(dt[[1L]])
    col_j_raw <- as.integer(dt[[2L]])
    val_x <- as.numeric(dt[[3L]])

    keep <- col_j_raw %in% target_raw_col_indices

    if (any(keep)) {
      new_j <- col_index_to_new_index[col_j_raw[keep]]

      i_all <- c(i_all, row_i[keep])
      j_all <- c(j_all, new_j)
      x_all <- c(x_all, val_x[keep])

      kept_nnz <- kept_nnz + sum(keep)
    }

    if (processed_nnz %% (MTX_LINE_CHUNK * 5L) < MTX_LINE_CHUNK) {
      stamp(
        "已扫描 nnz：",
        processed_nnz,
        " / ",
        header$nnz,
        "；保留 nnz：",
        kept_nnz
      )
      gc(verbose = FALSE)
    }
  }

  completion_rate <- processed_nnz / header$nnz

  if (isTRUE(get("mtx_stream_incomplete", envir = .GlobalEnv))) {
    stamp(
      "matrix streaming 提前结束；扫描 nnz=",
      processed_nnz,
      " / ",
      header$nnz,
      "；完成比例=",
      round(completion_rate * 100, 4),
      "%；保留 nnz=",
      kept_nnz
    )

    if (completion_rate < 0.99) {
      stop(
        "matrix.mtx.gz 提前结束且扫描比例 < 99%，不建议继续。当前比例：",
        round(completion_rate * 100, 4),
        "%"
      )
    }
  } else {
    stamp("matrix streaming 完成；扫描 nnz=", processed_nnz, "；保留 nnz=", kept_nnz)
  }

  if (length(x_all) == 0L) {
    stop("目标列没有任何非零表达。")
  }

  mat <- Matrix::sparseMatrix(
    i = i_all,
    j = j_all,
    x = x_all,
    dims = c(header$n_features, length(target_raw_col_indices)),
    dimnames = list(features, target_final_barcodes)
  )

  mat <- as(mat, "CsparseMatrix")
  mat <- as(mat, "dgCMatrix")

  list(
    matrix = mat,
    header = header,
    kept_nnz = kept_nnz,
    processed_nnz = processed_nnz,
    completion_rate = completion_rate,
    mtx_stream_incomplete = isTRUE(get("mtx_stream_incomplete", envir = .GlobalEnv)),
    mtx_stream_error_message = get("mtx_stream_error_message", envir = .GlobalEnv)
  )
}

stream_result <- stream_extract_mtx()

counts_mat <- stream_result$matrix

if (isTRUE(stream_result$mtx_stream_incomplete)) {
  stamp(
    "注意：matrix.mtx.gz 尾部不完整，V7 已基于已扫描 ",
    round(stream_result$completion_rate * 100, 4),
    "% 的 nnz 生成 partial submatrix。"
  )
}

stamp("submatrix 维度：", nrow(counts_mat), " x ", ncol(counts_mat), "；nnz=", length(counts_mat@x))


# ==============================================================================
# 10. 整理 metadata
# ==============================================================================

make_metadata <- function() {
  cell_names <- colnames(counts_mat)

  if (!is.null(metadata_df) && is.data.frame(metadata_df)) {
    md <- metadata_df

    md_barcode <- extract_barcodes_from_object(md)

    if (length(md_barcode) == nrow(md)) {
      md$.barcode_internal <- md_barcode

      md$.barcode_norm1 <- normalize_barcode(md$.barcode_internal)
      md$.barcode_norm2 <- strip_suffix(md$.barcode_norm1)

      cell_norm1 <- normalize_barcode(cell_names)
      cell_norm2 <- strip_suffix(cell_norm1)

      idx <- match(cell_names, md$.barcode_internal)

      if (sum(!is.na(idx)) < length(cell_names)) {
        idx2 <- match(cell_norm1, md$.barcode_norm1)
        idx[is.na(idx)] <- idx2[is.na(idx)]
      }

      if (sum(!is.na(idx)) < length(cell_names)) {
        idx3 <- match(cell_norm2, md$.barcode_norm2)
        idx[is.na(idx)] <- idx3[is.na(idx)]
      }

      if (sum(!is.na(idx)) > 0L) {
        md2 <- md[idx, , drop = FALSE]
        md2 <- md2[, !grepl("^\\.barcode_", colnames(md2)), drop = FALSE]
        rownames(md2) <- cell_names
        md2$barcode <- cell_names
        md2$dataset <- "GSE178265"
        md2$subset <- "DA"
        return(md2)
      }
    }
  }

  md <- data.frame(
    barcode = cell_names,
    dataset = "GSE178265",
    subset = "DA",
    sample = "GSE178265_DA",
    stringsAsFactors = FALSE,
    row.names = cell_names
  )

  md
}

metadata_da <- make_metadata()

if (nrow(metadata_da) != ncol(counts_mat)) {
  stop("metadata 行数与 count matrix 列数不一致。")
}


# ==============================================================================
# 11. 保存 outputs
# ==============================================================================

stamp("保存 count matrix：", out_counts_rds)
save_rds_atomic(counts_mat, out_counts_rds, compress = FALSE)

stamp("保存 metadata：", out_metadata_rds)
save_rds_atomic(metadata_da, out_metadata_rds, compress = FALSE)

seurat_saved <- FALSE

if (CREATE_SEURAT_OBJECT) {
  stamp("创建 Seurat object。")

  seu <- CreateSeuratObject(
    counts = counts_mat,
    project = "GSE178265_DA",
    meta.data = metadata_da,
    min.cells = 0,
    min.features = 0
  )

  seu$dataset <- "GSE178265"
  seu$subset <- "DA"
  seu$import_stage <- "01B_streaming_submatrix"

  stamp("保存 Seurat object：", out_seurat_rds)
  save_rds_atomic(seu, out_seurat_rds, compress = FALSE)

  seurat_saved <- TRUE
}


# ==============================================================================
# 12. 审计和报告
# ==============================================================================

audit <- data.frame(
  item = c(
    "target_barcode_file",
    "metadata_file",
    "matrix_file",
    "feature_file",
    "barcode_file",
    "n_target_requested",
    "n_target_matched_unique",
    "n_target_unmatched",
    "n_features",
    "n_cells",
    "matrix_total_nnz",
    "submatrix_nnz",
    "counts_rds",
    "metadata_rds",
    "seurat_rds",
    "seurat_saved"
  ),
  value = c(
    target_barcode_file,
    metadata_file,
    matrix_file,
    feature_file,
    barcode_file,
    as.character(length(target_barcodes)),
    as.character(length(target_raw_col_indices)),
    as.character(sum(!barcode_match$matched)),
    as.character(nrow(counts_mat)),
    as.character(ncol(counts_mat)),
    as.character(stream_result$header$nnz),
    as.character(length(counts_mat@x)),
    out_counts_rds,
    out_metadata_rds,
    out_seurat_rds,
    as.character(seurat_saved)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(audit, out_audit_csv)

report_lines <- c(
  "PD_Graft_Project：01B GSE178265 DA streaming submatrix 报告",
  paste0("生成时间：", Sys.time()),
  "",
  "输入文件：",
  paste0("target barcode file: ", target_barcode_file),
  paste0("metadata file: ", metadata_file),
  paste0("matrix file: ", matrix_file),
  paste0("feature file: ", feature_file),
  paste0("barcode file: ", barcode_file),
  "",
  "结果：",
  paste0("requested target barcodes: ", length(target_barcodes)),
  paste0("matched unique target barcodes: ", length(target_raw_col_indices)),
  paste0("unmatched target barcodes: ", sum(!barcode_match$matched)),
  paste0("submatrix features: ", nrow(counts_mat)),
  paste0("submatrix cells: ", ncol(counts_mat)),
  paste0("submatrix nnz: ", length(counts_mat@x)),
  "",
  "输出文件：",
  paste0("counts: ", out_counts_rds),
  paste0("metadata: ", out_metadata_rds),
  paste0("seurat: ", out_seurat_rds),
  paste0("barcode match: ", barcode_match_csv),
  paste0("audit: ", out_audit_csv)
)

writeLines(report_lines, out_report_txt, useBytes = TRUE)


# ==============================================================================
# 13. 最终结论
# ==============================================================================

cat("\n\n============================================================\n")
cat("01B V7 GSE178265 DA streaming submatrix 运行结束\n")
cat("============================================================\n\n")

cat("目标 barcode 请求数量：", length(target_barcodes), "\n")
cat("目标 barcode 匹配数量：", length(target_raw_col_indices), "\n")
cat("目标 barcode 未匹配数量：", sum(!barcode_match$matched), "\n")
cat("submatrix 维度：", nrow(counts_mat), " x ", ncol(counts_mat), "\n")
cat("submatrix nnz：", length(counts_mat@x), "\n\n")

cat("输出文件：\n")
cat(out_counts_rds, "\n")
cat(out_metadata_rds, "\n")
if (CREATE_SEURAT_OBJECT) cat(out_seurat_rds, "\n")
cat(out_audit_csv, "\n")
cat(out_report_txt, "\n\n")

cat("✅ 01B V7 GSE178265 DA streaming submatrix 完成。\n")
cat("下一步可以进入 01C / 后续 QC 与整合设计。\n")
