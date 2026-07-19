
PROJECT_ROOT <- "D:/PD_Graft_Project"

AUTO_INSTALL_CRAN <- TRUE

REBUILD_EXISTING <- FALSE

GSE157783_CHUNK_N_GENES <- 100L

MTX_LINE_CHUNK <- 500000L

options(stringsAsFactors = FALSE)
options(timeout = 7200)
options(future.globals.maxSize = 24 * 1024^3)

cat("\n============================================================\n")
cat("01A 正式生产版 V2：all-in-one dataset import and object building\n")
cat("============================================================\n\n")

if (!dir.exists(PROJECT_ROOT)) {
  stop("项目目录不存在：", PROJECT_ROOT)
}

PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = TRUE)

required_pkgs <- c("data.table", "Matrix", "Seurat", "R.utils")

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

  still_missing <- missing[
    !vapply(missing, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L))
  ]

  if (length(still_missing) > 0L) {
    stop("安装后仍缺少 R 包：", paste(still_missing, collapse = ", "))
  }

  invisible(TRUE)
}

install_if_missing(required_pkgs)

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(Seurat)
})

metadata_dir <- file.path(PROJECT_ROOT, "01_metadata")
reports_dir  <- file.path(PROJECT_ROOT, "06_reports")
objects_root <- file.path(PROJECT_ROOT, "02_objects", "01A_standardized")
tables_dir   <- file.path(PROJECT_ROOT, "03_tables", "01A_import")
temp_root    <- file.path(PROJECT_ROOT, "05_temp", "01A_formal_production")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(objects_root, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

log_file <- file.path(
  reports_dir,
  paste0("01A_formal_production_import_", run_stamp, ".log.txt")
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

overall_status_csv <- file.path(metadata_dir, "01A_overall_import_status.csv")
failure_csv        <- file.path(metadata_dir, "01A_import_failures.csv")
summary_csv        <- file.path(metadata_dir, "01A_dataset_level_summary.csv")
unified_meta_csv   <- file.path(metadata_dir, "01A_unified_sample_metadata.csv")
report_file        <- file.path(reports_dir, "01A_formal_production_import_report.txt")

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
    "\\.tar\\.gz$|\\.rds\\.gz$|\\.RDS\\.gz$|\\.rds$|\\.RDS$|\\.rda$|\\.RData$|\\.mtx\\.gz$|\\.mtx$|\\.txt\\.gz$|\\.tsv\\.gz$|\\.csv\\.gz$|\\.txt$|\\.tsv$|\\.csv$|\\.gz$",
    "",
    x,
    ignore.case = TRUE
  )
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

read_csv_safe <- function(path) {
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  )
}

atomic_write_csv <- function(df, final_path) {
  temp_path <- paste0(final_path, ".writing_", Sys.getpid(), "_", run_stamp)

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

backup_if_exists <- function(path) {
  if (!file.exists(path)) return(NA_character_)

  backup <- paste0(path, ".before_formal_01A_", run_stamp)

  ok <- file.copy(path, backup, overwrite = FALSE)

  if (!ok) {
    stop("备份失败：", path)
  }

  normalizePath(backup, winslash = "/", mustWork = TRUE)
}

save_rds_atomic <- function(object, final_path, compress = FALSE) {
  dir.create(dirname(final_path), recursive = TRUE, showWarnings = FALSE)

  temp_path <- paste0(final_path, ".writing_", Sys.getpid(), "_", run_stamp)

  saveRDS(object, temp_path, compress = compress)

  if (file.exists(final_path)) {
    unlink(final_path, force = TRUE)
  }

  if (!file.rename(temp_path, final_path)) {
    stop("无法保存 RDS：", final_path)
  }

  final_path
}

object_is_readable <- function(path) {
  if (!file.exists(path)) return(FALSE)

  ok <- tryCatch(
    {
      obj <- readRDS(path)
      valid <- TRUE
      if (inherits(obj, "Seurat")) {
        valid <- nrow(obj) > 0L && ncol(obj) > 0L
      }
      rm(obj)
      gc(verbose = FALSE)
      valid
    },
    warning = function(w) FALSE,
    error = function(e) FALSE
  )

  isTRUE(ok)
}

standardize_feature_names <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "UNKNOWN_FEATURE"
  make.unique(x, sep = "__dup")
}

standardize_cell_names <- function(x, sample_id) {
  x <- as.character(x)
  bad <- is.na(x) | x == ""

  if (any(bad)) {
    x[bad] <- paste0("cell_", seq_len(sum(bad)))
  }

  paste0(safe_name(sample_id), "__", x)
}

make_status_row <- function(
  dataset,
  sample,
  source_file,
  output_object = NA_character_,
  object = NULL,
  status,
  message,
  module = "formal_01A"
) {
  data.frame(
    dataset = dataset,
    sample = sample,
    source_file = normalize_path_text(source_file),
    output_object = normalize_path_text(output_object),
    object_class = if (is.null(object)) NA_character_ else paste(class(object), collapse = " | "),
    n_features = if (is.null(object)) NA_integer_ else tryCatch(nrow(object), error = function(e) tryCatch(dim(object)[1L], error = function(e2) NA_integer_)),
    n_cells = if (is.null(object)) NA_integer_ else tryCatch(ncol(object), error = function(e) tryCatch(dim(object)[2L], error = function(e2) NA_integer_)),
    module = module,
    status = status,
    message = message,
    stringsAsFactors = FALSE
  )
}

add_basic_metadata <- function(seu, dataset, sample, extra = list()) {
  n <- ncol(seu)

  meta <- c(
    list(
      dataset = dataset,
      sample = sample,
      project = "PD_Graft_Project",
      import_stage = "01A_formal_production"
    ),
    extra
  )

  for (nm in names(meta)) {
    val <- meta[[nm]]
    if (length(val) == 1L) val <- rep(val, n)
    if (length(val) != n) val <- rep(NA_character_, n)
    seu[[nm]] <- val
  }

  seu
}

copy_binary <- function(from, to) {
  if (!file.exists(from)) return(invisible(FALSE))

  in_con <- file(from, "rb")
  out_con <- file(to, "wb")

  on.exit({
    try(close(in_con), silent = TRUE)
    try(close(out_con), silent = TRUE)
  }, add = TRUE)

  repeat {
    buf <- readBin(in_con, what = "raw", n = 1024 * 1024)
    if (length(buf) == 0L) break
    writeBin(buf, out_con)
  }

  invisible(TRUE)
}

write_tsv_gz <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  con <- gzfile(path, open = "wt")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  write.table(
    df,
    file = con,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )

  invisible(path)
}

guess_sep <- function(path) {
  if (grepl("\\.csv(\\.gz)?$", path, ignore.case = TRUE)) "," else "\t"
}

open_text <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
}

read_mtx_header_path <- function(path) {
  con <- open_text(path)
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

    if (!startsWith(line, "%")) break
  }

  vals <- as.numeric(strsplit(trimws(line), "\\s+")[[1L]])

  if (length(vals) < 3L) {
    stop("MatrixMarket 维度行异常：", line)
  }

  c(
    n_features = as.integer(vals[1L]),
    n_cells = as.integer(vals[2L]),
    nnz = as.numeric(vals[3L])
  )
}

read_tsv_no_header <- function(path) {
  data.table::fread(
    path,
    sep = "\t",
    header = FALSE,
    quote = "",
    data.table = TRUE,
    showProgress = FALSE
  )
}

read_feature_names <- function(path) {
  dt <- read_tsv_no_header(path)

  if (ncol(dt) < 1L) {
    stop("features/genes 文件为空：", path)
  }

  col_use <- if (ncol(dt) >= 2L) 2L else 1L

  genes <- trim_na(dt[[col_use]])
  genes[!nzchar(genes)] <- paste0("feature_", which(!nzchar(genes)))

  standardize_feature_names(genes)
}

read_first_column <- function(path) {
  dt <- read_tsv_no_header(path)

  if (ncol(dt) < 1L) {
    stop("文件为空：", path)
  }

  trim_na(dt[[1L]])
}

sample_id_from_mtx <- function(path) {
  x <- basename(path)

  x <- sub("\\.mtx\\.recovered\\.mtx$", "", x, ignore.case = TRUE)
  x <- sub("_?matrix\\.mtx(\\.gz)?$", "", x, ignore.case = TRUE)
  x <- sub("matrix\\.mtx(\\.gz)?$", "", x, ignore.case = TRUE)
  x <- sub("\\.mtx(\\.gz)?$", "", x, ignore.case = TRUE)
  x <- sub("_?matrix$", "", x, ignore.case = TRUE)

  safe_name(x)
}

find_10x_files_for_dataset <- function(dataset_dir) {
  mtx_files <- list.files(
    dataset_dir,
    pattern = "\\.mtx(\\.gz)?$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  mtx_files <- normalizePath(mtx_files, winslash = "/", mustWork = FALSE)
  mtx_files <- mtx_files[file.exists(mtx_files)]

  if (length(mtx_files) == 0L) {
    return(data.frame())
  }

  sample_ids <- vapply(mtx_files, sample_id_from_mtx, character(1L))

  df <- data.frame(
    sample = sample_ids,
    matrix_file = mtx_files,
    stringsAsFactors = FALSE
  )

  score <- rep(0L, nrow(df))
  score <- score + ifelse(grepl("recovered|recover|salvage", df$matrix_file, ignore.case = TRUE), 100L, 0L)
  score <- score + ifelse(!grepl("\\.gz$", df$matrix_file, ignore.case = TRUE), 10L, 0L)

  df$score <- score
  df$size <- as.numeric(file.info(df$matrix_file)$size)

  df <- df[order(df$sample, -df$score, -df$size), ]

  df <- df[!duplicated(df$sample), , drop = FALSE]

  df[, c("sample", "matrix_file"), drop = FALSE]
}

find_companion_file <- function(dataset_dir, matrix_file, sample_id, type = c("features", "barcodes")) {
  type <- match.arg(type)

  matrix_dir <- dirname(matrix_file)

  all_files <- list.files(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )

  all_files <- normalizePath(all_files, winslash = "/", mustWork = FALSE)
  all_files <- all_files[file.exists(all_files)]

  base <- sample_id

  if (type == "features") {
    local_candidates <- c(
      file.path(matrix_dir, paste0(base, "_features.tsv.gz")),
      file.path(matrix_dir, paste0(base, "features.tsv.gz")),
      file.path(matrix_dir, paste0(base, "_genes.tsv.gz")),
      file.path(matrix_dir, paste0(base, "genes.tsv.gz")),
      file.path(matrix_dir, "features.tsv.gz"),
      file.path(matrix_dir, "genes.tsv.gz")
    )

    pat <- "(features|genes)\\.tsv(\\.gz)?$"
  } else {
    local_candidates <- c(
      file.path(matrix_dir, paste0(base, "_barcodes.tsv.gz")),
      file.path(matrix_dir, paste0(base, "barcodes.tsv.gz")),
      file.path(matrix_dir, paste0(base, "_barcode.tsv.gz")),
      file.path(matrix_dir, paste0(base, "barcode.tsv.gz")),
      file.path(matrix_dir, paste0(base, "_bcd.tsv.gz")),
      file.path(matrix_dir, paste0(base, "bcd.tsv.gz")),
      file.path(matrix_dir, "barcodes.tsv.gz"),
      file.path(matrix_dir, "barcode.tsv.gz"),
      file.path(matrix_dir, "bcd.tsv.gz")
    )

    pat <- "(barcodes|barcode|bcd|cells|cell)\\.tsv(\\.gz)?$"
  }

  local_candidates <- normalizePath(local_candidates, winslash = "/", mustWork = FALSE)
  local_hit <- local_candidates[file.exists(local_candidates)]

  if (length(local_hit) > 0L) {
    return(local_hit[1L])
  }

  hits <- all_files[
    grepl(pat, basename(all_files), ignore.case = TRUE)
  ]

  if (length(hits) == 0L) {
    return(NA_character_)
  }

  score <- rep(0L, length(hits))

  score <- score + ifelse(dirname(hits) == matrix_dir, 100L, 0L)
  score <- score + ifelse(grepl(sample_id, basename(hits), ignore.case = TRUE), 80L, 0L)
  score <- score + ifelse(grepl("features|barcodes", basename(hits), ignore.case = TRUE), 10L, 0L)

  hits[order(score, file.info(hits)$size, decreasing = TRUE)][1L]
}

read_feature_table_maybe <- function(path) {
  tryCatch(
    read_tsv_no_header(path),
    error = function(e) NULL
  )
}

find_feature_template <- function(dataset_dir, target_n_features) {
  feature_files <- list.files(
    dataset_dir,
    pattern = "(features|genes)\\.tsv(\\.gz)?$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  feature_files <- normalizePath(feature_files, winslash = "/", mustWork = FALSE)
  feature_files <- feature_files[file.exists(feature_files)]

  if (length(feature_files) == 0L) {
    return(NULL)
  }

  checks <- lapply(
    feature_files,
    function(p) {
      dt <- tryCatch(read_tsv_no_header(p), error = function(e) NULL)

      if (is.null(dt)) {
        return(
          data.frame(
            path = p,
            readable = FALSE,
            nrow = NA_integer_,
            ncol = NA_integer_,
            stringsAsFactors = FALSE
          )
        )
      }

      data.frame(
        path = p,
        readable = TRUE,
        nrow = nrow(dt),
        ncol = ncol(dt),
        stringsAsFactors = FALSE
      )
    }
  )

  checks <- do.call(rbind, checks)

  valid <- checks[
    checks$readable & checks$nrow == target_n_features,
    ,
    drop = FALSE
  ]

  if (nrow(valid) == 0L) {
    return(NULL)
  }

  valid <- valid[order(valid$ncol, decreasing = TRUE), ]

  read_tsv_no_header(valid$path[1L])
}

repair_or_validate_features <- function(dataset_dir, matrix_file, sample_id, feature_file, target_n_features) {
  feature_ok <- FALSE

  if (!is.na(feature_file) && file.exists(feature_file)) {
    feature_ok <- tryCatch(
      {
        dt <- read_tsv_no_header(feature_file)
        nrow(dt) == target_n_features
      },
      error = function(e) FALSE
    )
  }

  if (feature_ok) {
    return(feature_file)
  }

  template <- find_feature_template(dataset_dir, target_n_features)

  if (is.null(template)) {
    stop("features 文件不可读/行数不对，且找不到可用模板。sample=", sample_id)
  }

  if (is.na(feature_file) || !nzchar(feature_file)) {
    feature_file <- file.path(dirname(matrix_file), paste0(sample_id, "_features.tsv.gz"))
  }

  feature_file <- normalizePath(feature_file, winslash = "/", mustWork = FALSE)

  if (file.exists(feature_file)) {
    backup_path <- paste0(feature_file, ".corrupt_before_formal_01A_", run_stamp)
    copy_binary(feature_file, backup_path)
  }

  write_tsv_gz(template, feature_file)

  stamp("已自动修复 features：", feature_file)

  feature_file
}

import_one_10x_sample <- function(dataset_id, dataset_dir, sample_id, matrix_file) {
  out_dir <- file.path(objects_root, dataset_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  output_path <- file.path(
    out_dir,
    paste0("01A_", dataset_id, "_", safe_name(sample_id), ".rds")
  )

  if (file.exists(output_path) && !REBUILD_EXISTING && object_is_readable(output_path)) {
    obj <- readRDS(output_path)

    return(
      make_status_row(
        dataset = dataset_id,
        sample = sample_id,
        source_file = matrix_file,
        output_object = output_path,
        object = obj,
        status = "SKIPPED_EXISTING",
        message = "existing readable Seurat object",
        module = "10x_matrix_import"
      )
    )
  }

  tryCatch(
    {
      dims <- read_mtx_header_path(matrix_file)

      feature_file <- find_companion_file(dataset_dir, matrix_file, sample_id, "features")
      barcode_file <- find_companion_file(dataset_dir, matrix_file, sample_id, "barcodes")

      feature_file <- repair_or_validate_features(
        dataset_dir = dataset_dir,
        matrix_file = matrix_file,
        sample_id = sample_id,
        feature_file = feature_file,
        target_n_features = dims["n_features"]
      )

      features <- read_feature_names(feature_file)

      barcodes <- tryCatch(
        {
          if (is.na(barcode_file) || !file.exists(barcode_file)) {
            stop("barcode file missing")
          }
          read_first_column(barcode_file)
        },
        error = function(e) {
          stamp("barcodes 读取失败，使用 synthetic barcodes。sample=", sample_id, "；", conditionMessage(e))
          paste0("synthetic_barcode_", seq_len(dims["n_cells"]))
        }
      )

      if (length(features) != dims["n_features"]) {
        stop("features 行数与 matrix 行数不一致。")
      }

      if (length(barcodes) != dims["n_cells"]) {
        stamp("barcodes 数量不匹配，改用 synthetic barcodes。sample=", sample_id)
        barcodes <- paste0("synthetic_barcode_", seq_len(dims["n_cells"]))
      }

      stamp("读取 matrix：", matrix_file)

      mat <- suppressWarnings(Matrix::readMM(matrix_file))

      mat <- as(mat, "CsparseMatrix")
      mat <- as(mat, "dgCMatrix")

      rownames(mat) <- standardize_feature_names(features)
      colnames(mat) <- standardize_cell_names(barcodes, sample_id)

      seu <- CreateSeuratObject(
        counts = mat,
        project = dataset_id,
        min.cells = 0,
        min.features = 0
      )

      seu <- add_basic_metadata(
        seu,
        dataset = dataset_id,
        sample = sample_id,
        extra = list(
          source_note = "formal 01A 10x MatrixMarket import",
          matrix_file = matrix_file,
          feature_file = feature_file,
          barcode_file = ifelse(is.na(barcode_file), "", barcode_file)
        )
      )

      save_rds_atomic(seu, output_path, compress = FALSE)

      make_status_row(
        dataset = dataset_id,
        sample = sample_id,
        source_file = matrix_file,
        output_object = output_path,
        object = seu,
        status = "IMPORTED",
        message = "10x MatrixMarket imported",
        module = "10x_matrix_import"
      )
    },
    error = function(e) {
      make_status_row(
        dataset = dataset_id,
        sample = sample_id,
        source_file = matrix_file,
        output_object = output_path,
        object = NULL,
        status = "FAILED",
        message = conditionMessage(e),
        module = "10x_matrix_import"
      )
    }
  )
}

import_10x_dataset <- function(dataset_id, dataset_dir) {
  mtx_df <- find_10x_files_for_dataset(dataset_dir)

  if (nrow(mtx_df) == 0L) {
    return(list())
  }

  stamp(dataset_id, " 检测到 10x/MatrixMarket 样本数：", nrow(mtx_df))

  out <- vector("list", nrow(mtx_df))

  for (i in seq_len(nrow(mtx_df))) {
    stamp(dataset_id, " 10x导入：", i, "/", nrow(mtx_df), "；sample=", mtx_df$sample[i])

    out[[i]] <- import_one_10x_sample(
      dataset_id = dataset_id,
      dataset_dir = dataset_dir,
      sample_id = mtx_df$sample[i],
      matrix_file = mtx_df$matrix_file[i]
    )

    gc(verbose = FALSE)
  }

  out
}

first_bytes_hex <- function(path, n = 32L) {
  if (!file.exists(path)) return("FILE_NOT_FOUND")

  con <- NULL

  out <- tryCatch(
    {
      con <- file(path, "rb")
      raw <- readBin(con, what = "raw", n = n)
      paste(toupper(format(raw)), collapse = " ")
    },
    error = function(e) paste0("ERROR: ", conditionMessage(e)),
    finally = {
      if (!is.null(con)) try(close(con), silent = TRUE)
    }
  )

  out
}

try_read_object_once <- function(path) {
  errors <- character()

  obj <- tryCatch(
    readRDS(path),
    error = function(e) {
      errors <<- c(errors, paste0("readRDS(path): ", conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(obj)) {
    return(list(object = obj, method = "readRDS(path)", errors = errors))
  }

  obj <- tryCatch(
    {
      con <- gzfile(path, open = "rb")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      readRDS(con)
    },
    error = function(e) {
      errors <<- c(errors, paste0("readRDS(gzfile): ", conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(obj)) {
    return(list(object = obj, method = "readRDS(gzfile)", errors = errors))
  }

  env <- new.env(parent = emptyenv())

  loaded_names <- tryCatch(
    load(path, envir = env),
    error = function(e) {
      errors <<- c(errors, paste0("load(path): ", conditionMessage(e)))
      character()
    }
  )

  if (length(loaded_names) > 0L) {
    objs <- mget(loaded_names, envir = env, inherits = FALSE)
    return(list(object = objs, method = paste0("load(path): ", paste(loaded_names, collapse = ",")), errors = errors))
  }

  env <- new.env(parent = emptyenv())

  loaded_names <- tryCatch(
    {
      con <- gzfile(path, open = "rb")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      load(con, envir = env)
    },
    error = function(e) {
      errors <<- c(errors, paste0("load(gzfile): ", conditionMessage(e)))
      character()
    }
  )

  if (length(loaded_names) > 0L) {
    objs <- mget(loaded_names, envir = env, inherits = FALSE)
    return(list(object = objs, method = paste0("load(gzfile): ", paste(loaded_names, collapse = ",")), errors = errors))
  }

  list(object = NULL, method = NA_character_, errors = errors)
}

stream_gunzip_one_layer <- function(src, dest) {
  warnings_seen <- character()
  errors_seen <- character()

  in_con <- NULL
  out_con <- NULL
  ok <- TRUE

  tryCatch(
    withCallingHandlers(
      {
        in_con <- gzfile(src, "rb")
        out_con <- file(dest, "wb")

        repeat {
          buf <- readBin(in_con, what = "raw", n = 1024 * 1024)
          if (length(buf) == 0L) break
          writeBin(buf, out_con)
        }
      },
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      errors_seen <<- c(errors_seen, conditionMessage(e))
      ok <<- FALSE
    },
    finally = {
      if (!is.null(in_con)) try(close(in_con), silent = TRUE)
      if (!is.null(out_con)) try(close(out_con), silent = TRUE)
    }
  )

  file_ok <- file.exists(dest) && file.info(dest)$size > 0

  list(
    ok = isTRUE(ok) && file_ok,
    file_ok = file_ok,
    warnings = warnings_seen,
    errors = errors_seen,
    dest = dest,
    size = if (file_ok) file.info(dest)$size else NA_real_
  )
}

peel_and_read_object <- function(path, max_layers = 8L) {
  work_dir <- file.path(temp_root, paste0("peel_", safe_name(basename(path)), "_", run_stamp))
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  current <- path
  all_errors <- character()
  layer_info <- list()

  for (layer in 0:max_layers) {
    magic <- first_bytes_hex(current)

    read_try <- try_read_object_once(current)

    layer_info[[length(layer_info) + 1L]] <- data.frame(
      layer = layer,
      path = current,
      size_bytes = if (file.exists(current)) file.info(current)$size else NA_real_,
      first_bytes = magic,
      read_method = ifelse(is.na(read_try$method), "", read_try$method),
      errors = paste(read_try$errors, collapse = " || "),
      stringsAsFactors = FALSE
    )

    if (!is.null(read_try$object)) {
      return(
        list(
          object = read_try$object,
          method = paste0("layer_", layer, ":", read_try$method),
          layer_info = do.call(rbind, layer_info),
          errors = all_errors
        )
      )
    }

    all_errors <- c(all_errors, paste0("layer_", layer, ": ", paste(read_try$errors, collapse = " || ")))

    if (!grepl("^1F 8B", magic)) {
      break
    }

    next_path <- file.path(work_dir, paste0("layer_", layer + 1L, ".bin"))

    unzip_try <- stream_gunzip_one_layer(current, next_path)

    layer_info[[length(layer_info) + 1L]] <- data.frame(
      layer = layer + 0.5,
      path = next_path,
      size_bytes = unzip_try$size,
      first_bytes = if (file.exists(next_path)) first_bytes_hex(next_path) else "NO_OUTPUT",
      read_method = "stream_gunzip_one_layer",
      errors = paste(c(unzip_try$warnings, unzip_try$errors), collapse = " || "),
      stringsAsFactors = FALSE
    )

    if (!isTRUE(unzip_try$file_ok)) {
      all_errors <- c(
        all_errors,
        paste0("gunzip layer_", layer, " failed: ", paste(c(unzip_try$warnings, unzip_try$errors), collapse = " || "))
      )
      break
    }

    current <- next_path
  }

  list(
    object = NULL,
    method = NA_character_,
    layer_info = do.call(rbind, layer_info),
    errors = all_errors
  )
}

find_seurat_in_object <- function(x) {
  if (inherits(x, "Seurat")) return(x)

  if (is.list(x)) {
    for (element in x) {
      found <- find_seurat_in_object(element)
      if (!is.null(found)) return(found)
    }
  }

  NULL
}

find_matrix_in_object <- function(x) {
  if (inherits(x, c("dgCMatrix", "matrix"))) return(x)

  if (is.data.frame(x)) {
    numeric_cols <- vapply(x, is.numeric, logical(1L))
    if (sum(numeric_cols) >= 2L && nrow(x) > 100L) {
      mat <- as.matrix(x[, numeric_cols, drop = FALSE])
      rownames(mat) <- if (!is.null(rownames(x))) rownames(x) else paste0("feature_", seq_len(nrow(mat)))
      return(mat)
    }
  }

  if (is.list(x)) {
    preferred <- c("counts", "count", "matrix", "mat", "expr", "exprs", "data", "RNA", "assay", "x", "X")

    for (nm in preferred) {
      if (nm %in% names(x)) {
        found <- find_matrix_in_object(x[[nm]])
        if (!is.null(found)) return(found)
      }
    }

    for (element in x) {
      found <- find_matrix_in_object(element)
      if (!is.null(found)) return(found)
    }
  }

  NULL
}

convert_loaded_object_to_seurat <- function(obj, dataset_id, sample_id, source_note = "") {
  seu <- find_seurat_in_object(obj)

  if (!is.null(seu)) {
    return(
      add_basic_metadata(
        seu,
        dataset = dataset_id,
        sample = sample_id,
        extra = list(source_note = source_note)
      )
    )
  }

  if (requireNamespace("SingleCellExperiment", quietly = TRUE) &&
      inherits(obj, "SingleCellExperiment")) {
    seu <- as.Seurat(obj)
    return(
      add_basic_metadata(
        seu,
        dataset = dataset_id,
        sample = sample_id,
        extra = list(source_note = paste0(source_note, "; converted from SingleCellExperiment"))
      )
    )
  }

  mat <- find_matrix_in_object(obj)

  if (!is.null(mat)) {
    mat <- as(mat, "CsparseMatrix")
    mat <- as(mat, "dgCMatrix")

    if (is.null(rownames(mat))) rownames(mat) <- paste0("feature_", seq_len(nrow(mat)))
    if (is.null(colnames(mat))) colnames(mat) <- paste0("cell_", seq_len(ncol(mat)))

    rownames(mat) <- standardize_feature_names(rownames(mat))
    colnames(mat) <- standardize_cell_names(colnames(mat), sample_id)

    seu <- CreateSeuratObject(
      counts = mat,
      project = dataset_id,
      min.cells = 0,
      min.features = 0
    )

    return(
      add_basic_metadata(
        seu,
        dataset = dataset_id,
        sample = sample_id,
        extra = list(source_note = paste0(source_note, "; matrix extracted from object"))
      )
    )
  }

  stop("无法从对象提取 Seurat 或 count matrix；class=", paste(class(obj), collapse = " | "))
}

import_rds_dataset <- function(dataset_id, dataset_dir) {
  rds_files <- list.files(
    dataset_dir,
    pattern = "\\.rds(\\.gz)?$|\\.RDS(\\.gz)?$|\\.rda$|\\.RData$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  rds_files <- normalizePath(rds_files, winslash = "/", mustWork = FALSE)
  rds_files <- rds_files[file.exists(rds_files)]

  if (length(rds_files) == 0L) {
    return(list())
  }

  stamp(dataset_id, " 检测到 RDS/RData 文件数：", length(rds_files))

  out <- vector("list", length(rds_files))
  layer_reports <- list()

  for (i in seq_along(rds_files)) {
    path <- rds_files[i]
    sample_id <- safe_name(basename(path))

    out_dir <- file.path(objects_root, dataset_id)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    output_path <- file.path(
      out_dir,
      paste0("01A_", dataset_id, "_", sample_id, ".rds")
    )

    stamp(dataset_id, " RDS导入：", i, "/", length(rds_files), "；sample=", sample_id)

    if (file.exists(output_path) && !REBUILD_EXISTING && object_is_readable(output_path)) {
      obj <- readRDS(output_path)

      out[[i]] <- make_status_row(
        dataset = dataset_id,
        sample = sample_id,
        source_file = path,
        output_object = output_path,
        object = obj,
        status = "SKIPPED_EXISTING",
        message = "existing readable object",
        module = "rds_object_import"
      )

      rm(obj)
      gc(verbose = FALSE)
      next
    }

    out[[i]] <- tryCatch(
      {
        peeled <- peel_and_read_object(path, max_layers = 8L)

        if (!is.null(peeled$layer_info) && nrow(peeled$layer_info) > 0L) {
          li <- peeled$layer_info
          li$dataset <- dataset_id
          li$sample <- sample_id
          li$source_file <- path
          layer_reports[[length(layer_reports) + 1L]] <- li
        }

        if (is.null(peeled$object)) {
          stop("无法读取对象：", paste(peeled$errors, collapse = " || "))
        }

        obj <- peeled$object

        is_bulk_dds <- any(grepl("DESeqDataSet", class(obj), ignore.case = TRUE))

        if (is_bulk_dds || dataset_id == "GSE204795") {
          save_rds_atomic(obj, output_path, compress = FALSE)

          make_status_row(
            dataset = dataset_id,
            sample = sample_id,
            source_file = path,
            output_object = output_path,
            object = obj,
            status = "IMPORTED",
            message = paste0("bulk/R object accepted; class=", paste(class(obj), collapse = " | "), "; method=", peeled$method),
            module = "bulk_or_r_object_import"
          )
        } else {
          seu <- convert_loaded_object_to_seurat(
            obj,
            dataset_id = dataset_id,
            sample_id = sample_id,
            source_note = paste0("formal 01A recursive object import; method=", peeled$method)
          )

          save_rds_atomic(seu, output_path, compress = FALSE)

          make_status_row(
            dataset = dataset_id,
            sample = sample_id,
            source_file = path,
            output_object = output_path,
            object = seu,
            status = "IMPORTED",
            message = paste0("object imported as Seurat; method=", peeled$method),
            module = "rds_object_import"
          )
        }
      },
      error = function(e) {
        make_status_row(
          dataset = dataset_id,
          sample = sample_id,
          source_file = path,
          output_object = output_path,
          object = NULL,
          status = "FAILED",
          message = conditionMessage(e),
          module = "rds_object_import"
        )
      }
    )

    gc(verbose = FALSE)
  }

  if (length(layer_reports) > 0L) {
    layer_df <- do.call(rbind, layer_reports)
    atomic_write_csv(
      layer_df,
      file.path(metadata_dir, paste0("01A_", dataset_id, "_recursive_layer_report_", run_stamp, ".csv"))
    )
  }

  out
}

find_gse157783_files <- function(dataset_dir) {
  all_files <- list.files(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )

  all_files <- normalizePath(all_files, winslash = "/", mustWork = FALSE)
  all_files <- all_files[file.exists(all_files)]

  tar_files <- all_files[
    grepl("\\.tar\\.gz$|\\.tgz$|\\.tar$", basename(all_files), ignore.case = TRUE)
  ]

  for (tar_path in tar_files) {
    exdir <- file.path(dataset_dir, "01_extracted", paste0("formal_untar_", safe_name(basename(tar_path))))

    if (!dir.exists(exdir) || length(list.files(exdir, recursive = TRUE)) == 0L) {
      dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
      stamp("解压 GSE157783 archive：", tar_path)
      try(utils::untar(tarfile = tar_path, exdir = exdir), silent = TRUE)
    }
  }

  all_files <- list.files(dataset_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  all_files <- normalizePath(all_files, winslash = "/", mustWork = FALSE)
  all_files <- all_files[file.exists(all_files)]

  cell_candidates <- all_files[
    grepl("cell", basename(all_files), ignore.case = TRUE) &
      grepl("\\.tsv(\\.gz)?$|\\.txt(\\.gz)?$|\\.csv(\\.gz)?$", basename(all_files), ignore.case = TRUE)
  ]

  gene_candidates <- all_files[
    grepl("gene", basename(all_files), ignore.case = TRUE) &
      grepl("\\.tsv(\\.gz)?$|\\.txt(\\.gz)?$|\\.csv(\\.gz)?$", basename(all_files), ignore.case = TRUE)
  ]

  umi_candidates <- all_files[
    grepl("UMI|umi|count|matrix", basename(all_files), ignore.case = TRUE) &
      grepl("\\.tsv(\\.gz)?$|\\.txt(\\.gz)?$|\\.csv(\\.gz)?$", basename(all_files), ignore.case = TRUE)
  ]

  prefer_path <- function(paths, keyword_dir) {
    if (length(paths) == 0L) return(NA_character_)

    score <- rep(0L, length(paths))
    score <- score + ifelse(grepl(keyword_dir, dirname(paths), ignore.case = TRUE), 100L, 0L)
    score <- score + ifelse(!grepl("R7_untar|R8_untar|formal_untar", paths, ignore.case = TRUE), 50L, 0L)
    score <- score + ifelse(grepl("01_extracted", paths, ignore.case = TRUE), 10L, 0L)

    paths[order(score, file.info(paths)$mtime, decreasing = TRUE)][1L]
  }

  list(
    cell = prefer_path(cell_candidates, "/cell"),
    genes = prefer_path(gene_candidates, "/genes"),
    umi = prefer_path(umi_candidates, "/UMI|/umi")
  )
}

extract_ids_from_simple_file <- function(path, kind = c("cell", "gene")) {
  kind <- match.arg(kind)

  if (is.na(path) || !file.exists(path)) {
    stop("找不到 ", kind, " 文件：", path)
  }

  dt <- data.table::fread(
    path,
    sep = guess_sep(path),
    header = FALSE,
    quote = "",
    data.table = TRUE,
    showProgress = FALSE
  )

  if (nrow(dt) == 0L || ncol(dt) == 0L) {
    stop(kind, " 文件为空：", path)
  }

  ids <- as.character(dt[[1L]])

  if (kind == "gene" && ncol(dt) >= 2L) {
    second <- as.character(dt[[2L]])
    if (mean(nzchar(trimws(second))) > 0.8) {
      ids <- second
    }
  }

  ids <- trim_na(ids)

  bad_headers <- c(
    "cell", "cells", "barcode", "barcodes", "cell_id", "cellid",
    "gene", "genes", "symbol", "feature", "features", "gene_id", "geneid"
  )

  ids <- ids[!(tolower(ids) %in% bad_headers)]
  ids <- ids[nzchar(ids)]
  ids <- make.unique(ids, sep = "__dup")

  if (length(ids) == 0L) {
    stop("无法从文件提取 ", kind, " IDs：", path)
  }

  ids
}

parse_numeric_fast <- function(x) suppressWarnings(as.numeric(x))

split_line_bytes <- function(line) {
  strsplit(line, "\t", fixed = TRUE, useBytes = TRUE)[[1L]]
}

is_header_line <- function(fields, n_cells, cells) {
  if (length(fields) == 0L) return(FALSE)

  sample_fields <- fields[seq_len(min(length(fields), 100L))]
  numeric_test <- suppressWarnings(as.numeric(sample_fields))
  non_numeric_frac <- mean(is.na(numeric_test))
  overlap_frac <- mean(sample_fields %in% cells)
  length_ok <- length(fields) %in% c(n_cells, n_cells + 1L)

  length_ok && (non_numeric_frac > 0.5 || overlap_frac > 0.1)
}

open_text_con <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rt") else file(path, "rt")
}

process_umi_line <- function(line, gene_index, n_cells) {
  fields <- split_line_bytes(line)

  if (length(fields) == n_cells + 1L) {
    values <- fields[-1L]
  } else if (length(fields) == n_cells) {
    values <- fields
  } else if (length(fields) > n_cells) {
    values <- tail(fields, n_cells)
  } else {
    stop("UMI 第 ", gene_index, " 行列数不足：", length(fields), "；期望 ", n_cells, " 或 ", n_cells + 1L)
  }

  nums <- parse_numeric_fast(values)
  nums[is.na(nums)] <- 0

  nz <- which(nums != 0)

  if (length(nz) == 0L) {
    return(list(j = integer(), x = numeric()))
  }

  list(j = as.integer(nz), x = nums[nz])
}

stream_dense_umi_to_sparse <- function(umi_path, genes, cells, chunk_n_genes = 100L, dataset_id = "GSE157783") {
  if (is.na(umi_path) || !file.exists(umi_path)) stop("找不到 UMI 文件：", umi_path)

  n_genes_expected <- length(genes)
  n_cells <- length(cells)

  chunk_dir <- file.path(temp_root, paste0("GSE157783_chunks_", run_stamp))
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)

  stamp("开始流式读取 GSE157783 UMI：", umi_path)
  stamp("目标 cells=", n_cells, "；genes file rows=", n_genes_expected)

  con <- open_text_con(umi_path)
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  first_line <- readLines(con, n = 1L, warn = FALSE, skipNul = TRUE)
  if (length(first_line) == 0L) stop("UMI 文件为空：", umi_path)

  first_fields <- split_line_bytes(first_line)
  has_header <- is_header_line(first_fields, n_cells, cells)

  if (has_header) {
    stamp("UMI 第一行识别为 header。列数=", length(first_fields))

    if (length(first_fields) == n_cells) {
      cell_names <- first_fields
    } else if (length(first_fields) == n_cells + 1L) {
      cell_names <- first_fields[-1L]
    } else {
      cell_names <- cells
    }

    if (mean(cell_names %in% cells) < 0.2) {
      cell_names <- cells
    }

    pending_first_data_line <- NULL
  } else {
    stamp("UMI 第一行识别为数据行。列数=", length(first_fields))
    cell_names <- cells
    pending_first_data_line <- first_line
  }

  cell_names <- standardize_cell_names(cell_names, "IPDCO_hg_midbrain")
  gene_names_all <- standardize_feature_names(genes)

  chunk_paths <- character()
  chunk_i <- integer()
  chunk_j <- integer()
  chunk_x <- numeric()

  chunk_start_gene <- 1L
  current_chunk_rows <- 0L
  chunk_index <- 0L
  gene_index <- 0L
  total_nnz <- 0L

  save_chunk <- function(mat_chunk, chunk_index) {
    chunk_path <- file.path(chunk_dir, sprintf("chunk_%05d.rds", chunk_index))
    saveRDS(mat_chunk, chunk_path, compress = FALSE)
    chunk_path
  }

  flush_chunk <- function() {
    if (current_chunk_rows <= 0L) return(invisible(FALSE))

    row_start <- chunk_start_gene
    row_end <- chunk_start_gene + current_chunk_rows - 1L

    dimnames_now <- list(
      gene_names_all[row_start:row_end],
      cell_names
    )

    if (length(chunk_x) == 0L) {
      mat_chunk <- Matrix::sparseMatrix(
        i = integer(),
        j = integer(),
        x = numeric(),
        dims = c(current_chunk_rows, n_cells),
        dimnames = dimnames_now
      )
    } else {
      mat_chunk <- Matrix::sparseMatrix(
        i = chunk_i,
        j = chunk_j,
        x = chunk_x,
        dims = c(current_chunk_rows, n_cells),
        dimnames = dimnames_now
      )
    }

    chunk_index <<- chunk_index + 1L
    chunk_path <- save_chunk(mat_chunk, chunk_index)

    chunk_paths <<- c(chunk_paths, chunk_path)
    total_nnz <<- total_nnz + length(chunk_x)

    rm(mat_chunk)
    gc(verbose = FALSE)

    chunk_i <<- integer()
    chunk_j <<- integer()
    chunk_x <<- numeric()
    current_chunk_rows <<- 0L
    chunk_start_gene <<- gene_index + 1L

    invisible(TRUE)
  }

  handle_line <- function(line) {
    gene_index <<- gene_index + 1L

    if (gene_index > n_genes_expected) {
      return(invisible(FALSE))
    }

    parsed <- process_umi_line(
      line = line,
      gene_index = gene_index,
      n_cells = n_cells
    )

    current_chunk_rows <<- current_chunk_rows + 1L

    if (length(parsed$x) > 0L) {
      chunk_i <<- c(chunk_i, rep.int(current_chunk_rows, length(parsed$x)))
      chunk_j <<- c(chunk_j, parsed$j)
      chunk_x <<- c(chunk_x, parsed$x)
    }

    if (current_chunk_rows >= chunk_n_genes) {
      flush_chunk()
      stamp("已处理 gene rows：", gene_index, " / ", n_genes_expected, "；累计 nnz≈", total_nnz)
    }

    invisible(TRUE)
  }

  if (!is.null(pending_first_data_line)) {
    handle_line(pending_first_data_line)
  }

  repeat {
    lines <- readLines(con, n = chunk_n_genes, warn = FALSE, skipNul = TRUE)
    if (length(lines) == 0L) break

    for (line in lines) {
      handle_line(line)
      if (gene_index >= n_genes_expected) break
    }

    if (gene_index >= n_genes_expected) break
  }

  flush_chunk()

  if (gene_index < n_genes_expected) {
    diff_n <- n_genes_expected - gene_index

    if (diff_n <= 5L) {
      stamp("UMI 行数比 genes 少 ", diff_n, " 行；按 UMI 实际行数裁剪 genes。")
    } else {
      stop("UMI 行数明显少于 genes 数量：读取 ", gene_index, "；genes=", n_genes_expected)
    }
  }

  actual_n_genes <- gene_index
  gene_names <- gene_names_all[seq_len(actual_n_genes)]

  chunk_manifest <- data.frame(
    chunk_index = seq_along(chunk_paths),
    chunk_path = chunk_paths,
    stringsAsFactors = FALSE
  )

  atomic_write_csv(
    chunk_manifest,
    file.path(metadata_dir, paste0("01A_GSE157783_chunk_manifest_", run_stamp, ".csv"))
  )

  stamp("开始合并 GSE157783 chunks：", length(chunk_paths), " 个")

  current_mat <- NULL
  batch_size <- 20L

  for (start in seq(1L, length(chunk_paths), by = batch_size)) {
    end <- min(start + batch_size - 1L, length(chunk_paths))

    stamp("合并 chunks：", start, " - ", end, " / ", length(chunk_paths))

    mats <- lapply(chunk_paths[start:end], readRDS)
    batch_mat <- do.call(rbind, mats)

    rm(mats)
    gc(verbose = FALSE)

    if (is.null(current_mat)) {
      current_mat <- batch_mat
    } else {
      current_mat <- rbind(current_mat, batch_mat)
    }

    rm(batch_mat)
    gc(verbose = FALSE)
  }

  mat <- current_mat
  rm(current_mat)
  gc(verbose = FALSE)

  if (nrow(mat) != actual_n_genes || ncol(mat) != n_cells) {
    stop("GSE157783 最终矩阵维度异常：", nrow(mat), "x", ncol(mat))
  }

  rownames(mat) <- gene_names
  colnames(mat) <- cell_names

  stamp("GSE157783 sparse matrix 完成：", nrow(mat), " x ", ncol(mat), "；nnz=", length(mat@x))

  list(matrix = mat, chunk_dir = chunk_dir)
}

import_gse157783 <- function(dataset_id, dataset_dir) {
  sample_id <- "IPDCO_hg_midbrain"

  out_dir <- file.path(objects_root, dataset_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  output_path <- file.path(out_dir, paste0("01A_", dataset_id, "_", sample_id, ".rds"))

  if (file.exists(output_path) && !REBUILD_EXISTING && object_is_readable(output_path)) {
    obj <- readRDS(output_path)

    return(list(
      make_status_row(
        dataset = dataset_id,
        sample = sample_id,
        source_file = dataset_dir,
        output_object = output_path,
        object = obj,
        status = "SKIPPED_EXISTING",
        message = "existing readable GSE157783 Seurat object",
        module = "GSE157783_special_stream_import"
      )
    ))
  }

  res <- tryCatch(
    {
      files <- find_gse157783_files(dataset_dir)

      if (is.na(files$cell)) stop("找不到 cell 文件")
      if (is.na(files$genes)) stop("找不到 genes 文件")
      if (is.na(files$umi)) stop("找不到 UMI 文件")

      stamp("GSE157783 cell 文件：", files$cell)
      stamp("GSE157783 genes 文件：", files$genes)
      stamp("GSE157783 UMI 文件：", files$umi)

      cells <- extract_ids_from_simple_file(files$cell, kind = "cell")
      genes <- extract_ids_from_simple_file(files$genes, kind = "gene")

      stamp("GSE157783 cells 数量：", length(cells))
      stamp("GSE157783 genes 数量：", length(genes))

      stream <- stream_dense_umi_to_sparse(
        umi_path = files$umi,
        genes = genes,
        cells = cells,
        chunk_n_genes = GSE157783_CHUNK_N_GENES,
        dataset_id = dataset_id
      )

      mat <- stream$matrix

      stamp("创建 GSE157783 Seurat object。")

      seu <- CreateSeuratObject(
        counts = mat,
        project = dataset_id,
        min.cells = 0,
        min.features = 0
      )

      seu <- add_basic_metadata(
        seu,
        dataset = dataset_id,
        sample = sample_id,
        extra = list(
          biological_system = "IPDCO_hg_midbrain",
          source_note = "formal 01A GSE157783 dense UMI streamed to sparse matrix",
          cell_file = files$cell,
          genes_file = files$genes,
          umi_file = files$umi,
          chunk_dir = stream$chunk_dir
        )
      )

      save_rds_atomic(seu, output_path, compress = FALSE)

      make_status_row(
        dataset = dataset_id,
        sample = sample_id,
        source_file = files$umi,
        output_object = output_path,
        object = seu,
        status = "IMPORTED",
        message = paste0("GSE157783 special import completed; matrix=", nrow(seu), "x", ncol(seu)),
        module = "GSE157783_special_stream_import"
      )
    },
    error = function(e) {
      make_status_row(
        dataset = dataset_id,
        sample = sample_id,
        source_file = dataset_dir,
        output_object = output_path,
        object = NULL,
        status = "FAILED",
        message = conditionMessage(e),
        module = "GSE157783_special_stream_import"
      )
    }
  )

  list(res)
}

extract_barcodes_from_df <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) return(character())

  cn <- colnames(df)
  cn_lower <- tolower(cn)

  priority_patterns <- c("^barcode$", "barcode", "^cell$", "cell_id", "cellid", "cell_name", "cellname", "barcodes", "cells")

  for (pat in priority_patterns) {
    idx <- which(grepl(pat, cn_lower, ignore.case = TRUE))
    if (length(idx) > 0L) {
      vals <- unique(trim_na(df[[idx[1L]]]))
      vals <- vals[nzchar(vals)]
      if (length(vals) > 0L) return(vals)
    }
  }

  vals <- unique(trim_na(df[[1L]]))
  vals[nzchar(vals)]
}

prepare_gse178265_da <- function(dataset_id, dataset_dir) {
  out_dir <- file.path(objects_root, dataset_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  target_barcodes_rds <- file.path(metadata_dir, "01A_GSE178265_DA_target_barcodes.rds")
  target_barcodes_csv <- file.path(metadata_dir, "01A_GSE178265_DA_target_barcodes.csv")
  target_metadata_rds <- file.path(metadata_dir, "01A_GSE178265_DA_metadata_ready_for_01B.rds")
  target_metadata_csv <- file.path(metadata_dir, "01A_GSE178265_DA_metadata_ready_for_01B.csv")

  if (
    !REBUILD_EXISTING &&
      file.exists(target_barcodes_rds) &&
      file.exists(target_metadata_rds)
  ) {
    bc <- readRDS(target_barcodes_rds)
    md <- readRDS(target_metadata_rds)

    return(list(
      make_status_row(
        dataset = dataset_id,
        sample = "DA_target_for_01B",
        source_file = dataset_dir,
        output_object = target_barcodes_rds,
        object = NULL,
        status = "READY_FOR_01B_STREAMING_SUBSET",
        message = paste0("existing DA target barcode file; n=", length(bc)),
        module = "GSE178265_DA_target_preparation"
      )
    ))
  }

  all_files <- list.files(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )

  all_files <- normalizePath(all_files, winslash = "/", mustWork = FALSE)
  all_files <- all_files[file.exists(all_files)]

  barcode_candidates <- all_files[
    grepl("(barcodes|barcode|bcd|cells|cell)\\.tsv(\\.gz)?$", basename(all_files), ignore.case = TRUE)
  ]

  raw_barcodes <- character()

  if (length(barcode_candidates) > 0L) {
    barcode_file <- barcode_candidates[order(file.info(barcode_candidates)$size, decreasing = TRUE)][1L]
    raw_barcodes <- tryCatch(read_first_column(barcode_file), error = function(e) character())
  }

  meta_files <- all_files[
    grepl("\\.(csv|tsv|txt)(\\.gz)?$", basename(all_files), ignore.case = TRUE) &
      !grepl("matrix|features|genes|barcodes|barcode|bcd", basename(all_files), ignore.case = TRUE)
  ]

  da_terms <- "dopamin|dopaminergic|mDA|DA_neuron|DA neuron|DA-neuron|DAN|A9|A10|TH\\+"

  candidate_tables <- list()

  for (path in meta_files) {
    stamp("GSE178265 尝试读取 metadata：", path)

    df <- tryCatch(
      {
        data.table::fread(
          path,
          sep = guess_sep(path),
          header = TRUE,
          quote = "",
          data.table = FALSE,
          showProgress = FALSE
        )
      },
      error = function(e) NULL
    )

    if (is.null(df) || nrow(df) == 0L || ncol(df) < 2L) next

    text_cols <- vapply(df, function(x) is.character(x) || is.factor(x), logical(1L))

    if (sum(text_cols) == 0L) next

    hit <- rep(FALSE, nrow(df))

    for (coln in names(df)[text_cols]) {
      vals <- trim_na(df[[coln]])
      hit <- hit | grepl(da_terms, vals, ignore.case = TRUE, perl = TRUE)
    }

    if (sum(hit) == 0L) next

    bc <- extract_barcodes_from_df(df)

    if (length(bc) != nrow(df)) {

      next
    }

    df$barcode <- bc
    df$is_DA_target_01B <- hit
    candidate_tables[[length(candidate_tables) + 1L]] <- df
  }

  if (length(candidate_tables) == 0L) {

    diagnostic_file <- file.path(metadata_dir, "01A_GSE178265_DA_target_pending_note.txt")
    writeLines(
      c(
        "GSE178265 DA target barcodes were not automatically identified in formal 01A V2.",
        "This is not treated as a fatal 01A import failure.",
        "Before 01B, use the available GSE178265 metadata/annotation to create:",
        "D:/PD_Graft_Project/01_metadata/01A_GSE178265_DA_target_barcodes.rds",
        "D:/PD_Graft_Project/01_metadata/01A_GSE178265_DA_metadata_ready_for_01B.rds"
      ),
      diagnostic_file,
      useBytes = TRUE
    )

    return(list(
      make_status_row(
        dataset = dataset_id,
        sample = "DA_target_for_01B",
        source_file = dataset_dir,
        output_object = diagnostic_file,
        object = NULL,
        status = "READY_FOR_01B_TARGET_PENDING",
        message = "GSE178265 DA target barcodes not auto-identified; non-fatal in formal 01A V2; prepare target before 01B",
        module = "GSE178265_DA_target_preparation"
      )
    ))
  }

  md <- candidate_tables[[which.max(vapply(candidate_tables, nrow, integer(1L)))]]

  md_da <- md[md$is_DA_target_01B, , drop = FALSE]

  target_barcodes <- unique(trim_na(md_da$barcode))
  target_barcodes <- target_barcodes[nzchar(target_barcodes)]

  if (length(target_barcodes) == 0L) {
    diagnostic_file <- file.path(metadata_dir, "01A_GSE178265_DA_target_pending_note.txt")
    writeLines(
      c(
        "GSE178265 DA-like metadata rows were found, but barcode extraction was empty.",
        "This is not treated as a fatal 01A import failure in V2.",
        "Before 01B, manually/explicitly prepare DA target barcodes."
      ),
      diagnostic_file,
      useBytes = TRUE
    )

    return(list(
      make_status_row(
        dataset = dataset_id,
        sample = "DA_target_for_01B",
        source_file = dataset_dir,
        output_object = diagnostic_file,
        object = NULL,
        status = "READY_FOR_01B_TARGET_PENDING",
        message = "GSE178265 DA rows found but barcode extraction empty; non-fatal in formal 01A V2",
        module = "GSE178265_DA_target_preparation"
      )
    ))
  }

  if (length(raw_barcodes) > 0L) {
    norm <- function(x) sub("-[0-9]+$", "", sub("^.*__", "", trim_na(x)))

    raw_norm <- norm(raw_barcodes)
    target_norm <- norm(target_barcodes)

    keep <- target_barcodes %in% raw_barcodes | target_norm %in% raw_norm

    if (sum(keep) > 0L) {
      target_barcodes <- target_barcodes[keep]
      md_da <- md_da[norm(md_da$barcode) %in% norm(target_barcodes), , drop = FALSE]
    }
  }

  saveRDS(target_barcodes, target_barcodes_rds)
  saveRDS(md_da, target_metadata_rds)

  atomic_write_csv(
    data.frame(barcode = target_barcodes, stringsAsFactors = FALSE),
    target_barcodes_csv
  )

  atomic_write_csv(md_da, target_metadata_csv)

  list(
    make_status_row(
      dataset = dataset_id,
      sample = "DA_target_for_01B",
      source_file = dataset_dir,
      output_object = target_barcodes_rds,
      object = NULL,
      status = "READY_FOR_01B_STREAMING_SUBSET",
      message = paste0("GSE178265 DA target barcodes prepared; n=", length(target_barcodes)),
      module = "GSE178265_DA_target_preparation"
    )
  )
}

resolve_existing_standardized_objects <- function(dataset_id, dataset_dir) {
  ds_obj_dir <- file.path(objects_root, dataset_id)

  if (!dir.exists(ds_obj_dir)) {
    return(list())
  }

  rds_files <- list.files(
    ds_obj_dir,
    pattern = "\\.rds$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  rds_files <- normalizePath(rds_files, winslash = "/", mustWork = FALSE)
  rds_files <- rds_files[file.exists(rds_files)]

  if (length(rds_files) == 0L) {
    return(list())
  }

  readable <- vapply(rds_files, object_is_readable, logical(1L))

  rds_files <- rds_files[readable]

  if (length(rds_files) == 0L) {
    return(list())
  }

  lapply(
    rds_files,
    function(path) {
      obj <- readRDS(path)

      make_status_row(
        dataset = dataset_id,
        sample = safe_name(basename(path)),
        source_file = dataset_dir,
        output_object = path,
        object = obj,
        status = "SKIPPED_EXISTING",
        message = "existing standardized object resolved by formal 01A V2",
        module = "existing_standardized_object_fallback"
      )
    }
  )
}

import_dataset <- function(dataset_id, dataset_dir) {
  stamp("------------------------------------------------------------")
  stamp("开始 01A 数据集：", dataset_id)
  stamp("路径：", dataset_dir)

  if (dataset_id == "GSE178265") {
    return(prepare_gse178265_da(dataset_id, dataset_dir))
  }

  if (dataset_id == "GSE157783") {
    return(import_gse157783(dataset_id, dataset_dir))
  }

  statuses <- list()

  statuses <- c(statuses, import_10x_dataset(dataset_id, dataset_dir))

  statuses <- c(statuses, import_rds_dataset(dataset_id, dataset_dir))

  success_n <- sum(
    vapply(
      statuses,
      function(x) {
        toupper(trim_na(x$status[1L])) %in% c(
          "IMPORTED",
          "SKIPPED_EXISTING",
          "READY_FOR_01B_STREAMING_SUBSET",
          "RESOLVED_EXISTING_OBJECT",
          "READY_FOR_01B_TARGET_PENDING"
        )
      },
      logical(1L)
    )
  )

  if (length(statuses) == 0L || success_n == 0L) {
    existing_statuses <- resolve_existing_standardized_objects(dataset_id, dataset_dir)

    if (length(existing_statuses) > 0L) {
      statuses <- existing_statuses
    } else {
      statuses <- list(
        make_status_row(
          dataset = dataset_id,
          sample = "dataset_level",
          source_file = dataset_dir,
          output_object = NA_character_,
          object = NULL,
          status = "FAILED",
          message = "未识别到可导入 10x matrix 或 RDS/RData 对象，且未找到已有标准化对象",
          module = "dataset_level_format_detection"
        )
      )
    }
  }

  statuses
}

raw_root <- file.path(PROJECT_ROOT, "00_raw_data")

if (!dir.exists(raw_root)) {
  stop("找不到 00_raw_data：", raw_root)
}

dataset_dirs <- list.dirs(
  raw_root,
  recursive = FALSE,
  full.names = TRUE
)

dataset_dirs <- normalizePath(dataset_dirs, winslash = "/", mustWork = FALSE)
dataset_dirs <- dataset_dirs[dir.exists(dataset_dirs)]

dataset_ids <- basename(dataset_dirs)

keep <- grepl("^GSE[0-9]+$", dataset_ids, ignore.case = TRUE)

dataset_dirs <- dataset_dirs[keep]
dataset_ids <- dataset_ids[keep]

if (length(dataset_dirs) == 0L) {
  stop("00_raw_data 下没有找到 GSE 数据集目录。")
}

priority <- ifelse(dataset_ids == "GSE157783", 100L, 0L)
priority <- priority + ifelse(dataset_ids == "GSE178265", -10L, 0L)

ord <- order(priority, dataset_ids)

dataset_dirs <- dataset_dirs[ord]
dataset_ids <- dataset_ids[ord]

stamp("检测到数据集数量：", length(dataset_ids))
stamp("数据集列表：", paste(dataset_ids, collapse = ", "))

backup_overall <- backup_if_exists(overall_status_csv)
backup_failure <- backup_if_exists(failure_csv)
backup_summary <- backup_if_exists(summary_csv)
backup_unified <- backup_if_exists(unified_meta_csv)

all_status_list <- list()

for (i in seq_along(dataset_ids)) {
  ds <- dataset_ids[i]
  ds_dir <- dataset_dirs[i]

  result <- tryCatch(
    import_dataset(ds, ds_dir),
    error = function(e) {
      list(
        make_status_row(
          dataset = ds,
          sample = "dataset_level",
          source_file = ds_dir,
          output_object = NA_character_,
          object = NULL,
          status = "FAILED",
          message = conditionMessage(e),
          module = "dataset_level_unhandled_error"
        )
      )
    }
  )

  all_status_list <- c(all_status_list, result)

  gc(verbose = FALSE)
}

overall_status <- do.call(rbind, all_status_list)

if (is.null(overall_status) || nrow(overall_status) == 0L) {
  overall_status <- data.frame(
    dataset = character(),
    sample = character(),
    source_file = character(),
    output_object = character(),
    object_class = character(),
    n_features = integer(),
    n_cells = integer(),
    module = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

status_upper <- toupper(trim_na(overall_status$status))

failure_table <- overall_status[
  status_upper == "FAILED",
  ,
  drop = FALSE
]

datasets <- unique(trim_na(overall_status$dataset))
datasets[datasets == ""] <- "UNKNOWN"

summary_rows <- lapply(
  datasets,
  function(ds) {
    one <- overall_status[trim_na(overall_status$dataset) == ds, , drop = FALSE]
    status_upper_one <- toupper(trim_na(one$status))

    data.frame(
      dataset = ds,
      records = nrow(one),
      imported_or_ready = sum(
        status_upper_one %in% c(
          "IMPORTED",
          "SKIPPED_EXISTING",
          "READY_FOR_01B_STREAMING_SUBSET",
          "RESOLVED_EXISTING_OBJECT",
          "READY_FOR_01B_TARGET_PENDING"
        )
      ),
      failed = sum(status_upper_one == "FAILED"),
      total_features_sum = if ("n_features" %in% colnames(one)) {
        sum(suppressWarnings(as.numeric(one$n_features)), na.rm = TRUE)
      } else {
        NA_real_
      },
      total_cells_or_samples_sum = if ("n_cells" %in% colnames(one)) {
        sum(suppressWarnings(as.numeric(one$n_cells)), na.rm = TRUE)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }
)

dataset_summary <- do.call(rbind, summary_rows)
dataset_summary <- dataset_summary[order(dataset_summary$dataset), , drop = FALSE]

unified_sample_metadata <- overall_status[
  ,
  intersect(
    c(
      "dataset",
      "sample",
      "source_file",
      "output_object",
      "object_class",
      "n_features",
      "n_cells",
      "module",
      "status",
      "message"
    ),
    colnames(overall_status)
  ),
  drop = FALSE
]

atomic_write_csv(overall_status, overall_status_csv)
atomic_write_csv(failure_table, failure_csv)
atomic_write_csv(dataset_summary, summary_csv)
atomic_write_csv(unified_sample_metadata, unified_meta_csv)

report_lines <- c(
  "PD_Graft_Project：01A 正式生产版 all-in-one 导入报告",
  paste0("生成时间：", Sys.time()),
  "",
  "运行设置：",
  paste0("PROJECT_ROOT: ", PROJECT_ROOT),
  paste0("REBUILD_EXISTING: ", REBUILD_EXISTING),
  paste0("GSE157783_CHUNK_N_GENES: ", GSE157783_CHUNK_N_GENES),
  "",
  "结果概览：",
  paste0("数据集数量：", length(dataset_ids)),
  paste0("状态记录数：", nrow(overall_status)),
  paste0("失败记录数：", nrow(failure_table)),
  "",
  "数据集汇总：",
  capture.output(print(dataset_summary)),
  "",
  "失败记录：",
  if (nrow(failure_table) == 0L) {
    "无"
  } else {
    capture.output(
      print(
        failure_table[
          ,
          intersect(
            c("dataset", "sample", "source_file", "output_object", "status", "message"),
            colnames(failure_table)
          ),
          drop = FALSE
        ]
      )
    )
  },
  "",
  "输出文件：",
  paste0("overall_status_csv: ", overall_status_csv),
  paste0("failure_csv: ", failure_csv),
  paste0("summary_csv: ", summary_csv),
  paste0("unified_meta_csv: ", unified_meta_csv),
  paste0("log_file: ", log_file),
  "",
  "备份文件：",
  paste0("backup_overall: ", backup_overall),
  paste0("backup_failure: ", backup_failure),
  paste0("backup_summary: ", backup_summary),
  paste0("backup_unified: ", backup_unified)
)

writeLines(report_lines, report_file, useBytes = TRUE)

cat("\n\n============================================================\n")
cat("01A 正式生产版运行结束\n")
cat("============================================================\n\n")

cat("数据集数量：", length(dataset_ids), "\n")
cat("状态记录数：", nrow(overall_status), "\n")
cat("全局失败数量：", nrow(failure_table), "\n\n")

cat("输出文件：\n")
cat(overall_status_csv, "\n")
cat(failure_csv, "\n")
cat(summary_csv, "\n")
cat(unified_meta_csv, "\n")
cat(report_file, "\n\n")

if (nrow(failure_table) == 0L) {
  cat("✅ 01A全局失败数量：0。\n")
  cat("✅ 01A正式完成，可以进入01B。\n")
} else {
  cat("⚠️ 01A仍有失败记录，暂时不要进入01B。\n")
  cat("请把下面失败记录截图发回来。\n\n")

  print(
    failure_table[
      ,
      intersect(
        c("dataset", "sample", "source_file", "output_object", "status", "message"),
        colnames(failure_table)
      ),
      drop = FALSE
    ]
  )
}
