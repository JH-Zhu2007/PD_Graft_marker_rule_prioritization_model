
PROJECT_ROOT <- "D:/PD_Graft_Project"

N_WORKERS_LIGHT <- 2L

FORCE_REEXTRACT <- FALSE

AUTO_INSTALL_CRAN <- TRUE

AUTO_INSTALL_SEURATOBJECT <- TRUE

options(stringsAsFactors = FALSE)
options(timeout = 600)
set.seed(20260713)

required_cran <- c(
  "data.table",
  "openxlsx",
  "future",
  "future.apply"
)

missing_cran <- required_cran[
  !vapply(required_cran, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_cran) > 0L) {
  if (AUTO_INSTALL_CRAN) {
    install.packages(missing_cran, dependencies = TRUE)
  } else {
    stop(
      "缺少CRAN包：", paste(missing_cran, collapse = ", "),
      "\n请先安装后重新运行。"
    )
  }
}

if (!requireNamespace("SeuratObject", quietly = TRUE)) {
  if (AUTO_INSTALL_SEURATOBJECT) {
    install.packages("SeuratObject", dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(future)
  library(future.apply)
})

if (!dir.exists(PROJECT_ROOT)) {
  stop("项目目录不存在：", PROJECT_ROOT)
}

PROJECT_ROOT <- normalizePath(
  PROJECT_ROOT,
  winslash = "/",
  mustWork = TRUE
)

raw_root <- file.path(PROJECT_ROOT, "00_raw_data")
metadata_dir <- file.path(PROJECT_ROOT, "01_metadata")
objects_dir <- file.path(PROJECT_ROOT, "02_objects")
reports_dir <- file.path(PROJECT_ROOT, "06_reports")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(reports_dir, "00B_critical_metadata_live_log.txt")

timestamp_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

log_message <- function(...) {
  msg <- paste0(...)
  line <- paste0("[", timestamp_now(), "] ", msg)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

human_size <- function(bytes) {
  if (length(bytes) == 0L || is.na(bytes)) return(NA_character_)
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  idx <- 1L

  while (value >= 1024 && idx < length(units)) {
    value <- value / 1024
    idx <- idx + 1L
  }

  sprintf("%.2f %s", value, units[idx])
}

truncate_text <- function(x, n = 600L) {
  x <- paste(x, collapse = " | ")
  if (nchar(x) <= n) return(x)
  paste0(substr(x, 1L, n), " ...")
}

safe_unique_preview <- function(x, max_values = 15L) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) return(NA_character_)

  preview <- head(x, max_values)
  suffix <- if (length(x) > max_values) {
    paste0(" | ... total_unique=", length(x))
  } else {
    ""
  }

  paste0(paste(preview, collapse = " | "), suffix)
}

g200_tar <- file.path(
  raw_root,
  "GSE200610",
  "00_downloaded",
  "GSE200610_RAW.tar"
)

g233_tar <- file.path(
  raw_root,
  "GSE233885",
  "00_downloaded",
  "GSE233885_RAW.tar"
)

g200_extract <- file.path(
  raw_root,
  "GSE200610",
  "01_extracted"
)

g233_extract <- file.path(
  raw_root,
  "GSE233885",
  "01_extracted"
)

g178_external <- file.path(
  raw_root,
  "GSE178265",
  "02_external_metadata"
)

dir.create(g200_extract, recursive = TRUE, showWarnings = FALSE)
dir.create(g233_extract, recursive = TRUE, showWarnings = FALSE)
dir.create(g178_external, recursive = TRUE, showWarnings = FALSE)

missing_core <- c(
  if (!file.exists(g200_tar)) g200_tar else character(),
  if (!file.exists(g233_tar)) g233_tar else character()
)

if (length(missing_core) > 0L) {
  stop(
    "缺少00B需要的核心文件：\n",
    paste(missing_core, collapse = "\n")
  )
}

safe_extract_tar <- function(tar_path, out_dir, force = FALSE) {
  existing_files <- list.files(
    out_dir,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )

  if (length(existing_files) > 0L && !force) {
    log_message(
      "跳过重复解压：", basename(tar_path),
      "；当前已有 ", length(existing_files), " 个解压文件。"
    )
    return(normalizePath(
      existing_files,
      winslash = "/",
      mustWork = FALSE
    ))
  }

  if (force && dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE, force = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  log_message(
    "开始顺序解压：", basename(tar_path),
    "（", human_size(file.info(tar_path)$size), "）"
  )

  result <- tryCatch({
    utils::untar(
      tar_path,
      exdir = out_dir,
      tar = "internal"
    )
    TRUE
  }, error = function(e) {
    log_message(
      "internal untar失败，尝试系统tar：",
      conditionMessage(e)
    )

    tryCatch({
      utils::untar(
        tar_path,
        exdir = out_dir
      )
      TRUE
    }, error = function(e2) {
      stop(
        "解压失败：", tar_path, "\n",
        conditionMessage(e2)
      )
    })
  })

  files <- list.files(
    out_dir,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )

  if (!result || length(files) == 0L) {
    stop("压缩包解压后没有得到文件：", tar_path)
  }

  log_message(
    "解压完成：", basename(tar_path),
    "；得到 ", length(files), " 个文件。"
  )

  normalizePath(files, winslash = "/", mustWork = FALSE)
}

detect_extension <- function(path) {
  nm <- tolower(basename(path))

  patterns <- c(
    "\\.csv\\.gz$" = "csv.gz",
    "\\.tsv\\.gz$" = "tsv.gz",
    "\\.txt\\.gz$" = "txt.gz",
    "\\.rds\\.gz$" = "rds.gz",
    "\\.csv$" = "csv",
    "\\.tsv$" = "tsv",
    "\\.txt$" = "txt",
    "\\.rds$" = "rds",
    "\\.mtx\\.gz$" = "mtx.gz",
    "\\.mtx$" = "mtx"
  )

  for (pat in names(patterns)) {
    if (grepl(pat, nm, perl = TRUE)) {
      return(unname(patterns[[pat]]))
    }
  }

  tools::file_ext(nm)
}

open_text_connection <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
}

read_first_lines <- function(path, n = 5L) {
  con <- NULL

  tryCatch({
    con <- open_text_connection(path)
    readLines(con, n = n, warn = FALSE)
  }, error = function(e) {
    paste0("ERROR: ", conditionMessage(e))
  }, finally = {
    if (!is.null(con)) {
      try(close(con), silent = TRUE)
    }
  })
}

guess_separator <- function(path, first_line) {
  ext <- detect_extension(path)

  if (ext %in% c("csv", "csv.gz")) return(",")
  if (ext %in% c("tsv", "tsv.gz")) return("\t")

  comma_n <- lengths(regmatches(first_line, gregexpr(",", first_line, fixed = TRUE)))
  tab_n <- lengths(regmatches(first_line, gregexpr("\t", first_line, fixed = TRUE)))

  if (tab_n > comma_n) "\t" else ","
}

inspect_text_header <- function(path) {
  first_lines <- read_first_lines(path, n = 5L)

  if (length(first_lines) == 0L ||
      grepl("^ERROR:", first_lines[1L])) {
    return(data.table(
      file_path = path,
      file_name = basename(path),
      extension = detect_extension(path),
      size_bytes = file.info(path)$size,
      size_human = human_size(file.info(path)$size),
      header_status = if (length(first_lines) == 0L) {
        "EMPTY"
      } else {
        first_lines[1L]
      },
      separator = NA_character_,
      estimated_n_columns = NA_integer_,
      first_columns = NA_character_,
      first_lines_preview = truncate_text(first_lines)
    ))
  }

  sep <- guess_separator(path, first_lines[1L])
  headers <- strsplit(
    first_lines[1L],
    split = sep,
    fixed = TRUE
  )[[1L]]

  headers <- gsub(
    '^"|"$',
    "",
    headers
  )

  data.table(
    file_path = path,
    file_name = basename(path),
    extension = detect_extension(path),
    size_bytes = file.info(path)$size,
    size_human = human_size(file.info(path)$size),
    header_status = "OK",
    separator = ifelse(sep == "\t", "TAB", "COMMA"),
    estimated_n_columns = length(headers),
    first_columns = truncate_text(head(headers, 80L)),
    first_lines_preview = truncate_text(first_lines, n = 900L)
  )
}

extract_rat_id <- function(x) {
  x_low <- tolower(x)

  patterns <- c(
    "rat(?:sd)?[0-9]+[a-z0-9]*",
    "(?:sd|nude)-?no[0-9]+"
  )

  for (pat in patterns) {
    m <- regexpr(pat, x_low, perl = TRUE)

    if (m[1L] > 0L) {
      return(regmatches(x_low, m))
    }
  }

  NA_character_
}

extract_timepoint <- function(x) {
  x_low <- tolower(x)

  if (grepl("12m|12month", x_low)) return("12m")
  if (grepl("9m|9month", x_low)) return("9m")
  if (grepl("4m|4month", x_low)) return("4m")
  if (grepl("1m|1month", x_low)) return("1m")

  NA_character_
}

log_message("00B开始。")
log_message("项目路径：", PROJECT_ROOT)
log_message("轻量并行核心：", N_WORKERS_LIGHT)
log_message("大型RDS读取模式：严格顺序。")

g200_files <- safe_extract_tar(
  g200_tar,
  g200_extract,
  force = FORCE_REEXTRACT
)

gc(verbose = FALSE)

g233_files <- safe_extract_tar(
  g233_tar,
  g233_extract,
  force = FORCE_REEXTRACT
)

gc(verbose = FALSE)

log_message("开始GSE200610文件和表头审计。")

g200_inventory <- data.table(
  file_path = g200_files
)

g200_inventory[, file_name := basename(file_path)]
g200_inventory[, extension := vapply(
  file_path,
  detect_extension,
  character(1)
)]
g200_inventory[, size_bytes := file.info(file_path)$size]
g200_inventory[, size_human := vapply(
  size_bytes,
  human_size,
  character(1)
)]
g200_inventory[, rat_id_from_name := vapply(
  file_name,
  extract_rat_id,
  character(1)
)]
g200_inventory[, timepoint_from_name := vapply(
  file_name,
  extract_timepoint,
  character(1)
)]

g200_text_files <- g200_inventory[
  extension %in% c(
    "csv", "csv.gz",
    "tsv", "tsv.gz",
    "txt", "txt.gz"
  ),
  file_path
]

future::plan(
  future::multisession,
  workers = max(1L, min(N_WORKERS_LIGHT, 2L))
)

if (length(g200_text_files) > 0L) {
  g200_headers_list <- future_lapply(
    g200_text_files,
    inspect_text_header,
    future.seed = TRUE
  )

  g200_headers <- rbindlist(
    g200_headers_list,
    fill = TRUE
  )
} else {
  g200_headers <- data.table(
    file_path = character(),
    file_name = character(),
    extension = character(),
    size_bytes = numeric(),
    size_human = character(),
    header_status = character(),
    separator = character(),
    estimated_n_columns = integer(),
    first_columns = character(),
    first_lines_preview = character()
  )
}

future::plan(future::sequential)

g200_headers[, searchable_text := paste(
  file_name,
  first_columns,
  first_lines_preview,
  sep = " | "
)]

g200_headers[, has_clone_word := grepl(
  "clone|clonal",
  searchable_text,
  ignore.case = TRUE,
  perl = TRUE
)]

g200_headers[, has_barcode_word := grepl(
  "barcode|barcoded|molecular.?bc|clone.?id",
  searchable_text,
  ignore.case = TRUE,
  perl = TRUE
)]

g200_headers[, has_mapping_word := grepl(
  "cell.?to.?clone|clone.?mapping|barcode.?mapping|lineage",
  searchable_text,
  ignore.case = TRUE,
  perl = TRUE
)]

g200_headers[, appears_expression_matrix := (
  estimated_n_columns > 100L |
  grepl(
    "count|matrix|umi|nuc|vmcell|graft",
    file_name,
    ignore.case = TRUE
  )
)]

g200_candidate_files <- g200_headers[
  has_clone_word |
  has_barcode_word |
  has_mapping_word |
  grepl(
    "meta|annot|mapping|clone|barcode|lineage",
    file_name,
    ignore.case = TRUE
  )
]

g200_clone_confirmed <- any(
  g200_headers$has_mapping_word,
  na.rm = TRUE
)

g200_barcode_clue <- any(
  g200_headers$has_barcode_word |
  g200_headers$has_clone_word,
  na.rm = TRUE
)

g200_status <- data.table(
  geo = "GSE200610",
  extracted_file_count = nrow(g200_inventory),
  text_file_count = nrow(g200_headers),
  clone_or_barcode_text_clue = g200_barcode_clue,
  explicit_cell_to_clone_mapping_confirmed = g200_clone_confirmed,
  readiness = if (g200_clone_confirmed) {
    "READY_FOR_CLONE_AWARE_ANALYSIS"
  } else if (g200_barcode_clue) {
    "BARCODE_EXPERIMENT_CLUE_BUT_MAPPING_NOT_CONFIRMED"
  } else {
    "COUNT_MATRICES_ONLY_NO_CLONE_MAPPING_FOUND"
  },
  interpretation = if (g200_clone_confirmed) {
    paste(
      "检测到明确cell-to-clone或barcode mapping字段，",
      "后续可设计clone-aware分析，但仍需人工核对映射单位。"
    )
  } else {
    paste(
      "当前公开TAR内未确认细胞到克隆的显式映射。",
      "仍可进行细胞状态、动物级组成和Safety Risk分析，",
      "但暂时不能把它写成clone-aware监督机器学习。"
    )
  }
)

log_message(
  "GSE200610结果：",
  g200_status$readiness
)

has_nonempty_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

safe_read_rds <- function(path) {
  if (!grepl("\\.gz$", path, ignore.case = TRUE)) {
    return(readRDS(path))
  }

  con <- gzfile(path, open = "rb")
  on.exit(
    try(close(con), silent = TRUE),
    add = TRUE
  )

  readRDS(con)
}

ensure_table_schema <- function(x, schema) {
  if (is.null(x) || ncol(x) == 0L) {
    return(as.data.table(schema))
  }

  as.data.table(x)
}

write_sheet_safe <- function(wb, sheet, x, empty_message = "No records detected") {
  addWorksheet(wb, sheet)

  x <- as.data.table(x)

  if (ncol(x) == 0L || nrow(x) == 0L) {
    writeData(
      wb,
      sheet,
      data.frame(message = empty_message),
      startRow = 1L,
      startCol = 1L
    )
    freezePane(wb, sheet, firstRow = TRUE)
    setColWidths(wb, sheet, cols = 1L, widths = "auto")
    return(invisible(NULL))
  }

  writeDataTable(wb, sheet, x)
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(x)),
    widths = "auto"
  )

  invisible(NULL)
}

candidate_patterns <- list(
  rat = "rat|animal|subject|donor",
  timepoint = "time|month|9m|12m|age",
  projection = "projection|project|target|innerv|retro",
  barcode = "barcode|aav|bc$|bc_|retro",
  celltype = "cell.?type|annotation|cluster|subtype|ident",
  sample = "sample|orig.ident|library|batch|seq",
  condition = "condition|group|treatment|graft"
)

extract_generic_metadata <- function(obj) {
  md <- NULL
  source <- NA_character_

  if (inherits(obj, "Seurat")) {
    md <- tryCatch(
      obj[[]],
      error = function(e) NULL
    )

    if (is.null(md) &&
        methods::isS4(obj) &&
        "meta.data" %in% methods::slotNames(obj)) {
      md <- tryCatch(
        methods::slot(obj, "meta.data"),
        error = function(e) NULL
      )
    }

    source <- "Seurat_meta.data"
  }

  if (is.null(md) &&
      methods::isS4(obj) &&
      "meta.data" %in% methods::slotNames(obj)) {
    md <- tryCatch(
      methods::slot(obj, "meta.data"),
      error = function(e) NULL
    )
    source <- "S4_meta.data_slot"
  }

  if (is.null(md) &&
      methods::isS4(obj) &&
      "colData" %in% methods::slotNames(obj)) {
    md <- tryCatch(
      as.data.frame(methods::slot(obj, "colData")),
      error = function(e) NULL
    )
    source <- "S4_colData_slot"
  }

  if (is.null(md) && is.list(obj)) {
    candidate_names <- names(obj)
    possible <- c(
      "meta.data", "metadata", "meta",
      "cell_metadata", "cell.meta", "colData"
    )

    hit <- possible[possible %in% candidate_names]

    if (length(hit) > 0L) {
      md <- tryCatch(
        as.data.frame(obj[[hit[1L]]]),
        error = function(e) NULL
      )
      source <- paste0("list$", hit[1L])
    }
  }

  if (is.null(md) && is.data.frame(obj)) {
    md <- obj
    source <- "object_is_data.frame"
  }

  if (!is.null(md)) {
    md <- as.data.frame(
      md,
      stringsAsFactors = FALSE
    )

    if (is.null(rownames(md))) {
      rownames(md) <- paste0("cell_", seq_len(nrow(md)))
    }
  }

  list(
    metadata = md,
    source = source
  )
}

object_dimensions <- function(obj) {
  nr <- tryCatch(
    nrow(obj),
    error = function(e) NA_real_
  )

  nc <- tryCatch(
    ncol(obj),
    error = function(e) NA_real_
  )

  if ((is.na(nr) || is.na(nc)) && is.list(obj)) {
    matrix_names <- c(
      "counts", "count", "matrix",
      "data", "expr", "expression"
    )

    hit <- matrix_names[matrix_names %in% names(obj)]

    if (length(hit) > 0L) {
      mat <- obj[[hit[1L]]]

      nr <- tryCatch(
        nrow(mat),
        error = function(e) nr
      )

      nc <- tryCatch(
        ncol(mat),
        error = function(e) nc
      )
    }
  }

  c(n_features = nr, n_cells = nc)
}

find_candidate_columns <- function(columns) {
  result <- list()

  for (category in names(candidate_patterns)) {
    pat <- candidate_patterns[[category]]
    result[[category]] <- columns[
      grepl(
        pat,
        columns,
        ignore.case = TRUE,
        perl = TRUE
      )
    ]
  }

  result
}

log_message("开始GSE233885逐RDS顺序审计。")

g233_inventory <- data.table(
  file_path = g233_files
)

g233_inventory[, file_name := basename(file_path)]
g233_inventory[, extension := vapply(
  file_path,
  detect_extension,
  character(1)
)]
g233_inventory[, size_bytes := file.info(file_path)$size]
g233_inventory[, size_human := vapply(
  size_bytes,
  human_size,
  character(1)
)]
g233_inventory[, rat_id_from_name := vapply(
  file_name,
  extract_rat_id,
  character(1)
)]
g233_inventory[, timepoint_from_name := vapply(
  file_name,
  extract_timepoint,
  character(1)
)]

g233_rds_files <- g233_inventory[
  extension %in% c("rds", "rds.gz"),
  file_path
]

if (length(g233_rds_files) == 0L) {
  stop(
    "GSE233885解压后没有找到RDS文件，请检查：",
    g233_extract
  )
}

g233_rds_audit <- data.table(
  file_path = character(),
  file_name = character(),
  read_status = character(),
  object_class = character(),
  n_features = numeric(),
  n_cells = numeric(),
  metadata_source = character(),
  metadata_n_rows = integer(),
  metadata_n_columns = integer(),
  metadata_columns = character(),
  rat_id_from_name = character(),
  timepoint_from_name = character(),
  rat_columns = character(),
  timepoint_columns = character(),
  projection_columns = character(),
  barcode_columns = character(),
  celltype_columns = character(),
  sample_columns = character()
)

g233_candidate_values <- data.table(
  file_name = character(),
  category = character(),
  column = character(),
  unique_values_preview = character(),
  n_unique = integer()
)

g233_cell_metadata_list <- vector(
  "list",
  length(g233_rds_files)
)

for (i in seq_along(g233_rds_files)) {
  rds_path <- g233_rds_files[i]

  log_message(
    "[GSE233885 RDS ", i, "/",
    length(g233_rds_files), "] ",
    basename(rds_path)
  )

  obj <- tryCatch(
    safe_read_rds(rds_path),
    error = function(e) e
  )

  if (inherits(obj, "error")) {
    g233_rds_audit <- rbind(
      g233_rds_audit,
      data.table(
        file_path = rds_path,
        file_name = basename(rds_path),
        read_status = paste0(
          "ERROR: ",
          conditionMessage(obj)
        ),
        object_class = NA_character_,
        n_features = NA_real_,
        n_cells = NA_real_,
        metadata_source = NA_character_,
        metadata_n_rows = NA_integer_,
        metadata_n_columns = NA_integer_,
        metadata_columns = NA_character_,
        rat_id_from_name = extract_rat_id(
          basename(rds_path)
        ),
        timepoint_from_name = extract_timepoint(
          basename(rds_path)
        ),
        rat_columns = NA_character_,
        timepoint_columns = NA_character_,
        projection_columns = NA_character_,
        barcode_columns = NA_character_,
        celltype_columns = NA_character_,
        sample_columns = NA_character_
      ),
      fill = TRUE
    )

    rm(obj)
    gc(verbose = FALSE)
    next
  }

  dims <- object_dimensions(obj)
  md_info <- extract_generic_metadata(obj)
  md <- md_info$metadata

  if (is.null(md)) {
    metadata_columns <- character()
    candidate_cols <- lapply(
      candidate_patterns,
      function(x) character()
    )
    md_rows <- NA_integer_
    md_cols <- NA_integer_
  } else {
    metadata_columns <- names(md)
    candidate_cols <- find_candidate_columns(
      metadata_columns
    )
    md_rows <- nrow(md)
    md_cols <- ncol(md)
  }

  one_audit <- data.table(
    file_path = rds_path,
    file_name = basename(rds_path),
    read_status = "OK",
    object_class = paste(
      class(obj),
      collapse = " | "
    ),
    n_features = unname(dims["n_features"]),
    n_cells = unname(dims["n_cells"]),
    metadata_source = md_info$source,
    metadata_n_rows = md_rows,
    metadata_n_columns = md_cols,
    metadata_columns = truncate_text(
      metadata_columns,
      n = 2000L
    ),
    rat_id_from_name = extract_rat_id(
      basename(rds_path)
    ),
    timepoint_from_name = extract_timepoint(
      basename(rds_path)
    ),
    rat_columns = paste(
      candidate_cols$rat,
      collapse = " | "
    ),
    timepoint_columns = paste(
      candidate_cols$timepoint,
      collapse = " | "
    ),
    projection_columns = paste(
      candidate_cols$projection,
      collapse = " | "
    ),
    barcode_columns = paste(
      candidate_cols$barcode,
      collapse = " | "
    ),
    celltype_columns = paste(
      candidate_cols$celltype,
      collapse = " | "
    ),
    sample_columns = paste(
      candidate_cols$sample,
      collapse = " | "
    )
  )

  g233_rds_audit <- rbind(
    g233_rds_audit,
    one_audit,
    fill = TRUE
  )

  if (!is.null(md)) {
    all_candidate_columns <- unique(c(
      unlist(candidate_cols, use.names = FALSE)
    ))

    all_candidate_columns <- all_candidate_columns[
      all_candidate_columns %in% names(md)
    ]

    value_columns <- all_candidate_columns

    if (length(value_columns) > 0L) {
      for (col in value_columns) {
        g233_candidate_values <- rbind(
          g233_candidate_values,
          data.table(
            file_name = basename(rds_path),
            category = paste(
              names(candidate_cols)[
                vapply(
                  candidate_cols,
                  function(x) col %in% x,
                  logical(1)
                )
              ],
              collapse = " | "
            ),
            column = col,
            unique_values_preview = safe_unique_preview(
              md[[col]],
              max_values = 20L
            ),
            n_unique = length(unique(md[[col]]))
          ),
          fill = TRUE
        )
      }
    }

    keep_md <- data.table(
      cell_id = rownames(md),
      source_file = basename(rds_path),
      rat_id_from_filename = extract_rat_id(
        basename(rds_path)
      ),
      timepoint_from_filename = extract_timepoint(
        basename(rds_path)
      )
    )

    if (length(all_candidate_columns) > 0L) {
      candidate_md <- as.data.table(
        md[, all_candidate_columns, drop = FALSE]
      )

      keep_md <- cbind(
        keep_md,
        candidate_md
      )
    }

    g233_cell_metadata_list[[i]] <- keep_md
  }

  rm(obj, md, md_info)
  gc(verbose = FALSE)
}

g233_cell_metadata <- rbindlist(
  g233_cell_metadata_list,
  fill = TRUE,
  use.names = TRUE
)

g233_projection_confirmed <- any(
  has_nonempty_text(g233_rds_audit$projection_columns) |
  has_nonempty_text(g233_rds_audit$barcode_columns)
)

g233_rat_confirmed <- any(
  has_nonempty_text(g233_rds_audit$rat_columns)
) || all(
  has_nonempty_text(g233_rds_audit$rat_id_from_name)
)

g233_time_confirmed <- any(
  has_nonempty_text(g233_rds_audit$timepoint_columns)
) || all(
  has_nonempty_text(g233_rds_audit$timepoint_from_name)
)

g233_status <- data.table(
  geo = "GSE233885",
  extracted_file_count = nrow(g233_inventory),
  rds_file_count = length(g233_rds_files),
  successfully_read_rds = sum(
    g233_rds_audit$read_status == "OK"
  ),
  rat_id_confirmed = g233_rat_confirmed,
  timepoint_confirmed = g233_time_confirmed,
  projection_or_retrograde_field_confirmed = g233_projection_confirmed,
  readiness = if (
    g233_projection_confirmed &&
    g233_rat_confirmed
  ) {
    "READY_FOR_PROJECTION_ANALYSIS"
  } else if (
    g233_rat_confirmed &&
    g233_time_confirmed
  ) {
    "RAT_AND_TIME_CONFIRMED_PROJECTION_FIELD_NOT_FOUND"
  } else {
    "CRITICAL_METADATA_INCOMPLETE"
  },
  interpretation = if (g233_projection_confirmed) {
    paste(
      "RDS中检测到投射或retrograde/barcode相关metadata字段。",
      "后续仍需逐列人工确认哪个字段是真正的projection target标签。"
    )
  } else {
    paste(
      "RDS可读取，但当前未检测到明确projection target或retrograde barcode列。",
      "仍可分析长期移植物细胞状态；",
      "若要做投射监督模型，需要从论文补充资料或作者注释中继续寻找标签。"
    )
  }
)

saveRDS(
  g233_cell_metadata,
  file.path(
    objects_dir,
    "00B_GSE233885_candidate_cell_metadata.rds"
  ),
  compress = TRUE
)

log_message(
  "GSE233885结果：",
  g233_status$readiness
)

if (nrow(g233_candidate_values) == 0L) {
  log_message(
    "GSE233885没有检测到候选projection/retrograde/barcode metadata列；",
    "不会再误判为READY_FOR_PROJECTION_ANALYSIS。"
  )
}

log_message("开始GSE178265外部metadata目录检查。")

g178_files <- list.files(
  g178_external,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

g178_text_files <- g178_files[
  grepl(
    "\\.(csv|tsv|txt)(\\.gz)?$",
    g178_files,
    ignore.case = TRUE
  )
]

if (length(g178_text_files) > 0L) {
  future::plan(
    future::multisession,
    workers = max(1L, min(N_WORKERS_LIGHT, 2L))
  )

  g178_headers_list <- future_lapply(
    g178_text_files,
    inspect_text_header,
    future.seed = TRUE
  )

  future::plan(future::sequential)

  g178_headers <- rbindlist(
    g178_headers_list,
    fill = TRUE
  )
} else {
  g178_headers <- data.table(
    file_path = character(),
    file_name = character(),
    extension = character(),
    size_bytes = numeric(),
    size_human = character(),
    header_status = character(),
    separator = character(),
    estimated_n_columns = integer(),
    first_columns = character(),
    first_lines_preview = character()
  )
}

g178_search_text <- paste(
  g178_headers$file_name,
  g178_headers$first_columns,
  collapse = " | "
)

g178_has_barcode <- grepl(
  "barcode|cell.?id|cell.?name",
  g178_search_text,
  ignore.case = TRUE,
  perl = TRUE
)

g178_has_donor <- grepl(
  "donor|subject|individual",
  g178_search_text,
  ignore.case = TRUE,
  perl = TRUE
)

g178_has_disease <- grepl(
  "disease|diagnosis|pd|control|case",
  g178_search_text,
  ignore.case = TRUE,
  perl = TRUE
)

g178_has_region <- grepl(
  "region|snpc|vta|midbrain|substantia",
  g178_search_text,
  ignore.case = TRUE,
  perl = TRUE
)

g178_has_celltype <- grepl(
  "cell.?type|annotation|cluster|subtype|agtr1",
  g178_search_text,
  ignore.case = TRUE,
  perl = TRUE
)

g178_ready <- (
  length(g178_text_files) > 0L &&
  g178_has_barcode &&
  g178_has_donor &&
  g178_has_disease &&
  g178_has_celltype
)

g178_status <- data.table(
  geo = "GSE178265",
  external_metadata_folder = g178_external,
  external_files_found = length(g178_files),
  text_metadata_files_found = length(g178_text_files),
  barcode_column_clue = g178_has_barcode,
  donor_column_clue = g178_has_donor,
  disease_column_clue = g178_has_disease,
  region_column_clue = g178_has_region,
  celltype_or_subtype_clue = g178_has_celltype,
  readiness = if (g178_ready) {
    "READY_FOR_BARCODE_MATCH_TEST"
  } else {
    "EXTERNAL_CELL_METADATA_STILL_REQUIRED"
  },
  interpretation = if (g178_ready) {
    paste(
      "检测到barcode、donor、disease和cell type/subtype字段线索。",
      "下一步需要与GSE178265_Homo_bcd.tsv.gz进行精确barcode匹配。"
    )
  } else {
    paste(
      "当前02_external_metadata目录中仍缺少完整的细胞级注释。",
      "需要从Broad Single Cell Portal SCP1768的Download页面取得metadata，",
      "至少包含barcode、donor、PD/control、region和cell type/DA subtype。"
    )
  }
)

instruction_file <- file.path(
  reports_dir,
  "00B_GSE178265_external_metadata_instructions.txt"
)

instruction_lines <- c(
  "GSE178265 外部细胞级metadata要求",
  paste0("生成时间：", timestamp_now()),
  "",
  "请打开Broad Single Cell Portal研究：SCP1768",
  paste0(
    "https://singlecell.broadinstitute.org/single_cell/study/",
    "SCP1768/single-cell-genomic-profiling-of-human-dopamine-neurons-",
    "identifies-a-population-that-selectively-degenerates-in-parkinsons-",
    "disease-single-nuclei-data"
  ),
  "",
  "在Download页面优先下载metadata / cell annotation文件。",
  "下载后不要放进00_downloaded，而是放到：",
  g178_external,
  "",
  "至少需要的字段：",
  "1. barcode或cell ID",
  "2. donor / subject",
  "3. PD或control",
  "4. brain region（SNpc/VTA等）",
  "5. cell type",
  "6. DA subtype / cluster（最好包含AGTR1等亚型）",
  "",
  "放入文件后，重新运行本00B脚本即可自动检查。",
  "",
  "注意：00B不会读取4.74 GB表达矩阵，因此不会造成内存爆炸。"
)

writeLines(
  instruction_lines,
  instruction_file,
  useBytes = TRUE
)

log_message(
  "GSE178265结果：",
  g178_status$readiness
)

overall_status <- rbindlist(
  list(
    g200_status[, .(
      geo,
      readiness,
      interpretation
    )],
    g233_status[, .(
      geo,
      readiness,
      interpretation
    )],
    g178_status[, .(
      geo,
      readiness,
      interpretation
    )]
  ),
  fill = TRUE
)

overall_status[, can_enter_formal_analysis := fifelse(
  geo == "GSE200610",
  readiness %in% c(
    "READY_FOR_CLONE_AWARE_ANALYSIS",
    "BARCODE_EXPERIMENT_CLUE_BUT_MAPPING_NOT_CONFIRMED",
    "COUNT_MATRICES_ONLY_NO_CLONE_MAPPING_FOUND"
  ),
  fifelse(
    geo == "GSE233885",
    readiness %in% c(
      "READY_FOR_PROJECTION_ANALYSIS",
      "RAT_AND_TIME_CONFIRMED_PROJECTION_FIELD_NOT_FOUND"
    ),
    readiness == "READY_FOR_BARCODE_MATCH_TEST"
  )
)]

overall_status[, formal_analysis_scope := fifelse(
  geo == "GSE200610" &
  readiness != "READY_FOR_CLONE_AWARE_ANALYSIS",
  "可做状态与安全风险分析；暂不做clone-aware模型",
  fifelse(
    geo == "GSE233885" &
    readiness != "READY_FOR_PROJECTION_ANALYSIS",
    "可做长期移植物分析；暂不做projection监督模型",
    fifelse(
      geo == "GSE178265" &
      readiness != "READY_FOR_BARCODE_MATCH_TEST",
      "暂不能建立天然DA易损性参考；需补metadata",
      "可按完整设计继续"
    )
  )
)]

fwrite(
  g200_inventory,
  file.path(
    metadata_dir,
    "00B_GSE200610_file_inventory.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_headers,
  file.path(
    metadata_dir,
    "00B_GSE200610_header_audit.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_candidate_files,
  file.path(
    metadata_dir,
    "00B_GSE200610_candidate_barcode_files.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_status,
  file.path(
    metadata_dir,
    "00B_GSE200610_status.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_inventory,
  file.path(
    metadata_dir,
    "00B_GSE233885_file_inventory.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_rds_audit,
  file.path(
    metadata_dir,
    "00B_GSE233885_RDS_metadata_audit.csv"
  ),
  bom = TRUE
)

g233_candidate_values <- ensure_table_schema(
  g233_candidate_values,
  list(
    file_name = character(),
    category = character(),
    column = character(),
    unique_values_preview = character(),
    n_unique = integer()
  )
)

fwrite(
  g233_candidate_values,
  file.path(
    metadata_dir,
    "00B_GSE233885_candidate_column_values.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_status,
  file.path(
    metadata_dir,
    "00B_GSE233885_status.csv"
  ),
  bom = TRUE
)

fwrite(
  g178_headers,
  file.path(
    metadata_dir,
    "00B_GSE178265_external_metadata_audit.csv"
  ),
  bom = TRUE
)

fwrite(
  g178_status,
  file.path(
    metadata_dir,
    "00B_GSE178265_status.csv"
  ),
  bom = TRUE
)

fwrite(
  overall_status,
  file.path(
    metadata_dir,
    "00B_overall_critical_metadata_status.csv"
  ),
  bom = TRUE
)

wb <- createWorkbook()

write_sheet_safe(
  wb,
  "overall_status",
  overall_status
)

write_sheet_safe(
  wb,
  "GSE200610_status",
  g200_status
)

write_sheet_safe(
  wb,
  "GSE200610_files",
  g200_inventory
)

write_sheet_safe(
  wb,
  "GSE200610_headers",
  g200_headers
)

write_sheet_safe(
  wb,
  "GSE200610_candidates",
  g200_candidate_files,
  empty_message = "No clone/barcode candidate file was detected in the public GSE200610 TAR."
)

write_sheet_safe(
  wb,
  "GSE233885_status",
  g233_status
)

write_sheet_safe(
  wb,
  "GSE233885_RDS",
  g233_rds_audit
)

write_sheet_safe(
  wb,
  "GSE233885_values",
  g233_candidate_values,
  empty_message = "No projection/retrograde/barcode candidate metadata column was detected."
)

write_sheet_safe(
  wb,
  "GSE178265_status",
  g178_status
)

write_sheet_safe(
  wb,
  "GSE178265_external",
  g178_headers,
  empty_message = "No external GSE178265 cell-level metadata has been placed in 02_external_metadata."
)

saveWorkbook(
  wb,
  file.path(
    metadata_dir,
    "00B_critical_metadata_audit.xlsx"
  ),
  overwrite = TRUE
)

report_lines <- c(
  "PD干细胞治疗项目｜00B关键元数据审计",
  paste0("生成时间：", timestamp_now()),
  paste0("项目路径：", PROJECT_ROOT),
  "",
  "一、总体状态",
  paste(
    overall_status$geo,
    overall_status$readiness,
    sep = " : "
  ),
  "",
  "二、可进入正式分析的范围",
  paste(
    overall_status$geo,
    overall_status$formal_analysis_scope,
    sep = " : "
  ),
  "",
  "三、关键解释",
  paste(
    overall_status$geo,
    overall_status$interpretation,
    sep = " : "
  ),
  "",
  "四、GSE178265外部metadata目录",
  g178_external,
  "",
  "五、GSE233885候选cell metadata对象",
  file.path(
    objects_dir,
    "00B_GSE233885_candidate_cell_metadata.rds"
  )
)

writeLines(
  report_lines,
  file.path(
    reports_dir,
    "00B_critical_metadata_report.txt"
  ),
  useBytes = TRUE
)

cat("\n")
cat("============================================================\n")
cat("00B 关键元数据审计完成\n")
cat("============================================================\n")

print(
  overall_status[
    ,
    .(
      geo,
      readiness,
      can_enter_formal_analysis,
      formal_analysis_scope
    )
  ]
)

cat("\n主要输出：\n")
cat(
  file.path(
    metadata_dir,
    "00B_critical_metadata_audit.xlsx"
  ),
  "\n"
)
cat(
  file.path(
    metadata_dir,
    "00B_overall_critical_metadata_status.csv"
  ),
  "\n"
)
cat(
  file.path(
    reports_dir,
    "00B_critical_metadata_report.txt"
  ),
  "\n"
)
cat(
  instruction_file,
  "\n"
)

cat("\n下一步判定：\n")
cat(
  "1. GSE200610：根据结果决定是否保留clone-aware分析。\n"
)
cat(
  "2. GSE233885：根据检测到的metadata列决定是否可做projection监督分析。\n"
)
cat(
  "3. GSE178265：若仍显示EXTERNAL_CELL_METADATA_STILL_REQUIRED，",
  "先补Broad SCP1768 metadata，再进入正式分析。\n",
  sep = ""
)

future::plan(future::sequential)
gc(verbose = FALSE)
