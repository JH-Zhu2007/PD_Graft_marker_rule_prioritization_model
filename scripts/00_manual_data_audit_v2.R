# ==============================================================================
# 00_manual_data_audit_v2.R
# 项目：PD干细胞治疗“功能身份—疾病脆弱性悖论”
# 目的：审计手动下载的7个GEO数据，不下载数据、不做正式Seurat分析
# v2修复：允许00_raw_data为空时正常生成文件夹和缺失文件报告
# 适用：Windows + 32 GB RAM + 手动下载GEO
# ==============================================================================

# ----------------------------- 0. 用户只改这里 --------------------------------
PROJECT_ROOT <- "D:/PD_Graft_Project"

# 轻量文件检查允许4核；读取大型矩阵/RDS仍然顺序执行
N_WORKERS_LIGHT <- 4L

# 4.7 GB文件计算MD5会很慢，默认关闭
CALCULATE_MD5 <- FALSE

# 是否自动安装缺少的CRAN包
AUTO_INSTALL_CRAN <- TRUE
# ==============================================================================


# ----------------------------- 1. 基础设置 -------------------------------------
options(stringsAsFactors = FALSE)
options(timeout = 600)
set.seed(20260713)

required_cran <- c(
  "data.table",
  "openxlsx",
  "future",
  "future.apply",
  "digest"
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
      "\n请先运行 install.packages(c(",
      paste(sprintf('"%s"', missing_cran), collapse = ", "),
      "))"
    )
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(future)
  library(future.apply)
  library(digest)
})

if (!dir.exists(PROJECT_ROOT)) {
  dir.create(PROJECT_ROOT, recursive = TRUE, showWarnings = FALSE)
}

PROJECT_ROOT <- normalizePath(
  PROJECT_ROOT,
  winslash = "/",
  mustWork = TRUE
)

dirs <- c(
  "00_raw_data",
  "01_metadata",
  "02_objects",
  "03_tables",
  "04_figures",
  "05_models",
  "06_reports",
  "07_scripts",
  "08_github",
  "09_manuscript"
)

invisible(lapply(
  file.path(PROJECT_ROOT, dirs),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

geo_ids <- c(
  "GSE178265",
  "GSE157783",
  "GSE204795",
  "GSE204796",
  "GSE132758",
  "GSE200610",
  "GSE233885"
)

# 每个GEO都建立“原始下载”和“解压后”两个子文件夹
for (geo in geo_ids) {
  dir.create(
    file.path(PROJECT_ROOT, "00_raw_data", geo, "00_downloaded"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(PROJECT_ROOT, "00_raw_data", geo, "01_extracted"),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

message("项目根目录：", PROJECT_ROOT)
message("00阶段开始：只审计文件，不进行正式分析。")


# ----------------------------- 2. 官方文件清单 ---------------------------------
# exact_file：NCBI GEO页面列出的系列级补充文件
# external_metadata：GEO本身不足以完成本项目时，需要额外手动下载的注释
dataset_spec <- data.table(
  geo = geo_ids,
  data_type = c(
    "human/multi-species snRNA-seq",
    "human midbrain snRNA-seq",
    "bulk RNA-seq",
    "hPSC differentiation + graft scRNA-seq",
    "12-month graft scRNA-seq",
    "graft snRNA-seq + molecular barcode study",
    "graft snRNA-seq + retrograde projection barcode"
  ),
  expected_biological_unit = c(
    "human donor",
    "human donor",
    "differentiation batch",
    "time point / graft batch",
    "rat",
    "rat / clone（取决于公开映射）",
    "rat"
  ),
  exact_geo_file = c(
    paste(
      "GSE178265_Homo_bcd.tsv.gz",
      "GSE178265_Homo_features.tsv.gz",
      "GSE178265_Homo_matrix.mtx.gz",
      sep = " | "
    ),
    paste(
      "GSE157783_IPDCO_hg_midbrain_UMI.tar.gz",
      "GSE157783_IPDCO_hg_midbrain_cell.tar.gz",
      "GSE157783_IPDCO_hg_midbrain_genes.tar.gz",
      sep = " | "
    ),
    "GSE204795_bulk_dds.RDS.gz",
    "GSE204796_RAW.tar",
    "GSE132758_RAW.tar",
    "GSE200610_RAW.tar",
    "GSE233885_RAW.tar"
  ),
  external_metadata_needed = c(
    TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE
  ),
  critical_external_metadata = c(
    paste(
      "必须另行取得与barcode逐一对应的细胞/供者注释；",
      "优先从Broad Single Cell Portal SCP1768下载metadata，",
      "至少含donor、PD/control、region、cell type/DA subtype"
    ),
    "GEO的cell注释压缩包应提供主要细胞信息；后续仍核对donor和PD/control",
    "DESeq2对象的colData应包含group、day、batch",
    "文件名/对象metadata应区分Day8/14/21/28/35及4个月移植物组",
    "文件名至少可识别rat11、rat39、rat45及FACS状态",
    "必须检查公开文件是否真的含cell-to-clone/barcode映射；没有则降级为状态分析",
    "必须检查RDS metadata是否含projection target/retrograde barcode；没有则不能做投射监督模型"
  ),
  project_role = c(
    "天然DA易损/耐受参考",
    "独立PD患者验证",
    "bulk产品级转化",
    "分化轨迹与早期移植物",
    "长期移植物验证",
    "谱系与安全风险",
    "功能投射能力"
  )
)


# ----------------------------- 3. 辅助函数 -------------------------------------
detect_compound_extension <- function(path) {
  nm <- tolower(basename(path))
  patterns <- c(
    "\\.matrix\\.mtx\\.gz$" = "matrix.mtx.gz",
    "\\.mtx\\.gz$"        = "mtx.gz",
    "\\.tsv\\.gz$"        = "tsv.gz",
    "\\.csv\\.gz$"        = "csv.gz",
    "\\.txt\\.gz$"        = "txt.gz",
    "\\.tar\\.gz$"        = "tar.gz",
    "\\.rds\\.gz$"        = "rds.gz",
    "\\.tar$"             = "tar",
    "\\.rds$"             = "rds",
    "\\.mtx$"             = "mtx",
    "\\.tsv$"             = "tsv",
    "\\.csv$"             = "csv",
    "\\.txt$"             = "txt",
    "\\.xlsx$"            = "xlsx",
    "\\.xls$"             = "xls",
    "\\.h5ad$"            = "h5ad",
    "\\.h5$"              = "h5"
  )

  for (pat in names(patterns)) {
    if (grepl(pat, nm, perl = TRUE)) {
      return(unname(patterns[[pat]]))
    }
  }
  tools::file_ext(nm)
}


human_size <- function(bytes) {
  if (is.na(bytes)) return(NA_character_)
  units <- c("B", "KB", "MB", "GB", "TB")
  idx <- 1L
  value <- as.numeric(bytes)
  while (value >= 1024 && idx < length(units)) {
    value <- value / 1024
    idx <- idx + 1L
  }
  sprintf("%.2f %s", value, units[idx])
}


find_geo_from_path <- function(path, geo_ids) {
  hit <- geo_ids[vapply(
    geo_ids,
    function(g) grepl(g, path, fixed = TRUE, ignore.case = TRUE),
    logical(1)
  )]
  if (length(hit) == 0L) return(NA_character_)
  hit[[1]]
}


safe_read_first_lines <- function(path, n = 5L) {
  con <- NULL
  out <- tryCatch({
    if (grepl("\\.gz$", path, ignore.case = TRUE)) {
      con <- gzfile(path, open = "rt")
    } else {
      con <- file(path, open = "rt")
    }
    readLines(con, n = n, warn = FALSE)
  }, error = function(e) {
    paste0("ERROR: ", conditionMessage(e))
  }, finally = {
    if (!is.null(con)) {
      try(close(con), silent = TRUE)
    }
  })
  out
}


read_mtx_header <- function(path) {
  con <- NULL
  result <- list(
    matrix_rows = NA_real_,
    matrix_cols = NA_real_,
    matrix_nnz  = NA_real_,
    mtx_header_status = "NOT_TESTED"
  )

  tryCatch({
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
      gzfile(path, open = "rt")
    } else {
      file(path, open = "rt")
    }

    first <- readLines(con, n = 1L, warn = FALSE)
    if (length(first) == 0L || !grepl("^%%MatrixMarket", first)) {
      result$mtx_header_status <- "INVALID_MATRIX_MARKET_HEADER"
      return(result)
    }

    repeat {
      ln <- readLines(con, n = 1L, warn = FALSE)
      if (length(ln) == 0L) break
      if (!grepl("^%", ln)) {
        dims <- strsplit(trimws(ln), "\\s+")[[1]]
        if (length(dims) >= 3L) {
          result$matrix_rows <- suppressWarnings(as.numeric(dims[1]))
          result$matrix_cols <- suppressWarnings(as.numeric(dims[2]))
          result$matrix_nnz  <- suppressWarnings(as.numeric(dims[3]))
          result$mtx_header_status <- "OK"
        } else {
          result$mtx_header_status <- "DIMENSION_LINE_NOT_FOUND"
        }
        break
      }
    }
  }, error = function(e) {
    result$mtx_header_status <- paste0("ERROR: ", conditionMessage(e))
  }, finally = {
    if (!is.null(con)) {
      try(close(con), silent = TRUE)
    }
  })

  result
}


inspect_archive <- function(path) {
  result <- list(
    archive_status = "NOT_ARCHIVE",
    archive_n_files = NA_integer_,
    archive_preview = NA_character_
  )

  if (!grepl("\\.(tar|tar\\.gz)$", path, ignore.case = TRUE)) {
    return(result)
  }

  members <- tryCatch(
    utils::untar(path, list = TRUE),
    error = function(e) structure(character(0), error_message = conditionMessage(e))
  )

  if (length(members) == 0L) {
    err <- attr(members, "error_message")
    result$archive_status <- if (is.null(err)) "EMPTY_OR_UNREADABLE" else paste0("ERROR: ", err)
    return(result)
  }

  result$archive_status <- "OK"
  result$archive_n_files <- length(members)
  result$archive_preview <- paste(head(members, 12L), collapse = " | ")
  result
}


inspect_delimited_header <- function(path) {
  result <- list(
    table_status = "NOT_TABLE",
    table_ncol_preview = NA_integer_,
    table_columns_preview = NA_character_
  )

  if (!grepl("\\.(csv|tsv|txt)(\\.gz)?$", path, ignore.case = TRUE)) {
    return(result)
  }

  sep <- if (grepl("\\.csv(\\.gz)?$", path, ignore.case = TRUE)) "," else "\t"

  x <- tryCatch(
    data.table::fread(
      path,
      nrows = 5L,
      sep = sep,
      header = TRUE,
      showProgress = FALSE,
      data.table = TRUE
    ),
    error = function(e) e
  )

  if (inherits(x, "error")) {
    result$table_status <- paste0("ERROR: ", conditionMessage(x))
    return(result)
  }

  result$table_status <- "OK"
  result$table_ncol_preview <- ncol(x)
  result$table_columns_preview <- paste(head(names(x), 30L), collapse = " | ")
  result
}


extract_rat_id <- function(x) {
  x_low <- tolower(x)

  # 优先抓取明确的rat编号，例如rat11、rat7A、rat7A1、ratSD15
  m1 <- regexpr("rat(?:sd)?[0-9]+[a-z0-9]*", x_low, perl = TRUE)
  out <- ifelse(
    m1 > 0,
    regmatches(x_low, m1),
    NA_character_
  )

  # GSE200610部分文件只写SD-no12 / Nude-no12
  m2 <- regexpr("(?:sd|nude)-?no[0-9]+", x_low, perl = TRUE)
  out2 <- ifelse(
    m2 > 0,
    regmatches(x_low, m2),
    NA_character_
  )

  out[is.na(out)] <- out2[is.na(out)]
  out
}


extract_timepoint <- function(x) {
  x_low <- tolower(x)
  fifelse(
    grepl("12m|12month", x_low), "12m",
    fifelse(
      grepl("9m|9month", x_low), "9m",
      fifelse(
        grepl("4m|4month", x_low), "4m",
        fifelse(
          grepl("day[ _-]?35|d35", x_low), "Day35",
          fifelse(
            grepl("day[ _-]?28|d28", x_low), "Day28",
            fifelse(
              grepl("day[ _-]?21|d21", x_low), "Day21",
              fifelse(
                grepl("day[ _-]?14|d14", x_low), "Day14",
                fifelse(
                  grepl("day[ _-]?8|d8", x_low), "Day8",
                  NA_character_
                )
              )
            )
          )
        )
      )
    )
  )
}


inspect_rds_object <- function(path) {
  result <- data.table(
    file_path = path,
    rds_status = "NOT_TESTED",
    rds_class = NA_character_,
    n_features = NA_real_,
    n_cells = NA_real_,
    metadata_columns = NA_character_,
    assay_names = NA_character_
  )

  # .RDS.gz可直接通过gzfile读取
  obj <- tryCatch({
    if (grepl("\\.gz$", path, ignore.case = TRUE)) {
      readRDS(gzfile(path, open = "rb"))
    } else {
      readRDS(path)
    }
  }, error = function(e) e)

  if (inherits(obj, "error")) {
    result$rds_status <- paste0("ERROR: ", conditionMessage(obj))
    return(result)
  }

  result$rds_status <- "OK"
  result$rds_class <- paste(class(obj), collapse = " | ")

  # DESeqDataSet / SummarizedExperiment
  if (inherits(obj, "SummarizedExperiment") ||
      inherits(obj, "DESeqDataSet")) {
    result$n_features <- nrow(obj)
    result$n_cells <- ncol(obj)
    cd <- tryCatch(as.data.frame(SummarizedExperiment::colData(obj)), error = function(e) NULL)
    if (!is.null(cd)) {
      result$metadata_columns <- paste(names(cd), collapse = " | ")
    }
    an <- tryCatch(SummarizedExperiment::assayNames(obj), error = function(e) character(0))
    result$assay_names <- paste(an, collapse = " | ")
  }

  # Seurat对象：不触发正式分析，仅查看基本结构
  if (inherits(obj, "Seurat")) {
    result$n_features <- tryCatch(nrow(obj), error = function(e) NA_real_)
    result$n_cells <- tryCatch(ncol(obj), error = function(e) NA_real_)
    md <- tryCatch(obj[[]], error = function(e) NULL)
    if (!is.null(md)) {
      result$metadata_columns <- paste(names(md), collapse = " | ")
    }
    an <- tryCatch(names(obj@assays), error = function(e) character(0))
    result$assay_names <- paste(an, collapse = " | ")
  }

  # SingleCellExperiment
  if (inherits(obj, "SingleCellExperiment")) {
    result$n_features <- nrow(obj)
    result$n_cells <- ncol(obj)
    cd <- tryCatch(as.data.frame(SummarizedExperiment::colData(obj)), error = function(e) NULL)
    if (!is.null(cd)) {
      result$metadata_columns <- paste(names(cd), collapse = " | ")
    }
    an <- tryCatch(SummarizedExperiment::assayNames(obj), error = function(e) character(0))
    result$assay_names <- paste(an, collapse = " | ")
  }

  # 普通矩阵或data.frame
  if (is.matrix(obj) || inherits(obj, "Matrix")) {
    result$n_features <- nrow(obj)
    result$n_cells <- ncol(obj)
  }
  if (is.data.frame(obj)) {
    result$n_features <- nrow(obj)
    result$n_cells <- ncol(obj)
    result$metadata_columns <- paste(names(obj), collapse = " | ")
  }

  rm(obj)
  gc(verbose = FALSE)
  result
}


# ----------------------------- 4. 扫描全部本地文件 -----------------------------
all_files <- list.files(
  file.path(PROJECT_ROOT, "00_raw_data"),
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE,
  all.files = FALSE
)

if (length(all_files) == 0L) {
  warning(
    "00_raw_data中还没有文件。\n",
    "请把你手动下载的文件放到对应GEO/00_downloaded目录，",
    "然后重新运行本脚本。"
  )
}

file_inventory <- data.table(
  file_path = normalizePath(all_files, winslash = "/", mustWork = FALSE)
)

file_inventory[, file_name := basename(file_path)]
file_inventory[, geo := vapply(file_path, find_geo_from_path, character(1), geo_ids = geo_ids)]
file_inventory[, extension := vapply(file_path, detect_compound_extension, character(1))]
file_inventory[, size_bytes := file.info(file_path)$size]
file_inventory[, size_human := vapply(size_bytes, human_size, character(1))]
file_inventory[, modified_time := as.character(file.info(file_path)$mtime)]
file_inventory[, location_type := fifelse(
  grepl("/00_downloaded/", file_path, fixed = TRUE),
  "downloaded_archive_or_file",
  fifelse(
    grepl("/01_extracted/", file_path, fixed = TRUE),
    "extracted",
    "other"
  )
)]

if (CALCULATE_MD5 && nrow(file_inventory) > 0L) {
  message("正在计算MD5；大型文件可能需要较长时间……")
  file_inventory[, md5 := vapply(
    file_path,
    function(p) digest::digest(file = p, algo = "md5"),
    character(1)
  )]
} else {
  file_inventory[, md5 := NA_character_]
}

# 即使00_raw_data暂时为空，也预先建立后续会使用的检查列。
# 这样第一次运行只负责创建文件夹时，不会因为“列不存在”而中断。
inspection_defaults <- list(
  archive_status = character(),
  archive_n_files = integer(),
  archive_preview = character(),
  table_status = character(),
  table_ncol_preview = integer(),
  table_columns_preview = character(),
  matrix_rows = numeric(),
  matrix_cols = numeric(),
  matrix_nnz = numeric(),
  mtx_header_status = character()
)

for (nm in names(inspection_defaults)) {
  if (!nm %in% names(file_inventory)) {
    template <- inspection_defaults[[nm]]
    if (is.integer(template)) {
      file_inventory[, (nm) := NA_integer_]
    } else if (is.numeric(template)) {
      file_inventory[, (nm) := NA_real_]
    } else {
      file_inventory[, (nm) := NA_character_]
    }
  }
}


# ----------------------------- 5. 轻量并行检查 ---------------------------------
future::plan(
  future::multisession,
  workers = min(N_WORKERS_LIGHT, 4L)
)

message("轻量并行检查：", min(N_WORKERS_LIGHT, 4L), "核")

light_results <- future_lapply(
  seq_len(nrow(file_inventory)),
  function(i) {
    p <- file_inventory$file_path[i]
    ext <- file_inventory$extension[i]

    archive_info <- inspect_archive(p)
    table_info <- inspect_delimited_header(p)

    mtx_info <- if (ext %in% c("matrix.mtx.gz", "mtx.gz", "mtx")) {
      read_mtx_header(p)
    } else {
      list(
        matrix_rows = NA_real_,
        matrix_cols = NA_real_,
        matrix_nnz = NA_real_,
        mtx_header_status = "NOT_MTX"
      )
    }

    list(
      archive_status = archive_info$archive_status,
      archive_n_files = archive_info$archive_n_files,
      archive_preview = archive_info$archive_preview,
      table_status = table_info$table_status,
      table_ncol_preview = table_info$table_ncol_preview,
      table_columns_preview = table_info$table_columns_preview,
      matrix_rows = mtx_info$matrix_rows,
      matrix_cols = mtx_info$matrix_cols,
      matrix_nnz = mtx_info$matrix_nnz,
      mtx_header_status = mtx_info$mtx_header_status
    )
  },
  future.seed = TRUE
)

future::plan(future::sequential)

if (length(light_results) > 0L) {
  light_dt <- rbindlist(light_results, fill = TRUE)
  for (nm in names(light_dt)) {
    file_inventory[, (nm) := light_dt[[nm]]]
  }
}


# ----------------------------- 6. RDS顺序检查 ----------------------------------
# 不能并行读取多个RDS，避免32GB内存下复制对象导致abort
rds_files <- file_inventory[
  extension %in% c("rds", "rds.gz"),
  file_path
]

rds_audit <- data.table(
  file_path = character(),
  rds_status = character(),
  rds_class = character(),
  n_features = numeric(),
  n_cells = numeric(),
  metadata_columns = character(),
  assay_names = character(),
  geo = character(),
  rat_id_from_name = character(),
  timepoint_from_name = character()
)

if (length(rds_files) > 0L) {
  message("开始顺序检查RDS，共 ", length(rds_files), " 个。")
  for (i in seq_along(rds_files)) {
    message("[RDS ", i, "/", length(rds_files), "] ", basename(rds_files[i]))
    one <- inspect_rds_object(rds_files[i])
    one[, geo := find_geo_from_path(file_path, geo_ids)]
    one[, rat_id_from_name := extract_rat_id(basename(file_path))]
    one[, timepoint_from_name := extract_timepoint(basename(file_path))]
    rds_audit <- rbind(rds_audit, one, fill = TRUE)
    gc(verbose = FALSE)
  }
}


# ----------------------------- 7. 数据集完整性判断 -----------------------------
check_exact_file <- function(geo, expected_string, inventory) {
  expected <- trimws(strsplit(expected_string, "\\|")[[1]])
  # data.table作用域避免冲突，使用普通向量筛选
  observed_all <- inventory$file_name[inventory$geo == geo]

  hit <- vapply(
    expected,
    function(x) any(tolower(observed_all) == tolower(x)),
    logical(1)
  )

  data.table(
    geo = geo,
    expected_file = expected,
    found = hit
  )
}

exact_checks <- rbindlist(
  lapply(seq_len(nrow(dataset_spec)), function(i) {
    check_exact_file(
      geo = dataset_spec$geo[i],
      expected_string = dataset_spec$exact_geo_file[i],
      inventory = file_inventory
    )
  }),
  fill = TRUE
)

geo_summary <- exact_checks[
  ,
  .(
    expected_file_count = .N,
    found_exact_count = sum(found),
    missing_exact_files = paste(expected_file[!found], collapse = " | ")
  ),
  by = geo
]

# 如果用户已经解压但没有保留tar，也把“有解压文件”视为部分可用
extracted_summary <- file_inventory[
  ,
  .(
    n_local_files = .N,
    n_downloaded_files = sum(location_type == "downloaded_archive_or_file"),
    n_extracted_files = sum(location_type == "extracted"),
    total_size_bytes = sum(size_bytes, na.rm = TRUE)
  ),
  by = geo
]

geo_status <- merge(
  dataset_spec,
  geo_summary,
  by = "geo",
  all.x = TRUE
)

geo_status <- merge(
  geo_status,
  extracted_summary,
  by = "geo",
  all.x = TRUE
)

geo_status[
  is.na(n_local_files),
  `:=`(
    n_local_files = 0L,
    n_downloaded_files = 0L,
    n_extracted_files = 0L,
    total_size_bytes = 0
  )
]

geo_status[, total_size_human := vapply(total_size_bytes, human_size, character(1))]

geo_status[, geo_files_status := fifelse(
  found_exact_count == expected_file_count,
  "COMPLETE_OFFICIAL_FILES",
  fifelse(
    n_extracted_files > 0L,
    "EXTRACTED_FILES_PRESENT_BUT_OFFICIAL_ARCHIVE_INCOMPLETE",
    "MISSING_FILES"
  )
)]


# ----------------------------- 8. 关键字段审计 ---------------------------------
# 通过文件名、CSV/TSV表头、RDS metadata列名寻找关键字段
all_detectable_text <- paste(
  c(
    file_inventory$file_name,
    file_inventory$table_columns_preview,
    rds_audit$metadata_columns
  ),
  collapse = " | "
)

keyword_spec <- data.table(
  geo = c(
    rep("GSE178265", 4),
    rep("GSE157783", 3),
    rep("GSE204795", 3),
    rep("GSE204796", 3),
    rep("GSE132758", 2),
    rep("GSE200610", 4),
    rep("GSE233885", 4)
  ),
  field = c(
    "donor", "disease/PD-control", "cell type", "DA subtype",
    "donor/sample", "disease/PD-control", "cell type",
    "group", "day", "batch",
    "day/timepoint", "sorted/unsorted group", "graft status",
    "rat ID", "FACS status",
    "rat ID", "clone ID", "molecular barcode", "cell-to-clone mapping",
    "rat ID", "timepoint", "projection target", "retrograde barcode"
  ),
  regex = c(
    "donor|subject|individual", "disease|diagnosis|pd|control|ipd", "cell.?type|annotation|cluster", "subtype|agtr1|sox6|calb1",
    "donor|sample|subject", "disease|diagnosis|pd|control|ipd", "cell.?type|annotation|cluster",
    "group|condition|en1|lmx1a", "day|time", "batch|replicate",
    "day|stage|time", "ptpro|clstn2|sorted|unsorted", "graft|transplant",
    "rat[0-9]+", "facs",
    "rat|sd-no|nude-no", "clone|clonal", "barcode|bc", "cell.?to.?clone|clone.?id",
    "rat[0-9]+", "9m|12m|month", "projection|target|innervat", "retrograde|barcode|aav"
  )
)

keyword_checks <- copy(keyword_spec)

keyword_checks[, detected_anywhere := vapply(
  regex,
  function(pat) grepl(pat, all_detectable_text, ignore.case = TRUE, perl = TRUE),
  logical(1)
)]

# 注意：这是00阶段的“线索检查”，不是最终证明
keyword_checks[, interpretation := fifelse(
  detected_anywhere,
  "DETECTED_AS_TEXT_CLUE_NEEDS_MANUAL_CONFIRMATION",
  "NOT_DETECTED_IN_CURRENT_LOCAL_FILES"
)]


# ----------------------------- 9. 自动生成样本名解析表 -------------------------
sample_name_manifest <- copy(file_inventory)
sample_name_manifest[, rat_id_from_filename := extract_rat_id(file_name)]
sample_name_manifest[, timepoint_from_filename := extract_timepoint(file_name)]
sample_name_manifest[, facs_from_filename := fifelse(
  grepl("facs", file_name, ignore.case = TRUE),
  "FACS",
  NA_character_
)]
sample_name_manifest[, sorted_group_from_filename := fifelse(
  grepl("ptpro", file_name, ignore.case = TRUE),
  "PTPRO_sorted",
  fifelse(
    grepl("clstn2", file_name, ignore.case = TRUE),
    "CLSTN2_sorted",
    fifelse(
      grepl("unsorted", file_name, ignore.case = TRUE),
      "unsorted",
      NA_character_
    )
  )
)]


# ----------------------------- 10. 特殊警报 ------------------------------------
alerts <- data.table(
  level = character(),
  geo = character(),
  message = character()
)

add_alert <- function(level, geo, message) {
  alerts <<- rbind(
    alerts,
    data.table(level = level, geo = geo, message = message)
  )
}

# GSE178265必须额外有annotation metadata
g178_files <- file_inventory[geo == "GSE178265", file_name]
has_g178_annotation <- any(grepl(
  "meta|annotation|cluster|cell.?type",
  g178_files,
  ignore.case = TRUE,
  perl = TRUE
))

if (!has_g178_annotation) {
  add_alert(
    "CRITICAL",
    "GSE178265",
    paste(
      "GEO只检测到表达矩阵/feature/barcode线索，没有检测到可用于",
      "donor、PD/control、DA subtype的细胞级metadata。",
      "必须从Broad Single Cell Portal SCP1768或论文补充材料手动下载annotation metadata。"
    )
  )
}

# GSE200610 clone/barcode
g200_text <- paste(
  c(
    file_inventory[geo == "GSE200610", file_name],
    file_inventory[geo == "GSE200610", table_columns_preview],
    rds_audit[geo == "GSE200610", metadata_columns]
  ),
  collapse = " | "
)

if (!grepl("clone|barcode|bc", g200_text, ignore.case = TRUE)) {
  add_alert(
    "CRITICAL",
    "GSE200610",
    paste(
      "当前本地文件中尚未检测到明确clone/barcode字段。",
      "在确认cell-to-clone映射前，不允许把该数据写成谱系监督机器学习。"
    )
  )
}

# GSE233885 projection
g233_text <- paste(
  c(
    file_inventory[geo == "GSE233885", file_name],
    file_inventory[geo == "GSE233885", table_columns_preview],
    rds_audit[geo == "GSE233885", metadata_columns]
  ),
  collapse = " | "
)

if (!grepl("projection|target|retro|barcode|aav", g233_text, ignore.case = TRUE)) {
  add_alert(
    "CRITICAL",
    "GSE233885",
    paste(
      "当前RDS/文件名中尚未检测到明确projection target或retrograde barcode字段。",
      "在确认投射标签前，不允许建立投射监督模型。"
    )
  )
}

# 大文件警报
huge_files <- file_inventory[size_bytes >= 2 * 1024^3]
if (nrow(huge_files) > 0L) {
  for (i in seq_len(nrow(huge_files))) {
    add_alert(
      "MEMORY",
      huge_files$geo[i],
      paste0(
        "大型文件：", huge_files$file_name[i],
        "（", huge_files$size_human[i], "）。",
        "00阶段只读header；后续必须先按metadata筛选barcode，再构建对象。"
      )
    )
  }
}


# ----------------------------- 11. 总体可进入下一步判断 ------------------------
geo_status[, external_metadata_status := fifelse(
  !external_metadata_needed,
  "NOT_REQUIRED_AT_00",
  fifelse(
    geo == "GSE178265" & has_g178_annotation,
    "PRESENT_CLUE_NEEDS_CONFIRMATION",
    fifelse(
      geo == "GSE200610" & grepl("clone|barcode|bc", g200_text, ignore.case = TRUE),
      "PRESENT_CLUE_NEEDS_CONFIRMATION",
      fifelse(
        geo == "GSE233885" & grepl("projection|target|retro|barcode|aav", g233_text, ignore.case = TRUE),
        "PRESENT_CLUE_NEEDS_CONFIRMATION",
        "NOT_CONFIRMED"
      )
    )
  )
)]

geo_status[, readiness := fifelse(
  geo_files_status == "MISSING_FILES",
  "STOP_MISSING_FILES",
  fifelse(
    external_metadata_needed & external_metadata_status == "NOT_CONFIRMED",
    "PARTIAL_NEEDS_CRITICAL_METADATA",
    "READY_FOR_IMPORT_TEST"
  )
)]


# ----------------------------- 12. 保存CSV和Excel -------------------------------
metadata_dir <- file.path(PROJECT_ROOT, "01_metadata")
report_dir <- file.path(PROJECT_ROOT, "06_reports")

fwrite(
  file_inventory,
  file.path(metadata_dir, "00_all_local_files.csv"),
  bom = TRUE
)

fwrite(
  exact_checks,
  file.path(metadata_dir, "00_expected_file_check.csv"),
  bom = TRUE
)

fwrite(
  geo_status,
  file.path(metadata_dir, "00_dataset_readiness.csv"),
  bom = TRUE
)

fwrite(
  keyword_checks,
  file.path(metadata_dir, "00_key_field_check.csv"),
  bom = TRUE
)

fwrite(
  sample_name_manifest,
  file.path(metadata_dir, "00_filename_manifest.csv"),
  bom = TRUE
)

fwrite(
  rds_audit,
  file.path(metadata_dir, "00_RDS_metadata_audit.csv"),
  bom = TRUE
)

fwrite(
  alerts,
  file.path(metadata_dir, "00_critical_alerts.csv"),
  bom = TRUE
)

wb <- createWorkbook()

addWorksheet(wb, "dataset_spec")
writeDataTable(wb, "dataset_spec", dataset_spec)

addWorksheet(wb, "dataset_readiness")
writeDataTable(wb, "dataset_readiness", geo_status)

addWorksheet(wb, "all_local_files")
writeDataTable(wb, "all_local_files", file_inventory)

addWorksheet(wb, "expected_file_check")
writeDataTable(wb, "expected_file_check", exact_checks)

addWorksheet(wb, "key_field_check")
writeDataTable(wb, "key_field_check", keyword_checks)

addWorksheet(wb, "filename_manifest")
writeDataTable(wb, "filename_manifest", sample_name_manifest)

addWorksheet(wb, "RDS_audit")
writeDataTable(wb, "RDS_audit", rds_audit)

addWorksheet(wb, "critical_alerts")
writeDataTable(wb, "critical_alerts", alerts)

for (sheet in names(wb)) {
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(wb, sheet, cols = 1:50, widths = "auto")
}

saveWorkbook(
  wb,
  file.path(metadata_dir, "00_GEO_dataset_audit.xlsx"),
  overwrite = TRUE
)


# ----------------------------- 13. 生成人类可读报告 -----------------------------
report_lines <- c(
  "PD干细胞治疗项目｜00 手动下载数据审计报告",
  paste0("生成时间：", Sys.time()),
  paste0("项目路径：", PROJECT_ROOT),
  "",
  "一、每个GEO的准备状态",
  paste(
    geo_status$geo,
    geo_status$readiness,
    sep = " : "
  ),
  "",
  "二、严重警报",
  if (nrow(alerts) == 0L) {
    "没有检测到严重警报。"
  } else {
    paste0(
      "[", alerts$level, "] ",
      alerts$geo, " - ",
      alerts$message
    )
  },
  "",
  "三、00阶段判定规则",
  "1. STOP_MISSING_FILES：缺少官方核心文件，不能进入02。",
  "2. PARTIAL_NEEDS_CRITICAL_METADATA：表达矩阵存在，但关键标签未确认。",
  "3. READY_FOR_IMPORT_TEST：可以进入下一阶段的正式读取测试。",
  "",
  "四、重要说明",
  "GSE178265必须有与barcode逐一对应的donor、PD/control、cell type/DA subtype metadata。",
  "GSE200610必须确认cell-to-clone/barcode映射，才能做真正谱系监督分析。",
  "GSE233885必须确认projection target/retrograde barcode，才能做投射监督分析。",
  "任何GEO样本数都不能直接等同于独立供者或独立动物数。"
)

writeLines(
  report_lines,
  file.path(report_dir, "00_feasibility_report.txt"),
  useBytes = TRUE
)


# ----------------------------- 14. 控制台最终结果 -------------------------------
cat("\n")
cat("============================================================\n")
cat("00 手动下载数据审计完成\n")
cat("============================================================\n")

print(
  geo_status[
    ,
    .(
      geo,
      n_local_files,
      total_size_human,
      geo_files_status,
      external_metadata_status,
      readiness
    )
  ]
)

cat("\n严重警报数量：", nrow(alerts), "\n")
if (nrow(alerts) > 0L) {
  print(alerts)
}

cat("\n主要输出：\n")
cat(file.path(metadata_dir, "00_GEO_dataset_audit.xlsx"), "\n")
cat(file.path(metadata_dir, "00_dataset_readiness.csv"), "\n")
cat(file.path(metadata_dir, "00_critical_alerts.csv"), "\n")
cat(file.path(report_dir, "00_feasibility_report.txt"), "\n")

cat("\n下一步判定：\n")
cat("只有对应GEO显示 READY_FOR_IMPORT_TEST，才进入02正式读取测试。\n")

future::plan(future::sequential)
gc(verbose = FALSE)
