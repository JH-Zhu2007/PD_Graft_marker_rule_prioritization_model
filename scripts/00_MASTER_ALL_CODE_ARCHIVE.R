
PROJECT_ROOT <- "D:/PD_Graft_Project"

N_WORKERS_LIGHT <- 4L

CALCULATE_MD5 <- FALSE

AUTO_INSTALL_CRAN <- TRUE

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

  m1 <- regexpr("rat(?:sd)?[0-9]+[a-z0-9]*", x_low, perl = TRUE)
  out <- ifelse(
    m1 > 0,
    regmatches(x_low, m1),
    NA_character_
  )

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

check_exact_file <- function(geo, expected_string, inventory) {
  expected <- trimws(strsplit(expected_string, "\\|")[[1]])

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

keyword_checks[, interpretation := fifelse(
  detected_anywhere,
  "DETECTED_AS_TEXT_CLUE_NEEDS_MANUAL_CONFIRMATION",
  "NOT_DETECTED_IN_CURRENT_LOCAL_FILES"
)]

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

PROJECT_ROOT <- "D:/PD_Graft_Project"

PROXY_URL <- "http://127.0.0.1:7899"

CONNECTIONS_PER_FILE <- 4L

DOWNLOAD_GSE178265_LARGE_MATRIX <- TRUE

RUN_AUDIT_AFTER_DOWNLOAD <- FALSE

options(stringsAsFactors = FALSE)
options(timeout = 600)

if (!dir.exists(PROJECT_ROOT)) {
  dir.create(PROJECT_ROOT, recursive = TRUE, showWarnings = FALSE)
}

PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = TRUE)

geo_ids <- c(
  "GSE178265",
  "GSE157783",
  "GSE204795",
  "GSE204796",
  "GSE132758",
  "GSE200610",
  "GSE233885"
)

for (geo in geo_ids) {
  dir.create(
    file.path(PROJECT_ROOT, "00_raw_data", geo, "00_downloaded"),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

dir.create(
  file.path(PROJECT_ROOT, "06_reports"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  file.path(PROJECT_ROOT, "tools"),
  recursive = TRUE,
  showWarnings = FALSE
)

LOG_FILE <- file.path(
  PROJECT_ROOT,
  "06_reports",
  "00A2_aria2_download_log.txt"
)

STATUS_FILE <- file.path(
  PROJECT_ROOT,
  "06_reports",
  "00A2_aria2_download_status.csv"
)

stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

log_message <- function(...) {
  msg <- paste0(...)
  line <- paste0("[", stamp(), "] ", msg)
  cat(line, "\n")
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

human_size <- function(bytes) {
  if (is.na(bytes)) return(NA_character_)
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  idx <- 1L
  while (value >= 1024 && idx < length(units)) {
    value <- value / 1024
    idx <- idx + 1L
  }
  sprintf("%.2f %s", value, units[idx])
}

curl_candidates <- unique(c(
  Sys.which("curl.exe"),
  Sys.which("curl"),
  "C:/Windows/System32/curl.exe"
))

curl_candidates <- curl_candidates[
  nzchar(curl_candidates) & file.exists(curl_candidates)
]

if (length(curl_candidates) == 0L) {
  stop("没有找到Windows curl.exe。")
}

CURL_BIN <- normalizePath(
  curl_candidates[[1]],
  winslash = "/",
  mustWork = TRUE
)

find_aria2 <- function() {
  candidates <- unique(c(
    Sys.which("aria2c.exe"),
    Sys.which("aria2c"),
    list.files(
      file.path(PROJECT_ROOT, "tools"),
      pattern = "^aria2c\\.exe$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  ))

  candidates <- candidates[
    nzchar(candidates) & file.exists(candidates)
  ]

  if (length(candidates) == 0L) return(NA_character_)

  normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
}

install_aria2_portable <- function() {
  tools_dir <- file.path(PROJECT_ROOT, "tools")
  zip_path <- file.path(tools_dir, "aria2-1.37.0-win64.zip")
  extract_dir <- file.path(tools_dir, "aria2")

  aria2_url <- paste0(
    "https://github.com/aria2/aria2/releases/download/",
    "release-1.37.0/",
    "aria2-1.37.0-win-64bit-build1.zip"
  )

  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(zip_path) || file.info(zip_path)$size < 100000L) {
    log_message("正在从aria2官方GitHub下载便携版工具……")

    args <- c(
      "--location",
      "--fail",
      "--show-error",
      "--retry", "20",
      "--retry-all-errors",
      "--retry-delay", "5",
      "--connect-timeout", "30",
      "--ipv4",
      "--proxy", PROXY_URL,
      "--output", zip_path,
      aria2_url
    )

    status <- system2(
      CURL_BIN,
      args = args,
      stdout = "",
      stderr = ""
    )

    if (!identical(as.integer(status), 0L)) {
      stop(
        "aria2工具下载失败。\n",
        "请稍后重新运行本脚本；已经下载的GEO .part文件不会丢失。"
      )
    }
  }

  log_message("正在解压aria2工具……")

  try(
    unlink(extract_dir, recursive = TRUE, force = TRUE),
    silent = TRUE
  )
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

  unzip(zip_path, exdir = extract_dir)

  aria2_found <- list.files(
    extract_dir,
    pattern = "^aria2c\\.exe$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(aria2_found) == 0L) {
    stop("aria2压缩包已下载，但未找到aria2c.exe。")
  }

  normalizePath(aria2_found[[1]], winslash = "/", mustWork = TRUE)
}

ARIA2_BIN <- find_aria2()

if (is.na(ARIA2_BIN)) {
  ARIA2_BIN <- install_aria2_portable()
}

log_message("使用aria2：", ARIA2_BIN)
log_message("每个文件连接数：", CONNECTIONS_PER_FILE)
log_message("固定代理：", PROXY_URL)

log_message("固定使用Clash HTTP(S)代理：", PROXY_URL)
log_message("该端口已由Windows终端curl测试返回HTTP 200。")

proxy_check_output <- tryCatch(
  system2(
    CURL_BIN,
    args = c(
      "-I",
      "--ipv4",
      "--proxy", PROXY_URL,
      "--connect-timeout", "20",
      "--max-time", "40",
      "https://ftp.ncbi.nlm.nih.gov/"
    ),
    stdout = TRUE,
    stderr = TRUE
  ),
  error = function(e) paste0("R内部curl检查异常：", conditionMessage(e))
)

if (any(grepl("200 Connection established|HTTP/[0-9.]+ 200", proxy_check_output))) {
  log_message("R内部curl也检测到代理连接成功。")
} else {
  log_message(
    "R内部curl未识别到200，但不会中止；",
    "因为你已在Windows终端实测7899返回HTTP 200。"
  )
}

manifest <- data.frame(
  order = 1:11,
  geo = c(
    "GSE204795",
    "GSE157783", "GSE157783", "GSE157783",
    "GSE132758",
    "GSE200610",
    "GSE233885",
    "GSE204796",
    "GSE178265", "GSE178265", "GSE178265"
  ),
  filename = c(
    "GSE204795_bulk_dds.RDS.gz",
    "GSE157783_IPDCO_hg_midbrain_cell.tar.gz",
    "GSE157783_IPDCO_hg_midbrain_genes.tar.gz",
    "GSE157783_IPDCO_hg_midbrain_UMI.tar.gz",
    "GSE132758_RAW.tar",
    "GSE200610_RAW.tar",
    "GSE233885_RAW.tar",
    "GSE204796_RAW.tar",
    "GSE178265_Homo_bcd.tsv.gz",
    "GSE178265_Homo_features.tsv.gz",
    "GSE178265_Homo_matrix.mtx.gz"
  ),
  url = c(
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE204nnn/GSE204795/suppl/GSE204795_bulk_dds.RDS.gz",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE157nnn/GSE157783/suppl/GSE157783_IPDCO_hg_midbrain_cell.tar.gz",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE157nnn/GSE157783/suppl/GSE157783_IPDCO_hg_midbrain_genes.tar.gz",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE157nnn/GSE157783/suppl/GSE157783_IPDCO_hg_midbrain_UMI.tar.gz",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE132nnn/GSE132758/suppl/GSE132758_RAW.tar",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE200nnn/GSE200610/suppl/GSE200610_RAW.tar",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE233nnn/GSE233885/suppl/GSE233885_RAW.tar",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE204nnn/GSE204796/suppl/GSE204796_RAW.tar",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE178nnn/GSE178265/suppl/GSE178265_Homo_bcd.tsv.gz",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE178nnn/GSE178265/suppl/GSE178265_Homo_features.tsv.gz",
    "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE178nnn/GSE178265/suppl/GSE178265_Homo_matrix.mtx.gz"
  ),
  stringsAsFactors = FALSE
)

if (!DOWNLOAD_GSE178265_LARGE_MATRIX) {
  manifest <- manifest[
    manifest$filename != "GSE178265_Homo_matrix.mtx.gz",
    ,
    drop = FALSE
  ]
}

manifest <- manifest[order(manifest$order), , drop = FALSE]

get_remote_size <- function(url) {
  args <- c(
    "-sSIL",
    "--ipv4",
    "--http1.1",
    "--location",
    "--connect-timeout", "30",
    "--max-time", "120",
    "--proxy", PROXY_URL,
    url
  )

  output <- tryCatch(
    system2(
      CURL_BIN,
      args = args,
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) character(0)
  )

  hits <- grep(
    "^content-length\\s*:",
    trimws(tolower(output)),
    value = TRUE
  )

  if (length(hits) == 0L) return(NA_real_)

  sizes <- suppressWarnings(as.numeric(
    sub("^content-length\\s*:\\s*", "", hits)
  ))

  sizes <- sizes[is.finite(sizes) & sizes > 0]

  if (length(sizes) == 0L) return(NA_real_)

  tail(sizes, 1L)
}

download_one <- function(geo, filename, url) {
  dest_dir <- file.path(
    PROJECT_ROOT,
    "00_raw_data",
    geo,
    "00_downloaded"
  )

  final_path <- file.path(dest_dir, filename)
  part_name <- paste0(filename, ".part")
  part_path <- file.path(dest_dir, part_name)
  control_path <- paste0(part_path, ".aria2")

  remote_size <- get_remote_size(url)

  log_message("------------------------------------------------------------")
  log_message("准备下载：", geo, " / ", filename)

  if (is.finite(remote_size)) {
    log_message("远程大小：", human_size(remote_size))
  }

  if (file.exists(final_path)) {
    final_size <- file.info(final_path)$size

    if (!is.finite(remote_size) || final_size == remote_size) {
      log_message("完整文件已存在，跳过：", human_size(final_size))

      return(data.frame(
        geo = geo,
        filename = filename,
        status = "COMPLETE",
        local_size = human_size(final_size),
        path = final_path,
        stringsAsFactors = FALSE
      ))
    }

    if (file.exists(part_path)) {
      if (file.info(final_path)$size > file.info(part_path)$size) {
        file.remove(part_path)
        file.rename(final_path, part_path)
      } else {
        file.remove(final_path)
      }
    } else {
      file.rename(final_path, part_path)
    }
  }

  old_size <- if (file.exists(part_path)) {
    file.info(part_path)$size
  } else {
    0
  }

  log_message("断点起始大小：", human_size(old_size))

  args <- c(
    "--dir", dest_dir,
    "--out", part_name,
    "--continue=true",
    paste0("--max-connection-per-server=", CONNECTIONS_PER_FILE),
    paste0("--split=", CONNECTIONS_PER_FILE),
    "--min-split-size=5M",
    "--max-concurrent-downloads=1",
    "--max-tries=0",
    "--retry-wait=5",
    "--timeout=60",
    "--connect-timeout=30",
    "--lowest-speed-limit=1K",
    "--disable-ipv6=true",
    paste0("--all-proxy=", PROXY_URL),
    "--all-proxy-user=",
    "--all-proxy-passwd=",
    "--file-allocation=none",
    "--auto-file-renaming=false",
    "--allow-overwrite=true",
    "--remote-time=true",
    "--summary-interval=10",
    "--console-log-level=notice",
    url
  )

  status <- tryCatch(
    system2(
      ARIA2_BIN,
      args = args,
      stdout = "",
      stderr = ""
    ),
    error = function(e) {
      log_message("aria2调用错误：", conditionMessage(e))
      999L
    }
  )

  part_size <- if (file.exists(part_path)) {
    file.info(part_path)$size
  } else {
    0
  }

  size_ok <- !is.finite(remote_size) || part_size == remote_size

  if (identical(as.integer(status), 0L) && size_ok) {
    if (file.exists(final_path)) file.remove(final_path)

    ok <- file.rename(part_path, final_path)

    if (!ok) {
      ok <- file.copy(part_path, final_path, overwrite = TRUE)
      if (ok) file.remove(part_path)
    }

    if (!ok) {
      stop("下载完成，但无法将.part改为最终文件名：", filename)
    }

    if (file.exists(control_path)) {
      file.remove(control_path)
    }

    final_size <- file.info(final_path)$size

    log_message("下载完成：", filename, "；", human_size(final_size))

    return(data.frame(
      geo = geo,
      filename = filename,
      status = "DOWNLOADED",
      local_size = human_size(final_size),
      path = final_path,
      stringsAsFactors = FALSE
    ))
  }

  log_message(
    "本轮尚未完成；aria2状态=", status,
    "；当前大小=", human_size(part_size),
    "。重新运行脚本会继续。"
  )

  data.frame(
    geo = geo,
    filename = filename,
    status = "PARTIAL_RETRY_NEXT_RUN",
    local_size = human_size(part_size),
    path = part_path,
    stringsAsFactors = FALSE
  )
}

cat("\n")
cat("============================================================\n")
cat("GEO高速断点下载｜aria2四连接模式\n")
cat("============================================================\n")
cat("项目路径：", PROJECT_ROOT, "\n")
cat("固定Clash代理：", PROXY_URL, "\n")
cat("单文件连接数：", CONNECTIONS_PER_FILE, "\n")
cat("会接管旧curl留下的.part文件，不会重新从0开始。\n")
cat("一次只下载一个文件，不会同时抢带宽。\n\n")

results <- data.frame()

for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, , drop = FALSE]

  one <- download_one(
    geo = row$geo,
    filename = row$filename,
    url = row$url
  )

  results <- rbind(results, one)

  utils::write.csv(
    results,
    STATUS_FILE,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

cat("\n")
cat("============================================================\n")
cat("本轮下载结果\n")
cat("============================================================\n")
print(results)

remaining <- sum(results$status == "PARTIAL_RETRY_NEXT_RUN")

if (remaining == 0L) {
  cat("\n所有列入清单的GEO官方文件已完成。\n")
} else {
  cat(
    "\n仍有", remaining,
    "个文件未完成。不要删除.part和.aria2文件，重新Source即可继续。\n"
  )
}

if (RUN_AUDIT_AFTER_DOWNLOAD) {
  audit_candidates <- c(
    file.path(PROJECT_ROOT, "07_scripts", "00_manual_data_audit_v2.R"),
    file.path(PROJECT_ROOT, "07_scripts", "00_manual_data_audit.R")
  )

  audit_script <- audit_candidates[file.exists(audit_candidates)]

  if (length(audit_script) > 0L) {
    source(audit_script[[1]], encoding = "UTF-8")
  } else {
    warning("07_scripts中没有找到00审计脚本。")
  }
}

gc(verbose = FALSE)

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

PROJECT_ROOT <- "D:/PD_Graft_Project"

AUTO_INSTALL_CRAN <- TRUE

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

da_metadata_merged <- merge(
  da_unique,
  metadata_unique,
  by = "barcode_key",
  all.x = TRUE,
  suffixes = c("_DA", "_META"),
  sort = FALSE
)

da_metadata_merged[
  ,
  in_GEO_filtered_matrix :=
    barcode_key %in% geo_unique$barcode_key
]

metadata_unique[
  ,
  in_GEO_filtered_matrix :=
    barcode_key %in% geo_unique$barcode_key
]

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

PROJECT_ROOT <- "D:/PD_Graft_Project"

N_WORKERS_LIGHT <- 2L

AUTO_INSTALL_CRAN <- TRUE

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
      "缺少CRAN包：",
      paste(missing_cran, collapse = ", ")
    )
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

g200_dir <- file.path(
  PROJECT_ROOT,
  "00_raw_data",
  "GSE200610",
  "01_extracted"
)

g233_dir <- file.path(
  PROJECT_ROOT,
  "00_raw_data",
  "GSE233885",
  "01_extracted"
)

metadata_dir <- file.path(PROJECT_ROOT, "01_metadata")
objects_dir <- file.path(PROJECT_ROOT, "02_objects")
reports_dir <- file.path(PROJECT_ROOT, "06_reports")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(
  reports_dir,
  "00D_deep_label_forensic_audit_log.txt"
)

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
  if (length(bytes) == 0L || is.na(bytes)) {
    return(NA_character_)
  }

  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  idx <- 1L

  while (value >= 1024 && idx < length(units)) {
    value <- value / 1024
    idx <- idx + 1L
  }

  sprintf("%.2f %s", value, units[idx])
}

truncate_text <- function(x, n = 1200L) {
  x <- paste(as.character(x), collapse = " | ")

  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }

  if (nchar(x) <= n) {
    return(x)
  }

  paste0(substr(x, 1L, n), " ...")
}

nonempty <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

safe_unique <- function(x) {
  x <- unique(as.character(x))
  x[!is.na(x) & nzchar(trimws(x))]
}

empty_dt <- function(schema) {
  as.data.table(schema)
}

projection_target_patterns <- c(
  "\\bPFC\\b",
  "prefrontal",
  "\\bdlSTR\\b",
  "dorsolateral.?striat",
  "projection.?target",
  "target.?specific",
  "mesocortical",
  "nigrostriatal"
)

projection_tracer_patterns <- c(
  "MNM008",
  "mCherry",
  "tdTomato",
  "retro",
  "retrograde",
  "\\bAAV\\b",
  "AAV2",
  "WPRE",
  "traced",
  "tracing"
)

generic_projection_patterns <- unique(c(
  projection_target_patterns,
  projection_tracer_patterns,
  "projection",
  "projecting",
  "barcode"
))

clone_strong_patterns <- c(
  "clone.?id",
  "clonal",
  "cell.?to.?clone",
  "clone.?assignment",
  "clone.?mapping",
  "lineage.?id",
  "lineage.?mapping"
)

clone_barcode_patterns <- c(
  "molecular.?barcode",
  "viral.?barcode",
  "lentiviral",
  "lenti",
  "LVLib",
  "WPRE",
  "CellTag",
  "barcode"
)

generic_clone_patterns <- unique(c(
  clone_strong_patterns,
  clone_barcode_patterns,
  "lineage",
  "clone"
))

collapse_patterns <- function(patterns) {
  paste0("(", paste(patterns, collapse = "|"), ")")
}

find_keyword_rows <- function(
  values,
  patterns,
  source_file,
  source_location,
  max_hits = 500L
) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0L) {
    return(empty_dt(list(
      source_file = character(),
      source_location = character(),
      matched_value = character(),
      matched_pattern_group = character()
    )))
  }

  regex <- collapse_patterns(patterns)
  hit <- grepl(
    regex,
    values,
    ignore.case = TRUE,
    perl = TRUE
  )

  matched <- unique(values[hit])

  if (length(matched) == 0L) {
    return(empty_dt(list(
      source_file = character(),
      source_location = character(),
      matched_value = character(),
      matched_pattern_group = character()
    )))
  }

  matched <- head(matched, max_hits)

  data.table(
    source_file = source_file,
    source_location = source_location,
    matched_value = matched,
    matched_pattern_group = regex
  )
}

extract_rat_id <- function(x) {
  x <- tolower(as.character(x))

  patterns <- c(
    "rat[0-9]+[a-z0-9]*",
    "(?:sd|nude)-?no[0-9]+"
  )

  for (pat in patterns) {
    m <- regexpr(pat, x, perl = TRUE)

    if (m[1L] > 0L) {
      return(regmatches(x, m))
    }
  }

  NA_character_
}

extract_timepoint <- function(x) {
  x <- tolower(as.character(x))

  if (grepl("12m|12month", x, perl = TRUE)) return("12m")
  if (grepl("9m|9month", x, perl = TRUE)) return("9m")
  if (grepl("4m|4month", x, perl = TRUE)) return("4m")
  if (grepl("1m|1month", x, perl = TRUE)) return("1m")

  NA_character_
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

extract_metadata <- function(obj) {
  md <- NULL
  source <- NA_character_

  if (inherits(obj, "Seurat")) {
    md <- tryCatch(
      obj[[]],
      error = function(e) NULL
    )

    if (!is.null(md)) {
      source <- "Seurat_[[]]"
    }
  }

  if (
    is.null(md) &&
    methods::isS4(obj) &&
    "meta.data" %in% methods::slotNames(obj)
  ) {
    md <- tryCatch(
      methods::slot(obj, "meta.data"),
      error = function(e) NULL
    )

    if (!is.null(md)) {
      source <- "S4_meta.data_slot"
    }
  }

  if (
    is.null(md) &&
    methods::isS4(obj) &&
    "colData" %in% methods::slotNames(obj)
  ) {
    md <- tryCatch(
      as.data.frame(methods::slot(obj, "colData")),
      error = function(e) NULL
    )

    if (!is.null(md)) {
      source <- "S4_colData_slot"
    }
  }

  if (is.null(md) && is.list(obj)) {
    possible <- c(
      "meta.data",
      "metadata",
      "meta",
      "cell_metadata",
      "cell.meta",
      "colData"
    )

    hit <- possible[possible %in% names(obj)]

    if (length(hit) > 0L) {
      md <- tryCatch(
        as.data.frame(obj[[hit[1L]]]),
        error = function(e) NULL
      )

      if (!is.null(md)) {
        source <- paste0("list$", hit[1L])
      }
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
  }

  list(
    metadata = md,
    source = source
  )
}

get_idents_safe <- function(obj) {
  if (
    inherits(obj, "Seurat") &&
    requireNamespace("SeuratObject", quietly = TRUE)
  ) {
    return(
      tryCatch(
        as.character(SeuratObject::Idents(obj)),
        error = function(e) character()
      )
    )
  }

  if (
    methods::isS4(obj) &&
    "active.ident" %in% methods::slotNames(obj)
  ) {
    return(
      tryCatch(
        as.character(methods::slot(obj, "active.ident")),
        error = function(e) character()
      )
    )
  }

  character()
}

get_assay_names_safe <- function(obj) {
  if (
    methods::isS4(obj) &&
    "assays" %in% methods::slotNames(obj)
  ) {
    assays_obj <- tryCatch(
      methods::slot(obj, "assays"),
      error = function(e) NULL
    )

    if (!is.null(assays_obj)) {
      return(names(assays_obj))
    }
  }

  if (is.list(obj) && "assays" %in% names(obj)) {
    return(names(obj$assays))
  }

  character()
}

get_feature_names_safe <- function(obj) {
  feature_tables <- list()

  direct_features <- tryCatch(
    rownames(obj),
    error = function(e) NULL
  )

  if (!is.null(direct_features)) {
    feature_tables[["object_rownames"]] <- as.character(direct_features)
  }

  if (
    methods::isS4(obj) &&
    "assays" %in% methods::slotNames(obj)
  ) {
    assays_obj <- tryCatch(
      methods::slot(obj, "assays"),
      error = function(e) NULL
    )

    if (!is.null(assays_obj) && length(assays_obj) > 0L) {
      for (assay_name in names(assays_obj)) {
        assay_features <- tryCatch(
          rownames(assays_obj[[assay_name]]),
          error = function(e) NULL
        )

        if (!is.null(assay_features)) {
          feature_tables[[paste0(
            "assay:",
            assay_name
          )]] <- as.character(assay_features)
        }
      }
    }
  }

  feature_tables
}

get_cell_names_safe <- function(obj) {
  cells <- tryCatch(
    colnames(obj),
    error = function(e) NULL
  )

  if (is.null(cells)) {
    return(character())
  }

  as.character(cells)
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

  c(
    n_features = nr,
    n_cells = nc
  )
}

component_summary_row <- function(
  source_file,
  component_path,
  x
) {
  dims <- tryCatch(
    dim(x),
    error = function(e) NULL
  )

  x_names <- tryCatch(
    names(x),
    error = function(e) NULL
  )

  data.table(
    source_file = source_file,
    component_path = component_path,
    component_class = paste(class(x), collapse = " | "),
    object_size_bytes = as.numeric(object.size(x)),
    object_size_human = human_size(
      as.numeric(object.size(x))
    ),
    dim_preview = if (is.null(dims)) {
      NA_character_
    } else {
      paste(dims, collapse = " x ")
    },
    length_value = tryCatch(
      length(x),
      error = function(e) NA_integer_
    ),
    child_names_preview = truncate_text(
      head(x_names, 100L),
      n = 1500L
    )
  )
}

catalog_object_components <- function(
  obj,
  source_file
) {
  out <- list()
  idx <- 1L

  out[[idx]] <- component_summary_row(
    source_file,
    "object",
    obj
  )
  idx <- idx + 1L

  if (methods::isS4(obj)) {
    slots <- methods::slotNames(obj)

    for (slot_name in slots) {
      slot_value <- tryCatch(
        methods::slot(obj, slot_name),
        error = function(e) NULL
      )

      if (is.null(slot_value)) next

      out[[idx]] <- component_summary_row(
        source_file,
        paste0("slot:", slot_name),
        slot_value
      )
      idx <- idx + 1L

      if (
        is.list(slot_value) ||
        inherits(slot_value, "SimpleList")
      ) {
        child_names <- names(slot_value)

        if (length(child_names) > 0L) {
          for (child_name in head(child_names, 200L)) {
            child_value <- tryCatch(
              slot_value[[child_name]],
              error = function(e) NULL
            )

            if (is.null(child_value)) next

            out[[idx]] <- component_summary_row(
              source_file,
              paste0(
                "slot:",
                slot_name,
                "$",
                child_name
              ),
              child_value
            )
            idx <- idx + 1L
          }
        }
      }
    }
  } else if (is.list(obj)) {
    child_names <- names(obj)

    if (length(child_names) > 0L) {
      for (child_name in head(child_names, 300L)) {
        child_value <- tryCatch(
          obj[[child_name]],
          error = function(e) NULL
        )

        if (is.null(child_value)) next

        out[[idx]] <- component_summary_row(
          source_file,
          paste0("list$", child_name),
          child_value
        )
        idx <- idx + 1L
      }
    }
  }

  rbindlist(out, fill = TRUE)
}

scan_small_structure <- function(
  x,
  source_file,
  current_path = "object",
  depth = 0L,
  max_depth = 3L,
  max_atomic_values = 5000L,
  max_container_children = 300L
) {
  output <- list()
  idx <- 1L

  add_text <- function(location, values) {
    values <- safe_unique(values)

    if (length(values) == 0L) {
      return()
    }

    values <- head(values, max_atomic_values)

    output[[idx]] <<- data.table(
      source_file = source_file,
      source_location = location,
      text_value = values
    )

    idx <<- idx + 1L
  }

  recurse <- function(y, path, d) {
    y_names <- tryCatch(
      names(y),
      error = function(e) NULL
    )

    if (length(y_names) > 0L) {
      add_text(
        paste0(path, "::names"),
        y_names
      )
    }

    y_dimnames <- tryCatch(
      dimnames(y),
      error = function(e) NULL
    )

    if (!is.null(y_dimnames)) {
      for (j in seq_along(y_dimnames)) {
        if (!is.null(y_dimnames[[j]])) {
          add_text(
            paste0(path, "::dimnames", j),
            head(y_dimnames[[j]], max_atomic_values)
          )
        }
      }
    }

    if (
      is.atomic(y) &&
      is.null(dim(y)) &&
      length(y) <= max_atomic_values
    ) {
      add_text(
        paste0(path, "::values"),
        y
      )
    }

    if (d >= max_depth) {
      return()
    }

    if (methods::isS4(y)) {
      slots <- methods::slotNames(y)

      for (slot_name in head(slots, max_container_children)) {
        slot_value <- tryCatch(
          methods::slot(y, slot_name),
          error = function(e) NULL
        )

        if (is.null(slot_value)) next

        cls <- class(slot_value)
        is_matrix_like <- (
          is.matrix(slot_value) ||
          inherits(slot_value, "Matrix") ||
          inherits(slot_value, "dgCMatrix")
        )

        if (is_matrix_like) {
          recurse(
            slot_value,
            paste0(path, "@", slot_name),
            max_depth
          )
        } else {
          recurse(
            slot_value,
            paste0(path, "@", slot_name),
            d + 1L
          )
        }
      }
    } else if (is.list(y)) {
      child_names <- names(y)

      if (is.null(child_names)) {
        child_indices <- seq_len(
          min(length(y), max_container_children)
        )

        for (j in child_indices) {
          child <- tryCatch(
            y[[j]],
            error = function(e) NULL
          )

          if (is.null(child)) next

          recurse(
            child,
            paste0(path, "[[", j, "]]"),
            d + 1L
          )
        }
      } else {
        for (child_name in head(
          child_names,
          max_container_children
        )) {
          child <- tryCatch(
            y[[child_name]],
            error = function(e) NULL
          )

          if (is.null(child)) next

          recurse(
            child,
            paste0(path, "$", child_name),
            d + 1L
          )
        }
      }
    }
  }

  recurse(
    x,
    current_path,
    depth
  )

  if (length(output) == 0L) {
    return(empty_dt(list(
      source_file = character(),
      source_location = character(),
      text_value = character()
    )))
  }

  rbindlist(output, fill = TRUE)
}

audit_one_g233_rds <- function(rds_path) {
  file_name <- basename(rds_path)

  obj <- tryCatch(
    safe_read_rds(rds_path),
    error = function(e) e
  )

  if (inherits(obj, "error")) {
    return(list(
      summary = data.table(
        file_name = file_name,
        file_path = rds_path,
        read_status = paste0(
          "ERROR: ",
          conditionMessage(obj)
        ),
        object_class = NA_character_,
        n_features = NA_real_,
        n_cells = NA_real_,
        rat_id_from_filename = extract_rat_id(file_name),
        timepoint_from_filename = extract_timepoint(file_name),
        metadata_source = NA_character_,
        metadata_rows = NA_integer_,
        metadata_columns_n = NA_integer_,
        assay_names = NA_character_,
        idents_levels = NA_character_,
        strong_projection_hit_n = 0L,
        tracer_hit_n = 0L,
        generic_projection_hit_n = 0L
      ),
      component_catalog = empty_dt(list(
        source_file = character(),
        component_path = character(),
        component_class = character(),
        object_size_bytes = numeric(),
        object_size_human = character(),
        dim_preview = character(),
        length_value = integer(),
        child_names_preview = character()
      )),
      evidence = empty_dt(list(
        source_file = character(),
        source_location = character(),
        evidence_class = character(),
        matched_value = character()
      )),
      metadata_candidates = empty_dt(list(
        source_file = character(),
        metadata_column = character(),
        n_unique = integer(),
        unique_values_preview = character(),
        column_name_projection_hit = logical(),
        value_projection_hit = logical()
      )),
      feature_hits = empty_dt(list(
        source_file = character(),
        assay_or_source = character(),
        feature_name = character(),
        evidence_class = character()
      ))
    ))
  }

  dims <- object_dimensions(obj)
  md_info <- extract_metadata(obj)
  md <- md_info$metadata
  idents <- get_idents_safe(obj)
  cells <- get_cell_names_safe(obj)
  assays <- get_assay_names_safe(obj)
  feature_lists <- get_feature_names_safe(obj)

  component_catalog <- catalog_object_components(
    obj,
    file_name
  )

  structure_text <- scan_small_structure(
    obj,
    source_file = file_name,
    current_path = "object",
    max_depth = 3L
  )

  evidence_list <- list()
  e_idx <- 1L

  add_evidence <- function(
    location,
    values,
    evidence_class,
    patterns
  ) {
    hits <- find_keyword_rows(
      values = values,
      patterns = patterns,
      source_file = file_name,
      source_location = location
    )

    if (nrow(hits) == 0L) {
      return()
    }

    evidence_list[[e_idx]] <<- hits[
      ,
      .(
        source_file,
        source_location,
        evidence_class = evidence_class,
        matched_value
      )
    ]

    e_idx <<- e_idx + 1L
  }

  add_evidence(
    "filename",
    file_name,
    "projection_target",
    projection_target_patterns
  )

  add_evidence(
    "filename",
    file_name,
    "projection_tracer",
    projection_tracer_patterns
  )

  metadata_candidates <- empty_dt(list(
    source_file = character(),
    metadata_column = character(),
    n_unique = integer(),
    unique_values_preview = character(),
    column_name_projection_hit = logical(),
    value_projection_hit = logical()
  ))

  if (!is.null(md) && ncol(md) > 0L) {
    md_columns <- names(md)

    add_evidence(
      "metadata_column_names",
      md_columns,
      "projection_target",
      projection_target_patterns
    )

    add_evidence(
      "metadata_column_names",
      md_columns,
      "projection_tracer",
      projection_tracer_patterns
    )

    for (col in md_columns) {
      values <- md[[col]]

      if (
        is.factor(values) ||
        is.character(values) ||
        is.logical(values)
      ) {
        unique_values <- safe_unique(values)

        values_to_scan <- head(
          unique_values,
          10000L
        )

        column_name_hit <- grepl(
          collapse_patterns(
            generic_projection_patterns
          ),
          col,
          ignore.case = TRUE,
          perl = TRUE
        )

        value_hit <- any(grepl(
          collapse_patterns(
            generic_projection_patterns
          ),
          values_to_scan,
          ignore.case = TRUE,
          perl = TRUE
        ))

        if (column_name_hit || value_hit) {
          metadata_candidates <- rbind(
            metadata_candidates,
            data.table(
              source_file = file_name,
              metadata_column = col,
              n_unique = length(unique_values),
              unique_values_preview = truncate_text(
                head(unique_values, 80L),
                n = 2500L
              ),
              column_name_projection_hit = column_name_hit,
              value_projection_hit = value_hit
            ),
            fill = TRUE
          )
        }

        add_evidence(
          paste0("metadata_values:", col),
          values_to_scan,
          "projection_target",
          projection_target_patterns
        )

        add_evidence(
          paste0("metadata_values:", col),
          values_to_scan,
          "projection_tracer",
          projection_tracer_patterns
        )
      }
    }
  }

  if (length(idents) > 0L) {
    add_evidence(
      "Idents_levels",
      unique(idents),
      "projection_target",
      projection_target_patterns
    )

    add_evidence(
      "Idents_levels",
      unique(idents),
      "projection_tracer",
      projection_tracer_patterns
    )
  }

  if (length(cells) > 0L) {
    add_evidence(
      "cell_names",
      cells,
      "projection_target",
      projection_target_patterns
    )

    add_evidence(
      "cell_names",
      cells,
      "projection_tracer",
      projection_tracer_patterns
    )
  }

  feature_hits <- empty_dt(list(
    source_file = character(),
    assay_or_source = character(),
    feature_name = character(),
    evidence_class = character()
  ))

  if (length(feature_lists) > 0L) {
    for (feature_source in names(feature_lists)) {
      features <- feature_lists[[feature_source]]

      target_hit <- features[
        grepl(
          collapse_patterns(
            projection_target_patterns
          ),
          features,
          ignore.case = TRUE,
          perl = TRUE
        )
      ]

      tracer_hit <- features[
        grepl(
          collapse_patterns(
            projection_tracer_patterns
          ),
          features,
          ignore.case = TRUE,
          perl = TRUE
        )
      ]

      if (length(target_hit) > 0L) {
        feature_hits <- rbind(
          feature_hits,
          data.table(
            source_file = file_name,
            assay_or_source = feature_source,
            feature_name = unique(target_hit),
            evidence_class = "projection_target"
          ),
          fill = TRUE
        )
      }

      if (length(tracer_hit) > 0L) {
        feature_hits <- rbind(
          feature_hits,
          data.table(
            source_file = file_name,
            assay_or_source = feature_source,
            feature_name = unique(tracer_hit),
            evidence_class = "projection_tracer"
          ),
          fill = TRUE
        )
      }
    }
  }

  if (nrow(structure_text) > 0L) {
    structure_target_hit <- structure_text[
      grepl(
        collapse_patterns(
          projection_target_patterns
        ),
        text_value,
        ignore.case = TRUE,
        perl = TRUE
      )
    ]

    structure_tracer_hit <- structure_text[
      grepl(
        collapse_patterns(
          projection_tracer_patterns
        ),
        text_value,
        ignore.case = TRUE,
        perl = TRUE
      )
    ]

    if (nrow(structure_target_hit) > 0L) {
      evidence_list[[e_idx]] <- structure_target_hit[
        ,
        .(
          source_file,
          source_location,
          evidence_class = "projection_target",
          matched_value = text_value
        )
      ]
      e_idx <- e_idx + 1L
    }

    if (nrow(structure_tracer_hit) > 0L) {
      evidence_list[[e_idx]] <- structure_tracer_hit[
        ,
        .(
          source_file,
          source_location,
          evidence_class = "projection_tracer",
          matched_value = text_value
        )
      ]
      e_idx <- e_idx + 1L
    }
  }

  evidence <- if (length(evidence_list) == 0L) {
    empty_dt(list(
      source_file = character(),
      source_location = character(),
      evidence_class = character(),
      matched_value = character()
    ))
  } else {
    unique(
      rbindlist(evidence_list, fill = TRUE)
    )
  }

  if (nrow(feature_hits) > 0L) {
    feature_evidence <- feature_hits[
      ,
      .(
        source_file,
        source_location = paste0(
          "feature_names:",
          assay_or_source
        ),
        evidence_class,
        matched_value = feature_name
      )
    ]

    evidence <- unique(
      rbind(
        evidence,
        feature_evidence,
        fill = TRUE
      )
    )
  }

  strong_n <- evidence[
    evidence_class == "projection_target",
    .N
  ]

  tracer_n <- evidence[
    evidence_class == "projection_tracer",
    .N
  ]

  generic_n <- nrow(evidence)

  summary <- data.table(
    file_name = file_name,
    file_path = rds_path,
    read_status = "OK",
    object_class = paste(class(obj), collapse = " | "),
    n_features = unname(dims["n_features"]),
    n_cells = unname(dims["n_cells"]),
    rat_id_from_filename = extract_rat_id(file_name),
    timepoint_from_filename = extract_timepoint(file_name),
    metadata_source = md_info$source,
    metadata_rows = if (is.null(md)) {
      NA_integer_
    } else {
      nrow(md)
    },
    metadata_columns_n = if (is.null(md)) {
      NA_integer_
    } else {
      ncol(md)
    },
    assay_names = truncate_text(assays),
    idents_levels = truncate_text(
      unique(idents),
      n = 2500L
    ),
    strong_projection_hit_n = strong_n,
    tracer_hit_n = tracer_n,
    generic_projection_hit_n = generic_n
  )

  rm(
    obj,
    md,
    md_info,
    idents,
    cells,
    feature_lists,
    structure_text
  )

  gc(verbose = FALSE)

  list(
    summary = summary,
    component_catalog = component_catalog,
    evidence = evidence,
    metadata_candidates = metadata_candidates,
    feature_hits = feature_hits
  )
}

if (!dir.exists(g233_dir)) {
  stop(
    "没有找到GSE233885解压目录：\n",
    g233_dir,
    "\n请先完成00B。"
  )
}

g233_rds_files <- list.files(
  g233_dir,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.rds(\\.gz)?$",
  ignore.case = TRUE
)

if (length(g233_rds_files) == 0L) {
  stop(
    "GSE233885解压目录中没有找到RDS：\n",
    g233_dir
  )
}

log_message(
  "开始GSE233885深度投射标签审计，共",
  length(g233_rds_files),
  "个RDS。"
)

g233_results <- vector(
  "list",
  length(g233_rds_files)
)

for (i in seq_along(g233_rds_files)) {
  log_message(
    "[GSE233885 ",
    i,
    "/",
    length(g233_rds_files),
    "] ",
    basename(g233_rds_files[i])
  )

  g233_results[[i]] <- audit_one_g233_rds(
    g233_rds_files[i]
  )
}

g233_summary <- rbindlist(
  lapply(g233_results, `[[`, "summary"),
  fill = TRUE
)

g233_components <- rbindlist(
  lapply(g233_results, `[[`, "component_catalog"),
  fill = TRUE
)

g233_evidence <- rbindlist(
  lapply(g233_results, `[[`, "evidence"),
  fill = TRUE
)

g233_metadata_candidates <- rbindlist(
  lapply(g233_results, `[[`, "metadata_candidates"),
  fill = TRUE
)

g233_feature_hits <- rbindlist(
  lapply(g233_results, `[[`, "feature_hits"),
  fill = TRUE
)

rm(g233_results)
gc(verbose = FALSE)

g233_target_evidence <- g233_evidence[
  evidence_class == "projection_target"
]

g233_tracer_evidence <- g233_evidence[
  evidence_class == "projection_tracer"
]

g233_has_direct_target_label <- nrow(
  g233_target_evidence[
    grepl(
      "metadata|Idents|cell_names|object",
      source_location,
      ignore.case = TRUE
    )
  ]
) > 0L

g233_has_tracer_signal <- (
  nrow(g233_tracer_evidence) > 0L ||
  nrow(
    g233_feature_hits[
      evidence_class == "projection_tracer"
    ]
  ) > 0L
)

g233_recoverability <- if (
  g233_has_direct_target_label
) {
  "DIRECT_PROJECTION_LABEL_CANDIDATE_FOUND"
} else if (
  g233_has_tracer_signal
) {
  "TRACER_SIGNAL_FOUND_TARGET_MAPPING_STILL_REQUIRED"
} else {
  "NO_PROJECTION_OR_TRACER_LABEL_FOUND_IN_PUBLIC_RDS"
}

g233_decision <- data.table(
  dataset = "GSE233885",
  direct_projection_target_label_found =
    g233_has_direct_target_label,
  tracer_or_viral_signal_found =
    g233_has_tracer_signal,
  projection_target_evidence_n =
    nrow(g233_target_evidence),
  tracer_evidence_n =
    nrow(g233_tracer_evidence),
  recoverability = g233_recoverability,
  immediate_action = if (
    g233_has_direct_target_label
  ) {
    paste(
      "人工确认候选列/值，建立",
      "cell_barcode-rat_id-projection_target映射。"
    )
  } else if (
    g233_has_tracer_signal
  ) {
    paste(
      "检查tracer阳性规则和动物注射靶点；",
      "同时向作者索取PFC/dlSTR映射表。"
    )
  } else {
    paste(
      "公开RDS无法恢复真实投射标签；",
      "联系作者并启用A9/A10-like投射能力替代模块。"
    )
  }
)

log_message(
  "GSE233885深度审计结论：",
  g233_recoverability
)

is_10x_cell_barcode_like <- function(x) {
  x <- as.character(x)

  grepl(
    "[ACGTN]{14,20}-[0-9]+$",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
}

extract_dna_tokens <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) {
    return(character())
  }

  tokens <- unlist(
    strsplit(
      x,
      split = "[^ACGTNacgtn]+",
      perl = TRUE
    ),
    use.names = FALSE
  )

  tokens <- toupper(tokens)

  tokens[
    nchar(tokens) >= 8L &
    nchar(tokens) <= 80L &
    grepl("^[ACGTN]+$", tokens)
  ]
}

is_gene_like <- function(x) {
  x <- as.character(x)

  grepl(
    "^ENSG[0-9]+|^[A-Za-z][A-Za-z0-9.-]{1,30}$",
    x,
    perl = TRUE
  )
}

audit_one_g200_csv <- function(csv_path) {
  file_name <- basename(csv_path)

  header_dt <- tryCatch(
    fread(
      csv_path,
      nrows = 0L,
      showProgress = FALSE,
      data.table = TRUE
    ),
    error = function(e) e
  )

  if (inherits(header_dt, "error")) {
    return(list(
      summary = data.table(
        file_name = file_name,
        file_path = csv_path,
        read_status = paste0(
          "ERROR_HEADER: ",
          conditionMessage(header_dt)
        ),
        file_size_human = human_size(
          file.info(csv_path)$size
        ),
        n_columns = NA_integer_,
        first_column_name = NA_character_,
        header_10x_barcode_like_n = NA_integer_,
        header_dna_token_n = NA_integer_,
        first_column_values_n = NA_integer_,
        first_column_gene_like_pct = NA_real_,
        first_column_10x_barcode_like_n = NA_integer_,
        first_column_dna_token_n = NA_integer_,
        strong_clone_keyword_n = 0L,
        barcode_keyword_n = 0L
      ),
      evidence = empty_dt(list(
        source_file = character(),
        source_location = character(),
        evidence_class = character(),
        matched_value = character()
      )),
      dna_candidates = empty_dt(list(
        source_file = character(),
        source_location = character(),
        dna_token = character(),
        token_length = integer(),
        likely_10x_cell_barcode = logical()
      )),
      nonstandard_features = empty_dt(list(
        source_file = character(),
        first_column_value = character(),
        reason = character()
      ))
    ))
  }

  header_names <- names(header_dt)

  first_col_dt <- tryCatch(
    fread(
      csv_path,
      select = 1L,
      colClasses = "character",
      showProgress = FALSE,
      data.table = TRUE
    ),
    error = function(e) e
  )

  if (inherits(first_col_dt, "error")) {
    return(list(
      summary = data.table(
        file_name = file_name,
        file_path = csv_path,
        read_status = paste0(
          "ERROR_FIRST_COLUMN: ",
          conditionMessage(first_col_dt)
        ),
        file_size_human = human_size(
          file.info(csv_path)$size
        ),
        n_columns = length(header_names),
        first_column_name = header_names[1L],
        header_10x_barcode_like_n = sum(
          is_10x_cell_barcode_like(header_names)
        ),
        header_dna_token_n = length(
          unique(extract_dna_tokens(header_names))
        ),
        first_column_values_n = NA_integer_,
        first_column_gene_like_pct = NA_real_,
        first_column_10x_barcode_like_n = NA_integer_,
        first_column_dna_token_n = NA_integer_,
        strong_clone_keyword_n = 0L,
        barcode_keyword_n = 0L
      ),
      evidence = empty_dt(list(
        source_file = character(),
        source_location = character(),
        evidence_class = character(),
        matched_value = character()
      )),
      dna_candidates = empty_dt(list(
        source_file = character(),
        source_location = character(),
        dna_token = character(),
        token_length = integer(),
        likely_10x_cell_barcode = logical()
      )),
      nonstandard_features = empty_dt(list(
        source_file = character(),
        first_column_value = character(),
        reason = character()
      ))
    ))
  }

  first_values <- as.character(first_col_dt[[1L]])
  first_values <- first_values[
    !is.na(first_values) &
    nzchar(first_values)
  ]

  evidence_list <- list()
  idx <- 1L

  add_clone_hits <- function(
    location,
    values,
    evidence_class,
    patterns
  ) {
    hits <- find_keyword_rows(
      values = values,
      patterns = patterns,
      source_file = file_name,
      source_location = location
    )

    if (nrow(hits) == 0L) return()

    evidence_list[[idx]] <<- hits[
      ,
      .(
        source_file,
        source_location,
        evidence_class = evidence_class,
        matched_value
      )
    ]

    idx <<- idx + 1L
  }

  add_clone_hits(
    "filename",
    file_name,
    "clone_strong",
    clone_strong_patterns
  )

  add_clone_hits(
    "filename",
    file_name,
    "clone_barcode",
    clone_barcode_patterns
  )

  add_clone_hits(
    "column_names",
    header_names,
    "clone_strong",
    clone_strong_patterns
  )

  add_clone_hits(
    "column_names",
    header_names,
    "clone_barcode",
    clone_barcode_patterns
  )

  add_clone_hits(
    "first_column_values",
    first_values,
    "clone_strong",
    clone_strong_patterns
  )

  add_clone_hits(
    "first_column_values",
    first_values,
    "clone_barcode",
    clone_barcode_patterns
  )

  evidence <- if (length(evidence_list) == 0L) {
    empty_dt(list(
      source_file = character(),
      source_location = character(),
      evidence_class = character(),
      matched_value = character()
    ))
  } else {
    unique(
      rbindlist(evidence_list, fill = TRUE)
    )
  }

  header_dna <- unique(
    extract_dna_tokens(header_names)
  )

  first_dna <- unique(
    extract_dna_tokens(first_values)
  )

  dna_candidates <- rbind(
    if (length(header_dna) > 0L) {
      data.table(
        source_file = file_name,
        source_location = "column_names",
        dna_token = header_dna,
        token_length = nchar(header_dna),
        likely_10x_cell_barcode =
          header_dna %in%
          extract_dna_tokens(
            header_names[
              is_10x_cell_barcode_like(
                header_names
              )
            ]
          )
      )
    } else {
      empty_dt(list(
        source_file = character(),
        source_location = character(),
        dna_token = character(),
        token_length = integer(),
        likely_10x_cell_barcode = logical()
      ))
    },
    if (length(first_dna) > 0L) {
      data.table(
        source_file = file_name,
        source_location = "first_column_values",
        dna_token = first_dna,
        token_length = nchar(first_dna),
        likely_10x_cell_barcode =
          first_dna %in%
          extract_dna_tokens(
            first_values[
              is_10x_cell_barcode_like(
                first_values
              )
            ]
          )
      )
    } else {
      empty_dt(list(
        source_file = character(),
        source_location = character(),
        dna_token = character(),
        token_length = integer(),
        likely_10x_cell_barcode = logical()
      ))
    },
    fill = TRUE
  )

  feature_word_hit <- grepl(
    collapse_patterns(
      clone_barcode_patterns
    ),
    first_values,
    ignore.case = TRUE,
    perl = TRUE
  )

  non_gene_like <- !is_gene_like(first_values)

  nonstandard_values <- unique(
    first_values[
      feature_word_hit |
      non_gene_like
    ]
  )

  nonstandard_features <- if (
    length(nonstandard_values) > 0L
  ) {
    data.table(
      source_file = file_name,
      first_column_value =
        head(nonstandard_values, 5000L),
      reason = ifelse(
        grepl(
          collapse_patterns(
            clone_barcode_patterns
          ),
          head(nonstandard_values, 5000L),
          ignore.case = TRUE,
          perl = TRUE
        ),
        "keyword_or_viral_feature",
        "nonstandard_gene_like_string"
      )
    )
  } else {
    empty_dt(list(
      source_file = character(),
      first_column_value = character(),
      reason = character()
    ))
  }

  strong_n <- evidence[
    evidence_class == "clone_strong",
    .N
  ]

  barcode_n <- evidence[
    evidence_class == "clone_barcode",
    .N
  ]

  summary <- data.table(
    file_name = file_name,
    file_path = csv_path,
    read_status = "OK",
    file_size_human = human_size(
      file.info(csv_path)$size
    ),
    n_columns = length(header_names),
    first_column_name = header_names[1L],
    header_10x_barcode_like_n = sum(
      is_10x_cell_barcode_like(
        header_names
      )
    ),
    header_dna_token_n = length(header_dna),
    first_column_values_n = length(first_values),
    first_column_gene_like_pct = round(
      100 * mean(
        is_gene_like(first_values)
      ),
      4
    ),
    first_column_10x_barcode_like_n = sum(
      is_10x_cell_barcode_like(
        first_values
      )
    ),
    first_column_dna_token_n = length(first_dna),
    strong_clone_keyword_n = strong_n,
    barcode_keyword_n = barcode_n
  )

  list(
    summary = summary,
    evidence = evidence,
    dna_candidates = dna_candidates,
    nonstandard_features = nonstandard_features
  )
}

if (!dir.exists(g200_dir)) {
  stop(
    "没有找到GSE200610解压目录：\n",
    g200_dir,
    "\n请先完成00B。"
  )
}

g200_csv_files <- list.files(
  g200_dir,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.csv(\\.gz)?$",
  ignore.case = TRUE
)

if (length(g200_csv_files) == 0L) {
  stop(
    "GSE200610解压目录中没有找到CSV：\n",
    g200_dir
  )
}

log_message(
  "开始GSE200610深度克隆标签审计，共",
  length(g200_csv_files),
  "个CSV。"
)

future::plan(
  future::multisession,
  workers = max(
    1L,
    min(N_WORKERS_LIGHT, 2L)
  )
)

g200_results <- future_lapply(
  g200_csv_files,
  audit_one_g200_csv,
  future.seed = TRUE
)

future::plan(future::sequential)

g200_summary <- rbindlist(
  lapply(g200_results, `[[`, "summary"),
  fill = TRUE
)

g200_evidence <- rbindlist(
  lapply(g200_results, `[[`, "evidence"),
  fill = TRUE
)

g200_dna_candidates <- rbindlist(
  lapply(g200_results, `[[`, "dna_candidates"),
  fill = TRUE
)

g200_nonstandard_features <- rbindlist(
  lapply(g200_results, `[[`, "nonstandard_features"),
  fill = TRUE
)

rm(g200_results)
gc(verbose = FALSE)

g200_strong_evidence <- g200_evidence[
  evidence_class == "clone_strong"
]

g200_barcode_evidence <- g200_evidence[
  evidence_class == "clone_barcode"
]

g200_non10x_dna <- g200_dna_candidates[
  likely_10x_cell_barcode == FALSE
]

g200_has_explicit_mapping <- nrow(
  g200_strong_evidence[
    grepl(
      "column_names|first_column_values",
      source_location,
      ignore.case = TRUE
    )
  ]
) > 0L

g200_has_barcode_candidate <- (
  nrow(g200_barcode_evidence) > 0L ||
  nrow(g200_non10x_dna) > 0L ||
  nrow(
    g200_nonstandard_features[
      reason == "keyword_or_viral_feature"
    ]
  ) > 0L
)

g200_recoverability <- if (
  g200_has_explicit_mapping
) {
  "EXPLICIT_CLONE_MAPPING_CANDIDATE_FOUND"
} else if (
  g200_has_barcode_candidate
) {
  "BARCODE_OR_VIRAL_CANDIDATE_FOUND_MAPPING_STILL_REQUIRED"
} else {
  "NO_CLONE_MAPPING_FOUND_IN_PUBLIC_COUNT_MATRICES"
}

g200_decision <- data.table(
  dataset = "GSE200610",
  explicit_clone_mapping_candidate_found =
    g200_has_explicit_mapping,
  barcode_or_viral_candidate_found =
    g200_has_barcode_candidate,
  strong_clone_evidence_n =
    nrow(g200_strong_evidence),
  barcode_evidence_n =
    nrow(g200_barcode_evidence),
  non10x_dna_candidate_n =
    nrow(g200_non10x_dna),
  recoverability = g200_recoverability,
  immediate_action = if (
    g200_has_explicit_mapping
  ) {
    paste(
      "人工确认候选字段，建立",
      "cell_barcode-clone_id映射并验证克隆大小。"
    )
  } else if (
    g200_has_barcode_candidate
  ) {
    paste(
      "核查候选barcode/病毒feature是否为真实克隆码；",
      "同时向作者索取processed clone assignment。"
    )
  } else {
    paste(
      "公开count matrix无法恢复真实clone；",
      "联系作者并启用命运倾向与安全风险替代模块。"
    )
  }
)

log_message(
  "GSE200610深度审计结论：",
  g200_recoverability
)

overall_decision <- rbindlist(
  list(
    data.table(
      dataset = "GSE233885",
      target_module = "真实PFC/dlSTR投射监督模块",
      forensic_status = g233_recoverability,
      module_decision = if (
        g233_has_direct_target_label
      ) {
        "KEEP_AND_RECONSTRUCT"
      } else {
        "CONTACT_AUTHOR_AND_ACTIVATE_A9_A10_FALLBACK"
      },
      fallback_module = paste(
        "A9/nigrostriatal-like与A10/mesocortical-like",
        "分子身份及轴突/突触整合能力模块"
      )
    ),
    data.table(
      dataset = "GSE200610",
      target_module = "真实clone-aware谱系模块",
      forensic_status = g200_recoverability,
      module_decision = if (
        g200_has_explicit_mapping
      ) {
        "KEEP_AND_RECONSTRUCT"
      } else {
        "CONTACT_AUTHOR_AND_ACTIVATE_FATE_SAFETY_FALLBACK"
      },
      fallback_module = paste(
        "早期命运倾向、off-target谱系、",
        "残余祖细胞与安全风险模型"
      )
    )
  ),
  fill = TRUE
)

fwrite(
  g233_summary,
  file.path(
    metadata_dir,
    "00D_GSE233885_RDS_summary.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_components,
  file.path(
    metadata_dir,
    "00D_GSE233885_component_catalog.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_evidence,
  file.path(
    metadata_dir,
    "00D_GSE233885_projection_evidence.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_metadata_candidates,
  file.path(
    metadata_dir,
    "00D_GSE233885_metadata_candidates.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_feature_hits,
  file.path(
    metadata_dir,
    "00D_GSE233885_feature_hits.csv"
  ),
  bom = TRUE
)

fwrite(
  g233_decision,
  file.path(
    metadata_dir,
    "00D_GSE233885_recoverability.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_summary,
  file.path(
    metadata_dir,
    "00D_GSE200610_CSV_summary.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_evidence,
  file.path(
    metadata_dir,
    "00D_GSE200610_clone_evidence.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_dna_candidates,
  file.path(
    metadata_dir,
    "00D_GSE200610_DNA_candidates.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_nonstandard_features,
  file.path(
    metadata_dir,
    "00D_GSE200610_nonstandard_features.csv"
  ),
  bom = TRUE
)

fwrite(
  g200_decision,
  file.path(
    metadata_dir,
    "00D_GSE200610_recoverability.csv"
  ),
  bom = TRUE
)

fwrite(
  overall_decision,
  file.path(
    metadata_dir,
    "00D_overall_module_decision.csv"
  ),
  bom = TRUE
)

write_sheet_safe <- function(
  wb,
  sheet,
  x,
  empty_message = "No records detected."
) {
  addWorksheet(wb, sheet)

  x <- as.data.table(x)

  if (nrow(x) == 0L || ncol(x) == 0L) {
    writeData(
      wb,
      sheet,
      data.frame(message = empty_message)
    )

    setColWidths(
      wb,
      sheet,
      cols = 1L,
      widths = "auto"
    )

    return(invisible(NULL))
  }

  x_to_write <- head(x, 100000L)

  writeDataTable(
    wb,
    sheet,
    x_to_write
  )

  freezePane(
    wb,
    sheet,
    firstRow = TRUE
  )

  setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(x_to_write)),
    widths = "auto"
  )

  invisible(NULL)
}

wb <- createWorkbook()

write_sheet_safe(
  wb,
  "overall_decision",
  overall_decision
)

write_sheet_safe(
  wb,
  "G233_decision",
  g233_decision
)

write_sheet_safe(
  wb,
  "G233_summary",
  g233_summary
)

write_sheet_safe(
  wb,
  "G233_evidence",
  g233_evidence,
  "No projection/tracer evidence detected."
)

write_sheet_safe(
  wb,
  "G233_meta_candidates",
  g233_metadata_candidates,
  "No candidate metadata column detected."
)

write_sheet_safe(
  wb,
  "G233_feature_hits",
  g233_feature_hits,
  "No projection/tracer feature detected."
)

write_sheet_safe(
  wb,
  "G233_components",
  g233_components
)

write_sheet_safe(
  wb,
  "G200_decision",
  g200_decision
)

write_sheet_safe(
  wb,
  "G200_summary",
  g200_summary
)

write_sheet_safe(
  wb,
  "G200_evidence",
  g200_evidence,
  "No clone/barcode keyword evidence detected."
)

write_sheet_safe(
  wb,
  "G200_DNA_candidates",
  g200_dna_candidates,
  "No DNA-like token detected."
)

write_sheet_safe(
  wb,
  "G200_nonstandard",
  g200_nonstandard_features,
  "No nonstandard feature detected."
)

saveWorkbook(
  wb,
  file.path(
    metadata_dir,
    "00D_deep_label_forensic_audit.xlsx"
  ),
  overwrite = TRUE
)

email_lines <- c(
  "============================================================",
  "Email 1｜GSE233885 TARGET-seq projection metadata request",
  "============================================================",
  "",
  "Subject: Request for processed projection-target metadata for GSE233885",
  "",
  "Dear Dr. Storm and colleagues,",
  "",
  paste(
    "I am conducting a reproducible secondary analysis of",
    "GSE233885 to study the balance between therapeutic",
    "dopaminergic identity, long-term maturation, and",
    "Parkinson's disease vulnerability in stem cell-derived grafts."
  ),
  "",
  paste(
    "I have downloaded and audited all 21 processed RDS files.",
    "However, I could not identify a cell-level field that links",
    "nucleus/cell barcodes to retrograde tracing status or the",
    "PFC versus dlSTR projection target."
  ),
  "",
  paste(
    "Would it be possible to share a small processed annotation",
    "table containing, where available:"
  ),
  "cell_barcode | rat_id | tracing_status | projection_target",
  "",
  paste(
    "I do not require raw sequencing data. A derived metadata",
    "table used for the published TARGET-seq analysis would be",
    "sufficient. I would cite the original article and dataset",
    "appropriately."
  ),
  "",
  "Thank you very much for considering this request.",
  "",
  "Best regards,",
  "[Your name]",
  "University of Glasgow",
  "",
  "",
  "============================================================",
  "Email 2｜GSE200610 clone assignment request",
  "============================================================",
  "",
  "Subject: Request for processed cell-to-clone assignment for GSE200610",
  "",
  "Dear Dr. Storm and colleagues,",
  "",
  paste(
    "I am performing a reproducible secondary analysis of",
    "GSE200610 focused on graft cell-state diversity,",
    "off-target lineage risk, and therapeutic quality assessment."
  ),
  "",
  paste(
    "I audited the 14 public processed count matrices but could",
    "not identify a processed mapping between single-cell",
    "barcodes, molecular barcodes, and clone identities."
  ),
  "",
  paste(
    "Would it be possible to share the processed assignment",
    "table used for the lineage analysis, ideally containing:"
  ),
  "cell_barcode | molecular_barcode | clone_id | sample_or_rat_id",
  "",
  paste(
    "I do not require raw reads. A derived clone-assignment",
    "table would be sufficient, and I would cite the original",
    "article and GEO record appropriately."
  ),
  "",
  "Thank you very much for your time.",
  "",
  "Best regards,",
  "[Your name]",
  "University of Glasgow"
)

writeLines(
  email_lines,
  file.path(
    reports_dir,
    "00D_author_metadata_request_templates.txt"
  ),
  useBytes = TRUE
)

report_lines <- c(
  "PD干细胞治疗项目｜00D深度标签取证审计",
  paste0("生成时间：", timestamp_now()),
  "",
  "一、GSE233885",
  paste0("结论：", g233_recoverability),
  paste0(
    "真实投射靶点证据数：",
    nrow(g233_target_evidence)
  ),
  paste0(
    "tracer/viral证据数：",
    nrow(g233_tracer_evidence)
  ),
  paste0("行动：", g233_decision$immediate_action),
  "",
  "二、GSE200610",
  paste0("结论：", g200_recoverability),
  paste0(
    "强clone映射证据数：",
    nrow(g200_strong_evidence)
  ),
  paste0(
    "barcode/viral关键词证据数：",
    nrow(g200_barcode_evidence)
  ),
  paste0(
    "非10x DNA候选数：",
    nrow(g200_non10x_dna)
  ),
  paste0("行动：", g200_decision$immediate_action),
  "",
  "三、模块决策",
  paste(
    overall_decision$dataset,
    overall_decision$module_decision,
    sep = " : "
  ),
  "",
  "四、重要原则",
  paste(
    "表达相似、轨迹邻近或RNA velocity不能替代真实",
    "clone barcode或projection label。若标签无法恢复，",
    "必须使用明确标注为propensity/competence的替代模块。"
  )
)

writeLines(
  report_lines,
  file.path(
    reports_dir,
    "00D_deep_label_forensic_report.txt"
  ),
  useBytes = TRUE
)

cat("\n")
cat("============================================================\n")
cat("00D 深度标签取证审计完成\n")
cat("============================================================\n")

cat("\nGSE233885：\n")
print(g233_decision)

cat("\nGSE200610：\n")
print(g200_decision)

cat("\n总体模块决策：\n")
print(overall_decision)

cat("\n主要输出：\n")
cat(
  file.path(
    metadata_dir,
    "00D_deep_label_forensic_audit.xlsx"
  ),
  "\n"
)
cat(
  file.path(
    metadata_dir,
    "00D_overall_module_decision.csv"
  ),
  "\n"
)
cat(
  file.path(
    reports_dir,
    "00D_deep_label_forensic_report.txt"
  ),
  "\n"
)
cat(
  file.path(
    reports_dir,
    "00D_author_metadata_request_templates.txt"
  ),
  "\n"
)

cat("\n下一步：\n")
cat(
  "把控制台中的GSE233885、GSE200610和总体模块决策截图发来。\n"
)
cat(
  "若出现候选证据，再进入00E标签重建；",
  "若没有，则直接启动替代模块设计。\n",
  sep = ""
)

future::plan(future::sequential)
gc(verbose = FALSE)

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

PROJECT_ROOT <- "D:/PD_Graft_Project"

AUTO_INSTALL_CRAN <- TRUE

REBUILD_EXISTING <- FALSE

MTX_LINE_CHUNK <- 500000L

CREATE_SEURAT_OBJECT <- TRUE

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

read_feature_names <- function(path) {

  read_tsv_lines_feature_names(path)
}

read_barcodes <- function(path) {

  read_tsv_lines_first_col(path)
}

features <- read_feature_names(feature_file)
all_barcodes <- read_barcodes(barcode_file)

all_barcodes <- trim_na(all_barcodes)
all_barcodes <- all_barcodes[nzchar(all_barcodes)]

features <- trim_na(features)
features <- features[nzchar(features)]
features <- standardize_feature_names(features)

stamp("features 数量：", length(features))
stamp("raw matrix barcodes 数量：", length(all_barcodes))

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

fresh_barcode_match <- match_target_to_raw(target_barcodes, all_barcodes)
fresh_matched <- fresh_barcode_match[fresh_barcode_match$matched, , drop = FALSE]

barcode_match <- fresh_barcode_match

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

atomic_write_csv(barcode_match, barcode_match_csv)

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

mtx_stream_incomplete <- FALSE
mtx_stream_error_message <- NA_character_

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

PROJECT_DIR <- "D:/PD_Graft_Project"

REBUILD_EXISTING <- TRUE

SAVE_UPDATED_SEURAT_OBJECTS <- TRUE

SAVE_BASIC_QC_PLOTS <- TRUE

PLOT_MAX_CELLS <- 50000L

SAVE_RDS_COMPRESS <- FALSE

cat("\n============================================================\n")
cat("02A V2：object integrity and QC metrics\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "Matrix", "data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 02A。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(Matrix)
  library(data.table)
  library(ggplot2)
})

objects_root <- file.path(PROJECT_DIR, "02_objects")
metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_objects_dir <- file.path(objects_root, "02A_qc_metrics")
out_tables_dir <- file.path(tables_dir, "02A_qc")
out_figures_dir <- file.path(figures_dir, "02A_qc")

dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

status_csv <- file.path(metadata_dir, "02A_object_integrity_status.csv")
summary_csv <- file.path(out_tables_dir, "02A_seurat_object_qc_summary.csv")
cell_manifest_csv <- file.path(out_tables_dir, "02A_cell_qc_metrics_manifest.csv")
sample_summary_csv <- file.path(out_tables_dir, "02A_sample_level_qc_summary.csv")
non_seurat_csv <- file.path(out_tables_dir, "02A_non_seurat_objects.csv")
duplicate_cells_csv <- file.path(out_tables_dir, "02A_duplicate_cell_names_across_objects.csv")
report_txt <- file.path(reports_dir, "02A_object_integrity_and_qc_metrics_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

safe_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "/", x)
  x <- basename(x)
  x <- gsub("\\.rds$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

infer_dataset <- function(path) {
  p <- gsub("\\\\", "/", path)
  parts <- strsplit(p, "/", fixed = TRUE)[[1]]

  idx <- which(parts == "01A_standardized")
  if (length(idx) > 0 && length(parts) >= idx[1] + 1) {
    return(parts[idx[1] + 1])
  }

  if (grepl("01B_GSE178265_DA", p, ignore.case = TRUE)) {
    return("GSE178265_DA_01B")
  }

  m <- regmatches(p, regexpr("GSE[0-9]+", p))
  if (length(m) > 0 && nchar(m) > 0) {
    return(m[1])
  }

  "UNKNOWN_DATASET"
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

object_is_readable <- function(path) {
  tryCatch({
    readRDS(path)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

is_seurat_object <- function(obj) {
  inherits(obj, "Seurat")
}

is_bulk_like_object <- function(obj) {
  cls <- class(obj)
  any(grepl("DESeqDataSet|SummarizedExperiment|ExpressionSet|DGEList", cls))
}

get_assay_for_qc <- function(obj) {

  assays <- names(obj@assays)

  if ("RNA" %in% assays) {
    return("RNA")
  }

  da <- tryCatch({
    SeuratObject::DefaultAssay(obj)
  }, error = function(e) {
    NA_character_
  })

  if (!is.na(da) && da %in% assays) {
    return(da)
  }

  if (length(assays) > 0L) {
    return(assays[[1L]])
  }

  NA_character_
}

get_counts_matrix <- function(obj, assay) {

  mat <- tryCatch({
    SeuratObject::GetAssayData(obj, assay = assay, layer = "counts")
  }, error = function(e1) {
    tryCatch({
      SeuratObject::GetAssayData(obj, assay = assay, slot = "counts")
    }, error = function(e2) {
      NULL
    })
  })

  if (is.null(mat)) {
    stop("无法从 assay=", assay, " 读取 counts。")
  }

  mat
}

calc_percent_by_pattern <- function(counts, pattern) {
  genes <- rownames(counts)
  hit <- grep(pattern, genes, value = TRUE)

  total <- Matrix::colSums(counts)
  total[total == 0] <- NA_real_

  if (length(hit) == 0L) {
    return(rep(0, ncol(counts)))
  }

  pct <- Matrix::colSums(counts[hit, , drop = FALSE]) / total * 100
  pct[is.na(pct)] <- 0
  as.numeric(pct)
}

safe_quantile <- function(x, probs) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(rep(NA_real_, length(probs)))
  }
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

make_summary_row <- function(meta, dataset, object_id, object_path, assay, saved_path) {
  q_nf <- safe_quantile(meta$nFeature_RNA, c(0.01, 0.05, 0.50, 0.95, 0.99))
  q_nc <- safe_quantile(meta$nCount_RNA, c(0.01, 0.05, 0.50, 0.95, 0.99))
  q_mt <- safe_quantile(meta$percent.mt, c(0.01, 0.05, 0.50, 0.95, 0.99))
  q_rb <- safe_quantile(meta$percent.ribo, c(0.01, 0.05, 0.50, 0.95, 0.99))

  data.frame(
    dataset = dataset,
    object_id = object_id,
    object_path = object_path,
    saved_path = saved_path,
    assay_used = assay,
    n_cells = nrow(meta),
    nFeature_p01 = q_nf[1],
    nFeature_p05 = q_nf[2],
    nFeature_median = q_nf[3],
    nFeature_p95 = q_nf[4],
    nFeature_p99 = q_nf[5],
    nCount_p01 = q_nc[1],
    nCount_p05 = q_nc[2],
    nCount_median = q_nc[3],
    nCount_p95 = q_nc[4],
    nCount_p99 = q_nc[5],
    percent_mt_p01 = q_mt[1],
    percent_mt_p05 = q_mt[2],
    percent_mt_median = q_mt[3],
    percent_mt_p95 = q_mt[4],
    percent_mt_p99 = q_mt[5],
    percent_ribo_p01 = q_rb[1],
    percent_ribo_p05 = q_rb[2],
    percent_ribo_median = q_rb[3],
    percent_ribo_p95 = q_rb[4],
    percent_ribo_p99 = q_rb[5],
    stringsAsFactors = FALSE
  )
}

make_sample_summary <- function(cell_qc) {
  sample_col <- NULL

  candidate_cols <- c(
    "sample_id", "sample", "orig.ident", "donor", "condition",
    "dataset", "object_id"
  )

  for (cc in candidate_cols) {
    if (cc %in% colnames(cell_qc)) {
      sample_col <- cc
      break
    }
  }

  if (is.null(sample_col)) {
    cell_qc$sample_group_for_qc <- cell_qc$object_id
    sample_col <- "sample_group_for_qc"
  }

  dt <- data.table::as.data.table(cell_qc)
  dt[, sample_group_for_qc := as.character(get(sample_col))]

  dt[
    ,
    .(
      n_cells = .N,
      nFeature_median = median(nFeature_RNA, na.rm = TRUE),
      nFeature_p05 = as.numeric(quantile(nFeature_RNA, 0.05, na.rm = TRUE)),
      nFeature_p95 = as.numeric(quantile(nFeature_RNA, 0.95, na.rm = TRUE)),
      nCount_median = median(nCount_RNA, na.rm = TRUE),
      nCount_p05 = as.numeric(quantile(nCount_RNA, 0.05, na.rm = TRUE)),
      nCount_p95 = as.numeric(quantile(nCount_RNA, 0.95, na.rm = TRUE)),
      percent_mt_median = median(percent.mt, na.rm = TRUE),
      percent_mt_p95 = as.numeric(quantile(percent.mt, 0.95, na.rm = TRUE)),
      percent_ribo_median = median(percent.ribo, na.rm = TRUE),
      percent_ribo_p95 = as.numeric(quantile(percent.ribo, 0.95, na.rm = TRUE))
    ),
    by = .(dataset, object_id, sample_group_for_qc)
  ]
}

plot_qc_basic <- function(cell_qc, dataset, object_id, out_dir) {
  if (!SAVE_BASIC_QC_PLOTS) {
    return(invisible(NULL))
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (nrow(cell_qc) > PLOT_MAX_CELLS) {
    set.seed(20260714)
    cell_qc <- cell_qc[sample(seq_len(nrow(cell_qc)), PLOT_MAX_CELLS), , drop = FALSE]
  }

  plot_base <- safe_name(paste(dataset, object_id, sep = "__"))

  p1 <- ggplot(cell_qc, aes(x = nFeature_RNA)) +
    geom_histogram(bins = 80) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nFeature_RNA distribution",
      x = "nFeature_RNA",
      y = "Cell count"
    )

  p2 <- ggplot(cell_qc, aes(x = nCount_RNA)) +
    geom_histogram(bins = 80) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nCount_RNA distribution",
      x = "nCount_RNA",
      y = "Cell count"
    )

  p3 <- ggplot(cell_qc, aes(x = percent.mt)) +
    geom_histogram(bins = 80) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "percent.mt distribution",
      x = "percent.mt",
      y = "Cell count"
    )

  p4 <- ggplot(cell_qc, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(alpha = 0.25, size = 0.25) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nCount_RNA vs nFeature_RNA",
      x = "nCount_RNA",
      y = "nFeature_RNA"
    )

  png_file <- file.path(out_dir, paste0(plot_base, "_basic_qc.png"))
  pdf_file <- file.path(out_dir, paste0(plot_base, "_basic_qc.pdf"))

  grDevices::png(png_file, width = 2800, height = 2200, res = 220)
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  grDevices::dev.off()

  grDevices::pdf(pdf_file, width = 9, height = 7)
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  grDevices::dev.off()

  invisible(c(png_file, pdf_file))
}

stamp("收集 01A / 01B 对象。")

paths_01A <- character()

dir_01A <- file.path(objects_root, "01A_standardized")

if (dir.exists(dir_01A)) {
  paths_01A <- list.files(
    dir_01A,
    pattern = "\\.rds$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

path_01B <- file.path(
  objects_root,
  "01B_GSE178265_DA",
  "01B_GSE178265_DA_seurat.rds"
)

object_paths <- unique(c(paths_01A, if (file.exists(path_01B)) path_01B else character()))

object_paths <- object_paths[file.exists(object_paths)]

if (length(object_paths) == 0L) {
  stop("没有找到任何 01A/01B RDS 对象。请确认 01A 和 01B 已完成。")
}

stamp("找到 RDS 对象数量：", length(object_paths))

status_list <- list()
summary_list <- list()
cell_manifest_files <- list()
sample_summary_list <- list()
non_seurat_list <- list()
all_cell_names_index <- list()

for (idx in seq_along(object_paths)) {
  path <- object_paths[[idx]]
  dataset <- infer_dataset(path)
  object_id <- safe_name(path)

  stamp("处理对象 ", idx, " / ", length(object_paths), "：", dataset, " :: ", object_id)

  object_out_dir <- file.path(out_objects_dir, dataset)
  object_table_dir <- file.path(out_tables_dir, "per_object_cell_qc")
  object_fig_dir <- file.path(out_figures_dir, dataset)

  dir.create(object_out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(object_table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(object_fig_dir, recursive = TRUE, showWarnings = FALSE)

  saved_path <- file.path(object_out_dir, paste0(object_id, "_02A_qc.rds"))
  cell_qc_csv <- file.path(object_table_dir, paste0(dataset, "__", object_id, "__cell_qc.csv"))

  status_row <- data.frame(
    dataset = dataset,
    object_id = object_id,
    object_path = path,
    object_class = NA_character_,
    is_seurat = FALSE,
    is_bulk_like = FALSE,
    n_features = NA_integer_,
    n_cells = NA_integer_,
    assay_used = NA_character_,
    saved_path = NA_character_,
    status = "PENDING",
    message = NA_character_,
    stringsAsFactors = FALSE
  )

  obj <- tryCatch({
    readRDS(path)
  }, error = function(e) {
    status_row$status <<- "FAILED_READ_RDS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    next
  }

  status_row$object_class <- paste(class(obj), collapse = " / ")

  if (!is_seurat_object(obj)) {
    status_row$is_bulk_like <- is_bulk_like_object(obj)
    status_row$status <- ifelse(status_row$is_bulk_like, "VALID_NON_SEURAT_BULK_LIKE_SKIPPED_QC", "VALID_NON_SEURAT_SKIPPED_QC")
    status_row$message <- "不是 Seurat 对象；02A 只对 single-cell Seurat 对象计算 QC 指标。"

    non_seurat_list[[length(non_seurat_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      object_path = path,
      object_class = status_row$object_class,
      is_bulk_like = status_row$is_bulk_like,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    status_list[[length(status_list) + 1L]] <- status_row
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  status_row$is_seurat <- TRUE

  assay <- tryCatch({
    get_assay_for_qc(obj)
  }, error = function(e) {
    NA_character_
  })

  if (is.na(assay) || !assay %in% names(obj@assays)) {
    status_row$status <- "FAILED_NO_VALID_ASSAY"
    status_row$message <- "找不到可用于 QC 的 assay。"
    status_list[[length(status_list) + 1L]] <- status_row
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  tryCatch({ SeuratObject::DefaultAssay(obj) <- assay }, error = function(e) NULL)
  status_row$assay_used <- assay

  counts <- tryCatch({
    get_counts_matrix(obj, assay)
  }, error = function(e) {
    status_row$status <<- "FAILED_GET_COUNTS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(counts)) {
    status_list[[length(status_list) + 1L]] <- status_row
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  status_row$n_features <- nrow(counts)
  status_row$n_cells <- ncol(counts)

  obj$nCount_RNA <- as.numeric(Matrix::colSums(counts))
  obj$nFeature_RNA <- as.numeric(Matrix::colSums(counts > 0))

  obj$percent.mt <- calc_percent_by_pattern(
    counts,
    pattern = "^MT-|^mt-|^Mt-"
  )

  obj$percent.ribo <- calc_percent_by_pattern(
    counts,
    pattern = "^RPL|^RPS|^Rpl|^Rps|^rpl|^rps"
  )

  obj$percent.hb <- calc_percent_by_pattern(
    counts,
    pattern = "^HBA|^HBB|^HBM|^HBQ|^HBZ|^Hba|^Hbb|^Hbm|^Hbq|^Hbz"
  )

  obj$dataset_02A <- dataset
  obj$object_id_02A <- object_id
  obj$qc_stage <- "02A_qc_metrics"

  meta <- obj@meta.data
  meta$cell_barcode <- rownames(meta)
  meta$dataset <- dataset
  meta$object_id <- object_id
  meta$object_path <- path

  cell_qc_cols <- unique(c(
    "cell_barcode",
    "dataset",
    "object_id",
    "orig.ident",
    "sample",
    "sample_id",
    "condition",
    "nCount_RNA",
    "nFeature_RNA",
    "percent.mt",
    "percent.ribo",
    "percent.hb",
    "object_path"
  ))

  cell_qc_cols <- cell_qc_cols[cell_qc_cols %in% colnames(meta)]

  cell_qc <- meta[, cell_qc_cols, drop = FALSE]
  atomic_write_csv(cell_qc, cell_qc_csv)

  cell_manifest_files[[length(cell_manifest_files) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells = nrow(cell_qc),
    cell_qc_csv = cell_qc_csv,
    stringsAsFactors = FALSE
  )

  sample_summary_list[[length(sample_summary_list) + 1L]] <- as.data.frame(
    make_sample_summary(cell_qc)
  )

  summary_list[[length(summary_list) + 1L]] <- make_summary_row(
    meta = meta,
    dataset = dataset,
    object_id = object_id,
    object_path = path,
    assay = assay,
    saved_path = ifelse(SAVE_UPDATED_SEURAT_OBJECTS, saved_path, NA_character_)
  )

  all_cell_names_index[[length(all_cell_names_index) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    cell_barcode = colnames(obj),
    stringsAsFactors = FALSE
  )

  tryCatch({
    plot_qc_basic(cell_qc, dataset, object_id, object_fig_dir)
  }, error = function(e) {
    stamp("QC plot 失败但不中断：", dataset, " :: ", object_id, "；", conditionMessage(e))
  })

  if (SAVE_UPDATED_SEURAT_OBJECTS) {
    if (!REBUILD_EXISTING && file.exists(saved_path) && object_is_readable(saved_path)) {
      stamp("已存在可读 02A 对象，跳过保存：", saved_path)
    } else {
      stamp("保存 02A QC Seurat object：", saved_path)
      saveRDS(obj, saved_path, compress = SAVE_RDS_COMPRESS)
    }

    status_row$saved_path <- saved_path
  }

  status_row$status <- "SUCCESS_SEURAT_QC_METRICS"
  status_row$message <- "Seurat 对象可读；QC metrics 已计算。"

  status_list[[length(status_list) + 1L]] <- status_row

  rm(obj, counts, meta, cell_qc)
  gc(verbose = FALSE)
}

status_df <- data.table::rbindlist(status_list, fill = TRUE)
atomic_write_csv(status_df, status_csv)

if (length(summary_list) > 0L) {
  summary_df <- data.table::rbindlist(summary_list, fill = TRUE)
} else {
  summary_df <- data.frame()
}
atomic_write_csv(summary_df, summary_csv)

if (length(cell_manifest_files) > 0L) {
  cell_manifest_df <- data.table::rbindlist(cell_manifest_files, fill = TRUE)
} else {
  cell_manifest_df <- data.frame()
}
atomic_write_csv(cell_manifest_df, cell_manifest_csv)

if (length(sample_summary_list) > 0L) {
  sample_summary_df <- data.table::rbindlist(sample_summary_list, fill = TRUE)
} else {
  sample_summary_df <- data.frame()
}
atomic_write_csv(sample_summary_df, sample_summary_csv)

if (length(non_seurat_list) > 0L) {
  non_seurat_df <- data.table::rbindlist(non_seurat_list, fill = TRUE)
} else {
  non_seurat_df <- data.frame()
}
atomic_write_csv(non_seurat_df, non_seurat_csv)

if (length(all_cell_names_index) > 0L) {
  cell_index_df <- data.table::rbindlist(all_cell_names_index, fill = TRUE)
  dup_df <- cell_index_df[
    duplicated(cell_index_df$cell_barcode) |
      duplicated(cell_index_df$cell_barcode, fromLast = TRUE),
    ,
    drop = FALSE
  ]

  if (nrow(dup_df) > 0L) {
    dup_df <- dup_df[order(dup_df$cell_barcode), , drop = FALSE]
  }
} else {
  dup_df <- data.frame()
}

atomic_write_csv(dup_df, duplicate_cells_csv)

n_total <- nrow(status_df)
n_success_seurat <- sum(status_df$status == "SUCCESS_SEURAT_QC_METRICS", na.rm = TRUE)
n_non_seurat <- sum(grepl("NON_SEURAT", status_df$status), na.rm = TRUE)
n_failed <- sum(grepl("^FAILED", status_df$status), na.rm = TRUE)
n_cells_total <- if ("n_cells" %in% colnames(summary_df)) sum(summary_df$n_cells, na.rm = TRUE) else 0
n_duplicate_cells <- nrow(dup_df)

report_lines <- c(
  "02A V2 object integrity and QC metrics report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Total RDS objects scanned: ", n_total),
  paste0("Successful Seurat QC objects: ", n_success_seurat),
  paste0("Non-Seurat / bulk-like skipped objects: ", n_non_seurat),
  paste0("Failed objects: ", n_failed),
  paste0("Total Seurat cells summarized: ", n_cells_total),
  paste0("Duplicate cell-name rows across objects: ", n_duplicate_cells),
  "",
  "Output files:",
  paste0("Status CSV: ", status_csv),
  paste0("Object QC summary CSV: ", summary_csv),
  paste0("Cell QC manifest CSV: ", cell_manifest_csv),
  paste0("Sample-level QC summary CSV: ", sample_summary_csv),
  paste0("Non-Seurat objects CSV: ", non_seurat_csv),
  paste0("Duplicate cell names CSV: ", duplicate_cells_csv),
  paste0("02A updated Seurat objects: ", out_objects_dir),
  paste0("Basic QC figures: ", out_figures_dir),
  "",
  "Interpretation:",
  "02A only calculates and records QC metrics. It does not remove cells.",
  "02B should inspect these metrics and decide filtering thresholds.",
  "",
  "Next step:",
  "02B_QC_VISUALIZATION_AND_FILTERING_DECISION.R"
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("02A V2 object integrity and QC metrics 运行结束\n")
cat("============================================================\n\n")

cat("总 RDS 对象数量：", n_total, "\n")
cat("成功计算 QC 的 Seurat 对象数量：", n_success_seurat, "\n")
cat("跳过的 non-Seurat / bulk-like 对象数量：", n_non_seurat, "\n")
cat("失败对象数量：", n_failed, "\n")
cat("Seurat 细胞总数：", n_cells_total, "\n")
cat("跨对象重复 cell name 行数：", n_duplicate_cells, "\n\n")

cat("输出文件：\n")
cat(status_csv, "\n")
cat(summary_csv, "\n")
cat(cell_manifest_csv, "\n")
cat(sample_summary_csv, "\n")
cat(non_seurat_csv, "\n")
cat(duplicate_cells_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 02A V2 object integrity and QC metrics 完成。\n")
  cat("下一步可以进入 02B：QC 可视化、阈值判断和过滤策略。\n")
} else {
  cat("⚠️ 02A 完成，但存在失败对象。请先查看 02A_object_integrity_status.csv。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

REBUILD_EXISTING <- TRUE

APPLY_FILTERING_AND_SAVE_OBJECTS <- TRUE

SAVE_QC_PLOTS <- TRUE

PLOT_MAX_CELLS <- 50000L

SAVE_RDS_COMPRESS <- FALSE

NFEATURE_LOW_DEFAULT <- 200L
NFEATURE_LOW_LOWDEPTH <- 100L
LOW_DEPTH_MEDIAN_NFEATURE_CUTOFF <- 500L

NFEATURE_HIGH_QUANTILE <- 0.995

NCOUNT_HIGH_QUANTILE <- 0.995

MT_ENABLE_IF_P95_GT <- 0.1
MT_MIN_CUTOFF <- 15
MT_MAX_CUTOFF <- 30
MT_QUANTILE <- 0.99

NCOUNT_LOW_DEFAULT <- 0

cat("\n============================================================\n")
cat("02B：QC visualization and filtering decision\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "Matrix", "data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 02B。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(Matrix)
  library(data.table)
  library(ggplot2)
})

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_status_csv <- file.path(metadata_dir, "02A_object_integrity_status.csv")
input_manifest_csv <- file.path(tables_dir, "02A_qc", "02A_cell_qc_metrics_manifest.csv")
input_summary_csv <- file.path(tables_dir, "02A_qc", "02A_seurat_object_qc_summary.csv")

out_objects_dir <- file.path(objects_dir, "02B_qc_filtered")
out_tables_dir <- file.path(tables_dir, "02B_qc")
out_figures_dir <- file.path(figures_dir, "02B_qc")

dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

threshold_csv <- file.path(out_tables_dir, "02B_auto_qc_thresholds.csv")
cell_filter_summary_csv <- file.path(out_tables_dir, "02B_cell_filtering_summary.csv")
filtered_manifest_csv <- file.path(out_tables_dir, "02B_filtered_object_manifest.csv")
failed_csv <- file.path(out_tables_dir, "02B_failed_objects.csv")
report_txt <- file.path(reports_dir, "02B_QC_visualization_and_filtering_decision_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

safe_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "/", x)
  x <- basename(x)
  x <- gsub("\\.rds$|\\.csv$|\\.tsv$|\\.txt$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

safe_quantile <- function(x, prob) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path, "\n请先确认 02A V2 已成功完成。")
  }

  data.table::fread(path, data.table = FALSE)
}

make_threshold_for_object <- function(cell_qc, dataset, object_id) {
  nf <- as.numeric(cell_qc$nFeature_RNA)
  nc <- as.numeric(cell_qc$nCount_RNA)
  mt <- as.numeric(cell_qc$percent.mt)

  nf_median <- stats::median(nf, na.rm = TRUE)
  nc_median <- stats::median(nc, na.rm = TRUE)

  nf_min <- ifelse(
    is.finite(nf_median) && nf_median < LOW_DEPTH_MEDIAN_NFEATURE_CUTOFF,
    NFEATURE_LOW_LOWDEPTH,
    NFEATURE_LOW_DEFAULT
  )

  nf_max <- ceiling(safe_quantile(nf, NFEATURE_HIGH_QUANTILE))
  nc_min <- NCOUNT_LOW_DEFAULT
  nc_max <- ceiling(safe_quantile(nc, NCOUNT_HIGH_QUANTILE))

  mt_p95 <- safe_quantile(mt, 0.95)
  mt_p99 <- safe_quantile(mt, MT_QUANTILE)

  use_mt <- is.finite(mt_p95) && mt_p95 > MT_ENABLE_IF_P95_GT

  if (use_mt) {
    mt_max <- min(MT_MAX_CUTOFF, max(MT_MIN_CUTOFF, ceiling(mt_p99)))
  } else {
    mt_max <- 100
  }

  data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells_before = nrow(cell_qc),
    nFeature_min = as.numeric(nf_min),
    nFeature_max = as.numeric(nf_max),
    nCount_min = as.numeric(nc_min),
    nCount_max = as.numeric(nc_max),
    percent_mt_max = as.numeric(mt_max),
    use_percent_mt_filter = use_mt,
    nFeature_median = nf_median,
    nFeature_p01 = safe_quantile(nf, 0.01),
    nFeature_p05 = safe_quantile(nf, 0.05),
    nFeature_p95 = safe_quantile(nf, 0.95),
    nFeature_p995 = safe_quantile(nf, 0.995),
    nCount_median = nc_median,
    nCount_p995 = safe_quantile(nc, 0.995),
    percent_mt_median = stats::median(mt, na.rm = TRUE),
    percent_mt_p95 = mt_p95,
    percent_mt_p99 = mt_p99,
    stringsAsFactors = FALSE
  )
}

apply_filter_to_cell_qc <- function(cell_qc, thr) {
  keep <- rep(TRUE, nrow(cell_qc))

  keep <- keep & as.numeric(cell_qc$nFeature_RNA) >= thr$nFeature_min
  keep <- keep & as.numeric(cell_qc$nFeature_RNA) <= thr$nFeature_max
  keep <- keep & as.numeric(cell_qc$nCount_RNA) >= thr$nCount_min
  keep <- keep & as.numeric(cell_qc$nCount_RNA) <= thr$nCount_max

  if (isTRUE(thr$use_percent_mt_filter)) {
    keep <- keep & as.numeric(cell_qc$percent.mt) <= thr$percent_mt_max
  }

  keep[is.na(keep)] <- FALSE
  keep
}

plot_overall_retention <- function(summary_df, out_dir) {
  if (!SAVE_QC_PLOTS || nrow(summary_df) == 0L) return(invisible(NULL))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  p <- ggplot(summary_df, aes(x = reorder(object_id, retention_rate), y = retention_rate)) +
    geom_col() +
    coord_flip() +
    theme_bw(base_size = 10) +
    labs(
      title = "02B cell retention rate after conservative QC filtering",
      x = "Object",
      y = "Retention rate"
    )

  ggsave(
    filename = file.path(out_dir, "02B_overall_retention_rate.pdf"),
    plot = p,
    width = 10,
    height = max(6, 0.18 * nrow(summary_df)),
    limitsize = FALSE
  )
}

plot_object_qc_before_after <- function(cell_qc, keep, dataset, object_id, out_dir) {
  if (!SAVE_QC_PLOTS) return(invisible(NULL))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  plot_df <- cell_qc

  if (nrow(plot_df) > PLOT_MAX_CELLS) {
    set.seed(20260714)
    idx <- sample(seq_len(nrow(plot_df)), PLOT_MAX_CELLS)
    plot_df <- plot_df[idx, , drop = FALSE]
    keep_plot <- keep[idx]
  } else {
    keep_plot <- keep
  }

  plot_df$qc_keep_02B <- ifelse(keep_plot, "kept", "removed")

  p1 <- ggplot(plot_df, aes(x = nFeature_RNA, fill = qc_keep_02B)) +
    geom_histogram(bins = 80, alpha = 0.65, position = "identity") +
    theme_bw(base_size = 10) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nFeature_RNA before/after QC decision",
      x = "nFeature_RNA",
      y = "Cell count"
    )

  p2 <- ggplot(plot_df, aes(x = percent.mt, fill = qc_keep_02B)) +
    geom_histogram(bins = 80, alpha = 0.65, position = "identity") +
    theme_bw(base_size = 10) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "percent.mt before/after QC decision",
      x = "percent.mt",
      y = "Cell count"
    )

  p3 <- ggplot(plot_df, aes(x = nCount_RNA, y = nFeature_RNA, color = qc_keep_02B)) +
    geom_point(alpha = 0.35, size = 0.3) +
    theme_bw(base_size = 10) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nCount_RNA vs nFeature_RNA",
      x = "nCount_RNA",
      y = "nFeature_RNA"
    )

  file_pdf <- file.path(
    out_dir,
    paste0(safe_name(paste(dataset, object_id, sep = "__")), "_02B_qc_before_after.pdf")
  )

  grDevices::pdf(file_pdf, width = 9, height = 7)
  print(p1)
  print(p2)
  print(p3)
  grDevices::dev.off()

  invisible(file_pdf)
}

stamp("读取 02A V2 输出。")

status_df <- read_csv_required(input_status_csv)
manifest_df <- read_csv_required(input_manifest_csv)
summary_df_02A <- read_csv_required(input_summary_csv)

success_df <- status_df[
  status_df$status == "SUCCESS_SEURAT_QC_METRICS" &
    !is.na(status_df$saved_path) &
    file.exists(status_df$saved_path),
  ,
  drop = FALSE
]

if (nrow(success_df) == 0L) {
  stop("02A status 中没有找到成功的 Seurat QC 对象。请确认 02A V2 已完成。")
}

stamp("02A 成功 Seurat 对象数量：", nrow(success_df))

threshold_list <- list()
failed_list <- list()

for (i in seq_len(nrow(manifest_df))) {
  dataset <- manifest_df$dataset[[i]]
  object_id <- manifest_df$object_id[[i]]
  cell_qc_csv <- manifest_df$cell_qc_csv[[i]]

  if (!file.exists(cell_qc_csv)) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "threshold",
      status = "FAILED_MISSING_CELL_QC_CSV",
      message = cell_qc_csv,
      stringsAsFactors = FALSE
    )
    next
  }

  cell_qc <- tryCatch({
    data.table::fread(cell_qc_csv, data.table = FALSE)
  }, error = function(e) {
    failed_list[[length(failed_list) + 1L]] <<- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "threshold",
      status = "FAILED_READ_CELL_QC_CSV",
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
    NULL
  })

  if (is.null(cell_qc)) next

  needed <- c("nFeature_RNA", "nCount_RNA", "percent.mt")
  if (!all(needed %in% colnames(cell_qc))) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "threshold",
      status = "FAILED_MISSING_QC_COLUMNS",
      message = paste(setdiff(needed, colnames(cell_qc)), collapse = ","),
      stringsAsFactors = FALSE
    )
    next
  }

  threshold_list[[length(threshold_list) + 1L]] <- make_threshold_for_object(
    cell_qc = cell_qc,
    dataset = dataset,
    object_id = object_id
  )
}

threshold_df <- data.table::rbindlist(threshold_list, fill = TRUE)
atomic_write_csv(threshold_df, threshold_csv)

stamp("已生成 QC threshold 表：", threshold_csv)

filter_summary_list <- list()
filtered_manifest_list <- list()

for (i in seq_len(nrow(success_df))) {
  dataset <- success_df$dataset[[i]]
  object_id <- success_df$object_id[[i]]
  saved_path_02A <- success_df$saved_path[[i]]

  stamp("02B 过滤对象 ", i, " / ", nrow(success_df), "：", dataset, " :: ", object_id)

  thr <- threshold_df[
    threshold_df$dataset == dataset & threshold_df$object_id == object_id,
    ,
    drop = FALSE
  ]

  if (nrow(thr) == 0L) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "filter",
      status = "FAILED_NO_THRESHOLD",
      message = "没有找到该对象的 threshold。",
      stringsAsFactors = FALSE
    )
    next
  }

  thr <- thr[1, , drop = FALSE]

  obj <- tryCatch({
    readRDS(saved_path_02A)
  }, error = function(e) {
    failed_list[[length(failed_list) + 1L]] <<- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "filter",
      status = "FAILED_READ_02A_OBJECT",
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
    NULL
  })

  if (is.null(obj)) next

  meta <- obj@meta.data
  meta$cell_barcode <- rownames(meta)

  needed <- c("nFeature_RNA", "nCount_RNA", "percent.mt")
  if (!all(needed %in% colnames(meta))) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "filter",
      status = "FAILED_MISSING_META_QC_COLUMNS",
      message = paste(setdiff(needed, colnames(meta)), collapse = ","),
      stringsAsFactors = FALSE
    )
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  keep <- apply_filter_to_cell_qc(meta, thr)
  keep_cells <- rownames(meta)[keep]

  n_before <- ncol(obj)
  n_after <- length(keep_cells)
  retention_rate <- ifelse(n_before > 0, n_after / n_before, NA_real_)

  obj$qc_keep_02B <- keep
  obj$qc_reason_02B <- ifelse(keep, "kept", "removed_by_conservative_qc")
  obj$qc_stage <- "02B_qc_filtered"

  tryCatch({
    plot_object_qc_before_after(
      cell_qc = meta,
      keep = keep,
      dataset = dataset,
      object_id = object_id,
      out_dir = file.path(out_figures_dir, dataset)
    )
  }, error = function(e) {
    stamp("02B QC plot 失败但不中断：", dataset, " :: ", object_id, "；", conditionMessage(e))
  })

  out_dir_obj <- file.path(out_objects_dir, dataset)
  dir.create(out_dir_obj, recursive = TRUE, showWarnings = FALSE)

  out_rds <- file.path(out_dir_obj, paste0(object_id, "_02B_filtered.rds"))

  if (APPLY_FILTERING_AND_SAVE_OBJECTS) {
    if (!REBUILD_EXISTING && file.exists(out_rds)) {
      stamp("已存在 02B filtered object，跳过保存：", out_rds)
    } else {
      obj_filtered <- subset(obj, cells = keep_cells)
      saveRDS(obj_filtered, out_rds, compress = SAVE_RDS_COMPRESS)
      rm(obj_filtered)
    }
  }

  filter_summary_list[[length(filter_summary_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells_before = n_before,
    n_cells_after = n_after,
    n_cells_removed = n_before - n_after,
    retention_rate = retention_rate,
    nFeature_min = thr$nFeature_min,
    nFeature_max = thr$nFeature_max,
    nCount_min = thr$nCount_min,
    nCount_max = thr$nCount_max,
    percent_mt_max = thr$percent_mt_max,
    use_percent_mt_filter = thr$use_percent_mt_filter,
    stringsAsFactors = FALSE
  )

  filtered_manifest_list[[length(filtered_manifest_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    filtered_rds = out_rds,
    n_cells_before = n_before,
    n_cells_after = n_after,
    retention_rate = retention_rate,
    stringsAsFactors = FALSE
  )

  rm(obj, meta)
  gc(verbose = FALSE)
}

filter_summary_df <- data.table::rbindlist(filter_summary_list, fill = TRUE)
filtered_manifest_df <- data.table::rbindlist(filtered_manifest_list, fill = TRUE)

atomic_write_csv(filter_summary_df, cell_filter_summary_csv)
atomic_write_csv(filtered_manifest_df, filtered_manifest_csv)

if (length(failed_list) > 0L) {
  failed_df <- data.table::rbindlist(failed_list, fill = TRUE)
} else {
  failed_df <- data.frame()
}
atomic_write_csv(failed_df, failed_csv)

tryCatch({
  plot_overall_retention(filter_summary_df, out_figures_dir)
}, error = function(e) {
  stamp("总体 retention plot 失败但不中断：", conditionMessage(e))
})

n_objects <- nrow(success_df)
n_filtered_objects <- nrow(filtered_manifest_df)
n_failed <- nrow(failed_df)

total_before <- if ("n_cells_before" %in% colnames(filter_summary_df)) {
  sum(filter_summary_df$n_cells_before, na.rm = TRUE)
} else {
  0
}

total_after <- if ("n_cells_after" %in% colnames(filter_summary_df)) {
  sum(filter_summary_df$n_cells_after, na.rm = TRUE)
} else {
  0
}

overall_retention <- ifelse(total_before > 0, total_after / total_before, NA_real_)

report_lines <- c(
  "02B QC visualization and filtering decision report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  paste0("02A status: ", input_status_csv),
  paste0("02A manifest: ", input_manifest_csv),
  paste0("02A summary: ", input_summary_csv),
  "",
  "Summary:",
  paste0("02A successful Seurat objects used: ", n_objects),
  paste0("Filtered objects saved: ", n_filtered_objects),
  paste0("Failed objects/tasks: ", n_failed),
  paste0("Total cells before QC filtering: ", total_before),
  paste0("Total cells after QC filtering: ", total_after),
  paste0("Overall retention rate: ", round(overall_retention * 100, 3), "%"),
  "",
  "Filtering rule:",
  paste0("nFeature min default: ", NFEATURE_LOW_DEFAULT, " or ", NFEATURE_LOW_LOWDEPTH, " for low-depth objects"),
  paste0("nFeature max quantile: ", NFEATURE_HIGH_QUANTILE),
  paste0("nCount max quantile: ", NCOUNT_HIGH_QUANTILE),
  paste0("percent.mt filter enabled when p95 > ", MT_ENABLE_IF_P95_GT),
  paste0("percent.mt max: max(", MT_MIN_CUTOFF, ", p99), capped at ", MT_MAX_CUTOFF),
  "",
  "Output files:",
  paste0("Thresholds: ", threshold_csv),
  paste0("Cell filtering summary: ", cell_filter_summary_csv),
  paste0("Filtered object manifest: ", filtered_manifest_csv),
  paste0("Failed objects/tasks: ", failed_csv),
  paste0("Filtered Seurat objects: ", out_objects_dir),
  paste0("QC figures: ", out_figures_dir),
  "",
  "Next step:",
  "03A_NORMALIZATION_AND_PER_DATASET_REDUCTION.R",
  "",
  "Important note:",
  "02B uses conservative automatic thresholds. Before manuscript-level claims, inspect threshold and retention tables."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("02B QC visualization and filtering decision 运行结束\n")
cat("============================================================\n\n")

cat("02A 成功 Seurat 对象数量：", n_objects, "\n")
cat("已保存 filtered objects 数量：", n_filtered_objects, "\n")
cat("失败任务数量：", n_failed, "\n")
cat("过滤前总细胞数：", total_before, "\n")
cat("过滤后总细胞数：", total_after, "\n")
cat("总体保留率：", round(overall_retention * 100, 3), "%\n\n")

cat("输出文件：\n")
cat(threshold_csv, "\n")
cat(cell_filter_summary_csv, "\n")
cat(filtered_manifest_csv, "\n")
cat(failed_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 02B QC visualization and filtering decision 完成。\n")
  cat("下一步可以进入 03A：Normalize / variable features / PCA / UMAP 初步降维。\n")
} else {
  cat("⚠️ 02B 完成，但存在失败任务。请查看 failed CSV。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

REBUILD_EXISTING <- FALSE

SAVE_BASIC_PLOTS <- FALSE

N_VARIABLE_FEATURES <- 2000L
MAX_NPCS <- 20L
MAX_UMAP_DIMS <- 20L
CLUSTER_RESOLUTION <- 0.5

MIN_CELLS_FOR_REDUCTION <- 50L
MIN_FEATURES_FOR_REDUCTION <- 200L

SAVE_RDS_COMPRESS <- FALSE

FORCE_PCA_ONLY_ALL_OBJECTS <- TRUE

SAFE_LARGE_OBJECT_MODE <- TRUE
LARGE_OBJECT_CELL_CUTOFF <- 0L

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

options(future.globals.maxSize = 8 * 1024^3)
options(expressions = 5e5)

if (requireNamespace("future", quietly = TRUE)) {
  future::plan("sequential")
}

cat("\n============================================================\n")
cat("03A V3 PCA-only resume：normalization and per-object reduction\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "Matrix", "data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 03A V2。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(Matrix)
  library(data.table)
  library(ggplot2)
})

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest_csv <- file.path(tables_dir, "02B_qc", "02B_filtered_object_manifest.csv")

out_objects_dir <- file.path(objects_dir, "03A_normalized_reduced")
out_tables_dir <- file.path(tables_dir, "03A_reduction")
out_figures_dir <- file.path(figures_dir, "03A_reduction")

dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

status_csv <- file.path(metadata_dir, "03A_reduction_status.csv")
object_summary_csv <- file.path(out_tables_dir, "03A_object_reduction_summary.csv")
reduced_manifest_csv <- file.path(out_tables_dir, "03A_reduced_object_manifest.csv")
report_txt <- file.path(reports_dir, "03A_normalization_and_per_object_reduction_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

safe_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "/", x)
  x <- basename(x)
  x <- gsub("\\.rds$|\\.csv$|\\.tsv$|\\.txt$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path, "\n请先确认 02B 已成功完成。")
  }
  data.table::fread(path, data.table = FALSE)
}

object_is_readable <- function(path) {
  tryCatch({
    readRDS(path)
    TRUE
  }, error = function(e) FALSE)
}

get_assay_for_analysis <- function(obj) {
  assays <- names(obj@assays)

  if ("RNA" %in% assays) return("RNA")

  da <- tryCatch({
    SeuratObject::DefaultAssay(obj)
  }, error = function(e) NA_character_)

  if (!is.na(da) && da %in% assays) return(da)

  if (length(assays) > 0L) return(assays[[1L]])

  NA_character_
}

get_counts_matrix <- function(obj, assay) {
  mat <- tryCatch({
    SeuratObject::GetAssayData(obj, assay = assay, layer = "counts")
  }, error = function(e1) {
    tryCatch({
      SeuratObject::GetAssayData(obj, assay = assay, slot = "counts")
    }, error = function(e2) NULL)
  })

  if (is.null(mat)) stop("无法读取 counts matrix。")
  mat
}

choose_npcs <- function(n_cells, n_features) {
  npcs <- min(MAX_NPCS, n_cells - 1L, n_features - 1L)
  if (!is.finite(npcs) || npcs < 5L) return(NA_integer_)
  as.integer(npcs)
}

choose_dims <- function(npcs) {
  dims_n <- min(MAX_UMAP_DIMS, npcs)
  if (!is.finite(dims_n) || dims_n < 5L) return(NULL)
  seq_len(as.integer(dims_n))
}

append_status <- function(status_list, summary_list, manifest_list) {
  status_df <- data.table::rbindlist(status_list, fill = TRUE)

  summary_df <- if (length(summary_list) > 0L) {
    data.table::rbindlist(summary_list, fill = TRUE)
  } else {
    data.frame()
  }

  manifest_df <- if (length(manifest_list) > 0L) {
    data.table::rbindlist(manifest_list, fill = TRUE)
  } else {
    data.frame()
  }

  atomic_write_csv(status_df, status_csv)
  atomic_write_csv(summary_df, object_summary_csv)
  atomic_write_csv(manifest_df, reduced_manifest_csv)
}

stamp("读取 02B filtered object manifest。")

manifest <- read_csv_required(input_manifest_csv)

needed_cols <- c("dataset", "object_id", "filtered_rds")
if (!all(needed_cols %in% colnames(manifest))) {
  stop("02B manifest 缺少必要列：", paste(setdiff(needed_cols, colnames(manifest)), collapse = ", "))
}

manifest <- manifest[file.exists(manifest$filtered_rds), , drop = FALSE]

if (nrow(manifest) == 0L) {
  stop("02B manifest 中没有可用 filtered_rds。")
}

stamp("找到 02B filtered Seurat objects：", nrow(manifest))

status_list <- list()
summary_list <- list()
reduced_manifest_list <- list()

for (i in seq_len(nrow(manifest))) {
  dataset <- manifest$dataset[[i]]
  object_id <- manifest$object_id[[i]]
  in_rds <- manifest$filtered_rds[[i]]

  out_dir_obj <- file.path(out_objects_dir, dataset)
  dir.create(out_dir_obj, recursive = TRUE, showWarnings = FALSE)

  out_rds <- file.path(out_dir_obj, paste0(object_id, "_03A_reduced.rds"))

  stamp("03A V2 处理对象 ", i, " / ", nrow(manifest), "：", dataset, " :: ", object_id)

  status_row <- data.frame(
    dataset = dataset,
    object_id = object_id,
    input_rds = in_rds,
    output_rds = out_rds,
    n_cells = NA_integer_,
    n_features = NA_integer_,
    assay_used = NA_character_,
    n_variable_features = NA_integer_,
    npcs = NA_integer_,
    dims_used = NA_character_,
    n_clusters = NA_integer_,
    large_object_safe_mode = FALSE,
    status = "PENDING",
    message = NA_character_,
    stringsAsFactors = FALSE
  )

  if (!REBUILD_EXISTING && file.exists(out_rds) && object_is_readable(out_rds)) {
    obj_tmp <- readRDS(out_rds)

    status_row$n_cells <- ncol(obj_tmp)
    status_row$n_features <- nrow(obj_tmp)
    status_row$status <- "SKIPPED_EXISTING"
    status_row$message <- "Existing 03A object is readable; skipped."

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = ncol(obj_tmp),
      n_features = nrow(obj_tmp),
      status = "SKIPPED_EXISTING",
      stringsAsFactors = FALSE
    )

    rm(obj_tmp)
    gc(verbose = FALSE)
    next
  }

  obj <- tryCatch({
    readRDS(in_rds)
  }, error = function(e) {
    status_row$status <<- "FAILED_READ_RDS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    next
  }

  assay <- get_assay_for_analysis(obj)

  if (is.na(assay) || !assay %in% names(obj@assays)) {
    status_row$status <- "FAILED_NO_VALID_ASSAY"
    status_row$message <- "No valid assay found."
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  tryCatch({
    SeuratObject::DefaultAssay(obj) <- assay
  }, error = function(e) NULL)

  counts <- tryCatch({
    get_counts_matrix(obj, assay)
  }, error = function(e) {
    status_row$status <<- "FAILED_GET_COUNTS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(counts)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  n_cells <- ncol(counts)
  n_features <- nrow(counts)

  status_row$n_cells <- n_cells
  status_row$n_features <- n_features
  status_row$assay_used <- assay

  if (n_cells < MIN_CELLS_FOR_REDUCTION || n_features < MIN_FEATURES_FOR_REDUCTION) {
    status_row$status <- "SKIPPED_TOO_SMALL"
    status_row$message <- paste0("Too small for reduction: cells=", n_cells, "; features=", n_features)

    obj$analysis_stage <- "03A_skipped_too_small"
    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  stamp("  NormalizeData")
  obj <- tryCatch({
    NormalizeData(obj, assay = assay, normalization.method = "LogNormalize", scale.factor = 10000, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_NORMALIZE"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  stamp("  FindVariableFeatures")
  obj <- tryCatch({
    FindVariableFeatures(obj, assay = assay, selection.method = "vst", nfeatures = min(N_VARIABLE_FEATURES, n_features), verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_VARIABLE_FEATURES"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  hvgs <- SeuratObject::VariableFeatures(obj, assay = assay)
  hvgs <- hvgs[hvgs %in% rownames(obj)]
  status_row$n_variable_features <- length(hvgs)

  if (length(hvgs) < 50L) {
    status_row$status <- "SKIPPED_TOO_FEW_HVGS"
    status_row$message <- paste0("Too few HVGs: ", length(hvgs))

    obj$analysis_stage <- "03A_skipped_too_few_hvgs"
    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  npcs <- choose_npcs(n_cells, length(hvgs))
  dims_use <- choose_dims(npcs)

  if (is.na(npcs) || is.null(dims_use)) {
    status_row$status <- "SKIPPED_INVALID_PCA_DIMS"
    status_row$message <- paste0("Invalid PCA dims: npcs=", npcs)

    obj$analysis_stage <- "03A_skipped_invalid_dims"
    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  status_row$npcs <- npcs
  status_row$dims_used <- paste(range(dims_use), collapse = ":")

  stamp("  ScaleData")
  obj <- tryCatch({
    ScaleData(obj, assay = assay, features = hvgs, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_SCALE"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  stamp("  RunPCA")
  obj <- tryCatch({
    RunPCA(obj, assay = assay, features = hvgs, npcs = npcs, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_PCA"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  if (FORCE_PCA_ONLY_ALL_OBJECTS || (SAFE_LARGE_OBJECT_MODE && n_cells > LARGE_OBJECT_CELL_CUTOFF)) {
    stamp("  V3 PCA-only 模式：cells=", n_cells, "，跳过 UMAP/Neighbors/Clusters，保留 PCA。")

    obj$analysis_stage <- "03A_v3_pca_only_resume"
    obj$dataset_03A <- dataset
    obj$object_id_03A <- object_id

    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_row$status <- "SUCCESS_03A_PCA_ONLY"
    status_row$message <- paste0("V3 PCA-only safe mode. cells=", n_cells)
    status_row$large_object_safe_mode <- TRUE

    status_list[[length(status_list) + 1L]] <- status_row

    summary_list[[length(summary_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      n_cells = n_cells,
      n_features = n_features,
      assay_used = assay,
      n_variable_features = length(hvgs),
      npcs = npcs,
      dims_used = paste(range(dims_use), collapse = ":"),
      n_clusters = NA_integer_,
      reduced_rds = out_rds,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      n_variable_features = length(hvgs),
      npcs = npcs,
      dims_used = paste(range(dims_use), collapse = ":"),
      n_clusters = NA_integer_,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  stamp("  RunUMAP / FindNeighbors / FindClusters")
  obj <- tryCatch({
    obj <- RunUMAP(
      obj,
      reduction = "pca",
      dims = dims_use,
      reduction.name = "umap",
      reduction.key = "UMAP_",
      umap.method = "uwot",
      metric = "cosine",
      n.neighbors = 30L,
      min.dist = 0.3,
      verbose = FALSE
    )

    obj <- FindNeighbors(obj, reduction = "pca", dims = dims_use, verbose = FALSE)
    obj <- FindClusters(obj, resolution = CLUSTER_RESOLUTION, verbose = FALSE)

    obj
  }, error = function(e) {
    status_row$status <<- "FAILED_UMAP_CLUSTER"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  n_clusters <- if ("seurat_clusters" %in% colnames(obj@meta.data)) {
    length(unique(as.character(obj$seurat_clusters)))
  } else {
    NA_integer_
  }

  status_row$n_clusters <- n_clusters

  obj$analysis_stage <- "03A_normalized_reduced"
  obj$dataset_03A <- dataset
  obj$object_id_03A <- object_id

  stamp("保存 03A reduced object：", out_rds)
  saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

  status_row$status <- "SUCCESS_03A_REDUCED"
  status_row$message <- "Normalize/HVG/Scale/PCA/UMAP/cluster completed."

  status_list[[length(status_list) + 1L]] <- status_row

  summary_list[[length(summary_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells = n_cells,
    n_features = n_features,
    assay_used = assay,
    n_variable_features = length(hvgs),
    npcs = npcs,
    dims_used = paste(range(dims_use), collapse = ":"),
    n_clusters = n_clusters,
    reduced_rds = out_rds,
    status = status_row$status,
    stringsAsFactors = FALSE
  )

  reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    reduced_rds = out_rds,
    n_cells = n_cells,
    n_features = n_features,
    n_variable_features = length(hvgs),
    npcs = npcs,
    dims_used = paste(range(dims_use), collapse = ":"),
    n_clusters = n_clusters,
    status = "SUCCESS_03A_REDUCED",
    stringsAsFactors = FALSE
  )

  append_status(status_list, summary_list, reduced_manifest_list)

  rm(obj, counts)
  gc(verbose = FALSE)
}

append_status(status_list, summary_list, reduced_manifest_list)

status_df <- data.table::fread(status_csv, data.table = FALSE)
manifest_df <- data.table::fread(reduced_manifest_csv, data.table = FALSE)

n_total <- nrow(status_df)
n_success_full <- sum(status_df$status == "SUCCESS_03A_REDUCED", na.rm = TRUE)
n_success_pca_only <- sum(status_df$status %in% c("SUCCESS_03A_PCA_ONLY", "SUCCESS_03A_PCA_ONLY_LARGE_OBJECT"), na.rm = TRUE)
n_skipped_existing <- sum(status_df$status == "SKIPPED_EXISTING", na.rm = TRUE)
n_skipped_other <- sum(grepl("^SKIPPED", status_df$status) & status_df$status != "SKIPPED_EXISTING", na.rm = TRUE)
n_failed <- sum(grepl("^FAILED", status_df$status), na.rm = TRUE)

total_cells_done <- if ("n_cells" %in% colnames(manifest_df)) {
  sum(manifest_df$n_cells, na.rm = TRUE)
} else {
  0
}

report_lines <- c(
  "03A V3 PCA-only resume normalization and per-object reduction report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  paste0("02B filtered object manifest: ", input_manifest_csv),
  "",
  "Summary:",
  paste0("Total records: ", n_total),
  paste0("Full success reduced objects: ", n_success_full),
  paste0("PCA-only objects: ", n_success_pca_only),
  paste0("Skipped existing objects: ", n_skipped_existing),
  paste0("Other skipped objects: ", n_skipped_other),
  paste0("Failed objects: ", n_failed),
  paste0("Cells represented in reduced manifest: ", total_cells_done),
  "",
  "Safe settings:",
  paste0("REBUILD_EXISTING: ", REBUILD_EXISTING),
  paste0("SAVE_BASIC_PLOTS: ", SAVE_BASIC_PLOTS),
  paste0("MAX_NPCS: ", MAX_NPCS),
  paste0("MAX_UMAP_DIMS: ", MAX_UMAP_DIMS),
  paste0("FORCE_PCA_ONLY_ALL_OBJECTS: ", FORCE_PCA_ONLY_ALL_OBJECTS),
  paste0("SAFE_LARGE_OBJECT_MODE: ", SAFE_LARGE_OBJECT_MODE),
  paste0("LARGE_OBJECT_CELL_CUTOFF: ", LARGE_OBJECT_CELL_CUTOFF),
  "",
  "Output files:",
  paste0("Status CSV: ", status_csv),
  paste0("Object summary CSV: ", object_summary_csv),
  paste0("Reduced manifest CSV: ", reduced_manifest_csv),
  paste0("Reduced objects: ", out_objects_dir),
  "",
  "Next step:",
  "03B_MERGE_WITHIN_DATASET_AND_BATCH_CHECK.R",
  "",
  "Important note:",
  "Objects marked PCA-only are intentionally kept safe to avoid Windows/RStudio memory abort caused by UMAP/neighbor graph steps. UMAP/clustering can be performed later after merging/integration."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("03A V3 PCA-only resume 运行结束\n")
cat("============================================================\n\n")

cat("总记录数：", n_total, "\n")
cat("完整 reduced 成功对象：", n_success_full, "\n")
cat("PCA-only 成功对象：", n_success_pca_only, "\n")
cat("跳过已有成功对象：", n_skipped_existing, "\n")
cat("其他跳过对象：", n_skipped_other, "\n")
cat("失败对象：", n_failed, "\n")
cat("manifest 中细胞总数：", total_cells_done, "\n\n")

cat("输出文件：\n")
cat(status_csv, "\n")
cat(object_summary_csv, "\n")
cat(reduced_manifest_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 03A V3 PCA-only resume 完成。\n")
  cat("下一步可以进入 03B：按 dataset 合并对象、检查 batch effect、准备 integration。\n")
} else {
  cat("⚠️ 03A V3 完成，但仍有失败对象。请查看 03A_reduction_status.csv。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

REBUILD_EXISTING <- FALSE

MAX_CELLS_PER_OBJECT_FOR_03B <- 500L

MAX_CELLS_SINGLE_OBJECT_DATASET <- 8000L

N_VARIABLE_FEATURES <- 2000L
MAX_NPCS <- 30L

SAVE_RDS_COMPRESS <- FALSE
SAVE_PCA_PLOTS <- TRUE
PLOT_MAX_CELLS <- 50000L

set.seed(20260714)

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

if (requireNamespace("future", quietly = TRUE)) {
  future::plan("sequential")
}

options(future.globals.maxSize = 8 * 1024^3)
options(expressions = 5e5)

cat("\n============================================================\n")
cat("03B V6：JoinLayers + dimnames-safe matrix-only dataset batch check\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "Matrix", "data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 03B V4。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(Matrix)
  library(data.table)
  library(ggplot2)
})

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest_csv <- file.path(tables_dir, "03A_reduction", "03A_reduced_object_manifest.csv")

out_objects_dir <- file.path(objects_dir, "03B_dataset_merged_pca")
out_tables_dir <- file.path(tables_dir, "03B_dataset_merge")
out_figures_dir <- file.path(figures_dir, "03B_dataset_merge")

dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

status_csv <- file.path(metadata_dir, "03B_dataset_merge_status.csv")
dataset_summary_csv <- file.path(out_tables_dir, "03B_dataset_merge_summary.csv")
cell_composition_csv <- file.path(out_tables_dir, "03B_cell_composition_by_dataset_object.csv")
pca_variance_csv <- file.path(out_tables_dir, "03B_pca_variance_summary.csv")
merged_manifest_csv <- file.path(out_tables_dir, "03B_merged_dataset_manifest.csv")
report_txt <- file.path(reports_dir, "03B_merge_within_dataset_and_batch_check_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

safe_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "/", x)
  x <- basename(x)
  x <- gsub("\\.rds$|\\.csv$|\\.tsv$|\\.txt$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path, "\n请先确认 03A V3 已完成。")
  }
  data.table::fread(path, data.table = FALSE)
}

object_is_readable <- function(path) {
  tryCatch({
    readRDS(path)
    TRUE
  }, error = function(e) FALSE)
}

get_assay_for_analysis <- function(obj) {
  assays <- names(obj@assays)
  if ("RNA" %in% assays) return("RNA")

  da <- tryCatch({
    SeuratObject::DefaultAssay(obj)
  }, error = function(e) NA_character_)

  if (!is.na(da) && da %in% assays) return(da)
  if (length(assays) > 0L) return(assays[[1L]])
  NA_character_
}

get_counts_matrix_robust <- function(obj, assay) {

  assay_obj <- obj[[assay]]

  get_features <- function() {
    feats <- tryCatch(rownames(assay_obj), error = function(e) NULL)
    if (is.null(feats) || length(feats) == 0L) {
      feats <- tryCatch(rownames(obj), error = function(e) NULL)
    }
    feats
  }

  get_cells <- function() {
    cells <- tryCatch(colnames(assay_obj), error = function(e) NULL)
    if (is.null(cells) || length(cells) == 0L) {
      cells <- tryCatch(colnames(obj), error = function(e) NULL)
    }
    cells
  }

  repair_dimnames <- function(m) {
    if (is.null(m)) return(NULL)

    if (!inherits(m, "dgCMatrix")) {
      m <- as(m, "dgCMatrix")
    }

    feats <- get_features()
    cells <- get_cells()

    if (
      !is.null(feats) && !is.null(cells) &&
        nrow(m) == length(cells) && ncol(m) == length(feats)
    ) {
      m <- Matrix::t(m)
    }

    if ((is.null(rownames(m)) || any(is.na(rownames(m))) || any(rownames(m) == "")) &&
        !is.null(feats) && nrow(m) == length(feats)) {
      rownames(m) <- feats
    }

    if ((is.null(colnames(m)) || any(is.na(colnames(m))) || any(colnames(m) == "")) &&
        !is.null(cells) && ncol(m) == length(cells)) {
      colnames(m) <- cells
    }

    if (is.null(rownames(m)) || is.null(colnames(m))) {
      return(NULL)
    }

    if (nrow(m) == 0L || ncol(m) == 0L) {
      return(NULL)
    }

    colnames(m) <- make.unique(colnames(m), sep = "__dupCell")
    rownames(m) <- make.unique(rownames(m), sep = "__dupGene")

    m
  }

  layer_names <- tryCatch({
    SeuratObject::Layers(assay_obj)
  }, error = function(e) {
    tryCatch(names(assay_obj@layers), error = function(e2) character())
  })

  for (lyr in unique(c("counts", grep("^counts", layer_names, value = TRUE)))) {
    if (is.na(lyr) || lyr == "") next

    m <- tryCatch({
      SeuratObject::LayerData(obj, assay = assay, layer = lyr, fast = FALSE)
    }, error = function(e) NULL)

    m <- repair_dimnames(m)

    if (!is.null(m)) {
      return(m)
    }
  }

  obj_joined <- tryCatch({
    SeuratObject::JoinLayers(obj, assay = assay)
  }, error = function(e1) {
    tryCatch({
      Seurat::JoinLayers(obj, assay = assay)
    }, error = function(e2) {
      NULL
    })
  })

  if (!is.null(obj_joined)) {
    m <- tryCatch({
      SeuratObject::LayerData(obj_joined, assay = assay, layer = "counts", fast = FALSE)
    }, error = function(e) NULL)

    m <- repair_dimnames(m)

    if (!is.null(m)) {
      return(m)
    }

    m <- tryCatch({
      SeuratObject::GetAssayData(obj_joined, assay = assay, layer = "counts")
    }, error = function(e) NULL)

    m <- repair_dimnames(m)

    if (!is.null(m)) {
      return(m)
    }
  }

  m <- tryCatch({
    SeuratObject::GetAssayData(obj, assay = assay, layer = "counts")
  }, error = function(e) NULL)

  m <- repair_dimnames(m)

  if (!is.null(m)) {
    return(m)
  }

  m <- tryCatch({
    obj[[assay]]@counts
  }, error = function(e) NULL)

  m <- repair_dimnames(m)

  if (!is.null(m)) {
    return(m)
  }

  for (lyr in unique(c("data", grep("^data", layer_names, value = TRUE)))) {
    if (is.na(lyr) || lyr == "") next

    m <- tryCatch({
      SeuratObject::LayerData(obj, assay = assay, layer = lyr, fast = FALSE)
    }, error = function(e) NULL)

    m <- repair_dimnames(m)

    if (!is.null(m)) {
      warning("Using data layer as fallback for 03B batch inspection only: ", lyr)
      return(m)
    }
  }

  stop(
    "无法读取 counts/data matrix；assay=",
    assay,
    "；available layers=",
    paste(layer_names, collapse = ",")
  )
}

expand_to_union_genes <- function(mat, union_genes) {
  if (!inherits(mat, "dgCMatrix")) {
    mat <- as(mat, "dgCMatrix")
  }

  current_genes <- rownames(mat)

  if (identical(current_genes, union_genes)) {
    return(mat)
  }

  sm <- Matrix::summary(mat)

  if (nrow(sm) == 0L) {
    return(Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      x = numeric(),
      dims = c(length(union_genes), ncol(mat)),
      dimnames = list(union_genes, colnames(mat))
    ))
  }

  row_map <- match(current_genes, union_genes)

  Matrix::sparseMatrix(
    i = row_map[sm$i],
    j = sm$j,
    x = sm$x,
    dims = c(length(union_genes), ncol(mat)),
    dimnames = list(union_genes, colnames(mat))
  )
}

choose_npcs <- function(n_cells, n_features) {
  npcs <- min(MAX_NPCS, n_cells - 1L, n_features - 1L)
  if (!is.finite(npcs) || npcs < 5L) return(NA_integer_)
  as.integer(npcs)
}

write_all_outputs <- function(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list) {
  status_df <- if (length(status_list) > 0L) data.table::rbindlist(status_list, fill = TRUE) else data.frame()
  dataset_summary_df <- if (length(dataset_summary_list) > 0L) data.table::rbindlist(dataset_summary_list, fill = TRUE) else data.frame()
  composition_df <- if (length(composition_list) > 0L) data.table::rbindlist(composition_list, fill = TRUE) else data.frame()
  pca_variance_df <- if (length(pca_variance_list) > 0L) data.table::rbindlist(pca_variance_list, fill = TRUE) else data.frame()
  merged_manifest_df <- if (length(merged_manifest_list) > 0L) data.table::rbindlist(merged_manifest_list, fill = TRUE) else data.frame()

  atomic_write_csv(status_df, status_csv)
  atomic_write_csv(dataset_summary_df, dataset_summary_csv)
  atomic_write_csv(composition_df, cell_composition_csv)
  atomic_write_csv(pca_variance_df, pca_variance_csv)
  atomic_write_csv(merged_manifest_df, merged_manifest_csv)
}

make_pca_plot <- function(obj, dataset, out_dir) {
  if (!SAVE_PCA_PLOTS) return(invisible(NULL))
  if (!"pca" %in% names(obj@reductions)) return(invisible(NULL))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  emb <- as.data.frame(SeuratObject::Embeddings(obj, reduction = "pca")[, 1:2, drop = FALSE])
  emb$cell <- rownames(emb)

  meta <- obj@meta.data
  meta$cell <- rownames(meta)

  keep_meta_cols <- intersect(c("cell", "object_id_03B", "dataset_03B"), colnames(meta))

  plot_df <- merge(emb, meta[, keep_meta_cols, drop = FALSE], by = "cell", all.x = TRUE)

  if (nrow(plot_df) > PLOT_MAX_CELLS) {
    set.seed(20260714)
    plot_df <- plot_df[sample(seq_len(nrow(plot_df)), PLOT_MAX_CELLS), , drop = FALSE]
  }

  p <- ggplot(plot_df, aes(x = PC_1, y = PC_2, color = object_id_03B)) +
    geom_point(alpha = 0.35, size = 0.25) +
    theme_bw(base_size = 10) +
    labs(
      title = paste0(dataset, " 03B matrix-only merged PCA batch check"),
      subtitle = "Downsampled PCA object for batch inspection only",
      x = "PC1",
      y = "PC2",
      color = "object"
    )

  pdf_file <- file.path(out_dir, paste0(dataset, "_03B_matrix_only_PCA_batch_check.pdf"))
  png_file <- file.path(out_dir, paste0(dataset, "_03B_matrix_only_PCA_batch_check.png"))

  grDevices::pdf(pdf_file, width = 8, height = 6)
  print(p)
  grDevices::dev.off()

  grDevices::png(png_file, width = 2400, height = 1800, res = 220)
  print(p)
  grDevices::dev.off()

  invisible(c(pdf_file, png_file))
}

stamp("读取 03A reduced object manifest。")

manifest <- read_csv_required(input_manifest_csv)

needed_cols <- c("dataset", "object_id", "reduced_rds")

if (!all(needed_cols %in% colnames(manifest))) {
  stop("03A manifest 缺少必要列：", paste(setdiff(needed_cols, colnames(manifest)), collapse = ", "))
}

manifest <- manifest[file.exists(manifest$reduced_rds), , drop = FALSE]

if (nrow(manifest) == 0L) {
  stop("03A manifest 中没有可用 reduced_rds。")
}

datasets <- unique(manifest$dataset)

stamp("准备合并 dataset 数量：", length(datasets))
stamp("03A reduced objects 数量：", nrow(manifest))

status_list <- list()
dataset_summary_list <- list()
composition_list <- list()
pca_variance_list <- list()
merged_manifest_list <- list()

for (ds in datasets) {
  stamp("处理 dataset：", ds)

  ds_manifest <- manifest[manifest$dataset == ds, , drop = FALSE]

  out_ds_dir <- file.path(out_objects_dir, ds)
  out_fig_ds_dir <- file.path(out_figures_dir, ds)

  dir.create(out_ds_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_fig_ds_dir, recursive = TRUE, showWarnings = FALSE)

  out_rds <- file.path(out_ds_dir, paste0(ds, "_03B_merged_pca.rds"))

  status_row <- data.frame(
    dataset = ds,
    n_input_objects = nrow(ds_manifest),
    output_rds = out_rds,
    n_cells_original = NA_integer_,
    n_cells_used = NA_integer_,
    n_features = NA_integer_,
    n_variable_features = NA_integer_,
    npcs = NA_integer_,
    downsampled = TRUE,
    status = "PENDING",
    message = NA_character_,
    stringsAsFactors = FALSE
  )

  if (!REBUILD_EXISTING && file.exists(out_rds) && object_is_readable(out_rds)) {
    obj_tmp <- readRDS(out_rds)

    status_row$n_cells_original <- ncol(obj_tmp)
    status_row$n_cells_used <- ncol(obj_tmp)
    status_row$n_features <- nrow(obj_tmp)
    status_row$status <- "SKIPPED_EXISTING"
    status_row$message <- "Existing 03B dataset object is readable."

    status_list[[length(status_list) + 1L]] <- status_row

    merged_manifest_list[[length(merged_manifest_list) + 1L]] <- data.frame(
      dataset = ds,
      merged_rds = out_rds,
      n_input_objects = nrow(ds_manifest),
      n_cells_original = ncol(obj_tmp),
      n_cells_used = ncol(obj_tmp),
      n_features = nrow(obj_tmp),
      n_variable_features = NA_integer_,
      npcs = NA_integer_,
      downsampled = NA,
      status = "SKIPPED_EXISTING",
      stringsAsFactors = FALSE
    )

    rm(obj_tmp)
    gc(verbose = FALSE)
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  mat_list <- list()
  meta_list <- list()
  all_genes <- character()

  original_total_cells <- 0L
  used_total_cells <- 0L

  comp_df <- data.frame(
    dataset = character(),
    object_id_03B = character(),
    n_cells_original_object = integer(),
    n_cells_used_object = integer(),
    fraction_within_dataset_used = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(ds_manifest))) {
    object_id <- ds_manifest$object_id[[i]]
    rds <- ds_manifest$reduced_rds[[i]]

    stamp("  读取 counts：", object_id)

    obj <- tryCatch(readRDS(rds), error = function(e) {
      status_row$status <<- "FAILED_READ_OBJECT"
      status_row$message <<- paste0("Failed object ", object_id, ": ", conditionMessage(e))
      NULL
    })

    if (is.null(obj)) next

    assay <- get_assay_for_analysis(obj)

    if (is.na(assay) || !assay %in% names(obj@assays)) {
      status_row$status <- "FAILED_NO_VALID_ASSAY"
      status_row$message <- paste0("No valid assay in ", object_id)
      rm(obj)
      gc(verbose = FALSE)
      next
    }

    counts <- tryCatch(get_counts_matrix_robust(obj, assay), error = function(e) {
      msg <- paste0("Failed counts ", object_id, ": ", conditionMessage(e))
      stamp("  counts 提取失败：", msg)
      status_row$status <<- "FAILED_GET_COUNTS"
      status_row$message <<- msg
      NULL
    })

    if (is.null(counts)) {
      rm(obj)
      gc(verbose = FALSE)
      next
    }

    stamp("    counts dim：", nrow(counts), " x ", ncol(counts), "；nnz=", length(counts@x))

    n_original <- ncol(counts)
    original_total_cells <- original_total_cells + n_original

    max_cells_this <- if (nrow(ds_manifest) == 1L) {
      MAX_CELLS_SINGLE_OBJECT_DATASET
    } else {
      MAX_CELLS_PER_OBJECT_FOR_03B
    }

    if (n_original > max_cells_this) {
      keep_cells <- sample(colnames(counts), max_cells_this)
    } else {
      keep_cells <- colnames(counts)
    }

    counts_sub <- counts[, keep_cells, drop = FALSE]

    new_cell_names <- paste0(safe_name(object_id), "__", colnames(counts_sub))
    colnames(counts_sub) <- new_cell_names

    meta <- obj@meta.data

    if (nrow(meta) > 0L && all(keep_cells %in% rownames(meta))) {
      meta <- meta[keep_cells, , drop = FALSE]
    } else {

      meta <- data.frame(row.names = keep_cells)
    }

    bad_cols <- vapply(meta, function(z) is.list(z) || is.data.frame(z), logical(1))
    if (any(bad_cols)) meta <- meta[, !bad_cols, drop = FALSE]

    rownames(meta) <- new_cell_names
    meta$dataset_03B <- ds
    meta$object_id_03B <- object_id
    meta$source_rds_03B <- rds

    mat_list[[length(mat_list) + 1L]] <- counts_sub
    meta_list[[length(meta_list) + 1L]] <- meta
    all_genes <- union(all_genes, rownames(counts_sub))

    used_total_cells <- used_total_cells + ncol(counts_sub)

    comp_df <- rbind(
      comp_df,
      data.frame(
        dataset = ds,
        object_id_03B = object_id,
        n_cells_original_object = n_original,
        n_cells_used_object = ncol(counts_sub),
        fraction_within_dataset_used = NA_real_,
        stringsAsFactors = FALSE
      )
    )

    rm(obj, counts, counts_sub, meta)
    gc(verbose = FALSE)
  }

  if (length(mat_list) == 0L) {
    status_row$status <- ifelse(status_row$status == "PENDING", "FAILED_NO_MATRICES", status_row$status)
    status_row$message <- ifelse(is.na(status_row$message), "No matrices collected.", status_row$message)
    status_list[[length(status_list) + 1L]] <- status_row
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  comp_df$fraction_within_dataset_used <- comp_df$n_cells_used_object / sum(comp_df$n_cells_used_object)
  composition_list[[length(composition_list) + 1L]] <- comp_df

  stamp("  union genes：", length(all_genes), "；used cells=", used_total_cells)
  stamp("  扩展到 union genes 并 cbind sparse matrices")

  mat_list2 <- lapply(mat_list, expand_to_union_genes, union_genes = all_genes)
  merged_counts <- do.call(Matrix::cbind2, mat_list2)

  if (is.null(merged_counts)) {
    merged_counts <- do.call(cbind, mat_list2)
  }

  rm(mat_list, mat_list2)
  gc(verbose = FALSE)

  merged_meta <- do.call(rbind, meta_list)
  rm(meta_list)
  gc(verbose = FALSE)

  merged_meta <- merged_meta[colnames(merged_counts), , drop = FALSE]

  stamp("  CreateSeuratObject")
  merged <- tryCatch({
    CreateSeuratObject(
      counts = merged_counts,
      assay = "RNA",
      meta.data = merged_meta,
      project = safe_name(ds)
    )
  }, error = function(e) {
    status_row$status <<- "FAILED_CREATE_SEURAT_FROM_MATRIX"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  rm(merged_counts, merged_meta)
  gc(verbose = FALSE)

  if (is.null(merged)) {
    status_list[[length(status_list) + 1L]] <- status_row
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  n_cells_used <- ncol(merged)
  n_features <- nrow(merged)

  status_row$n_cells_original <- original_total_cells
  status_row$n_cells_used <- n_cells_used
  status_row$n_features <- n_features
  status_row$downsampled <- original_total_cells != n_cells_used

  stamp("  NormalizeData")
  merged <- tryCatch({
    NormalizeData(merged, normalization.method = "LogNormalize", scale.factor = 10000, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_NORMALIZE"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(merged)) {
    status_list[[length(status_list) + 1L]] <- status_row
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  stamp("  FindVariableFeatures")
  merged <- tryCatch({
    FindVariableFeatures(merged, selection.method = "vst", nfeatures = min(N_VARIABLE_FEATURES, n_features), verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_HVG"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(merged)) {
    status_list[[length(status_list) + 1L]] <- status_row
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  hvgs <- SeuratObject::VariableFeatures(merged)
  hvgs <- hvgs[hvgs %in% rownames(merged)]

  status_row$n_variable_features <- length(hvgs)

  if (length(hvgs) < 50L) {
    status_row$status <- "FAILED_TOO_FEW_HVGS"
    status_row$message <- paste0("Too few HVGs: ", length(hvgs))
    status_list[[length(status_list) + 1L]] <- status_row
    rm(merged)
    gc(verbose = FALSE)
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  npcs <- choose_npcs(n_cells_used, length(hvgs))
  status_row$npcs <- npcs

  if (is.na(npcs)) {
    status_row$status <- "FAILED_INVALID_PCA_DIMS"
    status_row$message <- "Invalid PCA dims."
    status_list[[length(status_list) + 1L]] <- status_row
    rm(merged)
    gc(verbose = FALSE)
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  stamp("  ScaleData")
  merged <- tryCatch({
    ScaleData(merged, features = hvgs, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_SCALE"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(merged)) {
    status_list[[length(status_list) + 1L]] <- status_row
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  stamp("  RunPCA")
  merged <- tryCatch({
    RunPCA(merged, features = hvgs, npcs = npcs, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_PCA"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(merged)) {
    status_list[[length(status_list) + 1L]] <- status_row
    write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)
    next
  }

  stdev <- tryCatch(merged[["pca"]]@stdev, error = function(e) numeric())

  if (length(stdev) > 0L) {
    var_exp <- stdev^2 / sum(stdev^2)

    pca_variance_list[[length(pca_variance_list) + 1L]] <- data.frame(
      dataset = ds,
      PC = seq_along(var_exp),
      variance_explained = var_exp,
      cumulative_variance = cumsum(var_exp),
      stringsAsFactors = FALSE
    )
  }

  tryCatch(make_pca_plot(merged, ds, out_fig_ds_dir), error = function(e) {
    stamp("  PCA plot 失败但不中断：", conditionMessage(e))
  })

  merged$analysis_stage <- "03B_matrix_only_downsample_batch_check"
  merged$downsampled_03B <- status_row$downsampled

  stamp("  保存 03B matrix-only merged PCA object：", out_rds)
  saveRDS(merged, out_rds, compress = SAVE_RDS_COMPRESS)

  status_row$status <- ifelse(status_row$downsampled, "SUCCESS_03B_MATRIX_ONLY_DOWNSAMPLED_PCA", "SUCCESS_03B_MATRIX_ONLY_FULL_PCA")
  status_row$message <- paste0("Matrix-only dataset-level PCA completed. original_cells=", original_total_cells, "; used_cells=", n_cells_used)

  status_list[[length(status_list) + 1L]] <- status_row

  dataset_summary_list[[length(dataset_summary_list) + 1L]] <- data.frame(
    dataset = ds,
    n_input_objects = nrow(ds_manifest),
    n_cells_original = original_total_cells,
    n_cells_used = n_cells_used,
    n_features = n_features,
    n_variable_features = length(hvgs),
    npcs = npcs,
    downsampled = status_row$downsampled,
    merged_rds = out_rds,
    stringsAsFactors = FALSE
  )

  merged_manifest_list[[length(merged_manifest_list) + 1L]] <- data.frame(
    dataset = ds,
    merged_rds = out_rds,
    n_input_objects = nrow(ds_manifest),
    n_cells_original = original_total_cells,
    n_cells_used = n_cells_used,
    n_features = n_features,
    n_variable_features = length(hvgs),
    npcs = npcs,
    downsampled = status_row$downsampled,
    status = status_row$status,
    stringsAsFactors = FALSE
  )

  write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)

  rm(merged)
  gc(verbose = FALSE)
}

write_all_outputs(status_list, dataset_summary_list, composition_list, pca_variance_list, merged_manifest_list)

status_df <- data.table::fread(status_csv, data.table = FALSE)
merged_manifest_df <- data.table::fread(merged_manifest_csv, data.table = FALSE)

n_total <- nrow(status_df)
n_success <- sum(status_df$status %in% c(
  "SUCCESS_03B_MATRIX_ONLY_DOWNSAMPLED_PCA",
  "SUCCESS_03B_MATRIX_ONLY_FULL_PCA",
  "SUCCESS_03B_MERGED_PCA",
  "SUCCESS_03B_DOWNSAMPLED_MERGED_PCA"
), na.rm = TRUE)
n_skipped_existing <- sum(status_df$status == "SKIPPED_EXISTING", na.rm = TRUE)
n_failed <- sum(grepl("^FAILED", status_df$status), na.rm = TRUE)

total_cells_original <- if ("n_cells_original" %in% colnames(merged_manifest_df)) {
  sum(merged_manifest_df$n_cells_original, na.rm = TRUE)
} else {
  0
}

total_cells_used <- if ("n_cells_used" %in% colnames(merged_manifest_df)) {
  sum(merged_manifest_df$n_cells_used, na.rm = TRUE)
} else {
  0
}

report_lines <- c(
  "03B V6 JoinLayers dimnames-safe matrix-only dataset batch check report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  paste0("03A reduced manifest: ", input_manifest_csv),
  "",
  "Summary:",
  paste0("Datasets processed: ", n_total),
  paste0("Successful/available PCA datasets: ", n_success),
  paste0("Skipped existing datasets: ", n_skipped_existing),
  paste0("Failed datasets: ", n_failed),
  paste0("Original cells represented: ", total_cells_original),
  paste0("Cells used in 03B PCA objects: ", total_cells_used),
  "",
  "Settings:",
  paste0("MAX_CELLS_PER_OBJECT_FOR_03B: ", MAX_CELLS_PER_OBJECT_FOR_03B),
  paste0("MAX_CELLS_SINGLE_OBJECT_DATASET: ", MAX_CELLS_SINGLE_OBJECT_DATASET),
  paste0("N_VARIABLE_FEATURES: ", N_VARIABLE_FEATURES),
  paste0("MAX_NPCS: ", MAX_NPCS),
  "",
  "Output files:",
  paste0("Status CSV: ", status_csv),
  paste0("Dataset summary CSV: ", dataset_summary_csv),
  paste0("Cell composition CSV: ", cell_composition_csv),
  paste0("PCA variance CSV: ", pca_variance_csv),
  paste0("Merged dataset manifest CSV: ", merged_manifest_csv),
  paste0("Merged dataset objects: ", out_objects_dir),
  paste0("PCA figures: ", out_figures_dir),
  "",
  "Next step:",
  "03C_CROSS_DATASET_INTEGRATION_OR_MAIN_OBJECT_DESIGN.R",
  "",
  "Important note for manuscript rigor:",
  "03B V6 downsampled JoinLayers/dimnames-safe matrix-only PCA objects are for dataset-level batch inspection only. They must not be used for final cell-state frequency, DEG, enrichment, scoring, or ML conclusions. If data-layer fallback is used for any object, this is also only for PCA inspection."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("03B V6 JoinLayers dimnames-safe matrix-only batch check 运行结束\n")
cat("============================================================\n\n")

cat("dataset 记录数：", n_total, "\n")
cat("成功/可用 PCA datasets：", n_success, "\n")
cat("跳过已有 datasets：", n_skipped_existing, "\n")
cat("失败 datasets：", n_failed, "\n")
cat("原始代表细胞总数：", total_cells_original, "\n")
cat("03B PCA 使用细胞总数：", total_cells_used, "\n\n")

cat("输出文件：\n")
cat(status_csv, "\n")
cat(dataset_summary_csv, "\n")
cat(cell_composition_csv, "\n")
cat(pca_variance_csv, "\n")
cat(merged_manifest_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 03B V6 JoinLayers dimnames-safe matrix-only batch check 完成。\n")
  cat("下一步可以进入 03C：跨数据集整合策略设计。\n")
} else {
  cat("⚠️ 03B V6 完成，但仍存在失败 dataset。请查看 03B_dataset_merge_status.csv。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

cat("\n============================================================\n")
cat("03C：main analysis strategy and manifest\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 03C。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_02B_manifest <- file.path(tables_dir, "02B_qc", "02B_filtered_object_manifest.csv")
input_03A_manifest <- file.path(tables_dir, "03A_reduction", "03A_reduced_object_manifest.csv")
input_03A_status <- file.path(metadata_dir, "03A_reduction_status.csv")
input_03B_status <- file.path(metadata_dir, "03B_dataset_merge_status.csv")
input_03B_manifest <- file.path(tables_dir, "03B_dataset_merge", "03B_merged_dataset_manifest.csv")

out_tables_dir <- file.path(tables_dir, "03C_strategy")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

dataset_role_csv <- file.path(out_tables_dir, "03C_dataset_role_and_usage.csv")
main_manifest_csv <- file.path(out_tables_dir, "03C_main_analysis_object_manifest.csv")
final_vs_qc_csv <- file.path(out_tables_dir, "03C_final_vs_qc_object_usage.csv")
next_steps_csv <- file.path(out_tables_dir, "03C_recommended_next_steps.csv")
report_txt <- file.path(reports_dir, "03C_main_analysis_strategy_and_manifest_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  data.table::fread(path, data.table = FALSE)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

dataset_role <- function(ds) {
  if (ds == "GSE178265_DA_01B") {
    return("core_DA_target_cells")
  }
  if (ds %in% c("GSE132758", "GSE204796", "GSE233885")) {
    return("graft_single_cell_atlas_and_validation")
  }
  if (ds == "GSE157783") {
    return("external_midbrain_single_cell_reference")
  }
  if (ds == "GSE200610") {
    return("auxiliary_single_cell_reference")
  }
  if (ds == "GSE204795") {
    return("bulk_RNA_support_validation")
  }
  "other"
}

dataset_final_usage <- function(ds) {
  if (ds == "GSE178265_DA_01B") {
    return("Final DA-like identity, A9/A10-like signature, projection-associated competence, and safety-risk scoring.")
  }
  if (ds %in% c("GSE132758", "GSE204796", "GSE233885")) {
    return("Final graft-state atlas/supporting validation; used for conserved graft cell-state signatures and safety-risk comparison.")
  }
  if (ds == "GSE157783") {
    return("Reference/supporting single-cell dataset; used for external pattern comparison, not primary graft frequency claims.")
  }
  if (ds == "GSE200610") {
    return("Auxiliary/reference dataset; used cautiously for robustness checks, not primary claims unless metadata compatibility is confirmed.")
  }
  if (ds == "GSE204795") {
    return("Bulk-level orthogonal support for pathways/signatures, not single-cell clustering.")
  }
  "To be decided."
}

dataset_integration_strategy <- function(ds) {
  if (ds == "GSE178265_DA_01B") {
    return("Analyze as core DA-target object; integrate/scoring with relevant graft datasets only after marker QC.")
  }
  if (ds %in% c("GSE132758", "GSE204796", "GSE233885")) {
    return("Dataset-specific analysis first; later compare signatures across datasets. Avoid relying on one global all-cell integration before annotation.")
  }
  if (ds %in% c("GSE157783", "GSE200610")) {
    return("Use as reference/auxiliary; do not force into final graft atlas until cell-type comparability is verified.")
  }
  if (ds == "GSE204795") {
    return("Analyze separately as bulk/pseudobulk support.")
  }
  "Dataset-specific evaluation first."
}

stamp("读取 02B / 03A / 03B 输出。")

m02B <- read_csv_required(input_02B_manifest)
m03A <- read_csv_required(input_03A_manifest)
s03A <- read_csv_optional(input_03A_status)
s03B <- read_csv_optional(input_03B_status)
m03B <- read_csv_optional(input_03B_manifest)

if (!all(c("dataset", "object_id", "filtered_rds") %in% colnames(m02B))) {
  stop("02B manifest 缺少 dataset/object_id/filtered_rds。")
}

if (!all(c("dataset", "object_id", "reduced_rds") %in% colnames(m03A))) {
  stop("03A manifest 缺少 dataset/object_id/reduced_rds。")
}

datasets_from_sc <- unique(c(m02B$dataset, m03A$dataset))

datasets_all <- unique(c(datasets_from_sc, "GSE204795"))

dataset_role_df <- data.frame(
  dataset = datasets_all,
  role = vapply(datasets_all, dataset_role, character(1)),
  final_usage = vapply(datasets_all, dataset_final_usage, character(1)),
  integration_strategy = vapply(datasets_all, dataset_integration_strategy, character(1)),
  use_02B_full_filtered_for_final = datasets_all %in% datasets_from_sc,
  use_03A_pca_for_qc_or_initial_reduction = datasets_all %in% datasets_from_sc,
  use_03B_downsampled_for_final_claims = FALSE,
  use_03B_only_for_batch_inspection = datasets_all %in% datasets_from_sc,
  stringsAsFactors = FALSE
)

if (nrow(s03B) > 0 && "dataset" %in% colnames(s03B) && "status" %in% colnames(s03B)) {
  s03B_small <- s03B[, intersect(c("dataset", "status", "message"), colnames(s03B)), drop = FALSE]
  colnames(s03B_small)[colnames(s03B_small) == "status"] <- "status_03B"
  colnames(s03B_small)[colnames(s03B_small) == "message"] <- "message_03B"
  dataset_role_df <- merge(dataset_role_df, s03B_small, by = "dataset", all.x = TRUE)
} else {
  dataset_role_df$status_03B <- NA_character_
  dataset_role_df$message_03B <- NA_character_
}

atomic_write_csv(dataset_role_df, dataset_role_csv)

m02B_small <- m02B[, intersect(c(
  "dataset", "object_id", "filtered_rds",
  "n_cells_before", "n_cells_after", "retention_rate"
), colnames(m02B)), drop = FALSE]

m03A_small <- m03A[, intersect(c(
  "dataset", "object_id", "reduced_rds",
  "n_cells", "n_features", "n_variable_features", "npcs", "dims_used", "n_clusters", "status"
), colnames(m03A)), drop = FALSE]

main_manifest <- merge(
  m02B_small,
  m03A_small,
  by = c("dataset", "object_id"),
  all.x = TRUE,
  suffixes = c("_02B", "_03A")
)

main_manifest$dataset_role <- vapply(main_manifest$dataset, dataset_role, character(1))
main_manifest$final_usage <- vapply(main_manifest$dataset, dataset_final_usage, character(1))
main_manifest$final_expression_object <- main_manifest$filtered_rds
main_manifest$initial_pca_object <- main_manifest$reduced_rds
main_manifest$use_for_final_annotation <- TRUE
main_manifest$use_for_final_scoring <- TRUE
main_manifest$use_for_final_DEG <- TRUE
main_manifest$use_for_final_ML_feature_building <- TRUE
main_manifest$use_03B_downsampled_object <- FALSE

main_manifest$primary_claim_dataset <- !(main_manifest$dataset %in% c("GSE157783", "GSE200610"))

atomic_write_csv(main_manifest, main_manifest_csv)

usage_df <- data.frame(
  object_stage = c(
    "01A standardized objects",
    "01B GSE178265 DA submatrix object",
    "02A QC-metric objects",
    "02B full filtered objects",
    "03A per-object normalized/PCA objects",
    "03B dataset-level merged/downsampled PCA objects",
    "GSE204795 bulk DESeqDataSet"
  ),
  use_for = c(
    "Raw standardized import backup and reproducibility.",
    "Core DA target single-cell input for downstream analysis.",
    "QC metric audit and traceability.",
    "Primary final single-cell expression source after QC filtering.",
    "Initial per-object PCA/reduction; input reference for main object design and inspection.",
    "Batch inspection only. Not for final biological claims.",
    "Bulk-level pathway/signature support only."
  ),
  can_use_for_final_biological_claim = c(
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE
  ),
  can_use_for_DEG_or_ML = c(
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE
  ),
  notes = c(
    "Do not analyze unfiltered cells directly.",
    "Use after QC filtering where available.",
    "Metrics only; not filtered expression source.",
    "Main full data source for annotation/scoring/DEG/ML.",
    "Useful but avoid over-interpreting per-object UMAP/cluster if present.",
    "Downsample/PCA only; 03B failures are documented and do not block main analysis.",
    "Separate bulk analysis; not merged with scRNA Seurat objects."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(usage_df, final_vs_qc_csv)

next_steps_df <- data.frame(
  step = c(
    "04A",
    "04B",
    "05A",
    "05B",
    "06A",
    "07A",
    "08A"
  ),
  name = c(
    "Marker gene panel and dataset-specific annotation preparation",
    "Cell-type annotation and marker validation",
    "DA-like/A9/A10-like identity scoring",
    "Projection-associated molecular competence scoring",
    "Safety-risk state scoring",
    "Pseudobulk/DEG/enrichment",
    "ML model with cross-validation and external validation"
  ),
  rigor_requirement = c(
    "Use curated marker lists and record marker source/version.",
    "Do not overclaim; annotation must be marker-supported and checked across datasets.",
    "Use predefined gene signatures; report gene coverage per dataset.",
    "Use molecular competence language, not real projection claims.",
    "Risk score is transcriptomic risk-associated state, not direct tumor proof.",
    "Use sample-aware/pseudobulk where possible; avoid treating cells as independent biological replicates for strong claims.",
    "Use train/test or cross-validation; report AUC, feature importance, and external validation."
  ),
  input_source = c(
    "03C_main_analysis_object_manifest.csv",
    "02B full filtered + 03A PCA objects",
    "02B/03A objects",
    "02B/03A objects",
    "02B/03A objects",
    "annotated objects + metadata",
    "scored/annotated objects + validation datasets"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(next_steps_df, next_steps_csv)

n_sc_objects <- nrow(main_manifest)
n_datasets <- length(unique(main_manifest$dataset))
n_primary_objects <- sum(main_manifest$primary_claim_dataset, na.rm = TRUE)
n_reference_objects <- sum(!main_manifest$primary_claim_dataset, na.rm = TRUE)

failed_03B <- if ("status_03B" %in% colnames(dataset_role_df)) {
  dataset_role_df$dataset[grepl("^FAILED", dataset_role_df$status_03B)]
} else {
  character()
}

report_lines <- c(
  "03C main analysis strategy and manifest report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Single-cell objects in main manifest: ", n_sc_objects),
  paste0("Single-cell datasets: ", n_datasets),
  paste0("Primary-claim objects: ", n_primary_objects),
  paste0("Reference/auxiliary objects: ", n_reference_objects),
  paste0("03B failed datasets documented: ", paste(failed_03B, collapse = ", ")),
  "",
  "Key decision:",
  "03B downsampled/batch-check objects are not used for final biological claims.",
  "Final biological analyses will use 02B full filtered objects and 03A per-object normalized/PCA outputs.",
  "",
  "Output files:",
  paste0("Dataset role table: ", dataset_role_csv),
  paste0("Main analysis manifest: ", main_manifest_csv),
  paste0("Final vs QC object usage: ", final_vs_qc_csv),
  paste0("Recommended next steps: ", next_steps_csv),
  "",
  "Next script:",
  "04A_MARKER_PANEL_AND_ANNOTATION_PREP.R",
  "",
  "Journal-rigor note:",
  "Downsampling and PCA-only modes were used only to stabilize QC/batch-inspection steps on Windows/RStudio. They are explicitly excluded from final DEG, enrichment, scoring, cell-frequency, and ML conclusions."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("03C main analysis strategy and manifest 运行结束\n")
cat("============================================================\n\n")

cat("single-cell objects in main manifest：", n_sc_objects, "\n")
cat("single-cell datasets：", n_datasets, "\n")
cat("primary-claim objects：", n_primary_objects, "\n")
cat("reference/auxiliary objects：", n_reference_objects, "\n")

if (length(failed_03B) > 0) {
  cat("03B failed datasets documented：", paste(failed_03B, collapse = ", "), "\n")
}

cat("\n输出文件：\n")
cat(dataset_role_csv, "\n")
cat(main_manifest_csv, "\n")
cat(final_vs_qc_csv, "\n")
cat(next_steps_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 03C main analysis strategy and manifest 完成。\n")
cat("下一步进入 04A：marker panel 和 annotation 准备。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

GENE_UNIVERSE_SOURCE <- "03A"

REBUILD_EXISTING <- TRUE

cat("\n============================================================\n")
cat("04A：marker panel and annotation prep\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04A。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(data.table)
})

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_main_manifest <- file.path(tables_dir, "03C_strategy", "03C_main_analysis_object_manifest.csv")
input_dataset_role <- file.path(tables_dir, "03C_strategy", "03C_dataset_role_and_usage.csv")

out_tables_dir <- file.path(tables_dir, "04A_annotation_prep")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

marker_master_csv <- file.path(out_tables_dir, "04A_marker_panel_master.csv")
marker_alias_csv <- file.path(out_tables_dir, "04A_marker_panel_alias_long.csv")
object_gene_summary_csv <- file.path(out_tables_dir, "04A_object_gene_universe_summary.csv")
object_marker_coverage_csv <- file.path(out_tables_dir, "04A_object_marker_coverage.csv")
dataset_marker_coverage_csv <- file.path(out_tables_dir, "04A_dataset_marker_coverage_summary.csv")
recommended_marker_csv <- file.path(out_tables_dir, "04A_recommended_marker_sets_for_04B.csv")
report_txt <- file.path(reports_dir, "04A_marker_panel_and_annotation_prep_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

title_case_gene <- function(x) {
  x <- as.character(x)
  out <- ifelse(
    nchar(x) <= 1,
    x,
    paste0(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))))
  )
  out
}

make_gene_aliases <- function(gene) {
  gene <- unique(as.character(gene))
  out <- unique(c(
    gene,
    toupper(gene),
    title_case_gene(gene),
    tolower(gene)
  ))
  out[!is.na(out) & nzchar(out)]
}

get_gene_universe <- function(obj) {
  genes <- tryCatch({
    rownames(obj)
  }, error = function(e) {
    character()
  })

  genes <- unique(as.character(genes))
  genes[!is.na(genes) & nzchar(genes)]
}

stamp("建立 marker panel。")

marker_panel <- data.frame(
  category = c(
    rep("DA_core_identity", 10),
    rep("A9_like_DA_identity", 7),
    rep("A10_like_DA_identity", 6),
    rep("midbrain_floor_plate_progenitor", 9),
    rep("neuronal_maturation_synapse", 10),
    rep("progenitor_neuroepithelial", 8),
    rep("cell_cycle_proliferation", 8),
    rep("pluripotency_immature_risk", 6),
    rep("astrocyte_glial", 7),
    rep("oligodendrocyte_OPC", 9),
    rep("microglia_macrophage_immune", 8),
    rep("vascular_pericyte_meningeal", 10),
    rep("GABAergic_neuron", 5),
    rep("glutamatergic_neuron", 5),
    rep("serotonergic_neuron", 4),
    rep("cholinergic_neuron", 3),
    rep("stress_apoptosis_response", 8),
    rep("extracellular_matrix_fibroblast", 8)
  ),
  gene_symbol = c(

    "TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "FOXA2", "LMX1A", "LMX1B", "PITX3", "EN1",

    "ALDH1A1", "KCNJ6", "SOX6", "DCLK3", "GCH1", "SLC10A4", "KCND3",

    "CALB1", "OTX2", "CCK", "SLC17A6", "VIP", "NRIP3",

    "FOXA2", "LMX1A", "LMX1B", "OTX2", "EN1", "EN2", "CORIN", "SHH", "WNT1",

    "RBFOX3", "MAP2", "TUBB3", "DCX", "STMN2", "SNAP25", "SYT1", "SYN1", "NEFL", "NEFM",

    "SOX2", "NES", "PAX6", "HES1", "HES5", "VIM", "ASCL1", "DCX",

    "MKI67", "TOP2A", "PCNA", "MCM2", "MCM5", "CENPF", "UBE2C", "CCNB1",

    "POU5F1", "NANOG", "LIN28A", "DPPA4", "TERT", "PROM1",

    "GFAP", "AQP4", "ALDH1L1", "SLC1A3", "S100B", "SOX9", "CLU",

    "OLIG1", "OLIG2", "PDGFRA", "CSPG4", "SOX10", "MBP", "PLP1", "MOG", "MAG",

    "PTPRC", "AIF1", "C1QA", "C1QB", "CX3CR1", "TYROBP", "LST1", "CD74",

    "PECAM1", "VWF", "KDR", "CLDN5", "PDGFRB", "RGS5", "ACTA2", "COL1A1", "COL1A2", "DCN",

    "GAD1", "GAD2", "SLC32A1", "DLX1", "DLX2",

    "SLC17A6", "SLC17A7", "SLC17A8", "TBR1", "NEUROD6",

    "TPH2", "SLC6A4", "FEV", "GATA3",

    "CHAT", "SLC18A3", "ACHE",

    "FOS", "JUN", "JUNB", "HSPA1A", "HSPA1B", "DDIT3", "ATF3", "BAX",

    "COL1A1", "COL1A2", "COL3A1", "DCN", "LUM", "FN1", "THY1", "TAGLN"
  ),
  interpretation = c(
    rep("Dopaminergic neuronal transcriptional identity; requires multi-marker support.", 10),
    rep("A9/substantia-nigra-like DA molecular identity; not proof of in vivo A9 function.", 7),
    rep("A10/VTA-like DA molecular identity; interpret relative to DA core markers.", 6),
    rep("Midbrain floor-plate/progenitor pattern; can indicate developmental stage.", 9),
    rep("Pan-neuronal maturation and synaptic features.", 10),
    rep("Neural progenitor / immature neuroepithelial-like state.", 8),
    rep("Cycling/proliferating cell state; important for graft safety-risk assessment.", 8),
    rep("Pluripotency/immature-risk-associated markers; absence/presence must be validated carefully.", 6),
    rep("Astrocytic or glial-associated identity.", 7),
    rep("OPC/oligodendrocyte lineage markers.", 9),
    rep("Immune/microglia/macrophage-like markers; species/context dependent.", 8),
    rep("Vascular, pericyte, mesenchymal or meningeal-associated markers.", 10),
    rep("GABAergic neuron markers.", 5),
    rep("Glutamatergic neuron markers.", 5),
    rep("Serotonergic neuron markers.", 4),
    rep("Cholinergic neuron markers.", 3),
    rep("Stress/apoptosis/immediate early response; not a cell type alone.", 8),
    rep("Extracellular matrix / fibroblast-like or mesenchymal markers.", 8)
  ),
  use_for = c(
    rep("DA annotation and DA-like score", 10),
    rep("A9-like score", 7),
    rep("A10-like score", 6),
    rep("Developmental-state annotation", 9),
    rep("Neuronal maturation score", 10),
    rep("Progenitor score", 8),
    rep("Cell-cycle/safety-risk score", 8),
    rep("Immature-risk score", 6),
    rep("Off-target/glial annotation", 7),
    rep("Off-target/oligodendrocyte annotation", 9),
    rep("Immune/off-target annotation", 8),
    rep("Vascular/mesenchymal/off-target annotation", 10),
    rep("Neuronal subtype annotation", 5),
    rep("Neuronal subtype annotation", 5),
    rep("Neuronal subtype annotation", 4),
    rep("Neuronal subtype annotation", 3),
    rep("Stress state score", 8),
    rep("ECM/mesenchymal score", 8)
  ),
  stringsAsFactors = FALSE
)

marker_panel$marker_id <- paste0(marker_panel$category, "__", marker_panel$gene_symbol)

atomic_write_csv(marker_panel, marker_master_csv)

alias_list <- list()

for (i in seq_len(nrow(marker_panel))) {
  aliases <- make_gene_aliases(marker_panel$gene_symbol[[i]])

  alias_list[[length(alias_list) + 1L]] <- data.frame(
    category = marker_panel$category[[i]],
    gene_symbol = marker_panel$gene_symbol[[i]],
    alias_symbol = aliases,
    marker_id = marker_panel$marker_id[[i]],
    stringsAsFactors = FALSE
  )
}

alias_df <- data.table::rbindlist(alias_list, fill = TRUE)
atomic_write_csv(alias_df, marker_alias_csv)

stamp("读取 03C main manifest。")

main_manifest <- read_csv_required(input_main_manifest)
dataset_role_df <- read_csv_required(input_dataset_role)

if (!all(c("dataset", "object_id") %in% colnames(main_manifest))) {
  stop("03C main manifest 缺少 dataset/object_id。")
}

if (GENE_UNIVERSE_SOURCE == "03A") {
  object_path_col <- "initial_pca_object"
} else {
  object_path_col <- "final_expression_object"
}

if (!object_path_col %in% colnames(main_manifest)) {
  stop("main manifest 中缺少对象路径列：", object_path_col)
}

main_manifest <- main_manifest[file.exists(main_manifest[[object_path_col]]), , drop = FALSE]

stamp("准备检查对象数量：", nrow(main_manifest))

object_summary_list <- list()
coverage_list <- list()

for (i in seq_len(nrow(main_manifest))) {
  ds <- main_manifest$dataset[[i]]
  oid <- main_manifest$object_id[[i]]
  path <- main_manifest[[object_path_col]][[i]]

  stamp("检查 gene universe：", i, " / ", nrow(main_manifest), "：", ds, " :: ", oid)

  obj <- tryCatch({
    readRDS(path)
  }, error = function(e) {
    NULL
  })

  if (is.null(obj)) {
    object_summary_list[[length(object_summary_list) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      object_path = path,
      status = "FAILED_READ_OBJECT",
      n_genes = NA_integer_,
      n_cells = NA_integer_,
      stringsAsFactors = FALSE
    )
    next
  }

  genes <- get_gene_universe(obj)

  object_summary_list[[length(object_summary_list) + 1L]] <- data.frame(
    dataset = ds,
    object_id = oid,
    object_path = path,
    status = "SUCCESS",
    n_genes = length(genes),
    n_cells = tryCatch(ncol(obj), error = function(e) NA_integer_),
    stringsAsFactors = FALSE
  )

  genes_upper <- toupper(genes)

  for (catg in unique(marker_panel$category)) {
    panel_genes <- unique(marker_panel$gene_symbol[marker_panel$category == catg])
    panel_upper <- toupper(panel_genes)

    present_upper <- panel_upper[panel_upper %in% genes_upper]
    missing_upper <- setdiff(panel_upper, genes_upper)

    present_actual <- genes[match(present_upper, genes_upper)]

    coverage_list[[length(coverage_list) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      category = catg,
      n_panel_genes = length(panel_upper),
      n_present = length(present_upper),
      coverage_fraction = ifelse(length(panel_upper) > 0, length(present_upper) / length(panel_upper), NA_real_),
      present_genes = paste(unique(present_actual), collapse = ";"),
      missing_genes = paste(missing_upper, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }

  rm(obj)
  gc(verbose = FALSE)
}

object_summary_df <- data.table::rbindlist(object_summary_list, fill = TRUE)
coverage_df <- data.table::rbindlist(coverage_list, fill = TRUE)

atomic_write_csv(object_summary_df, object_gene_summary_csv)
atomic_write_csv(coverage_df, object_marker_coverage_csv)

coverage_dt <- data.table::as.data.table(coverage_df)

dataset_coverage <- coverage_dt[
  ,
  .(
    n_objects = .N,
    median_coverage = median(coverage_fraction, na.rm = TRUE),
    min_coverage = min(coverage_fraction, na.rm = TRUE),
    max_coverage = max(coverage_fraction, na.rm = TRUE),
    objects_with_coverage_ge_50pct = sum(coverage_fraction >= 0.5, na.rm = TRUE),
    objects_with_coverage_ge_80pct = sum(coverage_fraction >= 0.8, na.rm = TRUE)
  ),
  by = .(dataset, category)
]

atomic_write_csv(as.data.frame(dataset_coverage), dataset_marker_coverage_csv)

recommended_categories <- c(
  "DA_core_identity",
  "A9_like_DA_identity",
  "A10_like_DA_identity",
  "midbrain_floor_plate_progenitor",
  "neuronal_maturation_synapse",
  "progenitor_neuroepithelial",
  "cell_cycle_proliferation",
  "pluripotency_immature_risk",
  "astrocyte_glial",
  "oligodendrocyte_OPC",
  "GABAergic_neuron",
  "glutamatergic_neuron",
  "serotonergic_neuron",
  "vascular_pericyte_meningeal",
  "stress_apoptosis_response"
)

recommended_df <- marker_panel[
  marker_panel$category %in% recommended_categories,
  c("category", "gene_symbol", "interpretation", "use_for"),
  drop = FALSE
]

atomic_write_csv(recommended_df, recommended_marker_csv)

n_objects <- nrow(object_summary_df)
n_success <- sum(object_summary_df$status == "SUCCESS", na.rm = TRUE)
n_failed <- sum(object_summary_df$status != "SUCCESS", na.rm = TRUE)
n_marker_categories <- length(unique(marker_panel$category))
n_markers <- length(unique(marker_panel$gene_symbol))

core_cov <- dataset_coverage[
  dataset %in% c("GSE178265_DA_01B", "GSE132758", "GSE204796", "GSE233885") &
    category %in% c("DA_core_identity", "A9_like_DA_identity", "cell_cycle_proliferation", "pluripotency_immature_risk"),
]

core_cov_lines <- if (nrow(core_cov) > 0) {
  apply(
    as.data.frame(core_cov),
    1,
    function(x) paste0(
      x[["dataset"]],
      " / ",
      x[["category"]],
      ": median coverage=",
      round(as.numeric(x[["median_coverage"]]) * 100, 1),
      "%"
    )
  )
} else {
  character()
}

report_lines <- c(
  "04A marker panel and annotation prep report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Objects checked: ", n_objects),
  paste0("Successfully read objects: ", n_success),
  paste0("Failed objects: ", n_failed),
  paste0("Marker categories: ", n_marker_categories),
  paste0("Unique marker genes: ", n_markers),
  paste0("Gene universe source: ", GENE_UNIVERSE_SOURCE),
  "",
  "Core marker coverage snapshot:",
  core_cov_lines,
  "",
  "Output files:",
  paste0("Marker panel master: ", marker_master_csv),
  paste0("Marker alias long table: ", marker_alias_csv),
  paste0("Object gene universe summary: ", object_gene_summary_csv),
  paste0("Object marker coverage: ", object_marker_coverage_csv),
  paste0("Dataset marker coverage: ", dataset_marker_coverage_csv),
  paste0("Recommended marker sets for 04B: ", recommended_marker_csv),
  "",
  "Next step:",
  "04B_MARKER_EXPRESSION_AND_PRELIMINARY_ANNOTATION.R",
  "",
  "Journal-rigor note:",
  "Marker-based annotation must be supported by multiple markers per cell state. DA-like, A9-like, and projection-associated terms describe transcriptomic identity/competence, not direct functional proof."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("04A marker panel and annotation prep 运行结束\n")
cat("============================================================\n\n")

cat("检查对象数量：", n_objects, "\n")
cat("成功读取对象：", n_success, "\n")
cat("失败对象：", n_failed, "\n")
cat("marker categories：", n_marker_categories, "\n")
cat("unique marker genes：", n_markers, "\n\n")

cat("输出文件：\n")
cat(marker_master_csv, "\n")
cat(marker_alias_csv, "\n")
cat(object_gene_summary_csv, "\n")
cat(object_marker_coverage_csv, "\n")
cat(dataset_marker_coverage_csv, "\n")
cat(recommended_marker_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 04A marker panel and annotation prep 完成。\n")
  cat("下一步进入 04B：marker expression 和 preliminary annotation。\n")
} else {
  cat("⚠️ 04A 完成，但有对象读取失败。请查看 object gene universe summary。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

EXPRESSION_OBJECT_COL <- "final_expression_object"
GROUP_OBJECT_COL <- "initial_pca_object"

WRITE_GENE_LEVEL_TABLE <- TRUE
ALLOW_OBJECT_LEVEL_IF_NO_CLUSTER <- TRUE

MIN_CATEGORY_COVERAGE_FOR_SUGGESTION <- 0.4
MIN_MEAN_SCORE_FOR_ACTIVE_SIGNAL <- 0.05

set.seed(20260714)

cat("\n============================================================\n")
cat("04B V4：direct-matrix marker expression list-fix\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table", "Matrix")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04B V4。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(data.table)
  library(Matrix)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_main_manifest <- file.path(tables_dir, "03C_strategy", "03C_main_analysis_object_manifest.csv")
input_marker_panel <- file.path(tables_dir, "04A_annotation_prep", "04A_marker_panel_master.csv")

out_tables_dir <- file.path(tables_dir, "04B_marker_expression")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

object_category_scores_csv <- file.path(out_tables_dir, "04B_object_marker_category_scores.csv")
group_category_scores_csv <- file.path(out_tables_dir, "04B_group_marker_category_scores.csv")
group_gene_expression_csv <- file.path(out_tables_dir, "04B_group_marker_gene_expression.csv")
preliminary_annotation_csv <- file.path(out_tables_dir, "04B_preliminary_annotation_suggestions.csv")
failed_objects_csv <- file.path(out_tables_dir, "04B_failed_objects.csv")
matrix_source_csv <- file.path(out_tables_dir, "04B_matrix_source_audit.csv")
report_txt <- file.path(reports_dir, "04B_marker_expression_and_preliminary_annotation_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

get_assay_for_analysis <- function(obj) {
  assays <- names(obj@assays)
  if ("RNA" %in% assays) return("RNA")

  da <- tryCatch(DefaultAssay(obj), error = function(e) NA_character_)
  if (!is.na(da) && da %in% assays) return(da)

  if (length(assays) > 0L) return(assays[[1L]])
  NA_character_
}

match_genes_case_insensitive <- function(query_genes, object_genes) {
  query_genes <- unique(as.character(query_genes))
  object_genes <- unique(as.character(object_genes))

  q_upper <- toupper(query_genes)
  g_upper <- toupper(object_genes)

  idx <- match(q_upper, g_upper)

  data.frame(
    query_gene = query_genes,
    query_upper = q_upper,
    matched_gene = ifelse(is.na(idx), NA_character_, object_genes[idx]),
    present = !is.na(idx),
    stringsAsFactors = FALSE
  )
}

get_assay5_layer_names <- function(assay_obj) {
  out <- tryCatch({
    names(slot(assay_obj, "layers"))
  }, error = function(e) {
    character()
  })

  out[!is.na(out) & nzchar(out)]
}

get_logmap_names <- function(logmap_obj, layer_name) {
  out <- tryCatch({
    if (layer_name %in% colnames(logmap_obj)) {
      rownames(logmap_obj)[as.logical(logmap_obj[, layer_name])]
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })

  out
}

repair_layer_dimnames <- function(mat, obj, assay_obj, layer_name) {
  if (is.null(mat)) return(NULL)

  if (!inherits(mat, "dgCMatrix")) {
    mat <- tryCatch(as(mat, "dgCMatrix"), error = function(e) NULL)
  }

  if (is.null(mat)) return(NULL)
  if (nrow(mat) == 0L || ncol(mat) == 0L) return(NULL)

  obj_features <- tryCatch(rownames(obj), error = function(e) NULL)
  obj_cells <- tryCatch(colnames(obj), error = function(e) NULL)

  fmap <- tryCatch(slot(assay_obj, "features"), error = function(e) NULL)
  cmap <- tryCatch(slot(assay_obj, "cells"), error = function(e) NULL)

  layer_features <- if (!is.null(fmap)) get_logmap_names(fmap, layer_name) else NULL
  layer_cells <- if (!is.null(cmap)) get_logmap_names(cmap, layer_name) else NULL

  candidate_features <- list(layer_features, obj_features)
  candidate_cells <- list(layer_cells, obj_cells)

  feature_lengths <- unique(na.omit(vapply(candidate_features, function(x) if (is.null(x)) NA_integer_ else length(x), integer(1))))
  cell_lengths <- unique(na.omit(vapply(candidate_cells, function(x) if (is.null(x)) NA_integer_ else length(x), integer(1))))

  if (length(feature_lengths) > 0L && length(cell_lengths) > 0L) {
    if (nrow(mat) %in% cell_lengths && ncol(mat) %in% feature_lengths &&
        !(nrow(mat) %in% feature_lengths && ncol(mat) %in% cell_lengths)) {
      mat <- Matrix::t(mat)
    }
  }

  if (is.null(rownames(mat)) || length(rownames(mat)) != nrow(mat) ||
      any(is.na(rownames(mat))) || any(rownames(mat) == "")) {

    rn <- NULL

    if (!is.null(layer_features) && length(layer_features) == nrow(mat)) {
      rn <- layer_features
    } else if (!is.null(obj_features) && length(obj_features) == nrow(mat)) {
      rn <- obj_features
    }

    if (!is.null(rn)) {
      rownames(mat) <- make.unique(as.character(rn), sep = "__dupGene")
    }
  }

  if (is.null(colnames(mat)) || length(colnames(mat)) != ncol(mat) ||
      any(is.na(colnames(mat))) || any(colnames(mat) == "")) {

    cn <- NULL

    if (!is.null(layer_cells) && length(layer_cells) == ncol(mat)) {
      cn <- layer_cells
    } else if (!is.null(obj_cells) && length(obj_cells) == ncol(mat)) {
      cn <- obj_cells
    }

    if (!is.null(cn)) {
      colnames(mat) <- make.unique(as.character(cn), sep = "__dupCell")
    }
  }

  if (is.null(rownames(mat)) || is.null(colnames(mat))) return(NULL)
  if (length(rownames(mat)) != nrow(mat) || length(colnames(mat)) != ncol(mat)) return(NULL)

  mat
}

extract_expression_matrix_direct <- function(obj) {
  assay <- get_assay_for_analysis(obj)

  if (is.na(assay) || !assay %in% names(obj@assays)) {
    stop("No valid assay found.")
  }

  assay_obj <- obj[[assay]]

  layer_names <- get_assay5_layer_names(assay_obj)

  if (length(layer_names) > 0L) {
    candidate_layers <- unique(c(
      "counts",
      grep("^counts", layer_names, value = TRUE),
      "data",
      grep("^data", layer_names, value = TRUE)
    ))

    candidate_layers <- candidate_layers[candidate_layers %in% layer_names]

    for (lyr in candidate_layers) {
      raw_mat <- tryCatch({
        slot(assay_obj, "layers")[[lyr]]
      }, error = function(e) NULL)

      mat <- repair_layer_dimnames(raw_mat, obj, assay_obj, lyr)

      if (!is.null(mat)) {
        return(list(
          matrix = mat,
          assay = assay,
          layer = lyr,
          layer_type = ifelse(grepl("^counts", lyr), "counts", "data"),
          method = "direct_Assay5_layers"
        ))
      }
    }
  }

  for (lyr in c("counts", "data")) {
    mat <- tryCatch({
      SeuratObject::LayerData(obj, assay = assay, layer = lyr, fast = FALSE)
    }, error = function(e) NULL)

    mat <- repair_layer_dimnames(mat, obj, assay_obj, lyr)

    if (!is.null(mat)) {
      return(list(
        matrix = mat,
        assay = assay,
        layer = lyr,
        layer_type = ifelse(lyr == "counts", "counts", "data"),
        method = "LayerData"
      ))
    }
  }

  for (sl in c("counts", "data")) {
    mat <- tryCatch({
      slot(assay_obj, sl)
    }, error = function(e) NULL)

    mat <- repair_layer_dimnames(mat, obj, assay_obj, sl)

    if (!is.null(mat)) {
      return(list(
        matrix = mat,
        assay = assay,
        layer = sl,
        layer_type = ifelse(sl == "counts", "counts", "data"),
        method = "Assay_slot"
      ))
    }
  }

  stop(
    "Cannot extract expression matrix. assay=",
    assay,
    "; layers=",
    paste(layer_names, collapse = ",")
  )
}

marker_expression_from_matrix <- function(expr_info, marker_genes) {
  mat <- expr_info$matrix

  object_genes <- rownames(mat)
  gene_match <- match_genes_case_insensitive(marker_genes, object_genes)

  present <- unique(gene_match$matched_gene[gene_match$present])
  present <- present[!is.na(present)]

  if (length(present) == 0L) return(NULL)

  mat_sub <- mat[present, , drop = FALSE]

  if (expr_info$layer_type == "counts") {
    lib <- Matrix::colSums(mat)
    lib[is.na(lib) | lib <= 0] <- 1
    mat_sub <- t(t(mat_sub) / lib * 10000)
    mat_sub <- log1p(mat_sub)
  }

  df <- as.data.frame(t(as.matrix(mat_sub)))
  df <- df[, present, drop = FALSE]

  df
}

choose_group_column <- function(obj) {
  md <- obj@meta.data
  cn <- colnames(md)

  preferred <- c("seurat_clusters", "RNA_snn_res.0.5", "SCT_snn_res.0.5", "integrated_snn_res.0.5")

  hit <- preferred[preferred %in% cn]
  if (length(hit) > 0L) return(hit[[1L]])

  hit2 <- cn[grepl("cluster|snn_res|louvain|leiden", cn, ignore.case = TRUE)]
  if (length(hit2) > 0L) return(hit2[[1L]])

  NA_character_
}

get_group_vector <- function(expr_obj, expr_cells, group_path) {
  if (!is.na(group_path) && file.exists(group_path)) {
    gobj <- tryCatch(readRDS(group_path), error = function(e) NULL)

    if (!is.null(gobj)) {
      group_col <- choose_group_column(gobj)

      if (!is.na(group_col) && group_col %in% colnames(gobj@meta.data)) {
        md <- gobj@meta.data
        common <- intersect(expr_cells, rownames(md))

        if (length(common) >= max(10L, floor(0.5 * length(expr_cells)))) {
          groups <- rep(NA_character_, length(expr_cells))
          names(groups) <- expr_cells
          groups[common] <- as.character(md[common, group_col])
          groups[is.na(groups)] <- "unmatched_cell"
          return(list(groups = groups, group_source = paste0("03A_", group_col)))
        }
      }
    }
  }

  group_col2 <- choose_group_column(expr_obj)

  if (!is.na(group_col2) && group_col2 %in% colnames(expr_obj@meta.data)) {
    md2 <- expr_obj@meta.data
    common2 <- intersect(expr_cells, rownames(md2))

    if (length(common2) > 0L) {
      groups <- rep(NA_character_, length(expr_cells))
      names(groups) <- expr_cells
      groups[common2] <- as.character(md2[common2, group_col2])
      groups[is.na(groups)] <- "unmatched_cell"
      return(list(groups = groups, group_source = paste0("expression_", group_col2)))
    }
  }

  if (ALLOW_OBJECT_LEVEL_IF_NO_CLUSTER) {
    groups <- rep("object_all", length(expr_cells))
    names(groups) <- expr_cells
    return(list(groups = groups, group_source = "object_all_no_cluster"))
  }

  stop("No grouping information found.")
}

suggest_label_from_scores <- function(score_dt) {
  if (nrow(score_dt) == 0L) {
    return(data.frame(
      preliminary_suggestion = "unassigned",
      supporting_categories = NA_character_,
      caution = "No score data.",
      stringsAsFactors = FALSE
    ))
  }

  score_dt <- score_dt[coverage_fraction >= MIN_CATEGORY_COVERAGE_FOR_SUGGESTION]
  score_dt <- score_dt[mean_score >= MIN_MEAN_SCORE_FOR_ACTIVE_SIGNAL]

  if (nrow(score_dt) == 0L) {
    return(data.frame(
      preliminary_suggestion = "unassigned_low_marker_signal",
      supporting_categories = NA_character_,
      caution = "No category passed coverage/signal threshold.",
      stringsAsFactors = FALSE
    ))
  }

  score_dt <- score_dt[order(-mean_score)]
  top_cats <- score_dt$category[seq_len(min(5L, nrow(score_dt)))]

  has <- function(cat) cat %in% score_dt$category

  suggestion <- "mixed_or_unassigned_marker_signal"

  if (has("DA_core_identity") && has("neuronal_maturation_synapse")) {
    suggestion <- "DA_like_neuronal_candidate"
  } else if (has("midbrain_floor_plate_progenitor") && has("progenitor_neuroepithelial")) {
    suggestion <- "midbrain_progenitor_like_candidate"
  } else if (has("cell_cycle_proliferation") && has("progenitor_neuroepithelial")) {
    suggestion <- "cycling_progenitor_safety_risk_candidate"
  } else if (has("pluripotency_immature_risk")) {
    suggestion <- "immature_pluripotency_risk_signal_candidate"
  } else if (has("astrocyte_glial")) {
    suggestion <- "astrocyte_glial_candidate"
  } else if (has("oligodendrocyte_OPC")) {
    suggestion <- "oligodendrocyte_OPC_candidate"
  } else if (has("microglia_macrophage_immune")) {
    suggestion <- "immune_microglia_macrophage_candidate"
  } else if (has("vascular_pericyte_meningeal") || has("extracellular_matrix_fibroblast")) {
    suggestion <- "vascular_mesenchymal_candidate"
  } else if (has("GABAergic_neuron")) {
    suggestion <- "GABAergic_neuronal_candidate"
  } else if (has("glutamatergic_neuron")) {
    suggestion <- "glutamatergic_neuronal_candidate"
  } else if (has("serotonergic_neuron")) {
    suggestion <- "serotonergic_neuronal_candidate"
  } else if (has("cholinergic_neuron")) {
    suggestion <- "cholinergic_neuronal_candidate"
  } else if (has("stress_apoptosis_response")) {
    suggestion <- "stress_response_high_candidate"
  }

  data.frame(
    preliminary_suggestion = suggestion,
    supporting_categories = paste(top_cats, collapse = ";"),
    caution = "Preliminary marker-based suggestion only; requires multi-marker validation and dataset-context review.",
    stringsAsFactors = FALSE
  )
}

write_outputs <- function(
  object_category_list,
  group_category_list,
  group_gene_list,
  prelim_list,
  failed_list,
  matrix_source_list
) {
  object_category_df <- if (length(object_category_list) > 0L) data.table::rbindlist(object_category_list, fill = TRUE) else data.frame()
  group_category_df <- if (length(group_category_list) > 0L) data.table::rbindlist(group_category_list, fill = TRUE) else data.frame()
  group_gene_df <- if (length(group_gene_list) > 0L) data.table::rbindlist(group_gene_list, fill = TRUE) else data.frame()
  prelim_df <- if (length(prelim_list) > 0L) data.table::rbindlist(prelim_list, fill = TRUE) else data.frame()
  failed_df <- if (length(failed_list) > 0L) data.table::rbindlist(failed_list, fill = TRUE) else data.frame()
  matrix_source_df <- if (length(matrix_source_list) > 0L) data.table::rbindlist(matrix_source_list, fill = TRUE) else data.frame()

  atomic_write_csv(object_category_df, object_category_scores_csv)
  atomic_write_csv(group_category_df, group_category_scores_csv)
  atomic_write_csv(group_gene_df, group_gene_expression_csv)
  atomic_write_csv(prelim_df, preliminary_annotation_csv)
  atomic_write_csv(failed_df, failed_objects_csv)
  atomic_write_csv(matrix_source_df, matrix_source_csv)

  invisible(list(
    object_category_df = object_category_df,
    group_category_df = group_category_df,
    group_gene_df = group_gene_df,
    prelim_df = prelim_df,
    failed_df = failed_df,
    matrix_source_df = matrix_source_df
  ))
}

process_one_object <- function(row, marker_panel, all_marker_genes, all_categories) {
  ds <- row$dataset
  oid <- row$object_id
  expr_path <- row[[EXPRESSION_OBJECT_COL]]
  group_path <- row[[GROUP_OBJECT_COL]]

  obj <- readRDS(expr_path)

  expr_info <- extract_expression_matrix_direct(obj)
  expr_mat <- expr_info$matrix

  stamp(
    "  matrix：",
    nrow(expr_mat), " genes x ", ncol(expr_mat), " cells；",
    "method=", expr_info$method, "；layer=", expr_info$layer
  )

  expr_df <- marker_expression_from_matrix(expr_info, all_marker_genes)

  if (is.null(expr_df) || ncol(expr_df) == 0L) {
    stop("No marker expression could be extracted from matrix.")
  }

  group_info <- get_group_vector(obj, rownames(expr_df), group_path)
  groups <- as.character(group_info$groups)
  group_source <- group_info$group_source

  matrix_source_df <- data.frame(
    dataset = ds,
    object_id = oid,
    expression_object = expr_path,
    group_object = group_path,
    assay = expr_info$assay,
    layer = expr_info$layer,
    layer_type = expr_info$layer_type,
    extraction_method = expr_info$method,
    n_matrix_genes = nrow(expr_mat),
    n_matrix_cells = ncol(expr_mat),
    n_marker_genes_extracted = ncol(expr_df),
    group_source = group_source,
    stringsAsFactors = FALSE
  )

  object_category_list_one <- list()
  group_category_list_one <- list()
  group_gene_list_one <- list()
  prelim_list_one <- list()

  category_score_dt_list <- list()

  for (catg in all_categories) {
    panel_genes <- unique(marker_panel$gene_symbol[marker_panel$category == catg])
    matched <- match_genes_case_insensitive(panel_genes, colnames(expr_df))

    cat_genes <- unique(matched$matched_gene[matched$present])
    cat_genes <- cat_genes[!is.na(cat_genes)]

    coverage_fraction <- length(cat_genes) / length(unique(panel_genes))

    if (length(cat_genes) == 0L) next

    mat_cat <- as.matrix(expr_df[, cat_genes, drop = FALSE])
    score <- rowMeans(mat_cat, na.rm = TRUE)

    tmp_dt <- data.table(
      cell = rownames(expr_df),
      group_id = groups,
      category = catg,
      marker_score = score
    )

    group_score <- tmp_dt[
      ,
      .(
        n_cells = .N,
        mean_score = mean(marker_score, na.rm = TRUE),
        median_score = median(marker_score, na.rm = TRUE),
        pct_cells_score_gt0 = mean(marker_score > 0, na.rm = TRUE)
      ),
      by = .(group_id, category)
    ]

    group_score[, dataset := ds]
    group_score[, object_id := oid]
    group_score[, group_source := group_source]
    group_score[, n_panel_genes := length(unique(panel_genes))]
    group_score[, n_present_genes := length(cat_genes)]
    group_score[, coverage_fraction := coverage_fraction]
    group_score[, present_genes := paste(cat_genes, collapse = ";")]

    group_category_list_one[[length(group_category_list_one) + 1L]] <- as.data.frame(group_score)
    category_score_dt_list[[length(category_score_dt_list) + 1L]] <- group_score

    object_category_list_one[[length(object_category_list_one) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      category = catg,
      n_cells = nrow(expr_df),
      n_panel_genes = length(unique(panel_genes)),
      n_present_genes = length(cat_genes),
      coverage_fraction = coverage_fraction,
      mean_score = mean(score, na.rm = TRUE),
      median_score = median(score, na.rm = TRUE),
      pct_cells_score_gt0 = mean(score > 0, na.rm = TRUE),
      present_genes = paste(cat_genes, collapse = ";"),
      stringsAsFactors = FALSE
    )

    if (WRITE_GENE_LEVEL_TABLE) {
      for (g in cat_genes) {
        gene_dt <- data.table(
          group_id = groups,
          expr = as.numeric(expr_df[[g]])
        )

        gene_sum <- gene_dt[
          ,
          .(
            n_cells = .N,
            mean_expression = mean(expr, na.rm = TRUE),
            median_expression = median(expr, na.rm = TRUE),
            pct_expressing = mean(expr > 0, na.rm = TRUE)
          ),
          by = group_id
        ]

        gene_sum[, dataset := ds]
        gene_sum[, object_id := oid]
        gene_sum[, group_source := group_source]
        gene_sum[, category := catg]
        gene_sum[, gene_symbol_matched := g]

        group_gene_list_one[[length(group_gene_list_one) + 1L]] <- as.data.frame(gene_sum)
      }
    }
  }

  if (length(category_score_dt_list) > 0L) {
    all_scores_obj <- data.table::rbindlist(category_score_dt_list, fill = TRUE)

    for (gid in unique(all_scores_obj$group_id)) {
      score_sub <- all_scores_obj[group_id == gid]
      sug <- suggest_label_from_scores(score_sub)
      top_score <- score_sub[order(-mean_score)][1]

      prelim_list_one[[length(prelim_list_one) + 1L]] <- data.frame(
        dataset = ds,
        object_id = oid,
        group_source = group_source,
        group_id = gid,
        n_cells_group = unique(score_sub$n_cells)[1],
        preliminary_suggestion = sug$preliminary_suggestion,
        supporting_categories = sug$supporting_categories,
        top_category = top_score$category,
        top_category_mean_score = top_score$mean_score,
        top_category_coverage_fraction = top_score$coverage_fraction,
        caution = sug$caution,
        stringsAsFactors = FALSE
      )
    }
  } else {
    stop("No category scores generated.")
  }

  rm(obj, expr_info, expr_mat, expr_df)
  gc(verbose = FALSE)

  list(
    object_category = object_category_list_one,
    group_category = group_category_list_one,
    group_gene = group_gene_list_one,
    prelim = prelim_list_one,
    matrix_source = list(matrix_source_df)
  )
}

stamp("读取 main manifest 和 marker panel。")

main_manifest <- read_csv_required(input_main_manifest)
marker_panel <- read_csv_required(input_marker_panel)

if (!EXPRESSION_OBJECT_COL %in% colnames(main_manifest)) {
  stop("main manifest 缺少表达对象路径列：", EXPRESSION_OBJECT_COL)
}

if (!GROUP_OBJECT_COL %in% colnames(main_manifest)) {
  main_manifest[[GROUP_OBJECT_COL]] <- NA_character_
}

if (!all(c("category", "gene_symbol") %in% colnames(marker_panel))) {
  stop("marker panel 缺少 category/gene_symbol。")
}

main_manifest <- main_manifest[file.exists(main_manifest[[EXPRESSION_OBJECT_COL]]), , drop = FALSE]

marker_panel$gene_symbol <- as.character(marker_panel$gene_symbol)
marker_panel$category <- as.character(marker_panel$category)

all_marker_genes <- unique(marker_panel$gene_symbol)
all_categories <- unique(marker_panel$category)

stamp("准备处理对象数量：", nrow(main_manifest))
stamp("marker categories：", length(all_categories), "；marker genes：", length(all_marker_genes))
stamp("V4 使用直接 matrix extraction，并修复 list 作用域。")

object_category_list <- list()
group_category_list <- list()
group_gene_list <- list()
prelim_list <- list()
failed_list <- list()
matrix_source_list <- list()

for (i in seq_len(nrow(main_manifest))) {
  ds <- main_manifest$dataset[[i]]
  oid <- main_manifest$object_id[[i]]

  stamp("04B V4 处理对象 ", i, " / ", nrow(main_manifest), "：", ds, " :: ", oid)

  row <- main_manifest[i, , drop = FALSE]

  result <- tryCatch({
    process_one_object(
      row = row,
      marker_panel = marker_panel,
      all_marker_genes = all_marker_genes,
      all_categories = all_categories
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    stamp("  对象失败但不中断：", msg)

    failed_list[[length(failed_list) + 1L]] <<- data.frame(
      dataset = ds,
      object_id = oid,
      expression_object = row[[EXPRESSION_OBJECT_COL]],
      group_object = row[[GROUP_OBJECT_COL]],
      stage = "object_processing",
      message = msg,
      stringsAsFactors = FALSE
    )

    NULL
  })

  if (!is.null(result)) {
    object_category_list <- c(object_category_list, result$object_category)
    group_category_list <- c(group_category_list, result$group_category)
    group_gene_list <- c(group_gene_list, result$group_gene)
    prelim_list <- c(prelim_list, result$prelim)
    matrix_source_list <- c(matrix_source_list, result$matrix_source)
  }

  write_outputs(
    object_category_list,
    group_category_list,
    group_gene_list,
    prelim_list,
    failed_list,
    matrix_source_list
  )
}

final_outputs <- write_outputs(
  object_category_list,
  group_category_list,
  group_gene_list,
  prelim_list,
  failed_list,
  matrix_source_list
)

object_category_df <- final_outputs$object_category_df
group_category_df <- final_outputs$group_category_df
group_gene_df <- final_outputs$group_gene_df
prelim_df <- final_outputs$prelim_df
failed_df <- final_outputs$failed_df
matrix_source_df <- final_outputs$matrix_source_df

n_objects_total <- nrow(main_manifest)

n_objects_success <- if (nrow(object_category_df) > 0L) {
  length(unique(paste(object_category_df$dataset, object_category_df$object_id, sep = "||")))
} else {
  0L
}

n_objects_failed <- if (nrow(failed_df) > 0L) {
  length(unique(paste(failed_df$dataset, failed_df$object_id, sep = "||")))
} else {
  0L
}

n_group_scores <- nrow(group_category_df)
n_gene_records <- nrow(group_gene_df)
n_prelim <- nrow(prelim_df)

suggestion_counts <- if (nrow(prelim_df) > 0L) {
  as.data.table(prelim_df)[, .N, by = preliminary_suggestion][order(-N)]
} else {
  data.table(preliminary_suggestion = character(), N = integer())
}

suggestion_lines <- if (nrow(suggestion_counts) > 0L) {
  paste0(suggestion_counts$preliminary_suggestion, ": ", suggestion_counts$N)
} else {
  character()
}

matrix_methods <- if (nrow(matrix_source_df) > 0L) {
  as.data.table(matrix_source_df)[, .N, by = .(extraction_method, layer_type, layer)][order(-N)]
} else {
  data.table()
}

matrix_method_lines <- if (nrow(matrix_methods) > 0L) {
  apply(as.data.frame(matrix_methods), 1, function(x) {
    paste0(x[["extraction_method"]], " / ", x[["layer_type"]], " / ", x[["layer"]], ": ", x[["N"]])
  })
} else {
  character()
}

report_lines <- c(
  "04B V4 direct-matrix marker expression and preliminary annotation report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Objects in manifest: ", n_objects_total),
  paste0("Objects with marker scores: ", n_objects_success),
  paste0("Objects failed: ", n_objects_failed),
  paste0("Group-category score rows: ", n_group_scores),
  paste0("Group-marker gene expression rows: ", n_gene_records),
  paste0("Preliminary suggestion rows: ", n_prelim),
  "",
  "Matrix extraction methods:",
  matrix_method_lines,
  "",
  "Preliminary suggestion counts:",
  suggestion_lines,
  "",
  "Output files:",
  paste0("Object category scores: ", object_category_scores_csv),
  paste0("Group category scores: ", group_category_scores_csv),
  paste0("Group marker gene expression: ", group_gene_expression_csv),
  paste0("Preliminary annotation suggestions: ", preliminary_annotation_csv),
  paste0("Matrix source audit: ", matrix_source_csv),
  paste0("Failed objects: ", failed_objects_csv),
  "",
  "Next step:",
  "04C_REVIEW_MARKER_EXPRESSION_AND_DEFINE_ANNOTATION_RULES.R",
  "",
  "Journal-rigor note:",
  "04B V4 directly extracts matrices from full filtered objects and uses log-normalized counts where counts layers are used. Outputs are preliminary marker-supported suggestions, not final annotation labels."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("04B V4 direct-matrix marker expression 运行结束\n")
cat("============================================================\n\n")

cat("manifest 对象数量：", n_objects_total, "\n")
cat("成功生成 marker scores 的对象：", n_objects_success, "\n")
cat("失败对象：", n_objects_failed, "\n")
cat("group-category score rows：", n_group_scores, "\n")
cat("group-marker gene expression rows：", n_gene_records, "\n")
cat("preliminary suggestion rows：", n_prelim, "\n\n")

cat("输出文件：\n")
cat(object_category_scores_csv, "\n")
cat(group_category_scores_csv, "\n")
cat(group_gene_expression_csv, "\n")
cat(preliminary_annotation_csv, "\n")
cat(matrix_source_csv, "\n")
cat(failed_objects_csv, "\n")
cat(report_txt, "\n\n")

if (n_objects_failed == 0L) {
  cat("✅ 04B V4 direct-matrix marker expression 完成。\n")
  cat("下一步进入 04C：review marker expression and define annotation rules。\n")
} else {
  cat("⚠️ 04B V4 完成，但有对象失败。请查看 04B_failed_objects.csv。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

MIN_COVERAGE <- 0.4
MIN_MEAN_SCORE <- 0.05
MIN_PCT_CELLS_SCORE_GT0 <- 0.05

MAX_REVIEW_GROUPS_PER_CATEGORY <- 200

cat("\n============================================================\n")
cat("04C：review marker expression and define annotation rules\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04C。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_group_category <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_category_scores.csv")
input_group_gene <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_gene_expression.csv")
input_prelim <- file.path(tables_dir, "04B_marker_expression", "04B_preliminary_annotation_suggestions.csv")
input_object_category <- file.path(tables_dir, "04B_marker_expression", "04B_object_marker_category_scores.csv")
input_failed <- file.path(tables_dir, "04B_marker_expression", "04B_failed_objects.csv")
input_matrix_audit <- file.path(tables_dir, "04B_marker_expression", "04B_matrix_source_audit.csv")
input_marker_panel <- file.path(tables_dir, "04A_annotation_prep", "04A_marker_panel_master.csv")
input_dataset_role <- file.path(tables_dir, "03C_strategy", "03C_dataset_role_and_usage.csv")

out_tables_dir <- file.path(tables_dir, "04C_annotation_review")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

annotation_rules_csv <- file.path(out_tables_dir, "04C_annotation_rule_table.csv")
suggestion_summary_csv <- file.path(out_tables_dir, "04C_preliminary_suggestion_summary_by_dataset.csv")
dataset_category_signal_csv <- file.path(out_tables_dir, "04C_dataset_marker_category_signal_summary.csv")
candidate_groups_csv <- file.path(out_tables_dir, "04C_candidate_groups_for_manual_review.csv")
da_a9_a10_csv <- file.path(out_tables_dir, "04C_DA_A9_A10_candidate_groups.csv")
safety_risk_csv <- file.path(out_tables_dir, "04C_safety_risk_candidate_groups.csv")
marker_gene_snapshot_csv <- file.path(out_tables_dir, "04C_key_marker_gene_snapshot.csv")
qc_audit_csv <- file.path(out_tables_dir, "04C_04B_QC_audit_summary.csv")
report_txt <- file.path(reports_dir, "04C_marker_review_and_annotation_rules_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

category_signal_flag <- function(mean_score, coverage_fraction, pct_cells_score_gt0) {
  !is.na(mean_score) &
    !is.na(coverage_fraction) &
    !is.na(pct_cells_score_gt0) &
    coverage_fraction >= MIN_COVERAGE &
    mean_score >= MIN_MEAN_SCORE &
    pct_cells_score_gt0 >= MIN_PCT_CELLS_SCORE_GT0
}

wide_category_scores <- function(dt) {

  base_cols <- c("dataset", "object_id", "group_source", "group_id", "n_cells")

  mean_wide <- dcast(
    dt,
    dataset + object_id + group_source + group_id + n_cells ~ category,
    value.var = "mean_score",
    fun.aggregate = max,
    fill = NA_real_
  )

  mean_wide
}

get_cat <- function(row, cat) {
  if (!cat %in% names(row)) return(NA_real_)
  as.numeric(row[[cat]])
}

stamp("读取 04B 输出。")

group_category <- read_csv_required(input_group_category)
group_gene <- read_csv_required(input_group_gene)
prelim <- read_csv_required(input_prelim)
object_category <- read_csv_required(input_object_category)
failed <- read_csv_optional(input_failed)
matrix_audit <- read_csv_optional(input_matrix_audit)
marker_panel <- read_csv_required(input_marker_panel)
dataset_role <- read_csv_optional(input_dataset_role)

gc_dt <- as.data.table(group_category)
gg_dt <- as.data.table(group_gene)
prelim_dt <- as.data.table(prelim)
oc_dt <- as.data.table(object_category)
failed_dt <- as.data.table(failed)
matrix_dt <- as.data.table(matrix_audit)

needed_gc <- c("dataset", "object_id", "group_id", "category", "mean_score", "coverage_fraction", "pct_cells_score_gt0")
if (!all(needed_gc %in% colnames(gc_dt))) {
  stop("04B_group_marker_category_scores 缺少必要列：", paste(setdiff(needed_gc, colnames(gc_dt)), collapse = ", "))
}

needed_prelim <- c("dataset", "object_id", "group_id", "preliminary_suggestion")
if (!all(needed_prelim %in% colnames(prelim_dt))) {
  stop("04B_preliminary_annotation_suggestions 缺少必要列。")
}

stamp("生成 annotation rule table。")

annotation_rules <- data.frame(
  proposed_label = c(
    "DA_like_neuronal_candidate",
    "A9_like_DA_supported_candidate",
    "A10_like_DA_supported_candidate",
    "midbrain_progenitor_like_candidate",
    "cycling_progenitor_safety_risk_candidate",
    "immature_pluripotency_risk_signal_candidate",
    "astrocyte_glial_candidate",
    "oligodendrocyte_OPC_candidate",
    "immune_microglia_macrophage_candidate",
    "vascular_mesenchymal_candidate",
    "GABAergic_neuronal_candidate",
    "glutamatergic_neuronal_candidate",
    "stress_response_high_candidate",
    "unassigned_low_marker_signal"
  ),
  required_positive_categories = c(
    "DA_core_identity;neuronal_maturation_synapse",
    "DA_core_identity;A9_like_DA_identity",
    "DA_core_identity;A10_like_DA_identity",
    "midbrain_floor_plate_progenitor;progenitor_neuroepithelial",
    "cell_cycle_proliferation;progenitor_neuroepithelial",
    "pluripotency_immature_risk",
    "astrocyte_glial",
    "oligodendrocyte_OPC",
    "microglia_macrophage_immune",
    "vascular_pericyte_meningeal OR extracellular_matrix_fibroblast",
    "GABAergic_neuron",
    "glutamatergic_neuron",
    "stress_apoptosis_response",
    "none"
  ),
  supporting_marker_examples = c(
    "TH/DDC/SLC6A3/SLC18A2/NR4A2 + RBFOX3/MAP2/SNAP25/SYT1",
    "ALDH1A1/KCNJ6/SOX6/DCLK3/GCH1 with DA core",
    "CALB1/OTX2/CCK/SLC17A6 with DA core",
    "FOXA2/LMX1A/LMX1B/OTX2/CORIN + SOX2/NES/HES1",
    "MKI67/TOP2A/PCNA/MCM2/CENPF + SOX2/NES/PAX6",
    "POU5F1/NANOG/LIN28A/DPPA4/TERT/PROM1",
    "GFAP/AQP4/ALDH1L1/SLC1A3/S100B",
    "OLIG1/OLIG2/PDGFRA/SOX10/MBP/PLP1",
    "PTPRC/AIF1/C1QA/TYROBP/LST1/CD74",
    "PECAM1/VWF/CLDN5/PDGFRB/RGS5/COL1A1/DCN",
    "GAD1/GAD2/SLC32A1/DLX1/DLX2",
    "SLC17A6/SLC17A7/SLC17A8/TBR1/NEUROD6",
    "FOS/JUN/HSPA1A/DDIT3/ATF3/BAX",
    "No robust category signal"
  ),
  caution_for_manuscript = c(
    "DA-like transcriptomic identity only; not proof of graft function.",
    "A9-like molecular support only; not direct substantia nigra functional identity.",
    "A10-like molecular support only; interpret relative to DA core.",
    "Developmental/progenitor state; not automatically unsafe.",
    "Safety-risk-associated transcriptomic state; not direct tumorigenicity proof.",
    "Immature-risk signal; requires checking expression level and dataset context.",
    "Off-target glial-like identity; requires multi-marker support.",
    "Off-target oligodendrocyte/OPC-like identity; requires multi-marker support.",
    "Immune/macrophage-like signal may reflect host cells or contamination depending dataset.",
    "Vascular/mesenchymal signal may reflect host/stromal/meningeal components.",
    "Subtype signal only; requires pan-neuronal support.",
    "Subtype signal only; requires pan-neuronal support.",
    "Stress state, not a cell type.",
    "Do not annotate strongly."
  ),
  use_as_final_label_without_04D_validation = FALSE,
  stringsAsFactors = FALSE
)

atomic_write_csv(annotation_rules, annotation_rules_csv)

stamp("汇总 preliminary suggestions。")

suggestion_summary <- prelim_dt[
  ,
  .(
    n_groups = .N,
    total_cells = sum(n_cells_group, na.rm = TRUE),
    median_group_size = median(n_cells_group, na.rm = TRUE)
  ),
  by = .(dataset, preliminary_suggestion)
][order(dataset, -n_groups)]

atomic_write_csv(as.data.frame(suggestion_summary), suggestion_summary_csv)

stamp("汇总 dataset marker-category signal。")

gc_dt[, signal_positive := category_signal_flag(mean_score, coverage_fraction, pct_cells_score_gt0)]

dataset_category <- gc_dt[
  ,
  .(
    n_groups = .N,
    n_positive_groups = sum(signal_positive, na.rm = TRUE),
    positive_group_fraction = mean(signal_positive, na.rm = TRUE),
    median_mean_score = median(mean_score, na.rm = TRUE),
    max_mean_score = max(mean_score, na.rm = TRUE),
    median_pct_cells_score_gt0 = median(pct_cells_score_gt0, na.rm = TRUE),
    median_coverage = median(coverage_fraction, na.rm = TRUE)
  ),
  by = .(dataset, category)
][order(dataset, category)]

atomic_write_csv(as.data.frame(dataset_category), dataset_category_signal_csv)

stamp("生成候选 group review table。")

score_wide <- wide_category_scores(gc_dt)

prelim_small <- prelim_dt[, .(
  dataset,
  object_id,
  group_source,
  group_id,
  n_cells_group,
  preliminary_suggestion,
  supporting_categories,
  top_category,
  top_category_mean_score,
  caution
)]

candidate <- merge(
  prelim_small,
  score_wide,
  by = c("dataset", "object_id", "group_source", "group_id"),
  all.x = TRUE
)

if (!"DA_core_identity" %in% names(candidate)) candidate[, DA_core_identity := NA_real_]
if (!"neuronal_maturation_synapse" %in% names(candidate)) candidate[, neuronal_maturation_synapse := NA_real_]
if (!"A9_like_DA_identity" %in% names(candidate)) candidate[, A9_like_DA_identity := NA_real_]
if (!"A10_like_DA_identity" %in% names(candidate)) candidate[, A10_like_DA_identity := NA_real_]
if (!"cell_cycle_proliferation" %in% names(candidate)) candidate[, cell_cycle_proliferation := NA_real_]
if (!"pluripotency_immature_risk" %in% names(candidate)) candidate[, pluripotency_immature_risk := NA_real_]
if (!"progenitor_neuroepithelial" %in% names(candidate)) candidate[, progenitor_neuroepithelial := NA_real_]
if (!"midbrain_floor_plate_progenitor" %in% names(candidate)) candidate[, midbrain_floor_plate_progenitor := NA_real_]
if (!"stress_apoptosis_response" %in% names(candidate)) candidate[, stress_apoptosis_response := NA_real_]

candidate[
  ,
  DA_maturation_combined := rowMeans(
    cbind(DA_core_identity, neuronal_maturation_synapse),
    na.rm = TRUE
  )
]

candidate[
  ,
  A9_minus_A10_score := A9_like_DA_identity - A10_like_DA_identity
]

candidate[
  ,
  safety_risk_combined := rowMeans(
    cbind(cell_cycle_proliferation, pluripotency_immature_risk, progenitor_neuroepithelial),
    na.rm = TRUE
  )
]

candidate[
  ,
  progenitor_combined := rowMeans(
    cbind(midbrain_floor_plate_progenitor, progenitor_neuroepithelial),
    na.rm = TRUE
  )
]

candidate[, review_priority := "standard_review"]

candidate[
  preliminary_suggestion %in% c(
    "DA_like_neuronal_candidate",
    "cycling_progenitor_safety_risk_candidate",
    "immature_pluripotency_risk_signal_candidate",
    "midbrain_progenitor_like_candidate"
  ),
  review_priority := "high_priority"
]

candidate[
  preliminary_suggestion %in% c(
    "unassigned_low_marker_signal",
    "mixed_or_unassigned_marker_signal"
  ),
  review_priority := "manual_check_if_large_group"
]

candidate[
  n_cells_group >= 500 & review_priority == "manual_check_if_large_group",
  review_priority := "high_priority_large_unassigned"
]

candidate <- candidate[order(dataset, -DA_maturation_combined, -safety_risk_combined, -n_cells_group)]

atomic_write_csv(as.data.frame(candidate), candidate_groups_csv)

da_candidates <- candidate[
  !is.na(DA_core_identity) &
    (
      preliminary_suggestion == "DA_like_neuronal_candidate" |
        DA_core_identity >= MIN_MEAN_SCORE |
        DA_maturation_combined >= MIN_MEAN_SCORE
    )
]

da_candidates[
  ,
  DA_subtype_bias := fifelse(
    is.na(A9_minus_A10_score),
    "unknown",
    fifelse(A9_minus_A10_score > 0.02, "A9_like_bias",
            fifelse(A9_minus_A10_score < -0.02, "A10_like_bias", "A9_A10_mixed_or_unclear"))
  )
]

da_candidates <- da_candidates[order(dataset, -DA_maturation_combined, -A9_like_DA_identity, -A10_like_DA_identity)]

if (nrow(da_candidates) > MAX_REVIEW_GROUPS_PER_CATEGORY) {
  da_candidates <- da_candidates[seq_len(MAX_REVIEW_GROUPS_PER_CATEGORY)]
}

atomic_write_csv(as.data.frame(da_candidates), da_a9_a10_csv)

safety_candidates <- candidate[
  !is.na(safety_risk_combined) &
    (
      preliminary_suggestion %in% c(
        "cycling_progenitor_safety_risk_candidate",
        "immature_pluripotency_risk_signal_candidate",
        "midbrain_progenitor_like_candidate"
      ) |
        safety_risk_combined >= MIN_MEAN_SCORE |
        cell_cycle_proliferation >= MIN_MEAN_SCORE |
        pluripotency_immature_risk >= MIN_MEAN_SCORE
    )
]

safety_candidates[
  ,
  safety_risk_reason := paste0(
    "cell_cycle=", round(cell_cycle_proliferation, 3),
    "; pluripotency=", round(pluripotency_immature_risk, 3),
    "; progenitor=", round(progenitor_neuroepithelial, 3),
    "; stress=", round(stress_apoptosis_response, 3)
  )
]

safety_candidates <- safety_candidates[order(dataset, -safety_risk_combined, -cell_cycle_proliferation, -pluripotency_immature_risk)]

if (nrow(safety_candidates) > MAX_REVIEW_GROUPS_PER_CATEGORY) {
  safety_candidates <- safety_candidates[seq_len(MAX_REVIEW_GROUPS_PER_CATEGORY)]
}

atomic_write_csv(as.data.frame(safety_candidates), safety_risk_csv)

stamp("生成 key marker gene snapshot。")

key_genes <- c(
  "TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "FOXA2", "LMX1A", "LMX1B", "PITX3",
  "ALDH1A1", "KCNJ6", "SOX6", "CALB1", "OTX2",
  "MKI67", "TOP2A", "PCNA", "SOX2", "NES", "POU5F1", "NANOG",
  "GFAP", "AQP4", "OLIG2", "PDGFRA", "PTPRC", "COL1A1"
)

if (nrow(gg_dt) > 0 && all(c("gene_symbol_matched", "dataset", "object_id", "group_id") %in% colnames(gg_dt))) {
  gg_dt[, gene_upper := toupper(gene_symbol_matched)]
  key_upper <- toupper(key_genes)

  marker_snapshot <- gg_dt[
    gene_upper %in% key_upper,
    .(
      dataset,
      object_id,
      group_source,
      group_id,
      category,
      gene_symbol_matched,
      n_cells,
      mean_expression,
      median_expression,
      pct_expressing
    )
  ][order(dataset, object_id, group_id, gene_symbol_matched)]
} else {
  marker_snapshot <- data.table()
}

atomic_write_csv(as.data.frame(marker_snapshot), marker_gene_snapshot_csv)

n_success_objects <- length(unique(paste(gc_dt$dataset, gc_dt$object_id, sep = "||")))
n_failed_objects <- if (nrow(failed_dt) > 0 && all(c("dataset", "object_id") %in% colnames(failed_dt))) {
  length(unique(paste(failed_dt$dataset, failed_dt$object_id, sep = "||")))
} else {
  0L
}

matrix_method_summary <- if (nrow(matrix_dt) > 0 && "extraction_method" %in% colnames(matrix_dt)) {
  matrix_dt[, .N, by = .(extraction_method, layer_type, layer)][order(-N)]
} else {
  data.table()
}

qc_audit <- data.frame(
  metric = c(
    "objects_with_marker_scores",
    "objects_failed",
    "group_category_rows",
    "group_gene_rows",
    "preliminary_suggestion_rows",
    "candidate_review_groups",
    "DA_candidate_groups",
    "safety_risk_candidate_groups"
  ),
  value = c(
    n_success_objects,
    n_failed_objects,
    nrow(gc_dt),
    nrow(gg_dt),
    nrow(prelim_dt),
    nrow(candidate),
    nrow(da_candidates),
    nrow(safety_candidates)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)

suggestion_lines <- if (nrow(suggestion_summary) > 0) {
  sug_top <- suggestion_summary[order(-n_groups)][seq_len(min(20, nrow(suggestion_summary)))]
  paste0(sug_top$dataset, " / ", sug_top$preliminary_suggestion, ": ", sug_top$n_groups)
} else {
  character()
}

matrix_method_lines <- if (nrow(matrix_method_summary) > 0) {
  apply(as.data.frame(matrix_method_summary), 1, function(x) {
    paste0(x[["extraction_method"]], " / ", x[["layer_type"]], " / ", x[["layer"]], ": ", x[["N"]])
  })
} else {
  character()
}

failed_lines <- if (nrow(failed_dt) > 0 && all(c("dataset", "object_id", "message") %in% colnames(failed_dt))) {
  apply(as.data.frame(failed_dt), 1, function(x) {
    paste0(x[["dataset"]], " :: ", x[["object_id"]], " — ", x[["message"]])
  })
} else {
  character()
}

report_lines <- c(
  "04C marker review and annotation rules report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "04B input summary:",
  paste0("Objects with marker scores: ", n_success_objects),
  paste0("Objects failed: ", n_failed_objects),
  paste0("Group-category score rows: ", nrow(gc_dt)),
  paste0("Group-marker gene expression rows: ", nrow(gg_dt)),
  paste0("Preliminary suggestion rows: ", nrow(prelim_dt)),
  "",
  "Matrix extraction method summary:",
  matrix_method_lines,
  "",
  "Failed object notes:",
  failed_lines,
  "",
  "Top preliminary suggestion summary:",
  suggestion_lines,
  "",
  "Candidate table summary:",
  paste0("Manual review groups: ", nrow(candidate)),
  paste0("DA/A9/A10 candidate groups: ", nrow(da_candidates)),
  paste0("Safety-risk candidate groups: ", nrow(safety_candidates)),
  "",
  "Output files:",
  paste0("Annotation rule table: ", annotation_rules_csv),
  paste0("Suggestion summary: ", suggestion_summary_csv),
  paste0("Dataset category signal summary: ", dataset_category_signal_csv),
  paste0("Candidate groups for manual review: ", candidate_groups_csv),
  paste0("DA/A9/A10 candidate groups: ", da_a9_a10_csv),
  paste0("Safety-risk candidate groups: ", safety_risk_csv),
  paste0("Key marker gene snapshot: ", marker_gene_snapshot_csv),
  paste0("QC audit summary: ", qc_audit_csv),
  "",
  "Next step:",
  "04D_APPLY_REVIEWED_ANNOTATION_LABELS.R",
  "",
  "Journal-rigor note:",
  "04C defines review rules and candidate groups only. Final labels must be applied after manual review and should avoid overclaiming real projection or therapeutic function."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("04C marker review and annotation rules 运行结束\n")
cat("============================================================\n\n")

cat("Objects with marker scores：", n_success_objects, "\n")
cat("Objects failed：", n_failed_objects, "\n")
cat("Group-category score rows：", nrow(gc_dt), "\n")
cat("Group-gene expression rows：", nrow(gg_dt), "\n")
cat("Preliminary suggestion rows：", nrow(prelim_dt), "\n")
cat("DA/A9/A10 candidate groups：", nrow(da_candidates), "\n")
cat("Safety-risk candidate groups：", nrow(safety_candidates), "\n\n")

cat("输出文件：\n")
cat(annotation_rules_csv, "\n")
cat(suggestion_summary_csv, "\n")
cat(dataset_category_signal_csv, "\n")
cat(candidate_groups_csv, "\n")
cat(da_a9_a10_csv, "\n")
cat(safety_risk_csv, "\n")
cat(marker_gene_snapshot_csv, "\n")
cat(qc_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 04C marker review and annotation rules 完成。\n")
cat("下一步：先人工查看 04C_DA_A9_A10_candidate_groups.csv 和 04C_safety_risk_candidate_groups.csv，再进入 04D。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"
SAVE_RDS_COMPRESS <- FALSE

cat("\n============================================================\n")
cat("04D V3：final audit and repair missing annotated objects\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04D V3。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_main_manifest <- file.path(tables_dir, "03C_strategy", "03C_main_analysis_object_manifest.csv")
input_group_annotation <- file.path(tables_dir, "04D_annotations", "04D_group_annotation_table.csv")
input_object_summary <- file.path(tables_dir, "04D_annotations", "04D_object_annotation_summary.csv")
input_annotated_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_failed <- file.path(tables_dir, "04D_annotations", "04D_failed_objects.csv")

out_tables_dir <- file.path(tables_dir, "04D_annotations")
out_objects_dir <- file.path(objects_dir, "04D_annotated_objects")

object_summary_csv <- input_object_summary
annotated_manifest_csv <- input_annotated_manifest
failed_objects_csv <- input_failed
final_audit_csv <- file.path(out_tables_dir, "04D_V3_final_audit_summary.csv")
report_txt <- file.path(reports_dir, "04D_V3_final_audit_and_repair_report.txt")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

safe_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "/", x)
  x <- basename(x)
  x <- gsub("\\.rds$|\\.csv$|\\.tsv$|\\.txt$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

extract_group_column_from_source <- function(group_source) {
  group_source <- as.character(group_source)

  if (is.na(group_source) || group_source == "") return(NA_character_)
  if (group_source == "object_all_no_cluster") return("object_all_no_cluster")

  group_source <- sub("^03A_", "", group_source)
  group_source <- sub("^expression_", "", group_source)

  group_source
}

ensure_annotation_columns <- function(meta) {
  if (!"annotation_04D_v1" %in% colnames(meta)) meta[["annotation_04D_v1"]] <- "unassigned_no_04B_group"
  if (!"annotation_04D_confidence" %in% colnames(meta)) meta[["annotation_04D_confidence"]] <- "none"
  if (!"annotation_04D_subtype_bias" %in% colnames(meta)) meta[["annotation_04D_subtype_bias"]] <- NA_character_
  if (!"annotation_04D_caution" %in% colnames(meta)) meta[["annotation_04D_caution"]] <- "No 04B group-level annotation available."
  if (!"annotation_04D_group_source" %in% colnames(meta)) meta[["annotation_04D_group_source"]] <- NA_character_
  if (!"annotation_04D_group_id" %in% colnames(meta)) meta[["annotation_04D_group_id"]] <- NA_character_
  if (!"annotation_04D_DA_maturation_combined" %in% colnames(meta)) meta[["annotation_04D_DA_maturation_combined"]] <- NA_real_
  if (!"annotation_04D_safety_risk_combined" %in% colnames(meta)) meta[["annotation_04D_safety_risk_combined"]] <- NA_real_
  if (!"annotation_04D_A9_minus_A10_score" %in% colnames(meta)) meta[["annotation_04D_A9_minus_A10_score"]] <- NA_real_
  meta
}

repair_one_object <- function(row, group_ann) {
  ds <- row$dataset
  oid <- row$object_id
  in_rds <- row$initial_pca_object

  out_dir <- file.path(out_objects_dir, ds)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_rds <- file.path(out_dir, paste0(safe_name(oid), "_04D_annotated.rds"))

  obj <- readRDS(in_rds)

  meta <- as.data.frame(obj@meta.data, stringsAsFactors = FALSE)
  meta <- ensure_annotation_columns(meta)

  sub_ann <- as.data.table(group_ann)[dataset == ds & object_id == oid]

  n_groups_written <- 0L

  if (nrow(sub_ann) == 0L) {
    meta[["annotation_04D_v1"]] <- "unassigned_no_04B_group"
    meta[["annotation_04D_confidence"]] <- "none"
    meta[["annotation_04D_caution"]] <- "No 04B group-level annotation available."
    meta[["annotation_04D_group_source"]] <- "no_04B_group"
    meta[["annotation_04D_group_id"]] <- "no_04B_group"
    status <- "REPAIRED_SAVED_UNASSIGNED_NO_04B"
  } else {
    for (j in seq_len(nrow(sub_ann))) {
      gs <- sub_ann$group_source[[j]]
      gid <- as.character(sub_ann$group_id[[j]])

      group_col <- extract_group_column_from_source(gs)

      if (is.na(group_col)) next

      if (group_col == "object_all_no_cluster") {
        cells_use <- rownames(meta)
      } else if (group_col %in% colnames(meta)) {
        cells_use <- rownames(meta)[as.character(meta[[group_col]]) == gid]
      } else if ("seurat_clusters" %in% colnames(meta)) {
        cells_use <- rownames(meta)[as.character(meta[["seurat_clusters"]]) == gid]
      } else {
        cells_use <- character()
      }

      if (length(cells_use) == 0L) next

      meta[cells_use, "annotation_04D_v1"] <- sub_ann$annotation_v1_conservative[[j]]
      meta[cells_use, "annotation_04D_confidence"] <- sub_ann$annotation_v1_confidence[[j]]
      meta[cells_use, "annotation_04D_subtype_bias"] <- sub_ann$annotation_v1_subtype_bias[[j]]
      meta[cells_use, "annotation_04D_caution"] <- sub_ann$annotation_v1_caution[[j]]
      meta[cells_use, "annotation_04D_group_source"] <- gs
      meta[cells_use, "annotation_04D_group_id"] <- gid
      meta[cells_use, "annotation_04D_DA_maturation_combined"] <- sub_ann$DA_maturation_combined[[j]]
      meta[cells_use, "annotation_04D_safety_risk_combined"] <- sub_ann$safety_risk_combined[[j]]
      meta[cells_use, "annotation_04D_A9_minus_A10_score"] <- sub_ann$A9_minus_A10_score[[j]]

      n_groups_written <- n_groups_written + 1L
    }

    status <- "REPAIRED_SUCCESS_ANNOTATED"
  }

  obj@meta.data <- meta

  ann_tab <- as.data.table(meta)[
    ,
    .N,
    by = annotation_04D_v1
  ][order(-N)]

  dominant_annotation <- if (nrow(ann_tab) > 0L) ann_tab$annotation_04D_v1[[1L]] else NA_character_
  dominant_n <- if (nrow(ann_tab) > 0L) ann_tab$N[[1L]] else NA_integer_

  saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

  list(
    object_summary = data.frame(
      dataset = ds,
      object_id = oid,
      n_cells = ncol(obj),
      n_groups_available = nrow(sub_ann),
      n_groups_written = n_groups_written,
      dominant_annotation = dominant_annotation,
      dominant_annotation_cells = dominant_n,
      status = status,
      stringsAsFactors = FALSE
    ),
    manifest = data.frame(
      dataset = ds,
      object_id = oid,
      input_rds = in_rds,
      annotated_rds = out_rds,
      status = status,
      stringsAsFactors = FALSE
    )
  )
}

stamp("读取 03C / 04D 输出。")

main_manifest <- read_csv_required(input_main_manifest)
group_ann <- read_csv_required(input_group_annotation)
object_summary <- read_csv_optional(input_object_summary)
annotated_manifest <- read_csv_optional(input_annotated_manifest)
failed_objects <- read_csv_optional(input_failed)

if (!all(c("dataset", "object_id", "initial_pca_object") %in% colnames(main_manifest))) {
  stop("03C main manifest 缺少 dataset/object_id/initial_pca_object。")
}

main_manifest <- main_manifest[file.exists(main_manifest$initial_pca_object), , drop = FALSE]

main_keys <- paste(main_manifest$dataset, main_manifest$object_id, sep = "||")

if (nrow(annotated_manifest) > 0 && all(c("dataset", "object_id", "annotated_rds") %in% colnames(annotated_manifest))) {
  ok_manifest <- annotated_manifest[
    file.exists(annotated_manifest$annotated_rds),
    ,
    drop = FALSE
  ]
  done_keys <- paste(ok_manifest$dataset, ok_manifest$object_id, sep = "||")
} else {
  done_keys <- character()
}

missing_keys <- setdiff(main_keys, done_keys)

stamp("main manifest objects：", length(main_keys))
stamp("已有 annotated rds objects：", length(done_keys))
stamp("缺失 objects：", length(missing_keys))

missing_rows <- main_manifest[main_keys %in% missing_keys, , drop = FALSE]

repair_summary_list <- list()
repair_manifest_list <- list()
repair_failed_list <- list()

if (nrow(missing_rows) > 0L) {
  for (i in seq_len(nrow(missing_rows))) {
    ds <- missing_rows$dataset[[i]]
    oid <- missing_rows$object_id[[i]]

    stamp("修复缺失对象 ", i, " / ", nrow(missing_rows), "：", ds, " :: ", oid)

    res <- tryCatch({
      repair_one_object(missing_rows[i, , drop = FALSE], group_ann)
    }, error = function(e) {
      msg <- conditionMessage(e)
      stamp("  修复失败：", msg)

      repair_failed_list[[length(repair_failed_list) + 1L]] <<- data.frame(
        dataset = ds,
        object_id = oid,
        input_rds = missing_rows$initial_pca_object[[i]],
        stage = "repair_missing_object",
        message = msg,
        stringsAsFactors = FALSE
      )

      NULL
    })

    if (!is.null(res)) {
      repair_summary_list[[length(repair_summary_list) + 1L]] <- res$object_summary
      repair_manifest_list[[length(repair_manifest_list) + 1L]] <- res$manifest
    }
  }
}

repair_summary <- if (length(repair_summary_list) > 0L) rbindlist(repair_summary_list, fill = TRUE) else data.frame()
repair_manifest <- if (length(repair_manifest_list) > 0L) rbindlist(repair_manifest_list, fill = TRUE) else data.frame()
repair_failed <- if (length(repair_failed_list) > 0L) rbindlist(repair_failed_list, fill = TRUE) else data.frame()

if (nrow(object_summary) > 0 && nrow(repair_summary) > 0) {
  existing_keys_summary <- paste(object_summary$dataset, object_summary$object_id, sep = "||")
  repair_keys_summary <- paste(repair_summary$dataset, repair_summary$object_id, sep = "||")
  object_summary <- object_summary[!existing_keys_summary %in% repair_keys_summary, , drop = FALSE]
  object_summary <- rbindlist(list(object_summary, repair_summary), fill = TRUE)
} else if (nrow(repair_summary) > 0) {
  object_summary <- repair_summary
}

if (nrow(annotated_manifest) > 0 && nrow(repair_manifest) > 0) {
  existing_keys_manifest <- paste(annotated_manifest$dataset, annotated_manifest$object_id, sep = "||")
  repair_keys_manifest <- paste(repair_manifest$dataset, repair_manifest$object_id, sep = "||")
  annotated_manifest <- annotated_manifest[!existing_keys_manifest %in% repair_keys_manifest, , drop = FALSE]
  annotated_manifest <- rbindlist(list(annotated_manifest, repair_manifest), fill = TRUE)
} else if (nrow(repair_manifest) > 0) {
  annotated_manifest <- repair_manifest
}

if (nrow(failed_objects) > 0 && nrow(repair_manifest) > 0 && all(c("dataset", "object_id") %in% colnames(failed_objects))) {
  repair_keys_manifest <- paste(repair_manifest$dataset, repair_manifest$object_id, sep = "||")
  failed_keys <- paste(failed_objects$dataset, failed_objects$object_id, sep = "||")
  failed_objects <- failed_objects[!failed_keys %in% repair_keys_manifest, , drop = FALSE]
}

if (nrow(repair_failed) > 0) {
  failed_objects <- rbindlist(list(failed_objects, repair_failed), fill = TRUE)
}

atomic_write_csv(as.data.frame(object_summary), object_summary_csv)
atomic_write_csv(as.data.frame(annotated_manifest), annotated_manifest_csv)
atomic_write_csv(as.data.frame(failed_objects), failed_objects_csv)

if (nrow(annotated_manifest) > 0 && all(c("dataset", "object_id", "annotated_rds") %in% colnames(annotated_manifest))) {
  annotated_manifest$file_exists <- file.exists(annotated_manifest$annotated_rds)
  final_done_keys <- paste(
    annotated_manifest$dataset[annotated_manifest$file_exists],
    annotated_manifest$object_id[annotated_manifest$file_exists],
    sep = "||"
  )
} else {
  final_done_keys <- character()
}

final_missing_keys <- setdiff(main_keys, final_done_keys)

final_audit <- data.frame(
  metric = c(
    "main_manifest_objects",
    "annotated_rds_existing_objects",
    "missing_objects_after_repair",
    "objects_repaired_now",
    "objects_repair_failed_now",
    "failed_table_rows_after_repair"
  ),
  value = c(
    length(main_keys),
    length(final_done_keys),
    length(final_missing_keys),
    nrow(repair_manifest),
    nrow(repair_failed),
    nrow(failed_objects)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(final_audit, final_audit_csv)

missing_lines <- if (length(final_missing_keys) > 0) final_missing_keys else "none"

repair_lines <- if (nrow(repair_manifest) > 0) {
  paste0(repair_manifest$dataset, " :: ", repair_manifest$object_id, " — ", repair_manifest$status)
} else {
  "none"
}

failed_lines <- if (nrow(repair_failed) > 0) {
  paste0(repair_failed$dataset, " :: ", repair_failed$object_id, " — ", repair_failed$message)
} else {
  "none"
}

report_lines <- c(
  "04D V3 final audit and repair report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Audit before repair:",
  paste0("Main manifest objects: ", length(main_keys)),
  paste0("Existing annotated RDS objects before repair: ", length(done_keys)),
  paste0("Missing objects before repair: ", length(missing_keys)),
  "",
  "Repair results:",
  repair_lines,
  "",
  "Repair failures:",
  failed_lines,
  "",
  "Final audit:",
  paste0("Annotated RDS existing objects after repair: ", length(final_done_keys)),
  paste0("Missing objects after repair: ", length(final_missing_keys)),
  "Missing keys:",
  missing_lines,
  "",
  "Output files:",
  paste0("Object summary: ", object_summary_csv),
  paste0("Annotated manifest: ", annotated_manifest_csv),
  paste0("Failed objects: ", failed_objects_csv),
  paste0("Final audit summary: ", final_audit_csv),
  "",
  "Next step:",
  "05A_DA_A9_A10_AND_PROJECTION_COMPETENCE_SCORING.R"
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("04D V3 final audit and repair 运行结束\n")
cat("============================================================\n\n")

cat("Main manifest objects：", length(main_keys), "\n")
cat("Annotated RDS existing objects after repair：", length(final_done_keys), "\n")
cat("Missing objects after repair：", length(final_missing_keys), "\n")
cat("Objects repaired now：", nrow(repair_manifest), "\n")
cat("Repair failed now：", nrow(repair_failed), "\n\n")

cat("输出文件：\n")
cat(object_summary_csv, "\n")
cat(annotated_manifest_csv, "\n")
cat(failed_objects_csv, "\n")
cat(final_audit_csv, "\n")
cat(report_txt, "\n\n")

if (length(final_missing_keys) == 0L && nrow(repair_failed) == 0L) {
  cat("✅ 04D V3 final audit and repair 完成。\n")
  cat("现在 04D annotated manifest 已完整，可以进入 05A。\n")
} else {
  cat("⚠️ 04D V3 完成，但仍有缺失/失败对象。请查看 04D_V3_final_audit_summary.csv。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

WRITE_CELL_LEVEL_SCORES <- TRUE

DA_LIKE_GROUP_MIN <- 0.08
PROJECTION_COMPETENCE_GROUP_MIN <- 0.08
A9_A10_BIAS_DELTA <- 0.02
SAFETY_RISK_LOW_MAX <- 0.20

set.seed(20260714)

cat("\n============================================================\n")
cat("05A：DA/A9/A10/projection competence scoring\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table", "Matrix")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 05A。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(data.table)
  library(Matrix)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_annotated_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")

out_tables_dir <- file.path(tables_dir, "05A_DA_projection_scoring")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

signature_gene_sets_csv <- file.path(out_tables_dir, "05A_signature_gene_sets.csv")
cell_level_scores_csv <- file.path(out_tables_dir, "05A_cell_level_scores.csv")
group_level_scores_csv <- file.path(out_tables_dir, "05A_group_level_scores.csv")
object_level_scores_csv <- file.path(out_tables_dir, "05A_object_level_scores.csv")
candidate_groups_csv <- file.path(out_tables_dir, "05A_DA_A9_A10_projection_candidate_groups.csv")
failed_objects_csv <- file.path(out_tables_dir, "05A_failed_objects.csv")
matrix_source_csv <- file.path(out_tables_dir, "05A_matrix_source_audit.csv")
report_txt <- file.path(reports_dir, "05A_DA_A9_A10_projection_competence_scoring_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

get_assay_for_analysis <- function(obj) {
  assays <- names(obj@assays)

  if ("RNA" %in% assays) return("RNA")

  da <- tryCatch(DefaultAssay(obj), error = function(e) NA_character_)

  if (!is.na(da) && da %in% assays) return(da)

  if (length(assays) > 0L) return(assays[[1L]])

  NA_character_
}

match_genes_case_insensitive <- function(query_genes, object_genes) {
  query_genes <- unique(as.character(query_genes))
  object_genes <- unique(as.character(object_genes))

  q_upper <- toupper(query_genes)
  g_upper <- toupper(object_genes)

  idx <- match(q_upper, g_upper)

  data.frame(
    query_gene = query_genes,
    query_upper = q_upper,
    matched_gene = ifelse(is.na(idx), NA_character_, object_genes[idx]),
    present = !is.na(idx),
    stringsAsFactors = FALSE
  )
}

get_assay5_layer_names <- function(assay_obj) {
  out <- tryCatch({
    names(slot(assay_obj, "layers"))
  }, error = function(e) {
    character()
  })

  out[!is.na(out) & nzchar(out)]
}

get_logmap_names <- function(logmap_obj, layer_name) {
  out <- tryCatch({
    if (layer_name %in% colnames(logmap_obj)) {
      rownames(logmap_obj)[as.logical(logmap_obj[, layer_name])]
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })

  out
}

repair_layer_dimnames <- function(mat, obj, assay_obj, layer_name) {
  if (is.null(mat)) return(NULL)

  if (!inherits(mat, "dgCMatrix")) {
    mat <- tryCatch(as(mat, "dgCMatrix"), error = function(e) NULL)
  }

  if (is.null(mat)) return(NULL)
  if (nrow(mat) == 0L || ncol(mat) == 0L) return(NULL)

  obj_features <- tryCatch(rownames(obj), error = function(e) NULL)
  obj_cells <- tryCatch(colnames(obj), error = function(e) NULL)

  fmap <- tryCatch(slot(assay_obj, "features"), error = function(e) NULL)
  cmap <- tryCatch(slot(assay_obj, "cells"), error = function(e) NULL)

  layer_features <- if (!is.null(fmap)) get_logmap_names(fmap, layer_name) else NULL
  layer_cells <- if (!is.null(cmap)) get_logmap_names(cmap, layer_name) else NULL

  candidate_features <- list(layer_features, obj_features)
  candidate_cells <- list(layer_cells, obj_cells)

  feature_lengths <- unique(na.omit(vapply(candidate_features, function(x) if (is.null(x)) NA_integer_ else length(x), integer(1))))
  cell_lengths <- unique(na.omit(vapply(candidate_cells, function(x) if (is.null(x)) NA_integer_ else length(x), integer(1))))

  if (length(feature_lengths) > 0L && length(cell_lengths) > 0L) {
    if (
      nrow(mat) %in% cell_lengths &&
        ncol(mat) %in% feature_lengths &&
        !(nrow(mat) %in% feature_lengths && ncol(mat) %in% cell_lengths)
    ) {
      mat <- Matrix::t(mat)
    }
  }

  if (
    is.null(rownames(mat)) ||
      length(rownames(mat)) != nrow(mat) ||
      any(is.na(rownames(mat))) ||
      any(rownames(mat) == "")
  ) {
    rn <- NULL

    if (!is.null(layer_features) && length(layer_features) == nrow(mat)) {
      rn <- layer_features
    } else if (!is.null(obj_features) && length(obj_features) == nrow(mat)) {
      rn <- obj_features
    }

    if (!is.null(rn)) {
      rownames(mat) <- make.unique(as.character(rn), sep = "__dupGene")
    }
  }

  if (
    is.null(colnames(mat)) ||
      length(colnames(mat)) != ncol(mat) ||
      any(is.na(colnames(mat))) ||
      any(colnames(mat) == "")
  ) {
    cn <- NULL

    if (!is.null(layer_cells) && length(layer_cells) == ncol(mat)) {
      cn <- layer_cells
    } else if (!is.null(obj_cells) && length(obj_cells) == ncol(mat)) {
      cn <- obj_cells
    }

    if (!is.null(cn)) {
      colnames(mat) <- make.unique(as.character(cn), sep = "__dupCell")
    }
  }

  if (is.null(rownames(mat)) || is.null(colnames(mat))) return(NULL)
  if (length(rownames(mat)) != nrow(mat) || length(colnames(mat)) != ncol(mat)) return(NULL)

  mat
}

extract_expression_matrix_direct <- function(obj) {
  assay <- get_assay_for_analysis(obj)

  if (is.na(assay) || !assay %in% names(obj@assays)) {
    stop("No valid assay found.")
  }

  assay_obj <- obj[[assay]]
  layer_names <- get_assay5_layer_names(assay_obj)

  if (length(layer_names) > 0L) {
    candidate_layers <- unique(c(
      "counts",
      grep("^counts", layer_names, value = TRUE),
      "data",
      grep("^data", layer_names, value = TRUE)
    ))

    candidate_layers <- candidate_layers[candidate_layers %in% layer_names]

    for (lyr in candidate_layers) {
      raw_mat <- tryCatch({
        slot(assay_obj, "layers")[[lyr]]
      }, error = function(e) NULL)

      mat <- repair_layer_dimnames(raw_mat, obj, assay_obj, lyr)

      if (!is.null(mat)) {
        return(list(
          matrix = mat,
          assay = assay,
          layer = lyr,
          layer_type = ifelse(grepl("^counts", lyr), "counts", "data"),
          method = "direct_Assay5_layers"
        ))
      }
    }
  }

  for (lyr in c("counts", "data")) {
    mat <- tryCatch({
      SeuratObject::LayerData(obj, assay = assay, layer = lyr, fast = FALSE)
    }, error = function(e) NULL)

    mat <- repair_layer_dimnames(mat, obj, assay_obj, lyr)

    if (!is.null(mat)) {
      return(list(
        matrix = mat,
        assay = assay,
        layer = lyr,
        layer_type = ifelse(lyr == "counts", "counts", "data"),
        method = "LayerData"
      ))
    }
  }

  for (sl in c("counts", "data")) {
    mat <- tryCatch({
      slot(assay_obj, sl)
    }, error = function(e) NULL)

    mat <- repair_layer_dimnames(mat, obj, assay_obj, sl)

    if (!is.null(mat)) {
      return(list(
        matrix = mat,
        assay = assay,
        layer = sl,
        layer_type = ifelse(sl == "counts", "counts", "data"),
        method = "Assay_slot"
      ))
    }
  }

  stop(
    "Cannot extract expression matrix. assay=",
    assay,
    "; layers=",
    paste(layer_names, collapse = ",")
  )
}

score_signatures_from_matrix <- function(expr_info, signature_sets) {
  mat <- expr_info$matrix

  all_sig_genes <- unique(unlist(signature_sets, use.names = FALSE))
  all_sig_genes <- all_sig_genes[!is.na(all_sig_genes) & nzchar(all_sig_genes)]

  gene_match <- match_genes_case_insensitive(all_sig_genes, rownames(mat))
  present_all <- unique(gene_match$matched_gene[gene_match$present])
  present_all <- present_all[!is.na(present_all)]

  if (length(present_all) == 0L) {
    return(NULL)
  }

  mat_sub_all <- mat[present_all, , drop = FALSE]

  if (expr_info$layer_type == "counts") {
    lib <- Matrix::colSums(mat)
    lib[is.na(lib) | lib <= 0] <- 1
    mat_sub_all <- t(t(mat_sub_all) / lib * 10000)
    mat_sub_all <- log1p(mat_sub_all)
  }

  score_dt <- data.table(cell = colnames(mat_sub_all))

  coverage_list <- list()

  for (sig_name in names(signature_sets)) {
    sig_genes <- unique(signature_sets[[sig_name]])
    matched <- match_genes_case_insensitive(sig_genes, rownames(mat_sub_all))

    sig_present <- unique(matched$matched_gene[matched$present])
    sig_present <- sig_present[!is.na(sig_present)]

    if (length(sig_present) == 0L) {
      score_dt[[paste0(sig_name, "_score")]] <- NA_real_
    } else {
      sig_mat <- mat_sub_all[sig_present, , drop = FALSE]
      score_dt[[paste0(sig_name, "_score")]] <- Matrix::colMeans(sig_mat)
    }

    coverage_list[[length(coverage_list) + 1L]] <- data.frame(
      signature = sig_name,
      n_signature_genes = length(sig_genes),
      n_present_genes = length(sig_present),
      coverage_fraction = ifelse(length(sig_genes) > 0L, length(sig_present) / length(sig_genes), NA_real_),
      present_genes = paste(sig_present, collapse = ";"),
      missing_genes = paste(setdiff(toupper(sig_genes), toupper(sig_present)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }

  list(
    scores = score_dt,
    coverage = rbindlist(coverage_list, fill = TRUE)
  )
}

safe_meta_col <- function(meta, col, default = NA_character_) {
  if (col %in% colnames(meta)) {
    return(meta[[col]])
  }
  rep(default, nrow(meta))
}

compute_composites <- function(dt) {
  needed <- c(
    "DA_core_identity_score",
    "DA_functional_machinery_score",
    "A9_like_DA_identity_score",
    "A10_like_DA_identity_score",
    "neuronal_maturation_synapse_score",
    "projection_associated_molecular_competence_score"
  )

  for (col in needed) {
    if (!col %in% colnames(dt)) {
      dt[[col]] <- NA_real_
    }
  }

  dt[
    ,
    DA_like_composite_score := rowMeans(
      cbind(
        DA_core_identity_score,
        DA_functional_machinery_score,
        neuronal_maturation_synapse_score
      ),
      na.rm = TRUE
    )
  ]

  dt[
    ,
    A9_minus_A10_score_05A := A9_like_DA_identity_score - A10_like_DA_identity_score
  ]

  dt[
    ,
    projection_competence_composite_score := rowMeans(
      cbind(
        projection_associated_molecular_competence_score,
        neuronal_maturation_synapse_score
      ),
      na.rm = TRUE
    )
  ]

  dt[
    ,
    DA_projection_competence_composite_score := rowMeans(
      cbind(
        DA_like_composite_score,
        projection_competence_composite_score
      ),
      na.rm = TRUE
    )
  ]

  dt[
    ,
    A9_A10_bias_label_05A := fifelse(
      is.na(A9_minus_A10_score_05A),
      "unknown",
      fifelse(
        A9_minus_A10_score_05A > A9_A10_BIAS_DELTA,
        "A9_like_bias",
        fifelse(
          A9_minus_A10_score_05A < -A9_A10_BIAS_DELTA,
          "A10_like_bias",
          "A9_A10_mixed_or_unclear"
        )
      )
    )
  ]

  dt
}

write_partial_outputs <- function(
  cell_score_list,
  group_score_list,
  object_score_list,
  candidate_list,
  failed_list,
  matrix_source_list
) {
  cell_df <- if (length(cell_score_list) > 0L && WRITE_CELL_LEVEL_SCORES) {
    rbindlist(cell_score_list, fill = TRUE)
  } else {
    data.frame()
  }

  group_df <- if (length(group_score_list) > 0L) rbindlist(group_score_list, fill = TRUE) else data.frame()
  object_df <- if (length(object_score_list) > 0L) rbindlist(object_score_list, fill = TRUE) else data.frame()
  candidate_df <- if (length(candidate_list) > 0L) rbindlist(candidate_list, fill = TRUE) else data.frame()
  failed_df <- if (length(failed_list) > 0L) rbindlist(failed_list, fill = TRUE) else data.frame()
  matrix_df <- if (length(matrix_source_list) > 0L) rbindlist(matrix_source_list, fill = TRUE) else data.frame()

  if (WRITE_CELL_LEVEL_SCORES) atomic_write_csv(cell_df, cell_level_scores_csv)
  atomic_write_csv(group_df, group_level_scores_csv)
  atomic_write_csv(object_df, object_level_scores_csv)
  atomic_write_csv(candidate_df, candidate_groups_csv)
  atomic_write_csv(failed_df, failed_objects_csv)
  atomic_write_csv(matrix_df, matrix_source_csv)

  invisible(list(
    cell_df = cell_df,
    group_df = group_df,
    object_df = object_df,
    candidate_df = candidate_df,
    failed_df = failed_df,
    matrix_df = matrix_df
  ))
}

stamp("建立 05A signature gene sets。")

signature_sets <- list(
  DA_core_identity = c(
    "TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2",
    "FOXA2", "LMX1A", "LMX1B", "PITX3", "EN1"
  ),

  DA_functional_machinery = c(
    "TH", "DDC", "SLC6A3", "SLC18A2",
    "GCH1", "ALDH1A1", "KCNJ6", "DRD2"
  ),

  A9_like_DA_identity = c(
    "ALDH1A1", "KCNJ6", "SOX6", "DCLK3", "GCH1", "SLC10A4", "KCND3"
  ),

  A10_like_DA_identity = c(
    "CALB1", "OTX2", "CCK", "SLC17A6", "VIP", "NRIP3"
  ),

  neuronal_maturation_synapse = c(
    "RBFOX3", "MAP2", "TUBB3", "DCX", "STMN2",
    "SNAP25", "SYT1", "SYN1", "NEFL", "NEFM"
  ),

  projection_associated_molecular_competence = c(

    "STMN2", "GAP43", "DCX", "MAP2", "NEFL", "NEFM",

    "SNAP25", "SYT1", "SYN1", "RAB3A", "STX1A",

    "L1CAM", "CNTN2", "ROBO1", "ROBO2", "DCC",
    "NTN1", "SEMA3A", "SEMA3C", "NRP1", "PLXNA1",
    "EPHB1", "EPHB2", "EFNB2", "SLIT2"
  )
)

sig_df <- rbindlist(lapply(names(signature_sets), function(sig) {
  data.frame(
    signature = sig,
    gene_symbol = signature_sets[[sig]],
    stringsAsFactors = FALSE
  )
}), fill = TRUE)

sig_df$interpretation <- ifelse(
  sig_df$signature == "projection_associated_molecular_competence",
  "Projection-associated molecular competence only; not proof of real anatomical projection.",
  ifelse(
    grepl("A9", sig_df$signature),
    "A9-like molecular identity bias; not proof of substantia nigra functional identity.",
    ifelse(
      grepl("A10", sig_df$signature),
      "A10-like molecular identity bias; not proof of VTA functional identity.",
      "Transcriptomic signature score."
    )
  )
)

atomic_write_csv(sig_df, signature_gene_sets_csv)

stamp("读取 04D annotated object manifest。")

manifest <- read_csv_required(input_annotated_manifest)

if (!all(c("dataset", "object_id", "annotated_rds") %in% colnames(manifest))) {
  stop("04D annotated manifest 缺少 dataset/object_id/annotated_rds。")
}

manifest <- manifest[file.exists(manifest$annotated_rds), , drop = FALSE]

stamp("准备 scoring 对象数量：", nrow(manifest))

cell_score_list <- list()
group_score_list <- list()
object_score_list <- list()
candidate_list <- list()
failed_list <- list()
matrix_source_list <- list()

for (i in seq_len(nrow(manifest))) {
  ds <- manifest$dataset[[i]]
  oid <- manifest$object_id[[i]]
  path <- manifest$annotated_rds[[i]]

  stamp("05A 处理对象 ", i, " / ", nrow(manifest), "：", ds, " :: ", oid)

  tryCatch({
    obj <- readRDS(path)

    expr_info <- extract_expression_matrix_direct(obj)

    stamp(
      "  matrix：",
      nrow(expr_info$matrix), " genes x ", ncol(expr_info$matrix),
      " cells；method=", expr_info$method, "；layer=", expr_info$layer
    )

    score_out <- score_signatures_from_matrix(expr_info, signature_sets)

    if (is.null(score_out)) {
      stop("No signature genes could be extracted.")
    }

    scores <- score_out$scores
    cov <- score_out$coverage

    scores <- compute_composites(scores)

    meta <- obj@meta.data
    common_cells <- intersect(scores$cell, rownames(meta))

    if (length(common_cells) == 0L) {
      stop("No overlapping cells between score table and object metadata.")
    }

    scores <- scores[cell %in% common_cells]
    meta_sub <- meta[scores$cell, , drop = FALSE]

    scores[, dataset := ds]
    scores[, object_id := oid]
    scores[, annotation_04D_v1 := as.character(safe_meta_col(meta_sub, "annotation_04D_v1", "unknown"))]
    scores[, annotation_04D_confidence := as.character(safe_meta_col(meta_sub, "annotation_04D_confidence", "unknown"))]
    scores[, annotation_04D_subtype_bias := as.character(safe_meta_col(meta_sub, "annotation_04D_subtype_bias", "unknown"))]
    scores[, annotation_04D_group_id := as.character(safe_meta_col(meta_sub, "annotation_04D_group_id", "object_all"))]
    scores[, annotation_04D_group_source := as.character(safe_meta_col(meta_sub, "annotation_04D_group_source", "object_all"))]
    scores[, annotation_04D_safety_risk_combined := suppressWarnings(as.numeric(safe_meta_col(meta_sub, "annotation_04D_safety_risk_combined", NA_real_)))]

    front_cols <- c(
      "dataset", "object_id", "cell",
      "annotation_04D_v1", "annotation_04D_confidence",
      "annotation_04D_subtype_bias", "annotation_04D_group_source", "annotation_04D_group_id"
    )
    scores <- scores[, c(front_cols, setdiff(colnames(scores), front_cols)), with = FALSE]

    if (WRITE_CELL_LEVEL_SCORES) {
      cell_score_list[[length(cell_score_list) + 1L]] <- as.data.frame(scores)
    }

    group_dt <- copy(scores)

    group_cols <- c(
      "dataset", "object_id", "annotation_04D_group_source",
      "annotation_04D_group_id", "annotation_04D_v1"
    )

    numeric_score_cols <- grep("_score$|_composite_score$|A9_minus_A10_score_05A|annotation_04D_safety_risk_combined", colnames(group_dt), value = TRUE)

    group_summary <- group_dt[
      ,
      c(
        list(
          n_cells = .N,
          dominant_confidence = paste(unique(annotation_04D_confidence), collapse = ";"),
          dominant_subtype_bias_04D = paste(unique(annotation_04D_subtype_bias), collapse = ";")
        ),
        lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE)),
        lapply(.SD, function(x) mean(as.numeric(x) > 0, na.rm = TRUE))
      ),
      by = group_cols,
      .SDcols = numeric_score_cols
    ]

    old_names <- colnames(group_summary)
    n_base <- length(group_cols) + 3
    score_names <- numeric_score_cols
    pct_names <- paste0("pct_cells_", numeric_score_cols, "_gt0")
    colnames(group_summary) <- c(
      group_cols,
      "n_cells",
      "dominant_confidence",
      "dominant_subtype_bias_04D",
      score_names,
      pct_names
    )

    group_summary <- compute_composites(group_summary)

    group_summary[
      ,
      candidate_class_05A := fifelse(
        DA_like_composite_score >= DA_LIKE_GROUP_MIN &
          projection_competence_composite_score >= PROJECTION_COMPETENCE_GROUP_MIN &
          (is.na(annotation_04D_safety_risk_combined) | annotation_04D_safety_risk_combined <= SAFETY_RISK_LOW_MAX),
        "DA_projection_competence_candidate_low_safety_signal",
        fifelse(
          DA_like_composite_score >= DA_LIKE_GROUP_MIN &
            A9_minus_A10_score_05A > A9_A10_BIAS_DELTA,
          "DA_like_with_A9_molecular_bias",
          fifelse(
            DA_like_composite_score >= DA_LIKE_GROUP_MIN &
              A9_minus_A10_score_05A < -A9_A10_BIAS_DELTA,
            "DA_like_with_A10_molecular_bias",
            fifelse(
              projection_competence_composite_score >= PROJECTION_COMPETENCE_GROUP_MIN &
                DA_like_composite_score < DA_LIKE_GROUP_MIN,
              "projection_competence_without_strong_DA_identity",
              "lower_priority_or_mixed_signal"
            )
          )
        )
      )
    ]

    group_score_list[[length(group_score_list) + 1L]] <- as.data.frame(group_summary)

    object_summary <- group_dt[
      ,
      c(
        list(
          n_cells = .N,
          n_annotation_labels = length(unique(annotation_04D_v1)),
          dominant_annotation = names(sort(table(annotation_04D_v1), decreasing = TRUE))[1]
        ),
        lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE))
      ),
      by = .(dataset, object_id),
      .SDcols = numeric_score_cols
    ]

    object_summary <- compute_composites(object_summary)

    object_score_list[[length(object_score_list) + 1L]] <- as.data.frame(object_summary)

    candidate <- group_summary[
      candidate_class_05A != "lower_priority_or_mixed_signal"
    ][
      order(-DA_projection_competence_composite_score, -DA_like_composite_score, -projection_competence_composite_score)
    ]

    if (nrow(candidate) > 0L) {
      candidate_list[[length(candidate_list) + 1L]] <- as.data.frame(candidate)
    }

    cov[, dataset := ds]
    cov[, object_id := oid]
    cov[, assay := expr_info$assay]
    cov[, layer := expr_info$layer]
    cov[, layer_type := expr_info$layer_type]
    cov[, extraction_method := expr_info$method]
    cov[, n_matrix_genes := nrow(expr_info$matrix)]
    cov[, n_matrix_cells := ncol(expr_info$matrix)]

    matrix_source_list[[length(matrix_source_list) + 1L]] <- as.data.frame(cov)

    rm(obj, expr_info, score_out, scores, meta, meta_sub, group_dt, group_summary)
    gc(verbose = FALSE)

  }, error = function(e) {
    msg <- conditionMessage(e)
    stamp("  对象失败但不中断：", msg)

    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      annotated_rds = path,
      stage = "05A_scoring",
      message = msg,
      stringsAsFactors = FALSE
    )
  })

  write_partial_outputs(
    cell_score_list,
    group_score_list,
    object_score_list,
    candidate_list,
    failed_list,
    matrix_source_list
  )
}

final_outputs <- write_partial_outputs(
  cell_score_list,
  group_score_list,
  object_score_list,
  candidate_list,
  failed_list,
  matrix_source_list
)

cell_df <- final_outputs$cell_df
group_df <- final_outputs$group_df
object_df <- final_outputs$object_df
candidate_df <- final_outputs$candidate_df
failed_df <- final_outputs$failed_df
matrix_df <- final_outputs$matrix_df

n_objects_total <- nrow(manifest)

n_objects_success <- if (nrow(object_df) > 0L) {
  length(unique(paste(object_df$dataset, object_df$object_id, sep = "||")))
} else {
  0L
}

n_objects_failed <- if (nrow(failed_df) > 0L) {
  length(unique(paste(failed_df$dataset, failed_df$object_id, sep = "||")))
} else {
  0L
}

candidate_summary <- if (nrow(candidate_df) > 0L) {
  as.data.table(candidate_df)[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells, na.rm = TRUE),
      median_DA_like = median(DA_like_composite_score, na.rm = TRUE),
      median_projection_competence = median(projection_competence_composite_score, na.rm = TRUE),
      median_A9_minus_A10 = median(A9_minus_A10_score_05A, na.rm = TRUE)
    ),
    by = .(dataset, candidate_class_05A)
  ][order(dataset, -n_groups)]
} else {
  data.table()
}

candidate_lines <- if (nrow(candidate_summary) > 0L) {
  apply(as.data.frame(candidate_summary), 1, function(x) {
    paste0(
      x[["dataset"]],
      " / ",
      x[["candidate_class_05A"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

coverage_summary <- if (nrow(matrix_df) > 0L) {
  as.data.table(matrix_df)[
    ,
    .(
      median_coverage = median(coverage_fraction, na.rm = TRUE),
      min_coverage = min(coverage_fraction, na.rm = TRUE),
      max_coverage = max(coverage_fraction, na.rm = TRUE)
    ),
    by = signature
  ][order(signature)]
} else {
  data.table()
}

coverage_lines <- if (nrow(coverage_summary) > 0L) {
  apply(as.data.frame(coverage_summary), 1, function(x) {
    paste0(
      x[["signature"]],
      ": median coverage=",
      round(as.numeric(x[["median_coverage"]]) * 100, 1),
      "%"
    )
  })
} else {
  character()
}

failed_lines <- if (nrow(failed_df) > 0L) {
  apply(as.data.frame(failed_df), 1, function(x) {
    paste0(x[["dataset"]], " :: ", x[["object_id"]], " — ", x[["message"]])
  })
} else {
  "none"
}

report_lines <- c(
  "05A DA/A9/A10/projection competence scoring report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Objects in 04D annotated manifest: ", n_objects_total),
  paste0("Objects scored successfully: ", n_objects_success),
  paste0("Objects failed: ", n_objects_failed),
  paste0("Cell-level score rows: ", nrow(cell_df)),
  paste0("Group-level score rows: ", nrow(group_df)),
  paste0("Object-level score rows: ", nrow(object_df)),
  paste0("Candidate group rows: ", nrow(candidate_df)),
  "",
  "Signature coverage summary:",
  coverage_lines,
  "",
  "Candidate group summary:",
  candidate_lines,
  "",
  "Failed objects:",
  failed_lines,
  "",
  "Output files:",
  paste0("Signature gene sets: ", signature_gene_sets_csv),
  paste0("Cell-level scores: ", cell_level_scores_csv),
  paste0("Group-level scores: ", group_level_scores_csv),
  paste0("Object-level scores: ", object_level_scores_csv),
  paste0("Candidate groups: ", candidate_groups_csv),
  paste0("Matrix source audit: ", matrix_source_csv),
  paste0("Failed objects: ", failed_objects_csv),
  "",
  "Next step:",
  "05B_SAFETY_RISK_SCORING_AND_CONTRAST.R",
  "",
  "Journal-rigor note:",
  "Projection-associated molecular competence is based on transcriptomic expression of axon guidance, neurite maturation, and synaptic machinery genes. It is not evidence of real anatomical projection or functional integration."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("05A DA/A9/A10/projection competence scoring 运行结束\n")
cat("============================================================\n\n")

cat("Objects in manifest：", n_objects_total, "\n")
cat("Objects scored successfully：", n_objects_success, "\n")
cat("Objects failed：", n_objects_failed, "\n")
cat("Cell-level score rows：", nrow(cell_df), "\n")
cat("Group-level score rows：", nrow(group_df), "\n")
cat("Object-level score rows：", nrow(object_df), "\n")
cat("Candidate group rows：", nrow(candidate_df), "\n\n")

cat("输出文件：\n")
cat(signature_gene_sets_csv, "\n")
cat(cell_level_scores_csv, "\n")
cat(group_level_scores_csv, "\n")
cat(object_level_scores_csv, "\n")
cat(candidate_groups_csv, "\n")
cat(matrix_source_csv, "\n")
cat(failed_objects_csv, "\n")
cat(report_txt, "\n\n")

if (n_objects_failed == 0L) {
  cat("✅ 05A DA/A9/A10/projection competence scoring 完成。\n")
  cat("下一步进入 05B：safety-risk scoring and contrast。\n")
} else {
  cat("⚠️ 05A 完成，但有对象失败。请查看 05A_failed_objects.csv。\n")
}

PROJECT_DIR <- "D:/PD_Graft_Project"

cat("\n============================================================\n")
cat("05A V2：final audit and failure record\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 05A V2。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_object_scores <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_group_scores <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_candidate_groups <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_DA_A9_A10_projection_candidate_groups.csv")
input_cell_scores <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_cell_level_scores.csv")
input_old_failed <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_failed_objects.csv")

out_dir <- file.path(tables_dir, "05A_DA_projection_scoring")

failed_objects_csv <- input_old_failed
final_audit_csv <- file.path(out_dir, "05A_V2_final_audit_summary.csv")
missing_objects_csv <- file.path(out_dir, "05A_V2_missing_unscored_objects.csv")
report_txt <- file.path(reports_dir, "05A_V2_final_audit_and_failure_record_report.txt")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

classify_missing_reason <- function(dataset, object_id, read_message = NA_character_) {
  if (dataset == "GSE157783") {
    return("reference_gene_symbol_incompatible_or_no_signature_overlap")
  }

  if (!is.na(read_message) && grepl("ReadItem|不存在0|新的R版本|read", read_message, ignore.case = TRUE)) {
    return("annotated_rds_read_error_or_version_incompatibility")
  }

  "unscored_unknown_reason"
}

stamp("读取 04D manifest 和 05A outputs。")

manifest <- read_csv_required(input_manifest)
object_scores <- read_csv_required(input_object_scores)
group_scores <- read_csv_optional(input_group_scores)
candidate_groups <- read_csv_optional(input_candidate_groups)
cell_scores <- read_csv_optional(input_cell_scores)
old_failed <- read_csv_optional(input_old_failed)

if (!all(c("dataset", "object_id", "annotated_rds") %in% colnames(manifest))) {
  stop("04D annotated manifest 缺少 dataset/object_id/annotated_rds。")
}

if (!all(c("dataset", "object_id") %in% colnames(object_scores))) {
  stop("05A object-level scores 缺少 dataset/object_id。")
}

manifest$file_exists <- file.exists(manifest$annotated_rds)

manifest_valid <- manifest[manifest$file_exists, , drop = FALSE]

main_keys <- paste(manifest_valid$dataset, manifest_valid$object_id, sep = "||")
scored_keys <- unique(paste(object_scores$dataset, object_scores$object_id, sep = "||"))

missing_keys <- setdiff(main_keys, scored_keys)

missing_rows <- manifest_valid[main_keys %in% missing_keys, , drop = FALSE]

stamp("04D annotated objects existing：", nrow(manifest_valid))
stamp("05A scored objects：", length(scored_keys))
stamp("05A missing / unscored objects：", length(missing_keys))

missing_records <- list()

if (nrow(missing_rows) > 0L) {
  for (i in seq_len(nrow(missing_rows))) {
    ds <- missing_rows$dataset[[i]]
    oid <- missing_rows$object_id[[i]]
    path <- missing_rows$annotated_rds[[i]]

    stamp("检查 missing object ", i, " / ", nrow(missing_rows), "：", ds, " :: ", oid)

    read_message <- NA_character_
    can_read <- FALSE
    n_cells <- NA_integer_
    n_features <- NA_integer_

    tryCatch({
      obj <- readRDS(path)
      can_read <- TRUE
      n_cells <- tryCatch(ncol(obj), error = function(e) NA_integer_)
      n_features <- tryCatch(nrow(obj), error = function(e) NA_integer_)
      rm(obj)
      gc(verbose = FALSE)
    }, error = function(e) {
      read_message <<- conditionMessage(e)
      can_read <<- FALSE
    })

    reason <- classify_missing_reason(ds, oid, read_message)

    missing_records[[length(missing_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      annotated_rds = path,
      file_exists = file.exists(path),
      can_read_rds = can_read,
      n_cells_if_readable = n_cells,
      n_features_if_readable = n_features,
      stage = "05A_scoring",
      missing_reason = reason,
      message = ifelse(is.na(read_message), "Object readable but no 05A score generated; likely no signature gene overlap.", read_message),
      blocking_for_05B = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

missing_df <- if (length(missing_records) > 0L) {
  rbindlist(missing_records, fill = TRUE)
} else {
  data.frame(
    dataset = character(),
    object_id = character(),
    annotated_rds = character(),
    file_exists = logical(),
    can_read_rds = logical(),
    n_cells_if_readable = integer(),
    n_features_if_readable = integer(),
    stage = character(),
    missing_reason = character(),
    message = character(),
    blocking_for_05B = logical()
  )
}

atomic_write_csv(as.data.frame(missing_df), missing_objects_csv)

failed_new <- if (nrow(missing_df) > 0L) {
  data.frame(
    dataset = missing_df$dataset,
    object_id = missing_df$object_id,
    annotated_rds = missing_df$annotated_rds,
    stage = "05A_final_audit",
    message = paste0(missing_df$missing_reason, " | ", missing_df$message),
    blocking_for_05B = missing_df$blocking_for_05B,
    stringsAsFactors = FALSE
  )
} else {
  data.frame(
    dataset = character(),
    object_id = character(),
    annotated_rds = character(),
    stage = character(),
    message = character(),
    blocking_for_05B = logical()
  )
}

atomic_write_csv(failed_new, failed_objects_csv)

n_manifest <- nrow(manifest_valid)
n_scored <- length(scored_keys)
n_missing <- length(missing_keys)
n_group_rows <- nrow(group_scores)
n_candidate_rows <- nrow(candidate_groups)
n_cell_rows <- nrow(cell_scores)

candidate_dt <- as.data.table(candidate_groups)

if (nrow(candidate_dt) > 0 && all(c("dataset", "candidate_class_05A") %in% colnames(candidate_dt))) {
  candidate_summary <- candidate_dt[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells, na.rm = TRUE)
    ),
    by = .(dataset, candidate_class_05A)
  ][order(dataset, -n_groups)]
} else {
  candidate_summary <- data.table()
}

audit_df <- data.frame(
  metric = c(
    "annotated_manifest_existing_objects",
    "05A_scored_objects",
    "05A_unscored_objects",
    "05A_cell_level_score_rows",
    "05A_group_level_score_rows",
    "05A_object_level_score_rows",
    "05A_candidate_group_rows",
    "blocking_failures_for_05B"
  ),
  value = c(
    n_manifest,
    n_scored,
    n_missing,
    n_cell_rows,
    n_group_rows,
    nrow(object_scores),
    n_candidate_rows,
    ifelse(nrow(failed_new) > 0, sum(failed_new$blocking_for_05B, na.rm = TRUE), 0)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(audit_df, final_audit_csv)

missing_lines <- if (nrow(missing_df) > 0L) {
  apply(as.data.frame(missing_df), 1, function(x) {
    paste0(
      x[["dataset"]],
      " :: ",
      x[["object_id"]],
      " — ",
      x[["missing_reason"]],
      " — ",
      x[["message"]]
    )
  })
} else {
  "none"
}

candidate_lines <- if (nrow(candidate_summary) > 0L) {
  apply(as.data.frame(candidate_summary), 1, function(x) {
    paste0(
      x[["dataset"]],
      " / ",
      x[["candidate_class_05A"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

report_lines <- c(
  "05A V2 final audit and failure record report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Audit summary:",
  paste0("Annotated manifest existing objects: ", n_manifest),
  paste0("05A scored objects: ", n_scored),
  paste0("05A unscored objects: ", n_missing),
  paste0("Cell-level score rows: ", n_cell_rows),
  paste0("Group-level score rows: ", n_group_rows),
  paste0("Object-level score rows: ", nrow(object_scores)),
  paste0("Candidate group rows: ", n_candidate_rows),
  "",
  "Unscored objects:",
  missing_lines,
  "",
  "Candidate group summary:",
  candidate_lines,
  "",
  "Output files:",
  paste0("Final audit summary: ", final_audit_csv),
  paste0("Missing/unscored objects: ", missing_objects_csv),
  paste0("Updated failed objects: ", failed_objects_csv),
  "",
  "Decision:",
  "05A is acceptable for downstream 05B because unscored objects are recorded and marked non-blocking. Core graft/DA objects have score outputs.",
  "",
  "Journal-rigor note:",
  "Unscored reference/auxiliary or unreadable objects are documented. Downstream biological claims should be based on successfully scored objects and should report object-level coverage."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("05A V2 final audit and failure record 运行结束\n")
cat("============================================================\n\n")

cat("Annotated manifest existing objects：", n_manifest, "\n")
cat("05A scored objects：", n_scored, "\n")
cat("05A unscored objects：", n_missing, "\n")
cat("Cell-level score rows：", n_cell_rows, "\n")
cat("Group-level score rows：", n_group_rows, "\n")
cat("Object-level score rows：", nrow(object_scores), "\n")
cat("Candidate group rows：", n_candidate_rows, "\n")
cat("Blocking failures for 05B：", ifelse(nrow(failed_new) > 0, sum(failed_new$blocking_for_05B, na.rm = TRUE), 0), "\n\n")

cat("输出文件：\n")
cat(final_audit_csv, "\n")
cat(missing_objects_csv, "\n")
cat(failed_objects_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 05A V2 final audit and failure record 完成。\n")
cat("05A 已可接受进入 05B；注意 downstream claims 只基于 successfully scored objects。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

SAFETY_LOW_MAX <- 0.20
SAFETY_HIGH_MIN <- 0.35

DA_HIGH_MIN <- 0.08
PROJECTION_HIGH_MIN <- 0.08

DA_PRESENT_MIN <- 0.05
PROJECTION_PRESENT_MIN <- 0.05

TOP_N_PER_DATASET_CLASS <- 50

cat("\n============================================================\n")
cat("05B：safety-risk scoring and contrast\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 05B。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_04B_group <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_category_scores.csv")
input_04B_object <- file.path(tables_dir, "04B_marker_expression", "04B_object_marker_category_scores.csv")
input_04D_group <- file.path(tables_dir, "04D_annotations", "04D_group_annotation_table.csv")
input_05A_group <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_05A_object <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_05A_audit <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_final_audit_summary.csv")

out_tables_dir <- file.path(tables_dir, "05B_safety_risk_scoring")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

safety_signature_csv <- file.path(out_tables_dir, "05B_safety_signature_definition.csv")
group_safety_csv <- file.path(out_tables_dir, "05B_group_safety_risk_scores.csv")
object_safety_csv <- file.path(out_tables_dir, "05B_object_safety_risk_scores.csv")
dataset_safety_csv <- file.path(out_tables_dir, "05B_dataset_safety_risk_summary.csv")
contrast_groups_csv <- file.path(out_tables_dir, "05B_DA_projection_vs_safety_contrast_groups.csv")
candidate_story_csv <- file.path(out_tables_dir, "05B_candidate_groups_for_story.csv")
qc_audit_csv <- file.path(out_tables_dir, "05B_QC_audit_summary.csv")
report_txt <- file.path(reports_dir, "05B_safety_risk_scoring_and_contrast_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

ensure_col <- function(dt, col, value = NA_real_) {
  if (!col %in% colnames(dt)) dt[[col]] <- value
  dt
}

to_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

wide_category_scores <- function(group_dt) {
  dcast(
    group_dt,
    dataset + object_id + group_source + group_id + n_cells ~ category,
    value.var = "mean_score",
    fun.aggregate = max,
    fill = NA_real_
  )
}

wide_category_pct <- function(group_dt) {
  dcast(
    group_dt,
    dataset + object_id + group_source + group_id + n_cells ~ category,
    value.var = "pct_cells_score_gt0",
    fun.aggregate = max,
    fill = NA_real_
  )
}

classify_safety_contrast <- function(dt) {

  dt[
    ,
    safety_contrast_class_05B := fifelse(
      DA_like_composite_score >= DA_HIGH_MIN &
        projection_competence_composite_score >= PROJECTION_HIGH_MIN &
        safety_risk_composite_05B <= SAFETY_LOW_MAX,
      "ideal_DA_projection_high_safety_low",
      fifelse(
        safety_risk_composite_05B >= SAFETY_HIGH_MIN &
          DA_like_composite_score < DA_PRESENT_MIN,
        "high_safety_risk_low_DA",
        fifelse(
          safety_risk_composite_05B >= SAFETY_HIGH_MIN &
            DA_like_composite_score >= DA_PRESENT_MIN,
          "mixed_DA_or_projection_with_safety_risk",
          fifelse(
            projection_competence_composite_score >= PROJECTION_PRESENT_MIN &
              DA_like_composite_score < DA_PRESENT_MIN &
              safety_risk_composite_05B <= SAFETY_LOW_MAX,
            "projection_competence_without_DA_low_safety",
            "lower_priority_or_mixed"
          )
        )
      )
    )
  ]

  dt
}

stamp("读取 04B / 04D / 05A 输出。")

g04B <- as.data.table(read_csv_required(input_04B_group))
o04B <- as.data.table(read_csv_required(input_04B_object))
g04D <- as.data.table(read_csv_required(input_04D_group))
g05A <- as.data.table(read_csv_required(input_05A_group))
o05A <- as.data.table(read_csv_required(input_05A_object))
audit05A <- as.data.table(read_csv_optional(input_05A_audit))

needed_04B <- c("dataset", "object_id", "group_source", "group_id", "category", "mean_score", "pct_cells_score_gt0", "coverage_fraction")
if (!all(needed_04B %in% colnames(g04B))) {
  stop("04B group table 缺少必要列：", paste(setdiff(needed_04B, colnames(g04B)), collapse = ", "))
}

needed_05A <- c("dataset", "object_id", "annotation_04D_group_id", "annotation_04D_v1", "DA_like_composite_score", "projection_competence_composite_score", "A9_minus_A10_score_05A")
if (!all(needed_05A %in% colnames(g05A))) {
  stop("05A group table 缺少必要列：", paste(setdiff(needed_05A, colnames(g05A)), collapse = ", "))
}

stamp("定义 safety-risk score。")

safety_def <- data.frame(
  component = c(
    "cell_cycle_proliferation",
    "progenitor_neuroepithelial",
    "pluripotency_immature_risk",
    "stress_apoptosis_response",
    "extracellular_matrix_fibroblast",
    "vascular_pericyte_meningeal"
  ),
  weight = c(
    1.20,
    1.00,
    1.40,
    0.60,
    0.40,
    0.30
  ),
  interpretation = c(
    "Cycling/proliferating state; major safety-risk-associated signal.",
    "Neural progenitor/immature state; developmental fate propensity.",
    "Pluripotency/immature-risk marker signal; high-priority safety review.",
    "Stress/apoptosis response; not a cell type but can confound graft quality.",
    "ECM/fibroblast-like or mesenchymal state; off-target/stromal risk signal.",
    "Vascular/pericyte/meningeal-associated marker signal; off-target/stromal context."
  ),
  manuscript_caution = c(
    "Not direct tumorigenicity proof.",
    "Not automatically unsafe without proliferation/pluripotency.",
    "Requires strict manual marker validation.",
    "Stress signal should not be interpreted as lineage alone.",
    "Context-dependent; may reflect host/stromal cells.",
    "Context-dependent; may reflect host/stromal cells."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(safety_def, safety_signature_csv)

safety_components <- safety_def$component
weights <- safety_def$weight
names(weights) <- safety_def$component

stamp("计算 group-level safety-risk score。")

g_wide <- as.data.table(wide_category_scores(g04B))
g_pct <- as.data.table(wide_category_pct(g04B))

for (comp in safety_components) {
  g_wide <- ensure_col(g_wide, comp, NA_real_)
  g_pct <- ensure_col(g_pct, comp, NA_real_)
}

score_mat <- as.matrix(g_wide[, safety_components, with = FALSE])
score_mat <- apply(score_mat, 2, to_numeric_safe)

weighted_score <- rep(NA_real_, nrow(g_wide))

for (i in seq_len(nrow(g_wide))) {
  vals <- as.numeric(score_mat[i, ])
  valid <- !is.na(vals)

  if (sum(valid) == 0L) {
    weighted_score[i] <- NA_real_
  } else {
    weighted_score[i] <- sum(vals[valid] * weights[valid]) / sum(weights[valid])
  }
}

g_wide[, safety_risk_composite_05B := weighted_score]

g_wide[, safety_cell_cycle_score_05B := to_numeric_safe(cell_cycle_proliferation)]
g_wide[, safety_progenitor_score_05B := to_numeric_safe(progenitor_neuroepithelial)]
g_wide[, safety_pluripotency_score_05B := to_numeric_safe(pluripotency_immature_risk)]
g_wide[, safety_stress_score_05B := to_numeric_safe(stress_apoptosis_response)]
g_wide[, safety_ecm_score_05B := to_numeric_safe(extracellular_matrix_fibroblast)]
g_wide[, safety_vascular_score_05B := to_numeric_safe(vascular_pericyte_meningeal)]

component_values <- g_wide[, safety_components, with = FALSE]
dominant_component <- apply(component_values, 1, function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(NA_character_)
  safety_components[which.max(replace(x, is.na(x), -Inf))]
})

g_wide[, dominant_safety_component_05B := dominant_component]

g_wide[
  ,
  safety_risk_label_05B := fifelse(
    is.na(safety_risk_composite_05B),
    "safety_score_unavailable",
    fifelse(
      safety_risk_composite_05B >= SAFETY_HIGH_MIN,
      "high_safety_risk_associated_state",
      fifelse(
        safety_risk_composite_05B <= SAFETY_LOW_MAX,
        "low_safety_risk_signal_state",
        "intermediate_safety_risk_signal_state"
      )
    )
  )
]

group_safety_cols <- c(
  "dataset", "object_id", "group_source", "group_id", "n_cells",
  "safety_risk_composite_05B",
  "safety_risk_label_05B",
  "dominant_safety_component_05B",
  "safety_cell_cycle_score_05B",
  "safety_progenitor_score_05B",
  "safety_pluripotency_score_05B",
  "safety_stress_score_05B",
  "safety_ecm_score_05B",
  "safety_vascular_score_05B"
)

group_safety <- g_wide[, group_safety_cols, with = FALSE]

atomic_write_csv(as.data.frame(group_safety), group_safety_csv)

stamp("合并 05A DA/projection scores，生成 contrast table。")

g05A2 <- copy(g05A)
g05A2[, group_id := as.character(annotation_04D_group_id)]

g05A2[is.na(group_id) | group_id == "", group_id := "object_all"]

contrast <- merge(
  group_safety,
  g05A2,
  by = c("dataset", "object_id", "group_id"),
  all.x = TRUE,
  suffixes = c("_05B", "_05A")
)

needed_contrast_numeric <- c(
  "DA_like_composite_score",
  "projection_competence_composite_score",
  "DA_projection_competence_composite_score",
  "A9_minus_A10_score_05A"
)

for (col in needed_contrast_numeric) {
  if (!col %in% colnames(contrast)) contrast[[col]] <- NA_real_
}

contrast <- classify_safety_contrast(as.data.table(contrast))

contrast[
  ,
  A9_A10_bias_label_05B := fifelse(
    is.na(A9_minus_A10_score_05A),
    "unknown",
    fifelse(
      A9_minus_A10_score_05A > 0.02,
      "A9_like_bias",
      fifelse(
        A9_minus_A10_score_05A < -0.02,
        "A10_like_bias",
        "A9_A10_mixed_or_unclear"
      )
    )
  )
]

contrast[
  ,
  story_priority_05B := fifelse(
    safety_contrast_class_05B == "ideal_DA_projection_high_safety_low",
    "high_priority_positive_graft_like",
    fifelse(
      safety_contrast_class_05B == "high_safety_risk_low_DA",
      "high_priority_safety_risk",
      fifelse(
        safety_contrast_class_05B == "mixed_DA_or_projection_with_safety_risk",
        "high_priority_mixed_warning",
        "standard_or_low_priority"
      )
    )
  )
]

contrast <- contrast[
  order(
    dataset,
    -fifelse(is.na(DA_projection_competence_composite_score), -Inf, DA_projection_competence_composite_score),
    -fifelse(is.na(safety_risk_composite_05B), -Inf, safety_risk_composite_05B)
  )
]

atomic_write_csv(as.data.frame(contrast), contrast_groups_csv)

stamp("计算 object-level safety-risk score。")

object_safety <- group_safety[
  ,
  .(
    n_groups = .N,
    total_cells_represented = sum(n_cells, na.rm = TRUE),
    mean_safety_risk_composite_05B = weighted.mean(safety_risk_composite_05B, w = pmax(n_cells, 1), na.rm = TRUE),
    median_safety_risk_composite_05B = median(safety_risk_composite_05B, na.rm = TRUE),
    max_safety_risk_composite_05B = max(safety_risk_composite_05B, na.rm = TRUE),
    n_high_safety_groups = sum(safety_risk_label_05B == "high_safety_risk_associated_state", na.rm = TRUE),
    n_low_safety_groups = sum(safety_risk_label_05B == "low_safety_risk_signal_state", na.rm = TRUE),
    dominant_safety_component_object = names(sort(table(dominant_safety_component_05B), decreasing = TRUE))[1]
  ),
  by = .(dataset, object_id)
]

if (nrow(o05A) > 0 && all(c("dataset", "object_id") %in% colnames(o05A))) {
  keep_05A_cols <- intersect(
    c(
      "dataset", "object_id", "n_cells",
      "DA_like_composite_score",
      "projection_competence_composite_score",
      "DA_projection_competence_composite_score",
      "A9_minus_A10_score_05A",
      "dominant_annotation"
    ),
    colnames(o05A)
  )

  object_safety <- merge(
    object_safety,
    o05A[, keep_05A_cols, with = FALSE],
    by = c("dataset", "object_id"),
    all.x = TRUE
  )
}

object_safety[
  ,
  object_safety_contrast_class_05B := fifelse(
    mean_safety_risk_composite_05B <= SAFETY_LOW_MAX &
      DA_projection_competence_composite_score >= DA_HIGH_MIN,
    "object_level_DA_projection_high_safety_low",
    fifelse(
      mean_safety_risk_composite_05B >= SAFETY_HIGH_MIN,
      "object_level_high_safety_risk",
      "object_level_intermediate_or_mixed"
    )
  )
]

atomic_write_csv(as.data.frame(object_safety), object_safety_csv)

stamp("计算 dataset-level safety-risk summary。")

dataset_safety <- object_safety[
  ,
  .(
    n_objects = .N,
    total_cells_represented = sum(total_cells_represented, na.rm = TRUE),
    mean_safety_risk_composite_05B = weighted.mean(mean_safety_risk_composite_05B, w = pmax(total_cells_represented, 1), na.rm = TRUE),
    median_object_safety_risk_05B = median(mean_safety_risk_composite_05B, na.rm = TRUE),
    max_object_safety_risk_05B = max(max_safety_risk_composite_05B, na.rm = TRUE),
    total_high_safety_groups = sum(n_high_safety_groups, na.rm = TRUE),
    total_low_safety_groups = sum(n_low_safety_groups, na.rm = TRUE),
    mean_DA_projection_competence = mean(DA_projection_competence_composite_score, na.rm = TRUE),
    mean_DA_like = mean(DA_like_composite_score, na.rm = TRUE),
    mean_projection_competence = mean(projection_competence_composite_score, na.rm = TRUE)
  ),
  by = dataset
][order(-mean_DA_projection_competence, mean_safety_risk_composite_05B)]

dataset_safety[
  ,
  dataset_story_class_05B := fifelse(
    mean_DA_projection_competence >= DA_HIGH_MIN &
      mean_safety_risk_composite_05B <= SAFETY_LOW_MAX,
    "dataset_with_favorable_DA_projection_vs_safety_profile",
    fifelse(
      mean_safety_risk_composite_05B >= SAFETY_HIGH_MIN,
      "dataset_with_high_safety_risk_signal",
      "dataset_with_intermediate_or_heterogeneous_profile"
    )
  )
]

atomic_write_csv(as.data.frame(dataset_safety), dataset_safety_csv)

stamp("提取 story candidate groups。")

candidate_story <- contrast[
  story_priority_05B != "standard_or_low_priority"
]

candidate_story <- candidate_story[
  ,
  head(.SD, TOP_N_PER_DATASET_CLASS),
  by = .(dataset, safety_contrast_class_05B)
]

candidate_story <- candidate_story[
  order(
    story_priority_05B,
    dataset,
    -DA_projection_competence_composite_score,
    -safety_risk_composite_05B
  )
]

atomic_write_csv(as.data.frame(candidate_story), candidate_story_csv)

qc_audit <- data.frame(
  metric = c(
    "04B_group_rows",
    "05A_group_rows",
    "05B_group_safety_rows",
    "05B_contrast_group_rows",
    "05B_object_rows",
    "05B_dataset_rows",
    "05B_story_candidate_rows",
    "05A_blocking_failures_for_05B"
  ),
  value = c(
    nrow(g04B),
    nrow(g05A),
    nrow(group_safety),
    nrow(contrast),
    nrow(object_safety),
    nrow(dataset_safety),
    nrow(candidate_story),
    ifelse(nrow(audit05A) > 0 && "metric" %in% colnames(audit05A), {
      val <- audit05A$value[audit05A$metric == "blocking_failures_for_05B"]
      ifelse(length(val) == 0, NA, val[1])
    }, NA)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)

dataset_lines <- if (nrow(dataset_safety) > 0) {
  apply(as.data.frame(dataset_safety), 1, function(x) {
    paste0(
      x[["dataset"]],
      ": safety=",
      round(as.numeric(x[["mean_safety_risk_composite_05B"]]), 4),
      "; DA_projection=",
      round(as.numeric(x[["mean_DA_projection_competence"]]), 4),
      "; class=",
      x[["dataset_story_class_05B"]]
    )
  })
} else {
  "none"
}

candidate_summary <- if (nrow(candidate_story) > 0) {
  candidate_story[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells_05B, na.rm = TRUE)
    ),
    by = .(dataset, safety_contrast_class_05B)
  ][order(dataset, safety_contrast_class_05B)]
} else {
  data.table()
}

candidate_lines <- if (nrow(candidate_summary) > 0) {
  apply(as.data.frame(candidate_summary), 1, function(x) {
    paste0(
      x[["dataset"]],
      " / ",
      x[["safety_contrast_class_05B"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

report_lines <- c(
  "05B safety-risk scoring and contrast report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Group safety rows: ", nrow(group_safety)),
  paste0("Contrast group rows: ", nrow(contrast)),
  paste0("Object safety rows: ", nrow(object_safety)),
  paste0("Dataset summary rows: ", nrow(dataset_safety)),
  paste0("Story candidate rows: ", nrow(candidate_story)),
  "",
  "Dataset-level summary:",
  dataset_lines,
  "",
  "Candidate story group summary:",
  candidate_lines,
  "",
  "Output files:",
  paste0("Safety signature definition: ", safety_signature_csv),
  paste0("Group safety-risk scores: ", group_safety_csv),
  paste0("Object safety-risk scores: ", object_safety_csv),
  paste0("Dataset safety-risk summary: ", dataset_safety_csv),
  paste0("DA/projection vs safety contrast groups: ", contrast_groups_csv),
  paste0("Candidate story groups: ", candidate_story_csv),
  paste0("QC audit summary: ", qc_audit_csv),
  "",
  "Next step:",
  "06A_FIGURE_TABLE_PREP_DA_PROJECTION_SAFETY.R",
  "",
  "Journal-rigor note:",
  "Safety-risk score is a transcriptional risk-associated state score based on proliferation, progenitor, pluripotency/immature, stress, ECM and vascular/mesenchymal signals. It is not direct proof of tumorigenicity or clinical safety."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("05B safety-risk scoring and contrast 运行结束\n")
cat("============================================================\n\n")

cat("Group safety rows：", nrow(group_safety), "\n")
cat("Contrast group rows：", nrow(contrast), "\n")
cat("Object safety rows：", nrow(object_safety), "\n")
cat("Dataset summary rows：", nrow(dataset_safety), "\n")
cat("Story candidate rows：", nrow(candidate_story), "\n\n")

cat("输出文件：\n")
cat(safety_signature_csv, "\n")
cat(group_safety_csv, "\n")
cat(object_safety_csv, "\n")
cat(dataset_safety_csv, "\n")
cat(contrast_groups_csv, "\n")
cat(candidate_story_csv, "\n")
cat(qc_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 05B safety-risk scoring and contrast 完成。\n")
cat("下一步进入 06A：整理 DA/projection/safety 的论文图表输入。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

MAKE_FIGURES <- TRUE
TOP_N_STORY_GROUPS <- 100

cat("\n============================================================\n")
cat("06A：figure/table preparation for DA/projection/safety story\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06A。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

if (MAKE_FIGURES && !has_ggplot2) {
  warning("未检测到 ggplot2；06A 会输出表格，但跳过图。")
  MAKE_FIGURES <- FALSE
}

if (MAKE_FIGURES) {
  suppressPackageStartupMessages(library(ggplot2))
}

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_05A_group <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_05A_object <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_05A_candidates <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_DA_A9_A10_projection_candidate_groups.csv")
input_05A_audit <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_final_audit_summary.csv")

input_05B_group <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_group_safety_risk_scores.csv")
input_05B_object <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_object_safety_risk_scores.csv")
input_05B_dataset <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_dataset_safety_risk_summary.csv")
input_05B_contrast <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")
input_05B_story <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_candidate_groups_for_story.csv")
input_05B_qc <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_QC_audit_summary.csv")

out_tables_dir <- file.path(tables_dir, "06A_figure_table_prep")
out_figures_dir <- file.path(figures_dir, "06A_figure_table_prep")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

dataset_overview_csv <- file.path(out_tables_dir, "06A_dataset_overview_DA_projection_safety.csv")
a9_a10_summary_csv <- file.path(out_tables_dir, "06A_A9_A10_bias_summary_by_dataset.csv")
candidate_class_summary_csv <- file.path(out_tables_dir, "06A_candidate_class_summary_by_dataset.csv")
story_groups_csv <- file.path(out_tables_dir, "06A_top_story_candidate_groups.csv")
object_summary_csv <- file.path(out_tables_dir, "06A_object_level_DA_projection_safety_summary.csv")
manuscript_numbers_csv <- file.path(out_tables_dir, "06A_manuscript_key_numbers.csv")
qc_summary_csv <- file.path(out_tables_dir, "06A_QC_summary_for_methods.csv")
report_txt <- file.path(reports_dir, "06A_figure_table_preparation_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

save_plot_both <- function(p, filename_base, width = 7, height = 5) {
  if (!MAKE_FIGURES) return(invisible(NULL))

  pdf_path <- file.path(out_figures_dir, paste0(filename_base, ".pdf"))
  png_path <- file.path(out_figures_dir, paste0(filename_base, ".png"))

  tryCatch({
    ggplot2::ggsave(pdf_path, p, width = width, height = height, limitsize = FALSE)
    ggplot2::ggsave(png_path, p, width = width, height = height, dpi = 300, limitsize = FALSE)
  }, error = function(e) {
    warning("保存图失败：", filename_base, "；", conditionMessage(e))
  })

  invisible(NULL)
}

num <- function(x) suppressWarnings(as.numeric(x))

stamp("读取 05A / 05B 输出。")

g05A <- as.data.table(read_csv_required(input_05A_group))
o05A <- as.data.table(read_csv_required(input_05A_object))
c05A <- as.data.table(read_csv_optional(input_05A_candidates))
audit05A <- as.data.table(read_csv_optional(input_05A_audit))

g05B <- as.data.table(read_csv_required(input_05B_group))
o05B <- as.data.table(read_csv_required(input_05B_object))
d05B <- as.data.table(read_csv_required(input_05B_dataset))
contrast <- as.data.table(read_csv_required(input_05B_contrast))
story <- as.data.table(read_csv_required(input_05B_story))
qc05B <- as.data.table(read_csv_optional(input_05B_qc))

stamp("整理 dataset-level overview。")

dataset_overview <- copy(d05B)

needed_dataset_cols <- c(
  "dataset",
  "n_objects",
  "total_cells_represented",
  "mean_safety_risk_composite_05B",
  "mean_DA_projection_competence",
  "mean_DA_like",
  "mean_projection_competence",
  "dataset_story_class_05B"
)

for (col in needed_dataset_cols) {
  if (!col %in% colnames(dataset_overview)) dataset_overview[[col]] <- NA
}

dataset_overview <- dataset_overview[
  ,
  needed_dataset_cols,
  with = FALSE
]

dataset_overview[
  ,
  favorable_index_06A := num(mean_DA_projection_competence) - num(mean_safety_risk_composite_05B)
]

dataset_overview <- dataset_overview[
  order(-favorable_index_06A)
]

atomic_write_csv(as.data.frame(dataset_overview), dataset_overview_csv)

stamp("整理 A9/A10 bias summary。")

if ("A9_A10_bias_label_05B" %in% colnames(contrast)) {
  a9_a10_summary <- contrast[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells_05B, na.rm = TRUE),
      median_A9_minus_A10 = median(A9_minus_A10_score_05A, na.rm = TRUE),
      median_DA_like = median(DA_like_composite_score, na.rm = TRUE),
      median_projection_competence = median(projection_competence_composite_score, na.rm = TRUE),
      median_safety_risk = median(safety_risk_composite_05B, na.rm = TRUE)
    ),
    by = .(dataset, A9_A10_bias_label_05B)
  ][order(dataset, A9_A10_bias_label_05B)]
} else {
  a9_a10_summary <- data.table()
}

atomic_write_csv(as.data.frame(a9_a10_summary), a9_a10_summary_csv)

stamp("整理 candidate class summary。")

if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  candidate_class_summary <- contrast[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells_05B, na.rm = TRUE),
      median_DA_projection = median(DA_projection_competence_composite_score, na.rm = TRUE),
      median_DA_like = median(DA_like_composite_score, na.rm = TRUE),
      median_projection_competence = median(projection_competence_composite_score, na.rm = TRUE),
      median_safety_risk = median(safety_risk_composite_05B, na.rm = TRUE),
      median_A9_minus_A10 = median(A9_minus_A10_score_05A, na.rm = TRUE)
    ),
    by = .(dataset, safety_contrast_class_05B)
  ][order(dataset, safety_contrast_class_05B)]
} else {
  candidate_class_summary <- data.table()
}

atomic_write_csv(as.data.frame(candidate_class_summary), candidate_class_summary_csv)

stamp("整理 top story candidate groups。")

story2 <- copy(story)

if (nrow(story2) > 0) {

  story2[
    ,
    story_rank_score_06A := num(DA_projection_competence_composite_score) -
      num(safety_risk_composite_05B)
  ]

  story2[
    safety_contrast_class_05B %in% c("high_safety_risk_low_DA", "mixed_DA_or_projection_with_safety_risk"),
    story_rank_score_06A := num(safety_risk_composite_05B)
  ]

  story2 <- story2[
    order(dataset, safety_contrast_class_05B, -story_rank_score_06A)
  ]

  story2 <- story2[
    ,
    head(.SD, TOP_N_STORY_GROUPS),
    by = .(dataset, safety_contrast_class_05B)
  ]
}

atomic_write_csv(as.data.frame(story2), story_groups_csv)

stamp("整理 object-level summary。")

object_summary <- copy(o05B)

object_keep <- intersect(
  c(
    "dataset", "object_id",
    "total_cells_represented",
    "mean_safety_risk_composite_05B",
    "median_safety_risk_composite_05B",
    "max_safety_risk_composite_05B",
    "n_high_safety_groups",
    "n_low_safety_groups",
    "DA_like_composite_score",
    "projection_competence_composite_score",
    "DA_projection_competence_composite_score",
    "A9_minus_A10_score_05A",
    "object_safety_contrast_class_05B",
    "dominant_annotation"
  ),
  colnames(object_summary)
)

object_summary <- object_summary[, object_keep, with = FALSE]

atomic_write_csv(as.data.frame(object_summary), object_summary_csv)

stamp("生成 manuscript key numbers。")

n_scored_objects <- length(unique(paste(o05A$dataset, o05A$object_id, sep = "||")))
n_scored_cells <- if ("n_cells" %in% colnames(o05A)) sum(o05A$n_cells, na.rm = TRUE) else NA_real_
n_group_scores <- nrow(g05A)
n_safety_groups <- nrow(g05B)
n_contrast_groups <- nrow(contrast)
n_story_groups <- nrow(story2)

n_ideal_groups <- if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  sum(contrast$safety_contrast_class_05B == "ideal_DA_projection_high_safety_low", na.rm = TRUE)
} else NA_integer_

n_high_risk_groups <- if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  sum(contrast$safety_contrast_class_05B == "high_safety_risk_low_DA", na.rm = TRUE)
} else NA_integer_

n_mixed_groups <- if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  sum(contrast$safety_contrast_class_05B == "mixed_DA_or_projection_with_safety_risk", na.rm = TRUE)
} else NA_integer_

manuscript_numbers <- data.frame(
  metric = c(
    "successfully_scored_objects_for_05A_05B",
    "successfully_scored_cells_for_05A",
    "group_level_DA_projection_score_rows",
    "group_level_safety_score_rows",
    "DA_projection_vs_safety_contrast_groups",
    "story_candidate_groups",
    "ideal_DA_projection_high_safety_low_groups",
    "high_safety_risk_low_DA_groups",
    "mixed_DA_or_projection_with_safety_risk_groups",
    "datasets_in_05B_summary"
  ),
  value = c(
    n_scored_objects,
    n_scored_cells,
    n_group_scores,
    n_safety_groups,
    n_contrast_groups,
    n_story_groups,
    n_ideal_groups,
    n_high_risk_groups,
    n_mixed_groups,
    nrow(dataset_overview)
  ),
  interpretation = c(
    "Objects used for downstream DA/projection/safety scoring.",
    "Cells represented in 05A cell-level score table.",
    "Group-level DA/A9/A10/projection score rows.",
    "Group-level safety-risk score rows.",
    "Merged group-level contrast rows.",
    "High-priority groups selected for story review.",
    "Groups with high DA/projection competence and low safety-risk score.",
    "Groups with high safety-risk score and low DA signal.",
    "Groups with DA/projection signal and concurrent safety-risk signal.",
    "Datasets represented in dataset-level 05B summary."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(manuscript_numbers, manuscript_numbers_csv)

stamp("整理 QC summary。")

qc_list <- list()

if (nrow(audit05A) > 0) {
  audit05A$source <- "05A_V2"
  qc_list[[length(qc_list) + 1L]] <- audit05A
}

if (nrow(qc05B) > 0) {
  qc05B$source <- "05B"
  qc_list[[length(qc_list) + 1L]] <- qc05B
}

qc_summary <- if (length(qc_list) > 0) {
  rbindlist(qc_list, fill = TRUE)
} else {
  data.table()
}

atomic_write_csv(as.data.frame(qc_summary), qc_summary_csv)

if (MAKE_FIGURES) {
  stamp("生成 06A 快速检查图。")

  p1 <- ggplot(dataset_overview, aes(
    x = mean_safety_risk_composite_05B,
    y = mean_DA_projection_competence,
    label = dataset
  )) +
    geom_point(size = 3) +
    geom_text(vjust = -0.7, size = 3) +
    labs(
      title = "Dataset-level DA/projection competence vs safety-risk score",
      x = "Mean safety-risk-associated score",
      y = "Mean DA/projection competence score"
    ) +
    theme_classic(base_size = 12)

  save_plot_both(p1, "06A_dataset_DA_projection_vs_safety_scatter", width = 7, height = 5)

  p2 <- ggplot(dataset_overview, aes(
    x = reorder(dataset, mean_safety_risk_composite_05B),
    y = mean_safety_risk_composite_05B
  )) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Dataset-level safety-risk-associated score",
      x = "Dataset",
      y = "Mean safety-risk score"
    ) +
    theme_classic(base_size = 12)

  save_plot_both(p2, "06A_dataset_safety_risk_barplot", width = 7, height = 5)

  p3 <- ggplot(dataset_overview, aes(
    x = reorder(dataset, mean_DA_projection_competence),
    y = mean_DA_projection_competence
  )) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Dataset-level DA/projection competence score",
      x = "Dataset",
      y = "Mean DA/projection competence score"
    ) +
    theme_classic(base_size = 12)

  save_plot_both(p3, "06A_dataset_DA_projection_barplot", width = 7, height = 5)

  if (nrow(candidate_class_summary) > 0) {
    p4 <- ggplot(candidate_class_summary, aes(
      x = dataset,
      y = n_groups,
      fill = safety_contrast_class_05B
    )) +
      geom_col(position = "stack") +
      labs(
        title = "Candidate classes by dataset",
        x = "Dataset",
        y = "Number of groups",
        fill = "05B class"
      ) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    save_plot_both(p4, "06A_candidate_class_by_dataset_barplot", width = 9, height = 5)
  }

  if (nrow(a9_a10_summary) > 0) {
    p5 <- ggplot(a9_a10_summary, aes(
      x = dataset,
      y = n_groups,
      fill = A9_A10_bias_label_05B
    )) +
      geom_col(position = "stack") +
      labs(
        title = "A9/A10 molecular bias groups by dataset",
        x = "Dataset",
        y = "Number of groups",
        fill = "A9/A10 bias"
      ) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    save_plot_both(p5, "06A_A9_A10_bias_by_dataset_barplot", width = 9, height = 5)
  }
}

dataset_lines <- if (nrow(dataset_overview) > 0) {
  apply(as.data.frame(dataset_overview), 1, function(x) {
    paste0(
      x[["dataset"]],
      ": DA_projection=",
      round(as.numeric(x[["mean_DA_projection_competence"]]), 4),
      "; safety=",
      round(as.numeric(x[["mean_safety_risk_composite_05B"]]), 4),
      "; favorable_index=",
      round(as.numeric(x[["favorable_index_06A"]]), 4),
      "; class=",
      x[["dataset_story_class_05B"]]
    )
  })
} else {
  "none"
}

number_lines <- apply(manuscript_numbers, 1, function(x) {
  paste0(x[["metric"]], ": ", x[["value"]])
})

report_lines <- c(
  "06A figure/table preparation report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Manuscript key numbers:",
  number_lines,
  "",
  "Dataset overview:",
  dataset_lines,
  "",
  "Output tables:",
  paste0("Dataset overview: ", dataset_overview_csv),
  paste0("A9/A10 summary: ", a9_a10_summary_csv),
  paste0("Candidate class summary: ", candidate_class_summary_csv),
  paste0("Top story groups: ", story_groups_csv),
  paste0("Object summary: ", object_summary_csv),
  paste0("Manuscript key numbers: ", manuscript_numbers_csv),
  paste0("QC summary: ", qc_summary_csv),
  "",
  "Output figures:",
  out_figures_dir,
  "",
  "Next step:",
  "06B_PUBLICATION_FIGURE_DRAFTS.R",
  "",
  "Journal-rigor note:",
  "06A figures are quick inspection drafts. Final manuscript claims should avoid saying real projection or proven safety; use projection-associated molecular competence and safety-risk-associated transcriptional state."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("06A figure/table preparation 运行结束\n")
cat("============================================================\n\n")

cat("Dataset overview rows：", nrow(dataset_overview), "\n")
cat("A9/A10 summary rows：", nrow(a9_a10_summary), "\n")
cat("Candidate class summary rows：", nrow(candidate_class_summary), "\n")
cat("Top story groups rows：", nrow(story2), "\n")
cat("Object summary rows：", nrow(object_summary), "\n")
cat("Manuscript key numbers rows：", nrow(manuscript_numbers), "\n\n")

cat("输出表格：\n")
cat(dataset_overview_csv, "\n")
cat(a9_a10_summary_csv, "\n")
cat(candidate_class_summary_csv, "\n")
cat(story_groups_csv, "\n")
cat(object_summary_csv, "\n")
cat(manuscript_numbers_csv, "\n")
cat(qc_summary_csv, "\n\n")

if (MAKE_FIGURES) {
  cat("输出图片目录：\n")
  cat(out_figures_dir, "\n\n")
}

cat("✅ 06A figure/table preparation 完成。\n")
cat("下一步进入 06B：publication figure drafts。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

TOP_N_GROUPS_FOR_TILE <- 35

SAVE_PDF <- TRUE
SAVE_PNG <- TRUE

cat("\n============================================================\n")
cat("06B V2：publication figure polish\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06B V2。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_dataset <- file.path(tables_dir, "06A_figure_table_prep", "06A_dataset_overview_DA_projection_safety.csv")
input_a9a10 <- file.path(tables_dir, "06A_figure_table_prep", "06A_A9_A10_bias_summary_by_dataset.csv")
input_class <- file.path(tables_dir, "06A_figure_table_prep", "06A_candidate_class_summary_by_dataset.csv")
input_story <- file.path(tables_dir, "06A_figure_table_prep", "06A_top_story_candidate_groups.csv")
input_numbers <- file.path(tables_dir, "06A_figure_table_prep", "06A_manuscript_key_numbers.csv")

out_tables_dir <- file.path(tables_dir, "06B_publication_figure_drafts_V2")
out_figures_dir <- file.path(figures_dir, "06B_publication_figure_drafts_V2")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

figure_index_csv <- file.path(out_tables_dir, "06B_V2_figure_index.csv")
dataset_plot_table_csv <- file.path(out_tables_dir, "06B_V2_dataset_plot_table.csv")
candidate_class_prop_csv <- file.path(out_tables_dir, "06B_V2_candidate_class_proportion_table.csv")
a9a10_prop_csv <- file.path(out_tables_dir, "06B_V2_A9_A10_proportion_table.csv")
top_tile_table_csv <- file.path(out_tables_dir, "06B_V2_top_story_group_tile_table.csv")
report_txt <- file.path(reports_dir, "06B_V2_publication_figure_polish_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

save_plot <- function(p, filename_base, width = 7, height = 5) {
  pdf_path <- file.path(out_figures_dir, paste0(filename_base, ".pdf"))
  png_path <- file.path(out_figures_dir, paste0(filename_base, ".png"))

  if (SAVE_PDF) {
    ggsave(pdf_path, p, width = width, height = height, limitsize = FALSE)
  }

  if (SAVE_PNG) {
    ggsave(png_path, p, width = width, height = height, dpi = 300, limitsize = FALSE)
  }

  data.frame(
    figure_id = filename_base,
    pdf_path = ifelse(SAVE_PDF, pdf_path, NA_character_),
    png_path = ifelse(SAVE_PNG, png_path, NA_character_),
    stringsAsFactors = FALSE
  )
}

num <- function(x) suppressWarnings(as.numeric(x))

short_dataset_label <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "GSE178265_DA_01B", "DA reference\nGSE178265", x)
  x <- ifelse(x == "GSE233885", "GSE233885", x)
  x <- ifelse(x == "GSE204796", "GSE204796", x)
  x <- ifelse(x == "GSE132758", "GSE132758", x)
  x <- ifelse(x == "GSE200610", "GSE200610", x)
  x
}

pretty_class <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "ideal_DA_projection_high_safety_low", "Ideal-like\nDA/proj high\nSafety low", x)
  x <- ifelse(x == "high_safety_risk_low_DA", "High safety-risk\nLow DA", x)
  x <- ifelse(x == "mixed_DA_or_projection_with_safety_risk", "Mixed\nDA/proj + risk", x)
  x <- ifelse(x == "projection_competence_without_DA_low_safety", "Projection-like\nDA low\nSafety low", x)
  x <- ifelse(x == "lower_priority_or_mixed", "Lower priority\nor mixed", x)
  x
}

pretty_bias <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "A9_like_bias", "A9-like", x)
  x <- ifelse(x == "A10_like_bias", "A10-like", x)
  x <- ifelse(x == "A9_A10_mixed_or_unclear", "Mixed/unclear", x)
  x <- ifelse(x == "unknown", "Unknown", x)
  x
}

pretty_score_type <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "DA_like_composite_score", "DA-like\nscore", x)
  x <- ifelse(x == "projection_competence_composite_score", "Projection\ncompetence", x)
  x <- ifelse(x == "safety_risk_composite_05B", "Safety-risk\nscore", x)
  x <- ifelse(x == "A9_minus_A10_score_05A", "A9−A10\nbias", x)
  x
}

stamp("读取 06A 表格。")

dataset_dt <- as.data.table(read_csv_required(input_dataset))
a9a10_dt <- as.data.table(read_csv_required(input_a9a10))
class_dt <- as.data.table(read_csv_required(input_class))
story_dt <- as.data.table(read_csv_required(input_story))
numbers_dt <- as.data.table(read_csv_optional(input_numbers))

stamp("整理 V2 plot tables。")

dataset_plot <- copy(dataset_dt)

required_dataset_cols <- c(
  "dataset",
  "mean_safety_risk_composite_05B",
  "mean_DA_projection_competence",
  "mean_DA_like",
  "mean_projection_competence",
  "favorable_index_06A",
  "dataset_story_class_05B"
)

for (col in required_dataset_cols) {
  if (!col %in% colnames(dataset_plot)) dataset_plot[[col]] <- NA
}

dataset_plot[, dataset_short := short_dataset_label(dataset)]

dataset_plot[
  ,
  dataset_order := factor(dataset_short, levels = dataset_short[order(favorable_index_06A)])
]

dataset_plot[
  ,
  story_quadrant_06B := fifelse(
    mean_DA_projection_competence >= median(mean_DA_projection_competence, na.rm = TRUE) &
      mean_safety_risk_composite_05B <= median(mean_safety_risk_composite_05B, na.rm = TRUE),
    "High DA/proj\nLow risk",
    fifelse(
      mean_DA_projection_competence < median(mean_DA_projection_competence, na.rm = TRUE) &
        mean_safety_risk_composite_05B > median(mean_safety_risk_composite_05B, na.rm = TRUE),
      "Low DA/proj\nHigh risk",
      "Intermediate\nor mixed"
    )
  )
]

atomic_write_csv(as.data.frame(dataset_plot), dataset_plot_table_csv)

class_prop <- copy(class_dt)

if (nrow(class_prop) > 0) {
  class_prop[, dataset_short := short_dataset_label(dataset)]
  class_prop[, class_short := pretty_class(safety_contrast_class_05B)]

  class_prop[
    ,
    total_groups_dataset := sum(n_groups, na.rm = TRUE),
    by = dataset
  ]

  class_prop[
    ,
    group_fraction := n_groups / total_groups_dataset
  ]

  class_prop[
    ,
    dataset_short := factor(dataset_short, levels = dataset_plot$dataset_short[order(dataset_plot$favorable_index_06A)])
  ]

  class_levels <- c(
    "Ideal-like\nDA/proj high\nSafety low",
    "Mixed\nDA/proj + risk",
    "High safety-risk\nLow DA",
    "Projection-like\nDA low\nSafety low",
    "Lower priority\nor mixed"
  )

  class_prop[
    ,
    class_short := factor(class_short, levels = class_levels)
  ]
}

atomic_write_csv(as.data.frame(class_prop), candidate_class_prop_csv)

a9a10_prop <- copy(a9a10_dt)

if (nrow(a9a10_prop) > 0) {
  a9a10_prop[, dataset_short := short_dataset_label(dataset)]
  a9a10_prop[, bias_short := pretty_bias(A9_A10_bias_label_05B)]

  a9a10_prop[
    ,
    total_groups_dataset := sum(n_groups, na.rm = TRUE),
    by = dataset
  ]

  a9a10_prop[
    ,
    group_fraction := n_groups / total_groups_dataset
  ]

  a9a10_prop[
    ,
    dataset_short := factor(dataset_short, levels = dataset_plot$dataset_short[order(dataset_plot$favorable_index_06A)])
  ]

  a9a10_prop[
    ,
    bias_short := factor(bias_short, levels = c("A9-like", "Mixed/unclear", "A10-like", "Unknown"))
  ]
}

atomic_write_csv(as.data.frame(a9a10_prop), a9a10_prop_csv)

tile_dt <- copy(story_dt)

if (nrow(tile_dt) > 0) {
  for (col in c(
    "DA_like_composite_score",
    "projection_competence_composite_score",
    "safety_risk_composite_05B",
    "A9_minus_A10_score_05A",
    "DA_projection_competence_composite_score"
  )) {
    if (!col %in% colnames(tile_dt)) tile_dt[[col]] <- NA_real_
  }

  tile_dt[, class_short := pretty_class(safety_contrast_class_05B)]

  tile_dt[
    ,
    story_rank_score_06B := fifelse(
      safety_contrast_class_05B == "ideal_DA_projection_high_safety_low",
      DA_projection_competence_composite_score - safety_risk_composite_05B,
      fifelse(
        safety_contrast_class_05B %in% c("high_safety_risk_low_DA", "mixed_DA_or_projection_with_safety_risk"),
        safety_risk_composite_05B,
        DA_projection_competence_composite_score
      )
    )
  ]

  tile_dt <- tile_dt[order(-story_rank_score_06B)]

  tile_dt <- head(tile_dt, TOP_N_GROUPS_FOR_TILE)

  tile_dt[
    ,
    group_label_06B := paste0(dataset, " | ", object_id, " | group ", group_id)
  ]

  tile_dt[
    ,
    group_label_short_06B := paste0(dataset, "_", seq_len(.N))
  ]
}

atomic_write_csv(as.data.frame(tile_dt), top_tile_table_csv)

stamp("生成 V2 publication draft figures。")

figure_records <- list()

x_med <- median(dataset_plot$mean_safety_risk_composite_05B, na.rm = TRUE)
y_med <- median(dataset_plot$mean_DA_projection_competence, na.rm = TRUE)

x_range <- range(dataset_plot$mean_safety_risk_composite_05B, na.rm = TRUE)
y_range <- range(dataset_plot$mean_DA_projection_competence, na.rm = TRUE)

x_pad <- diff(x_range) * 0.18
y_pad <- diff(y_range) * 0.18

p2a <- ggplot(
  dataset_plot,
  aes(
    x = mean_safety_risk_composite_05B,
    y = mean_DA_projection_competence
  )
) +
  geom_vline(xintercept = x_med, linetype = "dashed", linewidth = 0.35) +
  geom_hline(yintercept = y_med, linetype = "dashed", linewidth = 0.35) +
  geom_point(size = 3) +
  geom_text(aes(label = dataset_short), vjust = -0.8, size = 3.2, check_overlap = FALSE) +
  scale_x_continuous(
    limits = c(max(0, x_range[1] - x_pad), x_range[2] + x_pad),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  scale_y_continuous(
    limits = c(max(0, y_range[1] - y_pad), y_range[2] + y_pad),
    expand = expansion(mult = c(0.03, 0.10))
  ) +
  labs(
    title = "Dataset-level DA/projection competence versus safety-risk state",
    subtitle = "Projection score represents molecular competence, not anatomical projection",
    x = "Mean safety-risk-associated transcriptional score",
    y = "Mean DA/projection-associated molecular competence score"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.margin = margin(10, 18, 10, 14),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

figure_records[[length(figure_records) + 1L]] <- save_plot(
  p2a,
  "Figure2A_V2_dataset_DA_projection_vs_safety",
  width = 7.5,
  height = 5.7
)

p2b <- ggplot(
  dataset_plot,
  aes(
    x = reorder(dataset_short, favorable_index_06A),
    y = favorable_index_06A
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35) +
  geom_col(width = 0.72) +
  coord_flip() +
  labs(
    title = "Dataset-level favorable index",
    subtitle = "Favorable index = DA/projection competence score − safety-risk score",
    x = "Dataset",
    y = "Favorable index"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

figure_records[[length(figure_records) + 1L]] <- save_plot(
  p2b,
  "Figure2B_V2_dataset_favorable_index",
  width = 7.2,
  height = 5.2
)

if (nrow(class_prop) > 0) {
  p2c <- ggplot(
    class_prop,
    aes(
      x = dataset_short,
      y = group_fraction,
      fill = class_short
    )
  ) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Candidate class composition by dataset",
      x = "Dataset",
      y = "Fraction of groups",
      fill = "Candidate class"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.title = element_text(face = "bold")
    )

  figure_records[[length(figure_records) + 1L]] <- save_plot(
    p2c,
    "Figure2C_V2_candidate_class_composition_by_dataset",
    width = 9.2,
    height = 5.5
  )
}

if (nrow(a9a10_prop) > 0) {
  p2d <- ggplot(
    a9a10_prop,
    aes(
      x = dataset_short,
      y = group_fraction,
      fill = bias_short
    )
  ) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "A9/A10-like molecular bias composition",
      x = "Dataset",
      y = "Fraction of groups",
      fill = "A9/A10 bias"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold")
    )

  figure_records[[length(figure_records) + 1L]] <- save_plot(
    p2d,
    "Figure2D_V2_A9_A10_bias_composition_by_dataset",
    width = 8.8,
    height = 5.3
  )
}

if (nrow(tile_dt) > 0) {
  tile_long <- melt(
    tile_dt,
    id.vars = c(
      "dataset",
      "object_id",
      "group_id",
      "group_label_short_06B",
      "safety_contrast_class_05B",
      "class_short"
    ),
    measure.vars = c(
      "DA_like_composite_score",
      "projection_competence_composite_score",
      "safety_risk_composite_05B",
      "A9_minus_A10_score_05A"
    ),
    variable.name = "score_type",
    value.name = "score_value"
  )

  tile_long[
    ,
    score_type_short := pretty_score_type(score_type)
  ]

  tile_long[
    ,
    group_label_short_06B := factor(
      group_label_short_06B,
      levels = rev(unique(tile_dt$group_label_short_06B))
    )
  ]

  tile_long[
    ,
    score_type_short := factor(
      score_type_short,
      levels = c("DA-like\nscore", "Projection\ncompetence", "Safety-risk\nscore", "A9−A10\nbias")
    )
  ]

  p2e <- ggplot(
    tile_long,
    aes(
      x = score_type_short,
      y = group_label_short_06B,
      fill = score_value
    )
  ) +
    geom_tile() +
    facet_grid(class_short ~ ., scales = "free_y", space = "free_y") +
    labs(
      title = "Top story candidate groups",
      subtitle = "Rows are selected candidate groups; full IDs are in 06B_V2_top_story_group_tile_table.csv",
      x = "Score type",
      y = "Candidate group",
      fill = "Score"
    ) +
    theme_classic(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      axis.text.y = element_text(size = 6),
      strip.text.y = element_text(size = 7, angle = 0),
      plot.title = element_text(face = "bold")
    )

  figure_records[[length(figure_records) + 1L]] <- save_plot(
    p2e,
    "Figure2E_V2_top_story_candidate_groups_tile",
    width = 8.2,
    height = 9
  )
}

figure_index <- rbindlist(figure_records, fill = TRUE)

figure_index$intended_panel <- c(
  "Figure 2A",
  "Figure 2B",
  "Figure 2C",
  "Figure 2D",
  "Figure 2E"
)[seq_len(nrow(figure_index))]

figure_index$description <- c(
  "Dataset-level DA/projection competence versus safety-risk score with unclipped labels.",
  "Dataset-level favorable index ranking.",
  "Candidate class composition by dataset as proportions with shortened labels.",
  "A9/A10-like molecular bias composition by dataset as proportions.",
  "Polished heatmap-like tile plot for top selected story candidate groups."
)[seq_len(nrow(figure_index))]

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

number_lines <- if (nrow(numbers_dt) > 0 && all(c("metric", "value") %in% colnames(numbers_dt))) {
  paste0(numbers_dt$metric, ": ", numbers_dt$value)
} else {
  "No manuscript key numbers table found."
}

dataset_lines <- apply(as.data.frame(dataset_plot), 1, function(x) {
  paste0(
    x[["dataset"]],
    ": DA/projection=",
    round(as.numeric(x[["mean_DA_projection_competence"]]), 4),
    "; safety=",
    round(as.numeric(x[["mean_safety_risk_composite_05B"]]), 4),
    "; favorable_index=",
    round(as.numeric(x[["favorable_index_06A"]]), 4),
    "; quadrant=",
    x[["story_quadrant_06B"]]
  )
})

figure_lines <- if (nrow(figure_index) > 0) {
  paste0(figure_index$intended_panel, ": ", figure_index$figure_id)
} else {
  "none"
}

report_lines <- c(
  "06B V2 publication figure polish report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Manuscript key numbers:",
  number_lines,
  "",
  "Dataset figure interpretation:",
  dataset_lines,
  "",
  "Generated figure drafts:",
  figure_lines,
  "",
  "Output tables:",
  paste0("Figure index: ", figure_index_csv),
  paste0("Dataset plot table: ", dataset_plot_table_csv),
  paste0("Candidate class proportion table: ", candidate_class_prop_csv),
  paste0("A9/A10 proportion table: ", a9a10_prop_csv),
  paste0("Top tile table: ", top_tile_table_csv),
  "",
  "Output figure directory:",
  out_figures_dir,
  "",
  "Next step:",
  "06C_MANUSCRIPT_RESULTS_TEXT_DRAFT.R",
  "",
  "Journal-rigor note:",
  "V2 figures are improved drafts but remain transcriptional/molecular evidence. Do not claim real projection or proven safety."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("06B V2 publication figure polish 运行结束\n")
cat("============================================================\n\n")

cat("Figure drafts generated：", nrow(figure_index), "\n")
cat("Dataset rows：", nrow(dataset_plot), "\n")
cat("Candidate class proportion rows：", nrow(class_prop), "\n")
cat("A9/A10 proportion rows：", nrow(a9a10_prop), "\n")
cat("Top story tile rows：", nrow(tile_dt), "\n\n")

cat("输出表格：\n")
cat(figure_index_csv, "\n")
cat(dataset_plot_table_csv, "\n")
cat(candidate_class_prop_csv, "\n")
cat(a9a10_prop_csv, "\n")
cat(top_tile_table_csv, "\n\n")

cat("输出图片目录：\n")
cat(out_figures_dir, "\n\n")

cat("✅ 06B V2 publication figure polish 完成。\n")
cat("下一步进入 06C：manuscript results text draft。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

PROJECT_TITLE <- "Single-cell transcriptomic modelling of dopaminergic graft-like competence and safety-risk-associated states in Parkinsonian cell replacement datasets"

cat("\n============================================================\n")
cat("06C：manuscript results text draft\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06C。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_dataset <- file.path(tables_dir, "06A_figure_table_prep", "06A_dataset_overview_DA_projection_safety.csv")
input_a9a10 <- file.path(tables_dir, "06A_figure_table_prep", "06A_A9_A10_bias_summary_by_dataset.csv")
input_candidate_class <- file.path(tables_dir, "06A_figure_table_prep", "06A_candidate_class_summary_by_dataset.csv")
input_story_groups <- file.path(tables_dir, "06A_figure_table_prep", "06A_top_story_candidate_groups.csv")
input_numbers <- file.path(tables_dir, "06A_figure_table_prep", "06A_manuscript_key_numbers.csv")
input_05A_audit <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_final_audit_summary.csv")
input_05A_missing <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_missing_unscored_objects.csv")
input_05B_qc <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_QC_audit_summary.csv")

input_06B_v2_index <- file.path(tables_dir, "06B_publication_figure_drafts_V2", "06B_V2_figure_index.csv")
input_06B_index <- file.path(tables_dir, "06B_publication_figure_drafts", "06B_figure_index.csv")

out_tables_dir <- file.path(tables_dir, "06C_manuscript_results_text")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

results_en_md <- file.path(out_tables_dir, "06C_results_draft_EN.md")
results_cn_md <- file.path(out_tables_dir, "06C_results_draft_CN.md")
figure2_legend_md <- file.path(out_tables_dir, "06C_Figure2_legend_draft.md")
claims_cautions_csv <- file.path(out_tables_dir, "06C_key_claims_and_cautions.csv")
manuscript_outline_md <- file.path(out_tables_dir, "06C_manuscript_story_outline.md")
report_txt <- file.path(reports_dir, "06C_manuscript_results_text_draft_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

num <- function(x) suppressWarnings(as.numeric(x))

fmt <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

get_number <- function(numbers_dt, metric_name, default = "NA") {
  if (nrow(numbers_dt) == 0 || !"metric" %in% colnames(numbers_dt) || !"value" %in% colnames(numbers_dt)) {
    return(default)
  }
  val <- numbers_dt$value[numbers_dt$metric == metric_name]
  if (length(val) == 0) return(default)
  as.character(val[[1]])
}

safe_first <- function(x, default = NA_character_) {
  if (length(x) == 0) return(default)
  x[[1]]
}

dataset_sentence <- function(row) {
  paste0(
    row[["dataset"]],
    " showed a mean DA/projection competence score of ",
    fmt(row[["mean_DA_projection_competence"]]),
    " and a mean safety-risk-associated score of ",
    fmt(row[["mean_safety_risk_composite_05B"]]),
    " (favorable index = ",
    fmt(row[["favorable_index_06A"]]),
    ")."
  )
}

dataset_sentence_cn <- function(row) {
  paste0(
    row[["dataset"]],
    " 的 mean DA/projection competence score 为 ",
    fmt(row[["mean_DA_projection_competence"]]),
    "，mean safety-risk-associated score 为 ",
    fmt(row[["mean_safety_risk_composite_05B"]]),
    "，favorable index 为 ",
    fmt(row[["favorable_index_06A"]]),
    "。"
  )
}

stamp("读取 06A / 06B / 05A / 05B 输出。")

dataset_dt <- as.data.table(read_csv_required(input_dataset))
a9a10_dt <- as.data.table(read_csv_required(input_a9a10))
candidate_dt <- as.data.table(read_csv_required(input_candidate_class))
story_dt <- as.data.table(read_csv_required(input_story_groups))
numbers_dt <- as.data.table(read_csv_required(input_numbers))
audit05A <- as.data.table(read_csv_optional(input_05A_audit))
missing05A <- as.data.table(read_csv_optional(input_05A_missing))
qc05B <- as.data.table(read_csv_optional(input_05B_qc))

if (file.exists(input_06B_v2_index)) {
  fig_index <- as.data.table(read_csv_optional(input_06B_v2_index))
  fig_source <- "06B_V2"
} else {
  fig_index <- as.data.table(read_csv_optional(input_06B_index))
  fig_source <- "06B"
}

stamp("提取 key results。")

dataset_dt[, favorable_index_06A := num(favorable_index_06A)]
dataset_dt[, mean_DA_projection_competence := num(mean_DA_projection_competence)]
dataset_dt[, mean_safety_risk_composite_05B := num(mean_safety_risk_composite_05B)]

best_dataset <- dataset_dt[order(-favorable_index_06A)][1]
worst_dataset <- dataset_dt[order(favorable_index_06A)][1]
highest_da_dataset <- dataset_dt[order(-mean_DA_projection_competence)][1]
highest_safety_dataset <- dataset_dt[order(-mean_safety_risk_composite_05B)][1]
lowest_safety_dataset <- dataset_dt[order(mean_safety_risk_composite_05B)][1]

n_scored_objects <- get_number(numbers_dt, "successfully_scored_objects_for_05A_05B")
n_scored_cells <- get_number(numbers_dt, "successfully_scored_cells_for_05A")
n_group_rows <- get_number(numbers_dt, "group_level_DA_projection_score_rows")
n_safety_group_rows <- get_number(numbers_dt, "group_level_safety_score_rows")
n_contrast_groups <- get_number(numbers_dt, "DA_projection_vs_safety_contrast_groups")
n_story_groups <- get_number(numbers_dt, "story_candidate_groups")
n_ideal <- get_number(numbers_dt, "ideal_DA_projection_high_safety_low_groups")
n_high_risk <- get_number(numbers_dt, "high_safety_risk_low_DA_groups")
n_mixed <- get_number(numbers_dt, "mixed_DA_or_projection_with_safety_risk_groups")
n_datasets <- get_number(numbers_dt, "datasets_in_05B_summary")

if (nrow(a9a10_dt) > 0 && all(c("dataset", "A9_A10_bias_label_05B", "n_groups") %in% colnames(a9a10_dt))) {
  a9a10_leading <- a9a10_dt[
    order(dataset, -n_groups),
    .SD[1],
    by = dataset
  ]
} else {
  a9a10_leading <- data.table()
}

if (nrow(candidate_dt) > 0 && all(c("dataset", "safety_contrast_class_05B", "n_groups") %in% colnames(candidate_dt))) {
  candidate_leading <- candidate_dt[
    order(dataset, -n_groups),
    .SD[1],
    by = dataset
  ]
} else {
  candidate_leading <- data.table()
}

n_unscored <- if (nrow(missing05A) > 0) nrow(missing05A) else 0L

stamp("生成 key claims and cautions table。")

claims <- data.frame(
  claim_level = c(
    "Primary result",
    "Primary result",
    "Primary result",
    "Secondary result",
    "Quality control",
    "Boundary condition",
    "Boundary condition"
  ),
  claim = c(
    "The DA reference dataset and GSE233885 showed the most favorable DA/projection competence versus safety-risk profiles.",
    "GSE204796 and GSE132758 showed stronger safety-risk-associated transcriptional signals.",
    "A9/A10-like molecular bias was dataset-dependent rather than uniform across all graft-related datasets.",
    "Candidate groups could be separated into ideal-like, high-safety-risk, and mixed DA/projection-with-risk categories.",
    "05A/05B downstream scoring used successfully scored objects only.",
    "Projection-associated molecular competence is not evidence of real anatomical projection.",
    "Safety-risk-associated transcriptional state is not proof of tumorigenicity or clinical safety."
  ),
  supporting_output = c(
    "06A_dataset_overview_DA_projection_safety.csv; Figure 2A-B",
    "06A_dataset_overview_DA_projection_safety.csv; Figure 2A-B",
    "06A_A9_A10_bias_summary_by_dataset.csv; Figure 2D",
    "06A_candidate_class_summary_by_dataset.csv; Figure 2C",
    "05A_V2_final_audit_summary.csv",
    "05A_signature_gene_sets.csv; methods wording",
    "05B_safety_signature_definition.csv; methods wording"
  ),
  manuscript_safe_wording = c(
    "favorable DA/projection-associated molecular competence and low safety-risk-associated transcriptional signal",
    "elevated progenitor/cycling/stress-associated safety-risk transcriptional signal",
    "dataset-dependent A9-like or A10-like molecular bias",
    "transcriptionally defined candidate states",
    "analyses were restricted to successfully scored objects",
    "projection-associated molecular competence",
    "safety-risk-associated transcriptional state"
  ),
  forbidden_wording = c(
    "best therapeutic graft; proven functional integration",
    "tumorigenic cells; unsafe grafts",
    "true A9/A10 identity proven in vivo",
    "final cell type labels without validation",
    "all 54 objects were scored successfully",
    "real projection; retrograde projection",
    "proven tumorigenicity; proven clinical safety"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(claims, claims_cautions_csv)

stamp("生成英文 results draft。")

dataset_lines_en <- apply(as.data.frame(dataset_dt[order(-favorable_index_06A)]), 1, dataset_sentence)

a9_lines_en <- if (nrow(a9a10_leading) > 0) {
  apply(as.data.frame(a9a10_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " was dominated by ",
      x[["A9_A10_bias_label_05B"]],
      " groups among scored groups."
    )
  })
} else {
  character()
}

class_lines_en <- if (nrow(candidate_leading) > 0) {
  apply(as.data.frame(candidate_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " was dominated by the ",
      x[["safety_contrast_class_05B"]],
      " class among scored groups."
    )
  })
} else {
  character()
}

results_en <- c(
  paste0("# Results draft"),
  "",
  paste0("## Overview of scored single-cell objects"),
  "",
  paste0(
    "After quality control, marker-based annotation and final audit, downstream DA/projection and safety-risk scoring was performed on ",
    n_scored_objects,
    " successfully scored objects representing ",
    n_scored_cells,
    " cells. The analysis generated ",
    n_group_rows,
    " group-level DA/A9/A10/projection score rows and ",
    n_safety_group_rows,
    " group-level safety-risk score rows. Two objects were retained in the audit record but not used for downstream quantitative claims because no valid 05A score was generated."
  ),
  "",
  paste0("## Dataset-level DA/projection competence and safety-risk profiles"),
  "",
  paste0(
    "To compare graft-associated transcriptional states, we calculated a composite DA/projection-associated molecular competence score and contrasted it with a safety-risk-associated transcriptional score. ",
    best_dataset$dataset,
    " showed the most favorable profile, with a DA/projection competence score of ",
    fmt(best_dataset$mean_DA_projection_competence),
    " and a safety-risk-associated score of ",
    fmt(best_dataset$mean_safety_risk_composite_05B),
    " (favorable index = ",
    fmt(best_dataset$favorable_index_06A),
    "). ",
    highest_da_dataset$dataset,
    " had the highest DA/projection competence score, whereas ",
    highest_safety_dataset$dataset,
    " showed the highest safety-risk-associated score."
  ),
  "",
  paste(dataset_lines_en, collapse = "\n\n"),
  "",
  paste0("## Candidate state classes across datasets"),
  "",
  paste0(
    "Across scored groups, ",
    n_ideal,
    " groups were classified as ideal-like DA/projection-high and safety-low candidates, ",
    n_high_risk,
    " groups showed a high safety-risk/low-DA profile, and ",
    n_mixed,
    " groups showed mixed DA/projection signal together with elevated safety-risk signal. These classes should be interpreted as transcriptionally defined candidate states rather than final validated cell identities."
  ),
  "",
  paste(class_lines_en, collapse = "\n\n"),
  "",
  paste0("## Dataset-dependent A9/A10-like molecular bias"),
  "",
  paste0(
    "We next examined whether DA-like states showed A9-like or A10-like molecular bias. The distribution of A9/A10-like bias was dataset-dependent rather than uniform across all datasets. In particular, the favorable datasets showed stronger A9-like tendency, whereas other datasets contained mixed or A10-like-biased groups."
  ),
  "",
  paste(a9_lines_en, collapse = "\n\n"),
  "",
  paste0("## Interpretation and boundary of the evidence"),
  "",
  paste0(
    "These results support a working model in which DA/projection-associated molecular competence and safety-risk-associated transcriptional state can be jointly used to prioritize graft-like cell states. Importantly, the projection score represents molecular competence related to neurite maturation, synaptic machinery and axon-guidance-associated genes, and does not demonstrate real anatomical projection or functional integration. Likewise, the safety-risk score captures proliferation, progenitor, pluripotency/immature, stress and stromal-associated transcriptional signals, and should not be interpreted as direct proof of tumorigenicity or clinical safety."
  )
)

writeLines(results_en, results_en_md)

stamp("生成中文 results draft。")

dataset_lines_cn <- apply(as.data.frame(dataset_dt[order(-favorable_index_06A)]), 1, dataset_sentence_cn)

a9_lines_cn <- if (nrow(a9a10_leading) > 0) {
  apply(as.data.frame(a9a10_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " 在已评分 groups 中主要表现为 ",
      x[["A9_A10_bias_label_05B"]],
      "。"
    )
  })
} else {
  character()
}

class_lines_cn <- if (nrow(candidate_leading) > 0) {
  apply(as.data.frame(candidate_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " 在已评分 groups 中主要以 ",
      x[["safety_contrast_class_05B"]],
      " 为主。"
    )
  })
} else {
  character()
}

results_cn <- c(
  "# 中文 Results 草稿",
  "",
  "## 已评分对象概览",
  "",
  paste0(
    "经过 QC、marker-based annotation 和最终审计后，下游 DA/projection 与 safety-risk scoring 使用了 ",
    n_scored_objects,
    " 个成功评分对象，覆盖 ",
    n_scored_cells,
    " 个细胞。分析共产生 ",
    n_group_rows,
    " 行 group-level DA/A9/A10/projection score，以及 ",
    n_safety_group_rows,
    " 行 group-level safety-risk score。另有 2 个对象被保留在审计记录中，但由于未产生有效 05A score，不用于后续定量结论。"
  ),
  "",
  "## Dataset-level DA/projection competence 与 safety-risk profile",
  "",
  paste0(
    "为了比较不同 graft-associated transcriptional states，我们计算了 DA/projection-associated molecular competence composite score，并与 safety-risk-associated transcriptional score 进行对照。结果显示，",
    best_dataset$dataset,
    " 的 overall profile 最有利：DA/projection competence score 为 ",
    fmt(best_dataset$mean_DA_projection_competence),
    "，safety-risk-associated score 为 ",
    fmt(best_dataset$mean_safety_risk_composite_05B),
    "，favorable index 为 ",
    fmt(best_dataset$favorable_index_06A),
    "。其中 ",
    highest_da_dataset$dataset,
    " 具有最高 DA/projection competence，而 ",
    highest_safety_dataset$dataset,
    " 具有最高 safety-risk-associated score。"
  ),
  "",
  paste(dataset_lines_cn, collapse = "\n\n"),
  "",
  "## Candidate state classes across datasets",
  "",
  paste0(
    "在已评分 groups 中，",
    n_ideal,
    " 个 groups 被归为 ideal-like DA/projection-high and safety-low candidates，",
    n_high_risk,
    " 个 groups 表现为 high safety-risk/low-DA profile，",
    n_mixed,
    " 个 groups 同时具有 DA/projection signal 和较高 safety-risk signal。这里的 class 是 transcriptionally defined candidate states，不是最终不可更改的细胞类型标签。"
  ),
  "",
  paste(class_lines_cn, collapse = "\n\n"),
  "",
  "## Dataset-dependent A9/A10-like molecular bias",
  "",
  "随后我们分析了 DA-like states 的 A9-like / A10-like molecular bias。结果显示，A9/A10-like bias 并不是所有数据集一致，而是具有明显 dataset-dependent heterogeneity。整体更 favorable 的 dataset 更偏 A9-like tendency，而其他 dataset 则含 mixed 或 A10-like-biased groups。",
  "",
  paste(a9_lines_cn, collapse = "\n\n"),
  "",
  "## 证据边界",
  "",
  "这些结果支持一个工作模型：DA/projection-associated molecular competence 与 safety-risk-associated transcriptional state 可以联合用于筛选更理想的 graft-like cell states。但必须注意，projection score 只是基于 neurite maturation、synaptic machinery 和 axon-guidance-associated genes 的分子能力评分，并不能证明真实解剖投射或功能整合。同样，safety-risk score 反映 proliferation、progenitor、pluripotency/immature、stress 和 stromal-associated transcriptional signals，不能直接等同于肿瘤形成风险或临床安全性证明。"
)

writeLines(results_cn, results_cn_md)

stamp("生成 Figure 2 legend draft。")

figure_legend <- c(
  "# Figure 2 legend draft",
  "",
  "## Figure 2. Joint modelling of DA/projection-associated molecular competence and safety-risk-associated transcriptional states.",
  "",
  "**(A)** Dataset-level scatter plot comparing the mean safety-risk-associated transcriptional score and the mean DA/projection-associated molecular competence score. Dashed lines indicate median values across datasets. The projection score reflects molecular competence associated with neurite maturation, synaptic machinery and axon-guidance-associated genes, and should not be interpreted as direct evidence of anatomical projection.",
  "",
  "**(B)** Dataset-level favorable index, calculated as mean DA/projection-associated molecular competence score minus mean safety-risk-associated transcriptional score. Higher values indicate a more favorable balance between DA/projection-associated molecular competence and lower safety-risk-associated signal.",
  "",
  "**(C)** Candidate class composition across datasets. Groups were classified as ideal-like DA/projection-high and safety-low, high safety-risk and low-DA, mixed DA/projection-with-risk, projection-competent but DA-low, or lower-priority/mixed states. These classes represent transcriptionally defined candidate states.",
  "",
  "**(D)** A9/A10-like molecular bias composition across datasets. Bias labels were inferred from relative A9-like and A10-like molecular signature scores and should be interpreted as molecular bias rather than definitive anatomical subtype identity.",
  "",
  "**(E)** Heatmap-like summary of selected high-priority story candidate groups. Rows represent selected groups, and columns show DA-like score, projection competence score, safety-risk score and A9-minus-A10 bias score. Full group identifiers are provided in the corresponding 06B output table.",
  "",
  "All panels are based on successfully scored objects only. Unscored objects were retained in the audit record but excluded from downstream quantitative claims."
)

writeLines(figure_legend, figure2_legend_md)

stamp("生成 manuscript story outline。")

outline <- c(
  paste0("# Manuscript story outline"),
  "",
  paste0("## Working title"),
  PROJECT_TITLE,
  "",
  "## Central hypothesis",
  "Public single-cell graft-related datasets can be jointly modelled to identify cell states with favorable DA/projection-associated molecular competence and low safety-risk-associated transcriptional signal.",
  "",
  "## Main result modules",
  "",
  "### Module 1: DA/A9/A10/projection-associated molecular competence",
  "- DA-like identity and DA functional machinery were scored using curated dopaminergic marker sets.",
  "- A9-like and A10-like molecular bias were evaluated as relative molecular tendencies.",
  "- Projection-associated molecular competence was scored using neurite maturation, synaptic machinery and axon-guidance-associated genes.",
  "- This module must not be described as proof of real anatomical projection.",
  "",
  "### Module 2: Cell-state fate propensity and safety-risk-associated modelling",
  "- Safety-risk-associated transcriptional states were scored using proliferation, progenitor, pluripotency/immature, stress, ECM and vascular/mesenchymal components.",
  "- The score is a transcriptomic risk-associated state score, not proof of tumorigenicity.",
  "- Joint DA/projection versus safety-risk contrast identified ideal-like, high-risk and mixed states.",
  "",
  "## Current story direction",
  paste0("- Most favorable dataset: ", best_dataset$dataset, "."),
  paste0("- Highest DA/projection competence dataset: ", highest_da_dataset$dataset, "."),
  paste0("- Highest safety-risk-associated dataset: ", highest_safety_dataset$dataset, "."),
  paste0("- Lowest safety-risk-associated dataset: ", lowest_safety_dataset$dataset, "."),
  "",
  "## Suggested next section",
  "06D can generate a Discussion draft and figure-by-figure manuscript structure after the final figure drafts are reviewed."
)

writeLines(outline, manuscript_outline_md)

stamp("生成 06C report。")

report_lines <- c(
  "06C manuscript results text draft report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input summary:",
  paste0("Datasets in overview: ", nrow(dataset_dt)),
  paste0("Scored objects: ", n_scored_objects),
  paste0("Scored cells: ", n_scored_cells),
  paste0("Group-level DA/projection rows: ", n_group_rows),
  paste0("Group-level safety rows: ", n_safety_group_rows),
  paste0("Unscored objects retained in audit: ", n_unscored),
  "",
  "Key dataset results:",
  paste0("Best favorable index dataset: ", best_dataset$dataset, " (", fmt(best_dataset$favorable_index_06A), ")"),
  paste0("Highest DA/projection dataset: ", highest_da_dataset$dataset, " (", fmt(highest_da_dataset$mean_DA_projection_competence), ")"),
  paste0("Highest safety-risk dataset: ", highest_safety_dataset$dataset, " (", fmt(highest_safety_dataset$mean_safety_risk_composite_05B), ")"),
  "",
  "Output files:",
  paste0("English results draft: ", results_en_md),
  paste0("Chinese results draft: ", results_cn_md),
  paste0("Figure 2 legend draft: ", figure2_legend_md),
  paste0("Claims and cautions table: ", claims_cautions_csv),
  paste0("Manuscript story outline: ", manuscript_outline_md),
  "",
  "Next step:",
  "06D_DISCUSSION_AND_ABSTRACT_DRAFT.R",
  "",
  "Journal-rigor note:",
  "The draft intentionally uses projection-associated molecular competence and safety-risk-associated transcriptional state to avoid overclaiming real projection, functional integration, tumorigenicity or clinical safety."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("06C manuscript results text draft 运行结束\n")
cat("============================================================\n\n")

cat("Datasets in overview：", nrow(dataset_dt), "\n")
cat("Scored objects：", n_scored_objects, "\n")
cat("Scored cells：", n_scored_cells, "\n")
cat("Best favorable dataset：", best_dataset$dataset, "\n")
cat("Highest DA/projection dataset：", highest_da_dataset$dataset, "\n")
cat("Highest safety-risk dataset：", highest_safety_dataset$dataset, "\n")
cat("Unscored audit objects：", n_unscored, "\n\n")

cat("输出文件：\n")
cat(results_en_md, "\n")
cat(results_cn_md, "\n")
cat(figure2_legend_md, "\n")
cat(claims_cautions_csv, "\n")
cat(manuscript_outline_md, "\n")
cat(report_txt, "\n\n")

cat("✅ 06C manuscript results text draft 完成。\n")
cat("下一步：先打开 06C_results_draft_CN.md 和 06C_results_draft_EN.md，看故事是否符合预期。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

WORKING_TITLE <- "Single-cell transcriptomic modelling identifies dopaminergic graft-like competence and safety-risk-associated states in Parkinsonian cell replacement datasets"

SHORT_TITLE <- "Transcriptomic modelling of dopaminergic graft competence and safety-risk states"

cat("\n============================================================\n")
cat("06D：discussion, abstract and manuscript structure draft\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06D。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_dataset <- file.path(tables_dir, "06A_figure_table_prep", "06A_dataset_overview_DA_projection_safety.csv")
input_numbers <- file.path(tables_dir, "06A_figure_table_prep", "06A_manuscript_key_numbers.csv")
input_claims <- file.path(tables_dir, "06C_manuscript_results_text", "06C_key_claims_and_cautions.csv")
input_results_en <- file.path(tables_dir, "06C_manuscript_results_text", "06C_results_draft_EN.md")
input_results_cn <- file.path(tables_dir, "06C_manuscript_results_text", "06C_results_draft_CN.md")
input_figure2_legend <- file.path(tables_dir, "06C_manuscript_results_text", "06C_Figure2_legend_draft.md")
input_story_outline <- file.path(tables_dir, "06C_manuscript_results_text", "06C_manuscript_story_outline.md")

out_tables_dir <- file.path(tables_dir, "06D_discussion_abstract")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

abstract_en_md <- file.path(out_tables_dir, "06D_abstract_draft_EN.md")
abstract_cn_md <- file.path(out_tables_dir, "06D_abstract_draft_CN.md")
discussion_en_md <- file.path(out_tables_dir, "06D_discussion_draft_EN.md")
discussion_cn_md <- file.path(out_tables_dir, "06D_discussion_draft_CN.md")
manuscript_structure_md <- file.path(out_tables_dir, "06D_manuscript_structure_and_figure_plan.md")
limitations_csv <- file.path(out_tables_dir, "06D_limitations_and_safe_wording_table.csv")
title_keywords_md <- file.path(out_tables_dir, "06D_title_keywords_highlights.md")
report_txt <- file.path(reports_dir, "06D_discussion_abstract_and_structure_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

read_text_optional <- function(path) {
  if (!file.exists(path)) return(character())
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

fmt <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

get_number <- function(numbers_dt, metric_name, default = "NA") {
  if (nrow(numbers_dt) == 0 || !"metric" %in% colnames(numbers_dt) || !"value" %in% colnames(numbers_dt)) {
    return(default)
  }
  val <- numbers_dt$value[numbers_dt$metric == metric_name]
  if (length(val) == 0) return(default)
  as.character(val[[1]])
}

stamp("读取 06C / 06A 输出。")

dataset_dt <- as.data.table(read_csv_required(input_dataset))
numbers_dt <- as.data.table(read_csv_required(input_numbers))
claims_dt <- as.data.table(read_csv_optional(input_claims))

results_en <- read_text_optional(input_results_en)
results_cn <- read_text_optional(input_results_cn)
figure2_legend <- read_text_optional(input_figure2_legend)
story_outline <- read_text_optional(input_story_outline)

stamp("提取 manuscript key numbers。")

dataset_dt[, favorable_index_06A := suppressWarnings(as.numeric(favorable_index_06A))]
dataset_dt[, mean_DA_projection_competence := suppressWarnings(as.numeric(mean_DA_projection_competence))]
dataset_dt[, mean_safety_risk_composite_05B := suppressWarnings(as.numeric(mean_safety_risk_composite_05B))]

best_dataset <- dataset_dt[order(-favorable_index_06A)][1]
second_dataset <- dataset_dt[order(-favorable_index_06A)][2]
worst_dataset <- dataset_dt[order(favorable_index_06A)][1]
highest_da_dataset <- dataset_dt[order(-mean_DA_projection_competence)][1]
highest_safety_dataset <- dataset_dt[order(-mean_safety_risk_composite_05B)][1]

n_scored_objects <- get_number(numbers_dt, "successfully_scored_objects_for_05A_05B")
n_scored_cells <- get_number(numbers_dt, "successfully_scored_cells_for_05A")
n_contrast_groups <- get_number(numbers_dt, "DA_projection_vs_safety_contrast_groups")
n_story_groups <- get_number(numbers_dt, "story_candidate_groups")
n_ideal <- get_number(numbers_dt, "ideal_DA_projection_high_safety_low_groups")
n_high_risk <- get_number(numbers_dt, "high_safety_risk_low_DA_groups")
n_mixed <- get_number(numbers_dt, "mixed_DA_or_projection_with_safety_risk_groups")
n_datasets <- get_number(numbers_dt, "datasets_in_05B_summary")

stamp("生成 limitations and safe wording table。")

limitations <- data.frame(
  issue = c(
    "Projection evidence boundary",
    "Safety evidence boundary",
    "Annotation boundary",
    "Dataset heterogeneity",
    "Unscored objects",
    "Species/platform differences",
    "No direct functional validation",
    "Scoring threshold dependence"
  ),
  limitation = c(
    "Projection-associated score is inferred from axon guidance, neurite maturation and synaptic machinery genes.",
    "Safety-risk score is inferred from proliferation, progenitor, pluripotency/immature, stress and stromal marker programs.",
    "Annotation labels are conservative marker-supported labels and remain preliminary.",
    "Datasets differ in source, protocol, cell composition and biological context.",
    "Two objects were recorded as unscored and excluded from downstream quantitative claims.",
    "Cross-dataset comparison may be influenced by species, gene-symbol mapping and platform effects.",
    "No independent imaging, tracing, electrophysiology or transplantation outcome validation is included in this computational analysis.",
    "Candidate classes depend on curated signatures and predefined thresholds."
  ),
  safe_wording = c(
    "projection-associated molecular competence",
    "safety-risk-associated transcriptional state",
    "marker-supported candidate state",
    "dataset-dependent heterogeneity",
    "downstream claims were restricted to successfully scored objects",
    "cross-dataset transcriptional comparison",
    "computational prioritization framework",
    "threshold-based candidate prioritization"
  ),
  avoid_wording = c(
    "real projection; retrograde projection; proven anatomical integration",
    "tumorigenic; clinically unsafe; proven safety risk",
    "final cell type identity; definitive graft fate",
    "all datasets are directly equivalent",
    "all objects were scored successfully",
    "species-independent universal conclusion",
    "validated therapeutic function",
    "objective ground-truth classification"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(limitations, limitations_csv)

stamp("生成英文 abstract draft。")

abstract_en <- c(
  "# Abstract draft",
  "",
  "**Background:** Cell replacement therapy for Parkinson's disease requires grafted cells to acquire dopaminergic neuronal identity while minimizing immature, proliferative or off-target transcriptional states. However, public single-cell datasets are rarely evaluated using a unified framework that jointly models dopaminergic graft-like competence and safety-risk-associated cell states.",
  "",
  paste0(
    "**Methods:** We assembled and processed public single-cell and bulk transcriptomic datasets related to dopaminergic neurons and graft-associated cell replacement models. After quality control, marker-based annotation and final audit, downstream scoring was performed on ",
    n_scored_objects,
    " successfully scored objects representing ",
    n_scored_cells,
    " cells. We curated transcriptional signatures for DA-like identity, A9/A10-like molecular bias, neuronal maturation, projection-associated molecular competence and safety-risk-associated states. Safety-risk scoring integrated proliferation, progenitor, pluripotency/immature, stress and stromal-associated marker programs."
  ),
  "",
  paste0(
    "**Results:** Joint DA/projection and safety-risk modelling identified strong dataset-dependent heterogeneity. ",
    best_dataset$dataset,
    " showed the most favorable overall balance, with the highest favorable index (",
    fmt(best_dataset$favorable_index_06A),
    "), while ",
    highest_da_dataset$dataset,
    " showed the highest DA/projection-associated molecular competence score. Among graft-associated datasets, ",
    second_dataset$dataset,
    " displayed a favorable DA/projection-high and safety-low profile, whereas ",
    highest_safety_dataset$dataset,
    " showed the highest safety-risk-associated transcriptional score. Across ",
    n_contrast_groups,
    " contrasted groups, ",
    n_ideal,
    " groups were classified as ideal-like DA/projection-high and safety-low candidates, ",
    n_high_risk,
    " groups showed high safety-risk and low DA signal, and ",
    n_mixed,
    " groups showed mixed DA/projection signal with concurrent safety-risk-associated features."
  ),
  "",
  "**Conclusions:** This study establishes a transcriptomic framework for prioritizing graft-like dopaminergic cell states by jointly modelling DA/projection-associated molecular competence and safety-risk-associated transcriptional programs. The analysis supports the use of public single-cell datasets to nominate favorable and risk-associated graft cell states, while emphasizing that projection-associated scores do not prove anatomical projection and safety-risk scores do not prove tumorigenicity or clinical safety."
)

writeLines(abstract_en, abstract_en_md)

stamp("生成中文摘要解释版。")

abstract_cn <- c(
  "# 中文摘要草稿",
  "",
  "**背景：** 帕金森病细胞替代治疗要求移植细胞获得多巴胺能神经元样身份，同时尽量避免未成熟、增殖性或 off-target 转录组状态。然而，公开单细胞数据很少被放在一个统一框架下，同时评估 dopaminergic graft-like competence 和 safety-risk-associated cell states。",
  "",
  paste0(
    "**方法：** 本研究整合并处理了与多巴胺神经元和 graft-associated cell replacement models 相关的公开单细胞及 bulk 转录组数据。经过 QC、marker-based annotation 和最终审计后，下游 scoring 使用了 ",
    n_scored_objects,
    " 个成功评分对象，共代表 ",
    n_scored_cells,
    " 个细胞。我们构建了 DA-like identity、A9/A10-like molecular bias、neuronal maturation、projection-associated molecular competence 和 safety-risk-associated states 的转录组 signature。Safety-risk score 整合了 proliferation、progenitor、pluripotency/immature、stress 和 stromal-associated marker programs。"
  ),
  "",
  paste0(
    "**结果：** DA/projection 与 safety-risk 联合建模显示出明显 dataset-dependent heterogeneity。",
    best_dataset$dataset,
    " 具有最有利的整体平衡，favorable index 最高（",
    fmt(best_dataset$favorable_index_06A),
    "），而 ",
    highest_da_dataset$dataset,
    " 具有最高 DA/projection-associated molecular competence score。在 graft-associated datasets 中，",
    second_dataset$dataset,
    " 表现出较有利的 DA/projection-high and safety-low profile，而 ",
    highest_safety_dataset$dataset,
    " 具有最高 safety-risk-associated transcriptional score。在 ",
    n_contrast_groups,
    " 个 contrast groups 中，",
    n_ideal,
    " 个 groups 被归为 ideal-like DA/projection-high and safety-low candidates，",
    n_high_risk,
    " 个 groups 表现为 high safety-risk and low DA signal，",
    n_mixed,
    " 个 groups 同时具有 DA/projection signal 和 safety-risk-associated features。"
  ),
  "",
  "**结论：** 本研究建立了一个 transcriptomic framework，用于通过 DA/projection-associated molecular competence 和 safety-risk-associated transcriptional programs 的联合建模，筛选更理想的 dopaminergic graft-like cell states。需要强调的是，projection-associated score 不能证明真实解剖投射，safety-risk score 也不能证明肿瘤形成或临床安全性。"
)

writeLines(abstract_cn, abstract_cn_md)

stamp("生成英文 discussion draft。")

discussion_en <- c(
  "# Discussion draft",
  "",
  "## Principal findings",
  "",
  paste0(
    "In this study, we developed a single-cell transcriptomic framework to jointly evaluate dopaminergic graft-like molecular competence and safety-risk-associated transcriptional states across public Parkinsonian cell replacement datasets. The analysis was performed on ",
    n_scored_objects,
    " successfully scored objects representing ",
    n_scored_cells,
    " cells, after excluding unscored objects from downstream quantitative claims. The main finding is that DA/projection-associated molecular competence and safety-risk-associated transcriptional programs are not uniformly distributed across datasets. Instead, they form distinct dataset-level and group-level profiles that can be used to prioritize candidate graft-like states."
  ),
  "",
  paste0(
    "The DA reference dataset, ",
    best_dataset$dataset,
    ", showed the most favorable overall profile and the highest favorable index. This was expected because it represents a dopaminergic target/reference population, but it also provided an important positive anchor for evaluating graft-associated datasets. Among graft-associated datasets, ",
    second_dataset$dataset,
    " showed a more favorable balance between DA/projection-associated molecular competence and low safety-risk-associated signal. In contrast, ",
    highest_safety_dataset$dataset,
    " displayed the highest safety-risk-associated transcriptional score, supporting the presence of mixed or risk-associated transcriptional states in at least a subset of graft-related cells."
  ),
  "",
  "## DA-like identity and A9/A10-like molecular bias",
  "",
  "A major goal of dopaminergic cell replacement therapy is to generate grafted neurons with appropriate DA-like molecular identity. Our scoring framework separated DA-like identity, DA functional machinery, A9-like molecular bias, A10-like molecular bias, neuronal maturation and projection-associated molecular competence. This separation is important because a cell state may show partial DA-like marker expression without showing a favorable projection-associated or low-risk profile. The results suggest that A9/A10-like molecular bias is dataset-dependent rather than uniform, with some datasets showing stronger A9-like tendency and others showing mixed or A10-like-biased profiles.",
  "",
  "Importantly, A9-like or A10-like labels in this analysis refer to relative molecular bias based on curated marker signatures. They should not be interpreted as definitive substantia nigra or ventral tegmental area identity, because such anatomical and functional identity would require independent spatial, tracing or functional validation.",
  "",
  "## Projection-associated molecular competence",
  "",
  "The projection-associated molecular competence score was designed to capture transcriptional programs related to neurite maturation, axon guidance and synaptic machinery. This provides a computational way to ask whether graft-like cell states express molecular features consistent with the capacity for neuronal maturation and potential connectivity. However, this score does not demonstrate real anatomical projection, retrograde connectivity or functional integration. Therefore, the appropriate interpretation is that certain candidate groups show projection-associated molecular competence, not that they have formed verified projections in vivo.",
  "",
  "## Safety-risk-associated transcriptional states",
  "",
  paste0(
    "The second major module quantified safety-risk-associated transcriptional states using proliferation, progenitor, pluripotency/immature, stress and stromal-associated components. Across ",
    n_contrast_groups,
    " contrasted groups, the framework identified ",
    n_ideal,
    " ideal-like DA/projection-high and safety-low groups, ",
    n_high_risk,
    " high safety-risk and low-DA groups, and ",
    n_mixed,
    " mixed DA/projection-with-risk groups. This separation is biologically useful because it distinguishes potentially favorable graft-like states from immature or proliferative states that may require additional review."
  ),
  "",
  "Nevertheless, safety-risk-associated transcriptional scores should not be equated with direct tumorigenicity or clinical safety outcomes. A high score indicates enrichment of transcriptional programs associated with proliferation, progenitor identity or immature/pluripotency-related signals, but experimental validation would be required to determine actual safety risk.",
  "",
  "## Biological and translational implications",
  "",
  "This framework provides a practical approach for prioritizing cell states in public graft-related single-cell datasets. Rather than asking only whether cells express DA markers, the analysis evaluates whether DA-like identity is accompanied by projection-associated molecular competence and a low safety-risk-associated transcriptional state. This joint view is more informative for graft-quality assessment because a favorable therapeutic cell state should ideally combine dopaminergic maturation with low immature/proliferative risk.",
  "",
  "The results also highlight that graft-related datasets may contain heterogeneous mixtures of favorable, risk-associated and mixed states. Such heterogeneity could reflect differences in differentiation protocols, graft maturation stage, host environment, sampling time or dataset-specific technical factors. Future work could use this framework to compare new differentiation conditions, screen candidate graft preparations or build predictive models for favorable versus risk-associated graft-like states.",
  "",
  "## Limitations",
  "",
  "Several limitations should be considered. First, the study is computational and based on public transcriptomic datasets; it does not include direct experimental validation. Second, projection-associated molecular competence is inferred from gene expression and cannot prove anatomical projection or functional connectivity. Third, safety-risk-associated transcriptional state is not proof of tumorigenicity or clinical safety. Fourth, cross-dataset comparisons may be influenced by differences in species, protocols, sequencing platforms, annotation depth and cell composition. Fifth, scoring results depend on curated marker sets and thresholds, which should be refined as additional reference datasets become available.",
  "",
  "## Conclusion",
  "",
  "In summary, this study provides a reproducible transcriptomic framework for evaluating dopaminergic graft-like competence and safety-risk-associated states across public single-cell datasets. By jointly modelling DA/A9/A10-like molecular identity, projection-associated molecular competence and safety-risk-associated transcriptional programs, the framework prioritizes candidate graft-like states while preserving clear boundaries around what can and cannot be concluded from transcriptomic data alone."
)

writeLines(discussion_en, discussion_en_md)

stamp("生成中文 discussion 解释版。")

discussion_cn <- c(
  "# 中文 Discussion 草稿",
  "",
  "## 主要发现",
  "",
  paste0(
    "本研究建立了一个单细胞转录组分析框架，用于在公开 PD cell replacement / graft 相关数据集中联合评估 dopaminergic graft-like molecular competence 和 safety-risk-associated transcriptional states。经过审计后，下游定量结论基于 ",
    n_scored_objects,
    " 个成功评分对象，共 ",
    n_scored_cells,
    " 个细胞。核心发现是：DA/projection-associated molecular competence 和 safety-risk-associated transcriptional programs 在不同数据集中并不均一，而是形成了明显的 dataset-level 和 group-level 差异。"
  ),
  "",
  paste0(
    "DA reference dataset ",
    best_dataset$dataset,
    " 表现出最有利的 overall profile 和最高 favorable index。这个结果符合预期，因为它是 DA target/reference population，同时也为 graft-associated datasets 的比较提供了 positive anchor。在 graft-associated datasets 中，",
    second_dataset$dataset,
    " 展现出较有利的 DA/projection competence 与低 safety-risk signal 的平衡。相反，",
    highest_safety_dataset$dataset,
    " 展现最高 safety-risk-associated transcriptional score，说明其中至少部分 graft-related cells 具有 mixed 或 risk-associated transcriptional states。"
  ),
  "",
  "## DA-like identity 和 A9/A10-like molecular bias",
  "",
  "多巴胺能细胞替代治疗的核心目标之一，是获得具有合适 DA-like molecular identity 的 grafted neurons。本研究将 DA-like identity、DA functional machinery、A9-like molecular bias、A10-like molecular bias、neuronal maturation 和 projection-associated molecular competence 分开评分。这样做很重要，因为一个细胞状态可以表达部分 DA marker，但不一定同时具有良好的 projection-associated molecular competence 或低 safety-risk profile。",
  "",
  "需要注意的是，本研究中的 A9-like / A10-like 只是基于 marker signature 的 molecular bias，不能等同于已经证明了 substantia nigra 或 VTA 的真实解剖/功能身份。",
  "",
  "## Projection-associated molecular competence",
  "",
  "Projection-associated molecular competence score 主要用于捕捉 neurite maturation、axon guidance 和 synaptic machinery 相关转录程序。它可以从转录组层面判断某些 graft-like states 是否具备与神经元成熟和潜在连接能力相关的分子特征。但它不能证明真实解剖投射、retrograde connectivity 或 functional integration。因此，写文章时只能说 projection-associated molecular competence，不能说 real projection。",
  "",
  "## Safety-risk-associated transcriptional states",
  "",
  paste0(
    "第二个核心模块使用 proliferation、progenitor、pluripotency/immature、stress 和 stromal-associated components 计算 safety-risk-associated transcriptional states。在 ",
    n_contrast_groups,
    " 个 contrasted groups 中，我们识别到 ",
    n_ideal,
    " 个 ideal-like DA/projection-high and safety-low groups，",
    n_high_risk,
    " 个 high safety-risk and low-DA groups，以及 ",
    n_mixed,
    " 个 mixed DA/projection-with-risk groups。这个分类有助于区分更理想的 graft-like states 和需要重点审查的 immature/proliferative states。"
  ),
  "",
  "但是 safety-risk score 不能直接等同于肿瘤形成风险或临床安全性。它只能说明这些 groups 富集了 proliferation、progenitor 或 immature/pluripotency-related transcriptional programs，真实安全性仍需要实验验证。",
  "",
  "## 生物学和转化意义",
  "",
  "这个框架的价值在于，它不是只问细胞有没有 DA marker，而是进一步问：DA-like identity 是否同时伴随 projection-associated molecular competence，以及是否缺乏明显 safety-risk-associated transcriptional signal。这种联合评估更适合用于 graft quality assessment。",
  "",
  "结果也说明，不同 graft-related datasets 可能包含 favorable、risk-associated 和 mixed states 的不同组合。这些差异可能来自 differentiation protocol、graft maturation stage、host environment、sampling time 或技术差异。未来可以用这个框架比较新的 differentiation conditions，筛选 candidate graft preparations，或者训练 favorable vs risk-associated graft-like state 的预测模型。",
  "",
  "## 局限性",
  "",
  "本研究有几个局限。第一，这是一个计算分析，基于公开 transcriptomic datasets，没有直接实验验证。第二，projection-associated molecular competence 来自基因表达，不能证明真实投射或功能连接。第三，safety-risk-associated transcriptional state 不能证明肿瘤形成或临床安全性。第四，跨数据集比较可能受到 species、protocol、sequencing platform、annotation depth 和 cell composition 的影响。第五，评分依赖 curated marker sets 和 thresholds，后续可以随着更多 reference datasets 继续优化。",
  "",
  "## 总结",
  "",
  "总之，本研究提供了一个可复现的转录组框架，用于评估公开单细胞数据中的 dopaminergic graft-like competence 和 safety-risk-associated states。通过联合建模 DA/A9/A10-like molecular identity、projection-associated molecular competence 和 safety-risk-associated transcriptional programs，该框架可以帮助筛选候选 graft-like states，同时避免超出 transcriptomic evidence 的过度结论。"
)

writeLines(discussion_cn, discussion_cn_md)

stamp("生成 manuscript structure and figure plan。")

structure <- c(
  "# Manuscript structure and figure plan",
  "",
  paste0("## Working title"),
  WORKING_TITLE,
  "",
  "## Short title",
  SHORT_TITLE,
  "",
  "## Proposed manuscript structure",
  "",
  "### Introduction",
  "1. Parkinson's disease and the need for dopaminergic cell replacement.",
  "2. Challenge: grafted cells must acquire DA-like neuronal competence while minimizing immature/proliferative/off-target states.",
  "3. Gap: public single-cell graft datasets lack a unified framework for joint competence and safety-risk modelling.",
  "4. Aim: build a reproducible transcriptomic framework for DA/A9/A10-like competence, projection-associated molecular competence and safety-risk-associated state scoring.",
  "",
  "### Results",
  "1. Construction and QC of a multi-dataset PD graft/cell replacement transcriptomic resource.",
  "2. Marker panel construction and conservative annotation of candidate cell states.",
  "3. DA/A9/A10-like identity and projection-associated molecular competence scoring.",
  "4. Safety-risk-associated transcriptional state scoring.",
  "5. Joint DA/projection versus safety-risk contrast identifies favorable, risk-associated and mixed graft-like states.",
  "",
  "### Discussion",
  "1. Joint competence/risk modelling provides more information than DA markers alone.",
  "2. GSE233885 emerges as a favorable graft-associated dataset profile, while GSE204796/GSE132758 contain stronger mixed/risk-associated signals.",
  "3. A9/A10-like bias is heterogeneous and should be described as molecular bias, not definitive anatomical identity.",
  "4. Safety-risk scoring is a prioritization framework, not proof of tumorigenicity.",
  "5. Limitations and future validation.",
  "",
  "## Figure plan",
  "",
  "### Figure 1. Dataset processing and annotation workflow",
  "- Panel A: project workflow from public datasets to scoring modules.",
  "- Panel B: object QC and retained cells.",
  "- Panel C: marker panel categories.",
  "- Panel D: conservative annotation summary.",
  "",
  "### Figure 2. DA/projection competence and safety-risk contrast",
  "- Panel A: dataset-level DA/projection competence versus safety-risk scatter.",
  "- Panel B: favorable index ranking.",
  "- Panel C: candidate class composition by dataset.",
  "- Panel D: A9/A10-like molecular bias composition.",
  "- Panel E: top story candidate groups heatmap/tile plot.",
  "",
  "### Figure 3. Detailed DA/A9/A10/projection-associated molecular competence",
  "- Candidate DA-like groups and their DA core/A9/A10/projection scores.",
  "- Could include dotplot/heatmap of TH, DDC, SLC6A3, SLC18A2, ALDH1A1, KCNJ6, SOX6, CALB1, OTX2, SNAP25, SYT1, STMN2.",
  "",
  "### Figure 4. Safety-risk-associated transcriptional states",
  "- Candidate risk groups and their cell-cycle/progenitor/pluripotency/stress scores.",
  "- Could include MKI67, TOP2A, PCNA, SOX2, NES, POU5F1, NANOG, FOS/JUN/HSPA genes.",
  "",
  "### Figure 5. Predictive modelling module",
  "- Later module: ideal graft-like classifier and safety-risk classifier.",
  "- Include feature importance, ROC/AUC, cross-validation and external validation if available.",
  "",
  "## Tables",
  "- Table 1: Dataset sources and roles.",
  "- Table 2: Signature gene sets.",
  "- Table 3: Candidate DA/projection-high safety-low groups.",
  "- Table 4: Candidate safety-risk-associated groups.",
  "- Supplementary tables: all marker scores, audit records and failed/unscored objects."
)

writeLines(structure, manuscript_structure_md)

stamp("生成 title, keywords and highlights。")

title_keywords <- c(
  "# Title, keywords and highlights",
  "",
  "## Candidate titles",
  "",
  "1. Single-cell transcriptomic modelling identifies dopaminergic graft-like competence and safety-risk-associated states in Parkinsonian cell replacement datasets",
  "",
  "2. Joint modelling of dopaminergic molecular competence and safety-risk states in public Parkinson's disease graft single-cell datasets",
  "",
  "3. A transcriptomic framework for prioritizing dopaminergic graft-like cell states by DA/projection competence and safety-risk-associated signatures",
  "",
  "## Keywords",
  "",
  "- Parkinson's disease",
  "- dopaminergic neuron",
  "- cell replacement therapy",
  "- single-cell RNA-seq",
  "- graft safety",
  "- A9/A10 molecular identity",
  "- projection-associated molecular competence",
  "- safety-risk-associated transcriptional state",
  "- transcriptomic modelling",
  "",
  "## Highlights",
  "",
  "- A reproducible single-cell framework was built to evaluate dopaminergic graft-like competence across public datasets.",
  "- DA-like, A9/A10-like and projection-associated molecular competence scores were jointly modelled with safety-risk-associated transcriptional signatures.",
  "- GSE178265_DA_01B provided a DA-like reference anchor, while GSE233885 showed a favorable graft-associated DA/projection-high and safety-low profile.",
  "- GSE204796 and GSE132758 displayed stronger mixed or safety-risk-associated transcriptional states.",
  "- Projection-associated molecular competence and safety-risk-associated transcriptional state are computational evidence layers and require experimental validation."
)

writeLines(title_keywords, title_keywords_md)

stamp("生成 06D report。")

report_lines <- c(
  "06D discussion, abstract and manuscript structure draft report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input summary:",
  paste0("Datasets in overview: ", nrow(dataset_dt)),
  paste0("Scored objects: ", n_scored_objects),
  paste0("Scored cells: ", n_scored_cells),
  paste0("Contrasted groups: ", n_contrast_groups),
  paste0("Ideal groups: ", n_ideal),
  paste0("High-risk groups: ", n_high_risk),
  paste0("Mixed groups: ", n_mixed),
  "",
  "Key dataset results:",
  paste0("Best favorable dataset: ", best_dataset$dataset, " (", fmt(best_dataset$favorable_index_06A), ")"),
  paste0("Second favorable dataset: ", second_dataset$dataset, " (", fmt(second_dataset$favorable_index_06A), ")"),
  paste0("Highest DA/projection dataset: ", highest_da_dataset$dataset, " (", fmt(highest_da_dataset$mean_DA_projection_competence), ")"),
  paste0("Highest safety-risk dataset: ", highest_safety_dataset$dataset, " (", fmt(highest_safety_dataset$mean_safety_risk_composite_05B), ")"),
  "",
  "Output files:",
  paste0("Abstract EN: ", abstract_en_md),
  paste0("Abstract CN: ", abstract_cn_md),
  paste0("Discussion EN: ", discussion_en_md),
  paste0("Discussion CN: ", discussion_cn_md),
  paste0("Manuscript structure: ", manuscript_structure_md),
  paste0("Limitations/safe wording: ", limitations_csv),
  paste0("Title/keywords/highlights: ", title_keywords_md),
  "",
  "Next step:",
  "07A_ML_DATASET_PREPARATION_FOR_IDEAL_AND_SAFETY_MODELS.R",
  "",
  "Journal-rigor note:",
  "06D drafts intentionally avoid real projection/proven safety claims. They should be manually edited before manuscript use."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("06D discussion, abstract and manuscript structure 运行结束\n")
cat("============================================================\n\n")

cat("Datasets in overview：", nrow(dataset_dt), "\n")
cat("Scored objects：", n_scored_objects, "\n")
cat("Scored cells：", n_scored_cells, "\n")
cat("Best favorable dataset：", best_dataset$dataset, "\n")
cat("Second favorable dataset：", second_dataset$dataset, "\n")
cat("Highest DA/projection dataset：", highest_da_dataset$dataset, "\n")
cat("Highest safety-risk dataset：", highest_safety_dataset$dataset, "\n\n")

cat("输出文件：\n")
cat(abstract_en_md, "\n")
cat(abstract_cn_md, "\n")
cat(discussion_en_md, "\n")
cat(discussion_cn_md, "\n")
cat(manuscript_structure_md, "\n")
cat(limitations_csv, "\n")
cat(title_keywords_md, "\n")
cat(report_txt, "\n\n")

cat("✅ 06D discussion, abstract and manuscript structure draft 完成。\n")
cat("下一步进入 07A：准备 ML 数据集，用于 ideal graft-like model 和 safety-risk model。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

MIN_GROUPS_PER_CLASS_WARNING <- 10

cat("\n============================================================\n")
cat("07A V3：contrast-based ML dataset preparation\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 07A V3。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_04B_group <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_category_scores.csv")
input_05B_contrast <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")
input_05A_group <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_05B_group_safety <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_group_safety_risk_scores.csv")

out_tables_dir <- file.path(tables_dir, "07A_ML_dataset_preparation")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

ml_master_csv <- file.path(out_tables_dir, "07A_group_level_ML_master_table.csv")
ideal_train_csv <- file.path(out_tables_dir, "07A_ideal_graft_like_model_training_table.csv")
safety_train_csv <- file.path(out_tables_dir, "07A_safety_risk_model_training_table.csv")
feature_dictionary_csv <- file.path(out_tables_dir, "07A_feature_dictionary.csv")
label_definition_csv <- file.path(out_tables_dir, "07A_label_definition_table.csv")
split_plan_csv <- file.path(out_tables_dir, "07A_dataset_split_recommendation.csv")
class_balance_csv <- file.path(out_tables_dir, "07A_class_balance_summary.csv")
qc_audit_csv <- file.path(out_tables_dir, "07A_ML_dataset_QC_audit.csv")
merge_audit_csv <- file.path(out_tables_dir, "07A_V3_merge_audit.csv")
report_txt <- file.path(reports_dir, "07A_ML_dataset_preparation_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

num <- function(x) suppressWarnings(as.numeric(x))

make_key <- function(dataset, object_id, group_id) {
  paste(dataset, object_id, as.character(group_id), sep = "||")
}

wide_category_scores <- function(group_dt) {
  dcast(
    group_dt,
    dataset + object_id + group_id ~ category,
    value.var = "mean_score",
    fun.aggregate = max,
    fill = NA_real_
  )
}

safe_add_col <- function(dt, col, value = NA_character_) {
  if (!col %in% colnames(dt)) dt[[col]] <- value
  dt
}

stamp("读取 05B contrast table 作为 ML master base。")

contrast <- as.data.table(read_csv_required(input_05B_contrast))
g04B <- as.data.table(read_csv_optional(input_04B_group))
g05A <- as.data.table(read_csv_optional(input_05A_group))
g05B <- as.data.table(read_csv_optional(input_05B_group_safety))

needed_contrast <- c("dataset", "object_id", "group_id", "safety_contrast_class_05B")
if (!all(needed_contrast %in% colnames(contrast))) {
  stop("05B contrast table 缺少必要列：", paste(setdiff(needed_contrast, colnames(contrast)), collapse = ", "))
}

contrast[, group_id := as.character(group_id)]
contrast[, group_key := make_key(dataset, object_id, group_id)]

stamp("构建 contrast-based ML master。")

master <- copy(contrast)

for (cc in c("dataset", "object_id", "group_id", "group_key")) {
  if (!cc %in% colnames(master)) stop("master 缺少必要列：", cc)
}

if (!"n_cells" %in% colnames(master)) {

  if ("n_cells_05B" %in% colnames(master)) {
    master[, n_cells := n_cells_05B]
  } else if ("n_cells_05A" %in% colnames(master)) {
    master[, n_cells := n_cells_05A]
  } else {
    master[, n_cells := NA_integer_]
  }
}

needed_scores <- c(
  "DA_like_composite_score",
  "projection_competence_composite_score",
  "DA_projection_competence_composite_score",
  "A9_minus_A10_score_05A",
  "safety_risk_composite_05B",
  "safety_cell_cycle_score_05B",
  "safety_progenitor_score_05B",
  "safety_pluripotency_score_05B",
  "safety_stress_score_05B",
  "safety_ecm_score_05B",
  "safety_vascular_score_05B"
)

for (sc in needed_scores) {
  if (!sc %in% colnames(master)) master[[sc]] <- NA_real_
  master[[sc]] <- num(master[[sc]])
}

stamp("尝试 merge 04B marker-category features。")

marker_merge_status <- "not_attempted"
n_marker_features <- 0L
n_marker_matched <- 0L

if (nrow(g04B) > 0 && all(c("dataset", "object_id", "group_id", "category", "mean_score") %in% colnames(g04B))) {
  g04B[, group_id := as.character(group_id)]

  marker_wide <- wide_category_scores(g04B)

  marker_id_cols <- c("dataset", "object_id", "group_id")
  marker_feature_cols <- setdiff(colnames(marker_wide), marker_id_cols)

  for (col in marker_feature_cols) {
    setnames(marker_wide, col, paste0("marker_", col))
  }

  marker_wide[, group_key := make_key(dataset, object_id, group_id)]

  before_rows <- nrow(master)

  master <- merge(
    master,
    marker_wide[, c("group_key", paste0("marker_", marker_feature_cols)), with = FALSE],
    by = "group_key",
    all.x = TRUE
  )

  n_marker_features <- length(marker_feature_cols)
  marker_cols_final <- paste0("marker_", marker_feature_cols)
  n_marker_matched <- sum(rowSums(!is.na(master[, marker_cols_final, with = FALSE])) > 0)

  marker_merge_status <- "attempted"
} else {
  marker_merge_status <- "04B_marker_table_missing_or_incomplete"
}

stamp("定义 ideal / safety marker-rule-derived labels。")

master[
  ,
  ideal_graft_like_weak_label := fifelse(
    safety_contrast_class_05B == "ideal_DA_projection_high_safety_low",
    1L,
    fifelse(
      safety_contrast_class_05B %in% c(
        "high_safety_risk_low_DA",
        "mixed_DA_or_projection_with_safety_risk"
      ),
      0L,
      NA_integer_
    )
  )
]

master[
  ,
  safety_risk_weak_label := fifelse(
    safety_contrast_class_05B %in% c(
      "high_safety_risk_low_DA",
      "mixed_DA_or_projection_with_safety_risk"
    ),
    1L,
    fifelse(
      safety_contrast_class_05B %in% c(
        "ideal_DA_projection_high_safety_low",
        "projection_competence_without_DA_low_safety"
      ),
      0L,
      NA_integer_
    )
  )
]

master[, ML_label_source := "rule_derived_weak_label_from_05B_contrast"]
master[, ML_claim_boundary := "Marker-rule-derived labels are rule-derived from transcriptomic scores; they are not experimental ground truth."]

master[, has_required_DA_projection_scores := !is.na(DA_like_composite_score) & !is.na(projection_competence_composite_score)]
master[, has_required_safety_scores := !is.na(safety_risk_composite_05B)]

stamp("定义 ML feature columns。")

leakage_cols <- c(
  "DA_like_composite_score",
  "projection_competence_composite_score",
  "DA_projection_competence_composite_score",
  "safety_risk_composite_05B",
  "safety_cell_cycle_score_05B",
  "safety_progenitor_score_05B",
  "safety_pluripotency_score_05B",
  "safety_stress_score_05B",
  "safety_ecm_score_05B",
  "safety_vascular_score_05B"
)

numeric_cols <- names(master)[vapply(master, is.numeric, logical(1))]

exclude_numeric <- c(
  "ideal_graft_like_weak_label",
  "safety_risk_weak_label",
  "n_cells",
  "n_cells_05A",
  "n_cells_05B",
  "total_groups_dataset",
  "group_fraction"
)

full_feature_cols <- setdiff(numeric_cols, exclude_numeric)

primary_feature_cols <- setdiff(full_feature_cols, leakage_cols)

if (length(primary_feature_cols) == 0L) {
  primary_feature_cols <- full_feature_cols
}

stamp("输出 ML master 和 training tables。")

atomic_write_csv(as.data.frame(master), ml_master_csv)

id_cols <- intersect(
  c(
    "dataset", "object_id", "group_id", "group_key",
    "n_cells",
    "annotation_04D_v1",
    "safety_contrast_class_05B",
    "A9_A10_bias_label_05B",
    "story_priority_05B"
  ),
  colnames(master)
)

common_meta_cols <- intersect(
  c(
    "ML_label_source",
    "ML_claim_boundary",
    "has_required_DA_projection_scores",
    "has_required_safety_scores"
  ),
  colnames(master)
)

ideal_train <- master[
  !is.na(ideal_graft_like_weak_label) &
    has_required_DA_projection_scores == TRUE &
    has_required_safety_scores == TRUE
]

ideal_cols <- unique(c(
  id_cols,
  "ideal_graft_like_weak_label",
  primary_feature_cols,
  common_meta_cols
))

ideal_cols <- ideal_cols[ideal_cols %in% colnames(ideal_train)]
ideal_train <- ideal_train[, ideal_cols, with = FALSE]

safety_train <- master[
  !is.na(safety_risk_weak_label) &
    has_required_DA_projection_scores == TRUE &
    has_required_safety_scores == TRUE
]

safety_cols <- unique(c(
  id_cols,
  "safety_risk_weak_label",
  primary_feature_cols,
  common_meta_cols
))

safety_cols <- safety_cols[safety_cols %in% colnames(safety_train)]
safety_train <- safety_train[, safety_cols, with = FALSE]

atomic_write_csv(as.data.frame(ideal_train), ideal_train_csv)
atomic_write_csv(as.data.frame(safety_train), safety_train_csv)

stamp("生成 feature dictionary / label definitions。")

feature_dictionary <- rbindlist(
  list(
    data.table(
      feature = primary_feature_cols,
      feature_set = "primary_feature_set",
      recommended_primary_ML = TRUE,
      leakage_warning = fifelse(
        primary_feature_cols %in% leakage_cols,
        "Potential label leakage; use only for exploratory marker-rule-derived model.",
        "Lower leakage risk; still marker-rule-derived-derived context."
      )
    ),
    data.table(
      feature = setdiff(full_feature_cols, primary_feature_cols),
      feature_set = "descriptive_or_leakage_sensitive_feature_set",
      recommended_primary_ML = FALSE,
      leakage_warning = "Potential label leakage or descriptive-only feature."
    )
  ),
  fill = TRUE
)

feature_dictionary[
  ,
  interpretation := fifelse(
    grepl("^marker_", feature),
    "04B marker-category score.",
    fifelse(
      feature %in% leakage_cols,
      "Composite score used directly or indirectly in marker-rule-derived definition.",
      "Numeric score feature from 05A/05B contrast table."
    )
  )
]

atomic_write_csv(as.data.frame(feature_dictionary), feature_dictionary_csv)

label_definition <- data.frame(
  label_name = c(
    "ideal_graft_like_weak_label",
    "safety_risk_weak_label"
  ),
  positive_class = c(
    "safety_contrast_class_05B == ideal_DA_projection_high_safety_low",
    "safety_contrast_class_05B in high_safety_risk_low_DA or mixed_DA_or_projection_with_safety_risk"
  ),
  negative_class = c(
    "safety_contrast_class_05B in high_safety_risk_low_DA or mixed_DA_or_projection_with_safety_risk",
    "safety_contrast_class_05B in ideal_DA_projection_high_safety_low or projection_competence_without_DA_low_safety"
  ),
  excluded_class = c(
    "lower_priority_or_mixed and ambiguous/unscored groups",
    "lower_priority_or_mixed and ambiguous/unscored groups"
  ),
  evidence_boundary = c(
    "Marker-rule-derived label; not experimentally validated ideal graft outcome.",
    "Marker-rule-derived label; not proof of tumorigenicity or clinical safety risk."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(label_definition, label_definition_csv)

stamp("生成 split recommendation。")

datasets <- sort(unique(master$dataset))

split_plan <- data.frame(
  split_strategy = character(),
  train_datasets = character(),
  test_datasets = character(),
  use_case = character(),
  caution = character(),
  stringsAsFactors = FALSE
)

for (ds in datasets) {
  train_ds <- setdiff(datasets, ds)

  split_plan <- rbind(
    split_plan,
    data.frame(
      split_strategy = "leave_one_dataset_out",
      train_datasets = paste(train_ds, collapse = ";"),
      test_datasets = ds,
      use_case = "Cross-dataset robustness check.",
      caution = "Dataset number is small; marker-rule-derived labels are rule-derived.",
      stringsAsFactors = FALSE
    )
  )
}

split_plan <- rbind(
  split_plan,
  data.frame(
    split_strategy = "exploratory_random_split_stratified_by_label",
    train_datasets = paste(datasets, collapse = ";"),
    test_datasets = paste(datasets, collapse = ";"),
    use_case = "Internal exploratory benchmark only.",
    caution = "May overestimate performance due to dataset leakage and marker-rule-derived circularity.",
    stringsAsFactors = FALSE
  )
)

atomic_write_csv(split_plan, split_plan_csv)

stamp("计算 class balance 和 audit。")

class_balance_ideal <- ideal_train[
  ,
  .(
    n_groups = .N,
    total_cells = sum(n_cells, na.rm = TRUE)
  ),
  by = .(dataset, ideal_graft_like_weak_label)
]
class_balance_ideal[, model_task := "ideal_graft_like_model"]
setnames(class_balance_ideal, "ideal_graft_like_weak_label", "class_label")

class_balance_safety <- safety_train[
  ,
  .(
    n_groups = .N,
    total_cells = sum(n_cells, na.rm = TRUE)
  ),
  by = .(dataset, safety_risk_weak_label)
]
class_balance_safety[, model_task := "safety_risk_model"]
setnames(class_balance_safety, "safety_risk_weak_label", "class_label")

class_balance <- rbindlist(
  list(
    class_balance_ideal[, .(model_task, dataset, class_label, n_groups, total_cells)],
    class_balance_safety[, .(model_task, dataset, class_label, n_groups, total_cells)]
  ),
  fill = TRUE
)

atomic_write_csv(as.data.frame(class_balance), class_balance_csv)

ideal_pos <- sum(ideal_train$ideal_graft_like_weak_label == 1, na.rm = TRUE)
ideal_neg <- sum(ideal_train$ideal_graft_like_weak_label == 0, na.rm = TRUE)
safety_pos <- sum(safety_train$safety_risk_weak_label == 1, na.rm = TRUE)
safety_neg <- sum(safety_train$safety_risk_weak_label == 0, na.rm = TRUE)

qc_audit <- data.frame(
  metric = c(
    "ML_master_groups",
    "ideal_training_groups",
    "ideal_positive_groups",
    "ideal_negative_groups",
    "safety_training_groups",
    "safety_positive_groups",
    "safety_negative_groups",
    "primary_numeric_features",
    "full_numeric_features",
    "datasets_represented",
    "marker_merge_status",
    "marker_features_available",
    "groups_with_marker_features",
    "warning_ideal_positive_lt_min",
    "warning_ideal_negative_lt_min",
    "warning_safety_positive_lt_min",
    "warning_safety_negative_lt_min"
  ),
  value = c(
    nrow(master),
    nrow(ideal_train),
    ideal_pos,
    ideal_neg,
    nrow(safety_train),
    safety_pos,
    safety_neg,
    length(primary_feature_cols),
    length(full_feature_cols),
    length(unique(master$dataset)),
    marker_merge_status,
    n_marker_features,
    n_marker_matched,
    ideal_pos < MIN_GROUPS_PER_CLASS_WARNING,
    ideal_neg < MIN_GROUPS_PER_CLASS_WARNING,
    safety_pos < MIN_GROUPS_PER_CLASS_WARNING,
    safety_neg < MIN_GROUPS_PER_CLASS_WARNING
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)

merge_audit <- data.frame(
  item = c(
    "base_table",
    "base_rows",
    "marker_merge_status",
    "marker_features_available",
    "groups_with_marker_features",
    "training_table_reason"
  ),
  value = c(
    "05B_DA_projection_vs_safety_contrast_groups.csv",
    nrow(contrast),
    marker_merge_status,
    n_marker_features,
    n_marker_matched,
    "V3 uses 05B contrast as base to preserve marker-rule-derived labels and avoid group_id merge loss."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(merge_audit, merge_audit_csv)

balance_lines <- if (nrow(class_balance) > 0) {
  apply(as.data.frame(class_balance), 1, function(x) {
    paste0(
      x[["model_task"]],
      " / ",
      x[["dataset"]],
      " / class ",
      x[["class_label"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

report_lines <- c(
  "07A V3 contrast-based ML dataset preparation report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("ML master groups: ", nrow(master)),
  paste0("Ideal model training groups: ", nrow(ideal_train)),
  paste0("Ideal positive groups: ", ideal_pos),
  paste0("Ideal negative groups: ", ideal_neg),
  paste0("Safety model training groups: ", nrow(safety_train)),
  paste0("Safety positive groups: ", safety_pos),
  paste0("Safety negative groups: ", safety_neg),
  paste0("Primary numeric features: ", length(primary_feature_cols)),
  paste0("Full numeric features: ", length(full_feature_cols)),
  paste0("Datasets represented: ", paste(sort(unique(master$dataset)), collapse = "; ")),
  paste0("Marker merge status: ", marker_merge_status),
  paste0("Groups with marker features: ", n_marker_matched),
  "",
  "Class balance detail:",
  balance_lines,
  "",
  "Output files:",
  paste0("ML master table: ", ml_master_csv),
  paste0("Ideal training table: ", ideal_train_csv),
  paste0("Safety training table: ", safety_train_csv),
  paste0("Feature dictionary: ", feature_dictionary_csv),
  paste0("Label definition: ", label_definition_csv),
  paste0("Split recommendation: ", split_plan_csv),
  paste0("Class balance: ", class_balance_csv),
  paste0("QC audit: ", qc_audit_csv),
  paste0("Merge audit: ", merge_audit_csv),
  "",
  "Next step:",
  "07B_TRAIN_WEAK_LABEL_ML_MODELS.R",
  "",
  "Journal-rigor note:",
  "V3 fixes label loss by using 05B contrast as the master table. Labels remain rule-derived marker-rule-derived labels. Any downstream model must be reported as exploratory marker-rule-derived classification, not experimental prediction."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("07A V3 contrast-based ML dataset preparation 运行结束\n")
cat("============================================================\n\n")

cat("ML master groups：", nrow(master), "\n")
cat("Ideal model training groups：", nrow(ideal_train), "\n")
cat("Ideal positive groups：", ideal_pos, "\n")
cat("Ideal negative groups：", ideal_neg, "\n")
cat("Safety model training groups：", nrow(safety_train), "\n")
cat("Safety positive groups：", safety_pos, "\n")
cat("Safety negative groups：", safety_neg, "\n")
cat("Primary numeric features：", length(primary_feature_cols), "\n")
cat("Full numeric features：", length(full_feature_cols), "\n")
cat("Datasets represented：", length(unique(master$dataset)), "\n")
cat("Marker merge status：", marker_merge_status, "\n")
cat("Groups with marker features：", n_marker_matched, "\n\n")

cat("输出文件：\n")
cat(ml_master_csv, "\n")
cat(ideal_train_csv, "\n")
cat(safety_train_csv, "\n")
cat(feature_dictionary_csv, "\n")
cat(label_definition_csv, "\n")
cat(split_plan_csv, "\n")
cat(class_balance_csv, "\n")
cat(qc_audit_csv, "\n")
cat(merge_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 07A V3 contrast-based ML dataset preparation 完成。\n")
cat("下一步进入 07B：训练 exploratory marker-rule-derived prioritization model models。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260714
K_FOLDS <- 5
MAX_FEATURES_GLM <- 20
MIN_CLASS_PER_DATASET_FOR_LODO <- 2

TRAIN_RPART_IF_AVAILABLE <- TRUE

cat("\n============================================================\n")
cat("07B：exploratory marker-rule-derived prioritization model models\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 07B。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

HAS_RPART <- requireNamespace("rpart", quietly = TRUE)

if (!HAS_RPART) {
  message("未检测到 rpart；07B 将只训练 logistic marker-rule-derived model。")
}

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")
figures_dir <- file.path(PROJECT_DIR, "04_figures")

input_ideal_train <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_ideal_graft_like_model_training_table.csv")
input_safety_train <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_safety_risk_model_training_table.csv")
input_feature_dict <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_feature_dictionary.csv")
input_class_balance <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_class_balance_summary.csv")
input_qc07A <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_ML_dataset_QC_audit.csv")

out_tables_dir <- file.path(tables_dir, "07B_weak_label_ML_models")
out_figures_dir <- file.path(figures_dir, "07B_weak_label_ML_models")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

internal_pred_csv <- file.path(out_tables_dir, "07B_internal_CV_predictions.csv")
lodo_pred_csv <- file.path(out_tables_dir, "07B_leave_one_dataset_out_predictions.csv")
performance_csv <- file.path(out_tables_dir, "07B_model_performance_summary.csv")
feature_importance_csv <- file.path(out_tables_dir, "07B_feature_importance_summary.csv")
selected_features_csv <- file.path(out_tables_dir, "07B_selected_features_by_model.csv")
qc_audit_csv <- file.path(out_tables_dir, "07B_ML_training_QC_audit.csv")
report_txt <- file.path(reports_dir, "07B_exploratory_weak_label_ML_models_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

num <- function(x) suppressWarnings(as.numeric(x))

sigmoid <- function(x) {
  1 / (1 + exp(-pmax(pmin(x, 30), -30)))
}

auc_base <- function(labels, scores) {
  labels <- as.integer(labels)
  scores <- as.numeric(scores)

  ok <- !is.na(labels) & !is.na(scores)
  labels <- labels[ok]
  scores <- scores[ok]

  if (length(unique(labels)) < 2) return(NA_real_)

  pos <- scores[labels == 1]
  neg <- scores[labels == 0]

  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)

  r <- rank(c(pos, neg), ties.method = "average")
  n_pos <- length(pos)
  n_neg <- length(neg)

  auc <- (sum(r[seq_len(n_pos)]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  as.numeric(auc)
}

binary_metrics <- function(labels, probs, threshold = 0.5) {
  labels <- as.integer(labels)
  probs <- as.numeric(probs)

  ok <- !is.na(labels) & !is.na(probs)
  labels <- labels[ok]
  probs <- probs[ok]

  if (length(labels) == 0) {
    return(data.frame(
      n = 0, positives = 0, negatives = 0,
      accuracy = NA_real_, sensitivity = NA_real_, specificity = NA_real_,
      precision = NA_real_, f1 = NA_real_, auc = NA_real_
    ))
  }

  pred <- ifelse(probs >= threshold, 1L, 0L)

  tp <- sum(pred == 1 & labels == 1)
  tn <- sum(pred == 0 & labels == 0)
  fp <- sum(pred == 1 & labels == 0)
  fn <- sum(pred == 0 & labels == 1)

  acc <- (tp + tn) / length(labels)
  sens <- ifelse((tp + fn) == 0, NA_real_, tp / (tp + fn))
  spec <- ifelse((tn + fp) == 0, NA_real_, tn / (tn + fp))
  prec <- ifelse((tp + fp) == 0, NA_real_, tp / (tp + fp))
  f1 <- ifelse(is.na(prec) | is.na(sens) | (prec + sens) == 0, NA_real_, 2 * prec * sens / (prec + sens))
  auc <- auc_base(labels, probs)

  data.frame(
    n = length(labels),
    positives = sum(labels == 1),
    negatives = sum(labels == 0),
    accuracy = acc,
    sensitivity = sens,
    specificity = spec,
    precision = prec,
    f1 = f1,
    auc = auc
  )
}

get_feature_cols <- function(dt, label_col) {
  exclude <- c(
    "dataset", "object_id", "group_id", "group_key",
    "annotation_04D_v1", "safety_contrast_class_05B",
    "A9_A10_bias_label_05B", "story_priority_05B",
    "ML_label_source", "ML_claim_boundary",
    "has_required_DA_projection_scores", "has_required_safety_scores",
    "n_cells", label_col,
    "ideal_graft_like_weak_label", "safety_risk_weak_label"
  )

  numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
  setdiff(numeric_cols, exclude)
}

median_impute_fit <- function(train_dt, feature_cols) {
  med <- sapply(train_dt[, feature_cols, with = FALSE], function(x) {
    x <- as.numeric(x)
    if (all(is.na(x))) return(0)
    median(x, na.rm = TRUE)
  })

  sds <- sapply(train_dt[, feature_cols, with = FALSE], function(x) {
    x <- as.numeric(x)
    x[is.na(x)] <- median(x, na.rm = TRUE)
    sd(x, na.rm = TRUE)
  })

  sds[is.na(sds) | sds == 0] <- 1

  list(median = med, sd = sds)
}

apply_impute_scale <- function(dt, feature_cols, prep) {
  mat <- as.matrix(dt[, feature_cols, with = FALSE])

  for (j in seq_along(feature_cols)) {
    col <- feature_cols[[j]]
    mat[, j] <- as.numeric(mat[, j])
    mat[is.na(mat[, j]), j] <- prep$median[[col]]
    mat[, j] <- (mat[, j] - prep$median[[col]]) / prep$sd[[col]]
  }

  colnames(mat) <- make.names(feature_cols)
  as.data.frame(mat, check.names = FALSE)
}

rank_features <- function(dt, label_col, feature_cols, max_features = 20) {
  labels <- as.integer(dt[[label_col]])

  scores <- sapply(feature_cols, function(fc) {
    x <- as.numeric(dt[[fc]])
    if (all(is.na(x))) return(0)
    x[is.na(x)] <- median(x, na.rm = TRUE)

    if (length(unique(labels[!is.na(labels)])) < 2) return(0)

    m1 <- mean(x[labels == 1], na.rm = TRUE)
    m0 <- mean(x[labels == 0], na.rm = TRUE)
    sd_all <- sd(x, na.rm = TRUE)
    if (is.na(sd_all) || sd_all == 0) return(0)

    abs(m1 - m0) / sd_all
  })

  scores[is.na(scores)] <- 0

  ranked <- names(sort(scores, decreasing = TRUE))
  ranked <- ranked[scores[ranked] > 0]

  if (length(ranked) == 0) {
    ranked <- feature_cols
  }

  head(ranked, min(max_features, length(ranked)))
}

make_stratified_folds <- function(labels, k = 5, seed = 1) {
  set.seed(seed)
  labels <- as.integer(labels)
  folds <- rep(NA_integer_, length(labels))

  for (cl in sort(unique(labels))) {
    idx <- which(labels == cl)
    idx <- sample(idx)
    fold_ids <- rep(seq_len(k), length.out = length(idx))
    folds[idx] <- fold_ids
  }

  folds
}

train_predict_glm <- function(train_dt, test_dt, label_col, feature_cols) {

  selected <- rank_features(train_dt, label_col, feature_cols, max_features = MAX_FEATURES_GLM)

  prep <- median_impute_fit(train_dt, selected)
  x_train <- apply_impute_scale(train_dt, selected, prep)
  x_test <- apply_impute_scale(test_dt, selected, prep)

  train_model_dt <- data.frame(
    label = as.integer(train_dt[[label_col]]),
    x_train,
    check.names = FALSE
  )

  if (length(unique(train_model_dt$label)) < 2) {
    return(list(
      prob = rep(mean(train_model_dt$label, na.rm = TRUE), nrow(test_dt)),
      selected_features = selected,
      coef_table = data.frame(feature = selected, coefficient = NA_real_),
      status = "single_class_train_fallback"
    ))
  }

  fit <- tryCatch({
    suppressWarnings(glm(label ~ ., data = train_model_dt, family = binomial()))
  }, error = function(e) {
    NULL
  })

  if (is.null(fit)) {
    p <- mean(train_model_dt$label, na.rm = TRUE)
    return(list(
      prob = rep(p, nrow(test_dt)),
      selected_features = selected,
      coef_table = data.frame(feature = selected, coefficient = NA_real_),
      status = "glm_failed_mean_fallback"
    ))
  }

  prob <- tryCatch({
    as.numeric(predict(fit, newdata = x_test, type = "response"))
  }, error = function(e) {
    rep(mean(train_model_dt$label, na.rm = TRUE), nrow(test_dt))
  })

  coef_vec <- suppressWarnings(coef(fit))
  coef_dt <- data.frame(
    feature = names(coef_vec),
    coefficient = as.numeric(coef_vec),
    stringsAsFactors = FALSE
  )
  coef_dt <- coef_dt[coef_dt$feature != "(Intercept)", , drop = FALSE]

  name_map <- data.frame(
    feature_sanitized = make.names(selected),
    feature_original = selected,
    stringsAsFactors = FALSE
  )
  coef_dt <- merge(
    coef_dt,
    name_map,
    by.x = "feature",
    by.y = "feature_sanitized",
    all.x = TRUE
  )
  coef_dt$feature <- ifelse(is.na(coef_dt$feature_original), coef_dt$feature, coef_dt$feature_original)
  coef_dt$feature_original <- NULL

  list(
    prob = prob,
    selected_features = selected,
    coef_table = coef_dt,
    status = "ok"
  )
}

train_predict_rpart <- function(train_dt, test_dt, label_col, feature_cols) {
  if (!HAS_RPART || !TRAIN_RPART_IF_AVAILABLE) {
    return(NULL)
  }

  selected <- rank_features(train_dt, label_col, feature_cols, max_features = MAX_FEATURES_GLM)

  prep <- median_impute_fit(train_dt, selected)
  x_train <- apply_impute_scale(train_dt, selected, prep)
  x_test <- apply_impute_scale(test_dt, selected, prep)

  train_model_dt <- data.frame(
    label = factor(as.integer(train_dt[[label_col]]), levels = c(0, 1)),
    x_train,
    check.names = FALSE
  )

  if (length(unique(train_model_dt$label)) < 2) return(NULL)

  fit <- tryCatch({
    rpart::rpart(
      label ~ .,
      data = train_model_dt,
      method = "class",
      control = rpart::rpart.control(cp = 0.01, minsplit = 10)
    )
  }, error = function(e) {
    NULL
  })

  if (is.null(fit)) return(NULL)

  prob <- tryCatch({
    pp <- predict(fit, newdata = x_test, type = "prob")
    as.numeric(pp[, "1"])
  }, error = function(e) {
    rep(mean(as.integer(train_dt[[label_col]]), na.rm = TRUE), nrow(test_dt))
  })

  vi <- tryCatch({
    imp <- fit$variable.importance
    data.frame(
      feature = names(imp),
      importance = as.numeric(imp),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(feature = selected, importance = NA_real_)
  })

  list(
    prob = prob,
    selected_features = selected,
    importance_table = vi,
    status = "ok"
  )
}

run_internal_cv <- function(dt, label_col, model_task) {
  dt <- as.data.table(copy(dt))
  feature_cols <- get_feature_cols(dt, label_col)

  dt <- dt[!is.na(get(label_col))]
  dt[[label_col]] <- as.integer(dt[[label_col]])

  if (length(unique(dt[[label_col]])) < 2) {
    stop("训练数据只有一个类别，无法训练：", model_task)
  }

  folds <- make_stratified_folds(dt[[label_col]], k = min(K_FOLDS, min(table(dt[[label_col]]))), seed = SEED)
  dt[, fold := folds]

  preds <- list()
  imps <- list()
  selected_records <- list()

  for (fold_id in sort(unique(folds))) {
    stamp("  ", model_task, " internal CV fold ", fold_id)

    train_dt <- dt[fold != fold_id]
    test_dt <- dt[fold == fold_id]

    glm_res <- train_predict_glm(train_dt, test_dt, label_col, feature_cols)

    pred_dt <- test_dt[
      ,
      .(
        dataset,
        object_id,
        group_id,
        group_key,
        true_label = get(label_col),
        n_cells
      )
    ]

    pred_dt[, model_task := model_task]
    pred_dt[, evaluation_type := "internal_stratified_CV"]
    pred_dt[, fold := fold_id]
    pred_dt[, algorithm := "logistic_glm"]
    pred_dt[, predicted_probability := glm_res$prob]
    pred_dt[, model_status := glm_res$status]

    preds[[length(preds) + 1L]] <- pred_dt

    imp_dt <- as.data.table(glm_res$coef_table)
    if (nrow(imp_dt) > 0) {
      imp_dt[, model_task := model_task]
      imp_dt[, evaluation_type := "internal_stratified_CV"]
      imp_dt[, fold := fold_id]
      imp_dt[, algorithm := "logistic_glm"]
      imp_dt[, importance := abs(coefficient)]
      imps[[length(imps) + 1L]] <- imp_dt
    }

    selected_records[[length(selected_records) + 1L]] <- data.table(
      model_task = model_task,
      evaluation_type = "internal_stratified_CV",
      fold = fold_id,
      algorithm = "logistic_glm",
      feature = glm_res$selected_features
    )

    rp <- train_predict_rpart(train_dt, test_dt, label_col, feature_cols)

    if (!is.null(rp)) {
      pred_rp <- copy(pred_dt)
      pred_rp[, algorithm := "rpart_tree"]
      pred_rp[, predicted_probability := rp$prob]
      pred_rp[, model_status := rp$status]

      preds[[length(preds) + 1L]] <- pred_rp

      imp_rp <- as.data.table(rp$importance_table)
      if (nrow(imp_rp) > 0) {
        imp_rp[, model_task := model_task]
        imp_rp[, evaluation_type := "internal_stratified_CV"]
        imp_rp[, fold := fold_id]
        imp_rp[, algorithm := "rpart_tree"]
        if (!"coefficient" %in% colnames(imp_rp)) imp_rp[, coefficient := NA_real_]
        imps[[length(imps) + 1L]] <- imp_rp
      }

      selected_records[[length(selected_records) + 1L]] <- data.table(
        model_task = model_task,
        evaluation_type = "internal_stratified_CV",
        fold = fold_id,
        algorithm = "rpart_tree",
        feature = rp$selected_features
      )
    }
  }

  list(
    predictions = rbindlist(preds, fill = TRUE),
    importance = rbindlist(imps, fill = TRUE),
    selected = rbindlist(selected_records, fill = TRUE)
  )
}

run_lodo <- function(dt, label_col, model_task) {
  dt <- as.data.table(copy(dt))
  feature_cols <- get_feature_cols(dt, label_col)

  dt <- dt[!is.na(get(label_col))]
  dt[[label_col]] <- as.integer(dt[[label_col]])

  preds <- list()
  imps <- list()
  selected_records <- list()

  for (ds in sort(unique(dt$dataset))) {
    test_dt <- dt[dataset == ds]
    train_dt <- dt[dataset != ds]

    if (length(unique(train_dt[[label_col]])) < 2 || length(unique(test_dt[[label_col]])) < 2) {
      stamp("  ", model_task, " LODO skip ", ds, "：train/test class 不足。")
      next
    }

    if (min(table(train_dt[[label_col]])) < MIN_CLASS_PER_DATASET_FOR_LODO) {
      stamp("  ", model_task, " LODO warning ", ds, "：训练集某类别数量偏少。")
    }

    stamp("  ", model_task, " LODO test dataset：", ds)

    glm_res <- train_predict_glm(train_dt, test_dt, label_col, feature_cols)

    pred_dt <- test_dt[
      ,
      .(
        dataset,
        object_id,
        group_id,
        group_key,
        true_label = get(label_col),
        n_cells
      )
    ]

    pred_dt[, model_task := model_task]
    pred_dt[, evaluation_type := "leave_one_dataset_out"]
    pred_dt[, test_dataset := ds]
    pred_dt[, algorithm := "logistic_glm"]
    pred_dt[, predicted_probability := glm_res$prob]
    pred_dt[, model_status := glm_res$status]

    preds[[length(preds) + 1L]] <- pred_dt

    imp_dt <- as.data.table(glm_res$coef_table)
    if (nrow(imp_dt) > 0) {
      imp_dt[, model_task := model_task]
      imp_dt[, evaluation_type := "leave_one_dataset_out"]
      imp_dt[, test_dataset := ds]
      imp_dt[, algorithm := "logistic_glm"]
      imp_dt[, importance := abs(coefficient)]
      imps[[length(imps) + 1L]] <- imp_dt
    }

    selected_records[[length(selected_records) + 1L]] <- data.table(
      model_task = model_task,
      evaluation_type = "leave_one_dataset_out",
      test_dataset = ds,
      algorithm = "logistic_glm",
      feature = glm_res$selected_features
    )

    rp <- train_predict_rpart(train_dt, test_dt, label_col, feature_cols)

    if (!is.null(rp)) {
      pred_rp <- copy(pred_dt)
      pred_rp[, algorithm := "rpart_tree"]
      pred_rp[, predicted_probability := rp$prob]
      pred_rp[, model_status := rp$status]

      preds[[length(preds) + 1L]] <- pred_rp

      imp_rp <- as.data.table(rp$importance_table)
      if (nrow(imp_rp) > 0) {
        imp_rp[, model_task := model_task]
        imp_rp[, evaluation_type := "leave_one_dataset_out"]
        imp_rp[, test_dataset := ds]
        imp_rp[, algorithm := "rpart_tree"]
        if (!"coefficient" %in% colnames(imp_rp)) imp_rp[, coefficient := NA_real_]
        imps[[length(imps) + 1L]] <- imp_rp
      }

      selected_records[[length(selected_records) + 1L]] <- data.table(
        model_task = model_task,
        evaluation_type = "leave_one_dataset_out",
        test_dataset = ds,
        algorithm = "rpart_tree",
        feature = rp$selected_features
      )
    }
  }

  list(
    predictions = if (length(preds) > 0) rbindlist(preds, fill = TRUE) else data.table(),
    importance = if (length(imps) > 0) rbindlist(imps, fill = TRUE) else data.table(),
    selected = if (length(selected_records) > 0) rbindlist(selected_records, fill = TRUE) else data.table()
  )
}

summarize_performance <- function(pred_dt) {
  if (nrow(pred_dt) == 0) return(data.table())

  perf <- pred_dt[
    ,
    {
      m <- binary_metrics(true_label, predicted_probability)
      as.data.table(m)
    },
    by = .(model_task, evaluation_type, algorithm)
  ]

  if ("test_dataset" %in% colnames(pred_dt)) {
    lodo_perf <- pred_dt[
      evaluation_type == "leave_one_dataset_out" & !is.na(test_dataset),
      {
        m <- binary_metrics(true_label, predicted_probability)
        as.data.table(m)
      },
      by = .(model_task, evaluation_type, algorithm, test_dataset)
    ]

    if (nrow(lodo_perf) > 0) {
      lodo_perf[, group_level := "per_test_dataset"]
      perf[, test_dataset := NA_character_]
      perf[, group_level := "overall"]
      perf <- rbindlist(list(perf, lodo_perf), fill = TRUE)
    }
  } else {
    perf[, test_dataset := NA_character_]
    perf[, group_level := "overall"]
  }

  perf
}

set.seed(SEED)

stamp("读取 07A V3 ML training tables。")

ideal_train <- as.data.table(read_csv_required(input_ideal_train))
safety_train <- as.data.table(read_csv_required(input_safety_train))
feature_dict <- as.data.table(read_csv_optional(input_feature_dict))
class_balance <- as.data.table(read_csv_optional(input_class_balance))
qc07A <- as.data.table(read_csv_optional(input_qc07A))

if (!"ideal_graft_like_weak_label" %in% colnames(ideal_train)) {
  stop("ideal training table 缺少 ideal_graft_like_weak_label。")
}

if (!"safety_risk_weak_label" %in% colnames(safety_train)) {
  stop("safety training table 缺少 safety_risk_weak_label。")
}

stamp("Ideal training groups：", nrow(ideal_train))
stamp("Safety training groups：", nrow(safety_train))

stamp("训练 internal stratified CV models。")

ideal_cv <- run_internal_cv(
  ideal_train,
  label_col = "ideal_graft_like_weak_label",
  model_task = "ideal_graft_like_model"
)

safety_cv <- run_internal_cv(
  safety_train,
  label_col = "safety_risk_weak_label",
  model_task = "safety_risk_model"
)

internal_preds <- rbindlist(
  list(ideal_cv$predictions, safety_cv$predictions),
  fill = TRUE
)

atomic_write_csv(as.data.frame(internal_preds), internal_pred_csv)

stamp("训练 leave-one-dataset-out exploratory models。")

ideal_lodo <- run_lodo(
  ideal_train,
  label_col = "ideal_graft_like_weak_label",
  model_task = "ideal_graft_like_model"
)

safety_lodo <- run_lodo(
  safety_train,
  label_col = "safety_risk_weak_label",
  model_task = "safety_risk_model"
)

lodo_preds <- rbindlist(
  list(ideal_lodo$predictions, safety_lodo$predictions),
  fill = TRUE
)

atomic_write_csv(as.data.frame(lodo_preds), lodo_pred_csv)

stamp("汇总 model performance。")

perf_internal <- summarize_performance(internal_preds)
perf_lodo <- summarize_performance(lodo_preds)

performance <- rbindlist(
  list(perf_internal, perf_lodo),
  fill = TRUE
)

performance[
  ,
  claim_boundary := "Exploratory marker-rule-derived classification only; labels are rule-derived and performance may be circular if features overlap with label-defining scores."
]

atomic_write_csv(as.data.frame(performance), performance_csv)

stamp("汇总 feature importance。")

importance_all <- rbindlist(
  list(
    ideal_cv$importance,
    safety_cv$importance,
    ideal_lodo$importance,
    safety_lodo$importance
  ),
  fill = TRUE
)

if (nrow(importance_all) > 0) {
  if (!"importance" %in% colnames(importance_all)) {
    importance_all[, importance := abs(coefficient)]
  }

  importance_all[, importance := as.numeric(importance)]
  importance_all[is.na(importance), importance := 0]

  feature_importance <- importance_all[
    ,
    .(
      mean_importance = mean(importance, na.rm = TRUE),
      median_importance = median(importance, na.rm = TRUE),
      max_importance = max(importance, na.rm = TRUE),
      n_times_used = .N
    ),
    by = .(model_task, algorithm, feature)
  ][order(model_task, algorithm, -mean_importance)]
} else {
  feature_importance <- data.table()
}

atomic_write_csv(as.data.frame(feature_importance), feature_importance_csv)

selected_all <- rbindlist(
  list(
    ideal_cv$selected,
    safety_cv$selected,
    ideal_lodo$selected,
    safety_lodo$selected
  ),
  fill = TRUE
)

atomic_write_csv(as.data.frame(selected_all), selected_features_csv)

stamp("生成 07B QC audit。")

ideal_pos <- sum(ideal_train$ideal_graft_like_weak_label == 1, na.rm = TRUE)
ideal_neg <- sum(ideal_train$ideal_graft_like_weak_label == 0, na.rm = TRUE)
safety_pos <- sum(safety_train$safety_risk_weak_label == 1, na.rm = TRUE)
safety_neg <- sum(safety_train$safety_risk_weak_label == 0, na.rm = TRUE)

n_internal_pred <- nrow(internal_preds)
n_lodo_pred <- nrow(lodo_preds)

best_auc_internal <- if (nrow(perf_internal) > 0) max(perf_internal$auc, na.rm = TRUE) else NA_real_
best_auc_lodo <- if (nrow(perf_lodo) > 0) max(perf_lodo$auc, na.rm = TRUE) else NA_real_

if (is.infinite(best_auc_internal)) best_auc_internal <- NA_real_
if (is.infinite(best_auc_lodo)) best_auc_lodo <- NA_real_

qc_audit <- data.frame(
  metric = c(
    "ideal_training_groups",
    "ideal_positive_groups",
    "ideal_negative_groups",
    "safety_training_groups",
    "safety_positive_groups",
    "safety_negative_groups",
    "internal_CV_prediction_rows",
    "LODO_prediction_rows",
    "algorithms_trained",
    "best_internal_CV_AUC",
    "best_LODO_AUC",
    "rpart_available",
    "claim_boundary"
  ),
  value = c(
    nrow(ideal_train),
    ideal_pos,
    ideal_neg,
    nrow(safety_train),
    safety_pos,
    safety_neg,
    n_internal_pred,
    n_lodo_pred,
    paste(sort(unique(internal_preds$algorithm)), collapse = ";"),
    round(best_auc_internal, 4),
    round(best_auc_lodo, 4),
    HAS_RPART,
    "exploratory marker-rule-derived classification only"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)

perf_lines <- if (nrow(performance) > 0) {
  apply(as.data.frame(performance), 1, function(x) {
    paste0(
      x[["model_task"]],
      " / ",
      x[["evaluation_type"]],
      " / ",
      x[["algorithm"]],
      ifelse(!is.na(x[["test_dataset"]]) && x[["test_dataset"]] != "", paste0(" / ", x[["test_dataset"]]), ""),
      ": n=",
      x[["n"]],
      "; AUC=",
      round(as.numeric(x[["auc"]]), 4),
      "; accuracy=",
      round(as.numeric(x[["accuracy"]]), 4)
    )
  })
} else {
  "none"
}

top_feature_lines <- if (nrow(feature_importance) > 0) {
  top_features <- feature_importance[
    ,
    head(.SD, 10),
    by = .(model_task, algorithm)
  ]

  apply(as.data.frame(top_features), 1, function(x) {
    paste0(
      x[["model_task"]],
      " / ",
      x[["algorithm"]],
      " / ",
      x[["feature"]],
      ": mean_importance=",
      round(as.numeric(x[["mean_importance"]]), 4)
    )
  })
} else {
  "none"
}

report_lines <- c(
  "07B exploratory marker-rule-derived prioritization model models report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Training summary:",
  paste0("Ideal training groups: ", nrow(ideal_train), " (positive=", ideal_pos, ", negative=", ideal_neg, ")"),
  paste0("Safety training groups: ", nrow(safety_train), " (positive=", safety_pos, ", negative=", safety_neg, ")"),
  paste0("Internal CV predictions: ", n_internal_pred),
  paste0("LODO predictions: ", n_lodo_pred),
  paste0("Algorithms trained: ", paste(sort(unique(internal_preds$algorithm)), collapse = "; ")),
  "",
  "Performance summary:",
  perf_lines,
  "",
  "Top feature summary:",
  top_feature_lines,
  "",
  "Output files:",
  paste0("Internal CV predictions: ", internal_pred_csv),
  paste0("LODO predictions: ", lodo_pred_csv),
  paste0("Performance summary: ", performance_csv),
  paste0("Feature importance: ", feature_importance_csv),
  paste0("Selected features: ", selected_features_csv),
  paste0("QC audit: ", qc_audit_csv),
  "",
  "Next step:",
  "08A_UMAP_FEATURE_VALIDATION_FOR_KEY_DATASETS.R",
  "",
  "Journal-rigor note:",
  "07B models are exploratory marker-rule-derived classifiers. Do not describe them as validated clinical-use models or experimentally validated graft outcome models."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("07B exploratory marker-rule-derived prioritization model models 运行结束\n")
cat("============================================================\n\n")

cat("Ideal training groups：", nrow(ideal_train), "\n")
cat("Ideal positive / negative：", ideal_pos, " / ", ideal_neg, "\n")
cat("Safety training groups：", nrow(safety_train), "\n")
cat("Safety positive / negative：", safety_pos, " / ", safety_neg, "\n")
cat("Internal CV prediction rows：", n_internal_pred, "\n")
cat("LODO prediction rows：", n_lodo_pred, "\n")
cat("Best internal CV AUC：", round(best_auc_internal, 4), "\n")
cat("Best LODO AUC：", round(best_auc_lodo, 4), "\n")
cat("Algorithms：", paste(sort(unique(internal_preds$algorithm)), collapse = "; "), "\n\n")

cat("输出文件：\n")
cat(internal_pred_csv, "\n")
cat(lodo_pred_csv, "\n")
cat(performance_csv, "\n")
cat(feature_importance_csv, "\n")
cat(selected_features_csv, "\n")
cat(qc_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 07B exploratory marker-rule-derived prioritization model models 完成。\n")
cat("下一步进入 08A：UMAP / FeaturePlot / DotPlot validation for key datasets。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

KEY_DATASETS <- c(
  "GSE178265_DA_01B",
  "GSE233885",
  "GSE204796",
  "GSE132758"
)

MAX_OBJECTS_PER_DATASET <- 1
SEED <- 20260714

PDF_WIDTH <- 9.2
PDF_HEIGHT <- 6.2

RENDER_DPI <- 300
RENDER_WIDTH_PX <- 2760
RENDER_HEIGHT_PX <- 1860

POINT_SIZE_DISCRETE <- 0.58
POINT_SIZE_CONTINUOUS <- 0.60
CLUSTER_LABEL_SIZE <- 3.2

USE_FULL_CELLS_FOR_FIGURE <- TRUE

cat("\n============================================================\n")
cat("08A V19：memory-safe balanced publication PDF complete UMAP\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table", "Seurat", "ggplot2", "png")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 08A V18。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(Seurat)
  library(ggplot2)
  library(png)
  library(grid)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_05A_object <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_05A_cell <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_cell_level_scores.csv")
input_05B_contrast <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")
input_07A_master <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_group_level_ML_master_table.csv")

out_tables_dir <- file.path(tables_dir, "08A_complete_umap_score_annotation_V19_memory_safe_pdf")
out_figures_dir <- file.path(figures_dir, "08A_complete_umap_score_annotation_V19_memory_safe_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

selected_objects_csv <- file.path(out_tables_dir, "08A_V19_selected_validation_objects.csv")
score_mapping_audit_csv <- file.path(out_tables_dir, "08A_V19_score_mapping_audit.csv")
plot_audit_csv <- file.path(out_tables_dir, "08A_V19_plot_audit.csv")
object_error_csv <- file.path(out_tables_dir, "08A_V19_object_error_audit.csv")
figure_index_csv <- file.path(out_tables_dir, "08A_V19_figure_index.csv")
figure_notes_csv <- file.path(out_tables_dir, "08A_V19_figure_caution_notes.csv")
report_txt <- file.path(reports_dir, "08A_V19_final_publication_pdf_complete_umap_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

sanitize <- function(x) {
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

wrap_label <- function(x, width = 24) {
  vapply(as.character(x), function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1))
}

short_dataset_label <- function(x) {
  x <- as.character(x)
  ifelse(x == "GSE178265_DA_01B", "GSE178265 DA reference", x)
}

short_safety_class <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "Unassigned"

  x <- ifelse(x == "ideal_DA_projection_high_safety_low", "Ideal-like DA/projection-high safety-low", x)
  x <- ifelse(x == "mixed_DA_or_projection_with_safety_risk", "Mixed DA/projection with safety-risk signal", x)
  x <- ifelse(x == "high_safety_risk_low_DA", "High safety-risk, low DA", x)
  x <- ifelse(x == "projection_competence_without_DA_low_safety", "Projection-associated, DA-low safety-low", x)
  x <- ifelse(x == "lower_priority_or_mixed", "Lower-priority or mixed", x)

  wrap_label(x, width = 20)
}

short_annotation_label <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "Unassigned"

  x <- ifelse(x == "cycling_or_progenitor_safety_risk_state", "Cycling/progenitor risk", x)
  x <- ifelse(x == "immature_pluripotency_risk_signal_state", "Immature/pluripotency risk", x)
  x <- ifelse(x == "DA_projection_competence_candidate_low_safety_signal", "DA/projection-high safety-low", x)
  x <- ifelse(x == "DA_like_with_A9_molecular_bias", "DA-like A9-biased", x)
  x <- ifelse(x == "DA_like_with_A10_molecular_bias", "DA-like A10-biased", x)
  x <- ifelse(x == "projection_competence_without_strong_DA_identity", "Projection-associated DA-low", x)
  x <- ifelse(x == "lower_priority_or_mixed_signal", "Lower-priority/mixed", x)
  x <- ifelse(x == "unassigned_no_04B_group_score", "Unassigned", x)
  x <- ifelse(x == "unassigned_low_confidence", "Unassigned/low confidence", x)

  wrap_label(x, width = 20)
}

pretty_score_label <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "DA_like_composite_score", "DA-like score", x)
  x <- ifelse(x == "projection_competence_composite_score", "Projection-associated competence score", x)
  x <- ifelse(x == "DA_projection_competence_composite_score", "DA/projection-associated composite score", x)
  x <- ifelse(x == "A9_minus_A10_score_05A", "A9-minus-A10 molecular bias score", x)
  x <- ifelse(x == "safety_risk_composite_05B", "Safety-risk-associated score", x)
  x
}

short_plot_title <- function(dataset_label, plot_kind) {
  paste0(dataset_label, " | ", plot_kind)
}

detect_cell_column <- function(dt) {
  candidates <- c("cell", "cell_id", "cell_name", "barcode", "cell_barcode", "Cell", "cells", "cell_key")
  hit <- intersect(candidates, colnames(dt))
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

choose_group_col <- function(meta_cols) {
  candidates <- c(
    "annotation_04D_group_id",
    "group_id",
    "cluster_id",
    "seurat_clusters",
    "RNA_snn_res.0.5",
    "RNA_snn_res.0.3",
    "RNA_snn_res.0.8"
  )

  hit <- intersect(candidates, meta_cols)
  if (length(hit) > 0) return(hit[[1]])
  NA_character_
}

choose_annotation_col <- function(meta_cols) {
  candidates <- c(
    "annotation_04D_v1",
    "annotation_v1_conservative",
    "annotation_04D",
    "celltype_04D",
    "preliminary_annotation",
    "cell_state_04D",
    "cell_state",
    "cell_type",
    "celltype"
  )

  hit <- intersect(candidates, meta_cols)
  if (length(hit) > 0) return(hit[[1]])
  NA_character_
}

get_umap_dt <- function(obj) {
  if (!"umap" %in% names(obj@reductions)) return(NULL)

  emb <- Embeddings(obj, "umap")

  data.table(
    cell = rownames(emb),
    UMAP_1 = as.numeric(emb[, 1]),
    UMAP_2 = as.numeric(emb[, 2])
  )
}

add_05A_scores_to_meta <- function(meta_dt, dataset, object_id, score_dt, cell_col) {
  if (nrow(score_dt) == 0 || is.na(cell_col)) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "no_cell_score_table_or_no_cell_column"))
  }

  if (!all(c("dataset", "object_id") %in% colnames(score_dt))) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "score_table_missing_dataset_object_id"))
  }

  sub <- as.data.table(score_dt[score_dt$dataset == dataset & score_dt$object_id == object_id, , drop = FALSE])
  if (nrow(sub) == 0) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "no_scores_for_object"))
  }

  sub[[cell_col]] <- as.character(sub[[cell_col]])

  score_cols <- intersect(
    c(
      "DA_like_composite_score",
      "projection_competence_composite_score",
      "DA_projection_competence_composite_score",
      "A9_minus_A10_score_05A",
      "DA_core_identity_score",
      "DA_functional_machinery_score",
      "A9_like_DA_identity_score",
      "A10_like_DA_identity_score"
    ),
    colnames(sub)
  )

  if (length(score_cols) == 0) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "no_expected_score_columns"))
  }

  idx <- match(meta_dt$cell, sub[[cell_col]])
  matched <- sum(!is.na(idx))

  for (cc in score_cols) {
    vals <- rep(NA_real_, nrow(meta_dt))
    vals[!is.na(idx)] <- suppressWarnings(as.numeric(sub[[cc]][idx[!is.na(idx)]]))
    meta_dt[[cc]] <- vals
  }

  list(meta = meta_dt, matched = matched, cols = score_cols, message = "ok")
}

add_05B_scores_to_meta <- function(meta_dt, dataset, object_id, contrast_dt) {
  if (nrow(contrast_dt) == 0) {
    return(list(meta = meta_dt, matched = 0L, group_col = NA_character_, message = "no_contrast_table"))
  }

  group_col <- choose_group_col(colnames(meta_dt))

  if (is.na(group_col)) {
    return(list(meta = meta_dt, matched = 0L, group_col = NA_character_, message = "no_group_col_in_meta"))
  }

  sub <- as.data.table(contrast_dt[contrast_dt$dataset == dataset & contrast_dt$object_id == object_id, , drop = FALSE])
  if (nrow(sub) == 0 || !"group_id" %in% colnames(sub)) {
    return(list(meta = meta_dt, matched = 0L, group_col = group_col, message = "no_contrast_for_object_or_no_group_id"))
  }

  sub$group_id <- as.character(sub$group_id)
  mg <- as.character(meta_dt[[group_col]])

  score_map <- setNames(suppressWarnings(as.numeric(sub$safety_risk_composite_05B)), sub$group_id)
  class_map <- setNames(as.character(sub$safety_contrast_class_05B), sub$group_id)

  meta_dt$safety_risk_composite_05B <- score_map[mg]
  meta_dt$safety_contrast_class_05B_raw <- class_map[mg]
  meta_dt$safety_contrast_class_05B_short <- short_safety_class(class_map[mg])

  matched <- sum(!is.na(meta_dt$safety_risk_composite_05B))

  list(meta = meta_dt, matched = matched, group_col = group_col, message = "ok")
}

open_highres_png_device <- function(filename) {

  grDevices::png(
    filename = filename,
    width = RENDER_WIDTH_PX,
    height = RENDER_HEIGHT_PX,
    units = "px",
    res = RENDER_DPI,
    type = ifelse(isTRUE(capabilities("cairo")), "cairo", "windows"),
    bg = "white"
  )
}

save_pdf_atomic <- function(plot_obj, pdf_path, width = PDF_WIDTH, height = PDF_HEIGHT) {
  tmp_png <- paste0(pdf_path, ".tmp_render.png")
  tmp_pdf <- paste0(pdf_path, ".tmp.pdf")

  if (file.exists(tmp_png)) file.remove(tmp_png)
  if (file.exists(tmp_pdf)) file.remove(tmp_pdf)
  if (file.exists(pdf_path)) file.remove(pdf_path)

  ok <- FALSE
  msg <- NA_character_

  tryCatch({
    open_highres_png_device(tmp_png)
    print(plot_obj)
    grDevices::dev.off()

    if (!file.exists(tmp_png) || file.info(tmp_png)$size < 1000) {
      stop("temporary high-resolution render PNG missing or too small")
    }

    img <- png::readPNG(tmp_png, native = TRUE)

    grDevices::pdf(file = tmp_pdf, width = width, height = height, useDingbats = FALSE, paper = "special")
    grid::grid.newpage()
    grid::grid.raster(img, x = 0.5, y = 0.5, width = 1, height = 1, interpolate = TRUE)
    grDevices::dev.off()

    if (file.exists(tmp_pdf) && file.info(tmp_pdf)$size > 1000) {
      file.rename(tmp_pdf, pdf_path)
      ok <- TRUE
    } else {
      stop("temporary PDF missing or too small")
    }
  }, error = function(e) {
    msg <<- conditionMessage(e)
    tryCatch(grDevices::dev.off(), error = function(e2) NULL)
  })

  if (file.exists(tmp_png)) file.remove(tmp_png)
  if (file.exists(tmp_pdf)) file.remove(tmp_pdf)

  gc(verbose = FALSE)

  list(success = ok, message = msg)
}

base_theme <- function() {
  theme_classic(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 7.5),
      legend.key.size = unit(0.55, "lines"),
      legend.spacing.y = unit(0.18, "lines"),
      plot.margin = margin(8, 18, 8, 8)
    )
}

make_discrete_palette <- function(vals, type = "generic") {
  lev <- sort(unique(as.character(vals)))
  lev <- lev[!is.na(lev)]

  if (type == "safety_class") {
    out <- setNames(rep("grey70", length(lev)), lev)

    for (l in lev) {
      if (grepl("Ideal-like", l)) out[[l]] <- "#D95F8D"
      if (grepl("Lower", l)) out[[l]] <- "#4E9A06"
      if (grepl("Mixed", l)) out[[l]] <- "#0099CC"
      if (grepl("High safety", l)) out[[l]] <- "#D55E00"
      if (grepl("Projection", l)) out[[l]] <- "#7570B3"
      if (grepl("Unassigned", l)) out[[l]] <- "grey70"
    }

    return(out)
  }

  pal <- grDevices::hcl.colors(max(length(lev), 3), palette = "Dark 3")
  names(pal) <- lev
  pal
}

make_umap_discrete_plot <- function(umap_dt, meta_dt, color_col, title, legend_title, legend_type = "generic", label_clusters = FALSE) {
  df <- merge(umap_dt, meta_dt[, .(cell, value = get(color_col))], by = "cell", all.x = TRUE)
  df[is.na(value) | value == "", value := "Unassigned"]
  df[, value := as.character(value)]

  pal <- make_discrete_palette(df$value, type = legend_type)

  p <- ggplot(df, aes(x = UMAP_1, y = UMAP_2, color = value)) +
    geom_point(size = POINT_SIZE_DISCRETE, alpha = 1, stroke = 0) +
    coord_fixed() +
    scale_color_manual(values = pal, drop = FALSE) +
    labs(title = title, x = "UMAP 1", y = "UMAP 2", color = legend_title) +
    guides(color = guide_legend(override.aes = list(size = 2.4), ncol = 1)) +
    base_theme()

  if (label_clusters) {
    cent <- df[, .(UMAP_1 = median(UMAP_1), UMAP_2 = median(UMAP_2), n = .N), by = value][n >= 30]
    if (nrow(cent) > 0 && nrow(cent) <= 20) {
      p <- p + geom_text(
        data = cent,
        aes(x = UMAP_1, y = UMAP_2, label = value),
        inherit.aes = FALSE,
        size = CLUSTER_LABEL_SIZE,
        fontface = "bold",
        color = "black"
      )
    }
  }

  p
}

make_umap_continuous_plot <- function(umap_dt, meta_dt, color_col, title) {
  df <- merge(umap_dt, meta_dt[, .(cell, value_raw = get(color_col))], by = "cell", all.x = TRUE)
  df[, value_raw := suppressWarnings(as.numeric(value_raw))]
  df <- df[!is.na(value_raw)]

  if (nrow(df) == 0) stop("No non-NA values for ", color_col)

  q <- suppressWarnings(quantile(df$value_raw, probs = c(0.01, 0.99), na.rm = TRUE))
  if (all(is.finite(q)) && q[1] < q[2]) {
    df[, value := pmin(pmax(value_raw, q[1]), q[2])]
  } else {
    df[, value := value_raw]
  }

  ggplot(df, aes(x = UMAP_1, y = UMAP_2, color = value)) +
    geom_point(size = POINT_SIZE_CONTINUOUS, alpha = 1, stroke = 0) +
    coord_fixed() +
    scale_color_gradientn(
      colours = grDevices::hcl.colors(100, palette = "Viridis"),
      name = pretty_score_label(color_col)
    ) +
    labs(title = title, x = "UMAP 1", y = "UMAP 2") +
    guides(color = guide_colorbar(barheight = unit(2.3, "cm"), barwidth = unit(0.30, "cm"))) +
    base_theme()
}

safe_plot_record <- function(dataset, object_id, plot_type, pdf_path, plot_obj) {
  res <- save_pdf_atomic(plot_obj, pdf_path)

  data.frame(
    dataset = dataset,
    object_id = object_id,
    plot_type = plot_type,
    pdf_path = ifelse(res$success, pdf_path, NA_character_),
    success = res$success,
    message = res$message,
    stringsAsFactors = FALSE
  )
}

safe_make_plot <- function(dataset, object_id, plot_type, pdf_path, expr) {
  tryCatch({
    p <- force(expr)
    safe_plot_record(dataset, object_id, plot_type, pdf_path, p)
  }, error = function(e) {
    data.frame(
      dataset = dataset,
      object_id = object_id,
      plot_type = plot_type,
      pdf_path = NA_character_,
      success = FALSE,
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

set.seed(SEED)

stamp("读取 manifest / scores / contrast tables。")

manifest <- as.data.table(read_csv_required(input_manifest))
object_scores <- as.data.table(read_csv_optional(input_05A_object))
cell_scores <- as.data.table(read_csv_optional(input_05A_cell))
contrast <- as.data.table(read_csv_optional(input_05B_contrast))
ml_master <- as.data.table(read_csv_optional(input_07A_master))

if (!all(c("dataset", "object_id", "annotated_rds") %in% colnames(manifest))) {
  stop("04D annotated manifest 缺少 dataset/object_id/annotated_rds。")
}

cell_col_05A <- detect_cell_column(cell_scores)

stamp("05A cell score cell column：", ifelse(is.na(cell_col_05A), "not_found", cell_col_05A))
stamp("Render DPI：", RENDER_DPI)

stamp("选择 key dataset representative objects。")

manifest2 <- manifest[dataset %in% KEY_DATASETS]
manifest2[, file_exists := file.exists(annotated_rds)]

if (nrow(object_scores) > 0 && all(c("dataset", "object_id") %in% colnames(object_scores))) {
  obj_n_cols <- intersect(c("n_cells", "total_cells", "cells"), colnames(object_scores))
  if (length(obj_n_cols) > 0) {
    ncol_use <- obj_n_cols[[1]]
    obj_counts <- as.data.table(object_scores[, c("dataset", "object_id", ncol_use), with = FALSE])
    setnames(obj_counts, ncol_use, "n_cells_score")
  } else {
    obj_counts <- data.table(dataset = character(), object_id = character(), n_cells_score = numeric())
  }
} else {
  obj_counts <- data.table(dataset = character(), object_id = character(), n_cells_score = numeric())
}

if (nrow(ml_master) > 0 && all(c("dataset", "object_id", "n_cells") %in% colnames(ml_master))) {
  ml_counts <- ml_master[, .(n_cells_ml = sum(n_cells, na.rm = TRUE)), by = .(dataset, object_id)]
} else {
  ml_counts <- data.table(dataset = character(), object_id = character(), n_cells_ml = numeric())
}

manifest2 <- merge(manifest2, obj_counts, by = c("dataset", "object_id"), all.x = TRUE)
manifest2 <- merge(manifest2, ml_counts, by = c("dataset", "object_id"), all.x = TRUE)

manifest2[, n_cells_for_selection := fifelse(!is.na(n_cells_score), n_cells_score, n_cells_ml)]
manifest2[is.na(n_cells_for_selection), n_cells_for_selection := 0]

selected <- manifest2[file_exists == TRUE, head(.SD[order(-n_cells_for_selection)], MAX_OBJECTS_PER_DATASET), by = dataset]
selected <- selected[order(match(dataset, KEY_DATASETS), -n_cells_for_selection)]

atomic_write_csv(as.data.frame(selected), selected_objects_csv)

figure_notes <- data.frame(
  plot_family = c(
    "Cluster UMAP",
    "04D conservative annotation UMAP",
    "05B class UMAP",
    "DA-like score UMAP",
    "Projection-associated competence score UMAP",
    "DA/projection-associated composite score UMAP",
    "Safety-risk-associated score UMAP",
    "A9-minus-A10 score UMAP"
  ),
  caution_note = c(
    "UMAP and clusters are visualization outputs and do not define final cell identity by themselves.",
    "04D annotations are conservative marker-supported transcriptomic labels and should not be treated as experimentally validated cell fate labels.",
    "05B classes are rule-derived transcriptional candidate states, not experimentally validated graft outcomes.",
    "DA-like score reflects curated transcriptomic signatures, not definitive functional dopaminergic identity.",
    "Projection-associated competence is inferred from transcriptomic signatures and does not prove anatomical projection.",
    "DA/projection-associated composite score is a transcriptomic prioritization score and does not prove functional integration.",
    "Safety-risk-associated score reflects proliferation/progenitor/stress-like transcriptional programs and does not prove tumorigenicity or clinical safety.",
    "A9/A10-like score indicates molecular bias only, not definitive anatomical subtype identity."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(figure_notes, figure_notes_csv)

plot_records <- list()
score_records <- list()
object_error_records <- list()

if (nrow(selected) == 0) {
  stop("没有选中任何可用对象。请检查 04D annotated manifest。")
}

for (i in seq_len(nrow(selected))) {
  ds <- selected$dataset[[i]]
  oid <- selected$object_id[[i]]
  path <- selected$annotated_rds[[i]]

  stamp("处理对象 ", i, " / ", nrow(selected), "：", ds, " :: ", oid)

  base_name <- paste0(sanitize(ds), "__", sanitize(oid))
  ds_label <- short_dataset_label(ds)

  tryCatch({
    obj <- readRDS(path)

    if (!"umap" %in% names(obj@reductions)) {
      stop("No UMAP reduction found in this object.")
    }

    umap_dt <- get_umap_dt(obj)
    if (is.null(umap_dt) || nrow(umap_dt) == 0) {
      stop("UMAP dataframe unavailable.")
    }

    meta_dt <- as.data.table(obj@meta.data)
    meta_dt[, cell := rownames(obj@meta.data)]

    add05A <- add_05A_scores_to_meta(meta_dt, ds, oid, cell_scores, cell_col_05A)
    meta_dt <- add05A$meta

    add05B <- add_05B_scores_to_meta(meta_dt, ds, oid, contrast)
    meta_dt <- add05B$meta

    annotation_col_raw <- choose_annotation_col(colnames(meta_dt))
    annotation_mapping_message <- "no_annotation_column_found"
    if (!is.na(annotation_col_raw) && annotation_col_raw %in% colnames(meta_dt)) {
      meta_dt[, annotation_08A_short := short_annotation_label(get(annotation_col_raw))]
      annotation_mapping_message <- paste0("annotation_col=", annotation_col_raw)
    }

    score_records[[length(score_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      n_cells_full = ncol(obj),
      n_cells_visualized = nrow(umap_dt),
      downsampled_for_visualization = FALSE,
      cell_score_column_detected = ifelse(is.na(cell_col_05A), NA_character_, cell_col_05A),
      matched_05A_cells = add05A$matched,
      available_05A_score_columns = paste(add05A$cols, collapse = ";"),
      add05A_message = add05A$message,
      group_col_for_05B = add05B$group_col,
      matched_05B_cells = add05B$matched,
      add05B_message = add05B$message,
      annotation_col_raw = ifelse(is.na(annotation_col_raw), NA_character_, annotation_col_raw),
      annotation_mapping_message = annotation_mapping_message,
      stringsAsFactors = FALSE
    )

    if ("seurat_clusters" %in% colnames(meta_dt)) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_clusters.pdf"))
      rec <- safe_make_plot(
        ds, oid, "V19_UMAP_clusters", pdf_path,
        make_umap_discrete_plot(
          umap_dt, meta_dt,
          color_col = "seurat_clusters",
          title = short_plot_title(ds_label, "UMAP clusters"),
          legend_title = "Cluster",
          legend_type = "generic",
          label_clusters = TRUE
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    if ("annotation_08A_short" %in% colnames(meta_dt) && sum(!is.na(meta_dt$annotation_08A_short)) > 0) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_04D_conservative_annotation.pdf"))
      rec <- safe_make_plot(
        ds, oid, "V19_UMAP_04D_conservative_annotation", pdf_path,
        make_umap_discrete_plot(
          umap_dt, meta_dt,
          color_col = "annotation_08A_short",
          title = short_plot_title(ds_label, "04D conservative annotation"),
          legend_title = "04D annotation",
          legend_type = "generic",
          label_clusters = FALSE
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    if ("safety_contrast_class_05B_short" %in% colnames(meta_dt) && sum(!is.na(meta_dt$safety_contrast_class_05B_short)) > 0) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_05B_safety_contrast_class.pdf"))
      rec <- safe_make_plot(
        ds, oid, "V19_UMAP_05B_safety_contrast_class", pdf_path,
        make_umap_discrete_plot(
          umap_dt, meta_dt,
          color_col = "safety_contrast_class_05B_short",
          title = short_plot_title(ds_label, "DA/projection-associated class"),
          legend_title = "05B class",
          legend_type = "safety_class",
          label_clusters = FALSE
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    score_cols <- intersect(
      c(
        "DA_like_composite_score",
        "projection_competence_composite_score",
        "DA_projection_competence_composite_score",
        "A9_minus_A10_score_05A",
        "safety_risk_composite_05B"
      ),
      colnames(meta_dt)
    )

    score_cols <- score_cols[vapply(score_cols, function(cc) sum(!is.na(meta_dt[[cc]])) > 0, logical(1))]

    for (sc in score_cols) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_score_", sanitize(sc), ".pdf"))
      rec <- safe_make_plot(
        ds, oid, paste0("V19_UMAP_score_", sc), pdf_path,
        make_umap_continuous_plot(
          umap_dt, meta_dt,
          color_col = sc,
          title = short_plot_title(ds_label, pretty_score_label(sc))
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    rm(obj)
    gc(verbose = FALSE)

  }, error = function(e) {
    object_error_records[[length(object_error_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      stage = "object_loop",
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )

    plot_records[[length(plot_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      plot_type = "object_failed",
      pdf_path = NA_character_,
      success = FALSE,
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

plot_audit <- if (length(plot_records) > 0) rbindlist(plot_records, fill = TRUE) else data.table()
score_audit <- if (length(score_records) > 0) rbindlist(score_records, fill = TRUE) else data.table()
object_error_audit <- if (length(object_error_records) > 0) rbindlist(object_error_records, fill = TRUE) else data.table()

required_plot_cols <- c("dataset", "object_id", "plot_type", "pdf_path", "success", "message")
for (cc in required_plot_cols) {
  if (!cc %in% colnames(plot_audit)) {
    if (cc == "success") plot_audit[[cc]] <- FALSE else plot_audit[[cc]] <- NA_character_
  }
}

figure_index <- plot_audit[
  success == TRUE,
  c("dataset", "object_id", "plot_type", "pdf_path", "message"),
  with = FALSE
]

atomic_write_csv(as.data.frame(plot_audit), plot_audit_csv)
atomic_write_csv(as.data.frame(score_audit), score_mapping_audit_csv)
atomic_write_csv(as.data.frame(object_error_audit), object_error_csv)
atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

n_success_plots <- sum(plot_audit$success, na.rm = TRUE)
n_failed_plots <- sum(!plot_audit$success, na.rm = TRUE)

selected_lines <- apply(as.data.frame(selected), 1, function(x) {
  paste0(x[["dataset"]], " :: ", x[["object_id"]], " ; n_cells_for_selection=", x[["n_cells_for_selection"]])
})

score_lines <- if (nrow(score_audit) > 0) {
  apply(as.data.frame(score_audit), 1, function(x) {
    paste0(
      x[["dataset"]],
      " :: ",
      x[["object_id"]],
      " ; cells=",
      x[["n_cells_visualized"]],
      " ; matched_05A=",
      x[["matched_05A_cells"]],
      " ; matched_05B=",
      x[["matched_05B_cells"]],
      " ; ",
      x[["annotation_mapping_message"]]
    )
  })
} else {
  "none"
}

error_lines <- if (nrow(object_error_audit) > 0) {
  apply(as.data.frame(object_error_audit), 1, function(x) {
    paste0(x[["dataset"]], " :: ", x[["object_id"]], " ; ", x[["stage"]], " ; ", x[["message"]])
  })
} else {
  "none"
}

report_lines <- c(
  "08A V19 memory-safe balanced publication PDF complete UMAP report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Selected objects:",
  selected_lines,
  "",
  "Score / annotation mapping summary:",
  score_lines,
  "",
  "Object errors:",
  error_lines,
  "",
  "Plot summary:",
  paste0("Successful plots: ", n_success_plots),
  paste0("Failed plots: ", n_failed_plots),
  paste0("Output figure directory: ", out_figures_dir),
  paste0("Render DPI: ", RENDER_DPI),
  paste0("Render size px: ", RENDER_WIDTH_PX, "x", RENDER_HEIGHT_PX),
  "",
  "Output tables:",
  paste0("Selected objects: ", selected_objects_csv),
  paste0("Plot audit: ", plot_audit_csv),
  paste0("Figure index: ", figure_index_csv),
  paste0("Score mapping audit: ", score_mapping_audit_csv),
  paste0("Object error audit: ", object_error_csv),
  paste0("Figure caution notes: ", figure_notes_csv),
  "",
  "Next step:",
  "Check V19 PDFs. If clean, readable, and visually acceptable, keep V19 as final complete 08A UMAP validation output.",
  "",
  "Journal-rigor note:",
  "08A V19 figures are visualization outputs only. Projection-associated molecular competence does not prove anatomical projection. Safety-risk-associated transcriptional state does not prove tumorigenicity or clinical safety."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("08A V19 memory-safe balanced publication PDF complete UMAP 运行结束\n")
cat("============================================================\n\n")

cat("Selected validation objects：", nrow(selected), "\n")
cat("Successful plots：", n_success_plots, "\n")
cat("Failed plots：", n_failed_plots, "\n")
cat("Object error rows：", nrow(object_error_audit), "\n")
cat("Score mapping rows：", nrow(score_audit), "\n")
cat("Render DPI：", RENDER_DPI, "\n")
cat("Render size px：", RENDER_WIDTH_PX, "x", RENDER_HEIGHT_PX, "\n\n")

cat("输出表格：\n")
cat(selected_objects_csv, "\n")
cat(plot_audit_csv, "\n")
cat(figure_index_csv, "\n")
cat(score_mapping_audit_csv, "\n")
cat(object_error_csv, "\n")
cat(figure_notes_csv, "\n")
cat(report_txt, "\n\n")

cat("输出 PDF 图片目录：\n")
cat(out_figures_dir, "\n\n")

cat("✅ 08A V19 memory-safe balanced publication PDF complete UMAP 完成。\n")
cat("下一步：检查 V19 输出数量、点颜色深浅、以及 GSE204796/GSE132758 关键 PDF 是否正常。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

TOP_GENES_PER_STATE <- 20
SEED <- 20260714

cat("\n============================================================\n")
cat("08B FINAL V3：heatmap colorbar layout fixed PDF figures only\n")
cat("============================================================\n\n")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("缺少 data.table，请先安装。")
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_tables_dir <- file.path(tables_dir, "08B_FINAL_candidate_state_signature_interpretation")

input_class_category <- file.path(input_tables_dir, "08B_FINAL_class_category_program_summary.csv")
input_state_category <- file.path(input_tables_dir, "08B_FINAL_state_vs_rest_category_direction.csv")
input_top_marker <- file.path(input_tables_dir, "08B_FINAL_top_marker_genes_by_state.csv")

out_tables_dir <- file.path(tables_dir, "08B_FINAL_V3_heatmap_colorbar_fixed_figures")
out_figures_dir <- file.path(figures_dir, "08B_FINAL_V3_heatmap_colorbar_fixed_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

figure_index_csv <- file.path(out_tables_dir, "08B_FINAL_V3_figure_index.csv")
layout_audit_csv <- file.path(out_tables_dir, "08B_FINAL_V3_layout_audit.csv")
report_txt <- file.path(reports_dir, "08B_FINAL_V3_heatmap_colorbar_fixed_figures_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_required <- function(path) {
  if (!file.exists(path)) stop("找不到输入表：", path)
  data.table::fread(path, data.table = TRUE, showProgress = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

pretty_category <- function(x) {
  x <- as.character(x)
  x <- gsub("_", " ", x)
  x
}

state_order <- c(
  "ideal_DA_projection_high_safety_low",
  "mixed_DA_or_projection_with_safety_risk",
  "projection_competence_without_DA_low_safety",
  "high_safety_risk_low_DA",
  "lower_priority_or_mixed"
)

state_short_title <- function(x) {
  x <- as.character(x)
  out <- x
  out[x == "ideal_DA_projection_high_safety_low"] <- "Ideal-like"
  out[x == "mixed_DA_or_projection_with_safety_risk"] <- "Mixed-risk"
  out[x == "projection_competence_without_DA_low_safety"] <- "Projection DA-low"
  out[x == "high_safety_risk_low_DA"] <- "High-risk low-DA"
  out[x == "lower_priority_or_mixed"] <- "Lower-priority"
  out
}

state_short_axis <- function(x) {
  x <- as.character(x)
  out <- x
  out[x == "ideal_DA_projection_high_safety_low"] <- "Ideal-like\nDA/proj high\nsafety-low"
  out[x == "mixed_DA_or_projection_with_safety_risk"] <- "Mixed\nDA/proj +\nrisk"
  out[x == "projection_competence_without_DA_low_safety"] <- "Projection\nDA-low\nsafety-low"
  out[x == "high_safety_risk_low_DA"] <- "High risk\nlow DA"
  out[x == "lower_priority_or_mixed"] <- "Lower\npriority/mixed"
  out
}

safe_pdf <- function(path, width, height) {
  grDevices::pdf(path, width = width, height = height, useDingbats = FALSE)
}

xlim_with_padding <- function(vals, force_zero = TRUE, pad_frac = 0.12) {
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(c(-1, 1))

  rng <- range(vals)
  if (force_zero) rng <- range(c(rng, 0))

  span <- diff(rng)
  if (!is.finite(span) || span == 0) {
    span <- max(abs(rng), 1)
    rng <- c(rng[1] - span * 0.5, rng[2] + span * 0.5)
  }

  pad <- span * pad_frac
  c(rng[1] - pad, rng[2] + pad)
}

set.seed(SEED)

stamp("读取 08B FINAL 输出表。")

class_category <- read_required(input_class_category)
state_category <- read_required(input_state_category)
top_marker <- read_required(input_top_marker)

states <- intersect(state_order, unique(state_category$safety_contrast_class_05B))
states <- c(states, setdiff(unique(state_category$safety_contrast_class_05B), states))

stamp("states：", paste(state_short_title(states), collapse = "; "))
stamp("category rows：", nrow(state_category))
stamp("top marker rows：", nrow(top_marker))

plot_heatmap_fixed <- function(summary_dt, pdf_path) {
  dt <- copy(summary_dt)
  dt <- dt[safety_contrast_class_05B %in% states]

  cats <- sort(unique(dt$category))
  sts <- states

  value_col <- "mean_expr_z_category_object"
  if (!value_col %in% names(dt)) {
    value_col <- "mean_expr_z_gene_object"
  }

  mat <- matrix(
    NA_real_,
    nrow = length(cats),
    ncol = length(sts),
    dimnames = list(pretty_category(cats), state_short_axis(sts))
  )

  for (i in seq_along(cats)) {
    for (j in seq_along(sts)) {
      val <- dt[category == cats[[i]] & safety_contrast_class_05B == sts[[j]], mean(get(value_col), na.rm = TRUE)]
      if (length(val) == 1 && is.finite(val)) mat[i, j] <- val
    }
  }

  mat[is.na(mat)] <- 0
  mat <- mat[order(rownames(mat)), , drop = FALSE]

  pdf_height <- max(7.8, 0.30 * nrow(mat) + 2.9)

  safe_pdf(pdf_path, width = 13.8, height = pdf_height)
  on.exit(grDevices::dev.off(), add = TRUE)

  par(mar = c(10.2, 11.2, 3.4, 5.0), xpd = FALSE)

  zlim <- max(abs(mat), na.rm = TRUE)
  if (!is.finite(zlim) || zlim == 0) zlim <- 1

  pal <- grDevices::colorRampPalette(c("#2166AC", "white", "#B2182B"))(101)
  breaks <- seq(-zlim, zlim, length.out = length(pal) + 1)

  x_heat <- seq_len(ncol(mat))
  y_heat <- seq_len(nrow(mat))
  xlim_full <- c(0.5, ncol(mat) + 1.35)
  ylim_full <- c(0.5, nrow(mat) + 0.5)

  image(
    x = x_heat,
    y = y_heat,
    z = t(mat),
    col = pal,
    breaks = breaks,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = "08B candidate-state marker program heatmap",
    xlim = xlim_full,
    ylim = ylim_full
  )

  axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), las = 2, cex.axis = 0.70, tick = FALSE)
  axis(2, at = seq_len(nrow(mat)), labels = rownames(mat), las = 2, cex.axis = 0.66, tick = FALSE)

  rect(0.5, 0.5, ncol(mat) + 0.5, nrow(mat) + 0.5, border = "black", lwd = 1)

  legend_x1 <- ncol(mat) + 0.72
  legend_x2 <- ncol(mat) + 0.92
  legend_y <- seq(1.0, nrow(mat), length.out = length(pal))

  rect(
    xleft = legend_x1,
    ybottom = legend_y[-length(legend_y)],
    xright = legend_x2,
    ytop = legend_y[-1],
    col = pal,
    border = NA
  )

  text(legend_x2 + 0.16, min(legend_y), labels = round(-zlim, 2), cex = 0.58, adj = 0)
  text(legend_x2 + 0.16, max(legend_y), labels = round(zlim, 2), cex = 0.58, adj = 0)
  text(legend_x2 + 0.16, mean(range(legend_y)), labels = "z", cex = 0.65, adj = 0)

  invisible(TRUE)
}

plot_state_category_bars_fixed <- function(cat_dt, pdf_path) {
  safe_pdf(pdf_path, width = 11.4, height = 7.2)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (st in states) {
    sub <- cat_dt[safety_contrast_class_05B == st]
    if (nrow(sub) == 0) next

    sub <- sub[order(delta_state_vs_rest)]
    vals <- sub$delta_state_vs_rest
    labs <- pretty_category(sub$category)
    cols <- ifelse(vals >= 0, "#B2182B", "#2166AC")

    par(mar = c(5.3, 12.2, 3.4, 2.2), xpd = FALSE)

    barplot(
      vals,
      horiz = TRUE,
      names.arg = labs,
      las = 2,
      col = cols,
      border = NA,
      cex.names = 0.68,
      cex.axis = 0.78,
      xlab = "State-vs-rest category program difference",
      main = paste0("08B category programs | ", state_short_title(st)),
      cex.main = 0.95,
      cex.lab = 0.88,
      xlim = xlim_with_padding(vals, force_zero = TRUE, pad_frac = 0.16)
    )

    abline(v = 0, lty = 2, col = "grey40")
  }

  invisible(TRUE)
}

plot_top_marker_genes_fixed <- function(top_dt, pdf_path) {
  safe_pdf(pdf_path, width = 10.4, height = 7.3)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (st in states) {
    sub <- top_dt[safety_contrast_class_05B == st]
    if (nrow(sub) == 0) next

    sub <- sub[order(delta_state_vs_rest)]
    if (nrow(sub) > TOP_GENES_PER_STATE) {
      sub <- tail(sub, TOP_GENES_PER_STATE)
    }

    vals <- sub$delta_state_vs_rest
    labs <- sub$gene
    cols <- ifelse(vals >= 0, "#B2182B", "#2166AC")

    par(mar = c(5.2, 7.6, 3.4, 2.0), xpd = FALSE)

    barplot(
      vals,
      horiz = TRUE,
      names.arg = labs,
      las = 2,
      col = cols,
      border = NA,
      cex.names = 0.72,
      cex.axis = 0.78,
      xlab = "State-vs-rest marker gene difference",
      main = paste0("08B top marker genes | ", state_short_title(st)),
      cex.main = 0.95,
      cex.lab = 0.88,
      xlim = xlim_with_padding(vals, force_zero = TRUE, pad_frac = 0.16)
    )

    abline(v = 0, lty = 2, col = "grey40")
  }

  invisible(TRUE)
}

stamp("生成布局修正版 PDF。")

figure_records <- list()

pdf1 <- file.path(out_figures_dir, "08B_FINAL_V3_candidate_state_category_program_heatmap_layout_fixed.pdf")
ok <- FALSE; msg <- NA_character_
tryCatch({
  ok <- plot_heatmap_fixed(class_category, pdf1)
}, error = function(e) {
  msg <<- conditionMessage(e)
})
figure_records[[length(figure_records) + 1L]] <- data.table(
  figure_type = "category_program_heatmap_layout_fixed",
  pdf_path = ifelse(isTRUE(ok) && file.exists(pdf1), pdf1, NA_character_),
  success = isTRUE(ok) && file.exists(pdf1),
  message = msg
)

pdf2 <- file.path(out_figures_dir, "08B_FINAL_V3_state_vs_rest_category_program_barplots_layout_fixed.pdf")
ok <- FALSE; msg <- NA_character_
tryCatch({
  ok <- plot_state_category_bars_fixed(state_category, pdf2)
}, error = function(e) {
  msg <<- conditionMessage(e)
})
figure_records[[length(figure_records) + 1L]] <- data.table(
  figure_type = "state_vs_rest_category_barplots_layout_fixed",
  pdf_path = ifelse(isTRUE(ok) && file.exists(pdf2), pdf2, NA_character_),
  success = isTRUE(ok) && file.exists(pdf2),
  message = msg
)

pdf3 <- file.path(out_figures_dir, "08B_FINAL_V3_top_marker_genes_by_candidate_state_layout_fixed.pdf")
ok <- FALSE; msg <- NA_character_
tryCatch({
  ok <- plot_top_marker_genes_fixed(top_marker, pdf3)
}, error = function(e) {
  msg <<- conditionMessage(e)
})
figure_records[[length(figure_records) + 1L]] <- data.table(
  figure_type = "top_marker_genes_layout_fixed",
  pdf_path = ifelse(isTRUE(ok) && file.exists(pdf3), pdf3, NA_character_),
  success = isTRUE(ok) && file.exists(pdf3),
  message = msg
)

figure_index <- rbindlist(figure_records, fill = TRUE)
atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

layout_audit <- data.table(
  metric = c(
    "input_class_category_rows",
    "input_state_category_rows",
    "input_top_marker_rows",
    "states",
    "successful_figures",
    "output_figure_directory",
    "claim_boundary"
  ),
  value = c(
    nrow(class_category),
    nrow(state_category),
    nrow(top_marker),
    paste(state_short_title(states), collapse = "; "),
    sum(figure_index$success, na.rm = TRUE),
    out_figures_dir,
    "Layout-only figure regeneration; no change to 08B numerical results."
  )
)

atomic_write_csv(as.data.frame(layout_audit), layout_audit_csv)

report_lines <- c(
  "08B FINAL V3 heatmap colorbar-fixed figures report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Purpose:",
  "Regenerate the three 08B FINAL PDF figures with shorter titles, larger margins, wider PDF layout, and a heatmap colorbar that stays inside the PDF canvas.",
  "",
  "Successful figures:",
  paste0(sum(figure_index$success, na.rm = TRUE), " / ", nrow(figure_index)),
  "",
  "Output figures:",
  figure_index$pdf_path,
  "",
  "Output tables:",
  paste0("Figure index: ", figure_index_csv),
  paste0("Layout audit: ", layout_audit_csv),
  "",
  "Note:",
  "This script does not recalculate 08B results. It only fixes PDF layout and label clipping."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("08B FINAL V3 heatmap colorbar-fixed PDF figures 运行结束\n")
cat("============================================================\n\n")

cat("Successful figures：", sum(figure_index$success, na.rm = TRUE), " / ", nrow(figure_index), "\n\n")

cat("输出 PDF 图片目录：\n")
cat(out_figures_dir, "\n\n")

cat("输出文件：\n")
cat(figure_index_csv, "\n")
cat(layout_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 08B FINAL V3 heatmap colorbar-fixed PDF figures 完成。\n")
cat("下一步：检查 V3_heatmap_colorbar_fixed_pdf 文件夹里的三个 PDF，重点看 heatmap 右侧 colorbar 数字是否完整。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

TARGET_DATASET <- "GSE132758"
TARGET_OBJECT_ID_CONTAINS <- "rat45_1a"

CLASS_A <- c("ideal_DA_projection_high_safety_low")
CLASS_B <- c("lower_priority_or_mixed")
CONTRAST_NAME <- "ideal_vs_lower_priority"

MIN_PCT <- 0.05

TABLE_LOG2FC_CUTOFF <- 0.25
TABLE_PADJ_CUTOFF <- 0.05

VOLCANO_LOG2FC_CUTOFF <- 1
VOLCANO_PADJ_CUTOFF <- 0.05

GENE_CHUNK_SIZE <- 1000

X_CAP <- 8
Y_CAP <- 45

TOP_LABEL_GENES <- 15

POINT_SIZE <- 1.5
POINT_ALPHA <- 0.75

SEED <- 20260714

cat("\n============================================================\n")
cat("08C JOURNAL-STANDARD：all-filtered-genes chunked DEG volcano\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table", "Matrix", "Seurat", "ggplot2", "ggrepel")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 08C journal-standard script。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(Seurat)
  library(ggplot2)
  library(ggrepel)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_05B <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")

out_tables_dir <- file.path(tables_dir, "08C_JOURNAL_STANDARD_all_filtered_genes_chunked")
out_figures_dir <- file.path(figures_dir, "08C_JOURNAL_STANDARD_all_filtered_genes_chunked_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

deg_all_csv <- file.path(out_tables_dir, "08C_JOURNAL_all_filtered_genes_DEG_table.csv")
deg_sig_csv <- file.path(out_tables_dir, "08C_JOURNAL_significant_DEG_table.csv")
plot_table_csv <- file.path(out_tables_dir, "08C_JOURNAL_volcano_plot_table.csv")
label_table_csv <- file.path(out_tables_dir, "08C_JOURNAL_labeled_genes.csv")
summary_csv <- file.path(out_tables_dir, "08C_JOURNAL_summary.csv")
chunk_audit_csv <- file.path(out_tables_dir, "08C_JOURNAL_chunk_audit.csv")
replicate_audit_csv <- file.path(out_tables_dir, "08C_JOURNAL_replicate_level_audit.csv")
figure_index_csv <- file.path(out_tables_dir, "08C_JOURNAL_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08C_JOURNAL_method_and_claim_boundary_note.txt")
report_txt <- file.path(reports_dir, "08C_JOURNAL_STANDARD_all_filtered_genes_chunked_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

choose_group_col <- function(meta_cols) {
  candidates <- c(
    "annotation_04D_group_id",
    "group_id",
    "cluster_id",
    "seurat_clusters",
    "RNA_snn_res.0.5",
    "RNA_snn_res.0.3",
    "RNA_snn_res.0.8"
  )

  hit <- intersect(candidates, meta_cols)
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

detect_replicate_columns <- function(meta_dt) {

  candidates <- c(
    "sample_id", "sample", "Sample", "orig.ident", "orig_ident",
    "donor", "Donor", "patient", "Patient",
    "animal", "Animal", "rat", "mouse",
    "replicate", "replicate_id", "bio_rep", "biological_replicate",
    "batch", "Batch", "GSM", "gsm", "library", "Library"
  )

  hit <- intersect(candidates, colnames(meta_dt))

  nm <- colnames(meta_dt)
  fuzzy <- nm[grepl("sample|donor|patient|animal|replicate|batch|orig|gsm|library", nm, ignore.case = TRUE)]
  unique(c(hit, fuzzy))
}

safe_padj <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- rep(NA_real_, length(p))
  ok <- !is.na(p)
  out[ok] <- p.adjust(p[ok], method = "BH")
  out
}

sparse_row_vars <- function(m, row_means = NULL) {
  n <- ncol(m)
  if (is.null(row_means)) row_means <- Matrix::rowMeans(m)

  if (inherits(m, "sparseMatrix")) {
    m2 <- m
    m2@x <- m2@x ^ 2
    row_sq_means <- Matrix::rowMeans(m2)
    rm(m2)
    gc(verbose = FALSE)
  } else {
    row_sq_means <- rowMeans(m ^ 2)
  }

  vars <- (row_sq_means - row_means ^ 2) * n / max(n - 1, 1)
  vars[!is.finite(vars)] <- 0
  pmax(vars, 0)
}

get_expr_matrix <- function(obj) {
  assay_use <- DefaultAssay(obj)
  assay_obj <- obj[[assay_use]]

  if ("layers" %in% slotNames(assay_obj)) {
    layer_names <- names(assay_obj@layers)

    if ("data" %in% layer_names) {
      mat <- SeuratObject::LayerData(obj, assay = assay_use, layer = "data")
      return(list(mat = mat, layer = "data", is_counts = FALSE, assay = assay_use))
    }

    if ("counts" %in% layer_names) {
      mat <- SeuratObject::LayerData(obj, assay = assay_use, layer = "counts")
      return(list(mat = mat, layer = "counts", is_counts = TRUE, assay = assay_use))
    }

    mat <- SeuratObject::LayerData(obj, assay = assay_use, layer = layer_names[[1]])
    return(list(
      mat = mat,
      layer = layer_names[[1]],
      is_counts = grepl("count", layer_names[[1]], ignore.case = TRUE),
      assay = assay_use
    ))
  }

  mat_data <- tryCatch(GetAssayData(obj, assay = assay_use, slot = "data"), error = function(e) NULL)
  if (!is.null(mat_data) && nrow(mat_data) > 0 && ncol(mat_data) > 0) {
    return(list(mat = mat_data, layer = "data", is_counts = FALSE, assay = assay_use))
  }

  mat_counts <- GetAssayData(obj, assay = assay_use, slot = "counts")
  list(mat = mat_counts, layer = "counts", is_counts = TRUE, assay = assay_use)
}

save_plot_pdf_base <- function(p, path, width = 9, height = 7) {
  if (file.exists(path)) file.remove(path)

  ok <- FALSE
  msg <- NA_character_

  tryCatch({
    grDevices::pdf(file = path, width = width, height = height, useDingbats = FALSE, paper = "special")
    print(p)
    grDevices::dev.off()

    ok <- file.exists(path) && file.info(path)$size > 1000
    if (!ok) msg <- "PDF missing or too small"
  }, error = function(e) {
    msg <<- conditionMessage(e)
    tryCatch(grDevices::dev.off(), error = function(e2) NULL)
  })

  list(success = ok, message = msg)
}

set.seed(SEED)

stamp("读取 manifest 和 05B candidate class table。")

manifest <- fread(input_manifest, data.table = TRUE)
contrast <- fread(input_05B, data.table = TRUE)

if (!all(c("dataset", "object_id", "annotated_rds") %in% names(manifest))) {
  stop("04D manifest 缺少 dataset/object_id/annotated_rds。")
}

if (!all(c("dataset", "object_id", "group_id", "safety_contrast_class_05B") %in% names(contrast))) {
  stop("05B contrast table 缺少 dataset/object_id/group_id/safety_contrast_class_05B。")
}

cand <- manifest[
  dataset == TARGET_DATASET &
    grepl(TARGET_OBJECT_ID_CONTAINS, object_id, fixed = TRUE) &
    file.exists(annotated_rds)
]

if (nrow(cand) == 0) {
  cand <- manifest[dataset == TARGET_DATASET & file.exists(annotated_rds)]
}

if (nrow(cand) == 0) {
  stop("找不到可用对象：", TARGET_DATASET, " ; contains=", TARGET_OBJECT_ID_CONTAINS)
}

target <- cand[1]

stamp("目标对象：", target$dataset, " :: ", target$object_id)
stamp("RDS：", target$annotated_rds)

stamp("读取单个 Seurat RDS。")
obj <- readRDS(target$annotated_rds)

meta <- as.data.table(obj@meta.data)
meta[, cell := rownames(obj@meta.data)]

group_col <- choose_group_col(names(meta))
if (is.na(group_col)) stop("对象 meta.data 找不到 group column。")

sub_05B <- contrast[dataset == target$dataset & object_id == target$object_id]
if (nrow(sub_05B) == 0) {
  stop("05B 里面找不到该对象 class：", target$dataset, " :: ", target$object_id)
}

sub_05B[, group_id := as.character(group_id)]

groups_A <- unique(sub_05B[safety_contrast_class_05B %in% CLASS_A, group_id])
groups_B <- unique(sub_05B[safety_contrast_class_05B %in% CLASS_B, group_id])

if (length(groups_A) == 0 || length(groups_B) == 0) {
  stop("该对象没有足够 group 做 contrast。A groups=", length(groups_A), "; B groups=", length(groups_B))
}

meta[, group_for_08C := as.character(get(group_col))]
meta[, comparison_class_08C := "other"]
meta[group_for_08C %in% groups_A, comparison_class_08C := "A"]
meta[group_for_08C %in% groups_B, comparison_class_08C := "B"]

cells_A <- meta[comparison_class_08C == "A", cell]
cells_B <- meta[comparison_class_08C == "B", cell]

if (length(cells_A) < 20 || length(cells_B) < 20) {
  stop("细胞数不足。A=", length(cells_A), "; B=", length(cells_B))
}

stamp("meta group column：", group_col)
stamp("groups A：", paste(groups_A, collapse = ";"))
stamp("groups B：", paste(groups_B, collapse = ";"))
stamp("cells A：", length(cells_A))
stamp("cells B：", length(cells_B))

stamp("检查 biological replicate / sample-level metadata。")

rep_cols <- detect_replicate_columns(meta)

rep_audit_list <- list()

if (length(rep_cols) == 0) {
  rep_audit <- data.table(
    candidate_replicate_column = NA_character_,
    unique_values_total = NA_integer_,
    unique_values_A = NA_integer_,
    unique_values_B = NA_integer_,
    usable_for_pseudobulk = FALSE,
    note = "No candidate sample/replicate/donor/animal metadata column detected in this object."
  )
} else {
  for (cc in rep_cols) {
    vals_all <- as.character(meta[[cc]])
    vals_A <- as.character(meta[comparison_class_08C == "A", get(cc)])
    vals_B <- as.character(meta[comparison_class_08C == "B", get(cc)])

    u_all <- unique(vals_all[!is.na(vals_all) & vals_all != ""])
    u_A <- unique(vals_A[!is.na(vals_A) & vals_A != ""])
    u_B <- unique(vals_B[!is.na(vals_B) & vals_B != ""])

    usable <- length(u_A) >= 2 && length(u_B) >= 2

    rep_audit_list[[length(rep_audit_list) + 1L]] <- data.table(
      candidate_replicate_column = cc,
      unique_values_total = length(u_all),
      unique_values_A = length(u_A),
      unique_values_B = length(u_B),
      usable_for_pseudobulk = usable,
      note = ifelse(
        usable,
        "Potentially usable for downstream pseudo-bulk validation, but must confirm it represents biological replicates.",
        "Not sufficient for pseudo-bulk validation of this contrast."
      )
    )
  }

  rep_audit <- rbindlist(rep_audit_list, fill = TRUE)
}

atomic_write_csv(as.data.frame(rep_audit), replicate_audit_csv)

has_potential_pseudobulk <- any(rep_audit$usable_for_pseudobulk, na.rm = TRUE)

if (has_potential_pseudobulk) {
  stamp("检测到可能可用于 pseudo-bulk 的 replicate column；后续建议做 08E pseudo-bulk validation。")
} else {
  stamp("未检测到足够 biological replicate metadata；08C 只能作为 single-object gene-level exploratory validation。")
}

stamp("提取表达矩阵。")

expr <- get_expr_matrix(obj)
mat <- expr$mat

if (is.null(rownames(mat)) || is.null(colnames(mat))) {
  stop("表达矩阵缺少 rownames/colnames。")
}

cells_A <- intersect(cells_A, colnames(mat))
cells_B <- intersect(cells_B, colnames(mat))

if (length(cells_A) < 20 || length(cells_B) < 20) {
  stop("和 matrix 取交集后细胞数不足。")
}

genes_all <- rownames(mat)
n_genes_total <- length(genes_all)
nA <- length(cells_A)
nB <- length(cells_B)

stamp("matrix dims：", nrow(mat), " genes x ", ncol(mat), " cells")
stamp("assay/layer：", expr$assay, " / ", expr$layer)
stamp("有效 matrix cells A：", nA)
stamp("有效 matrix cells B：", nB)
stamp("总 genes：", n_genes_total)
stamp("chunk size：", GENE_CHUNK_SIZE)

chunks <- split(genes_all, ceiling(seq_along(genes_all) / GENE_CHUNK_SIZE))

deg_records <- list()
chunk_records <- list()

for (i in seq_along(chunks)) {
  genes_chunk <- chunks[[i]]

  stamp("chunk ", i, " / ", length(chunks), "；genes=", length(genes_chunk))

  res_chunk <- tryCatch({
    mat_a <- mat[genes_chunk, cells_A, drop = FALSE]
    mat_b <- mat[genes_chunk, cells_B, drop = FALSE]

    if (expr$is_counts) {
      mat_a_work <- log1p(mat_a)
      mat_b_work <- log1p(mat_b)
    } else {
      mat_a_work <- mat_a
      mat_b_work <- mat_b
    }

    mean_a_log <- Matrix::rowMeans(mat_a_work)
    mean_b_log <- Matrix::rowMeans(mat_b_work)

    pct_a <- Matrix::rowSums(mat_a_work > 0) / nA
    pct_b <- Matrix::rowSums(mat_b_work > 0) / nB

    mean_a_raw <- expm1(mean_a_log)
    mean_b_raw <- expm1(mean_b_log)

    avg_log2FC <- log2((mean_a_raw + 1e-6) / (mean_b_raw + 1e-6))

    dt0 <- data.table(
      gene = rownames(mat_a_work),
      mean_expr_A = as.numeric(mean_a_log),
      mean_expr_B = as.numeric(mean_b_log),
      pct_A = as.numeric(pct_a),
      pct_B = as.numeric(pct_b),
      avg_log2FC = as.numeric(avg_log2FC)
    )

    dt0[, max_pct := pmax(pct_A, pct_B)]
    dt <- dt0[max_pct >= MIN_PCT & !is.na(avg_log2FC)]

    if (nrow(dt) == 0) {
      rm(mat_a, mat_b, mat_a_work, mat_b_work, dt0)
      gc(verbose = FALSE)

      list(
        deg = data.table(),
        audit = data.table(
          chunk_id = i,
          genes_in_chunk = length(genes_chunk),
          genes_tested_after_min_pct = 0,
          status = "no_genes_after_min_pct"
        )
      )
    } else {
      genes_test <- dt$gene

      a_test <- mat_a_work[genes_test, , drop = FALSE]
      b_test <- mat_b_work[genes_test, , drop = FALSE]

      mean_a <- Matrix::rowMeans(a_test)
      mean_b <- Matrix::rowMeans(b_test)

      var_a <- sparse_row_vars(a_test, mean_a)
      var_b <- sparse_row_vars(b_test, mean_b)

      se <- sqrt(var_a / nA + var_b / nB)
      t_stat <- (mean_a - mean_b) / se
      t_stat[!is.finite(t_stat)] <- 0

      num <- (var_a / nA + var_b / nB) ^ 2
      den <- ((var_a / nA) ^ 2) / max(nA - 1, 1) + ((var_b / nB) ^ 2) / max(nB - 1, 1)
      df <- num / den
      df[!is.finite(df) | df < 1] <- 1

      pvals <- 2 * stats::pt(-abs(t_stat), df = df)
      pvals[!is.finite(pvals)] <- 1

      dt <- dt[match(genes_test, gene)]
      dt[, p_val := pvals]
      dt[, t_stat := as.numeric(t_stat)]
      dt[, df_welch := as.numeric(df)]

      rm(mat_a, mat_b, mat_a_work, mat_b_work, a_test, b_test, dt0)
      gc(verbose = FALSE)

      list(
        deg = dt,
        audit = data.table(
          chunk_id = i,
          genes_in_chunk = length(genes_chunk),
          genes_tested_after_min_pct = nrow(dt),
          status = "ok"
        )
      )
    }
  }, error = function(e) {
    gc(verbose = FALSE)
    list(
      deg = data.table(),
      audit = data.table(
        chunk_id = i,
        genes_in_chunk = length(genes_chunk),
        genes_tested_after_min_pct = 0,
        status = paste0("failed: ", conditionMessage(e))
      )
    )
  })

  deg_records[[length(deg_records) + 1L]] <- res_chunk$deg
  chunk_records[[length(chunk_records) + 1L]] <- res_chunk$audit

  atomic_write_csv(as.data.frame(rbindlist(chunk_records, fill = TRUE)), chunk_audit_csv)
}

deg <- rbindlist(deg_records, fill = TRUE)
chunk_audit <- rbindlist(chunk_records, fill = TRUE)

if (nrow(deg) == 0) {
  stop("所有 chunk 完成后没有任何 gene 通过 MIN_PCT 过滤。")
}

deg[, p_val_adj := safe_padj(p_val)]
deg[is.na(p_val_adj), p_val_adj := 1]
deg[, neg_log10_padj := -log10(pmax(p_val_adj, 1e-300))]

deg[, DEG_call_table := fifelse(
  p_val_adj < TABLE_PADJ_CUTOFF & avg_log2FC >= TABLE_LOG2FC_CUTOFF,
  "Up_in_A",
  fifelse(
    p_val_adj < TABLE_PADJ_CUTOFF & avg_log2FC <= -TABLE_LOG2FC_CUTOFF,
    "Up_in_B",
    "Not_significant"
  )
)]

deg[, volcano_group := "NS"]
deg[p_val_adj < VOLCANO_PADJ_CUTOFF & avg_log2FC >= VOLCANO_LOG2FC_CUTOFF, volcano_group := "UP"]
deg[p_val_adj < VOLCANO_PADJ_CUTOFF & avg_log2FC <= -VOLCANO_LOG2FC_CUTOFF, volcano_group := "DOWN"]
deg[, volcano_group := factor(volcano_group, levels = c("DOWN", "NS", "UP"))]

deg[, dataset := target$dataset]
deg[, object_id := target$object_id]
deg[, contrast_name := CONTRAST_NAME]
deg[, class_A := paste(CLASS_A, collapse = ";")]
deg[, class_B := paste(CLASS_B, collapse = ";")]
deg[, n_cells_A := nA]
deg[, n_cells_B := nB]
deg[, assay := expr$assay]
deg[, layer := expr$layer]
deg[, min_pct_filter := MIN_PCT]
deg[, replicate_metadata_available_for_pseudobulk := has_potential_pseudobulk]
deg[, test_method := "all_filtered_genes_chunked_matrix_based_Welch_style_single_object_exploratory"]

setcolorder(
  deg,
  c(
    "dataset", "object_id", "contrast_name",
    "gene", "class_A", "class_B", "n_cells_A", "n_cells_B",
    "avg_log2FC", "p_val", "p_val_adj", "neg_log10_padj",
    "mean_expr_A", "mean_expr_B", "pct_A", "pct_B", "max_pct",
    "t_stat", "df_welch",
    "DEG_call_table", "volcano_group",
    "assay", "layer", "min_pct_filter",
    "replicate_metadata_available_for_pseudobulk",
    "test_method"
  )
)

deg <- deg[order(p_val_adj, -abs(avg_log2FC))]
sig <- deg[DEG_call_table != "Not_significant"]

atomic_write_csv(as.data.frame(deg), deg_all_csv)
atomic_write_csv(as.data.frame(sig), deg_sig_csv)
atomic_write_csv(as.data.frame(chunk_audit), chunk_audit_csv)

stamp("生成 volcano plot table。")

plot_dt <- copy(deg)
plot_dt[, log2FoldChange := avg_log2FC]
plot_dt[, padj := p_val_adj]
plot_dt[, mlog10 := neg_log10_padj]

plot_dt[, x_capped := pmax(pmin(log2FoldChange, X_CAP), -X_CAP)]
plot_dt[, y_capped := pmin(mlog10, Y_CAP)]

label_pool <- plot_dt[volcano_group != "NS"]
if (nrow(label_pool) == 0) label_pool <- copy(plot_dt)
label_pool <- label_pool[order(padj, -abs(log2FoldChange))]
label_genes <- head(unique(label_pool$gene), TOP_LABEL_GENES)

plot_dt[, label := NA_character_]
plot_dt[gene %in% label_genes, label := gene]
label_dt <- plot_dt[!is.na(label)]

atomic_write_csv(as.data.frame(plot_dt), plot_table_csv)
atomic_write_csv(as.data.frame(label_dt), label_table_csv)

stamp("输出 volcano PDF。")

DEG_COLORS <- c(
  "UP" = "#E64B35",
  "DOWN" = "#4DBBD5",
  "NS" = "grey75"
)

plot_title <- paste0(target$dataset, " | ", CONTRAST_NAME)
plot_subtitle <- paste0(
  "A: ideal-like DA/projection-high safety-low vs B: lower-priority/mixed | all filtered genes: ",
  nrow(plot_dt)
)

make_volcano <- function(df, labels, capped = TRUE) {
  df_plot <- copy(df)
  lab_plot <- copy(labels)

  if (capped) {
    df_plot[, x_plot := x_capped]
    df_plot[, y_plot := y_capped]
    lab_plot[, x_plot := x_capped]
    lab_plot[, y_plot := y_capped]

    x_limits <- c(-X_CAP, X_CAP)
    y_limits <- c(0, Y_CAP)
    x_breaks <- seq(-X_CAP, X_CAP, by = 2)
    caption <- paste0(
      "Display capped at |log2FC| <= ", X_CAP,
      " and -log10(padj) <= ", Y_CAP,
      "; testing included all genes with MIN_PCT >= ", MIN_PCT,
      "."
    )
  } else {
    df_plot[, x_plot := log2FoldChange]
    df_plot[, y_plot := mlog10]
    lab_plot[, x_plot := log2FoldChange]
    lab_plot[, y_plot := mlog10]

    x_limits <- NULL
    y_limits <- NULL
    x_breaks <- pretty(df_plot$x_plot, n = 6)
    caption <- "Full-axis audit plot; no display cap."
  }

  ggplot(df_plot, aes(x = x_plot, y = y_plot)) +
    geom_point(aes(color = volcano_group), size = POINT_SIZE, alpha = POINT_ALPHA) +
    scale_color_manual(
      values = DEG_COLORS,
      breaks = c("UP", "DOWN", "NS"),
      name = NULL
    ) +
    geom_vline(
      xintercept = c(-VOLCANO_LOG2FC_CUTOFF, VOLCANO_LOG2FC_CUTOFF),
      linetype = "dashed",
      linewidth = 0.60,
      color = "grey40"
    ) +
    geom_hline(
      yintercept = -log10(VOLCANO_PADJ_CUTOFF),
      linetype = "dashed",
      linewidth = 0.60,
      color = "grey40"
    ) +
    geom_text_repel(
      data = lab_plot,
      aes(x = x_plot, y = y_plot, label = label),
      inherit.aes = FALSE,
      size = 3,
      max.overlaps = Inf,
      box.padding = 0.50,
      point.padding = 0.25,
      segment.color = "grey50",
      min.segment.length = 0
    ) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    coord_cartesian(ylim = y_limits, clip = "off") +
    theme_classic(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey95", linewidth = 0.3),
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 9.5),
      plot.caption = element_text(size = 8, color = "grey35", hjust = 0.5),
      axis.title = element_text(face = "bold", color = "black"),
      axis.text = element_text(color = "black"),
      plot.margin = margin(12, 34, 14, 12)
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "log2 Fold Change",
      y = "-log10 adjusted p-value",
      caption = caption
    )
}

pdf_capped <- file.path(out_figures_dir, "08C_JOURNAL_all_filtered_genes_volcano_capped_publication.pdf")
pdf_full <- file.path(out_figures_dir, "08C_JOURNAL_all_filtered_genes_volcano_full_axis_audit.pdf")

p_capped <- make_volcano(plot_dt, label_dt, capped = TRUE)
p_full <- make_volcano(plot_dt, label_dt, capped = FALSE)

res_capped <- save_plot_pdf_base(p_capped, pdf_capped, width = 9, height = 7)
res_full <- save_plot_pdf_base(p_full, pdf_full, width = 9, height = 7)

figure_index <- data.table(
  figure_type = c("all_filtered_genes_capped_publication", "all_filtered_genes_full_axis_audit"),
  pdf_path = c(pdf_capped, pdf_full),
  success = c(res_capped$success, res_full$success),
  message = c(res_capped$message, res_full$message)
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

summary_dt <- data.table(
  metric = c(
    "dataset",
    "object_id",
    "contrast_name",
    "class_A",
    "class_B",
    "group_col",
    "groups_A",
    "groups_B",
    "cells_A",
    "cells_B",
    "matrix_genes_total",
    "min_pct_filter",
    "genes_tested_after_min_pct",
    "arbitrary_top_gene_cap_used",
    "p_adjust_scope",
    "chunk_size",
    "chunks_total",
    "chunks_failed",
    "DEG_table_up_in_A_log2FC_0.25",
    "DEG_table_up_in_B_log2FC_0.25",
    "volcano_UP_log2FC_1",
    "volcano_DOWN_log2FC_1",
    "volcano_NS",
    "assay",
    "layer",
    "potential_replicate_metadata_for_pseudobulk",
    "capped_pdf",
    "full_axis_pdf",
    "claim_boundary"
  ),
  value = c(
    target$dataset,
    target$object_id,
    CONTRAST_NAME,
    paste(CLASS_A, collapse = ";"),
    paste(CLASS_B, collapse = ";"),
    group_col,
    paste(groups_A, collapse = ";"),
    paste(groups_B, collapse = ";"),
    nA,
    nB,
    n_genes_total,
    MIN_PCT,
    nrow(deg),
    "FALSE",
    "BH correction across all genes tested after MIN_PCT filtering",
    GENE_CHUNK_SIZE,
    length(chunks),
    sum(grepl("^failed", chunk_audit$status)),
    nrow(deg[DEG_call_table == "Up_in_A"]),
    nrow(deg[DEG_call_table == "Up_in_B"]),
    nrow(plot_dt[volcano_group == "UP"]),
    nrow(plot_dt[volcano_group == "DOWN"]),
    nrow(plot_dt[volcano_group == "NS"]),
    expr$assay,
    expr$layer,
    has_potential_pseudobulk,
    pdf_capped,
    pdf_full,
    "Journal-aware gene-level single-object exploratory validation; not a sample-level pseudo-bulk validation unless 08E confirms replicate-level results."
  )
)

atomic_write_csv(as.data.frame(summary_dt), summary_csv)

method_lines <- c(
  "08C method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "For the selected object-level candidate-state comparison, genes expressed in at least ",
    MIN_PCT * 100,
    "% of cells in either comparison group were retained. Differential expression was tested across all retained genes using a chunked matrix-based Welch-style procedure, and Benjamini-Hochberg correction was applied across all tested genes."
  ),
  "",
  "Figure caption-ready wording:",
  paste0(
    "Volcano plot of all genes passing the expression filter (MIN_PCT >= ",
    MIN_PCT,
    ") for ideal-like DA/projection-high safety-low versus lower-priority/mixed states. Displayed log2FC and -log10 adjusted P values were capped for visualization only; statistical classification used the uncapped values."
  ),
  "",
  "Claim boundary:",
  "This analysis is appropriate as a gene-level exploratory validation of transcriptional separation between candidate cell states within a selected object. It should not be described as sample-level pseudo-bulk validation, clinical prediction, functional integration, anatomical-projection claim, tumorigenicity proof, or therapeutic outcome validation.",
  "",
  "Replicate audit:",
  paste0("Potential replicate metadata usable for pseudo-bulk: ", has_potential_pseudobulk),
  paste0("Replicate audit table: ", replicate_audit_csv)
)

writeLines(method_lines, method_note_txt)

report_lines <- c(
  "08C JOURNAL-STANDARD all-filtered-genes chunked DEG volcano report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Target:",
  paste0("Dataset: ", target$dataset),
  paste0("Object ID: ", target$object_id),
  paste0("Contrast: ", CONTRAST_NAME),
  paste0("Cells A: ", nA),
  paste0("Cells B: ", nB),
  "",
  "Academic-standard correction:",
  paste0("Matrix genes total: ", n_genes_total),
  paste0("MIN_PCT filter: ", MIN_PCT),
  paste0("Genes tested after MIN_PCT: ", nrow(deg)),
  "No arbitrary top-gene cap was used.",
  "p.adjust was performed across all genes tested after objective expression filtering.",
  "",
  "Result summary:",
  paste0("DEG table Up_in_A at |log2FC| >= ", TABLE_LOG2FC_CUTOFF, ": ", nrow(deg[DEG_call_table == "Up_in_A"])),
  paste0("DEG table Up_in_B at |log2FC| >= ", TABLE_LOG2FC_CUTOFF, ": ", nrow(deg[DEG_call_table == "Up_in_B"])),
  paste0("Volcano style UP at |log2FC| >= ", VOLCANO_LOG2FC_CUTOFF, ": ", nrow(plot_dt[volcano_group == "UP"])),
  paste0("Volcano style DOWN at |log2FC| >= ", VOLCANO_LOG2FC_CUTOFF, ": ", nrow(plot_dt[volcano_group == "DOWN"])),
  paste0("Chunks failed: ", sum(grepl("^failed", chunk_audit$status))),
  paste0("Potential replicate metadata for pseudo-bulk: ", has_potential_pseudobulk),
  "",
  "Outputs:",
  paste0("All filtered genes DEG: ", deg_all_csv),
  paste0("Significant DEG: ", deg_sig_csv),
  paste0("Replicate audit: ", replicate_audit_csv),
  paste0("Chunk audit: ", chunk_audit_csv),
  paste0("Capped volcano PDF: ", pdf_capped),
  paste0("Full-axis volcano PDF: ", pdf_full),
  paste0("Method note: ", method_note_txt),
  paste0("Summary: ", summary_csv),
  "",
  "Next step:",
  "Use the significant DEG table for 08D GO/KEGG only if chunks_failed = 0. For stronger journal-level biological claims, add 08E pseudo-bulk replicate-level validation if replicate metadata are available.",
  "",
  "Journal-rigor note:",
  "This script corrects the earlier 10000-gene exploratory compromise. It tests all genes passing objective expression filtering and adjusts P values across all tested genes. However, it remains a selected single-object cell-state contrast unless replicated by a pseudo-bulk/sample-level analysis."
)

writeLines(report_lines, report_txt)

rm(obj, mat)
gc(verbose = FALSE)

cat("\n============================================================\n")
cat("08C JOURNAL-STANDARD all-filtered-genes chunked DEG volcano 运行结束\n")
cat("============================================================\n\n")

cat("Dataset：", target$dataset, "\n")
cat("Object ID：", target$object_id, "\n")
cat("Contrast：", CONTRAST_NAME, "\n")
cat("Cells A：", nA, "\n")
cat("Cells B：", nB, "\n")
cat("Matrix genes total：", n_genes_total, "\n")
cat("MIN_PCT：", MIN_PCT, "\n")
cat("Genes tested after MIN_PCT：", nrow(deg), "\n")
cat("Arbitrary top-gene cap used：FALSE\n")
cat("p.adjust scope：all tested filtered genes\n")
cat("Chunks total：", length(chunks), "\n")
cat("Chunks failed：", sum(grepl('^failed', chunk_audit$status)), "\n")
cat("DEG table Up_in_A：", nrow(deg[DEG_call_table == "Up_in_A"]), "\n")
cat("DEG table Up_in_B：", nrow(deg[DEG_call_table == "Up_in_B"]), "\n")
cat("Volcano style UP / DOWN / NS：", nrow(plot_dt[volcano_group == "UP"]), " / ", nrow(plot_dt[volcano_group == "DOWN"]), " / ", nrow(plot_dt[volcano_group == "NS"]), "\n")
cat("Potential replicate metadata for pseudo-bulk：", has_potential_pseudobulk, "\n")
cat("Capped PDF success：", res_capped$success, "\n")
cat("Full-axis PDF success：", res_full$success, "\n\n")

cat("输出文件：\n")
cat(deg_all_csv, "\n")
cat(deg_sig_csv, "\n")
cat(plot_table_csv, "\n")
cat(label_table_csv, "\n")
cat(summary_csv, "\n")
cat(chunk_audit_csv, "\n")
cat(replicate_audit_csv, "\n")
cat(figure_index_csv, "\n")
cat(method_note_txt, "\n")
cat(report_txt, "\n\n")

cat("输出 PDF：\n")
cat(pdf_capped, "\n")
cat(pdf_full, "\n\n")

cat("✅ 08C JOURNAL-STANDARD all-filtered-genes chunked DEG volcano 完成。\n")
cat("下一步：检查 summary 里的 Chunks failed 是否为 0，并检查 capped PDF；若通过，再用 significant DEG table 做 08D GO/KEGG。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

INPUT_08C_DEG_ALL <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_all_filtered_genes_DEG_table.csv"
)

INPUT_08C_CHUNK_AUDIT <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_chunk_audit.csv"
)

REQUIRE_08C_CHUNKS_FAILED_ZERO <- TRUE

DEG_PADJ_CUTOFF <- 0.05
DEG_LOG2FC_CUTOFF <- 0.25

ENRICH_PVALUE_CUTOFF <- 1
ENRICH_QVALUE_CUTOFF <- 1

PLOT_PADJ_CUTOFF <- 0.05
MIN_GENES_FOR_ENRICHMENT <- 10
MIN_GS_SIZE <- 10
MAX_GS_SIZE <- 500

GO_TOP_TERMS_PER_ONTOLOGY <- 5
GO_MAX_GENES <- 42
GO_MAX_GENES_PER_TERM <- 8
GO_MAX_TERMS_PER_GENE <- 4
GO_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT <- FALSE

GO_LINK_ALPHA_IN_COLOR <- 0.26
GO_LINK_LAYER_ALPHA <- 0.55
GO_LINK_WIDTH <- 0.42

GO_PDF_WIDTH <- 13.5
GO_PDF_HEIGHT <- 12.5

SEED <- 20260714

options(timeout = 60000)

cat("\n============================================================\n")
cat("08D1 GO FINAL VERIFIED V2：GO gene-term relationship UP/DOWN\n")
cat("============================================================\n\n")

required_pkgs <- c(
  "data.table",
  "dplyr",
  "ggplot2",
  "clusterProfiler",
  "msigdbr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  cran_pkgs <- intersect(missing_pkgs, c("data.table", "dplyr", "ggplot2", "msigdbr"))
  bioc_pkgs <- setdiff(missing_pkgs, cran_pkgs)

  if (length(cran_pkgs) > 0) {
    install.packages(cran_pkgs, repos = "https://cloud.r-project.org")
  }

  if (length(bioc_pkgs) > 0) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager", repos = "https://cloud.r-project.org")
    }
    BiocManager::install(bioc_pkgs, ask = FALSE, update = FALSE, force = FALSE)
  }
}

missing_pkgs2 <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs2) > 0) {
  stop("以下 R 包仍然缺失：", paste(missing_pkgs2, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(clusterProfiler)
  library(msigdbr)
})

options(error = NULL)
options(bitmapType = "cairo")
set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "08D1_GO_FINAL_VERIFIED_V2")
out_figures_dir <- file.path(figures_dir, "08D1_GO_FINAL_VERIFIED_V2_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

gene_list_summary_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_gene_list_summary.csv")
gene_list_long_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_gene_lists_long.csv")
go_term2gene_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_TERM2GENE_MSigDB_C5_GO.csv")

go_enrichment_all_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_all_GO_enrichment_results.csv")
go_enrichment_sig_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_significant_GO_enrichment_results.csv")
go_task_status_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_enrichment_task_status.csv")

go_up_plot_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_UP_gene_term_plot_table.csv")
go_down_plot_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_DOWN_gene_term_plot_table.csv")
go_up_link_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_UP_gene_term_links.csv")
go_down_link_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_DOWN_gene_term_links.csv")

figure_index_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_sessionInfo.txt")
report_txt <- file.path(reports_dir, "08D1_GO_FINAL_VERIFIED_V2_report.txt")

go_up_pdf <- file.path(out_figures_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_UP_gene_term_relationship_BP_CC_MF.pdf")
go_down_pdf <- file.path(out_figures_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_DOWN_gene_term_relationship_BP_CC_MF.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  data.table::fwrite(df, path)

  if (!file.exists(path)) {
    stop("CSV 未生成：", path)
  }

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

save_pdf_plot <- function(plot_obj, file_path, width, height) {
  if (is.null(plot_obj)) {
    stop("plot_obj 是 NULL，不能保存：", file_path)
  }

  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(file_path)) {
    unlink(file_path, force = TRUE)
  }

  ggplot2::ggsave(
    filename = file_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::pdf,
    useDingbats = FALSE,
    limitsize = FALSE
  )

  if (!file.exists(file_path)) {
    stop("PDF 未生成：", file_path)
  }

  size_bytes <- file.info(file_path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", file_path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(file_path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")

  invisible(file_path)
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

clean_gene <- function(x) {
  x <- unique(as.character(x))
  x <- trimws(x)
  x <- x[!is.na(x)]
  x <- x[x != "" & x != "-"]
  x <- sub("\\..*$", "", x)
  unique(x)
}

clean_gene_keep_length <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- sub("\\..*$", "", x)
  x
}

parse_ratio <- function(x) {
  sapply(strsplit(as.character(x), "/", fixed = TRUE), function(z) {
    if (length(z) == 2) {
      return(as.numeric(z[1]) / as.numeric(z[2]))
    } else {
      return(as.numeric(z[1]))
    }
  })
}

detect_go_ontology <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  out[grepl("^GOBP_|BIOLOGICAL_PROCESS|\\bBP\\b", x, ignore.case = TRUE)] <- "BP"
  out[grepl("^GOCC_|CELLULAR_COMPONENT|\\bCC\\b", x, ignore.case = TRUE)] <- "CC"
  out[grepl("^GOMF_|MOLECULAR_FUNCTION|\\bMF\\b", x, ignore.case = TRUE)] <- "MF"
  out
}

clean_go_term <- function(x) {
  x <- as.character(x)
  x <- gsub("^GOBP_", "", x)
  x <- gsub("^GOCC_", "", x)
  x <- gsub("^GOMF_", "", x)
  x <- gsub("_", " ", x)
  x <- tolower(trimws(x))
  x
}

shorten_term <- function(x, max_chars = 72) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
}

split_gene_id <- function(x) {
  x <- as.character(x)
  x <- strsplit(x, "/", fixed = TRUE)
  unique(unlist(x))
}

ontology_strip_fill <- c(
  BP = "#E6EEF7",
  CC = "#EAF4EA",
  MF = "#F3E6F3"
)

make_distinct_palette <- function(n, alpha = 1) {
  base_cols <- c(
    "#7FCDBB", "#B2DF8A", "#FDBF6F", "#CAB2D6", "#A6CEE3",
    "#FB9A99", "#FFFF99", "#B15928", "#8DD3C7", "#BEBADA",
    "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
    "#BC80BD", "#CCEBC5", "#FFED6F", "#1F78B4", "#33A02C",
    "#E31A1C", "#FF7F00", "#6A3D9A", "#A6761D", "#666666"
  )

  if (n <= length(base_cols)) {
    cols <- base_cols[seq_len(n)]
  } else {
    cols <- grDevices::colorRampPalette(base_cols)(n)
  }

  grDevices::adjustcolor(cols, alpha.f = alpha)
}

stamp("读取 08C JOURNAL DEG table。")

if (!file.exists(INPUT_08C_DEG_ALL)) {
  stop("找不到 08C all-filtered DEG table：", INPUT_08C_DEG_ALL)
}

deg <- fread(INPUT_08C_DEG_ALL, data.table = TRUE, showProgress = FALSE)

required_deg_cols <- c("gene", "avg_log2FC", "p_val_adj")
missing_cols <- setdiff(required_deg_cols, names(deg))
if (length(missing_cols) > 0) {
  stop("08C DEG table 缺少列：", paste(missing_cols, collapse = ", "))
}

deg[, gene := clean_gene_keep_length(gene)]
deg[, avg_log2FC := safe_num(avg_log2FC)]
deg[, p_val_adj := safe_num(p_val_adj)]

deg <- deg[
  !is.na(gene) &
    gene != "" &
    !is.na(avg_log2FC) &
    !is.na(p_val_adj)
]

if (nrow(deg) == 0) {
  stop("08C DEG table 过滤后没有有效 genes。")
}

if (file.exists(INPUT_08C_CHUNK_AUDIT)) {
  chunk_audit <- fread(INPUT_08C_CHUNK_AUDIT, data.table = TRUE)
  chunks_failed <- sum(grepl("^failed", chunk_audit$status), na.rm = TRUE)

  if (isTRUE(REQUIRE_08C_CHUNKS_FAILED_ZERO) && chunks_failed > 0) {
    stop(
      "08C chunk audit 显示有 failed chunks：", chunks_failed,
      "。期刊标准下不继续 08D1。请先修复 08C。"
    )
  }
} else {
  chunks_failed <- NA_integer_
  warning("没有找到 08C chunk audit；08D1 会继续，但 final 解释时需要人工确认 08C 完整性。")
}

dataset_name <- if ("dataset" %in% names(deg)) unique(deg$dataset)[1] else "Dataset"
contrast_name <- if ("contrast_name" %in% names(deg)) unique(deg$contrast_name)[1] else "contrast"

stamp("Dataset：", dataset_name)
stamp("Contrast：", contrast_name)
stamp("All tested filtered genes rows：", nrow(deg))
stamp("08C chunks failed：", chunks_failed)

universe_genes <- clean_gene(deg$gene)

up_genes <- clean_gene(deg[
  p_val_adj < DEG_PADJ_CUTOFF &
    avg_log2FC >= DEG_LOG2FC_CUTOFF,
  gene
])

down_genes <- clean_gene(deg[
  p_val_adj < DEG_PADJ_CUTOFF &
    avg_log2FC <= -DEG_LOG2FC_CUTOFF,
  gene
])

gene_list_summary <- data.table(
  list_name = c("universe_all_tested_filtered_genes", "UP_in_A_ideal_like", "DOWN_in_A_up_in_B_lower_priority"),
  definition = c(
    "All genes tested in 08C after objective expression filtering",
    paste0("p_adj < ", DEG_PADJ_CUTOFF, " and avg_log2FC >= ", DEG_LOG2FC_CUTOFF),
    paste0("p_adj < ", DEG_PADJ_CUTOFF, " and avg_log2FC <= -", DEG_LOG2FC_CUTOFF)
  ),
  n_gene_symbols = c(length(universe_genes), length(up_genes), length(down_genes))
)

gene_list_long <- rbindlist(list(
  data.table(list_name = "universe_all_tested_filtered_genes", gene = universe_genes),
  data.table(list_name = "UP_in_A_ideal_like", gene = up_genes),
  data.table(list_name = "DOWN_in_A_up_in_B_lower_priority", gene = down_genes)
), fill = TRUE)

atomic_write_csv(as.data.frame(gene_list_summary), gene_list_summary_csv)
atomic_write_csv(as.data.frame(gene_list_long), gene_list_long_csv)

stamp("Universe genes：", length(universe_genes))
stamp("UP genes：", length(up_genes))
stamp("DOWN genes：", length(down_genes))

if (length(up_genes) < MIN_GENES_FOR_ENRICHMENT && length(down_genes) < MIN_GENES_FOR_ENRICHMENT) {
  stop("UP 和 DOWN genes 都少于 MIN_GENES_FOR_ENRICHMENT，无法做稳定 GO 富集。")
}

stamp("读取 MSigDB GO gene sets。")

get_msigdb_safe <- function(collection, subcollection = NULL) {
  out <- tryCatch({
    if (is.null(subcollection)) {
      suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", collection = collection))
    } else {
      suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", collection = collection, subcollection = subcollection))
    }
  }, error = function(e1) {
    tryCatch({
      if (is.null(subcollection)) {
        suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", category = collection))
      } else {
        suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", category = collection, subcategory = subcollection))
      }
    }, error = function(e2) {
      NULL
    })
  })

  out
}

msig_go_raw <- get_msigdb_safe("C5")
if (is.null(msig_go_raw) || nrow(msig_go_raw) == 0) {
  stop("无法读取 MSigDB C5 GO gene sets。")
}

msig_go <- msig_go_raw

go_keep <- rep(FALSE, nrow(msig_go))

if ("gs_name" %in% names(msig_go)) {
  go_keep <- go_keep |
    grepl("^GOBP_", msig_go$gs_name) |
    grepl("^GOCC_", msig_go$gs_name) |
    grepl("^GOMF_", msig_go$gs_name)
}

if ("gs_subcat" %in% names(msig_go)) {
  go_keep <- go_keep | grepl("GO", msig_go$gs_subcat, ignore.case = TRUE)
}

if ("gs_subcollection" %in% names(msig_go)) {
  go_keep <- go_keep | grepl("GO", msig_go$gs_subcollection, ignore.case = TRUE)
}

if (any(go_keep)) {
  msig_go <- msig_go[go_keep, ]
}

if (!"gs_name" %in% names(msig_go) || !"gene_symbol" %in% names(msig_go)) {
  stop("MSigDB GO 数据缺少 gs_name 或 gene_symbol 列。")
}

go_term2gene <- as.data.table(msig_go)[, .(term = as.character(gs_name), gene = as.character(gene_symbol))]
go_term2gene <- unique(go_term2gene[!is.na(term) & !is.na(gene) & gene != ""])
go_term2gene <- go_term2gene[gene %in% universe_genes]

atomic_write_csv(as.data.frame(go_term2gene), go_term2gene_csv)

stamp("GO terms in universe：", length(unique(go_term2gene$term)))
stamp("GO TERM2GENE rows：", nrow(go_term2gene))

if (nrow(go_term2gene) < MIN_GENES_FOR_ENRICHMENT) {
  stop("GO TERM2GENE 太少，无法做 GO 富集。")
}

stamp("运行 clusterProfiler::enricher GO enrichment。")

run_go_enricher_task <- function(task_id, Direction, biological_direction, genes) {
  out <- tryCatch({
    obj <- clusterProfiler::enricher(
      gene = unique(genes),
      universe = unique(universe_genes),
      TERM2GENE = as.data.frame(go_term2gene[, .(term, gene)]),
      pAdjustMethod = "BH",
      pvalueCutoff = ENRICH_PVALUE_CUTOFF,
      qvalueCutoff = ENRICH_QVALUE_CUTOFF,
      minGSSize = MIN_GS_SIZE,
      maxGSSize = MAX_GS_SIZE
    )

    dt <- as.data.table(as.data.frame(obj))

    if (nrow(dt) > 0) {
      dt[, task_id := task_id]
      dt[, enrichment_type := "GO"]
      dt[, Direction := Direction]
      dt[, biological_direction := biological_direction]
      dt[, input_gene_symbol_n := length(unique(genes))]
      dt[, universe_gene_symbol_n := length(unique(universe_genes))]
      dt[, gene_set_source := "MSigDB_C5_GO"]
      dt[, enrichment_method := "clusterProfiler::enricher"]
      dt[, GeneRatioNum := parse_ratio(GeneRatio)]
      dt[, p.adjust := as.numeric(p.adjust)]
      dt[, Count := as.numeric(Count)]
      dt[, Description := as.character(Description)]
      dt[, ID := as.character(ID)]
      dt[, ontology := detect_go_ontology(ID)]
      dt[is.na(ontology), ontology := detect_go_ontology(Description)]
    }

    list(
      task_id = task_id,
      Direction = Direction,
      biological_direction = biological_direction,
      status = "ok",
      message = NA_character_,
      n_terms = nrow(dt),
      result = dt
    )
  }, error = function(e) {
    list(
      task_id = task_id,
      Direction = Direction,
      biological_direction = biological_direction,
      status = "failed",
      message = conditionMessage(e),
      n_terms = 0L,
      result = data.table()
    )
  })

  out
}

go_results <- list()

if (length(up_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("GO enrichment task：GO_UP")
  go_results[["GO_UP"]] <- run_go_enricher_task("GO_UP", "UP", "UP_in_A_ideal_like", up_genes)
}

if (length(down_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("GO enrichment task：GO_DOWN")
  go_results[["GO_DOWN"]] <- run_go_enricher_task("GO_DOWN", "DOWN", "DOWN_in_A_up_in_B_lower_priority", down_genes)
}

go_task_status <- rbindlist(lapply(go_results, function(x) {
  data.table(
    task_id = x$task_id,
    enrichment_type = "GO",
    Direction = x$Direction,
    biological_direction = x$biological_direction,
    status = x$status,
    message = x$message,
    n_terms = x$n_terms
  )
}), fill = TRUE)

go_enrich_all <- rbindlist(lapply(go_results, function(x) x$result), fill = TRUE)

if (nrow(go_enrich_all) > 0) {
  go_enrich_all <- go_enrich_all[order(Direction, ontology, p.adjust)]
}

go_enrich_sig <- if (nrow(go_enrich_all) > 0) {
  go_enrich_all[!is.na(p.adjust) & p.adjust <= PLOT_PADJ_CUTOFF]
} else {
  data.table()
}

atomic_write_csv(as.data.frame(go_task_status), go_task_status_csv)
atomic_write_csv(as.data.frame(go_enrich_all), go_enrichment_all_csv)
atomic_write_csv(as.data.frame(go_enrich_sig), go_enrichment_sig_csv)

stamp("GO tasks failed：", nrow(go_task_status[status == "failed"]))
stamp("GO enrichment rows all：", nrow(go_enrich_all))
stamp("GO enrichment rows significant：", nrow(go_enrich_sig))

select_go_terms_for_relation <- function(enrich_dt, direction_value) {
  dt <- as.data.table(enrich_dt)
  dt <- dt[
    Direction == direction_value &
      ontology %in% c("BP", "CC", "MF") &
      !is.na(geneID) &
      geneID != ""
  ]

  if (nrow(dt) == 0) return(data.table())

  dt_sig <- dt[p.adjust <= PLOT_PADJ_CUTOFF]
  display_mode <- "significant_FDR_le_0.05"

  if (nrow(dt_sig) == 0) {
    if (!isTRUE(GO_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT)) {
      return(data.table())
    }
    dt_sig <- copy(dt)
    display_mode <- "fallback_top_ranked_not_significant"
  }

  out <- rbindlist(lapply(c("BP", "CC", "MF"), function(ont) {
    sub <- dt_sig[ontology == ont]
    if (nrow(sub) == 0) return(data.table())
    sub <- sub[order(p.adjust, -Count)]
    head(sub, GO_TOP_TERMS_PER_ONTOLOGY)
  }), fill = TRUE)

  if (nrow(out) > 0) {
    out[, display_mode := display_mode]
    out[, term_clean := shorten_term(clean_go_term(Description), max_chars = 68)]
    out[, neg_log10_padj := -log10(p.adjust)]
  }

  out
}

build_go_relation_tables <- function(go_terms_dt, direction_value) {
  if (nrow(go_terms_dt) == 0) {
    return(list(term_df = data.table(), gene_df = data.table(), link_df = data.table()))
  }

  go_terms_dt <- copy(go_terms_dt)
  go_terms_dt[, ontology := factor(ontology, levels = c("BP", "CC", "MF"))]
  setorder(go_terms_dt, ontology, p.adjust, -Count)

  links <- rbindlist(lapply(seq_len(nrow(go_terms_dt)), function(i) {
    genes_i <- split_gene_id(go_terms_dt$geneID[i])
    genes_i <- genes_i[genes_i %in% if (direction_value == "UP") up_genes else down_genes]

    if (length(genes_i) == 0) return(data.table())

    data.table(
      term_id = go_terms_dt$ID[i],
      term_clean = go_terms_dt$term_clean[i],
      ontology = as.character(go_terms_dt$ontology[i]),
      Direction = direction_value,
      gene = genes_i
    )
  }), fill = TRUE)

  if (nrow(links) == 0) {
    return(list(term_df = data.table(), gene_df = data.table(), link_df = data.table()))
  }

  gene_stats <- unique(deg[, .(gene, avg_log2FC, p_val_adj)])

  links <- merge(
    links,
    gene_stats,
    by = "gene",
    all.x = TRUE
  )

  links[is.na(avg_log2FC), avg_log2FC := 0]
  links[is.na(p_val_adj), p_val_adj := 1]

  links[, direction_log2FC := if (direction_value == "UP") avg_log2FC else -avg_log2FC]
  links[is.na(direction_log2FC) | direction_log2FC < 0, direction_log2FC := 0]
  links[, gene_priority := -log10(pmax(p_val_adj, .Machine$double.xmin)) * (direction_log2FC + 0.01)]

  term_priority <- go_terms_dt[, .(
    term_id = ID,
    term_p_adjust = p.adjust,
    term_count = Count,
    term_gene_ratio = GeneRatioNum
  )]

  links <- merge(
    links,
    term_priority,
    by = "term_id",
    all.x = TRUE
  )

  setorder(links, term_id, -gene_priority, p_val_adj, -direction_log2FC, gene)
  links <- links[, head(.SD, GO_MAX_GENES_PER_TERM), by = term_id]

  setorder(links, gene, term_p_adjust, -gene_priority, term_id)
  links <- links[, head(.SD, GO_MAX_TERMS_PER_GENE), by = gene]

  gene_rank <- links[, .(
    link_count = .N,
    best_gene_priority = max(gene_priority, na.rm = TRUE),
    best_p = min(p_val_adj, na.rm = TRUE)
  ), by = gene][order(-best_gene_priority, best_p, -link_count, gene)]

  keep_genes <- head(gene_rank$gene, GO_MAX_GENES)
  links <- links[gene %in% keep_genes]

  keep_terms <- unique(links$term_id)
  go_terms_dt <- go_terms_dt[ID %in% keep_terms]

  term_df_list <- list()
  y_current <- 1

  for (ont in c("MF", "CC", "BP")) {
    sub <- go_terms_dt[as.character(ontology) == ont]
    if (nrow(sub) == 0) next

    sub <- sub[order(p.adjust, -Count)]
    sub[, y := seq(from = y_current, length.out = .N)]
    y_current <- max(sub$y) + 1.6
    term_df_list[[ont]] <- sub
  }

  term_df <- rbindlist(term_df_list, fill = TRUE)

  if (nrow(term_df) == 0) {
    return(list(term_df = data.table(), gene_df = data.table(), link_df = data.table()))
  }

  term_palette <- make_distinct_palette(nrow(term_df), alpha = 1)
  term_df[, term_color := term_palette]

  links <- merge(
    links,
    term_df[, .(term_id = ID, term_y = y, GeneRatioNum, Count, p.adjust, neg_log10_padj, display_mode, term_color)],
    by = "term_id",
    all.x = TRUE
  )

  links <- links[!is.na(term_y)]

  gene_rank <- links[, .N, by = gene][order(-N, gene)]
  gene_rank[, y := seq(from = min(term_df$y), to = max(term_df$y), length.out = .N)]

  gene_df <- gene_rank[, .(gene, gene_y = y, link_count = N)]

  gene_palette <- make_distinct_palette(nrow(gene_df), alpha = 0.85)
  gene_df[, gene_color := gene_palette]

  links <- merge(links, gene_df, by = "gene", all.x = TRUE)
  links[, link_color := grDevices::adjustcolor(term_color, alpha.f = GO_LINK_ALPHA_IN_COLOR)]

  list(term_df = term_df, gene_df = gene_df, link_df = links)
}

make_go_gene_term_plot <- function(go_terms_dt, direction_value, plot_title) {
  tbl <- build_go_relation_tables(go_terms_dt, direction_value)

  term_df <- tbl$term_df
  gene_df <- tbl$gene_df
  link_df <- tbl$link_df

  if (nrow(term_df) == 0 || nrow(gene_df) == 0 || nrow(link_df) == 0) {
    stop("GO ", direction_value, " gene-term plot table 为空，拒绝输出白图。")
  }

  dot_x_min <- 0.68
  dot_x_max <- 0.96
  dot_width <- dot_x_max - dot_x_min

  gr_max <- max(term_df$GeneRatioNum, na.rm = TRUE)
  if (!is.finite(gr_max) || gr_max <= 0) gr_max <- 0.01

  term_df[, dot_x := dot_x_min + (GeneRatioNum / gr_max) * dot_width]

  panel_df <- term_df[, .(
    ymin = min(y) - 0.5,
    ymax = max(y) + 0.5,
    ymid = mean(range(y))
  ), by = ontology]

  panel_df[, xmin := dot_x_min - 0.02]
  panel_df[, xmax := dot_x_max + 0.02]
  panel_df[, strip_xmin := dot_x_max + 0.025]
  panel_df[, strip_xmax := dot_x_max + 0.055]

  panel_df[, strip_xmid := (strip_xmin + strip_xmax) / 2]
  panel_df[, ontology_label := as.character(ontology)]
  panel_df[, strip_fill_col := ontology_strip_fill[as.character(ontology)]]

  y_grid_min_value <- min(panel_df$ymin)
  y_grid_max_value <- max(panel_df$ymax)

  gene_df[, xmin := 0.035]
  gene_df[, xmax := 0.058]
  gene_df[, ymin := gene_y - 0.33]
  gene_df[, ymax := gene_y + 0.33]

  term_df[, xmin := 0.580]
  term_df[, xmax := 0.618]
  term_df[, ymin := y - 0.38]
  term_df[, ymax := y + 0.38]

  y_min <- min(c(term_df$y, gene_df$gene_y), na.rm = TRUE) - 1.0
  y_max <- max(c(term_df$y, gene_df$gene_y), na.rm = TRUE) + 1.0

  ratio_ticks <- pretty(c(0, gr_max), n = 4)
  ratio_ticks <- ratio_ticks[ratio_ticks >= 0 & ratio_ticks <= gr_max]
  axis_df <- data.table(
    ratio = ratio_ticks,
    x = dot_x_min + (ratio_ticks / gr_max) * dot_width,
    y = y_min + 0.15,
    label = formatC(ratio_ticks, format = "f", digits = 2)
  )

  axis_df[, y_grid_min := y_grid_min_value]
  axis_df[, y_grid_max := y_grid_max_value]

  term_df[, grid_xmin := dot_x_min - 0.02]
  term_df[, grid_xmax := dot_x_max + 0.02]

  p <- ggplot() +
    geom_rect(
      data = panel_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white",
      color = "black",
      linewidth = 0.45
    ) +
    geom_rect(
      data = panel_df,
      aes(xmin = strip_xmin, xmax = strip_xmax, ymin = ymin, ymax = ymax),
      fill = panel_df$strip_fill_col,
      color = "black",
      linewidth = 0.35
    ) +
    geom_text(
      data = panel_df,
      aes(x = strip_xmid, y = ymid, label = ontology_label),
      angle = 90,
      fontface = "bold",
      size = 5.0
    ) +
    geom_segment(
      data = axis_df,
      aes(x = x, xend = x, y = y_grid_min, yend = y_grid_max),
      color = "grey92",
      linewidth = 0.25
    ) +
    geom_segment(
      data = term_df,
      aes(x = grid_xmin, xend = grid_xmax, y = y, yend = y),
      color = "grey94",
      linewidth = 0.25
    )

  p <- p +
    geom_curve(
      data = link_df,
      aes(x = 0.06, y = gene_y, xend = 0.585, yend = term_y, color = I(link_color)),
      curvature = 0.22,
      alpha = GO_LINK_LAYER_ALPHA,
      linewidth = GO_LINK_WIDTH
    )

  p <- p +
    geom_rect(
      data = gene_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(gene_color)),
      color = NA
    ) +
    geom_text(
      data = gene_df,
      aes(x = 0.028, y = gene_y, label = gene),
      hjust = 1,
      size = 3.2,
      fontface = "bold"
    )

  p <- p +
    geom_rect(
      data = term_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(term_color)),
      color = NA
    )

  p <- p +
    geom_text(
      data = term_df,
      aes(x = 0.575, y = y, label = term_clean),
      hjust = 1,
      size = 4.0,
      fontface = "bold"
    ) +
    geom_point(
      data = term_df,
      aes(x = dot_x, y = y, size = Count, color = neg_log10_padj),
      alpha = 0.90
    ) +
    scale_size_continuous(name = "Count", range = c(3.5, 9.5)) +
    scale_color_gradient(
      name = "-log10(p.adjust)",
      low = "#4EDAC0",
      high = "red"
    ) +
    geom_segment(
      aes(x = dot_x_min, xend = dot_x_max, y = y_min + 0.35, yend = y_min + 0.35),
      color = "black",
      linewidth = 0.45
    ) +
    geom_segment(
      data = axis_df,
      aes(x = x, xend = x, y = y_min + 0.27, yend = y_min + 0.43),
      color = "black",
      linewidth = 0.35
    ) +
    geom_text(
      data = axis_df,
      aes(x = x, y = y_min + 0.05, label = label),
      size = 3.6
    ) +
    annotate(
      "text",
      x = 0.31,
      y = y_min - 0.45,
      label = "Gene-Term relationship",
      fontface = "bold",
      size = 5.3
    ) +
    annotate(
      "text",
      x = (dot_x_min + dot_x_max) / 2,
      y = y_min - 0.45,
      label = "GeneRatio",
      fontface = "bold",
      size = 5.0
    ) +
    labs(title = plot_title) +
    coord_cartesian(xlim = c(0, 1.08), ylim = c(y_min - 0.8, y_max), clip = "off") +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(8, 25, 25, 8)
    )

  list(plot = p, term_df = term_df, gene_df = gene_df, link_df = link_df)
}

stamp("生成 GO gene-term relationship 图。")

go_up_terms <- select_go_terms_for_relation(go_enrich_all, "UP")
go_down_terms <- select_go_terms_for_relation(go_enrich_all, "DOWN")

go_up_obj <- make_go_gene_term_plot(
  go_up_terms,
  direction_value = "UP",
  plot_title = "GO gene-term relationship: ideal-like upregulated"
)

go_down_obj <- make_go_gene_term_plot(
  go_down_terms,
  direction_value = "DOWN",
  plot_title = "GO gene-term relationship: lower-priority upregulated"
)

atomic_write_csv(as.data.frame(go_up_obj$term_df), go_up_plot_table_csv)
atomic_write_csv(as.data.frame(go_down_obj$term_df), go_down_plot_table_csv)
atomic_write_csv(as.data.frame(go_up_obj$link_df), go_up_link_table_csv)
atomic_write_csv(as.data.frame(go_down_obj$link_df), go_down_link_table_csv)

save_pdf_plot(go_up_obj$plot, go_up_pdf, width = GO_PDF_WIDTH, height = GO_PDF_HEIGHT)
save_pdf_plot(go_down_obj$plot, go_down_pdf, width = GO_PDF_WIDTH, height = GO_PDF_HEIGHT)

figure_index <- data.table(
  figure_id = c("GO_UP_gene_term", "GO_DOWN_gene_term"),
  title = c(
    "GO gene-term relationship: ideal-like upregulated",
    "GO gene-term relationship: lower-priority upregulated"
  ),
  pdf_path = c(go_up_pdf, go_down_pdf),
  table_path = c(go_up_plot_table_csv, go_down_plot_table_csv),
  pdf_size_bytes = c(file.info(go_up_pdf)$size, file.info(go_down_pdf)$size),
  plot_type = c("GO_gene_term_relationship_BP_CC_MF", "GO_gene_term_relationship_BP_CC_MF"),
  plot_engine = "ggplot2"
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

method_lines <- c(
  "08D1 GO FINAL VERIFIED V2 method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "GO over-representation analysis was performed using clusterProfiler::enricher with MSigDB C5 GO gene sets obtained through the msigdbr R package. ",
    "GO gene sets were separated into BP, CC, and MF categories according to the GO term prefix. ",
    "Genes upregulated in the ideal-like DA/projection-high safety-low state and genes upregulated in the lower-priority/mixed state were analysed separately. ",
    "The enrichment universe was defined as all genes tested in the 08C differential expression analysis after objective expression filtering. ",
    "GO results were visualized as gene-term relationship plots linking selected representative genes to enriched GO terms, with gene-specific node colors, term-specific node colors, semi-transparent term-colored links, aligned GeneRatio bubbles, and BP/CC/MF category strips. ",
    "For visualization clarity, representative genes were selected per term based on differential-expression evidence, and each gene was allowed to connect to at most a limited number of GO terms; all displayed links represent true gene-term membership. The GO_UP panel should be interpreted as a coherent mitochondrial oxidative phosphorylation/electron-transport module because multiple enriched GO terms share overlapping respiratory-chain genes."
  ),
  "",
  "Strict parameters:",
  paste0("DEG list cutoff: FDR < ", DEG_PADJ_CUTOFF, " and |log2FC| >= ", DEG_LOG2FC_CUTOFF),
  paste0("Enrichment method: clusterProfiler::enricher; BH correction; minGSSize = ", MIN_GS_SIZE, "; maxGSSize = ", MAX_GS_SIZE),
  paste0("Universe: all ", length(universe_genes), " genes tested in 08C after objective expression filtering"),
  "",
  "Claim boundary:",
  "08D1 inherits the claim boundary of 08C. If 08C is a single-object cell-state contrast, then 08D1 supports pathway-level interpretation of that contrast, not sample-level pseudo-bulk validation or clinical/functional proof."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "08D1 GO FINAL VERIFIED V2 report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  INPUT_08C_DEG_ALL,
  "",
  "Gene lists:",
  paste0("Universe gene symbols: ", length(universe_genes)),
  paste0("UP gene symbols: ", length(up_genes)),
  paste0("DOWN gene symbols: ", length(down_genes)),
  "",
  "GO gene sets:",
  paste0("GO terms in universe: ", length(unique(go_term2gene$term))),
  paste0("GO TERM2GENE rows: ", nrow(go_term2gene)),
  "",
  "GO enrichment results:",
  paste0("GO tasks failed: ", nrow(go_task_status[status == "failed"])),
  paste0("All GO enrichment rows: ", nrow(go_enrich_all)),
  paste0("Significant GO enrichment rows: ", nrow(go_enrich_sig)),
  "",
  "Figures:",
  capture.output(print(figure_index)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)

required_output_files <- c(
  gene_list_summary_csv,
  gene_list_long_csv,
  go_term2gene_csv,
  go_enrichment_all_csv,
  go_enrichment_sig_csv,
  go_task_status_csv,
  go_up_plot_table_csv,
  go_down_plot_table_csv,
  go_up_link_table_csv,
  go_down_link_table_csv,
  figure_index_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  go_up_pdf,
  go_down_pdf
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(file.exists(required_output_files), file.info(required_output_files)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

missing_required <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]

if (nrow(missing_required) > 0) {
  print(missing_required)
  stop("08D1 GO FINAL VERIFIED V2 未通过输出验证。")
}

cat("\n============================================================\n")
cat("08D1 GO FINAL VERIFIED V2 运行结束\n")
cat("============================================================\n\n")

cat("GO：gene-term relationship UP/DOWN\n")
cat("Universe genes：", length(universe_genes), "\n")
cat("UP genes：", length(up_genes), "\n")
cat("DOWN genes：", length(down_genes), "\n")
cat("GO terms in universe：", length(unique(go_term2gene$term)), "\n")
cat("GO tasks failed：", nrow(go_task_status[status == "failed"]), "\n")
cat("All GO enrichment rows：", nrow(go_enrich_all), "\n")
cat("Significant GO enrichment rows：", nrow(go_enrich_sig), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要 PDF 图：\n")
cat(go_up_pdf, "\n")
cat(go_down_pdf, "\n\n")

cat("✅ 08D1 GO FINAL VERIFIED V2 完成。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

INPUT_08C_DEG_ALL <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_all_filtered_genes_DEG_table.csv"
)

INPUT_08C_CHUNK_AUDIT <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_chunk_audit.csv"
)

REQUIRE_08C_CHUNKS_FAILED_ZERO <- TRUE

DEG_PADJ_CUTOFF <- 0.05
DEG_LOG2FC_CUTOFF <- 0.25

ENRICH_PVALUE_CUTOFF <- 1
ENRICH_QVALUE_CUTOFF <- 1

PLOT_PADJ_CUTOFF <- 0.05
KEGG_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT <- FALSE

MIN_GENES_FOR_ENRICHMENT <- 10
MIN_GS_SIZE <- 10
MAX_GS_SIZE <- 500

KEGG_TOP_N_PER_DIRECTION <- 15

KEGG_PDF_WIDTH <- 11.8
KEGG_PDF_HEIGHT <- 8.8

SEED <- 20260714

options(timeout = 60000)

cat("\n============================================================\n")
cat("08D2 KEGG FINAL：UP/DOWN dotplot\n")
cat("============================================================\n\n")

required_pkgs <- c(
  "data.table",
  "dplyr",
  "ggplot2",
  "clusterProfiler",
  "msigdbr"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "缺少 R 包，请先手动安装：",
    paste(missing_pkgs, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(clusterProfiler)
  library(msigdbr)
})

options(error = NULL)
options(bitmapType = "cairo")
set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "08D2_KEGG_FINAL")
out_figures_dir <- file.path(figures_dir, "08D2_KEGG_FINAL_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

gene_list_summary_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_gene_list_summary.csv")
gene_list_long_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_gene_lists_long.csv")
kegg_term2gene_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_KEGG_TERM2GENE_MSigDB_C2_CP_KEGG_no_MEDICUS.csv")

kegg_enrichment_all_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_all_KEGG_enrichment_results.csv")
kegg_enrichment_sig_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_significant_KEGG_enrichment_results.csv")
kegg_task_status_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_KEGG_enrichment_task_status.csv")
kegg_plot_table_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_KEGG_UP_DOWN_DOTPLOT_table.csv")

figure_index_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08D2_KEGG_FINAL_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "08D2_KEGG_FINAL_sessionInfo.txt")
report_txt <- file.path(reports_dir, "08D2_KEGG_FINAL_report.txt")

kegg_pdf <- file.path(out_figures_dir, "08D2_KEGG_FINAL_KEGG_UP_DOWN_DOTPLOT.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  data.table::fwrite(df, path)

  if (!file.exists(path)) {
    stop("CSV 未生成：", path)
  }

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

save_pdf_plot <- function(plot_obj, file_path, width, height) {
  if (is.null(plot_obj)) {
    stop("plot_obj 是 NULL，不能保存：", file_path)
  }

  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(file_path)) {
    removed <- file.remove(file_path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", file_path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑，或删除旧 PDF。"
      )
    }
  }

  ggplot2::ggsave(
    filename = file_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::pdf,
    useDingbats = FALSE,
    limitsize = FALSE
  )

  if (!file.exists(file_path)) {
    stop("PDF 未生成：", file_path)
  }

  size_bytes <- file.info(file_path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", file_path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(file_path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")

  invisible(file_path)
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

clean_gene <- function(x) {
  x <- unique(as.character(x))
  x <- trimws(x)
  x <- x[!is.na(x)]
  x <- x[x != "" & x != "-"]
  x <- sub("\\..*$", "", x)
  unique(x)
}

clean_gene_keep_length <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- sub("\\..*$", "", x)
  x
}

parse_ratio <- function(x) {
  sapply(strsplit(as.character(x), "/", fixed = TRUE), function(z) {
    if (length(z) == 2) {
      return(as.numeric(z[1]) / as.numeric(z[2]))
    } else {
      return(as.numeric(z[1]))
    }
  })
}

clean_kegg_term <- function(x) {
  x <- as.character(x)
  x <- gsub("^KEGG_", "", x)
  x <- gsub("_", " ", x)
  x <- tolower(trimws(x))
  x
}

shorten_term <- function(x, max_chars = 78) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
}

stamp("读取 08C JOURNAL DEG table。")

if (!file.exists(INPUT_08C_DEG_ALL)) {
  stop("找不到 08C all-filtered DEG table：", INPUT_08C_DEG_ALL)
}

deg <- fread(INPUT_08C_DEG_ALL, data.table = TRUE, showProgress = FALSE)

required_deg_cols <- c("gene", "avg_log2FC", "p_val_adj")
missing_cols <- setdiff(required_deg_cols, names(deg))
if (length(missing_cols) > 0) {
  stop("08C DEG table 缺少列：", paste(missing_cols, collapse = ", "))
}

deg[, gene := clean_gene_keep_length(gene)]
deg[, avg_log2FC := safe_num(avg_log2FC)]
deg[, p_val_adj := safe_num(p_val_adj)]

deg <- deg[
  !is.na(gene) &
    gene != "" &
    !is.na(avg_log2FC) &
    !is.na(p_val_adj)
]

if (nrow(deg) == 0) {
  stop("08C DEG table 过滤后没有有效 genes。")
}

if (file.exists(INPUT_08C_CHUNK_AUDIT)) {
  chunk_audit <- fread(INPUT_08C_CHUNK_AUDIT, data.table = TRUE)
  chunks_failed <- sum(grepl("^failed", chunk_audit$status), na.rm = TRUE)

  if (isTRUE(REQUIRE_08C_CHUNKS_FAILED_ZERO) && chunks_failed > 0) {
    stop(
      "08C chunk audit 显示有 failed chunks：", chunks_failed,
      "。期刊标准下不继续 08D2。请先修复 08C。"
    )
  }
} else {
  chunks_failed <- NA_integer_
  warning("没有找到 08C chunk audit；08D2 会继续，但 final 解释时需要人工确认 08C 完整性。")
}

dataset_name <- if ("dataset" %in% names(deg)) unique(deg$dataset)[1] else "Dataset"
contrast_name <- if ("contrast_name" %in% names(deg)) unique(deg$contrast_name)[1] else "contrast"

stamp("Dataset：", dataset_name)
stamp("Contrast：", contrast_name)
stamp("All tested filtered genes rows：", nrow(deg))
stamp("08C chunks failed：", chunks_failed)

universe_genes <- clean_gene(deg$gene)

up_genes <- clean_gene(deg[
  p_val_adj < DEG_PADJ_CUTOFF &
    avg_log2FC >= DEG_LOG2FC_CUTOFF,
  gene
])

down_genes <- clean_gene(deg[
  p_val_adj < DEG_PADJ_CUTOFF &
    avg_log2FC <= -DEG_LOG2FC_CUTOFF,
  gene
])

gene_list_summary <- data.table(
  list_name = c("universe_all_tested_filtered_genes", "UP_in_A_ideal_like", "DOWN_in_A_up_in_B_lower_priority"),
  definition = c(
    "All genes tested in 08C after objective expression filtering",
    paste0("p_adj < ", DEG_PADJ_CUTOFF, " and avg_log2FC >= ", DEG_LOG2FC_CUTOFF),
    paste0("p_adj < ", DEG_PADJ_CUTOFF, " and avg_log2FC <= -", DEG_LOG2FC_CUTOFF)
  ),
  n_gene_symbols = c(length(universe_genes), length(up_genes), length(down_genes))
)

gene_list_long <- rbindlist(list(
  data.table(list_name = "universe_all_tested_filtered_genes", gene = universe_genes),
  data.table(list_name = "UP_in_A_ideal_like", gene = up_genes),
  data.table(list_name = "DOWN_in_A_up_in_B_lower_priority", gene = down_genes)
), fill = TRUE)

atomic_write_csv(as.data.frame(gene_list_summary), gene_list_summary_csv)
atomic_write_csv(as.data.frame(gene_list_long), gene_list_long_csv)

stamp("Universe genes：", length(universe_genes))
stamp("UP genes：", length(up_genes))
stamp("DOWN genes：", length(down_genes))

if (length(up_genes) < MIN_GENES_FOR_ENRICHMENT && length(down_genes) < MIN_GENES_FOR_ENRICHMENT) {
  stop("UP 和 DOWN genes 都少于 MIN_GENES_FOR_ENRICHMENT，无法做稳定 KEGG 富集。")
}

stamp("读取 MSigDB KEGG gene sets。")

get_msigdb_safe <- function(collection, subcollection = NULL) {
  out <- tryCatch({
    if (is.null(subcollection)) {
      suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", collection = collection))
    } else {
      suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", collection = collection, subcollection = subcollection))
    }
  }, error = function(e1) {
    tryCatch({
      if (is.null(subcollection)) {
        suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", category = collection))
      } else {
        suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", category = collection, subcategory = subcollection))
      }
    }, error = function(e2) {
      NULL
    })
  })

  out
}

msig_kegg <- get_msigdb_safe("C2", "CP:KEGG")

if (is.null(msig_kegg) || nrow(msig_kegg) == 0) {
  msig_c2 <- get_msigdb_safe("C2")
  if (!is.null(msig_c2) && nrow(msig_c2) > 0) {
    keep <- rep(FALSE, nrow(msig_c2))
    if ("gs_name" %in% names(msig_c2)) {
      keep <- keep | grepl("KEGG", msig_c2$gs_name, ignore.case = TRUE)
    }
    if ("gs_subcat" %in% names(msig_c2)) {
      keep <- keep | grepl("KEGG", msig_c2$gs_subcat, ignore.case = TRUE)
    }
    if ("gs_subcollection" %in% names(msig_c2)) {
      keep <- keep | grepl("KEGG", msig_c2$gs_subcollection, ignore.case = TRUE)
    }
    msig_kegg <- msig_c2[keep, ]
  }
}

if (is.null(msig_kegg) || nrow(msig_kegg) == 0) {
  stop("无法读取 MSigDB KEGG gene sets。")
}

if (!"gs_name" %in% names(msig_kegg) || !"gene_symbol" %in% names(msig_kegg)) {
  stop("MSigDB KEGG 数据缺少 gs_name 或 gene_symbol 列。")
}

kegg_term2gene <- as.data.table(msig_kegg)[, .(term = as.character(gs_name), gene = as.character(gene_symbol))]
kegg_term2gene <- unique(kegg_term2gene[!is.na(term) & !is.na(gene) & gene != ""])

kegg_term2gene <- kegg_term2gene[!grepl("MEDICUS", term, ignore.case = TRUE)]

kegg_term2gene <- kegg_term2gene[gene %in% universe_genes]

atomic_write_csv(as.data.frame(kegg_term2gene), kegg_term2gene_csv)

stamp("KEGG terms in universe：", length(unique(kegg_term2gene$term)))
stamp("KEGG TERM2GENE rows：", nrow(kegg_term2gene))

if (nrow(kegg_term2gene) < MIN_GENES_FOR_ENRICHMENT) {
  stop("KEGG TERM2GENE 太少，无法做 KEGG 富集。")
}

stamp("运行 clusterProfiler::enricher KEGG enrichment。")

run_kegg_enricher_task <- function(task_id, Direction, biological_direction, genes) {
  out <- tryCatch({
    obj <- clusterProfiler::enricher(
      gene = unique(genes),
      universe = unique(universe_genes),
      TERM2GENE = as.data.frame(kegg_term2gene[, .(term, gene)]),
      pAdjustMethod = "BH",
      pvalueCutoff = ENRICH_PVALUE_CUTOFF,
      qvalueCutoff = ENRICH_QVALUE_CUTOFF,
      minGSSize = MIN_GS_SIZE,
      maxGSSize = MAX_GS_SIZE
    )

    dt <- as.data.table(as.data.frame(obj))

    if (nrow(dt) > 0) {
      dt[, task_id := task_id]
      dt[, enrichment_type := "KEGG"]
      dt[, Direction := Direction]
      dt[, biological_direction := biological_direction]
      dt[, input_gene_symbol_n := length(unique(genes))]
      dt[, universe_gene_symbol_n := length(unique(universe_genes))]
      dt[, gene_set_source := "MSigDB_C2_CP_KEGG_no_MEDICUS"]
      dt[, enrichment_method := "clusterProfiler::enricher"]
      dt[, GeneRatioNum := parse_ratio(GeneRatio)]
      dt[, p.adjust := as.numeric(p.adjust)]
      dt[, Count := as.numeric(Count)]
      dt[, Description := as.character(Description)]
    }

    list(
      task_id = task_id,
      Direction = Direction,
      biological_direction = biological_direction,
      status = "ok",
      message = NA_character_,
      n_terms = nrow(dt),
      result = dt
    )
  }, error = function(e) {
    list(
      task_id = task_id,
      Direction = Direction,
      biological_direction = biological_direction,
      status = "failed",
      message = conditionMessage(e),
      n_terms = 0L,
      result = data.table()
    )
  })

  out
}

kegg_results <- list()

if (length(up_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("KEGG enrichment task：KEGG_UP")
  kegg_results[["KEGG_UP"]] <- run_kegg_enricher_task("KEGG_UP", "UP", "UP_in_A_ideal_like", up_genes)
}

if (length(down_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("KEGG enrichment task：KEGG_DOWN")
  kegg_results[["KEGG_DOWN"]] <- run_kegg_enricher_task("KEGG_DOWN", "DOWN", "DOWN_in_A_up_in_B_lower_priority", down_genes)
}

kegg_task_status <- rbindlist(lapply(kegg_results, function(x) {
  data.table(
    task_id = x$task_id,
    enrichment_type = "KEGG",
    Direction = x$Direction,
    biological_direction = x$biological_direction,
    status = x$status,
    message = x$message,
    n_terms = x$n_terms
  )
}), fill = TRUE)

kegg_enrich_all <- rbindlist(lapply(kegg_results, function(x) x$result), fill = TRUE)

if (nrow(kegg_enrich_all) > 0) {
  kegg_enrich_all <- kegg_enrich_all[order(Direction, p.adjust)]
}

kegg_enrich_sig <- if (nrow(kegg_enrich_all) > 0) {
  kegg_enrich_all[!is.na(p.adjust) & p.adjust <= PLOT_PADJ_CUTOFF]
} else {
  data.table()
}

atomic_write_csv(as.data.frame(kegg_task_status), kegg_task_status_csv)
atomic_write_csv(as.data.frame(kegg_enrich_all), kegg_enrichment_all_csv)
atomic_write_csv(as.data.frame(kegg_enrich_sig), kegg_enrichment_sig_csv)

stamp("KEGG tasks failed：", nrow(kegg_task_status[status == "failed"]))
stamp("KEGG enrichment rows all：", nrow(kegg_enrich_all))
stamp("KEGG enrichment rows significant：", nrow(kegg_enrich_sig))

theme_dot_original <- theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 19),
    axis.title = element_text(face = "plain", color = "black", size = 16),
    axis.text = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    legend.title = element_text(face = "plain", size = 14),
    legend.text = element_text(size = 11),
    legend.position = "right",
    plot.margin = margin(10, 25, 10, 55)
  )

make_kegg_subtable <- function(enrich_dt, direction, top_n, allow_fallback) {
  dt <- as.data.table(enrich_dt)
  dt <- dt[Direction == direction]
  dt <- dt[!is.na(p.adjust) & !is.na(GeneRatioNum) & !is.na(Count)]

  if (nrow(dt) == 0) return(data.table())

  dt_sig <- dt[p.adjust <= PLOT_PADJ_CUTOFF]
  display_mode <- "significant_FDR_le_0.05"

  if (nrow(dt_sig) == 0) {
    if (!isTRUE(allow_fallback)) return(data.table())
    dt_sig <- copy(dt)
    display_mode <- "fallback_top_ranked_not_significant"
  }

  dt_sig <- dt_sig[order(p.adjust, -Count)]
  dt_sig <- head(dt_sig, top_n)
  dt_sig[, Term := shorten_term(clean_kegg_term(Description), max_chars = 78)]
  dt_sig[, display_mode := display_mode]
  dt_sig[, significant_for_display := p.adjust <= PLOT_PADJ_CUTOFF]

  dt_sig
}

make_kegg_plot_table <- function(enrich_dt) {
  down <- make_kegg_subtable(enrich_dt, "DOWN", KEGG_TOP_N_PER_DIRECTION, KEGG_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT)
  up <- make_kegg_subtable(enrich_dt, "UP", KEGG_TOP_N_PER_DIRECTION, KEGG_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT)

  if (nrow(down) > 0) {
    down <- down[rev(seq_len(nrow(down)))]
    down[, y := seq_len(.N)]
  }

  if (nrow(up) > 0) {
    up <- up[rev(seq_len(nrow(up)))]
    up[, y := seq(from = nrow(down) + 3, length.out = .N)]
  }

  rbindlist(list(down, up), fill = TRUE)
}

make_kegg_dotplot_original <- function(plot_df, title) {
  if (nrow(plot_df) == 0) stop("KEGG plot table 为空，不画白图。")

  x_max <- max(plot_df$GeneRatioNum, na.rm = TRUE)
  if (!is.finite(x_max) || x_max <= 0) x_max <- 0.01

  x_upper <- x_max * 1.12

  min_point_x <- min(plot_df$GeneRatioNum[plot_df$GeneRatioNum > 0], na.rm = TRUE)
  if (!is.finite(min_point_x) || min_point_x <= 0) min_point_x <- x_upper * 0.08

  line_x <- max(x_upper * 0.012, min_point_x * 0.35)
  label_x <- line_x

  x_breaks <- pretty(c(0, x_upper), n = 4)
  x_breaks <- x_breaks[x_breaks >= 0 & x_breaks <= x_upper]

  if (any(plot_df$display_mode == "fallback_top_ranked_not_significant", na.rm = TRUE)) {
    title <- paste0(title, " (top-ranked terms; none FDR <= 0.05)")
  }

  p <- ggplot(plot_df, aes(x = GeneRatioNum, y = y)) +
    geom_point(aes(size = Count, color = p.adjust), alpha = 0.95) +
    scale_color_gradient(low = "red", high = "blue", name = "p.adj") +
    scale_size_continuous(name = "Count", range = c(3.2, 9.5)) +
    scale_y_continuous(
      breaks = plot_df$y,
      labels = plot_df$Term,
      expand = expansion(add = c(0.8, 1.2))
    ) +
    scale_x_continuous(
      limits = c(0, x_upper),
      breaks = x_breaks,
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    labs(title = title, x = "GeneRatio", y = "Pathway name") +
    coord_cartesian(clip = "off") +
    theme_dot_original

  up_df <- plot_df[Direction == "UP"]
  down_df <- plot_df[Direction == "DOWN"]

  if (nrow(up_df) > 0) {
    p <- p +
      annotate("segment", x = line_x, xend = line_x, y = min(up_df$y), yend = max(up_df$y), color = "red", linewidth = 1.1) +
      annotate("text", x = label_x, y = max(up_df$y) + 0.85, label = "Up-regulated", color = "red", size = 4.0, hjust = 0)
  }

  if (nrow(down_df) > 0) {
    p <- p +
      annotate("segment", x = line_x, xend = line_x, y = min(down_df$y), yend = max(down_df$y), color = "darkgreen", linewidth = 1.1) +
      annotate("text", x = label_x, y = max(down_df$y) + 0.85, label = "Down-regulated", color = "darkgreen", size = 4.0, hjust = 0)
  }

  p
}

stamp("生成 KEGG UP/DOWN dotplot。")

kegg_plot_table <- make_kegg_plot_table(kegg_enrich_all)

if (nrow(kegg_plot_table) == 0) {
  stop("KEGG plot table 为空，不生成白图。")
}

atomic_write_csv(as.data.frame(kegg_plot_table), kegg_plot_table_csv)

kegg_plot <- make_kegg_dotplot_original(
  kegg_plot_table,
  title = "KEGG: ideal-like vs lower-priority"
)

save_pdf_plot(kegg_plot, kegg_pdf, width = KEGG_PDF_WIDTH, height = KEGG_PDF_HEIGHT)

figure_index <- data.table(
  figure_id = "KEGG_UP_DOWN_dotplot",
  title = "KEGG: ideal-like vs lower-priority",
  pdf_path = kegg_pdf,
  table_path = kegg_plot_table_csv,
  pdf_size_bytes = file.info(kegg_pdf)$size,
  plot_type = "KEGG_original_UP_DOWN_dotplot",
  plot_engine = "ggplot2"
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

method_lines <- c(
  "08D2 KEGG FINAL method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "KEGG over-representation analysis was performed using clusterProfiler::enricher with MSigDB C2 CP:KEGG gene sets obtained through the msigdbr R package after excluding MEDICUS-specific entries. ",
    "Genes upregulated in the ideal-like DA/projection-high safety-low state and genes upregulated in the lower-priority/mixed state were analysed separately. ",
    "The enrichment universe was defined as all genes tested in the 08C differential expression analysis after objective expression filtering. ",
    "KEGG results were visualized using the original UP/DOWN dotplot layout."
  ),
  "",
  "Strict parameters:",
  paste0("DEG list cutoff: FDR < ", DEG_PADJ_CUTOFF, " and |log2FC| >= ", DEG_LOG2FC_CUTOFF),
  paste0("Enrichment method: clusterProfiler::enricher; BH correction; minGSSize = ", MIN_GS_SIZE, "; maxGSSize = ", MAX_GS_SIZE),
  paste0("Universe: all ", length(universe_genes), " genes tested in 08C after objective expression filtering"),
  "",
  "Claim boundary:",
  "08D2 inherits the claim boundary of 08C. If 08C is a single-object cell-state contrast, then 08D2 supports pathway-level interpretation of that contrast, not sample-level pseudo-bulk validation or clinical/functional proof."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "08D2 KEGG FINAL report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  INPUT_08C_DEG_ALL,
  "",
  "Gene lists:",
  paste0("Universe gene symbols: ", length(universe_genes)),
  paste0("UP gene symbols: ", length(up_genes)),
  paste0("DOWN gene symbols: ", length(down_genes)),
  "",
  "KEGG gene sets:",
  paste0("KEGG terms in universe: ", length(unique(kegg_term2gene$term))),
  paste0("KEGG TERM2GENE rows: ", nrow(kegg_term2gene)),
  "",
  "KEGG enrichment results:",
  paste0("KEGG tasks failed: ", nrow(kegg_task_status[status == "failed"])),
  paste0("All KEGG enrichment rows: ", nrow(kegg_enrich_all)),
  paste0("Significant KEGG enrichment rows: ", nrow(kegg_enrich_sig)),
  "",
  "Figures:",
  capture.output(print(figure_index)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)

required_output_files <- c(
  gene_list_summary_csv,
  gene_list_long_csv,
  kegg_term2gene_csv,
  kegg_enrichment_all_csv,
  kegg_enrichment_sig_csv,
  kegg_task_status_csv,
  kegg_plot_table_csv,
  figure_index_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  kegg_pdf
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(file.exists(required_output_files), file.info(required_output_files)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

missing_required <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]

if (nrow(missing_required) > 0) {
  print(missing_required)
  stop("08D2 KEGG FINAL 未通过输出验证。")
}

cat("\n============================================================\n")
cat("08D2 KEGG FINAL 运行结束\n")
cat("============================================================\n\n")

cat("KEGG：UP/DOWN dotplot\n")
cat("Universe genes：", length(universe_genes), "\n")
cat("UP genes：", length(up_genes), "\n")
cat("DOWN genes：", length(down_genes), "\n")
cat("KEGG terms in universe：", length(unique(kegg_term2gene$term)), "\n")
cat("KEGG tasks failed：", nrow(kegg_task_status[status == "failed"]), "\n")
cat("All KEGG enrichment rows：", nrow(kegg_enrich_all), "\n")
cat("Significant KEGG enrichment rows：", nrow(kegg_enrich_sig), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要 PDF 图：\n")
cat(kegg_pdf, "\n\n")

cat("✅ 08D2 KEGG FINAL 完成。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

INPUT_08C_DEG_ALL <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_all_filtered_genes_DEG_table.csv"
)

INPUT_08C_CHUNK_AUDIT <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_chunk_audit.csv"
)

REQUIRE_08C_CHUNKS_FAILED_ZERO <- TRUE

SPECIES_FOR_MSIGDB <- "Homo sapiens"

MIN_GS_SIZE <- 10
MAX_GS_SIZE <- 500

GSEA_PVALUE_CUTOFF <- 1
GSEA_PADJUST_METHOD <- "BH"
PLOT_PADJ_CUTOFF <- 0.05

PLOT_TOP_N_POSITIVE_NES <- 15
PLOT_TOP_N_NEGATIVE_NES <- 15

PDF_WIDTH <- 10.8
PDF_HEIGHT <- 8.2

SEED <- 20260714

options(timeout = 60000)

cat("\n============================================================\n")
cat("08E Hallmark GSEA FINAL V4\n")
cat("============================================================\n\n")

required_pkgs <- c(
  "data.table",
  "dplyr",
  "ggplot2",
  "fgsea",
  "BiocParallel",
  "msigdbr"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "缺少 R 包，请先手动安装：",
    paste(missing_pkgs, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(fgsea)
  library(BiocParallel)
  library(msigdbr)
})

BiocParallel::register(BiocParallel::SerialParam(), default = TRUE)

options(error = NULL)
options(bitmapType = "cairo")
set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "08E_HALLMARK_GSEA_FINAL_V4")
out_figures_dir <- file.path(figures_dir, "08E_HALLMARK_GSEA_FINAL_V4_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

ranked_gene_table_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_ranked_gene_list.csv")
ranked_gene_duplicate_audit_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_ranked_gene_duplicate_audit.csv")
hallmark_term2gene_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_MSigDB_Hallmark_TERM2GENE.csv")
hallmark_geneset_summary_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_geneset_summary.csv")

gsea_all_results_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_all_results.csv")
gsea_significant_results_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_significant_results.csv")
gsea_plot_table_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_plot_table.csv")
gsea_direction_summary_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_direction_summary.csv")

figure_index_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_sessionInfo.txt")
report_txt <- file.path(reports_dir, "08E_HALLMARK_GSEA_FINAL_V4_report.txt")

gsea_dotplot_pdf <- file.path(out_figures_dir, "08E_HALLMARK_GSEA_FINAL_V4_NES_DOTPLOT.pdf")
gsea_barplot_pdf <- file.path(out_figures_dir, "08E_HALLMARK_GSEA_FINAL_V4_NES_BARPLOT.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  data.table::fwrite(df, path)

  if (!file.exists(path)) {
    stop("CSV 未生成：", path)
  }

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

save_pdf_plot <- function(plot_obj, file_path, width, height) {
  if (is.null(plot_obj)) {
    stop("plot_obj 是 NULL，不能保存：", file_path)
  }

  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(file_path)) {
    removed <- file.remove(file_path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", file_path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑，或删除旧 PDF。"
      )
    }
  }

  ggplot2::ggsave(
    filename = file_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::pdf,
    useDingbats = FALSE,
    limitsize = FALSE
  )

  if (!file.exists(file_path)) {
    stop("PDF 未生成：", file_path)
  }

  size_bytes <- file.info(file_path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", file_path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(file_path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")

  invisible(file_path)
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

clean_gene_keep_length <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- sub("\\..*$", "", x)
  x
}

clean_hallmark_name <- function(x) {
  x <- as.character(x)
  x <- gsub("^HALLMARK_", "", x)
  x <- gsub("_", " ", x)
  x <- tolower(trimws(x))
  x
}

shorten_term <- function(x, max_chars = 70) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
}

stamp("读取 08C JOURNAL all-filtered DEG table。")

if (!file.exists(INPUT_08C_DEG_ALL)) {
  stop("找不到 08C all-filtered DEG table：", INPUT_08C_DEG_ALL)
}

deg <- fread(INPUT_08C_DEG_ALL, data.table = TRUE, showProgress = FALSE)

required_cols <- c("gene", "avg_log2FC", "p_val_adj")
missing_cols <- setdiff(required_cols, names(deg))

if (length(missing_cols) > 0) {
  stop("08C DEG table 缺少列：", paste(missing_cols, collapse = ", "))
}

if ("p_val" %in% names(deg)) {
  p_rank_col <- "p_val"
} else {
  p_rank_col <- "p_val_adj"
  warning("08C DEG table 没有 p_val 列；08E ranking 将使用 p_val_adj。")
}

deg[, gene := clean_gene_keep_length(gene)]
deg[, avg_log2FC := safe_num(avg_log2FC)]
deg[, p_val_adj := safe_num(p_val_adj)]
deg[, p_for_rank := safe_num(get(p_rank_col))]

deg <- deg[
  !is.na(gene) &
    gene != "" &
    !is.na(avg_log2FC) &
    is.finite(avg_log2FC) &
    !is.na(p_for_rank) &
    is.finite(p_for_rank)
]

if (nrow(deg) == 0) {
  stop("08C DEG table 过滤后没有有效 genes。")
}

if (file.exists(INPUT_08C_CHUNK_AUDIT)) {
  chunk_audit <- fread(INPUT_08C_CHUNK_AUDIT, data.table = TRUE)
  chunks_failed <- sum(grepl("^failed", chunk_audit$status), na.rm = TRUE)

  if (isTRUE(REQUIRE_08C_CHUNKS_FAILED_ZERO) && chunks_failed > 0) {
    stop(
      "08C chunk audit 显示有 failed chunks：", chunks_failed,
      "。期刊标准下不继续 08E。请先修复 08C。"
    )
  }
} else {
  chunks_failed <- NA_integer_
  warning("没有找到 08C chunk audit；08E 会继续，但 final 解释时需要人工确认 08C 完整性。")
}

dataset_name <- if ("dataset" %in% names(deg)) unique(deg$dataset)[1] else "Dataset"
contrast_name <- if ("contrast_name" %in% names(deg)) unique(deg$contrast_name)[1] else "contrast"

stamp("Dataset：", dataset_name)
stamp("Contrast：", contrast_name)
stamp("All tested filtered DEG rows after basic cleaning：", nrow(deg))
stamp("08C chunks failed：", chunks_failed)
stamp("Ranking p-value column：", p_rank_col)

stamp("构建 Hallmark GSEA ranked gene list。")

deg[p_for_rank <= 0 | is.na(p_for_rank), p_for_rank := .Machine$double.xmin]

deg[, signed_rank_metric_raw := sign(avg_log2FC) * (-log10(p_for_rank))]

deg[!is.finite(signed_rank_metric_raw), signed_rank_metric_raw := 0]

duplicate_audit <- deg[, .N, by = gene][N > 1][order(-N, gene)]

deg[, abs_signed_rank_metric := abs(signed_rank_metric_raw)]
deg[, abs_avg_log2FC_for_order := abs(avg_log2FC)]

setorder(
  deg,
  gene,
  -abs_signed_rank_metric,
  p_for_rank,
  -abs_avg_log2FC_for_order
)

rank_dt <- deg[, .SD[1], by = gene]

rank_dt[, tie_breaker := rank(avg_log2FC, ties.method = "first") * 1e-12]
rank_dt[, signed_rank_metric := signed_rank_metric_raw + tie_breaker]

rank_dt <- rank_dt[
  !is.na(gene) &
    gene != "" &
    is.finite(signed_rank_metric)
]

setorder(rank_dt, -signed_rank_metric)

gene_list <- rank_dt$signed_rank_metric
names(gene_list) <- rank_dt$gene

if (any(duplicated(names(gene_list)))) {
  stop("Ranked gene list 仍然存在重复 gene names。")
}

if (length(gene_list) < 100) {
  stop("Ranked gene list 过短，不适合 Hallmark GSEA：", length(gene_list))
}

ranked_gene_out <- rank_dt[, .(
  gene,
  avg_log2FC,
  p_for_rank,
  p_val_adj,
  signed_rank_metric_raw,
  signed_rank_metric,
  rank_position = seq_len(.N)
)]

atomic_write_csv(as.data.frame(ranked_gene_out), ranked_gene_table_csv)
atomic_write_csv(as.data.frame(duplicate_audit), ranked_gene_duplicate_audit_csv)

stamp("Ranked genes used for GSEA：", length(gene_list))
stamp("Duplicate gene symbols resolved：", nrow(duplicate_audit))

stamp("读取 MSigDB Hallmark gene sets。")

get_hallmark_msigdb <- function() {
  out <- tryCatch({
    suppressWarnings(msigdbr::msigdbr(species = SPECIES_FOR_MSIGDB, collection = "H"))
  }, error = function(e1) {
    tryCatch({
      suppressWarnings(msigdbr::msigdbr(species = SPECIES_FOR_MSIGDB, category = "H"))
    }, error = function(e2) {
      NULL
    })
  })

  out
}

msig_h <- get_hallmark_msigdb()

if (is.null(msig_h) || nrow(msig_h) == 0) {
  stop("无法读取 MSigDB Hallmark H collection。")
}

if (!"gs_name" %in% names(msig_h) || !"gene_symbol" %in% names(msig_h)) {
  stop("MSigDB Hallmark 数据缺少 gs_name 或 gene_symbol 列。")
}

hallmark_term2gene <- as.data.table(msig_h)[, .(
  term = as.character(gs_name),
  gene = as.character(gene_symbol)
)]

hallmark_term2gene <- unique(hallmark_term2gene[
  !is.na(term) &
    term != "" &
    !is.na(gene) &
    gene != ""
])

hallmark_term2gene <- hallmark_term2gene[gene %in% names(gene_list)]

geneset_summary <- hallmark_term2gene[, .(
  genes_in_ranked_list = uniqueN(gene)
), by = term][order(term)]

geneset_summary[, pass_size_filter := genes_in_ranked_list >= MIN_GS_SIZE & genes_in_ranked_list <= MAX_GS_SIZE]

atomic_write_csv(as.data.frame(hallmark_term2gene), hallmark_term2gene_csv)
atomic_write_csv(as.data.frame(geneset_summary), hallmark_geneset_summary_csv)

stamp("Hallmark terms total in ranked list：", uniqueN(hallmark_term2gene$term))
stamp("Hallmark TERM2GENE rows in ranked list：", nrow(hallmark_term2gene))
stamp("Hallmark terms passing size filter：", nrow(geneset_summary[pass_size_filter == TRUE]))

if (nrow(geneset_summary[pass_size_filter == TRUE]) < 5) {
  stop("通过 size filter 的 Hallmark gene sets 太少，不适合 GSEA。")
}

stamp("运行 fgsea Hallmark GSEA serial。")

pathway_list <- split(hallmark_term2gene$gene, hallmark_term2gene$term)
pathway_list <- lapply(pathway_list, unique)

pathway_sizes <- vapply(pathway_list, length, numeric(1))
pathway_list <- pathway_list[
  pathway_sizes >= MIN_GS_SIZE &
    pathway_sizes <= MAX_GS_SIZE
]

if (length(pathway_list) < 5) {
  stop("通过 size filter 的 Hallmark pathways 太少，不适合 fgsea。")
}

gene_list <- sort(gene_list, decreasing = TRUE)

fgsea_res <- tryCatch({
  fgsea::fgsea(
    pathways = pathway_list,
    stats = gene_list,
    minSize = MIN_GS_SIZE,
    maxSize = MAX_GS_SIZE,
    eps = 0,
    gseaParam = 1,
    BPPARAM = BiocParallel::SerialParam()
  )
}, error = function(e1) {

  warning("fgsea with BPPARAM failed; retrying with nproc = 0. Error: ", conditionMessage(e1))

  tryCatch({
    fgsea::fgsea(
      pathways = pathway_list,
      stats = gene_list,
      minSize = MIN_GS_SIZE,
      maxSize = MAX_GS_SIZE,
      eps = 0,
      gseaParam = 1,
      nproc = 0
    )
  }, error = function(e2) {
    warning("fgsea with nproc = 0 failed; retrying with nproc = 1. Error: ", conditionMessage(e2))

    fgsea::fgsea(
      pathways = pathway_list,
      stats = gene_list,
      minSize = MIN_GS_SIZE,
      maxSize = MAX_GS_SIZE,
      eps = 0,
      gseaParam = 1,
      nproc = 1
    )
  })
})

gsea_all <- as.data.table(fgsea_res)

if (nrow(gsea_all) == 0) {
  stop("Hallmark fgsea 没有返回任何结果。")
}

setnames(
  gsea_all,
  old = intersect(c("pathway", "pval", "padj", "ES", "size"), names(gsea_all)),
  new = c(
    pathway = "ID",
    pval = "pvalue",
    padj = "p.adjust",
    ES = "enrichmentScore",
    size = "setSize"
  )[intersect(c("pathway", "pval", "padj", "ES", "size"), names(gsea_all))]
)

if (!"ID" %in% names(gsea_all)) {
  stop("fgsea 结果缺少 pathway/ID 列。")
}

gsea_all[, Description := ID]

if ("leadingEdge" %in% names(gsea_all)) {
  gsea_all[, leadingEdge_genes := vapply(
    leadingEdge,
    function(x) paste(as.character(x), collapse = "/"),
    character(1)
  )]
  gsea_all[, leadingEdge := NULL]
}

for (cc in c("NES", "enrichmentScore", "pvalue", "p.adjust", "setSize")) {
  if (cc %in% names(gsea_all)) {
    gsea_all[, (cc) := safe_num(get(cc))]
  }
}

if (!"p.adjust" %in% names(gsea_all)) {
  stop("fgsea 结果缺少 p.adjust / padj 列。")
}

if (!"NES" %in% names(gsea_all)) {
  stop("fgsea 结果缺少 NES 列。")
}

gsea_all[, qvalue := p.adjust]
gsea_all[, Description_clean := shorten_term(clean_hallmark_name(Description), max_chars = 70)]
gsea_all[, NES_direction := ifelse(NES >= 0, "positive_NES_ideal_like_enriched", "negative_NES_lower_priority_enriched")]
gsea_all[, abs_NES := abs(NES)]
gsea_all[, neg_log10_padj := -log10(pmax(p.adjust, .Machine$double.xmin))]
gsea_all[, dataset := dataset_name]
gsea_all[, contrast := contrast_name]
gsea_all[, ranked_gene_n := length(gene_list)]
gsea_all[, hallmark_source := "MSigDB_H_collection"]
gsea_all[, gsea_engine := "fgsea_serial"]
gsea_all[, ranking_metric := paste0("sign(avg_log2FC) * -log10(", p_rank_col, ")")]

setorder(gsea_all, p.adjust, -abs_NES)

gsea_sig <- gsea_all[!is.na(p.adjust) & p.adjust <= PLOT_PADJ_CUTOFF]

atomic_write_csv(as.data.frame(gsea_all), gsea_all_results_csv)
atomic_write_csv(as.data.frame(gsea_sig), gsea_significant_results_csv)

stamp("Hallmark GSEA results all：", nrow(gsea_all))
stamp("Hallmark GSEA significant FDR <= ", PLOT_PADJ_CUTOFF, "：", nrow(gsea_sig))

if (nrow(gsea_sig) == 0) {
  stop(
    "没有 FDR <= ", PLOT_PADJ_CUTOFF,
    " 的 Hallmark GSEA 结果。正式图不使用 non-significant fallback。"
  )
}

plot_pos <- gsea_sig[NES > 0][order(p.adjust, -NES)]
plot_neg <- gsea_sig[NES < 0][order(p.adjust, NES)]

plot_pos <- head(plot_pos, PLOT_TOP_N_POSITIVE_NES)
plot_neg <- head(plot_neg, PLOT_TOP_N_NEGATIVE_NES)

plot_dt <- rbindlist(list(plot_neg, plot_pos), fill = TRUE)

if (nrow(plot_dt) == 0) {
  stop("Hallmark GSEA plot table 为空。")
}

plot_dt[, plot_label := Description_clean]
plot_dt[, plot_order_metric := NES]
setorder(plot_dt, plot_order_metric)
plot_dt[, plot_label_factor := factor(plot_label, levels = unique(plot_label))]

direction_summary <- gsea_all[, .(
  n_terms = .N,
  n_significant_FDR_0_05 = sum(p.adjust <= PLOT_PADJ_CUTOFF, na.rm = TRUE),
  top_term_by_padj = Description[which.min(p.adjust)],
  top_NES = max(NES, na.rm = TRUE),
  bottom_NES = min(NES, na.rm = TRUE)
), by = NES_direction]

atomic_write_csv(as.data.frame(plot_dt), gsea_plot_table_csv)
atomic_write_csv(as.data.frame(direction_summary), gsea_direction_summary_csv)

stamp("生成 Hallmark GSEA PDF 图。")

theme_gsea <- theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "plain", color = "black", size = 14),
    axis.text = element_text(color = "black", size = 11),
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 25)
  )

x_max_abs <- max(abs(plot_dt$NES), na.rm = TRUE)
x_lim <- c(-x_max_abs * 1.18, x_max_abs * 1.18)

gsea_dotplot <- ggplot(plot_dt, aes(x = NES, y = plot_label_factor)) +
  geom_vline(xintercept = 0, linewidth = 0.55, color = "grey35") +
  geom_point(aes(size = setSize, color = p.adjust), alpha = 0.95) +
  scale_color_gradient(low = "red", high = "blue", name = "FDR") +
  scale_size_continuous(name = "Set size", range = c(3.2, 8.8)) +
  scale_x_continuous(limits = x_lim) +
  labs(
    title = "Hallmark GSEA: ideal-like vs lower-priority",
    x = "Normalized enrichment score (NES)",
    y = "Hallmark gene set"
  ) +
  annotate(
    "text",
    x = x_lim[2] * 0.98,
    y = nrow(plot_dt) + 0.65,
    label = "ideal-like enriched",
    hjust = 1,
    size = 4.0
  ) +
  annotate(
    "text",
    x = x_lim[1] * 0.98,
    y = nrow(plot_dt) + 0.65,
    label = "lower-priority enriched",
    hjust = 0,
    size = 4.0
  ) +
  theme_gsea

gsea_barplot <- ggplot(plot_dt, aes(x = NES, y = plot_label_factor)) +
  geom_vline(xintercept = 0, linewidth = 0.55, color = "grey35") +
  geom_col(aes(fill = NES_direction), width = 0.72, alpha = 0.90) +
  scale_fill_manual(
    values = c(
      positive_NES_ideal_like_enriched = "red",
      negative_NES_lower_priority_enriched = "darkgreen"
    ),
    name = "Direction",
    labels = c(
      positive_NES_ideal_like_enriched = "ideal-like enriched",
      negative_NES_lower_priority_enriched = "lower-priority enriched"
    )
  ) +
  scale_x_continuous(limits = x_lim) +
  labs(
    title = "Hallmark GSEA NES",
    x = "Normalized enrichment score (NES)",
    y = "Hallmark gene set"
  ) +
  theme_gsea

save_pdf_plot(gsea_dotplot, gsea_dotplot_pdf, width = PDF_WIDTH, height = PDF_HEIGHT)
save_pdf_plot(gsea_barplot, gsea_barplot_pdf, width = PDF_WIDTH, height = PDF_HEIGHT)

figure_index <- data.table(
  figure_id = c("Hallmark_GSEA_NES_dotplot", "Hallmark_GSEA_NES_barplot"),
  title = c(
    "Hallmark GSEA NES dotplot",
    "Hallmark GSEA NES barplot"
  ),
  pdf_path = c(gsea_dotplot_pdf, gsea_barplot_pdf),
  table_path = c(gsea_plot_table_csv, gsea_plot_table_csv),
  pdf_size_bytes = c(file.info(gsea_dotplot_pdf)$size, file.info(gsea_barplot_pdf)$size),
  plot_type = c("GSEA_NES_dotplot", "GSEA_NES_barplot"),
  plot_engine = "ggplot2"
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

method_lines <- c(
  "08E Hallmark GSEA FINAL V4 method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "Hallmark gene set enrichment analysis was performed using fgsea with a serial BiocParallel backend and MSigDB Hallmark gene sets obtained through the msigdbr R package. ",
    "The ranked gene list included all genes tested in the 08C differential expression analysis after objective expression filtering. ",
    "Genes were ranked by sign(avg_log2FC) multiplied by -log10(", p_rank_col, "). ",
    "Duplicate gene symbols were resolved by retaining the record with the largest absolute ranking metric. ",
    "The enrichment universe was therefore not restricted to significant DEGs. ",
    "GSEA results were adjusted using the Benjamini-Hochberg method, and final visualization included Hallmark terms with FDR <= ", PLOT_PADJ_CUTOFF, "."
  ),
  "",
  "Strict parameters:",
  paste0("Ranked genes: ", length(gene_list)),
  paste0("Ranking metric: sign(avg_log2FC) * -log10(", p_rank_col, ")"),
  paste0("GSEA method: fgsea serial backend; pAdjustMethod = ", GSEA_PADJUST_METHOD),
  paste0("minGSSize = ", MIN_GS_SIZE, "; maxGSSize = ", MAX_GS_SIZE),
  paste0("Plot threshold: FDR <= ", PLOT_PADJ_CUTOFF),
  "",
  "Claim boundary:",
  "08E inherits the claim boundary of 08C. If 08C is a single-object cell-state contrast, then 08E supports ranked-gene pathway-level interpretation of that contrast, not sample-level pseudo-bulk validation, functional proof, treatment efficacy, or clinical safety."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "08E Hallmark GSEA FINAL V4 report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  INPUT_08C_DEG_ALL,
  "",
  "Dataset / contrast:",
  paste0("Dataset: ", dataset_name),
  paste0("Contrast: ", contrast_name),
  "",
  "Ranked gene list:",
  paste0("Ranking p-value column: ", p_rank_col),
  paste0("Ranked genes: ", length(gene_list)),
  paste0("Duplicate gene symbols resolved: ", nrow(duplicate_audit)),
  "",
  "Hallmark gene sets:",
  paste0("Hallmark terms in ranked list: ", uniqueN(hallmark_term2gene$term)),
  paste0("Hallmark TERM2GENE rows in ranked list: ", nrow(hallmark_term2gene)),
  paste0("Hallmark terms passing size filter: ", nrow(geneset_summary[pass_size_filter == TRUE])),
  "",
  "GSEA results:",
  paste0("All Hallmark GSEA rows: ", nrow(gsea_all)),
  paste0("Significant Hallmark GSEA rows FDR <= ", PLOT_PADJ_CUTOFF, ": ", nrow(gsea_sig)),
  "",
  "Figures:",
  capture.output(print(figure_index)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)

required_output_files <- c(
  ranked_gene_table_csv,
  ranked_gene_duplicate_audit_csv,
  hallmark_term2gene_csv,
  hallmark_geneset_summary_csv,
  gsea_all_results_csv,
  gsea_significant_results_csv,
  gsea_plot_table_csv,
  gsea_direction_summary_csv,
  figure_index_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  gsea_dotplot_pdf,
  gsea_barplot_pdf
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(file.exists(required_output_files), file.info(required_output_files)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

missing_required <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]

if (nrow(missing_required) > 0) {
  print(missing_required)
  stop("08E Hallmark GSEA FINAL V4 未通过输出验证。")
}

cat("\n============================================================\n")
cat("08E Hallmark GSEA FINAL V4 运行结束\n")
cat("============================================================\n\n")

cat("Dataset：", dataset_name, "\n")
cat("Contrast：", contrast_name, "\n")
cat("Ranked genes：", length(gene_list), "\n")
cat("Ranking p-value column：", p_rank_col, "\n")
cat("Hallmark terms in ranked list：", uniqueN(hallmark_term2gene$term), "\n")
cat("All Hallmark GSEA rows：", nrow(gsea_all), "\n")
cat("Significant Hallmark GSEA rows FDR <= ", PLOT_PADJ_CUTOFF, "：", nrow(gsea_sig), "\n\n", sep = "")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要 PDF 图：\n")
cat(gsea_dotplot_pdf, "\n")
cat(gsea_barplot_pdf, "\n\n")

cat("关键文件：\n")
cat(ranked_gene_table_csv, "\n")
cat(gsea_all_results_csv, "\n")
cat(gsea_significant_results_csv, "\n")
cat(gsea_plot_table_csv, "\n")
cat(method_note_txt, "\n")
cat(report_txt, "\n\n")

cat("✅ 08E Hallmark GSEA FINAL V4 完成。\n")

