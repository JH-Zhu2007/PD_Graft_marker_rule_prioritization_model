
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
