
cat("\n[11G FINAL] Starting PD GWAS / genetic-context support...\n")
cat("[11G FINAL] Mode: complete standalone 11G rebuild; no previous 11G dependency; no internet; no 00-10P rerun.\n")
cat("[11G FINAL] Parser: robust full-script replacement; avoids locked-binding collision in optional gene harvesting.\n")
cat("[11G FINAL] Claim boundary: PD genetic-context support only; no clinical prediction or diagnostic biomarker claim.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "11G_PD_GWAS_genetic_context_support_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "11G_PD_GWAS_genetic_context_support_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "11G_PD_GWAS_genetic_context_support_FINAL_COMPLETE_STANDALONE"
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
  out <- suppressWarnings(as.numeric(value_obj))
  out
}

clean_gene_symbol <- function(value_obj) {
  out <- toupper(trimws(safe_chr(value_obj)))
  out <- gsub("[^A-Z0-9.-]", "", out)
  out[out %in% c("", "NA", "NAN", "NULL", "NONE")] <- ""
  out
}

first_existing <- function(file_values) {
  file_values <- safe_chr(file_values)
  hit <- file_values[file.exists(file_values)]
  if (length(hit) < 1) return("")
  hit[1]
}

write_csv_safe <- function(df_value, file_value) {
  utils::write.csv(df_value, file_value, row.names = FALSE, na = "")
  cat("[11G FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(df_value, file_value) {
  utils::write.table(df_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[11G FINAL] Wrote:", file_value, "\n")
}

read_table_safe <- function(file_value) {
  if (!file.exists(file_value)) return(data.frame(stringsAsFactors = FALSE))
  ext_value <- tolower(tools::file_ext(file_value))
  out <- data.frame(stringsAsFactors = FALSE)
  try({
    if (ext_value %in% c("tsv", "txt")) {
      out <- utils::read.table(file_value, sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, quote = "", comment.char = "")
    } else {
      out <- utils::read.csv(file_value, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, silent = TRUE)
  if (!is.data.frame(out)) out <- data.frame(stringsAsFactors = FALSE)
  out
}

safe_bind_rows <- function(list_value) {
  list_value <- list_value[vapply(list_value, is.data.frame, logical(1))]
  list_value <- list_value[vapply(list_value, nrow, integer(1)) > 0]
  if (length(list_value) < 1) return(data.frame(stringsAsFactors = FALSE))
  all_cols <- unique(unlist(lapply(list_value, colnames), use.names = FALSE))
  list_fixed <- lapply(list_value, function(df_value) {
    miss_cols <- setdiff(all_cols, colnames(df_value))
    if (length(miss_cols) > 0) {
      for (col_value in miss_cols) df_value[[col_value]] <- NA
    }
    df_value[, all_cols, drop = FALSE]
  })
  do.call(base::rbind, list_fixed)
}

split_gene_cell <- function(value_obj) {

  value_chr <- safe_chr(value_obj)
  if (length(value_chr) < 1) return(character(0))
  gene_part_vec <- character(0)
  for (one_value in value_chr) {
    one_value <- safe_chr(one_value)
    if (length(one_value) < 1 || nchar(one_value[1]) < 1) next
    split_vec <- unlist(strsplit(one_value[1], "[;,/| ]+"), use.names = FALSE)
    if (length(split_vec) > 0) gene_part_vec <- c(gene_part_vec, split_vec)
  }
  gene_part_vec <- clean_gene_symbol(gene_part_vec)
  unique(gene_part_vec[gene_part_vec != ""])
}

split_gene_vector_safe <- function(value_obj) {
  value_chr <- safe_chr(value_obj)
  if (length(value_chr) < 1) return(character(0))
  out_gene_vec <- character(0)
  for (idx_value in seq_along(value_chr)) {
    tmp_gene_vec <- character(0)
    try({ tmp_gene_vec <- split_gene_cell(value_chr[idx_value]) }, silent = TRUE)
    if (length(tmp_gene_vec) > 0) out_gene_vec <- c(out_gene_vec, tmp_gene_vec)
  }
  out_gene_vec <- clean_gene_symbol(out_gene_vec)
  unique(out_gene_vec[out_gene_vec != ""])
}

open_pdf_safe <- function(filename, width_value = 10, height_value = 6) {
  file_primary <- file.path(out_fig_dir, filename)
  if (file.exists(file_primary)) {
    suppressWarnings(try(file.remove(file_primary), silent = TRUE))
  }
  ok <- tryCatch({
    grDevices::pdf(file_primary, width = width_value, height = height_value, onefile = FALSE, useDingbats = FALSE, paper = "special")
    TRUE
  }, error = function(err_obj) {
    FALSE
  })
  if (!ok) {
    alt_name <- paste0(sub("\\.pdf$", "", filename), "_ALT_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    file_alt <- file.path(out_fig_dir, alt_name)
    grDevices::pdf(file_alt, width = width_value, height = height_value, onefile = FALSE, useDingbats = FALSE, paper = "special")
    cat("[11G FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 1.05, font = 2, adj = c(0.5, 0.5))
  if (nchar(subtitle_value) > 0) text(0.5, 0.925, subtitle_value, cex = 0.52, col = "gray35", adj = c(0.5, 0.5))
}

value_to_gray <- function(value_obj, max_obj) {
  value_num <- safe_num(value_obj)
  max_num <- max(safe_num(max_obj), na.rm = TRUE)
  if (!is.finite(max_num) || max_num <= 0) max_num <- 1
  frac <- value_num / max_num
  frac[!is.finite(frac)] <- 0
  frac[frac < 0] <- 0
  frac[frac > 1] <- 1
  grays <- gray(0.92 - 0.55 * frac)
  grays
}

make_pd_catalog <- function() {
  monogenic <- c(
    "SNCA", "LRRK2", "GBA1", "PRKN", "PINK1", "PARK7", "VPS35",
    "ATP13A2", "PLA2G6", "FBXO7", "DNAJC6", "SYNJ1", "VPS13C",
    "RAB39B", "GCH1", "DNAJC13", "HTRA2", "UCHL1", "DCTN1"
  )
  gwas_core <- c(
    "SNCA", "MAPT", "LRRK2", "GBA1", "BST1", "TMEM175", "GPNMB",
    "GAK", "DGKQ", "HLA-DRA", "HLA-DRB1", "MCCC1", "NUCKS1",
    "RIT2", "STK39", "SIPA1L2", "INPP5F", "BAG3", "SCARB2",
    "GCH1", "SYT11", "RAB25", "ACMSD", "STX1B", "SATB1",
    "FAM47E", "DLG2", "KCNS3", "SREBF1", "DYRK1A", "ITGA8",
    "MBNL2", "SPPL2B", "GALC", "HIP1R", "CCDC62", "FGF20",
    "STBD1", "TMEM163", "CTSB", "KAT8", "NSF", "RAB29", "MMRN1",
    "CRHR1", "WNT3", "KANSL1", "CRHR1", "SETD1A", "FYN", "CAMK2D",
    "MMP16", "BCKDK", "CAMK2D", "SH3GL2", "RAB7L1"
  )
  lysosomal_mito <- c(
    "GBA1", "SCARB2", "TMEM175", "CTSB", "GALC", "ATP13A2", "PINK1",
    "PRKN", "PARK7", "VPS35", "DNAJC6", "SYNJ1", "PLA2G6", "VPS13C"
  )
  synaptic_neuronal <- c(
    "SNCA", "SYT11", "STX1B", "SYNJ1", "DNAJC6", "VPS35", "RIT2",
    "DLG2", "SH3GL2", "FYN", "CAMK2D", "KCNS3", "FGF20"
  )
  immune_glial <- c(
    "HLA-DRA", "HLA-DRB1", "GPNMB", "BST1", "CTSB", "MMP16"
  )

  df_list <- list(
    data.frame(gene_symbol = monogenic, evidence_class = "monogenic_or_high_confidence_PD_gene", evidence_weight = 3, stringsAsFactors = FALSE),
    data.frame(gene_symbol = gwas_core, evidence_class = "PD_GWAS_locus_nominated_gene", evidence_weight = 2, stringsAsFactors = FALSE),
    data.frame(gene_symbol = lysosomal_mito, evidence_class = "PD_lysosomal_mitochondrial_context_gene", evidence_weight = 1.5, stringsAsFactors = FALSE),
    data.frame(gene_symbol = synaptic_neuronal, evidence_class = "PD_synaptic_neuronal_context_gene", evidence_weight = 1.5, stringsAsFactors = FALSE),
    data.frame(gene_symbol = immune_glial, evidence_class = "PD_immune_glial_context_gene", evidence_weight = 1, stringsAsFactors = FALSE)
  )
  raw_df <- safe_bind_rows(df_list)
  raw_df$gene_symbol <- clean_gene_symbol(raw_df$gene_symbol)
  raw_df <- raw_df[raw_df$gene_symbol != "", , drop = FALSE]

  genes <- sort(unique(raw_df$gene_symbol))
  out <- data.frame(
    gene_symbol = genes,
    pd_genetic_evidence_classes = "",
    max_pd_genetic_weight = 0,
    n_pd_evidence_classes = 0,
    stringsAsFactors = FALSE
  )
  for (ii in seq_len(nrow(out))) {
    sub_df <- raw_df[raw_df$gene_symbol == out$gene_symbol[ii], , drop = FALSE]
    out$pd_genetic_evidence_classes[ii] <- paste(sort(unique(sub_df$evidence_class)), collapse = ";")
    out$max_pd_genetic_weight[ii] <- max(safe_num(sub_df$evidence_weight), na.rm = TRUE)
    out$n_pd_evidence_classes[ii] <- length(unique(sub_df$evidence_class))
  }
  out$genetic_context_tier <- ifelse(out$max_pd_genetic_weight >= 3, "high_confidence_PD_gene",
                                     ifelse(out$max_pd_genetic_weight >= 2, "GWAS_locus_gene",
                                            "context_support_gene"))
  out
}

pd_catalog <- make_pd_catalog()
write_csv_safe(pd_catalog, file.path(out_table_dir, "11G_FINAL_PD_genetic_context_seed_catalog.csv"))

module_gene_list <- list(
  DA_core = c("TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "FOXA2", "LMX1A", "LMX1B", "EN1", "PITX3", "ALDH1A1"),
  A9_like = c("SOX6", "ALDH1A1", "KCNJ6", "DCC", "GIRK2", "VGF", "SLC10A4", "DAB1", "LMO3"),
  A10_like = c("CALB1", "OTX2", "SOX6", "SLC17A6", "VIP", "NTS", "GRP", "CRHBP"),
  projection_competence = c("DCC", "ROBO1", "ROBO2", "SLIT1", "SLIT2", "NTN1", "SEMA3A", "SEMA3C", "PLXNA4", "EPHA4", "EPHB1", "EFNA5", "NCAM1", "L1CAM"),
  axon_guidance = c("GAP43", "NEFL", "NEFM", "NEFH", "CNTN2", "DCX", "TUBB3", "MAP1B", "STMN2", "DPYSL2", "DPYSL3"),
  synaptic_maturation = c("SYN1", "SYP", "SNAP25", "STX1A", "VAMP2", "DLG4", "SHANK2", "SYT1", "RIMS1", "UNC13A"),
  neuronal_maturation = c("MAP2", "RBFOX3", "TUBB3", "DCX", "STMN2", "NEFL", "NEFM", "CAMK2D", "GAP43"),
  proliferation_risk = c("MKI67", "TOP2A", "HMGB2", "CCNB1", "CCNB2", "CDK1", "PCNA", "MCM2", "MCM5", "TYMS"),
  stress_p53_apoptosis = c("TP53", "BBC3", "BAX", "BCL2L11", "CASP2", "CASP3", "CASP9", "DDIT3", "FOS", "JUN", "HSPA1A", "HSP90AA1"),
  inflammatory_NFkB = c("NFKB1", "RELA", "TNF", "IL1B", "IL6", "CXCL8", "CCL2", "TLR4", "IRF1", "STAT1")
)

module_df_list <- list()
for (module_name in names(module_gene_list)) {
  module_df_list[[length(module_df_list) + 1]] <- data.frame(
    source_type = "built_in_project_signature",
    source_name = module_name,
    gene_symbol = clean_gene_symbol(module_gene_list[[module_name]]),
    signature_direction = ifelse(grepl("risk|stress|inflammatory|proliferation", module_name, ignore.case = TRUE), "risk_associated", "favorable_or_identity_associated"),
    stringsAsFactors = FALSE
  )
}
builtin_marker_df <- safe_bind_rows(module_df_list)
builtin_marker_df <- builtin_marker_df[builtin_marker_df$gene_symbol != "", , drop = FALSE]
write_csv_safe(builtin_marker_df, file.path(out_table_dir, "11G_FINAL_builtin_candidate_marker_signature_genes.csv"))

harvest_candidate_files <- function(root_value) {
  table_root <- file.path(root_value, "03_tables")
  if (!dir.exists(table_root)) return(character(0))
  all_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
  if (length(all_files) < 1) return(character(0))
  base_lower <- tolower(basename(all_files))
  path_lower <- tolower(all_files)

  not_11g <- !grepl("11g_pd_gwas|11g_", path_lower)
  keep <- grepl("feature|importance|marker|module|crispr|crisper|proxy|gene_presence|11c|11d|11f|09c|10k", base_lower) |
    grepl("11c|11d|11f|09c|10k", path_lower)
  all_files <- all_files[keep & not_11g]
  if (length(all_files) < 1) return(character(0))
  file_info <- file.info(all_files)
  all_files <- all_files[is.finite(file_info$size) & file_info$size > 0 & file_info$size < 25 * 1024 * 1024]
  all_files <- sort(unique(all_files))
  if (length(all_files) > 250) all_files <- all_files[seq_len(250)]
  all_files
}

extract_gene_rows_from_table <- function(file_value) {

  out_empty <- data.frame(stringsAsFactors = FALSE)
  tryCatch({
    df_value <- read_table_safe(file_value)
    if (!is.data.frame(df_value) || nrow(df_value) < 1 || ncol(df_value) < 1) return(out_empty)
    col_values <- colnames(df_value)
    col_lower <- tolower(col_values)
    gene_cols <- col_values[grepl("^gene$|gene_symbol|symbol$|hgnc|feature$|features$|marker|genes$|gene_name|target_gene", col_lower)]
    if (length(gene_cols) < 1) return(out_empty)
    out_list <- list()
    for (col_name in gene_cols) {
      value_vec <- safe_chr(df_value[[col_name]])
      if (length(value_vec) > 5000) value_vec <- value_vec[seq_len(5000)]
      gene_vec <- character(0)

      for (idx in seq_along(value_vec)) {
        one_gene_vec <- tryCatch({
          split_gene_cell(value_vec[idx])
        }, error = function(e) {
          character(0)
        })
        if (length(one_gene_vec) > 0) gene_vec <- c(gene_vec, one_gene_vec)
      }
      gene_vec <- clean_gene_symbol(gene_vec)
      gene_vec <- unique(gene_vec[gene_vec != ""])
      if (length(gene_vec) > 0) {
        out_list[[length(out_list) + 1]] <- data.frame(
          source_type = "harvested_upstream_table",
          source_name = basename(file_value),
          source_column = col_name,
          gene_symbol = gene_vec,
          stringsAsFactors = FALSE
        )
      }
    }
    safe_bind_rows(out_list)
  }, error = function(err_obj) {
    cat("[11G FINAL] WARNING: skipped upstream table due to parser error: ", basename(file_value), "\n", sep = "")
    out_empty
  })
}

candidate_files <- harvest_candidate_files(project_root)
file_inventory_df <- data.frame(file_path = candidate_files, stringsAsFactors = FALSE)
write_csv_safe(file_inventory_df, file.path(out_table_dir, "11G_FINAL_optional_upstream_gene_file_inventory.csv"))

harvest_list <- list()
if (length(candidate_files) > 0) {
  for (ii in seq_along(candidate_files)) {
    if (ii %% 25 == 0) cat("[11G FINAL] Harvesting optional upstream gene tables ", ii, "/", length(candidate_files), "\n", sep = "")
    tmp_df <- tryCatch({
      extract_gene_rows_from_table(candidate_files[ii])
    }, error = function(err_obj) {
      cat("[11G FINAL] WARNING: skipped file ", ii, " due to error: ", basename(candidate_files[ii]), "\n", sep = "")
      data.frame(stringsAsFactors = FALSE)
    })
    if (is.data.frame(tmp_df) && nrow(tmp_df) > 0) harvest_list[[length(harvest_list) + 1]] <- tmp_df
  }
}

harvested_df <- safe_bind_rows(harvest_list)
if (nrow(harvested_df) < 1) {
  harvested_df <- data.frame(
    source_type = character(0),
    source_name = character(0),
    source_column = character(0),
    gene_symbol = character(0),
    stringsAsFactors = FALSE
  )
}
harvested_df$gene_symbol <- clean_gene_symbol(harvested_df$gene_symbol)
harvested_df <- harvested_df[harvested_df$gene_symbol != "", , drop = FALSE]
write_csv_safe(harvested_df, file.path(out_table_dir, "11G_FINAL_harvested_upstream_candidate_genes.csv"))

load_user_gwas_genes <- function(root_value) {
  dirs <- c(
    file.path(root_value, "03_tables", "11G_user_PD_GWAS_inputs"),
    file.path(root_value, "00_data", "11G_user_PD_GWAS_inputs"),
    file.path(root_value, "00_raw", "11G_user_PD_GWAS_inputs")
  )
  files <- character(0)
  for (dir_value in dirs) {
    if (dir.exists(dir_value)) {
      files <- c(files, list.files(dir_value, pattern = "\\.(csv|tsv|txt)$", full.names = TRUE, recursive = TRUE))
    }
  }
  files <- unique(files)
  out_list <- list()
  if (length(files) > 0) {
    for (file_value in files) {
      df_value <- read_table_safe(file_value)
      if (nrow(df_value) < 1) next
      col_values <- colnames(df_value)
      gene_cols <- col_values[grepl("gene|symbol|mapped|reported|nearest|hgnc", tolower(col_values))]
      if (length(gene_cols) < 1) next
      for (col_name in gene_cols) {
        genes <- split_gene_vector_safe(safe_chr(df_value[[col_name]]))
        genes <- clean_gene_symbol(genes)
        genes <- genes[genes != ""]
        if (length(genes) > 0) {
          out_list[[length(out_list) + 1]] <- data.frame(
            gene_symbol = genes,
            evidence_class = "user_supplied_PD_GWAS_gene",
            evidence_weight = 2.5,
            source_file = basename(file_value),
            source_column = col_name,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  safe_bind_rows(out_list)
}

user_gwas_df <- load_user_gwas_genes(project_root)
if (nrow(user_gwas_df) > 0) {
  user_gwas_df$gene_symbol <- clean_gene_symbol(user_gwas_df$gene_symbol)
  user_gwas_df <- user_gwas_df[user_gwas_df$gene_symbol != "", , drop = FALSE]
  write_csv_safe(user_gwas_df, file.path(out_table_dir, "11G_FINAL_user_supplied_PD_GWAS_genes_loaded.csv"))

  user_genes <- sort(unique(user_gwas_df$gene_symbol))
  add_rows <- data.frame(
    gene_symbol = user_genes,
    pd_genetic_evidence_classes = "user_supplied_PD_GWAS_gene",
    max_pd_genetic_weight = 2.5,
    n_pd_evidence_classes = 1,
    genetic_context_tier = "user_supplied_GWAS_gene",
    stringsAsFactors = FALSE
  )
  pd_catalog <- safe_bind_rows(list(pd_catalog, add_rows))

  genes_all <- sort(unique(pd_catalog$gene_symbol))
  collapsed <- data.frame(
    gene_symbol = genes_all,
    pd_genetic_evidence_classes = "",
    max_pd_genetic_weight = 0,
    n_pd_evidence_classes = 0,
    genetic_context_tier = "",
    stringsAsFactors = FALSE
  )
  for (ii in seq_len(nrow(collapsed))) {
    sub_df <- pd_catalog[pd_catalog$gene_symbol == collapsed$gene_symbol[ii], , drop = FALSE]
    classes <- unique(unlist(strsplit(paste(safe_chr(sub_df$pd_genetic_evidence_classes), collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
    classes <- classes[classes != ""]
    collapsed$pd_genetic_evidence_classes[ii] <- paste(sort(classes), collapse = ";")
    collapsed$max_pd_genetic_weight[ii] <- max(safe_num(sub_df$max_pd_genetic_weight), na.rm = TRUE)
    collapsed$n_pd_evidence_classes[ii] <- length(classes)
    collapsed$genetic_context_tier[ii] <- ifelse(collapsed$max_pd_genetic_weight[ii] >= 3, "high_confidence_PD_gene",
                                                ifelse(collapsed$max_pd_genetic_weight[ii] >= 2.5, "user_supplied_GWAS_gene",
                                                       ifelse(collapsed$max_pd_genetic_weight[ii] >= 2, "GWAS_locus_gene", "context_support_gene")))
  }
  pd_catalog <- collapsed
} else {
  write_csv_safe(data.frame(note = "No optional user-supplied PD GWAS files detected.", stringsAsFactors = FALSE), file.path(out_table_dir, "11G_FINAL_user_supplied_PD_GWAS_genes_loaded.csv"))
}
write_csv_safe(pd_catalog, file.path(out_table_dir, "11G_FINAL_PD_genetic_context_catalog_with_optional_user_inputs.csv"))

harvested_marker_df <- data.frame(
  source_type = harvested_df$source_type,
  source_name = harvested_df$source_name,
  gene_symbol = harvested_df$gene_symbol,
  signature_direction = "upstream_project_gene",
  stringsAsFactors = FALSE
)
all_marker_raw <- safe_bind_rows(list(builtin_marker_df, harvested_marker_df))
all_marker_raw$gene_symbol <- clean_gene_symbol(all_marker_raw$gene_symbol)
all_marker_raw <- all_marker_raw[all_marker_raw$gene_symbol != "", , drop = FALSE]
write_csv_safe(all_marker_raw, file.path(out_table_dir, "11G_FINAL_all_candidate_genes_raw_sources.csv"))

gene_values <- sort(unique(all_marker_raw$gene_symbol))
master_gene_df <- data.frame(
  gene_symbol = gene_values,
  n_project_sources = 0,
  project_sources = "",
  project_signature_directions = "",
  in_PD_genetic_context_catalog = FALSE,
  pd_genetic_evidence_classes = "",
  max_pd_genetic_weight = 0,
  pd_genetic_context_tier = "none",
  stringsAsFactors = FALSE
)
for (ii in seq_len(nrow(master_gene_df))) {
  sub_marker <- all_marker_raw[all_marker_raw$gene_symbol == master_gene_df$gene_symbol[ii], , drop = FALSE]
  src <- sort(unique(paste(sub_marker$source_type, sub_marker$source_name, sep = ":")))
  dir_values <- sort(unique(safe_chr(sub_marker$signature_direction)))
  master_gene_df$n_project_sources[ii] <- length(src)
  master_gene_df$project_sources[ii] <- paste(src, collapse = ";")
  master_gene_df$project_signature_directions[ii] <- paste(dir_values, collapse = ";")
  pd_hit <- pd_catalog[pd_catalog$gene_symbol == master_gene_df$gene_symbol[ii], , drop = FALSE]
  if (nrow(pd_hit) > 0) {
    master_gene_df$in_PD_genetic_context_catalog[ii] <- TRUE
    master_gene_df$pd_genetic_evidence_classes[ii] <- pd_hit$pd_genetic_evidence_classes[1]
    master_gene_df$max_pd_genetic_weight[ii] <- safe_num(pd_hit$max_pd_genetic_weight[1])
    master_gene_df$pd_genetic_context_tier[ii] <- pd_hit$genetic_context_tier[1]
  }
}
master_gene_df <- master_gene_df[order(master_gene_df$in_PD_genetic_context_catalog, master_gene_df$max_pd_genetic_weight, master_gene_df$n_project_sources, decreasing = TRUE), , drop = FALSE]
write_csv_safe(master_gene_df, file.path(out_table_dir, "11G_FINAL_gene_level_PD_genetic_context_overlap_table.csv"))
write_tsv_safe(master_gene_df, file.path(out_table_dir, "11G_FINAL_gene_level_PD_genetic_context_overlap_table.tsv"))

module_rows <- list()
for (module_name in names(module_gene_list)) {
  genes <- clean_gene_symbol(module_gene_list[[module_name]])
  genes <- unique(genes[genes != ""])
  hit_genes <- intersect(genes, pd_catalog$gene_symbol)
  hit_df <- pd_catalog[pd_catalog$gene_symbol %in% hit_genes, , drop = FALSE]
  n_genes <- length(genes)
  n_hits <- length(hit_genes)

  universe_n <- 20000
  pd_n <- length(unique(pd_catalog$gene_symbol))
  p_value <- ifelse(n_hits > 0, stats::phyper(n_hits - 1, pd_n, universe_n - pd_n, n_genes, lower.tail = FALSE), 1)
  high_hit <- any(hit_df$max_pd_genetic_weight >= 3)
  support_tier <- ifelse(n_hits >= 3 || high_hit, "genetic_context_support_high",
                         ifelse(n_hits == 2, "genetic_context_support_intermediate",
                                ifelse(n_hits == 1, "genetic_context_support_low", "no_detected_PD_genetic_context_overlap")))
  module_rows[[length(module_rows) + 1]] <- data.frame(
    module_name = module_name,
    signature_direction = ifelse(grepl("risk|stress|inflammatory|proliferation", module_name, ignore.case = TRUE), "risk_associated", "favorable_or_identity_associated"),
    n_module_genes = n_genes,
    n_PD_genetic_context_genes = n_hits,
    overlap_ratio = ifelse(n_genes > 0, n_hits / n_genes, 0),
    PD_genetic_context_genes = paste(sort(hit_genes), collapse = ";"),
    max_PD_genetic_weight_in_module = ifelse(nrow(hit_df) > 0, max(safe_num(hit_df$max_pd_genetic_weight), na.rm = TRUE), 0),
    nominal_hypergeom_p = p_value,
    genetic_context_support_tier = support_tier,
    stringsAsFactors = FALSE
  )
}
module_support_df <- safe_bind_rows(module_rows)
module_support_df$BH_adjusted_p <- stats::p.adjust(safe_num(module_support_df$nominal_hypergeom_p), method = "BH")
module_support_df <- module_support_df[order(module_support_df$n_PD_genetic_context_genes, module_support_df$max_PD_genetic_weight_in_module, decreasing = TRUE), , drop = FALSE]
write_csv_safe(module_support_df, file.path(out_table_dir, "11G_FINAL_module_level_PD_genetic_context_support.csv"))

support_score <- rep(0, nrow(module_support_df))
support_score[module_support_df$genetic_context_support_tier == "genetic_context_support_low"] <- 1
support_score[module_support_df$genetic_context_support_tier == "genetic_context_support_intermediate"] <- 2
support_score[module_support_df$genetic_context_support_tier == "genetic_context_support_high"] <- 3
support_score <- support_score + pmin(module_support_df$max_PD_genetic_weight_in_module, 3) * 0.25

input_11h_df <- data.frame(
  evidence_layer = "11G_PD_genetic_context_support",
  feature_unit = "module_signature",
  module_name = module_support_df$module_name,
  signature_direction = module_support_df$signature_direction,
  n_module_genes = module_support_df$n_module_genes,
  n_PD_genetic_context_genes = module_support_df$n_PD_genetic_context_genes,
  PD_genetic_context_genes = module_support_df$PD_genetic_context_genes,
  genetic_context_support_tier = module_support_df$genetic_context_support_tier,
  genetic_context_support_score = support_score,
  nominal_hypergeom_p = module_support_df$nominal_hypergeom_p,
  BH_adjusted_p = module_support_df$BH_adjusted_p,
  allowed_interpretation = "PD genetic-context support for transcriptomic prioritisation only",
  prohibited_interpretation = "No clinical prediction, no PD diagnosis, no causal validation, no validated biomarker claim",
  stringsAsFactors = FALSE
)
write_csv_safe(input_11h_df, file.path(out_table_dir, "11G_FINAL_PD_genetic_context_support_table_for_11H.csv"))

marker_signature_df <- master_gene_df[master_gene_df$in_PD_genetic_context_catalog, , drop = FALSE]
marker_signature_df$marker_signature_role <- ifelse(grepl("risk", marker_signature_df$project_signature_directions, ignore.case = TRUE),
                                                     "risk_associated_candidate_marker_signature_gene",
                                                     "favorable_or_identity_candidate_marker_signature_gene")
marker_signature_df$claim_boundary <- "candidate transcriptomic marker signature only; not a clinical biomarker"
write_csv_safe(marker_signature_df, file.path(out_table_dir, "11G_FINAL_candidate_marker_signature_genes_with_PD_genetic_context.csv"))

fig_a <- open_pdf_safe("11G_FINAL_FigA_PD_genetic_context_catalog_summary.pdf", 10.8, 6.2)
new_canvas()
draw_title("PD genetic-context catalog for 11G", "Conservative seed catalog plus optional local GWAS files; used only as supportive genetic context.")
class_values <- unlist(strsplit(paste(pd_catalog$pd_genetic_evidence_classes, collapse = ";"), ";", fixed = TRUE), use.names = FALSE)
class_values <- class_values[class_values != ""]
class_tab <- sort(table(class_values), decreasing = TRUE)
if (length(class_tab) < 1) class_tab <- c(no_class = 0)
max_count <- max(as.numeric(class_tab), na.rm = TRUE)
y_pos <- seq(0.78, 0.20, length.out = length(class_tab))
bar_x0 <- 0.34
bar_x1 <- 0.90
for (ii in seq_along(class_tab)) {
  yy <- y_pos[ii]
  count_val <- as.numeric(class_tab[ii])
  width_val <- ifelse(max_count > 0, count_val / max_count, 0)
  text(bar_x0 - 0.02, yy, names(class_tab)[ii], cex = 0.55, adj = c(1, 0.5), col = "gray15")
  rect(bar_x0, yy - 0.022, bar_x0 + width_val * (bar_x1 - bar_x0), yy + 0.022, col = value_to_gray(count_val, max_count), border = "gray35", lwd = 0.5)
  text(bar_x0 + width_val * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_val), cex = 0.55, adj = c(0, 0.5), col = "gray15")
}
text(0.5, 0.09, paste0("Unique PD genetic-context genes in catalog: ", nrow(pd_catalog)), cex = 0.65, col = "gray20")
dev.off()
cat("[11G FINAL] Wrote figure:", fig_a, "\n")

fig_b <- open_pdf_safe("11G_FINAL_FigB_module_PD_genetic_context_support_heatmap.pdf", 11.5, 6.8)
new_canvas()
draw_title("Module-level PD genetic-context support", "Overlap of project marker signatures with conservative PD genetic-context genes.")
plot_df <- module_support_df
if (nrow(plot_df) > 0) {
  plot_df <- plot_df[order(plot_df$n_PD_genetic_context_genes, decreasing = TRUE), , drop = FALSE]
  metrics <- c("n_PD_genetic_context_genes", "overlap_ratio", "max_PD_genetic_weight_in_module", "genetic_support_score")
  tmp_score <- rep(0, nrow(plot_df))
  tmp_score[plot_df$genetic_context_support_tier == "genetic_context_support_low"] <- 1
  tmp_score[plot_df$genetic_context_support_tier == "genetic_context_support_intermediate"] <- 2
  tmp_score[plot_df$genetic_context_support_tier == "genetic_context_support_high"] <- 3
  mat <- cbind(
    n_PD_genetic_context_genes = safe_num(plot_df$n_PD_genetic_context_genes),
    overlap_ratio = safe_num(plot_df$overlap_ratio),
    max_PD_genetic_weight = safe_num(plot_df$max_PD_genetic_weight_in_module),
    support_tier_score = tmp_score
  )

  mat_norm <- mat
  for (jj in seq_len(ncol(mat_norm))) {
    val <- mat_norm[, jj]
    mn <- min(val, na.rm = TRUE)
    mx <- max(val, na.rm = TRUE)
    if (!is.finite(mn) || !is.finite(mx) || abs(mx - mn) < 1e-12) {
      mat_norm[, jj] <- 0
    } else {
      mat_norm[, jj] <- (val - mn) / (mx - mn)
    }
  }
  hm_x0 <- 0.34
  hm_x1 <- 0.86
  hm_y0 <- 0.16
  hm_y1 <- 0.84
  nr <- nrow(mat_norm)
  nc <- ncol(mat_norm)
  cell_w <- (hm_x1 - hm_x0) / nc
  cell_h <- (hm_y1 - hm_y0) / nr
  for (ii in seq_len(nr)) {
    for (jj in seq_len(nc)) {
      val <- mat_norm[ii, jj]
      rect(hm_x0 + (jj - 1) * cell_w, hm_y1 - ii * cell_h,
           hm_x0 + jj * cell_w, hm_y1 - (ii - 1) * cell_h,
           col = gray(0.95 - 0.65 * val), border = "white", lwd = 0.35)
    }
  }
  rect(hm_x0, hm_y0, hm_x1, hm_y1, border = "gray35", lwd = 0.7)
  for (ii in seq_len(nr)) {
    yy <- hm_y1 - (ii - 0.5) * cell_h
    text(hm_x0 - 0.012, yy, plot_df$module_name[ii], cex = 0.48, adj = c(1, 0.5), col = "gray10")
  }
  col_labs <- c("PD genes", "Overlap", "Max weight", "Tier score")
  for (jj in seq_len(nc)) {
    xx <- hm_x0 + (jj - 0.5) * cell_w
    text(xx, 0.085, col_labs[jj], cex = 0.48, srt = 90, adj = c(0.5, 0.5), col = "gray10")
  }
  text(0.91, 0.78, "darker = stronger", cex = 0.48, srt = 90, col = "gray30")
} else {
  text(0.5, 0.5, "No module support rows available.", cex = 0.8)
}
dev.off()
cat("[11G FINAL] Wrote figure:", fig_b, "\n")

fig_c <- open_pdf_safe("11G_FINAL_FigC_candidate_marker_PD_overlap_lollipop.pdf", 11.2, 6.4)
new_canvas()
draw_title("Candidate marker signature overlap with PD genetic context", "Descriptive overlap only; not clinical biomarker validation.")
plot_df <- module_support_df
plot_df <- plot_df[order(plot_df$n_PD_genetic_context_genes, decreasing = FALSE), , drop = FALSE]
if (nrow(plot_df) > 0) {
  y_pos <- seq(0.18, 0.82, length.out = nrow(plot_df))
  x0 <- 0.28
  x1 <- 0.88
  max_val <- max(plot_df$n_PD_genetic_context_genes, na.rm = TRUE)
  if (!is.finite(max_val) || max_val < 1) max_val <- 1
  for (tick_val in seq(0, max_val, length.out = 6)) {
    xx <- x0 + tick_val / max_val * (x1 - x0)
    segments(xx, 0.14, xx, 0.86, col = "gray92", lwd = 0.5)
    text(xx, 0.10, as.character(round(tick_val, 1)), cex = 0.5, col = "gray25")
  }
  for (ii in seq_len(nrow(plot_df))) {
    yy <- y_pos[ii]
    vv <- plot_df$n_PD_genetic_context_genes[ii]
    xx <- x0 + vv / max_val * (x1 - x0)
    text(x0 - 0.012, yy, plot_df$module_name[ii], cex = 0.52, adj = c(1, 0.5), col = "gray10")
    segments(x0, yy, xx, yy, col = "gray70", lwd = 0.8)
    point_cex <- 0.65 + 0.25 * min(plot_df$max_PD_genetic_weight_in_module[ii], 3)
    points(xx, yy, pch = 21, bg = value_to_gray(vv, max_val), col = "gray20", cex = point_cex, lwd = 0.5)
    label_genes <- plot_df$PD_genetic_context_genes[ii]
    if (nchar(label_genes) > 34) label_genes <- paste0(substr(label_genes, 1, 31), "...")
    text(xx + 0.012, yy, label_genes, cex = 0.42, adj = c(0, 0.5), col = "gray25")
  }
  text((x0 + x1) / 2, 0.045, "Number of PD genetic-context genes overlapping each module", cex = 0.63, col = "gray15")
} else {
  text(0.5, 0.5, "No overlap rows available.", cex = 0.8)
}
dev.off()
cat("[11G FINAL] Wrote figure:", fig_c, "\n")

fig_d <- open_pdf_safe("11G_FINAL_FigD_PD_genetic_context_support_tier_summary.pdf", 9.8, 5.8)
new_canvas()
draw_title("11G genetic-context support tier summary", "Used for downstream 11H integration; conservative proxy/support evidence only.")
tier_order <- c("genetic_context_support_high", "genetic_context_support_intermediate", "genetic_context_support_low", "no_detected_PD_genetic_context_overlap")
tier_tab <- table(factor(module_support_df$genetic_context_support_tier, levels = tier_order), useNA = "no")
tier_counts <- as.numeric(tier_tab)
names(tier_counts) <- tier_order
max_count <- max(tier_counts, na.rm = TRUE)
if (!is.finite(max_count) || max_count < 1) max_count <- 1
y_pos <- seq(0.76, 0.28, length.out = length(tier_counts))
bar_x0 <- 0.34
bar_x1 <- 0.84
for (ii in seq_along(tier_counts)) {
  yy <- y_pos[ii]
  vv <- tier_counts[ii]
  width_val <- vv / max_count
  lab <- gsub("genetic_context_support_", "", names(tier_counts)[ii])
  lab <- gsub("no_detected_PD_genetic_context_overlap", "none", lab)
  text(bar_x0 - 0.02, yy, lab, cex = 0.58, adj = c(1, 0.5), col = "gray10")
  rect(bar_x0, yy - 0.034, bar_x0 + width_val * (bar_x1 - bar_x0), yy + 0.034, col = value_to_gray(vv, max_count), border = "gray35", lwd = 0.6)
  text(bar_x0 + width_val * (bar_x1 - bar_x0) + 0.014, yy, as.character(vv), cex = 0.6, adj = c(0, 0.5), col = "gray10")
}
text(0.5, 0.12, "Full module-level table: 11G_FINAL_PD_genetic_context_support_table_for_11H.csv", cex = 0.50, col = "gray35")
dev.off()
cat("[11G FINAL] Wrote figure:", fig_d, "\n")

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
    "PD genetic-context support for prioritised transcriptomic modules",
    "Candidate transcriptomic marker signatures with partial PD genetic support",
    "Supportive evidence layer for 11H integrated evidence tier",
    "Descriptive overlap/enrichment analysis only",
    "Clinical prediction model for Parkinson disease",
    "Diagnostic biomarker discovery",
    "Prognostic biomarker validation",
    "Causal validation of graft outcome or PD risk",
    "Therapeutic response biomarker claim"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_boundary_df, file.path(out_table_dir, "11G_FINAL_claim_boundary.csv"))

summary_df <- data.frame(
  item = c(
    "pd_genetic_context_catalog_genes",
    "built_in_marker_signature_rows",
    "optional_upstream_files_scanned",
    "harvested_upstream_gene_rows",
    "master_candidate_gene_count",
    "master_candidate_genes_with_PD_genetic_context",
    "module_support_rows_for_11H",
    "modules_with_high_or_intermediate_genetic_context_support",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(nrow(pd_catalog)),
    as.character(nrow(builtin_marker_df)),
    as.character(length(candidate_files)),
    as.character(nrow(harvested_df)),
    as.character(nrow(master_gene_df)),
    as.character(sum(master_gene_df$in_PD_genetic_context_catalog, na.rm = TRUE)),
    as.character(nrow(input_11h_df)),
    as.character(sum(input_11h_df$genetic_context_support_tier %in% c("genetic_context_support_high", "genetic_context_support_intermediate"), na.rm = TRUE)),
    "4",
    "INPUT_READY_FOR_11H_INTEGRATION_AS_PD_GENETIC_CONTEXT_SUPPORT"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "11G_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "11G_FINAL_execution_summary.tsv"))

report_lines <- c(
  "11G FINAL report",
  "================",
  "Module: PD GWAS / genetic-context support",
  "Mode: complete standalone 11G rebuild; no previous 11G output dependency; no internet; no 00-10P rerun.",
  "",
  paste0("PD genetic-context catalog genes: ", nrow(pd_catalog)),
  paste0("Built-in marker signature rows: ", nrow(builtin_marker_df)),
  paste0("Optional upstream files scanned: ", length(candidate_files)),
  paste0("Harvested upstream gene rows: ", nrow(harvested_df)),
  paste0("Master candidate gene count: ", nrow(master_gene_df)),
  paste0("Candidate genes with PD genetic context: ", sum(master_gene_df$in_PD_genetic_context_catalog, na.rm = TRUE)),
  paste0("Module support rows for 11H: ", nrow(input_11h_df)),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_table_dir, "11G_FINAL_PD_genetic_context_support_table_for_11H.csv")),
  paste0("- ", file.path(out_table_dir, "11G_FINAL_candidate_marker_signature_genes_with_PD_genetic_context.csv")),
  paste0("- ", file.path(out_table_dir, "11G_FINAL_gene_level_PD_genetic_context_overlap_table.csv")),
  paste0("- ", file.path(out_table_dir, "11G_FINAL_module_level_PD_genetic_context_support.csv")),
  "",
  "Allowed interpretation:",
  "- PD genetic-context support for transcriptomic prioritisation.",
  "- Candidate transcriptomic marker signatures with partial PD genetic support.",
  "- Downstream 11H integration only as one conservative evidence layer.",
  "",
  "Prohibited interpretation:",
  "- No clinical prediction.",
  "- No PD diagnosis.",
  "- No causal validation.",
  "- No validated clinical biomarker claim.",
  "",
  "Decision: INPUT_READY_FOR_11H_INTEGRATION_AS_PD_GENETIC_CONTEXT_SUPPORT"
)
report_file <- file.path(out_text_dir, "11G_FINAL_PD_GWAS_genetic_context_support_report.txt")
writeLines(report_lines, report_file)
cat("[11G FINAL] Wrote:", report_file, "\n")

cat("\n[11G FINAL] Completed PD GWAS / genetic-context support.\n")
cat("[11G FINAL] PD genetic-context catalog genes:", nrow(pd_catalog), "\n")
cat("[11G FINAL] Optional upstream files scanned:", length(candidate_files), "\n")
cat("[11G FINAL] Harvested upstream gene rows:", nrow(harvested_df), "\n")
cat("[11G FINAL] Master candidate gene count:", nrow(master_gene_df), "\n")
cat("[11G FINAL] Candidate genes with PD genetic context:", sum(master_gene_df$in_PD_genetic_context_catalog, na.rm = TRUE), "\n")
cat("[11G FINAL] Module support rows for 11H:", nrow(input_11h_df), "\n")
cat("[11G FINAL] Figures written: 4\n")
cat("[11G FINAL] Decision: INPUT_READY_FOR_11H_INTEGRATION_AS_PD_GENETIC_CONTEXT_SUPPORT\n")
cat("[11G FINAL] Output tables:", out_table_dir, "\n")
cat("[11G FINAL] Output figs  :", out_fig_dir, "\n")
cat("[11G FINAL] Output text  :", out_text_dir, "\n")
cat("[11G FINAL] Next         : review 11G FINAL PDFs; if accepted, proceed to 11H integrated evidence tier + marker signature table.\n")
