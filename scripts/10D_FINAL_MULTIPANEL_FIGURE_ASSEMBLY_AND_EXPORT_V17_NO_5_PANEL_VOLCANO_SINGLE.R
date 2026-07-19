# ============================================================
# 10D_FINAL_MULTIPANEL_FIGURE_ASSEMBLY_AND_EXPORT_V17_NO_5_PANEL_VOLCANO_SINGLE.R
# ============================================================
#
# PD_Graft_Project — 10D
# V16 storyline layout：读取 10C V16，不改 source lock；按强期刊叙事顺序重排 main figures：atlas → prioritization/molecular program → enrichment → ML → negative controls → external validation → disease-context validation。
# V15 JPEG-only renderer：保留 V13/V14 六主图拆分；PDF source 全部优先 JPEG 渲染，绕开 libpng IDAT CRC error 和透明层粉色块。
# V14 native-raster white-bg fix：保留 V13 六主图拆分；修复 as.raster() 产生 '#FFFFFF\0' NUL 字符串错误。
# V13 six-main-figure polish：读取 10C V16，不改 source lock；修复透明背景粉色块；主图从 5 张改为 6 张，拆分原 Figure 4 以提高可读性。
# V12 layout polish：读取 10C V16，不改 source lock；最终拼图时删除 Figure 1A workflow，将 Figure 1 原 B/C/D/E 重新标为 A/B/C/D，并修正 Figure 5B panel title。
# V9 read 10C V16：读取修正后的 10C V16 manifest，确保 Supplementary Figure 7 使用 Hallmark GSEA barplot。
# V8 supp height fix：把 current_supp_height_in 提前到补图 panel 循环前定义，修复 V7 找不到对象错误。
# V6 grid fix：去掉 pushViewport/popViewport + grid.arrange 混用，改为 arrangeGrob + grid.draw，避免 viewport stack error。
# V5 font fix：Windows grDevices::pdf() 不识别 family='Arial'，改为 R 内置 family='sans'。
# V3：PDF source 渲染使用 PNG 优先，失败自动 JPEG fallback。
# Final multi-panel figure assembly, layout standardization and export
#
# 输入：
#   10C final = V13_SUPPLEMENT_DEDUP_S2B_S10B_LOCKED
#
# 本脚本作用：
#   1. 读取 10C V13 已锁定的 main/supplementary source manifest。
#   2. 不重跑任何分析，不重新选择图源。
#   3. 把 10C 复制好的 source PDF 组装成：
#        Figure 1–5
#        Supplementary Figure 1–10
#   4. 添加 panel letters。
#   5. 统一页面尺寸、边距、标题和 panel spacing。
#   6. 导出 PDF 和 TIFF。
#   7. 生成 transformation audit、assembly manifest、output verification。
#
# 重要边界：
#   - 10D 只改变排版，不改变数据。
#   - 不修改 source PDF 本身。
#   - 不改变 axis、legend、point、stat annotation。
#   - 自动拼图会把 PDF panel 渲染后排版；10C 中仍保留原始 source PDF。
#
# 需要 R 包：
#   install.packages(c("data.table", "pdftools", "png", "gridExtra", "digest"))
#
# 如果你想导出 TIFF：
#   推荐额外安装：
#   install.packages("ragg")
#
# 运行：
#   source("10D_FINAL_MULTIPANEL_FIGURE_ASSEMBLY_AND_EXPORT_V17_NO_5_PANEL_VOLCANO_SINGLE.R")
#
# V2 修正：
#   - V1 使用 pdf_render_page() 后直接 readPNG()，会导致 libpng error: Not a PNG file。
#   - V2 改为 pdftools::pdf_convert() -> 临时 PNG -> png::readPNG()。
#   - 字体 warning 如 No display font for ArialNarrow 通常是渲染器字体替代，不等于失败。
#
# 成功标志：
#   ✅ 10D final multi-panel figure assembly V17 no-5-panel-volcano-single 完成。
#
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

INPUT_10C_TAG <- "10C_final_V16_F2E_S7A_HALLMARK_BARPLOT_F5B_CLUSTER_SIZE"

# 期刊级自动拼图默认参数。
EXPORT_PDF <- TRUE
EXPORT_TIFF <- FALSE

# 渲染 panel PDF 的分辨率。
# 300 可快速检查；600 更适合投稿级位图导出。
PANEL_RENDER_DPI <- 300

# TIFF 导出分辨率。
TIFF_DPI <- 300

# 主图页面尺寸，单位 inch。
# 180 mm 宽约 7.09 inch；这里用 7.2 inch 适配常见双栏/整页宽。
MAIN_FIG_WIDTH_IN <- 7.2
MAIN_FIG_HEIGHT_IN <- 8.8

# 补图页面尺寸，稍高一点。
SUPP_FIG_WIDTH_IN <- 7.2
SUPP_FIG_HEIGHT_IN <- 9.2

# 字体。
BASE_FAMILY <- "sans"

# panel 字母大小。
PANEL_LETTER_CEX <- 1.25

# 标题大小。
TITLE_CEX <- 0.90

# 是否导出每个 panel 的 raster preview，便于检查。
EXPORT_PANEL_PREVIEWS <- FALSE

# V17: 投稿级排版时，图中不再放大号全局标题。
# Figure title 和解释性文字应放在 legend/caption，而不是图像主体里。
SHOW_GLOBAL_FIGURE_TITLE <- FALSE

# V17: 保持 source panel 原始宽高比，避免 UMAP/heatmap/barplot 被拉伸。
PRESERVE_PANEL_ASPECT_RATIO <- TRUE


# ============================================================
# 1. 包检查
# ============================================================

required_pkgs <- c(
  "data.table",
  "pdftools",
  "png",
  "jpeg",
  "gridExtra",
  "digest"
)

missing_pkgs <- required_pkgs[
  !vapply(
    required_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_pkgs) > 0L) {
  stop(
    paste0(
      "缺少 R 包：",
      paste(missing_pkgs, collapse = ", "),
      "\n请先运行：install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(grid)
  library(gridExtra)
})

HAS_RAGG <- requireNamespace(
  "ragg",
  quietly = TRUE
)


# ============================================================
# 2. 路径
# ============================================================

TABLE_ROOT <- file.path(
  PROJECT_DIR,
  "03_tables"
)

REPORT_ROOT <- file.path(
  PROJECT_DIR,
  "06_reports"
)

MANUSCRIPT_ROOT <- file.path(
  PROJECT_DIR,
  "09_manuscript"
)

DIR_10C_TABLE <- file.path(
  TABLE_ROOT,
  INPUT_10C_TAG
)

DIR_10C_REPORT <- file.path(
  REPORT_ROOT,
  INPUT_10C_TAG
)

DIR_10C_PACKAGE <- file.path(
  MANUSCRIPT_ROOT,
  INPUT_10C_TAG
)

INPUT_MAIN_MANIFEST <- file.path(
  DIR_10C_TABLE,
  "10C_V16_main_figure_source_manifest.csv"
)

INPUT_SUPP_MANIFEST <- file.path(
  DIR_10C_TABLE,
  "10C_V16_supplementary_figure_source_manifest.csv"
)

INPUT_PANEL_MAPPING <- file.path(
  DIR_10C_TABLE,
  "10C_V16_manuscript_panel_mapping.csv"
)

INPUT_SELECTION_SUMMARY <- file.path(
  DIR_10C_TABLE,
  "10C_V16_selection_confidence_summary.csv"
)

INPUT_COPY_AUDIT <- file.path(
  DIR_10C_TABLE,
  "10C_V16_copy_and_hash_integrity_audit.csv"
)

INPUT_10C_ASSEMBLY_BRIEF <- file.path(
  DIR_10C_REPORT,
  "10C_V16_10D_assembly_brief.txt"
)

OUT_ROOT <- file.path(
  MANUSCRIPT_ROOT,
  "10D_final_multipanel_figure_assembly_V17"
)

OUT_MAIN_DIR <- file.path(
  OUT_ROOT,
  "main_figures"
)

OUT_SUPP_DIR <- file.path(
  OUT_ROOT,
  "supplementary_figures"
)

OUT_PANEL_PREVIEW_DIR <- file.path(
  OUT_ROOT,
  "panel_previews"
)

OUT_TABLE_DIR <- file.path(
  TABLE_ROOT,
  "10D_final_multipanel_figure_assembly_V17"
)

OUT_REPORT_DIR <- file.path(
  REPORT_ROOT,
  "10D_final_multipanel_figure_assembly_V17"
)

dir.create(
  OUT_MAIN_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  OUT_SUPP_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  OUT_PANEL_PREVIEW_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  OUT_TABLE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  OUT_REPORT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. 输出文件
# ============================================================

OUT_INPUT_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10D_V17_input_audit.csv"
)

OUT_SOURCE_PANEL_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10D_V17_source_panel_render_audit.csv"
)

OUT_ASSEMBLY_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10D_V17_multiplanel_assembly_audit.csv"
)

OUT_TRANSFORMATION_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10D_V17_transformation_audit.csv"
)

OUT_OUTPUT_VERIFICATION <- file.path(
  OUT_TABLE_DIR,
  "10D_V17_output_verification.csv"
)

OUT_FIGURE_INDEX <- file.path(
  OUT_TABLE_DIR,
  "10D_V17_final_figure_file_index.csv"
)

OUT_LAYOUT_POLICY_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10D_V17_layout_policy_exclusions_and_title_fixes.csv"
)

OUT_REPORT <- file.path(
  OUT_REPORT_DIR,
  "10D_V17_final_multiplanel_figure_assembly_report.txt"
)

OUT_SESSION <- file.path(
  OUT_REPORT_DIR,
  "10D_V17_sessionInfo.txt"
)


# ============================================================
# 4. 工具函数
# ============================================================

stamp <- function(...) {
  cat(
    "[",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] ",
    ...,
    "\n",
    sep = ""
  )
}

normalize_path <- function(x) {
  normalizePath(
    x,
    winslash = "/",
    mustWork = FALSE
  )
}

safe_fread <- function(path) {
  if (!file.exists(path)) {
    stop("找不到输入文件：", path)
  }

  data.table::fread(
    path,
    data.table = FALSE,
    showProgress = FALSE,
    encoding = "UTF-8"
  )
}

safe_read_text <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )
}

sha256_file <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  tryCatch(
    digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    ),
    error = function(e) NA_character_
  )
}

atomic_write_csv <- function(df, path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(
      status = "empty",
      stringsAsFactors = FALSE
    )
  }

  tmp <- paste0(
    path,
    ".tmp_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  data.table::fwrite(
    df,
    tmp,
    bom = TRUE
  )

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  if (!file.rename(tmp, path)) {
    stop("CSV 写入失败：", path)
  }

  if (!file.exists(path) ||
      !is.finite(file.info(path)$size) ||
      file.info(path)$size <= 0) {
    stop("CSV 输出无效：", path)
  }

  invisible(path)
}

atomic_write_text <- function(lines, path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  tmp <- paste0(
    path,
    ".tmp_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  writeLines(
    enc2utf8(as.character(lines)),
    con = tmp,
    useBytes = TRUE
  )

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  if (!file.rename(tmp, path)) {
    stop("文本写入失败：", path)
  }

  if (!file.exists(path) ||
      !is.finite(file.info(path)$size) ||
      file.info(path)$size <= 0) {
    stop("文本输出无效：", path)
  }

  invisible(path)
}

sanitize_filename <- function(x) {
  x <- gsub(
    "[^A-Za-z0-9_-]+",
    "_",
    x
  )

  x <- gsub(
    "_+",
    "_",
    x
  )

  gsub(
    "^_|_$",
    "",
    x
  )
}

render_pdf_page_to_raster <- function(
  pdf_path,
  dpi = PANEL_RENDER_DPI
) {
  if (!file.exists(pdf_path)) {
    stop("panel source 不存在：", pdf_path)
  }

  # V17 robust rendering:
  # Use JPEG as the primary renderer for all source PDFs.
  #
  # Why:
  #   - PNG rendering repeatedly triggered libpng IDAT CRC errors on this Windows/R setup.
  #   - PNG transparency could appear as magenta/pink blocks in WPS/Windows PDF viewers.
  #   - JPEG is opaque by design, so it avoids both transparency artifacts and libpng.
  #
  # Fallback:
  #   If JPEG conversion truly fails, try PNG native read once.
  #   But normal path should be JPEG.

  cleanup_paths <- character()
  on.exit(
    unlink(
      unique(cleanup_paths),
      force = TRUE
    ),
    add = TRUE
  )

  jpg_error <- NA_character_

  tmp_jpg <- paste0(
    tempfile(pattern = "10D_panel_render_jpeg_primary_"),
    "_%d.%s"
  )

  jpg_result <- tryCatch(
    {
      converted_jpg <- pdftools::pdf_convert(
        pdf = pdf_path,
        format = "jpeg",
        pages = 1,
        filenames = tmp_jpg,
        dpi = dpi,
        verbose = FALSE
      )

      cleanup_paths <<- c(
        cleanup_paths,
        tmp_jpg,
        converted_jpg
      )

      jpg_path <- if (length(converted_jpg) >= 1L && file.exists(converted_jpg[[1]])) {
        converted_jpg[[1]]
      } else {
        created <- Sys.glob(gsub("%d.%s$", "*.*", tmp_jpg))
        if (length(created) >= 1L && file.exists(created[[1]])) {
          created[[1]]
        } else {
          stop("pdf_convert did not create JPEG")
        }
      }

      raster <- jpeg::readJPEG(
        jpg_path,
        native = TRUE
      )

      dim_info <- dim(raster)

      list(
        raster = raster,
        width_px = dim_info[[2]],
        height_px = dim_info[[1]],
        render_method = "pdf_convert_jpeg_primary_native_raster",
        fallback_used = FALSE,
        png_error = NA_character_
      )
    },
    error = function(e) {
      jpg_error <<- conditionMessage(e)
      NULL
    }
  )

  if (!is.null(jpg_result)) {
    return(jpg_result)
  }

  # Last-resort PNG fallback, without alpha compositing.
  # This is intentionally secondary because PNG was the unstable path.
  tmp_png <- paste0(
    tempfile(pattern = "10D_panel_render_png_last_resort_"),
    "_%d.%s"
  )

  png_result <- tryCatch(
    {
      converted_png <- pdftools::pdf_convert(
        pdf = pdf_path,
        format = "png",
        pages = 1,
        filenames = tmp_png,
        dpi = dpi,
        verbose = FALSE
      )

      cleanup_paths <<- c(
        cleanup_paths,
        tmp_png,
        converted_png
      )

      png_path <- if (length(converted_png) >= 1L && file.exists(converted_png[[1]])) {
        converted_png[[1]]
      } else {
        created <- Sys.glob(gsub("%d.%s$", "*.*", tmp_png))
        if (length(created) >= 1L && file.exists(created[[1]])) {
          created[[1]]
        } else {
          stop("pdf_convert did not create PNG")
        }
      }

      raster <- png::readPNG(
        png_path,
        native = TRUE
      )

      dim_info <- dim(raster)

      list(
        raster = raster,
        width_px = dim_info[[2]],
        height_px = dim_info[[1]],
        render_method = "pdf_convert_png_last_resort_native_raster",
        fallback_used = TRUE,
        png_error = NA_character_
      )
    },
    error = function(e) {
      stop(
        "Both JPEG primary and PNG fallback rendering failed for: ",
        pdf_path,
        "\nJPEG error: ",
        jpg_error,
        "\nPNG error: ",
        conditionMessage(e)
      )
    }
  )

  png_result
}


make_panel_grob <- function(
  pdf_path,
  panel_letter,
  panel_title = "",
  dpi = PANEL_RENDER_DPI,
  slot_aspect = 1.30
) {
  rendered <- render_pdf_page_to_raster(
    pdf_path,
    dpi = dpi
  )

  image_aspect <- rendered$width_px / rendered$height_px

  # V17 aspect-safe placement:
  # The rendered source PDF is placed inside its panel slot without stretching.
  # In npc units, display aspect = (width_npc * slot_width) / (height_npc * slot_height).
  # Therefore display aspect = (width_npc / height_npc) * slot_aspect.
  max_w <- 0.94
  max_h <- 0.80

  if (isTRUE(PRESERVE_PANEL_ASPECT_RATIO)) {
    if (image_aspect >= slot_aspect) {
      img_w <- max_w
      img_h <- max_w * slot_aspect / image_aspect
      if (img_h > max_h) {
        img_h <- max_h
        img_w <- max_h * image_aspect / slot_aspect
      }
    } else {
      img_h <- max_h
      img_w <- max_h * image_aspect / slot_aspect
      if (img_w > max_w) {
        img_w <- max_w
        img_h <- max_w * slot_aspect / image_aspect
      }
    }
  } else {
    img_w <- 0.94
    img_h <- 0.80
  }

  img_w <- max(min(img_w, 0.96), 0.10)
  img_h <- max(min(img_h, 0.82), 0.10)

  img <- rasterGrob(
    rendered$raster,
    x = unit(0.5, "npc"),
    y = unit(0.43, "npc"),
    width = unit(img_w, "npc"),
    height = unit(img_h, "npc"),
    just = "center",
    interpolate = TRUE
  )

  letter <- textGrob(
    panel_letter,
    x = unit(0.015, "npc"),
    y = unit(0.985, "npc"),
    just = c("left", "top"),
    gp = gpar(
      fontfamily = BASE_FAMILY,
      fontface = "bold",
      fontsize = 15,
      col = "black"
    )
  )

  title <- textGrob(
    panel_title,
    x = unit(0.09, "npc"),
    y = unit(0.975, "npc"),
    just = c("left", "top"),
    gp = gpar(
      fontfamily = BASE_FAMILY,
      fontsize = 8.2,
      col = "black"
    )
  )

  frame <- rectGrob(
    gp = gpar(
      col = NA,
      fill = NA
    )
  )

  grobTree(
    frame,
    img,
    letter,
    title
  )
}


device_open_pdf <- function(
  path,
  width,
  height
) {
  # V5 fix:
  # On Windows, grDevices::pdf() does not reliably accept family = "Arial"
  # unless that PDF font family is registered. Use built-in "sans" instead.
  grDevices::pdf(
    file = path,
    width = width,
    height = height,
    family = "sans",
    onefile = TRUE,
    useDingbats = FALSE
  )
}

device_open_tiff <- function(
  path,
  width,
  height,
  dpi
) {
  if (HAS_RAGG) {
    ragg::agg_tiff(
      filename = path,
      width = width,
      height = height,
      units = "in",
      res = dpi,
      compression = "lzw"
    )
  } else {
    grDevices::tiff(
      filename = path,
      width = width,
      height = height,
      units = "in",
      res = dpi,
      compression = "lzw"
    )
  }
}

draw_figure_layout <- function(
  grobs,
  figure_title,
  layout_type = "main_5"
) {
  # V17 aspect-safe grid:
  # - no manual viewport popping
  # - optional global title
  # - source panels keep original aspect ratio inside their slots

  grid::grid.newpage()

  if (layout_type == "main_5") {
    body_grob <- gridExtra::arrangeGrob(
      grobs = grobs,
      layout_matrix = rbind(
        c(1, 2),
        c(3, 4),
        c(5, 5)
      ),
      padding = grid::unit(0.055, "in")
    )
  } else if (layout_type == "main_4") {
    body_grob <- gridExtra::arrangeGrob(
      grobs = grobs,
      layout_matrix = rbind(
        c(1, 2),
        c(3, 4)
      ),
      padding = grid::unit(0.055, "in")
    )
  } else if (layout_type == "main_3") {
    body_grob <- gridExtra::arrangeGrob(
      grobs = grobs,
      layout_matrix = rbind(
        c(1, 2),
        c(3, 3)
      ),
      padding = grid::unit(0.055, "in")
    )
  } else if (layout_type == "main_3_horizontal") {
    body_grob <- gridExtra::arrangeGrob(
      grobs = grobs,
      ncol = 3,
      padding = grid::unit(0.055, "in")
    )
  } else if (layout_type == "two_panel") {
    body_grob <- gridExtra::arrangeGrob(
      grobs = grobs,
      ncol = 2,
      padding = grid::unit(0.055, "in")
    )
  } else if (layout_type == "one_panel") {
    body_grob <- gridExtra::arrangeGrob(
      grobs = grobs,
      ncol = 1,
      padding = grid::unit(0.055, "in")
    )
  } else {
    n <- length(grobs)
    ncol <- ifelse(n <= 2, n, 2)

    body_grob <- gridExtra::arrangeGrob(
      grobs = grobs,
      ncol = ncol,
      padding = grid::unit(0.055, "in")
    )
  }

  if (isTRUE(SHOW_GLOBAL_FIGURE_TITLE)) {
    title_grob <- grid::textGrob(
      figure_title,
      x = grid::unit(0, "npc"),
      y = grid::unit(0.5, "npc"),
      just = c("left", "center"),
      gp = grid::gpar(
        fontfamily = BASE_FAMILY,
        fontface = "bold",
        fontsize = 10
      )
    )

    full_grob <- gridExtra::arrangeGrob(
      title_grob,
      body_grob,
      ncol = 1,
      heights = grid::unit.c(
        grid::unit(0.28, "in"),
        grid::unit(1, "null")
      )
    )
  } else {
    full_grob <- body_grob
  }

  grid::grid.draw(full_grob)

  invisible(full_grob)
}


export_assembled_figure <- function(
  grobs,
  figure_title,
  out_pdf,
  out_tiff,
  width_in,
  height_in,
  layout_type
) {
  outputs <- list()

  if (EXPORT_PDF) {
    device_open_pdf(
      out_pdf,
      width = width_in,
      height = height_in
    )

    draw_figure_layout(
      grobs = grobs,
      figure_title = figure_title,
      layout_type = layout_type
    )

    dev.off()

    outputs$pdf <- out_pdf
  } else {
    outputs$pdf <- NA_character_
  }

  if (EXPORT_TIFF) {
    device_open_tiff(
      out_tiff,
      width = width_in,
      height = height_in,
      dpi = TIFF_DPI
    )

    draw_figure_layout(
      grobs = grobs,
      figure_title = figure_title,
      layout_type = layout_type
    )

    dev.off()

    outputs$tiff <- out_tiff
  } else {
    outputs$tiff <- NA_character_
  }

  outputs
}

write_panel_preview <- function(
  rendered,
  out_path
) {
  if (!EXPORT_PANEL_PREVIEWS) {
    return(FALSE)
  }

  png::writePNG(
    rendered$raster,
    target = out_path
  )

  TRUE
}


# ============================================================
# 5. 输入审计
# ============================================================

cat("\n============================================================\n")
cat("10D V17：Final multi-panel figure assembly and export\n")
cat("============================================================\n\n")

stamp("读取 10C V16 locked manifests。")

input_paths <- c(
  INPUT_MAIN_MANIFEST,
  INPUT_SUPP_MANIFEST,
  INPUT_PANEL_MAPPING,
  INPUT_SELECTION_SUMMARY,
  INPUT_COPY_AUDIT,
  INPUT_10C_ASSEMBLY_BRIEF,
  DIR_10C_PACKAGE
)

input_audit <- data.frame(
  input = input_paths,
  exists = file.exists(input_paths) | dir.exists(input_paths),
  type = ifelse(
    dir.exists(input_paths),
    "directory",
    "file"
  ),
  size_bytes = ifelse(
    file.exists(input_paths),
    file.info(input_paths)$size,
    NA_real_
  ),
  sha256 = ifelse(
    file.exists(input_paths),
    vapply(
      input_paths,
      sha256_file,
      character(1)
    ),
    NA_character_
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  input_audit,
  OUT_INPUT_AUDIT
)

missing_inputs <- input_audit[
  !input_audit$exists,
  ,
  drop = FALSE
]

if (nrow(missing_inputs) > 0L) {
  print(missing_inputs)
  stop("10D 缺少 10C V13 输入，不能继续。")
}

main_manifest <- safe_fread(
  INPUT_MAIN_MANIFEST
)

supp_manifest <- safe_fread(
  INPUT_SUPP_MANIFEST
)

panel_mapping <- safe_fread(
  INPUT_PANEL_MAPPING
)

selection_summary <- safe_fread(
  INPUT_SELECTION_SUMMARY
)

copy_audit <- safe_fread(
  INPUT_COPY_AUDIT
)

required_manifest_cols <- c(
  "item_id",
  "figure_id",
  "panel",
  "panel_title",
  "copied_path",
  "source_path",
  "source_sha256",
  "selection_status"
)

for (nm in required_manifest_cols) {
  if (!nm %in% names(main_manifest)) {
    stop("main_manifest 缺少列：", nm)
  }

  if (!nm %in% names(supp_manifest)) {
    stop("supp_manifest 缺少列：", nm)
  }
}

if (any(main_manifest$selection_status != "resolved")) {
  stop("main_manifest 中存在未 resolved source。")
}

if (any(supp_manifest$selection_status != "resolved")) {
  stop("supp_manifest 中存在未 resolved source。")
}

if (any(!file.exists(main_manifest$copied_path))) {
  print(
    main_manifest[
      !file.exists(main_manifest$copied_path),
      ,
      drop = FALSE
    ]
  )
  stop("部分 main copied source 不存在。")
}

if (any(!file.exists(supp_manifest$copied_path))) {
  print(
    supp_manifest[
      !file.exists(supp_manifest$copied_path),
      ,
      drop = FALSE
    ]
  )
  stop("部分 supplementary copied source 不存在。")
}



# ============================================================
# 5B. V12 layout policy：删除 Figure 1A workflow + 修正 Figure 5B 标题
# ============================================================

stamp("应用 V12 layout policy：删除 Figure 1A workflow；Figure 1 重新标号；修正 Figure 5B 标题。")

main_manifest$original_item_id <- main_manifest$item_id
main_manifest$original_panel <- main_manifest$panel
main_manifest$original_panel_title <- main_manifest$panel_title
main_manifest$layout_action <- "unchanged"

supp_manifest$original_item_id <- supp_manifest$item_id
supp_manifest$original_panel <- supp_manifest$panel
supp_manifest$original_panel_title <- supp_manifest$panel_title
supp_manifest$layout_action <- "unchanged"

layout_policy_rows <- list()

# 1. Remove Figure 1A workflow/framework panel from final assembled Figure 1.
drop_f1a <- main_manifest$figure_id == "Figure 1" &
  main_manifest$item_id == "F1A"

if (sum(drop_f1a) != 1L) {
  stop("V12 期望删除 1 个 Figure 1A workflow panel，但找到数量 = ", sum(drop_f1a))
}

excluded_f1a <- main_manifest[drop_f1a, , drop = FALSE]

layout_policy_rows[[length(layout_policy_rows) + 1L]] <- data.frame(
  action = "exclude_from_final_assembly",
  figure_id = excluded_f1a$figure_id[[1]],
  item_id = excluded_f1a$item_id[[1]],
  original_panel = excluded_f1a$original_panel[[1]],
  new_panel = NA_character_,
  original_panel_title = excluded_f1a$original_panel_title[[1]],
  new_panel_title = NA_character_,
  reason = "User requested removal of Figure 1 workflow/framework panel from final assembled Figure 1.",
  source_copied_path = normalize_path(excluded_f1a$copied_path[[1]]),
  stringsAsFactors = FALSE
)

main_manifest <- main_manifest[!drop_f1a, , drop = FALSE]

# 2. Relabel Figure 1 remaining panels:
#    old F1B/F1C/F1D/F1E -> display panels A/B/C/D.
f1_idx <- main_manifest$figure_id == "Figure 1"
f1_rows <- main_manifest[f1_idx, , drop = FALSE]
f1_rows <- f1_rows[order(f1_rows$original_panel), , drop = FALSE]

if (nrow(f1_rows) != 4L) {
  stop("V12 删除 F1A 后，Figure 1 应剩 4 个 panel；当前 n = ", nrow(f1_rows))
}

new_f1_panels <- c("A", "B", "C", "D")

for (i in seq_len(nrow(f1_rows))) {
  row_index <- which(
    main_manifest$item_id == f1_rows$item_id[[i]] &
      main_manifest$figure_id == "Figure 1"
  )

  old_panel <- main_manifest$panel[[row_index]]
  old_title <- main_manifest$panel_title[[row_index]]

  main_manifest$panel[[row_index]] <- new_f1_panels[[i]]
  main_manifest$layout_action[[row_index]] <- "figure1_reletter_after_F1A_exclusion"

  layout_policy_rows[[length(layout_policy_rows) + 1L]] <- data.frame(
    action = "reletter_panel",
    figure_id = "Figure 1",
    item_id = main_manifest$item_id[[row_index]],
    original_panel = old_panel,
    new_panel = main_manifest$panel[[row_index]],
    original_panel_title = old_title,
    new_panel_title = main_manifest$panel_title[[row_index]],
    reason = "Figure 1 workflow panel removed; remaining panels relettered for final assembly.",
    source_copied_path = normalize_path(main_manifest$copied_path[[row_index]]),
    stringsAsFactors = FALSE
  )
}

# 3. Fix Figure 5B displayed title after source replacement in 10C V16.
f5b_idx <- main_manifest$figure_id == "Figure 5" &
  main_manifest$item_id == "F5B"

if (sum(f5b_idx) != 1L) {
  stop("V12 期望找到 1 个 Figure 5B panel，但找到数量 = ", sum(f5b_idx))
}

old_f5b_title <- main_manifest$panel_title[f5b_idx][[1]]
new_f5b_title <- "GSE243639 disease-context cluster sizes"

main_manifest$panel_title[f5b_idx] <- new_f5b_title
main_manifest$layout_action[f5b_idx] <- "title_corrected_after_10C_V16_source_replacement"

layout_policy_rows[[length(layout_policy_rows) + 1L]] <- data.frame(
  action = "correct_panel_title",
  figure_id = "Figure 5",
  item_id = "F5B",
  original_panel = main_manifest$original_panel[f5b_idx][[1]],
  new_panel = main_manifest$panel[f5b_idx][[1]],
  original_panel_title = old_f5b_title,
  new_panel_title = new_f5b_title,
  reason = "10C V16 replaced F5B source from marker overlap to context cluster size barplot; displayed panel title must match final source.",
  source_copied_path = normalize_path(main_manifest$copied_path[f5b_idx][[1]]),
  stringsAsFactors = FALSE
)


# 4. V17 no-5-panel storyline-level main figure remapping:
#    User requested:
#      - Volcano plot as a standalone main figure.
#      - GO / KEGG / Hallmark together in one main figure.
#      - No main figure should contain 5 panels.
#
#    V17 main figure plan:
#      Figure 1  Discovery atlas and transcriptional scoring atlas         F1B/F1C/F1D
#      Figure 2  Dataset prioritization and candidate-state program        F1E/F2A
#      Figure 3  Differential expression volcano                           F2B
#      Figure 4  Functional enrichment evidence                            F2C/F2D/F2E
#      Figure 5  Machine-learning model audit and generalization           F3A/F3B/F3C
#      Figure 6  Machine-learning feature interpretation and stability      F3D/F3E
#      Figure 7  Negative-control robustness                               F4A/F4B
#      Figure 8  GSE183248 external validation                             F4C/F4D/F4E
#      Figure 9  GSE243639 import and disease-context cluster landscape     F5A/F5B
#      Figure 10 GSE243639 molecular validation and priority scoring        F5C/F5D/F5E
#
# This is a layout-level reassignment only. Source files and biological results are unchanged.

storyline_map <- data.frame(
  item_id = c(
    "F1B", "F1C", "F1D",
    "F1E", "F2A",
    "F2B",
    "F2C", "F2D", "F2E",
    "F3A", "F3B", "F3C",
    "F3D", "F3E",
    "F4A", "F4B",
    "F4C", "F4D", "F4E",
    "F5A", "F5B",
    "F5C", "F5D", "F5E"
  ),
  new_figure_id = c(
    "Figure 1", "Figure 1", "Figure 1",
    "Figure 2", "Figure 2",
    "Figure 3",
    "Figure 4", "Figure 4", "Figure 4",
    "Figure 5", "Figure 5", "Figure 5",
    "Figure 6", "Figure 6",
    "Figure 7", "Figure 7",
    "Figure 8", "Figure 8", "Figure 8",
    "Figure 9", "Figure 9",
    "Figure 10", "Figure 10", "Figure 10"
  ),
  new_panel = c(
    "A", "B", "C",
    "A", "B",
    "A",
    "A", "B", "C",
    "A", "B", "C",
    "A", "B",
    "A", "B",
    "A", "B", "C",
    "A", "B",
    "A", "B", "C"
  ),
  storyline_group = c(
    rep("Discovery atlas and transcriptional scoring atlas", 3),
    rep("Dataset prioritization and candidate-state program", 2),
    "Differential expression volcano as standalone evidence",
    rep("GO/KEGG/Hallmark functional enrichment evidence", 3),
    rep("Machine-learning model audit and generalization", 3),
    rep("Machine-learning feature interpretation and stability", 2),
    rep("Negative-control robustness", 2),
    rep("GSE183248 external validation", 3),
    rep("GSE243639 import and disease-context cluster landscape", 2),
    rep("GSE243639 molecular validation and priority scoring", 3)
  ),
  stringsAsFactors = FALSE
)

missing_storyline_items <- setdiff(
  storyline_map$item_id,
  main_manifest$item_id
)

if (length(missing_storyline_items) > 0L) {
  stop(
    "V17 storyline_map 中存在 main_manifest 找不到的 item_id：",
    paste(missing_storyline_items, collapse = ", ")
  )
}

# Apply the storyline remap.
for (i in seq_len(nrow(storyline_map))) {
  idx_item <- which(main_manifest$item_id == storyline_map$item_id[[i]])

  if (length(idx_item) != 1L) {
    stop(
      "V17 期望 item_id 唯一，但 ",
      storyline_map$item_id[[i]],
      " 数量 = ",
      length(idx_item)
    )
  }

  old_fig <- main_manifest$figure_id[[idx_item]]
  old_panel <- main_manifest$panel[[idx_item]]
  old_title <- main_manifest$panel_title[[idx_item]]

  main_manifest$figure_id[[idx_item]] <- storyline_map$new_figure_id[[i]]
  main_manifest$panel[[idx_item]] <- storyline_map$new_panel[[i]]
  main_manifest$layout_action[[idx_item]] <- "v17_no_5_panel_storyline_main_figure_remap"

  layout_policy_rows[[length(layout_policy_rows) + 1L]] <- data.frame(
    action = "v17_no_5_panel_storyline_main_figure_remap",
    figure_id = main_manifest$figure_id[[idx_item]],
    item_id = main_manifest$item_id[[idx_item]],
    original_panel = old_panel,
    new_panel = main_manifest$panel[[idx_item]],
    original_panel_title = old_title,
    new_panel_title = main_manifest$panel_title[[idx_item]],
    reason = paste0(
      "V17 regrouped main figures to keep every main figure at <=3 panels, place the volcano plot alone, and group GO/KEGG/Hallmark together. Storyline group: ",
      storyline_map$storyline_group[[i]],
      ". Original figure was ",
      old_fig,
      "."
    ),
    source_copied_path = normalize_path(main_manifest$copied_path[[idx_item]]),
    stringsAsFactors = FALSE
  )
}

# Confirm final main figure structure.
expected_v17_counts <- data.frame(
  figure_id = paste0("Figure ", 1:10),
  expected_n = c(3L, 2L, 1L, 3L, 3L, 2L, 2L, 3L, 2L, 3L),
  stringsAsFactors = FALSE
)

actual_v17_counts <- as.data.frame(table(main_manifest$figure_id))
names(actual_v17_counts) <- c("figure_id", "actual_n")
actual_v17_counts$actual_n <- as.integer(actual_v17_counts$actual_n)

v17_count_check <- merge(
  expected_v17_counts,
  actual_v17_counts,
  by = "figure_id",
  all.x = TRUE
)

v17_count_check$actual_n[is.na(v17_count_check$actual_n)] <- 0L

bad_v17_counts <- v17_count_check[
  v17_count_check$expected_n != v17_count_check$actual_n,
  ,
  drop = FALSE
]

if (nrow(bad_v17_counts) > 0L) {
  print(bad_v17_counts)
  stop("V17 no-5-panel main figure panel count check failed.")
}

if (any(v17_count_check$actual_n > 3L)) {
  print(v17_count_check)
  stop("V17 要求每个 main figure 不超过 3 个 panel，但发现 >3。")
}


layout_policy_audit <- data.table::rbindlist(
  layout_policy_rows,
  fill = TRUE
)

atomic_write_csv(
  as.data.frame(layout_policy_audit),
  OUT_LAYOUT_POLICY_AUDIT
)



# ============================================================
# 6. V4 memory-safe source panel audit
# ============================================================

stamp("V17 aspect-safe memory-safe：跳过一次性预渲染 43 个 PDF，保持原始宽高比，每张 figure 内部顺序渲染。")

all_sources <- rbind(
  main_manifest,
  supp_manifest
)

render_audit_rows <- list()

# V4 不再对 43 个 source 进行全量预渲染。
# 这里只审计文件是否存在、大小、hash。
# 真正渲染会在每张 Figure 拼图时逐个 panel 完成，并立刻释放内存。
for (i in seq_len(nrow(all_sources))) {
  row <- all_sources[
    i,
    ,
    drop = FALSE
  ]

  render_audit_rows[[
    length(render_audit_rows) + 1L
  ]] <- data.frame(
    item_type = row$item_type[[1]],
    item_id = row$item_id[[1]],
    figure_id = row$figure_id[[1]],
    panel = row$panel[[1]],
    panel_title = row$panel_title[[1]],
    copied_path = normalize_path(row$copied_path[[1]]),
    rendered = NA,
    width_px = NA_integer_,
    height_px = NA_integer_,
    aspect_ratio = NA_real_,
    render_method = "deferred_per_figure_memory_safe",
    fallback_used = NA,
    png_error_before_fallback = NA_character_,
    preview_path = NA_character_,
    error = NA_character_,
    exists = file.exists(row$copied_path[[1]]),
    size_bytes = if (file.exists(row$copied_path[[1]])) file.info(row$copied_path[[1]])$size else NA_real_,
    sha256 = sha256_file(row$copied_path[[1]]),
    stringsAsFactors = FALSE
  )
}

render_audit <- data.table::rbindlist(
  render_audit_rows,
  fill = TRUE
)

atomic_write_csv(
  as.data.frame(render_audit),
  OUT_SOURCE_PANEL_AUDIT
)

bad_render <- render_audit[
  !exists |
    is.na(size_bytes) |
    size_bytes <= 0 |
    is.na(sha256) |
    !nzchar(sha256),
  ,
  drop = FALSE
]

if (nrow(bad_render) > 0L) {
  print(bad_render)
  stop("部分 source panel 文件不存在或 hash 无效，不能进行 10D 拼图。")
}

# ============================================================
# 7. 拼主图 Figure 1–5
# ============================================================

stamp("组装主图 Figure 1–5。")

assembly_rows <- list()
transformation_rows <- list()
figure_index_rows <- list()

main_figure_ids <- paste0(
  "Figure ",
  1:10
)

for (fig in main_figure_ids) {
  rows <- main_manifest[
    main_manifest$figure_id == fig,
    ,
    drop = FALSE
  ]

  rows <- rows[
    order(rows$panel),
    ,
    drop = FALSE
  ]

  expected_panels <- if (fig == "Figure 3") {
    1L
  } else if (fig %in% c("Figure 2", "Figure 6", "Figure 7", "Figure 9")) {
    2L
  } else {
    3L
  }

  current_main_layout_type <- if (fig == "Figure 3") {
    "one_panel"
  } else if (fig %in% c("Figure 2", "Figure 6", "Figure 7", "Figure 9")) {
    "two_panel"
  } else if (fig == "Figure 1") {
    "main_3_horizontal"
  } else {
    "main_3"
  }

  current_main_height_in <- if (fig == "Figure 3") {
    6.5
  } else if (fig %in% c("Figure 2", "Figure 6", "Figure 7", "Figure 9")) {
    4.9
  } else if (fig == "Figure 1") {
    4.2
  } else {
    7.0
  }

  current_main_layout_rows <- if (current_main_layout_type == "main_3_horizontal") {
    1
  } else if (current_main_layout_type == "one_panel") {
    1
  } else if (current_main_layout_type == "two_panel") {
    1
  } else if (current_main_layout_type == "main_3") {
    2
  } else {
    2
  }

  if (nrow(rows) != expected_panels) {
    stop(
      fig,
      " panel 数量不符合 V12 layout policy；期望 n = ",
      expected_panels,
      "，当前 n = ",
      nrow(rows)
    )
  }

  grobs <- vector(
    "list",
    nrow(rows)
  )

  for (i in seq_len(nrow(rows))) {
    slot_width_in <- if (
      current_main_layout_type == "one_panel"
    ) {
      MAIN_FIG_WIDTH_IN
    } else if (
      current_main_layout_type == "main_3_horizontal"
    ) {
      MAIN_FIG_WIDTH_IN / 3
    } else if (
      current_main_layout_type == "main_3" &&
        rows$panel[[i]] == "C"
    ) {
      MAIN_FIG_WIDTH_IN
    } else if (current_main_layout_type == "two_panel") {
      MAIN_FIG_WIDTH_IN / 2
    } else {
      MAIN_FIG_WIDTH_IN / 2
    }

    slot_height_in <- current_main_height_in / current_main_layout_rows

    main_slot_aspect <- slot_width_in / slot_height_in

    grobs[[i]] <- make_panel_grob(
      pdf_path = rows$copied_path[[i]],
      panel_letter = rows$panel[[i]],
      panel_title = rows$panel_title[[i]],
      dpi = PANEL_RENDER_DPI,
      slot_aspect = main_slot_aspect
    )

    transformation_rows[[
      length(transformation_rows) + 1L
    ]] <- data.frame(
      figure_id = fig,
      item_id = rows$item_id[[i]],
      panel = rows$panel[[i]],
      panel_title = rows$panel_title[[i]],
      source_copied_path = normalize_path(rows$copied_path[[i]]),
      transformation = "render_source_pdf_page1_to_raster_then_aspect_preserving_place_in_grid",
      render_dpi = PANEL_RENDER_DPI,
      source_data_modified = FALSE,
      axis_or_stat_modified = FALSE,
      panel_letter_added = TRUE,
      title_text_added = TRUE,
      stringsAsFactors = FALSE
    )
  }

  fig_num <- gsub(
    "[^0-9]",
    "",
    fig
  )

  out_pdf <- file.path(
    OUT_MAIN_DIR,
    paste0(
      "Figure_",
      fig_num,
      "_10D_V17_final_assembly.pdf"
    )
  )

  out_tiff <- file.path(
    OUT_MAIN_DIR,
    paste0(
      "Figure_",
      fig_num,
      "_10D_V17_final_assembly_600dpi.tiff"
    )
  )

  out <- export_assembled_figure(
    grobs = grobs,
    figure_title = paste0(
      fig,
      ". Final assembled multi-panel figure"
    ),
    out_pdf = out_pdf,
    out_tiff = out_tiff,
    width_in = MAIN_FIG_WIDTH_IN,
    height_in = current_main_height_in,
    layout_type = current_main_layout_type
  )

  assembly_rows[[
    length(assembly_rows) + 1L
  ]] <- data.frame(
    figure_type = "main",
    figure_id = fig,
    n_panels = nrow(rows),
    layout_type = current_main_layout_type,
    width_in = MAIN_FIG_WIDTH_IN,
    height_in = current_main_height_in,
    pdf_path = normalize_path(out$pdf),
    tiff_path = normalize_path(out$tiff),
    stringsAsFactors = FALSE
  )

  rm(grobs)
  invisible(gc())

  figure_index_rows[[
    length(figure_index_rows) + 1L
  ]] <- data.frame(
    figure_type = "main",
    figure_id = fig,
    pdf_path = normalize_path(out$pdf),
    tiff_path = normalize_path(out$tiff),
    pdf_exists = file.exists(out$pdf),
    tiff_exists = file.exists(out$tiff),
    pdf_size_bytes = if (file.exists(out$pdf)) {
      file.info(out$pdf)$size
    } else {
      NA_real_
    },
    tiff_size_bytes = if (file.exists(out$tiff)) {
      file.info(out$tiff)$size
    } else {
      NA_real_
    },
    pdf_sha256 = sha256_file(out$pdf),
    tiff_sha256 = sha256_file(out$tiff),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 8. 拼补图 Supplementary Figure 1–10
# ============================================================

stamp("组装补图 Supplementary Figure 1–10。")

supp_ids <- unique(
  supp_manifest$figure_id
)

supp_ids <- supp_ids[
  order(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        supp_ids
      )
    )
  )
]

for (fig in supp_ids) {
  rows <- supp_manifest[
    supp_manifest$figure_id == fig,
    ,
    drop = FALSE
  ]

  rows <- rows[
    order(rows$panel),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    next
  }

  # V8 fix:
  # current_supp_height_in must be defined before the panel loop,
  # because make_panel_grob() uses it to calculate slot_aspect.
  current_supp_height_in <- if (nrow(rows) == 1L) {
    7.2
  } else if (nrow(rows) == 2L) {
    5.4
  } else {
    SUPP_FIG_HEIGHT_IN
  }

  grobs <- vector(
    "list",
    nrow(rows)
  )

  for (i in seq_len(nrow(rows))) {
    supp_slot_aspect <- ifelse(
      nrow(rows) == 1L,
      SUPP_FIG_WIDTH_IN / current_supp_height_in,
      (SUPP_FIG_WIDTH_IN / 2) / current_supp_height_in
    )

    grobs[[i]] <- make_panel_grob(
      pdf_path = rows$copied_path[[i]],
      panel_letter = rows$panel[[i]],
      panel_title = rows$panel_title[[i]],
      dpi = PANEL_RENDER_DPI,
      slot_aspect = supp_slot_aspect
    )

    transformation_rows[[
      length(transformation_rows) + 1L
    ]] <- data.frame(
      figure_id = fig,
      item_id = rows$item_id[[i]],
      panel = rows$panel[[i]],
      panel_title = rows$panel_title[[i]],
      source_copied_path = normalize_path(rows$copied_path[[i]]),
      transformation = "render_source_pdf_page1_to_raster_then_aspect_preserving_place_in_grid",
      render_dpi = PANEL_RENDER_DPI,
      source_data_modified = FALSE,
      axis_or_stat_modified = FALSE,
      panel_letter_added = TRUE,
      title_text_added = TRUE,
      stringsAsFactors = FALSE
    )
  }

  fig_num <- gsub(
    "[^0-9]",
    "",
    fig
  )

  layout_type <- if (nrow(rows) == 1L) {
    "one_panel"
  } else if (nrow(rows) == 2L) {
    "two_panel"
  } else {
    "adaptive"
  }

  out_pdf <- file.path(
    OUT_SUPP_DIR,
    paste0(
      "Supplementary_Figure_",
      fig_num,
      "_10D_V17_final_assembly.pdf"
    )
  )

  out_tiff <- file.path(
    OUT_SUPP_DIR,
    paste0(
      "Supplementary_Figure_",
      fig_num,
      "_10D_V17_final_assembly_600dpi.tiff"
    )
  )

  out <- export_assembled_figure(
    grobs = grobs,
    figure_title = paste0(
      fig,
      ". Final assembled supplementary figure"
    ),
    out_pdf = out_pdf,
    out_tiff = out_tiff,
    width_in = SUPP_FIG_WIDTH_IN,
    height_in = current_supp_height_in,
    layout_type = layout_type
  )

  assembly_rows[[
    length(assembly_rows) + 1L
  ]] <- data.frame(
    figure_type = "supplementary",
    figure_id = fig,
    n_panels = nrow(rows),
    layout_type = layout_type,
    width_in = SUPP_FIG_WIDTH_IN,
    height_in = current_supp_height_in,
    pdf_path = normalize_path(out$pdf),
    tiff_path = normalize_path(out$tiff),
    stringsAsFactors = FALSE
  )

  rm(grobs)
  invisible(gc())

  figure_index_rows[[
    length(figure_index_rows) + 1L
  ]] <- data.frame(
    figure_type = "supplementary",
    figure_id = fig,
    pdf_path = normalize_path(out$pdf),
    tiff_path = normalize_path(out$tiff),
    pdf_exists = file.exists(out$pdf),
    tiff_exists = file.exists(out$tiff),
    pdf_size_bytes = if (file.exists(out$pdf)) {
      file.info(out$pdf)$size
    } else {
      NA_real_
    },
    tiff_size_bytes = if (file.exists(out$tiff)) {
      file.info(out$tiff)$size
    } else {
      NA_real_
    },
    pdf_sha256 = sha256_file(out$pdf),
    tiff_sha256 = sha256_file(out$tiff),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 9. 写出 assembly audit
# ============================================================

assembly_audit <- data.table::rbindlist(
  assembly_rows,
  fill = TRUE
)

transformation_audit <- data.table::rbindlist(
  transformation_rows,
  fill = TRUE
)

figure_index <- data.table::rbindlist(
  figure_index_rows,
  fill = TRUE
)

atomic_write_csv(
  as.data.frame(assembly_audit),
  OUT_ASSEMBLY_AUDIT
)

atomic_write_csv(
  as.data.frame(transformation_audit),
  OUT_TRANSFORMATION_AUDIT
)

atomic_write_csv(
  as.data.frame(figure_index),
  OUT_FIGURE_INDEX
)


# ============================================================
# 10. 输出验证
# ============================================================

stamp("验证 10D 输出。")

required_outputs <- c(
  OUT_INPUT_AUDIT,
  OUT_SOURCE_PANEL_AUDIT,
  OUT_ASSEMBLY_AUDIT,
  OUT_TRANSFORMATION_AUDIT,
  OUT_FIGURE_INDEX,
  OUT_LAYOUT_POLICY_AUDIT,
  OUT_REPORT,
  OUT_SESSION
)

# 报告和 session 会在下一步写出；这里先占位追加主/补图输出。
final_figure_files <- c(
  figure_index$pdf_path,
  figure_index$tiff_path
)

final_figure_files <- final_figure_files[
  !is.na(final_figure_files) &
    nzchar(final_figure_files)
]

# 先不写 verification，等 report/session 写完。


# ============================================================
# 11. 报告
# ============================================================

n_main <- sum(
  assembly_audit$figure_type == "main"
)

n_supp <- sum(
  assembly_audit$figure_type == "supplementary"
)

n_pdf <- sum(
  figure_index$pdf_exists == TRUE,
  na.rm = TRUE
)

n_tiff <- sum(
  figure_index$tiff_exists == TRUE,
  na.rm = TRUE
)

n_transforms <- nrow(
  transformation_audit
)


n_jpeg_fallback <- sum(
  render_audit$fallback_used == TRUE,
  na.rm = TRUE
)

report_lines <- c(
  "10D V17 final multi-panel figure assembly report",
  paste0(
    "Run time: ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ),
  "",
  "Input:",
  "10C final = V14_S7A_HALLMARK_BARPLOT_LOCK",
  normalize_path(DIR_10C_PACKAGE),
  "",
  paste0(
    "Main figures assembled: ",
    n_main
  ),
  paste0(
    "Supplementary figures assembled: ",
    n_supp
  ),
  paste0(
    "Final PDF files generated: ",
    n_pdf
  ),
  paste0(
    "Final TIFF files generated: ",
    n_tiff
  ),
  paste0(
    "Panel transformations recorded: ",
    n_transforms
  ),
  paste0(
    "PDF panels rendered by JPEG fallback: ",
    n_jpeg_fallback
  ),
  "",
  "Assembly settings:",
  paste0(
    "Panel render DPI: ",
    PANEL_RENDER_DPI
  ),
  paste0(
    "TIFF DPI: ",
    TIFF_DPI
  ),
  paste0(
    "Main figure size: ",
    MAIN_FIG_WIDTH_IN,
    " x ",
    MAIN_FIG_HEIGHT_IN,
    " inch"
  ),
  paste0(
    "Supplementary figure size: ",
    SUPP_FIG_WIDTH_IN,
    " x ",
    SUPP_FIG_HEIGHT_IN,
    " inch"
  ),
  paste0(
    "Base font family: ",
    BASE_FAMILY
  ),
  "",
  "V12 layout policy:",
  "Figure 1A workflow/framework panel was excluded from final assembled Figure 1 at user request.",
  "Figure 1 original B/C/D/E panels were relettered to A/B/C/D.",
  "Figure 5B displayed panel title was corrected to match the 10C V16 cluster-size source.",
  "Main figures were regrouped by storyline into ten figures with <=3 panels each.",
  "The volcano plot is exported as a standalone main figure.",
  "GO, KEGG, and Hallmark enrichment are grouped together in one main figure.",
  "All rendered panels are rendered primarily through JPEG to avoid PNG IDAT CRC errors and magenta/pink transparency artifacts.",
  "",
  "Claim boundary:",
  "10D performed layout assembly and panel-title correction only.",
  "No biological analysis was rerun.",
  "No axis value, point coordinate, legend value or statistical annotation was intentionally modified.",
  "Original source PDFs remain preserved in the 10C locked source package.",
  "",
  "Important note:",
  "V17 aspect-safe memory-safe run exports assembled PDF files only. TIFF export is intentionally disabled to avoid RStudio session abort.",
  "After PDF layout inspection passes, TIFF export should be run as a separate lightweight step.",
  "The automatically assembled PDF files are layout-ready manuscript figures.",
  "Before final journal submission, visually inspect all panels for text readability, cropping and panel-letter placement.",
  "",
  "Next step:",
  "10E_FINAL_TEXT_FIGURE_NUMERIC_CONSISTENCY_AUDIT"
)

atomic_write_text(
  report_lines,
  OUT_REPORT
)

atomic_write_text(
  capture.output(
    sessionInfo()
  ),
  OUT_SESSION
)

all_required_outputs <- c(
  OUT_INPUT_AUDIT,
  OUT_SOURCE_PANEL_AUDIT,
  OUT_ASSEMBLY_AUDIT,
  OUT_TRANSFORMATION_AUDIT,
  OUT_FIGURE_INDEX,
  OUT_REPORT,
  OUT_SESSION,
  final_figure_files
)

verification <- data.frame(
  file = all_required_outputs,
  exists = file.exists(all_required_outputs),
  size_bytes = ifelse(
    file.exists(all_required_outputs),
    file.info(all_required_outputs)$size,
    NA_real_
  ),
  sha256 = vapply(
    all_required_outputs,
    sha256_file,
    character(1)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  verification,
  OUT_OUTPUT_VERIFICATION
)

bad_outputs <- verification[
  !verification$exists |
    is.na(verification$size_bytes) |
    verification$size_bytes <= 0 |
    is.na(verification$sha256) |
    !nzchar(verification$sha256),
  ,
  drop = FALSE
]

if (nrow(bad_outputs) > 0L) {
  print(bad_outputs)
  stop("10D 输出验证失败。")
}


# ============================================================
# 12. 完成
# ============================================================

cat("\n============================================================\n")
cat("10D FINAL MULTI-PANEL FIGURE ASSEMBLY V17 NO-5-PANEL VOLCANO-SINGLE 运行结束\n")
cat("============================================================\n\n")

cat("Input 10C package：", normalize_path(DIR_10C_PACKAGE), "\n", sep = "")
cat("Main figures assembled：", n_main, "\n", sep = "")
cat("Supplementary figures assembled：", n_supp, "\n", sep = "")
cat("PDF generated：", n_pdf, "\n", sep = "")
cat("TIFF generated：", n_tiff, "  [V17 memory-safe: TIFF intentionally disabled]\n", sep = "")
cat("Panel transformations recorded：", n_transforms, "\n", sep = "")
cat("PDF panels rendered by JPEG fallback：", n_jpeg_fallback, "\n\n", sep = "")

cat("主图输出目录：\n")
cat(normalize_path(OUT_MAIN_DIR), "\n\n")

cat("补图输出目录：\n")
cat(normalize_path(OUT_SUPP_DIR), "\n\n")

cat("核心审计表：\n")
cat(OUT_SOURCE_PANEL_AUDIT, "\n")
cat(OUT_ASSEMBLY_AUDIT, "\n")
cat(OUT_TRANSFORMATION_AUDIT, "\n")
cat(OUT_FIGURE_INDEX, "\n")
cat(OUT_LAYOUT_POLICY_AUDIT, "\n")
cat(OUT_OUTPUT_VERIFICATION, "\n\n")

cat("✅ 10D final multi-panel figure assembly V17 no-5-panel-volcano-single 完成。\n")
cat("下一步进入 10E：正文-图-数字一致性终审。\n")
