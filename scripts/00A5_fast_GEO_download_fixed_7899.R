
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
