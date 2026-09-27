# Script to download and install TTK pre-built binaries
# Usage: Rscript tools/inst_ttk/install_ttk.R

install_ttk <- function(ttk_version = "1.4.0", force = FALSE) {
  
  os <- detect_os()
  message("Detected OS: ", os$name, " ", os$version)
  
  url <- get_ttk_url(ttk_version, os)
  if (is.null(url)) {
    stop("No pre-built binaries available for your system.")
  }
  
  # Install into src/ttk
  ttk_dir <- file.path("src", "ttk")
  dir.create(ttk_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (dir.exists(file.path(ttk_dir, "include", "ttk")) && !force) {
    message("TTK ", ttk_version, " already installed at ", ttk_dir)
    return(invisible(ttk_dir))
  }
  
  message("Downloading TTK ", ttk_version, " from: ", url)
  temp_file <- tempfile(fileext = get_extension(url))
  
  tryCatch({
    download.file(url, temp_file, mode = "wb", quiet = FALSE)
  }, error = function(e) {
    stop("Failed to download TTK: ", e$message)
  })
  
  message("Extracting TTK to ", ttk_dir, "...")
  extract_ttk(temp_file, ttk_dir, os)
  
  # Verify installation
  possible_locations <- c(
    file.path(ttk_dir, "include", "ttk", "Triangulation.h"),
    file.path(ttk_dir, "include", "ttk", "base", "Triangulation.h")
  )
  
  found <- FALSE
  for (loc in possible_locations) {
    if (file.exists(loc)) {
      message("Found Triangulation.h at: ", loc)
      found <- TRUE
      break
    }
  }
  
  if (!found) {
    message("Warning: Triangulation.h not found in expected locations")
    message("Listing contents of ", ttk_dir, ":")
    files <- list.files(ttk_dir, recursive = TRUE, full.names = FALSE)
    message("  ", paste(head(files, 15), collapse = "\n  "))
  }
  
  message("TTK ", ttk_version, " installed at ", ttk_dir)
  unlink(temp_file)
  
  return(invisible(ttk_dir))
}

detect_os <- function() {
  sysname <- Sys.info()["sysname"]
  
  if (sysname == "Linux") {
    if (file.exists("/etc/os-release")) {
      os_release <- readLines("/etc/os-release")
      id_line <- grep("^ID=", os_release, value = TRUE)
      version_line <- grep("^VERSION_ID=", os_release, value = TRUE)
      
      if (length(id_line) > 0 && grepl("ubuntu", id_line, ignore.case = TRUE)) {
        version <- gsub("VERSION_ID=\"(.+)\"", "\\1", version_line)
        return(list(name = "ubuntu", version = version, type = "linux"))
      }
    }
    return(list(name = "linux", version = "unknown", type = "linux"))
    
  } else if (sysname == "Darwin") {
    macos_version <- system("sw_vers -productVersion", intern = TRUE)
    major_version <- as.numeric(strsplit(macos_version, "\\.")[[1]][1])
    return(list(name = "macos", version = as.character(major_version), type = "macos"))
    
  } else if (sysname == "Windows") {
    return(list(name = "windows", version = "10", type = "windows"))
    
  } else {
    return(list(name = "unknown", version = "unknown", type = "unknown"))
  }
}

get_ttk_url <- function(version, os) {
  base_url <- "https://github.com/topology-tool-kit/ttk/releases/download"
  
  if (os$type == "linux" && os$name == "ubuntu") {
    ubuntu_versions <- c("20.04", "22.04", "24.04", "26.04")
    if (os$version %in% ubuntu_versions) {
      return(paste0(base_url, "/", version,
                    "/ttk-", version, "-ubuntu-", os$version, ".deb"))
    }
  } else if (os$type == "macos") {
    macos_versions <- c("14", "15", "26")
    if (os$version %in% macos_versions) {
      return(paste0(base_url, "/", version,
                    "/ttk-", version, "-macos-", os$version, ".tar.gz"))
    }
  } else if (os$type == "windows") {
    return(paste0(base_url, "/", version, "/ttk-", version, ".exe"))
  }
  
  return(NULL)
}

get_extension <- function(url) {
  if (grepl("\\.tar\\.gz$", url)) return(".tar.gz")
  if (grepl("\\.deb$", url)) return(".deb")
  if (grepl("\\.exe$", url)) return(".exe")
  return(".bin")
}

extract_ttk <- function(file, dest, os) {
  
  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }
  
  abs_dest <- normalizePath(dest, mustWork = FALSE)
  abs_file <- normalizePath(file, mustWork = TRUE)
  
  if (os$type == "linux" && grepl("\\.deb$", file)) {
    message("Extracting .deb package using dpkg-deb...")
    
    if (!nzchar(Sys.which("dpkg-deb"))) {
      stop("dpkg-deb not found. Install with: sudo apt-get install dpkg")
    }
    
    temp_extract <- tempfile(pattern = "ttk_extract_")
    dir.create(temp_extract)
    
    exit_code <- system2("dpkg-deb",
                         c("-x", shQuote(abs_file), shQuote(temp_extract)),
                         stdout = "", stderr = "")
    
    if (exit_code != 0) {
      stop("dpkg-deb extraction failed with exit code ", exit_code)
    }
    
    message("Searching for TTK files in extracted package...")
    
    # Find TTK within the extracted package
    possible_paths <- c(
      file.path(temp_extract, "usr", "local"),
      file.path(temp_extract, "usr"),
      temp_extract
    )
    
    ttk_source <- NULL
    for (path in possible_paths) {
      if (dir.exists(file.path(path, "include", "ttk"))) {
        ttk_source <- path
        message("Found TTK at: ", path)
        break
      }
    }
    
    if (is.null(ttk_source)) {
      # Try to find include/ttk anywhere
      result <- system2("find",
                        c(shQuote(temp_extract), "-type", "d", "-name", "ttk"),
                        stdout = TRUE)
      if (length(result) > 0) {
        ttk_include <- result[1]
        ttk_source <- dirname(dirname(ttk_include))
        message("Found TTK include at: ", ttk_include)
      } else {
        stop("Could not find TTK include directory in extracted package")
      }
    }
    
    message("Copying TTK files to ", abs_dest)
    
    # Copy include directory
    src_include <- file.path(ttk_source, "include")
    if (dir.exists(src_include)) {
      exit_code <- system2("cp",
                           c("-r", shQuote(src_include), shQuote(abs_dest)),
                           stdout = "", stderr = "")
      if (exit_code != 0) {
        stop("Failed to copy include directory")
      }
    }
    
    # Copy lib directory
    src_lib <- file.path(ttk_source, "lib")
    if (dir.exists(src_lib)) {
      exit_code <- system2("cp",
                           c("-r", shQuote(src_lib), shQuote(abs_dest)),
                           stdout = "", stderr = "")
      if (exit_code != 0) {
        stop("Failed to copy lib directory")
      }
    }
    
    # Copy share directory if exists
    src_share <- file.path(ttk_source, "share")
    if (dir.exists(src_share)) {
      system2("cp", c("-r", shQuote(src_share), shQuote(abs_dest)),
              stdout = "", stderr = "")
    }
    
    unlink(temp_extract, recursive = TRUE)
    
  } else if (os$type == "macos" && grepl("\\.tar\\.gz$", file)) {
    exit_code <- system2("tar", c("-xzf", shQuote(abs_file), "-C", shQuote(abs_dest)),
                         stdout = "", stderr = "")
    if (exit_code != 0) {
      stop("Failed to extract .tar.gz package")
    }
    
  } else if (os$type == "windows" && grepl("\\.exe$", file)) {
    if (nzchar(Sys.which("7z"))) {
      exit_code <- system2("7z", c("x", shQuote(abs_file),
                                   paste0("-o", shQuote(abs_dest))),
                           stdout = "", stderr = "")
      if (exit_code != 0) {
        stop("Failed to extract .exe with 7z")
      }
    }
  }
  
  if (!dir.exists(file.path(abs_dest, "include", "ttk"))) {
    warning("TTK include directory not found after extraction")
  }
  
  message("TTK extraction completed")
}

# Run if called directly
if (sys.nframe() == 0) {
  install_ttk(force = TRUE)
}