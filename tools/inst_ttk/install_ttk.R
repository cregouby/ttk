install_ttk <- function(ttk_version = "1.4.0", force = FALSE) {
  
  os <- detect_os()
  message("Detected OS: ", os$name, " ", os$version)
  
  url <- get_ttk_url(ttk_version, os)
  if (is.null(url)) {
    stop("No pre-built binaries available for your system.")
  }
  
  # KEY CHANGE: install into src/ttk instead of inst/ttk
  ttk_dir <- file.path("src", "ttk")
  dir.create(ttk_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (dir.exists(file.path(ttk_dir, "include", "ttk")) && !force) {
    message("TTK ", ttk_version, " already installed at ", ttk_dir)
    return(invisible(ttk_dir))
  }
  
  message("Downloading TTK ", ttk_version, "...")
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
    message("Contents of ", ttk_dir, "/include/ttk/:")
    files <- list.files(file.path(ttk_dir, "include", "ttk"), recursive = TRUE)
    message("  ", paste(head(files, 15), collapse = "\n  "))
    stop("TTK extraction failed or files not found")
  }
  
  message("TTK ", ttk_version, " installed successfully at ", ttk_dir)
  unlink(temp_file)
  
  return(invisible(ttk_dir))
}

# ... detect_os, get_ttk_url, get_extension restent identiques ...

extract_ttk <- function(file, dest, os) {
  
  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }
  
  abs_dest <- normalizePath(dest, mustWork = FALSE)
  abs_file <- normalizePath(file, mustWork = TRUE)
  
  if (os$type == "linux" && grepl("\\.deb$", file)) {
    message("Extracting .deb package without installing dependencies...")
    
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
    
    message("Extraction completed. Searching for TTK files...")
    
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
      stop("Could not find TTK include directory in extracted package")
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
    stop("TTK include directory not found after extraction")
  }
  
  message("TTK extraction completed successfully")
}

if (sys.nframe() == 0) {
  install_ttk()
}