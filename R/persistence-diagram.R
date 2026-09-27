#' Compute persistence diagram from a scalar field on a mesh
#'
#' @param mesh A mesh3d object with a scalar field
#' @param vertex_scalar_field Name of the scalar field (default: "data")
#' @param persistence_threshold Minimum persistence value to keep (default: 0)
#' @return A list with persistence pairs information
#' @export
ttk_persistence_diagram <- function(mesh,
                                    vertex_scalar_field = "data",
                                    persistence_threshold = 0) {
  
  # Validate input type
  if (!inherits(mesh, "mesh3d")) {
    stop("Input must be a mesh3d object from package rgl")
  }
  
  # Validate scalar field exists
  if (!vertex_scalar_field %in% names(mesh)) {
    stop("Mesh does not contain scalar field: ", vertex_scalar_field)
  }
  
  # Validate scalar field length matches vertex count
  n_vertices <- ncol(mesh$vb)
  scalar_data <- mesh[[vertex_scalar_field]]
  if (length(scalar_data) != n_vertices) {
    stop("Scalar field length does not match vertex count. ",
         "Expected ", n_vertices, " but got ", length(scalar_data))
  }
  
  # Validate threshold
  if (!is.numeric(persistence_threshold) || persistence_threshold < 0) {
    stop("persistence_threshold must be a non-negative number")
  }
  
  # Call C++ implementation
  result <- persistence_diagram_cpp(
    mesh,
    vertex_scalar_field,
    persistence_threshold
  )
  
  # Convert to data frame for convenience
  result_df <- data.frame(
    birth_id = result$birth_id,
    death_id = result$death_id,
    birth_value = result$birth_value,
    death_value = result$death_value,
    dimension = result$dimension,
    persistence = result$persistence
  )
  
  # Add class for S3 dispatch
  class(result) <- c("ttk_persistence_diagram", "list")
  result$data <- result_df
  
  return(result)
}

#' Plot method for persistence diagram
#'
#' @param x A ttk_persistence_diagram object
#' @param ... Additional arguments passed to plot
#' @export
plot.ttk_persistence_diagram <- function(x, ...) {
  if (x$n_pairs == 0) {
    message("No persistence pairs to plot")
    return(invisible(NULL))
  }
  
  plot(x$data$birth_value, x$data$death_value,
       xlab = "Birth value",
       ylab = "Death value",
       main = "Persistence Diagram",
       pch = 19,
       col = rgb(0, 0, 1, 0.5),
       ...)
  
  # Add diagonal line (persistence = 0)
  range_val <- range(c(x$data$birth_value, x$data$death_value))
  lines(range_val, range_val, lty = 2, col = "gray")
  
  invisible(NULL)
}

#' Convert ttk_persistence_diagram to phutil persistence format
#'
#' @param x A ttk_persistence_diagram object
#' @param warn Logical, whether to warn about unordered pairs (default: TRUE)
#' @param ... Additional arguments (currently unused)
#' @return An object of class 'persistence' compatible with phutil
#' @export
#' @examples
#' \dontrun{
#' result <- ttk_persistence_diagram(mesh)
#' # Convert to phutil format
#' pd <- as_persistence(result)
#' # Use with tdaverse packages
#' library(phutil)
#' print(pd)
#' }
as_persistence.ttk_persistence_diagram <- function(x, warn = TRUE, ...) {
  
  if (!inherits(x, "ttk_persistence_diagram")) {
    stop("Input must be a ttk_persistence_diagram object")
  }
  
  # Check if phutil is available
  if (!requireNamespace("phutil", quietly = TRUE)) {
    stop("Package 'phutil' is required for this conversion. ",
         "Install it from GitHub: remotes::install_github('tdaverse/phutil')")
  }
  
  # Get data from ttk result
  data <- x$data
  
  if (nrow(data) == 0) {
    # Empty diagram
    pairs <- list()
  } else {
    # Split by dimension and convert to matrices
    dimensions <- unique(data$dimension)
    dimensions <- sort(dimensions[dimensions >= 0 & is.finite(dimensions)])
    
    pairs <- lapply(dimensions, function(d) {
      subset_data <- data[data$dimension == d, ]
      mat <- cbind(
        birth = subset_data$birth_value,
        death = subset_data$death_value
      )
      colnames(mat) <- c("birth", "death")
      
      # Check if pairs are ordered
      if (warn && any(mat[, 1] > mat[, 2])) {
        warning("Some pairs have birth > death in dimension ", d)
      }
      
      mat
    })
    
    names(pairs) <- as.character(dimensions)
    
    # Fill missing dimensions with empty matrices
    if (length(dimensions) > 0) {
      max_dim <- max(dimensions)
      all_dims <- 0:max_dim
      missing_dims <- setdiff(all_dims, dimensions)
      
      for (d in missing_dims) {
        pairs[[as.character(d)]] <- matrix(NA_real_, nrow = 0, ncol = 2)
      }
      
      # Reorder by dimension
      pairs <- pairs[order(as.integer(names(pairs)))]
    }
    
    pairs <- unname(pairs)
  }
  
  # Build metadata
  metadata <- list(
    ordered_pairs = if (nrow(data) > 0) all(data$birth_value <= data$death_value) else TRUE,
    data = "mesh3d object",
    engine = "ttk::ttk_persistence_diagram",
    filtration = "Sublevel set",
    parameters = list(
      scalar_field = x$scalar_field,
      persistence_threshold = x$persistence_threshold %||% 0
    ),
    call = match.call()
  )
  
  # Create persistence object
  result <- list(
    pairs = pairs,
    metadata = metadata
  )
  
  class(result) <- "persistence"
  
  return(result)
}

#' Convert ttk_persistence_diagram to TDA diagram format
#'
#' @param x A ttk_persistence_diagram object
#' @param list Logical, whether to wrap in a list (default: TRUE for TDA compatibility)
#' @param ... Additional arguments (currently unused)
#' @return An object of class 'diagram' compatible with TDA package
#' @export
as_diagram.ttk_persistence_diagram <- function(x, list = TRUE, ...) {
  
  # Convert to persistence first
  pd <- as_persistence.ttk_persistence_diagram(x, warn = FALSE)
  
  # Use phutil's conversion if available
  if (requireNamespace("phutil", quietly = TRUE)) {
    return(phutil::as_diagram(pd, list = list))
  }
  
  # Fallback: manual conversion to TDA format
  if (length(pd$pairs) == 0) {
    res <- matrix(NA_real_, nrow = 0, ncol = 3)
  } else {
    res <- mapply(
      function(.x, .i) cbind(.i, .x),
      pd$pairs,
      seq_along(pd$pairs) - 1L,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
    res <- do.call(rbind, res)
  }
  
  colnames(res) <- c("Dimension", "Birth", "Death")
  class(res) <- "diagram"
  attr(res, "maxdimension") <- if (nrow(res) > 0) max(res[, 1]) else 0
  attr(res, "scale") <- if (nrow(res) > 0) {
    finite_rows <- !apply(is.infinite(res[, c(2, 3)]), 1, any)
    range(res[finite_rows, c(2, 3)])
  } else {
    c(0, 1)
  }
  
  if (list) {
    res <- list(diagram = res)
  }
  
  return(res)
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x