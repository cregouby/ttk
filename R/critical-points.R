#' Extract critical points from a scalar field on a mesh
#'
#' @param mesh A mesh3d object with a scalar field
#' @param vertex_scalar_field Name of the scalar field (default: "data")
#' @param compute_minima Logical, compute minima (default: TRUE)
#' @param compute_maxima Logical, compute maxima (default: TRUE)
#' @param compute_saddle_points Logical, compute saddle points (default: TRUE)
#' @return A list with critical points coordinates and types
#' @export
ttk_critical_points <- function(mesh,
                                vertex_scalar_field = "data",
                                compute_minima = TRUE,
                                compute_maxima = TRUE,
                                compute_saddle_points = TRUE) {
  
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
  
  # Call C++ implementation (no ttk_ prefix to avoid double prefix)
  result <- critical_points_cpp(
    mesh,
    vertex_scalar_field,
    compute_minima,
    compute_maxima,
    compute_saddle_points
  )
  
  class(result) <- c("ttk_critical_points", "list")
  
  return(result)
}