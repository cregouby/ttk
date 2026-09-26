#' Extract critical points from scalar field
#'
#' @param mesh A mesh3d object with scalar field 'data'
#' @param vertex_scalar_field Name of the scalar field (default: "data")
#' @param compute_minima Logical, compute minima (default: TRUE)
#' @param compute_maxima Logical, compute maxima (default: TRUE)
#' @param compute_saddle_points Logical, compute saddle points (default: TRUE)
#'
#' @return A list with critical points coordinates and types
#' @export
#'
#' @examples
#' \dontrun{
#' mesh <- rgl::mesh3d(...)
#' mesh$data <- runif(length(mesh$vb[1,]))
#' cp <- ttk_critical_points(mesh)
#' }
ttk_critical_points <- function(mesh,
                                vertex_scalar_field = "data",
                                compute_minima = TRUE,
                                compute_maxima = TRUE,
                                compute_saddle_points = TRUE) {
  
  if (!inherits(mesh, "mesh3d")) {
    stop("Input must be a mesh3d object from package rgl")
  }
  
  result <- ttk_critical_points_cpp(
    mesh,
    vertex_scalar_field,
    compute_minima,
    compute_maxima,
    compute_saddle_points
  )
  
  # Add class for S3 methods
  class(result) <- c("ttk_critical_points", "list")
  
  return(result)
}