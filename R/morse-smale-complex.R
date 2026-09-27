#' Compute Morse-Smale complex
#'
#' @param mesh A mesh3d object with scalar field
#' @param vertex_scalar_field Name of the scalar field
#' @param simplify_threshold Simplification threshold (default: 0.0)
#'
#' @return A list with cells, separatrices, and critical points
#' @export
ttk_morse_smale_complex <- function(mesh,
                                    vertex_scalar_field = "data",
                                    simplify_threshold = 0.0) {
  if (!inherits(mesh, "mesh3d")) {
    stop("Input must be a mesh3d object from package rgl")
  }
  
  if (!vertex_scalar_field %in% names(mesh)) {
    stop("Mesh does not contain scalar field: ", vertex_scalar_field)
  }
  
  n_vertices <- ncol(mesh$vb)
  scalar_data <- mesh[[vertex_scalar_field]]
  
  if (length(scalar_data) != n_vertices) {
    stop(
      "Scalar field length does not match vertex count. ",
      "Expected ", n_vertices, " but got ", length(scalar_data)
    )
  }
  
  result <- morse_smale_complex_cpp(
    mesh,
    vertex_scalar_field,
    simplify_threshold
  )
  
  class(result) <- c("ttk_morse_smale_complex", "list")
  result
}