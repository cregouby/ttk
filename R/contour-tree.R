#' Compute contour tree from a scalar field on a mesh
#'
#' @param mesh A mesh3d object with a scalar field
#' @param vertex_scalar_field Name of the scalar field (default: "data")
#' @return A list with contour tree nodes and edges
#' @export
ttk_contour_tree <- function(mesh, vertex_scalar_field = "data") {
  
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
  
  result <- contour_tree_cpp(mesh, vertex_scalar_field)
  
  class(result) <- c("ttk_contour_tree", "list")
  
  result
}

#' Print method for contour tree
#'
#' @param x A ttk_contour_tree object
#' @param ... Additional arguments, currently ignored
#' @export
print.ttk_contour_tree <- function(x, ...) {
  cat("TTK contour tree\n")
  cat("  scalar field:", x$scalar_field, "\n")
  cat("  vertices:", x$total_vertices, "\n")
  cat("  triangles:", x$total_triangles, "\n")
  cat("  nodes:", x$n_nodes, "\n")
  cat("  edges:", x$n_edges, "\n")
  invisible(x)
}