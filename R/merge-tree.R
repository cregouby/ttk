#' Compute merge tree
#'
#' @param mesh A mesh3d object with scalar field
#' @param vertex_scalar_field Name of the scalar field
#' @param tree_type Type of merge tree ("join" or "split", default: "join")
#' @param simplify_tree Logical, simplify the tree (default: FALSE)
#'
#' @return A list with tree structure information
#' @export
ttk_merge_tree <- function(mesh,
                           vertex_scalar_field = "data",
                           tree_type = "join",
                           simplify_tree = FALSE) {
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
  
  result <- merge_tree_cpp(
    mesh,
    vertex_scalar_field,
    tree_type,
    simplify_tree
  )
  
  class(result) <- c("ttk_merge_tree", "list")
  result
}