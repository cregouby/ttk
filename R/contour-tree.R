#' Compute contour tree
#'
#' @param mesh A mesh3d object with scalar field
#' @param vertex_scalar_field Name of the scalar field
#' @param simplify_tree Logical, simplify the tree (default: FALSE)
#'
#' @return A list with nodes, edges, and hierarchy information
#' @export
ttk_contour_tree <- function(mesh,
                             vertex_scalar_field = "data",
                             simplify_tree = FALSE) {
  result <- .Call("contour_tree_cpp",
                  mesh,
                  vertex_scalar_field,
                  simplify_tree)
  
  return(result)
}