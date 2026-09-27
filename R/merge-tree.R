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
  result <- .Call("merge_tree_cpp",
                  mesh,
                  vertex_scalar_field,
                  tree_type,
                  simplify_tree)
  
  return(result)
}