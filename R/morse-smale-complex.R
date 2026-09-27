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
  result <- .Call("morse_smale_complex_cpp",
                  mesh,
                  vertex_scalar_field,
                  simplify_threshold)
  
  return(result)
}