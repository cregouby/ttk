#' ttk: R Interface to the Topology Toolkit
#'
#' The ttk package provides R bindings to the Topology Toolkit (TTK) C++ library
#' for topological data analysis on 3D meshes. It wraps core TTK algorithms
#' including critical points, persistence diagrams, contour trees, merge trees,
#' and Morse-Smale complexes as R functions operating on mesh3d objects.
#'
#' @useDynLib ttk, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @keywords internal
"_PACKAGE"