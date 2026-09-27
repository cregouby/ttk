library(testthat)

test_that("ttk_contour_tree rejects non-mesh3d input", {
  bad_input <- list(
    vb = matrix(runif(12), nrow = 3),
    it = matrix(c(1L, 2L, 3L), nrow = 3)
  )
  
  expect_error(
    ttk_contour_tree(bad_input),
    "must be a mesh3d object"
  )
})

test_that("ttk_contour_tree rejects mesh without scalar field", {

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4,
      3, 1, 4,
      1, 2, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  
  expect_error(
    ttk_contour_tree(mesh),
    "does not contain scalar field"
  )
})

test_that("ttk_contour_tree returns correct structure", {
  skip_if_not(
    is.loaded("_ttk_contour_tree_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 5,
      3, 4, 5,
      4, 1, 5),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0, 0, 0, 1)
  
  result <- ttk_contour_tree(mesh)
  
  expect_type(result, "list")
  expect_s3_class(result, "ttk_contour_tree")
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("n_nodes" %in% names(result))
  expect_true("n_edges" %in% names(result))
  expect_true("scalar_field" %in% names(result))
})

test_that("ttk_contour_tree returns valid node and edge data", {
  skip_if_not(
    is.loaded("_ttk_contour_tree_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.5, 0.5, 1)
  
  result <- ttk_contour_tree(mesh)
  
  # Should have at least some nodes and edges
  expect_true(result$n_nodes > 0)
  expect_true(result$n_edges >= 0)
  
  # Node IDs should be valid (1-based)
  if (result$n_nodes > 0) {
    expect_true(all(result$nodes$ids >= 1))
    expect_true(all(result$nodes$ids <= result$total_vertices))
    expect_true(all(is.finite(result$nodes$values)))
  }
  
  # Edge indices should be valid
  if (result$n_edges > 0) {
    expect_true(all(result$edges$source >= 1))
    expect_true(all(result$edges$target >= 1))
    expect_true(all(result$edges$source <= result$n_nodes))
    expect_true(all(result$edges$target <= result$n_nodes))
  }
})