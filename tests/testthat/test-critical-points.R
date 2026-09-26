# tests/testthat/test-critical-points.R
# Unit tests for the ttk_critical_points function
# All comments are in English for international collaboration

library(testthat)

test_that("ttk_critical_points rejects non-mesh3d input", {
  # Passing a plain list should raise an error
  
  bad_input <- list(
    vb = matrix(runif(12), nrow = 3),
    it = matrix(c(1L, 2L, 3L), nrow = 3)
  )
  
  expect_error(
    ttk_critical_points(bad_input),
    "must be a mesh3d object"
  )
})

test_that("ttk_critical_points rejects NULL input", {
  expect_error(
    ttk_critical_points(NULL),
    "must be a mesh3d object"
  )
})

test_that("ttk_critical_points rejects mesh without scalar field", {
  # Build a valid mesh but remove the scalar field
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4,
      3, 1, 4,
      1, 2, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  
  # No scalar field named "data" on this mesh
  
  expect_error(
    ttk_critical_points(mesh),
    "does not contain scalar field"
  )
})

test_that("ttk_critical_points rejects mismatched scalar field length", {
  # Scalar field length must equal the number of vertices
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  
  # Only 2 values for 4 vertices
  
  mesh$data <- c(0, 1)
  
  expect_error(
    ttk_critical_points(mesh),
    "Scalar field length does not match"
  )
})

test_that("ttk_critical_points returns correct structure", {
  # This test requires TTK to be installed and compiled
  # It will be skipped automatically if the shared library is not available
  
  skip_if_not(
    is.loaded("ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  # Build a simple pyramid with a single peak at the top vertex
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      1, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
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
  
  result <- ttk_critical_points(mesh)
  
  # Verify the top level is a list
  
  expect_type(result, "list")
  
  # Verify the expected component names
  
  expect_true("minima" %in% names(result))
  expect_true("maxima" %in% names(result))
  expect_true("saddles" %in% names(result))
  expect_true("total_vertices" %in% names(result))
  expect_true("total_triangles" %in% names(result))
  expect_true("scalar_field" %in% names(result))
})

test_that("ttk_critical_points returns correct metadata", {
  skip_if_not(
    is.loaded("ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4,
      3, 1, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.5, 0.5, 1)
  
  result <- ttk_critical_points(mesh)
  
  # Check vertex and triangle counts
  
  expect_equal(result$total_vertices, 4)
  expect_equal(result$total_triangles, 3)
  expect_equal(result$scalar_field, "data")
})

test_that("ttk_critical_points respects compute flags", {
  skip_if_not(
    is.loaded("ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.2, 0.3, 1)
  
  # Request only maxima
  
  result <- ttk_critical_points(
    mesh,
    compute_minima = FALSE,
    compute_maxima = TRUE,
    compute_saddle_points = FALSE
  )
  
  # Minima and saddles should be empty
  
  expect_equal(length(result$minima$vertex_ids), 0)
  expect_equal(length(result$saddles$vertex_ids), 0)
})

test_that("ttk_critical_points handles custom scalar field name", {
  skip_if_not(
    is.loaded("ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  
  # Use a non-default scalar field name
  
  mesh$elevation <- c(0, 0.5, 0.5, 1)
  
  result <- ttk_critical_points(
    mesh,
    vertex_scalar_field = "elevation"
  )
  
  expect_type(result, "list")
  expect_equal(result$scalar_field, "elevation")
})

test_that("ttk_critical_points handles negative scalar values", {
  skip_if_not(
    is.loaded("ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  
  # Negative values should be handled correctly
  
  mesh$data <- c(-1, -0.5, -0.5, 0)
  
  result <- ttk_critical_points(mesh)
  
  expect_type(result, "list")
  expect_true(result$total_vertices == 4)
})

test_that("ttk_critical_points rejects empty mesh", {
  skip_if_not(
    is.loaded("ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  # A mesh with zero vertices should fail gracefully
  
  vertices <- matrix(numeric(0), ncol = 3)
  triangles <- matrix(integer(0), ncol = 3)
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- numeric(0)
  
  expect_error(
    ttk_critical_points(mesh)
  )
})

test_that("ttk_critical_points result class is correct", {
  skip_if_not(
    is.loaded("ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0.5, 0.5, 1),
    ncol = 3, byrow = TRUE
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.5, 0.5, 1)
  
  result <- ttk_critical_points(mesh)
  
  # The result should carry a custom class for S3 dispatch
  
  expect_s3_class(result, "ttk_critical_points")
})