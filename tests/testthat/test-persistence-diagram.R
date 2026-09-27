library(testthat)

test_that("ttk_persistence_diagram rejects non-mesh3d input", {
  bad_input <- list(
    vb = matrix(runif(12), nrow = 3),
    it = matrix(c(1L, 2L, 3L), nrow = 3)
  )
  
  expect_error(
    ttk_persistence_diagram(bad_input),
    "must be a mesh3d object"
  )
})

test_that("ttk_persistence_diagram rejects mesh without scalar field", {
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4,
      3, 1, 4,
      1, 2, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  
  expect_error(
    ttk_persistence_diagram(mesh),
    "does not contain scalar field"
  )
})

test_that("ttk_persistence_diagram rejects negative threshold", {
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.5, 0.5, 1)
  
  expect_error(
    ttk_persistence_diagram(mesh, persistence_threshold = -1),
    "persistence_threshold must be a non-negative number"
  )
})

test_that("ttk_persistence_diagram returns correct structure", {
  skip_if_not(
    is.loaded("_ttk_persistence_diagram_cpp", PACKAGE = "ttk"),
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
  
  result <- ttk_persistence_diagram(mesh)
  
  expect_type(result, "list")
  expect_s3_class(result, "ttk_persistence_diagram")
  expect_true("birth_id" %in% names(result))
  expect_true("death_id" %in% names(result))
  expect_true("birth_value" %in% names(result))
  expect_true("death_value" %in% names(result))
  expect_true("dimension" %in% names(result))
  expect_true("persistence" %in% names(result))
  expect_true("n_pairs" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true(is.data.frame(result$data))
})

test_that("ttk_persistence_diagram respects threshold", {
  skip_if_not(
    is.loaded("_ttk_persistence_diagram_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )
  
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.1, 0.2, 1)
  
  # With high threshold, should have fewer pairs
  result_low <- ttk_persistence_diagram(mesh, persistence_threshold = 0)
  result_high <- ttk_persistence_diagram(mesh, persistence_threshold = 0.5)
  
  expect_true(result_high$n_pairs <= result_low$n_pairs)
})

test_that("ttk_persistence_diagram returns valid persistence values", {
  skip_if_not(
    is.loaded("_ttk_persistence_diagram_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.5, 0.5, 1)
  
  result <- ttk_persistence_diagram(mesh)
  
  if (result$n_pairs > 0) {
    # All persistence values should be non-negative
    expect_true(all(result$persistence >= 0))
    # Birth values should be defined
    expect_true(all(is.finite(result$birth_value)))
    # Death values should be defined
    expect_true(all(is.finite(result$death_value)))
  }
})