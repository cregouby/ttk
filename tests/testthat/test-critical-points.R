vertices <- matrix(
  c(0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    0.5, 0.5, 1),
  ncol = 3, byrow = TRUE
)


test_that("ttk_critical_points rejects non-mesh3d input", {
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
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4,
      3, 1, 4,
      1, 2, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  
  expect_error(
    ttk_critical_points(mesh),
    "does not contain scalar field"
  )
})

test_that("ttk_critical_points rejects mismatched scalar field length", {
  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 1)
  
  expect_error(
    ttk_critical_points(mesh),
    "Scalar field length does not match"
  )
})

test_that("ttk_critical_points returns correct structure", {
  skip_if_not(
    is.loaded("_ttk_ttk_critical_points_cpp", PACKAGE = "ttk"),
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
  
  result <- ttk_critical_points(mesh)
  
  expect_type(result, "list")
  expect_true("minima" %in% names(result))
  expect_true("maxima" %in% names(result))
  expect_true("saddles" %in% names(result))
  expect_true("total_vertices" %in% names(result))
  expect_true("total_triangles" %in% names(result))
  expect_true("scalar_field" %in% names(result))
})

test_that("ttk_critical_points returns correct metadata", {
  skip_if_not(
    is.loaded("_ttk_ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
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
  
  expect_equal(result$total_vertices, 4)
  expect_equal(result$total_triangles, 3)
  expect_equal(result$scalar_field, "data")
})

test_that("ttk_critical_points respects compute flags", {
  skip_if_not(
    is.loaded("_ttk_ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.2, 0.3, 1)
  
  result <- ttk_critical_points(
    mesh,
    compute_minima = FALSE,
    compute_maxima = TRUE,
    compute_saddle_points = FALSE
  )
  
  expect_equal(length(result$minima$vertex_ids), 0)
  expect_equal(length(result$saddles$vertex_ids), 0)
})

test_that("ttk_critical_points handles custom scalar field name", {
  skip_if_not(
    is.loaded("_ttk_ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
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
    is.loaded("_ttk_ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(-1, -0.5, -0.5, 0)
  
  result <- ttk_critical_points(mesh)
  
  expect_type(result, "list")
  expect_true(result$total_vertices == 4)
})

test_that("ttk_critical_points result class is correct", {
  skip_if_not(
    is.loaded("_ttk_ttk_critical_points_cpp", PACKAGE = "ttk"),
    message = "TTK shared library not loaded"
  )

  triangles <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  
  mesh <- rgl::mesh3d(t(vertices), triangles = t(triangles))
  mesh$data <- c(0, 0.5, 0.5, 1)
  
  result <- ttk_critical_points(mesh)
  
  expect_s3_class(result, "ttk_critical_points")
})