context("South Asia Visualization Tests")

# Setup test data
setup_south_asia_test_data <- function() {
  # Create a small test dataset
  test_data <- data.frame(
    year = rep(2020, 4),
    region = rep("South Asia", 4),
    country = c("India", "Pakistan", "Bangladesh", "Nepal"),
    pathogen = rep("Ecoli", 4),
    antibiotic_name = rep("Ciprofloxacin", 4),
    resistance_rate = c(0.58, 0.55, 0.60, 0.52),
    sample_count = c(200, 180, 150, 120),
    resistant_count = c(116, 99, 90, 62)
  )
  
  return(test_data)
}

# Test that the function runs without errors
test_that("create_south_asia_map works with valid data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("maps")
  
  # Setup test data
  test_data <- setup_south_asia_test_data()
  
  # Test with minimal parameters
  expect_error(
    create_south_asia_map(test_data),
    NA  # Expect no error
  )
  
  # Test with pathogen and antibiotic specified
  expect_error(
    create_south_asia_map(
      test_data,
      pathogen = "Ecoli",
      antibiotic = "Ciprofloxacin"
    ),
    NA  # Expect no error
  )
  
  # Test with color palette specified
  expect_error(
    create_south_asia_map(
      test_data,
      color_palette = "red_blue"
    ),
    NA  # Expect no error
  )
})

# Test error conditions
test_that("create_south_asia_map handles invalid data correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("maps")
  
  # Empty data frame
  empty_data <- data.frame()
  expect_error(create_south_asia_map(empty_data))
  
  # Data with no South Asian countries
  non_sa_data <- data.frame(
    country = c("USA", "France", "Germany"),
    resistance_rate = c(0.1, 0.2, 0.3)
  )
  expect_error(create_south_asia_map(non_sa_data))
  
  # Data with South Asian countries but missing required columns
  missing_cols_data <- data.frame(
    country = c("India", "Pakistan"),
    some_other_column = c(1, 2)
  )
  expect_error(create_south_asia_map(missing_cols_data))
  
  # Test with non-existent pathogen
  test_data <- setup_south_asia_test_data()
  expect_error(
    create_south_asia_map(
      test_data,
      pathogen = "NonExistentPathogen"
    )
  )
})

# Test that the output is a ggplot object
test_that("create_south_asia_map returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("maps")
  
  test_data <- setup_south_asia_test_data()
  
  result <- create_south_asia_map(test_data)
  expect_s3_class(result, "ggplot")
  
  # Check that some expected elements exist in the plot
  expect_true("title" %in% names(result$labels))
  expect_true("subtitle" %in% names(result$labels))
  
  # When using specific title
  custom_title <- "Custom Test Title"
  result_with_title <- create_south_asia_map(test_data, title = custom_title)
  expect_equal(result_with_title$labels$title, custom_title)
}) 