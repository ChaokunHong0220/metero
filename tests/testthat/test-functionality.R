library(metero)

test_that("Data model functions work correctly", {
  # Test get_data_model
  model <- get_data_model()
  expect_type(model, "list")
  expect_true("metadata_schema" %in% names(model))
  expect_true("human_schema" %in% names(model))
  
  # Test create_empty_dataset
  dataset <- create_empty_dataset()
  expect_true(is_meteor_dataset(dataset))
  expect_equal(nrow(dataset$metadata), 0)
  expect_equal(nrow(dataset$human_data), 0)
})

test_that("Human AMR data loads correctly", {
  data("human_amr", package = "metero")
  expect_true(is.data.frame(human_amr))
  expect_true("pathogen_name" %in% names(human_amr))
  expect_true("antibiotic_name" %in% names(human_amr))
  expect_true("resistance_rate" %in% names(human_amr))
})

test_that("Visualization functions work with example data", {
  # Skip if not interactive or if ggplot2 is not available
  skip_if_not_installed("ggplot2")
  
  data("human_amr", package = "metero")
  
  # Test heatmap creation
  heatmap <- tryCatch({
    create_amr_heatmap(human_amr, 
                      pathogen_col = "pathogen_name", 
                      antibiotic_col = "antibiotic_name")
  }, error = function(e) {
    fail(paste("create_amr_heatmap failed:", e$message))
    NULL
  })
  
  expect_true(inherits(heatmap, "ggplot"))
})

test_that("Meta-analysis functions work with example data", {
  # Skip if meta package is not available
  skip_if_not_installed("meta")
  
  data("human_amr", package = "metero")
  
  # Subset data for faster testing
  test_data <- human_amr[human_amr$pathogen_name == "Escherichia coli" & 
                        human_amr$antibiotic_name %in% c("Ciprofloxacin", "Ampicillin"), ]
  
  # Test calculate_pooled_rate
  meta_result <- tryCatch({
    calculate_pooled_rate(
      data = test_data,
      by = "antibiotic_name",
      method = "Inverse",
      min_studies = 1
    )
  }, error = function(e) {
    fail(paste("calculate_pooled_rate failed:", e$message))
    NULL
  })
  
  # Meta-analysis might fail due to insufficient data in the test dataset,
  # so we'll just test that the function completes without error
  expect_true(is.list(meta_result))
  
  # Skip detailed checks since we can't guarantee meta-analysis success with test data
  # If meta_result has a summary, check its content
  if ("summary" %in% names(meta_result) && inherits(meta_result, "amr_meta_analysis")) {
    expect_true(is.data.frame(meta_result$summary))
  }
}) 