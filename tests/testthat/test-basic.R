test_that("metero package loads correctly", {
  # Verify that the package can be loaded
  expect_true("metero" %in% .packages())
  
  # Verify the human_amr dataset exists and has expected structure
  expect_true(exists("human_amr", where = asNamespace("metero")))
})

test_that("package data structure is valid", {
  # Load the data
  data(human_amr, envir = environment())
  
  # Test data structure
  expect_true(is.data.frame(human_amr))
  expect_true(nrow(human_amr) > 0)
  
  # Test that required columns exist
  required_cols <- c("study_id", "pathogen_name", "antibiotic_name", 
                    "sample_count", "resistant_count", "resistance_rate")
  for (col in required_cols) {
    expect_true(col %in% colnames(human_amr), 
               info = paste("Column", col, "should exist in human_amr"))
  }
}) 