testthat::test_that("parse_cruise_id correctly parses FlowCam patterns", {
  # Sample input data
  ecotaxa_file <- data.frame(
    object_id = c(
      "10414_0000_01_1_20x_d_00080",  # Matches flowcam_pattern
      "10423_0800_22_1_20x_2_d_00116" # Matches flowcam_pattern2
    ),
    stringsAsFactors = FALSE
  )

  # Call the function
  result <- parse_cruise_id(ecotaxa_file)

  # Check the output
  testthat::expect_equal(result$pattern, "flowcam")
  testthat::expect_equal(ncol(result$parsed_file), 8) # Ensure parsed columns are added
  testthat::expect_true(all(c("cruise", "depth", "niskin", "mode", "magnification") %in% colnames(result$parsed_file)))
})

testthat::test_that("parse_cruise_id returns NULL for unmatched patterns", {
  # Sample input data with no matching patterns
  ecotaxa_file <- data.frame(
    object_id = c("unmatched_pattern_12345"),
    stringsAsFactors = FALSE
  )

  # Call the function
  result <- parse_cruise_id(ecotaxa_file)

  # Check the output
  testthat::expect_null(result)
})