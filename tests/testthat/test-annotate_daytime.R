testthat::test_that("annotate_daytime adds is_day column correctly", {
  # Sample input data
  eco_taxa_df <- data.frame(
    cruise = c("cruise1", "cruise1"),
    moc = c("moc1", "moc1"),
    object_lat = c(34.5, 34.5),
    object_lon = c(-120.5, -120.5),
    object_date = c("2025-03-26", "2025-03-26"),
    object_time = c("12:00:00", "23:00:00"),
    stringsAsFactors = FALSE
  )

  # Call the function
  result <- ecotaxaLoadR::annotate_daytime(eco_taxa_df)

  # Check that the output has the same number of rows
  testthat::expect_equal(nrow(result), nrow(eco_taxa_df))

  # Check that the `is_day` column is added
  testthat::expect_true("is_day" %in% colnames(result))

  # Check that `is_day` contains logical values
  testthat::expect_type(result$is_day, "logical")

})

testthat::test_that("annotate_daytime correctly identifies day and night", {
  # Sample input data with known day and night times
  eco_taxa_df <- data.frame(
    cruise = c("cruise1", "cruise1"),
    moc = c("moc1", "moc1"),
    object_lat = c(34.5, 34.5),
    object_lon = c(-120.5, -120.5),
    object_date = c("2025-03-26", "2025-03-26"),
    object_time = c("12:00:00", "23:00:00"), # noon (night) and 11 PM (day) - UTC!
    stringsAsFactors = FALSE
  )

  # Call the function
  result <- ecotaxaLoadR::annotate_daytime(eco_taxa_df)

  # Check that the `is_day` column correctly identifies day and night
  testthat::expect_equal(result$is_day, c(FALSE, TRUE))
})

testthat::test_that("annotate_daytime preserves the number of rows", {
  # Sample input data
  eco_taxa_df <- data.frame(
    cruise = c("cruise1", "cruise1"),
    moc = c("moc1", "moc1"),
    object_lat = c(34.5, 34.5),
    object_lon = c(-120.5, -120.5),
    object_date = c("2025-03-26", "2025-03-26"),
    object_time = c("12:00:00", "13:00:00"),
    stringsAsFactors = FALSE
  )

  # Call the function
  result <- ecotaxaLoadR::annotate_daytime(eco_taxa_df)

  # Check that the number of rows in the input and output match
  testthat::expect_equal(nrow(result), nrow(eco_taxa_df))
})

testthat::test_that("annotate_daytime handles empty input gracefully", {
  # Empty input data
  eco_taxa_df <- data.frame(
    cruise = character(0),
    moc = character(0),
    object_lat = numeric(0),
    object_lon = numeric(0),
    object_date = character(0),
    object_time = character(0),
    stringsAsFactors = FALSE
  )

  # Call the function
  result <- ecotaxaLoadR::annotate_daytime(eco_taxa_df)

  # Check that the output is also empty
  testthat::expect_equal(nrow(result), 0)
  testthat::expect_true("is_day" %in% colnames(result))
})