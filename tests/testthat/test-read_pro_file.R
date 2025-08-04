testthat::test_that("load_pro_files works with test directory containing PRO files", {
  # Test with directory containing both PRO files
  test_dir <- testthat::test_path("test-data", "pro_files")

  testthat::expect_true(
    dir.exists(test_dir),
    info = "Test directory should exist"
  )

  result <- ecotaxaLoadR::load_pro_files(test_dir)

  # Check that result is a list
  testthat::expect_type(result, "list")

  # Check that we processed some files
  testthat::expect_gt(length(result), 0)

  # Check for required attributes
  testthat::expect_true(
    "failed_files" %in% names(attributes(result)),
    info = "Should have failed_files attribute"
  )
  testthat::expect_true(
    "error_messages" %in% names(attributes(result)),
    info = "Should have error_messages attribute"
  )
  testthat::expect_true(
    "processing_summary" %in% names(attributes(result)),
    info = "Should have processing_summary attribute"
  )

  # Each item in the list should be a data frame using purrr::map_lgl
  all_dataframes <- purrr::map_lgl(result, ~ inherits(.x, "data.frame"))
  testthat::expect_true(all(all_dataframes))
})

testthat::test_that("load_pro_files handles empty directory", {
  # Create a temporary directory with no PRO files
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_test_dir")
  dir.create(empty_dir, showWarnings = FALSE)

  testthat::expect_error(
    ecotaxaLoadR::load_pro_files(empty_dir),
    "DNF PRO files in this directory"
  )

  # Clean up
  unlink(empty_dir, recursive = TRUE)
})

testthat::test_that("ingest_pro_file works with MOC8 PRO file", {
  # Test with MOC8 file
  pro_file <- testthat::test_path("test-data", "pro_files", "MOC8_08A.PRO")

  testthat::expect_true(file.exists(pro_file))

  result <- ecotaxaLoadR::ingest_pro_file(pro_file)

  # Check that result is a data frame
  testthat::expect_s3_class(result, "data.frame")

  # Check that result has rows and columns
  testthat::expect_gt(nrow(result), 0)
  testthat::expect_gt(ncol(result), 0)

  # Check for expected columns that should be added by ingest_pro_file using purrr::map_lgl
  expected_added_cols <- c(
    "datetime_gmt",
    "datetime_local",
    "timezone",
    "file_name"
  )
  cols_present <- purrr::map_lgl(expected_added_cols, ~ .x %in% names(result))
  purrr::walk2(
    expected_added_cols,
    cols_present,
    ~ {
      testthat::expect_true(.y, info = paste("Should have", .x, "column"))
    }
  )

  # Check for original PRO file columns
  expected_pro_cols <- c("time", "pres", "temp", "sal")
  present_cols <- intersect(expected_pro_cols, names(result))
  testthat::expect_gt(length(present_cols), 0)

  # Check that datetime columns are POSIXct
  testthat::expect_s3_class(result$datetime_gmt, "POSIXct")
  testthat::expect_s3_class(result$datetime_local, "POSIXct")
})

testthat::test_that("ingest_pro_file works with MOC5 PRO file", {
  # Test with MOC5 file
  pro_file <- testthat::test_path("test-data", "pro_files", "MOC5_05A.PRO")

  testthat::expect_true(file.exists(pro_file))

  result <- ecotaxaLoadR::ingest_pro_file(pro_file)

  # Check that result is a data frame
  testthat::expect_s3_class(result, "data.frame")

  # Check that result has rows and columns
  testthat::expect_gt(nrow(result), 0)
  testthat::expect_gt(ncol(result), 0)

  # Check for expected added columns using purrr::walk
  expected_added_cols <- c(
    "datetime_gmt",
    "datetime_local",
    "timezone",
    "file_name"
  )
  purrr::walk(
    expected_added_cols,
    ~ {
      testthat::expect_true(
        .x %in% names(result),
        info = paste("Should have", .x, "column")
      )
    }
  )

  # Check that numeric columns are actually numeric using purrr::walk
  numeric_cols <- c("time", "pres", "temp", "sal")
  intersected_cols <- intersect(numeric_cols, names(result))
  purrr::walk(
    intersected_cols,
    ~ {
      testthat::expect_true(
        is.numeric(result[[.x]]),
        info = paste("Column", .x, "should be numeric")
      )
    }
  )
})

testthat::test_that("ingest_pro_file fails with non-existent file", {
  non_existent_file <- testthat::test_path(
    "test-data",
    "pro_files",
    "nonexistent.PRO"
  )

  testthat::expect_error(
    ecotaxaLoadR::ingest_pro_file(non_existent_file),
    info = "Should error with non-existent file"
  )
})

testthat::test_that("ingest_pro_file handles invalid file path", {
  invalid_paths <- c("", NULL)
  purrr::walk(
    invalid_paths,
    ~ {
      testthat::expect_error(
        ecotaxaLoadR::ingest_pro_file(.x),
        info = paste("Should error with invalid path:", deparse(.x))
      )
    }
  )
})

testthat::test_that("ingest_pro_file returns consistent structure across files", {
  pro_files <- c(
    moc8 = testthat::test_path("test-data", "pro_files", "MOC8_08A.PRO"),
    moc5 = testthat::test_path("test-data", "pro_files", "MOC5_05A.PRO")
  )

  results <- purrr::map(pro_files, ecotaxaLoadR::ingest_pro_file)

  # Both should be data frames using purrr::map_lgl
  are_dataframes <- purrr::map_lgl(results, ~ inherits(.x, "data.frame"))
  testthat::expect_true(all(are_dataframes))

  # Both should have data using purrr::map_int
  row_counts <- purrr::map_int(results, nrow)
  col_counts <- purrr::map_int(results, ncol)
  testthat::expect_true(all(row_counts > 0))
  testthat::expect_true(all(col_counts > 0))

  # Both should have the same added columns using purrr::map and purrr::every
  added_cols <- c("datetime_gmt", "datetime_local", "timezone", "file_name")
  has_all_cols <- purrr::map(results, function(result_df) {
    purrr::every(added_cols, function(col) col %in% names(result_df))
  })
  testthat::expect_true(all(unlist(has_all_cols)))
})

testthat::test_that("ingest_pro_file preserves data integrity", {
  pro_file <- testthat::test_path("test-data", "pro_files", "MOC5_05A.PRO")

  result <- ecotaxaLoadR::ingest_pro_file(pro_file)

  # Check that time column is monotonic (if present)
  if ("time" %in% names(result) && nrow(result) > 1) {
    time_diffs <- diff(result$time)
    # Time should generally be increasing (allowing for small variations)
    testthat::expect_true(all(time_diffs >= -0.01))
  }

  # Check that pressure values are reasonable (if present) using purrr::map_lgl
  if ("pres" %in% names(result)) {
    pres_checks <- list(
      non_negative = all(result$pres >= 0, na.rm = TRUE),
      reasonable = all(result$pres < 10000, na.rm = TRUE)
    )
    testthat::expect_true(pres_checks$non_negative)
    testthat::expect_true(pres_checks$reasonable)
  }

  # Check that temperature values are reasonable (if present)
  if ("temp" %in% names(result)) {
    testthat::expect_true(all(
      result$temp > -5 & result$temp < 50,
      na.rm = TRUE
    ))
  }

  # Check datetime and metadata columns using purrr::map
  datetime_checks <- purrr::map(
    list(
      gmt_no_na = all(!is.na(result$datetime_gmt)),
      local_no_na = all(!is.na(result$datetime_local)),
      timezone_consistent = length(unique(result$timezone)) == 1,
      filename_consistent = length(unique(result$file_name)) == 1
    ),
    identity
  )

  testthat::expect_true(datetime_checks$gmt_no_na)
  testthat::expect_true(datetime_checks$local_no_na)
  testthat::expect_true(datetime_checks$timezone_consistent)
  testthat::expect_true(datetime_checks$filename_consistent)
})

testthat::test_that("load_pro_files processes both test files successfully", {
  test_dir <- testthat::test_path("test-data", "pro_files")

  result <- ecotaxaLoadR::load_pro_files(test_dir)

  # Should process both files
  testthat::expect_equal(length(result), 2)

  # Check that no files failed
  failed_files <- attr(result, "failed_files")
  testthat::expect_equal(length(failed_files), 0)

  # Check processing summary using purrr::map_lgl
  summary_info <- attr(result, "processing_summary")
  testthat::expect_type(summary_info, "list")

  expected_summary_fields <- c("total_files", "successful_files")
  summary_checks <- purrr::map_lgl(
    expected_summary_fields,
    ~ .x %in% names(summary_info)
  )
  purrr::walk2(
    expected_summary_fields,
    summary_checks,
    ~ {
      testthat::expect_true(.y, info = paste("Should have", .x, "in summary"))
    }
  )
})