testthat::test_that("load_pid_files works with test directory containing PID files", {
  # Test with directory containing both PID files
  test_dir <- testthat::test_path("test-data", "pid_files")
  
  testthat::expect_true(dir.exists(test_dir))
  
  result <- ecotaxaLoadR::load_pid_files(test_dir)
  
  # Check that result is a list
  testthat::expect_type(result, "list")
  
  # Check for expected components
  expected_components <- c("metadata", "records", "attributes", "file_count", "processed_files")
  components_present <- purrr::map_lgl(expected_components, ~ .x %in% names(result))
  purrr::walk2(expected_components, components_present, ~ {
    testthat::expect_true(.y, info = paste("Should have", .x, "component"))
  })
  
  # Check that we processed some files
  testthat::expect_gt(result$file_count, 0)
  
  # Each data component should be a tibble
  data_components <- c("metadata", "records", "attributes")
  purrr::walk(data_components, ~ {
    testthat::expect_s3_class(result[[.x]], "data.frame")
  })
})

testthat::test_that("load_pid_files handles empty directory", {
  # Create a temporary directory with no PID files
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_pid_dir")
  dir.create(empty_dir, showWarnings = FALSE)
  
  testthat::expect_warning(
    result <- ecotaxaLoadR::load_pid_files(empty_dir),
    "No PID files found"
  )
  
  # Should return empty tibbles
  testthat::expect_equal(nrow(result$metadata), 0)
  testthat::expect_equal(nrow(result$records), 0)
  testthat::expect_equal(nrow(result$attributes), 0)
  
  # Clean up
  unlink(empty_dir, recursive = TRUE)
})

testthat::test_that("parse_pid_file works with sr2407 PID file", {
  # Test with sr2407 file
  pid_file <- testthat::test_path("test-data", "pid_files", "sr2407_m6_n3_d2_b_1_dat1.pid")
  
  testthat::expect_true(file.exists(pid_file))
  
  result <- ecotaxaLoadR:::parse_pid_file(pid_file)
  
  # Check that result is a list with expected components
  expected_components <- c("image_metadata", "image_records", "image_attributes")
  components_present <- purrr::map_lgl(expected_components, ~ .x %in% names(result))
  testthat::expect_true(all(components_present))
  
  # Each component should be a data frame
  purrr::walk(expected_components, ~ {
    testthat::expect_s3_class(result[[.x]], "data.frame")
  })
  
  # Check that metadata has expected structure
  metadata <- result$image_metadata
  if (nrow(metadata) > 0) {
    expected_metadata_cols <- c("scan_id", "section_name", "key", "value")
    metadata_cols_present <- purrr::map_lgl(expected_metadata_cols, ~ .x %in% names(metadata))
    testthat::expect_true(all(metadata_cols_present))
    
    # Should have filename record
    testthat::expect_true(any(metadata$key == "filename"))
    testthat::expect_true(any(metadata$section_name == "file_info"))
  }
  
  # Check that records has expected structure
  records <- result$image_records
  if (nrow(records) > 0) {
    expected_record_cols <- c("scan_id", "image_number")
    record_cols_present <- purrr::map_lgl(expected_record_cols, ~ .x %in% names(records))
    testthat::expect_true(all(record_cols_present))
    
    # Image numbers should be integers
    testthat::expect_true(is.integer(records$image_number) || is.numeric(records$image_number))
  }
  
  # Check that attributes has expected structure
  attributes <- result$image_attributes
  if (nrow(attributes) > 0) {
    expected_attr_cols <- c("scan_id", "image_number", "attribute_name", "attribute_value")
    attr_cols_present <- purrr::map_lgl(expected_attr_cols, ~ .x %in% names(attributes))
    testthat::expect_true(all(attr_cols_present))
  }
})

testthat::test_that("parse_pid_file works with sr2408 PID file", {
  # Test with sr2408 file
  pid_file <- testthat::test_path("test-data", "pid_files", "sr2408_m13_n7_d2_a_1_dat1.pid")
  
  testthat::expect_true(file.exists(pid_file))
  
  result <- ecotaxaLoadR:::parse_pid_file(pid_file)
  
  # Check basic structure
  expected_components <- c("image_metadata", "image_records", "image_attributes")
  components_present <- purrr::map_lgl(expected_components, ~ .x %in% names(result))
  testthat::expect_true(all(components_present))
  
  # Check that scan_id is correctly extracted
  metadata <- result$image_metadata
  if (nrow(metadata) > 0) {
    scan_ids <- unique(metadata$scan_id)
    testthat::expect_length(scan_ids, 1)
    testthat::expect_true(grepl("sr2408_m13_n7_d2_a_1", scan_ids[1]))
  }
})

testthat::test_that("parse_pid_file handles missing file", {
  non_existent_file <- testthat::test_path("test-data", "pid_files", "nonexistent.pid")
  
  testthat::expect_error(
    ecotaxaLoadR:::parse_pid_file(non_existent_file)
  )
})

testthat::test_that("parse_metadata_sections_long works correctly", {
  # Create sample metadata lines
  metadata_lines <- c(
    "PID",
    "[Image]",
    "Scanning_date= 20250701_1741",
    "Scanning_area= large",
    "[Sample]", 
    "Project= hypoxia",
    "SampleId= sr2407_m6_n3"
  )
  
  result <- ecotaxaLoadR:::parse_metadata_sections_long(
    metadata_lines, 
    "test_scan", 
    "test_file.pid"
  )
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("scan_id", "section_name", "key", "value")
  cols_present <- purrr::map_lgl(expected_cols, ~ .x %in% names(result))
  testthat::expect_true(all(cols_present))
  
  # Should have filename record
  testthat::expect_true(any(result$key == "filename"))
  testthat::expect_true(any(result$section_name == "file_info"))
  
  # Should have parsed metadata
  testthat::expect_true(any(result$section_name == "Image"))
  testthat::expect_true(any(result$section_name == "Sample"))
  testthat::expect_true(any(result$key == "Scanning_date"))
  testthat::expect_true(any(result$key == "Project"))
})

testthat::test_that("parse_data_section_long works correctly", {
  # Create sample data
  headers <- c("Item", "Label", "Area", "Mean", "X", "Y")
  data_lines <- c(
    "1;test_001;1234.5;128.5;100;200",
    "2;test_002;2345.6;135.2;150;250"
  )
  
  result <- ecotaxaLoadR:::parse_data_section_long(
    data_lines, 
    headers, 
    "test_scan", 
    "test_file.pid"
  )
  
  testthat::expect_type(result, "list")
  testthat::expect_true("image_records" %in% names(result))
  testthat::expect_true("image_attributes" %in% names(result))
  
  # Check image records
  records <- result$image_records
  testthat::expect_s3_class(records, "data.frame")
  testthat::expect_equal(nrow(records), 2)
  
  expected_record_cols <- c("scan_id", "image_number")
  record_cols_present <- purrr::map_lgl(expected_record_cols, ~ .x %in% names(records))
  testthat::expect_true(all(record_cols_present))
  
  # Check image attributes
  attributes <- result$image_attributes
  testthat::expect_s3_class(attributes, "data.frame")
  testthat::expect_true(nrow(attributes) > 0)
  
  expected_attr_cols <- c("scan_id", "image_number", "attribute_name", "attribute_value")
  attr_cols_present <- purrr::map_lgl(expected_attr_cols, ~ .x %in% names(attributes))
  testthat::expect_true(all(attr_cols_present))
  
  # Should have attributes for both images
  unique_images <- unique(attributes$image_number)
  testthat::expect_length(unique_images, 2)
  testthat::expect_true(all(c(1, 2) %in% unique_images))
})

testthat::test_that("parse_data_section_long handles empty data", {
  result <- ecotaxaLoadR:::parse_data_section_long(
    character(0), 
    character(0), 
    "test_scan", 
    "test_file.pid"
  )
  
  testthat::expect_type(result, "list")
  testthat::expect_equal(nrow(result$image_records), 0)
  testthat::expect_equal(nrow(result$image_attributes), 0)
})

testthat::test_that("parse_data_section_long handles malformed data", {
  headers <- c("Item", "Label", "Area")
  # Line with wrong number of fields
  data_lines <- c(
    "1;test_001;1234.5;extra_field",  # Too many fields
    "2;test_002"  # Too few fields
  )
  
  result <- ecotaxaLoadR:::parse_data_section_long(
    data_lines, 
    headers, 
    "test_scan", 
    "test_file.pid"
  )
  
  # Should handle malformed data gracefully
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result$image_records, "data.frame")
  testthat::expect_s3_class(result$image_attributes, "data.frame")
})

testthat::test_that("load_pid_files processes both test files successfully", {
  test_dir <- testthat::test_path("test-data", "pid_files")
  
  result <- ecotaxaLoadR::load_pid_files(test_dir)
  
  # Should process both files
  testthat::expect_equal(result$file_count, 2)
  testthat::expect_length(result$processed_files, 2)
  
  # Check that filenames are preserved
  file_names <- result$processed_files
  expected_files <- c("sr2407_m6_n3_d2_b_1_dat1.pid", "sr2408_m13_n7_d2_a_1_dat1.pid")
  files_present <- purrr::map_lgl(expected_files, ~ .x %in% file_names)
  testthat::expect_true(all(files_present))
  
  # Check combined data has records from both files
  metadata <- result$metadata
  if (nrow(metadata) > 0) {
    scan_ids <- unique(metadata$scan_id)
    testthat::expect_true(length(scan_ids) >= 2)
  }
})

testthat::test_that("load_pid_files handles different file patterns", {
  test_dir <- testthat::test_path("test-data", "pid_files")
  
  # Test with specific pattern
  result <- ecotaxaLoadR::load_pid_files(test_dir, file_pattern = "sr2407.*\\.pid$")
  
  # Should only process sr2407 file
  testthat::expect_equal(result$file_count, 1)
  testthat::expect_true(grepl("sr2407", result$processed_files[1]))
})

testthat::test_that("PID parsing preserves data integrity", {
  pid_file <- testthat::test_path("test-data", "pid_files", "sr2407_m6_n3_d2_b_1_dat1.pid")
  
  result <- ecotaxaLoadR:::parse_pid_file(pid_file)
  
  # Check that scan_id is consistent across all components
  scan_ids <- purrr::map(result, ~ if(nrow(.x) > 0) unique(.x$scan_id) else character(0))
  non_empty_scan_ids <- purrr::keep(scan_ids, ~ length(.x) > 0)
  
  if (length(non_empty_scan_ids) > 0) {
    all_scan_ids <- purrr::reduce(non_empty_scan_ids, c)
    testthat::expect_length(unique(all_scan_ids), 1)
  }
  
  # Check that image numbers are consistent between records and attributes
  records <- result$image_records
  attributes <- result$image_attributes
  
  if (nrow(records) > 0 && nrow(attributes) > 0) {
    record_images <- unique(records$image_number)
    attr_images <- unique(attributes$image_number)
    
    # All images in attributes should exist in records
    images_match <- purrr::every(attr_images, ~ .x %in% record_images)
    testthat::expect_true(images_match)
  }
})

testthat::test_that("PID metadata extraction handles various section types", {
  # Test metadata parsing with various section types
  metadata_lines <- c(
    "PID",
    "[Image]",
    "Scanning_date= 20250701_1741",
    "Vuescan_version= 9.7.67",
    "[VueScan]",
    "[Input]", 
    "Source=PerfectionV800",
    "Options=2",
    "[Sample]",
    "Project= hypoxia",
    "SampleId= sr2407_m6_n3"
  )
  
  result <- ecotaxaLoadR:::parse_metadata_sections_long(
    metadata_lines, 
    "test_scan", 
    "test_file.pid"
  )
  
  # Should have multiple sections
  unique_sections <- unique(result$section_name)
  expected_sections <- c("file_info", "Image", "Input", "Sample")
  sections_found <- purrr::map_lgl(expected_sections, ~ .x %in% unique_sections)
  testthat::expect_true(all(sections_found))
  
  # Check that key-value pairs are correctly associated with sections
  input_records <- dplyr::filter(result, section_name == "Input")
  if (nrow(input_records) > 0) {
    testthat::expect_true("Source" %in% input_records$key)
    testthat::expect_true("Options" %in% input_records$key)
  }
  
  sample_records <- dplyr::filter(result, section_name == "Sample")
  if (nrow(sample_records) > 0) {
    testthat::expect_true("Project" %in% sample_records$key)
    testthat::expect_true("SampleId" %in% sample_records$key)
  }
})

testthat::test_that("PID functions handle edge cases gracefully", {
  # Test with empty metadata
  empty_result <- ecotaxaLoadR:::parse_metadata_sections_long(
    character(0), 
    "test_scan", 
    "test_file.pid"
  )
  
  testthat::expect_s3_class(empty_result, "data.frame")
  testthat::expect_equal(nrow(empty_result), 1)  # Should have filename record
  testthat::expect_equal(empty_result$key[1], "filename")
  
  # Test with only section headers, no key-value pairs
  headers_only <- c("[Section1]", "[Section2]")
  headers_result <- ecotaxaLoadR:::parse_metadata_sections_long(
    headers_only, 
    "test_scan", 
    "test_file.pid"
  )
  
  testthat::expect_s3_class(headers_result, "data.frame")
  testthat::expect_equal(nrow(headers_result), 1)  # Should only have filename record
})