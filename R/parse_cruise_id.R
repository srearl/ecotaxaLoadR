#' @title Parse the cruise ID column of an EcoTaxa file
#'
#' @description \code{parse_cruise_id} Parses the `cruise_id` column of an
#' EcoTaxa file of type MOC, UVP, or FlowCam.
#'
#' @param ecotaxa_file (character) The unquoted name of a tibble or data frame
#' in the R environment that reflecte the data in the EcoTaxa file.
#' @param debug A logical value indicating whether to enable debug mode.
#' Defaults to `FALSE`.
#'
#' @return A list with two elements:
#'   \item{extracted_file}{A data frame with extracted columns.}
#'   \item{pattern}{A string indicating the matched pattern type ("flowcam",
#'   "moc", "uvp", or "none").}
#'
#' @export
#'
parse_cruise_id <- function(
  ecotaxa_file,
  debug = FALSE
) {
  # test for data collection type from a subsample of the data
  set.seed(123)
  sample_size <- min(1000, nrow(ecotaxa_file))
  subsample <- ecotaxa_file[
    sample(x = nrow(ecotaxa_file), size = sample_size),
    ,
    drop = FALSE
  ]

  flowcam_pattern_true <- FALSE
  moc_pattern_true     <- FALSE
  uvp_pattern_true     <- FALSE

  # FLOWCAM

  flowcam_pattern <- "^([0-9]{5})_([0-9]{4})_([0-9]{2})_([0-9]{1})_([0-9]+[a-zA-Z]+)_([a-zA-Z])_([0-9]+)$"
  # flowcam_pattern: "10414_0000_01_1_20x_d_00080"

  flowcam_pattern2 <- "^([0-9]{5})_([0-9]{4})_([0-9]{2})_([0-9]{1})_([0-9]+[a-zA-Z]+)_([0-9]+_[a-zA-Z])_([0-9]+)$"
  # flowcam_pattern2:  "10423_0800_22_1_20x_2_d_00116"

  # check if rows in subsample match flowcam_patterns
  matches_flowcam_pattern <- base::regmatches(
    subsample$object_id,
    base::regexec(flowcam_pattern, subsample$object_id)
  )
  matches_flowcam_pattern2 <- base::regmatches(
    subsample$object_id,
    base::regexec(flowcam_pattern2, subsample$object_id)
  )

  flowcam_pattern_true <- all(
    sapply(matches_flowcam_pattern, function(x) length(x) > 1) |
      sapply(matches_flowcam_pattern2, function(x) length(x) > 1)
  )

  if (flowcam_pattern_true == TRUE) {
    message("flowcam pattern matched")

    flowcam_patterns <- list(
      flowcam_pattern  = flowcam_pattern,
      flowcam_pattern2 = flowcam_pattern2
    )

    extracted_file <- extract_flowcam_columns(
      ecotaxa_file = ecotaxa_file,
      pattern      = flowcam_patterns,
      debug        = debug
    )

    return(
      list(
        parsed_file = extracted_file,
        pattern = "flowcam"
      )
    )
  }

  # MOC

  pattern1 <- "^([0-9]{2})([0-9]{2})([0-9]{2})_([0-9]{4})_([0-9]+_[0-9]+)_([a-zA-Z0-9]+_[a-zA-Z0-9]+)_([0-9]+_[0-9]+)$"
  # pattern2 <- "^([a-zA-Z0-9]+)_([a-zA-Z0-9]+)_([a-zA-Z0-9]+)_([0-9]+)_([0-9]+_[0-9]+)$"
  pattern3 <- "^([0-9]{2})([0-9]{2})([0-9]{2})_([0-9]+)_([0-9]+)$"
  pattern4 <- "^([0-9]{2})([0-9]{2})([0-9]{2})_([0-9]{4})_([0-9]{1}_[0-9]+)_([0-9]{1}_[0-9]+)$"
  pattern5 <- "^([0-9]{2})([0-9]{2})([0-9]{2})_([0-9]{4})_([0-9]+_[0-9]+)$"
  pattern6 <- "^([a-zA-Z]+[0-9]+)_([a-zA-Z][0-9]+)_([a-zA-Z][0-9]+)_([a-zA-Z0-9]+)_([0-9]+_[0-9]+)$"
  # pattern7 <- "^([a-zA-Z]+[0-9]+)_([a-zA-Z][0-9]+)_([a-zA-Z][0-9]+)_([a-zA-Z][0-9]+)_[a-zA-Z]_([0-9]+_[0-9]+)$"
  pattern8 <- "^([a-zA-Z]+[0-9]+)_([a-zA-Z][0-9]+)_([a-zA-Z][0-9]+)_([a-zA-Z][0-9]+)_([a-zA-Z])_([0-9]+_[0-9]+)$"
  pattern10 <- "^([a-zA-Z]+[0-9]+)_m([0-9]+)_n([0-9]+)_d([0-9]+)_([ab])_([0-9]+)_([0-9]+)_([0-9]+)$" # Case 3: 8 parts, with lab_split (a or b)

  matches1 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern1, ecotaxa_file$object_id)
  )
  matches3 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern3, ecotaxa_file$object_id)
  )
  matches4 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern4, ecotaxa_file$object_id)
  )
  matches5 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern5, ecotaxa_file$object_id)
  )
  matches6 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern6, ecotaxa_file$object_id)
  )
  matches8 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern8, ecotaxa_file$object_id)
  )
  matches10 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern10, ecotaxa_file$object_id)
  )

  moc_pattern_true <- any(
    sapply(matches1, function(x) length(x) > 1) |
      sapply(matches3, function(x) length(x) > 1) |
      sapply(matches4, function(x) length(x) > 1) |
      sapply(matches5, function(x) length(x) > 1) |
      sapply(matches6, function(x) length(x) > 1) |
      sapply(matches8, function(x) length(x) > 1) |
      sapply(matches10, function(x) length(x) > 1)
  )

  # DEBUGGING
  if (debug == TRUE) {
    message("=== MOC PATTERN DEBUGGING ===")
    message(
      "Sample object_ids: ",
      paste(head(ecotaxa_file$object_id, 3), collapse = ", ")
    )

    pattern_results <- list(
      pattern1  = sum(sapply(matches1, function(x) length(x) > 1)),
      pattern3  = sum(sapply(matches3, function(x) length(x) > 1)),
      pattern4  = sum(sapply(matches4, function(x) length(x) > 1)),
      pattern5  = sum(sapply(matches5, function(x) length(x) > 1)),
      pattern6  = sum(sapply(matches6, function(x) length(x) > 1)),
      pattern8  = sum(sapply(matches8, function(x) length(x) > 1)),
      pattern10 = sum(sapply(matches10, function(x) length(x) > 1))
    )

    for (i in 1:length(pattern_results)) {
      message(sprintf(
        "%-10s: %d matches",
        names(pattern_results)[i],
        pattern_results[[i]]
      ))
    }

    # Test pattern8 specifically on first few object_ids
    test_ids <- head(ecotaxa_file$object_id, 5)
    message("\nPattern8 testing on first 5 object_ids:")
    for (id in test_ids) {
      match_result <- regmatches(id, regexec(pattern8, id))
      if (length(match_result[[1]]) > 1) {
        message(sprintf(
          "✓ %s -> %s",
          id,
          paste(match_result[[1]][-1], collapse = " | ")
        ))
      } else {
        message(sprintf("✗ %s -> NO MATCH", id))
      }
    }
    message("moc_pattern_true: ", moc_pattern_true)
    message("===============================")
  }

  # DEBGUGGING
  if (debug == TRUE) {
    message("=== DETAILED UNMATCHED PATTERN ANALYSIS ===")

    # Check which object_ids don't match any pattern
    pattern_matches <- sapply(matches1, function(x) length(x) > 1) |
      sapply(matches3, function(x) length(x) > 1) |
      sapply(matches4, function(x) length(x) > 1) |
      sapply(matches5, function(x) length(x) > 1) |
      sapply(matches6, function(x) length(x) > 1) |
      sapply(matches8, function(x) length(x) > 1) |
      sapply(matches10, function(x) length(x) > 1)

    total_rows     <- length(ecotaxa_file$object_id)
    matched_rows   <- sum(pattern_matches)
    unmatched_rows <- total_rows - matched_rows

    message(sprintf("Total rows: %d", total_rows))
    message(sprintf("Matched rows: %d", matched_rows))
    message(sprintf("Unmatched rows: %d", unmatched_rows))
    message(sprintf(
      "Match percentage: %.2f%%",
      (matched_rows / total_rows) * 100
    ))

    # Show examples of unmatched object_ids
    if (unmatched_rows > 0) {
      unmatched_ids <- ecotaxa_file$object_id[!pattern_matches]
      message("\nFirst 10 unmatched object_ids:")
      for (id in head(unmatched_ids, 10)) {
        message(sprintf("  %s", id))
      }

      # Look for patterns in unmatched IDs
      message("\nAnalyzing unmatched patterns...")
      unique_unmatched <- unique(unmatched_ids)
      if (length(unique_unmatched) <= 20) {
        message("All unique unmatched patterns:")
        for (pattern in unique_unmatched) {
          message(sprintf("  %s", pattern))
        }
      } else {
        message(sprintf(
          "Too many unique patterns (%d). Showing first 20:",
          length(unique_unmatched)
        ))
        for (pattern in head(unique_unmatched, 20)) {
          message(sprintf("  %s", pattern))
        }
      }
    } else {
      message("All object_ids matched successfully!")
    }

    message("==============================================")
  }

  if (moc_pattern_true == TRUE) {
    message("MOC pattern matched")

    moc_patterns <- list(
      pattern1  = pattern1,
      pattern3  = pattern3,
      pattern4  = pattern4,
      pattern5  = pattern5,
      pattern6  = pattern6,
      pattern8  = pattern8,
      pattern10 = pattern10
    )

    extracted_file <- extract_moc_columns(
      ecotaxa_file = ecotaxa_file,
      pattern      = moc_patterns,
      debug        = debug
    )

    return(
      list(
        parsed_file = extracted_file,
        pattern     = "moc"
      )
    )
  }

  # UVP

  uvp_pattern <- "^([0-9]{4})([0-9]{2})([0-9]{2})-([0-9]{2})([0-9]{2})([0-9]{2})-([0-9]{3})_([0-9]+)$"

  # check if rows in subsample match uvp_pattern
  matches_uvp_pattern <- base::regmatches(
    subsample$object_id,
    base::regexec(uvp_pattern, subsample$object_id)
  )

  uvp_pattern_true <- all(
    sapply(matches_uvp_pattern, function(x) length(x) > 1)
  )

  if (uvp_pattern_true == TRUE) {
    message("uvp pattern matched")

    uvp_patterns <- list(
      uvp_patterns = uvp_pattern
    )

    extracted_file <- extract_uvp_columns(
      ecotaxa_file = ecotaxa_file,
      pattern      = uvp_patterns,
      debug        = debug
    )

    return(
      list(
        parsed_file = extracted_file,
        pattern     = "uvp"
      )
    )
  }

  if (
    !flowcam_pattern_true &&
      !moc_pattern_true &&
      !uvp_pattern_true
  ) {
    message("could match the object_id to any patterns")
    return(NULL)
  }
}

#' @title Parse the cruise ID column of an EcoTaxa file of type MOC
#'
#' @description Parses the `cruise_id` column of an EcoTaxa file of type MOC.
#'
#' @param ecotaxa_file (character) The unquoted name of a tibble or data frame
#' in the R environment that reflecte the data in the EcoTaxa file.
#' @param pattern (list) A list of MOC patterns to match.
#' @param debug (logical) A logical value indicating whether to enable debug
#' mode. Defaults to `FALSE`.
#'
#' @note A helper function to \code{parse_cruise_id}
#'
#' @importFrom dplyr select any_of filter vars
#' @importFrom purrr map2 reduce walk
#' @importFrom pointblank create_agent col_vals_not_null interrogate
#' @importFrom glue glue
#'
#' @return A data frame or tibble of the input EcoTaxa data with additional
#' columns reflecting the parsed values of `column_id`.
#'
extract_moc_columns <- function(
  ecotaxa_file,
  pattern = moc_patterns,
  debug = FALSE
) {
  ecotaxa_file$cruise <- NA
  ecotaxa_file$photo_id <- NA
  ecotaxa_file$moc <- NA
  ecotaxa_file$net <- NA
  ecotaxa_file$fraction <- NA
  ecotaxa_file$lab_split <- NA
  ecotaxa_file$split_fraction <- NA

  # extract components for pattern1
  matches1 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$pattern1, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches1, function(x) base::length(x) > 1),
    yes = base::sapply(matches1, function(x) x[2]),
    no = ecotaxa_file$cruise
  )
  ecotaxa_file$moc <- base::ifelse(
    test = base::sapply(matches1, function(x) base::length(x) > 1),
    yes = base::sapply(matches1, function(x) x[3]),
    no = ecotaxa_file$moc
  )
  ecotaxa_file$net <- base::ifelse(
    test = base::sapply(matches1, function(x) base::length(x) > 1),
    yes = base::sapply(matches1, function(x) x[4]),
    no = ecotaxa_file$net
  )
  ecotaxa_file$fraction <- base::ifelse(
    test = base::sapply(matches1, function(x) base::length(x) > 1),
    yes = base::sapply(matches1, function(x) x[5]),
    no = ecotaxa_file$fraction
  )
  ecotaxa_file$lab_split <- base::ifelse(
    test = base::sapply(matches1, function(x) base::length(x) > 1),
    yes = base::sapply(matches1, function(x) x[6]),
    no = ecotaxa_file$lab_split
  )
  ecotaxa_file$split_fraction <- base::ifelse(
    test = base::sapply(matches1, function(x) base::length(x) > 1),
    yes = base::sapply(matches1, function(x) x[7]),
    no = ecotaxa_file$split_fraction
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches1, function(x) base::length(x) > 1),
    yes = base::sapply(matches1, function(x) x[8]),
    no = ecotaxa_file$photo_id
  )

  # extract components for pattern3
  matches3 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$pattern3, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches3, function(x) base::length(x) > 1),
    yes = base::sapply(matches3, function(x) x[2]),
    no = ecotaxa_file$cruise
  )
  ecotaxa_file$moc <- base::ifelse(
    test = base::sapply(matches3, function(x) base::length(x) > 1),
    yes = base::sapply(matches3, function(x) x[3]),
    no = ecotaxa_file$moc
  )
  ecotaxa_file$net <- base::ifelse(
    test = base::sapply(matches3, function(x) base::length(x) > 1),
    yes = base::sapply(matches3, function(x) x[4]),
    no = ecotaxa_file$net
  )
  ecotaxa_file$fraction <- base::ifelse(
    test = base::sapply(matches3, function(x) base::length(x) > 1),
    yes = base::sapply(matches3, function(x) x[5]),
    no = ecotaxa_file$fraction
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches3, function(x) base::length(x) > 1),
    yes = base::sapply(matches3, function(x) x[6]),
    no = ecotaxa_file$photo_id
  )

  # extract components for pattern4
  matches4 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$pattern4, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches4, function(x) base::length(x) > 1),
    yes = base::sapply(matches4, function(x) x[2]),
    no = ecotaxa_file$cruise
  )
  ecotaxa_file$moc <- base::ifelse(
    test = base::sapply(matches4, function(x) base::length(x) > 1),
    yes = base::sapply(matches4, function(x) x[3]),
    no = ecotaxa_file$moc
  )
  ecotaxa_file$net <- base::ifelse(
    test = base::sapply(matches4, function(x) base::length(x) > 1),
    yes = base::sapply(matches4, function(x) x[4]),
    no = ecotaxa_file$net
  )
  ecotaxa_file$fraction <- base::ifelse(
    test = base::sapply(matches4, function(x) base::length(x) > 1),
    yes = base::sapply(matches4, function(x) x[5]),
    no = ecotaxa_file$fraction
  )
  ecotaxa_file$lab_split <- base::ifelse(
    test = base::sapply(matches4, function(x) base::length(x) > 1),
    yes = base::sapply(matches4, function(x) x[6]),
    no = ecotaxa_file$lab_split
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches4, function(x) base::length(x) > 1),
    yes = base::sapply(matches4, function(x) x[7]),
    no = ecotaxa_file$photo_id
  )

  # extract components for pattern5
  matches5 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$pattern5, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches5, function(x) base::length(x) > 1),
    yes = base::sapply(matches5, function(x) x[2]),
    no = ecotaxa_file$cruise
  )
  ecotaxa_file$moc <- base::ifelse(
    test = base::sapply(matches5, function(x) base::length(x) > 1),
    yes = base::sapply(matches5, function(x) x[3]),
    no = ecotaxa_file$moc
  )
  ecotaxa_file$net <- base::ifelse(
    test = base::sapply(matches5, function(x) base::length(x) > 1),
    yes = base::sapply(matches5, function(x) x[4]),
    no = ecotaxa_file$net
  )
  ecotaxa_file$fraction <- base::ifelse(
    test = base::sapply(matches5, function(x) base::length(x) > 1),
    yes = base::sapply(matches5, function(x) x[5]),
    no = ecotaxa_file$fraction
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches5, function(x) base::length(x) > 1),
    yes = base::sapply(matches5, function(x) x[6]),
    no = ecotaxa_file$photo_id
  )

  # extract components for pattern6
  matches6 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$pattern6, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches6, function(x) base::length(x) > 1),
    yes = base::sapply(matches6, function(x) x[2]),
    no = ecotaxa_file$cruise
  )
  ecotaxa_file$moc <- base::ifelse(
    test = base::sapply(matches6, function(x) base::length(x) > 1),
    yes = base::sapply(matches6, function(x) {
      stringr::str_extract(x[3], "[0-9]+")
    }), # extract digits only
    no = ecotaxa_file$moc
  )
  ecotaxa_file$net <- base::ifelse(
    test = base::sapply(matches6, function(x) base::length(x) > 1),
    yes = base::sapply(matches6, function(x) {
      stringr::str_extract(x[4], "[0-9]+")
    }), # extract digits only
    no = ecotaxa_file$net
  )
  ecotaxa_file$fraction <- base::ifelse(
    test = base::sapply(matches6, function(x) base::length(x) > 1),
    yes = base::sapply(matches6, function(x) {
      stringr::str_extract(x[5], "[0-9]+")
    }), # extract digits only
    no = ecotaxa_file$fraction
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches6, function(x) base::length(x) > 1),
    yes = base::sapply(matches6, function(x) x[6]),
    no = ecotaxa_file$photo_id
  )

  # extract components for pattern8
  matches8 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$pattern8, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches8, function(x) base::length(x) > 1),
    yes = base::sapply(matches8, function(x) x[2]),
    no = ecotaxa_file$cruise
  )
  ecotaxa_file$moc <- base::ifelse(
    test = base::sapply(matches8, function(x) base::length(x) > 1),
    yes = base::sapply(matches8, function(x) {
      stringr::str_extract(x[3], "[0-9]+")
    }), # extract digits only
    no = ecotaxa_file$moc
  )
  ecotaxa_file$net <- base::ifelse(
    test = base::sapply(matches8, function(x) base::length(x) > 1),
    yes = base::sapply(matches8, function(x) {
      stringr::str_extract(x[4], "[0-9]+")
    }), # extract digits only
    no = ecotaxa_file$net
  )
  ecotaxa_file$fraction <- base::ifelse(
    test = base::sapply(matches8, function(x) base::length(x) > 1),
    yes = base::sapply(matches8, function(x) {
      stringr::str_extract(x[5], "[0-9]+")
    }), # extract digits only
    no = ecotaxa_file$fraction
  )
  ecotaxa_file$lab_split <- base::ifelse(
    test = base::sapply(matches8, function(x) base::length(x) > 1),
    yes = base::sapply(matches8, function(x) x[6]),
    no = ecotaxa_file$lab_split
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches8, function(x) base::length(x) > 1),
    yes = base::sapply(matches8, function(x) x[7]),
    no = ecotaxa_file$photo_id
  )

  # extract components for pattern10 (Case 3: ae1712_m9_n8_d3_b_2_1_1251 - WITH
  # lab_split, skip middle digit)
  matches10 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$pattern10, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches10, function(x) base::length(x) > 1),
    yes = base::sapply(matches10, function(x) x[2]), # ae1712
    no = ecotaxa_file$cruise
  )
  ecotaxa_file$moc <- base::ifelse(
    test = base::sapply(matches10, function(x) base::length(x) > 1),
    yes = base::sapply(matches10, function(x) x[3]), # 9
    no = ecotaxa_file$moc
  )
  ecotaxa_file$net <- base::ifelse(
    test = base::sapply(matches10, function(x) base::length(x) > 1),
    yes = base::sapply(matches10, function(x) x[4]), # 8
    no = ecotaxa_file$net
  )
  ecotaxa_file$fraction <- base::ifelse(
    test = base::sapply(matches10, function(x) base::length(x) > 1),
    yes = base::sapply(matches10, function(x) x[5]), # 3
    no = ecotaxa_file$fraction
  )
  ecotaxa_file$lab_split <- base::ifelse(
    test = base::sapply(matches10, function(x) base::length(x) > 1),
    yes = base::sapply(matches10, function(x) x[6]), # b (or a)
    no = ecotaxa_file$lab_split
  )
  # Skip group 7 (the "2") and combine groups 8 and 9 for photo_id
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches10, function(x) base::length(x) > 1),
    yes = base::sapply(matches10, function(x) paste(x[8], x[9], sep = "_")), # 1_1251 (skips the "2")
    no = ecotaxa_file$photo_id
  )

  # DEBUGGING
  if (debug == TRUE) {
    message("=== PATTERN8 EXTRACTION DEBUGGING ===")
    pattern8_matches <- sum(sapply(matches8, function(x) length(x) > 1))
    message("Pattern8 matches found: ", pattern8_matches)

    if (pattern8_matches > 0) {
      # Show first successful match
      first_match_idx <- which(sapply(matches8, function(x) length(x) > 1))[1]
      if (!is.na(first_match_idx)) {
        match_groups <- matches8[[first_match_idx]]
        message(sprintf(
          "First successful match (%s):",
          ecotaxa_file$object_id[first_match_idx]
        ))
        message(sprintf("  Group 1 (cruise): %s", match_groups[2]))
        message(sprintf("  Group 2 (moc): %s", match_groups[3]))
        message(sprintf("  Group 3 (net): %s", match_groups[4]))
        message(sprintf("  Group 4 (fraction): %s", match_groups[5]))
        message(sprintf("  Group 5 (lab_split): %s", match_groups[6]))
        message(sprintf("  Group 6 (photo_id): %s", match_groups[7]))
      }
    }
    message("=====================================")
  }

  # add the pattern invoked for testing and debugging (optional)

  patterns <- list(
    pattern$pattern1,
    pattern$pattern3,
    pattern$pattern4,
    pattern$pattern5,
    pattern$pattern6,
    pattern$pattern8,
    pattern$pattern10
  )

  pattern_names <- c(
    "pattern1",
    "pattern3",
    "pattern4",
    "pattern5",
    "pattern6",
    "pattern8",
    "pattern10"
  )

  # use purrr::map2() to iterate over patterns and pattern_names
  ecotaxa_file$pattern <- NA
  ecotaxa_file$pattern <- purrr::reduce(
    .x = purrr::map2(
      patterns,
      pattern_names,
      ~ {
        base::ifelse(
          test = base::sapply(
            base::regmatches(
              ecotaxa_file$object_id,
              base::regexec(.x, ecotaxa_file$object_id)
            ),
            function(x) base::length(x) > 1
          ),
          yes = .y,
          no = ecotaxa_file$pattern
        )
      }
    ),
    .f = ~ ifelse(
      test = is.na(.x),
      yes = .y,
      no = .x
    )
  )

  agent <- pointblank::create_agent(tbl = ecotaxa_file) |>
    pointblank::col_vals_not_null(
      columns = dplyr::vars(
        cruise,
        moc,
        net,
        fraction
      ),
      label = "cruise_moc_net_fraction"
    ) |>
    pointblank::col_vals_not_null(
      columns = c("lab_split"),
      preconditions = function(x) {
        x |>
          dplyr::filter(
            pattern %in% c("pattern1", "pattern4", "pattern8", "pattern10")
          )
      }
    ) |>
    pointblank::col_vals_not_null(
      columns = c("split_fraction"),
      preconditions = function(x) {
        x |>
          dplyr::filter(pattern %in% c("pattern1"))
      }
    ) |>
    pointblank::interrogate()

  if (any(agent$validation_set$all_passed == FALSE)) {
    failed_indices <- which(agent$validation_set$all_passed == FALSE)

    purrr::walk(
      .x = failed_indices,
      .f = function(i) {
        failed_brief <- agent$validation_set$brief[i]
        failed_rows <- agent$validation_set$n_failed[i]
        message(glue::glue(
          "validation failed for: {failed_brief} ({failed_rows} rows failed)"
        ))
      }
    )
  }

  moc_cols <- c(
    "object_id",
    "cruise",
    "moc",
    "net",
    "fraction",
    "lab_split",
    "split_fraction",
    "photo_id",
    "pattern"
  )

  if (debug == TRUE) {
    print(agent)
  }

  # remove any parsed columns that are empty
  ecotaxa_file <- ecotaxa_file |>
    dplyr::select(
      -dplyr::any_of(moc_cols[sapply(
        ecotaxa_file[moc_cols],
        function(x) all(is.na(x))
      )])
    ) |>
    dplyr::select(-pattern) |> # remove pattern column
    dplyr::select(dplyr::any_of(moc_cols), dplyr::everything()) # MOC columns first, then everything else

  return(ecotaxa_file)

}


#' @title Parse the cruise ID column of an EcoTaxa file of type FlowCam.
#'
#' @description Parses the `cruise_id` column of an EcoTaxa file of
#' type FlowCam.
#'
#' @param ecotaxa_file (character) The unquoted name of a tibble or data frame
#' in the R environment that reflecte the data in the EcoTaxa file.
#' @param pattern (list) A list of FlowCam patterns to match.
#' @param debug (logical) A logical value indicating whether to enable debug
#' mode. Defaults to `FALSE`.
#'
#' @note A helper function to \code{parse_cruise_id}
#'
#' @importFrom dplyr select any_of filter vars
#' @importFrom purrr map2 reduce walk
#' @importFrom pointblank create_agent col_vals_not_null interrogate
#' @importFrom glue glue
#'
#' @return A data frame or tibble of the input EcoTaxa data with additional
#' columns reflecting the parsed values of `column_id`.
#'
extract_flowcam_columns <- function(
  ecotaxa_file,
  pattern = flowcam_patterns,
  debug   = FALSE
) {

  ecotaxa_file$cruise             <- NA
  ecotaxa_file$photo_id           <- NA
  ecotaxa_file$depth              <- NA
  ecotaxa_file$niskin             <- NA
  ecotaxa_file$mode               <- NA
  ecotaxa_file$magnification      <- NA
  ecotaxa_file$duplicates_removed <- NA

  # extract components for flowcam_pattern
  matches_flowcam <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$flowcam_pattern, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches_flowcam, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam, function(x) x[2]),
    no   = ecotaxa_file$cruise
  )
  ecotaxa_file$depth <- base::ifelse(
    test = base::sapply(matches_flowcam, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam, function(x) x[3]),
    no   = ecotaxa_file$depth
  )
  ecotaxa_file$niskin <- base::ifelse(
    test = base::sapply(matches_flowcam, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam, function(x) x[4]),
    no   = ecotaxa_file$niskin
  )
  ecotaxa_file$mode <- base::ifelse(
    test = base::sapply(matches_flowcam, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam, function(x) x[5]),
    no   = ecotaxa_file$mode
  )
  ecotaxa_file$magnification <- base::ifelse(
    test = base::sapply(matches_flowcam, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam, function(x) x[6]),
    no   = ecotaxa_file$magnification
  )
  ecotaxa_file$duplicates_removed <- base::ifelse(
    test = base::sapply(matches_flowcam, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam, function(x) x[7]),
    no   = ecotaxa_file$duplicates_removed
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches_flowcam, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam, function(x) x[8]),
    no   = ecotaxa_file$photo_id
  )

  # extract components for flowcam_pattern2
  matches_flowcam2 <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$flowcam_pattern2, ecotaxa_file$object_id)
  )

  ecotaxa_file$cruise <- base::ifelse(
    test = base::sapply(matches_flowcam2, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam2, function(x) x[2]),
    no   = ecotaxa_file$cruise
  )
  ecotaxa_file$depth <- base::ifelse(
    test = base::sapply(matches_flowcam2, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam2, function(x) x[3]),
    no   = ecotaxa_file$depth
  )
  ecotaxa_file$niskin <- base::ifelse(
    test = base::sapply(matches_flowcam2, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam2, function(x) x[4]),
    no   = ecotaxa_file$niskin
  )
  ecotaxa_file$mode <- base::ifelse(
    test = base::sapply(matches_flowcam2, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam2, function(x) x[5]),
    no   = ecotaxa_file$mode
  )
  ecotaxa_file$magnification <- base::ifelse(
    test = base::sapply(matches_flowcam2, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam2, function(x) x[6]),
    no   = ecotaxa_file$magnification
  )
  ecotaxa_file$duplicates_removed <- base::ifelse(
    test = base::sapply(matches_flowcam2, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam2, function(x) x[7]),
    no   = ecotaxa_file$duplicates_removed
  )
  ecotaxa_file$photo_id <- base::ifelse(
    test = base::sapply(matches_flowcam2, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_flowcam2, function(x) x[8]),
    no   = ecotaxa_file$photo_id
  )

  patterns <- list(
    pattern$flowcam_pattern,
    pattern$flowcam_pattern2
  )

  pattern_names <- c(
    "flowcam_pattern",
    "flowcam_pattern2"
  )

  # iterate over patterns and pattern_names
  ecotaxa_file$pattern <- NA
  ecotaxa_file$pattern <- purrr::reduce(
    .x = purrr::map2(
      patterns,
      pattern_names,
      ~ {
        base::ifelse(
          test = base::sapply(
            base::regmatches(
              ecotaxa_file$object_id,
              base::regexec(.x, ecotaxa_file$object_id)
            ),
            function(x) base::length(x) > 1
          ),
          yes = .y,
          no = ecotaxa_file$pattern
        )
      }
    ),
    .f = ~ ifelse(
      test = is.na(.x),
      yes  = .y,
      no   = .x
    )
  )

  agent <- pointblank::create_agent(tbl = ecotaxa_file) |>
    pointblank::col_vals_not_null(
      columns = dplyr::vars(
        cruise,
        photo_id,
        depth,
        niskin,
        mode,
        magnification,
        duplicates_removed
      ),
      label = "flowcam_cols"
    ) |>
    pointblank::interrogate()

  if (any(agent$validation_set$all_passed == FALSE)) {
    failed_indices <- which(agent$validation_set$all_passed == FALSE)

    purrr::walk(
      .x = failed_indices,
      .f = function(i) {
        failed_brief <- agent$validation_set$brief[i]
        failed_rows <- agent$validation_set$n_failed[i]
        message(glue::glue(
          "validation failed for: {failed_brief} ({failed_rows} rows failed)"
        ))
      }
    )
  }

  flowcam_cols <- c(
    "object_id",
    "cruise",
    "depth",
    "niskin",
    "mode",
    "magnification",
    "duplicates_removed",
    "photo_id",
    "pattern"
  )

  if (debug == TRUE) {
    print(agent)

    ecotaxa_file <- ecotaxa_file |>
      dplyr::select(dplyr::any_of(flowcam_cols))
  } else {
    ecotaxa_file <- ecotaxa_file |>
      # remove any parsed columns that are empty
      dplyr::select(
        -dplyr::any_of(
          flowcam_cols[sapply(
            ecotaxa_file[flowcam_cols],
            function(x) all(is.na(x))
          )]
        )
      ) |>
      # remove pattern column
      dplyr::select(-pattern)
  }

  return(ecotaxa_file)
}

#' @title Parse the cruise ID column of an EcoTaxa file of type UVP
#'
#' @description Parses the `cruise_id` column of an EcoTaxa file of
#' type UVP.
#'
#' @param ecotaxa_file (character) The unquoted name of a tibble or data frame
#' in the R environment that reflecte the data in the EcoTaxa file.
#' @param pattern (list) A list of UVP patterns to match.
#' @param debug (logical) A logical value indicating whether to enable debug
#' mode. Defaults to `FALSE`.
#'
#' @note A helper function to \code{parse_cruise_id}
#'
#' @importFrom dplyr select any_of filter vars
#' @importFrom purrr map2 reduce walk
#' @importFrom pointblank create_agent col_vals_not_null interrogate
#' @importFrom glue glue
#'
#' @return A data frame or tibble of the input EcoTaxa data with additional
#' columns reflecting the parsed values of `column_id`.
#'
extract_uvp_columns <- function(
  ecotaxa_file,
  pattern = uvp_patterns,
  debug   = FALSE
) {

  ecotaxa_file$year          <- NA
  ecotaxa_file$month         <- NA
  ecotaxa_file$day           <- NA
  ecotaxa_file$hour          <- NA
  ecotaxa_file$minute        <- NA
  ecotaxa_file$second        <- NA
  ecotaxa_file$millisecond   <- NA
  ecotaxa_file$object_number <- NA

  # extract components for flowcam_pattern
  matches_uvp <- base::regmatches(
    ecotaxa_file$object_id,
    base::regexec(pattern$uvp_pattern, ecotaxa_file$object_id)
  )

  ecotaxa_file$year <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[2]),
    no   = ecotaxa_file$year
  )
  ecotaxa_file$month <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[3]),
    no   = ecotaxa_file$month
  )
  ecotaxa_file$day <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[4]),
    no   = ecotaxa_file$day
  )
  ecotaxa_file$hour <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[5]),
    no   = ecotaxa_file$hour
  )
  ecotaxa_file$minute <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[6]),
    no   = ecotaxa_file$minute
  )
  ecotaxa_file$second <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[7]),
    no   = ecotaxa_file$second
  )
  ecotaxa_file$millisecond <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[8]),
    no   = ecotaxa_file$millisecond
  )
  ecotaxa_file$object_number <- base::ifelse(
    test = base::sapply(matches_uvp, function(x) base::length(x) > 1),
    yes  = base::sapply(matches_uvp, function(x) x[9]),
    no   = ecotaxa_file$object_number
  )

  patterns <- list(
    pattern$uvp_pattern
  )

  pattern_names <- c(
    "uvp_pattern"
  )

  # iterate over patterns and pattern_names
  ecotaxa_file$pattern <- NA
  ecotaxa_file$pattern <- purrr::reduce(
    .x = purrr::map2(
      patterns,
      pattern_names,
      ~ {
        base::ifelse(
          test = base::sapply(
            base::regmatches(
              ecotaxa_file$object_id,
              base::regexec(.x, ecotaxa_file$object_id)
            ),
            function(x) base::length(x) > 1
          ),
          yes = .y,
          no = ecotaxa_file$pattern
        )
      }
    ),
    .f = ~ ifelse(
      test = is.na(.x),
      yes  = .y,
      no   = .x
    )
  )

  agent <- pointblank::create_agent(tbl = ecotaxa_file) |>
    pointblank::col_vals_not_null(
      columns = dplyr::vars(
        year,
        month,
        day,
        hour,
        minute,
        second,
        millisecond,
        object_number
      ),
      label = "uvp_cols"
    ) |>
    pointblank::interrogate()

  if (any(agent$validation_set$all_passed == FALSE)) {
    failed_indices <- which(agent$validation_set$all_passed == FALSE)

    purrr::walk(
      .x = failed_indices,
      .f = function(i) {
        failed_brief <- agent$validation_set$brief[i]
        failed_rows <- agent$validation_set$n_failed[i]
        message(glue::glue(
          "validation failed for: {failed_brief} ({failed_rows} rows failed)"
        ))
      }
    )
  }

  uvp_cols <- c(
    "object_id",
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "second",
    "millisecond",
    "object_number"
  )

  if (debug == TRUE) {
    print(agent)

    ecotaxa_file <- ecotaxa_file |>
      dplyr::select(dplyr::any_of(uvp_cols))

  } else {

    ecotaxa_file <- ecotaxa_file |>
      # remove any parsed columns that are empty
      dplyr::select(
        -dplyr::any_of(
          uvp_cols[sapply(ecotaxa_file[uvp_cols], function(x) all(is.na(x)))]
        )
      ) |>
      # remove pattern column
      dplyr::select(-pattern)

  }

  return(ecotaxa_file)

}