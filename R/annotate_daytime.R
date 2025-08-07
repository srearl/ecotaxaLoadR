#' @title Annotate records as day or night based on position and datetime
#'
#' @description \code{annotate_daytime} annotates whether a given observation
#'   was collected during the day or night based on position and datetime.
#'   The function can handle two data source types:
#'   \itemize{
#'     \item EcoTaxa data: requires columns \code{object_lat}, \code{object_lon}, 
#'           \code{object_date}, and \code{object_time}
#'     \item PRO data: requires columns \code{lat}, \code{lon}, 
#'           and \code{datetime_gmt}
#'   }
#'   The function automatically detects the data source based on available columns.
#'
#' @note This function uses parallel processing via the \code{furrr} 
#'   package. Use \code{setup_parallel_ecotaxa()} to configure parallel 
#'   processing before calling this function, or it will run sequentially.
#'
#' @param data (data.frame) Input data frame containing either EcoTaxa format
#'   or PRO data format.
#'
#' @return The input data as a data frame with an additional column \code{is_day}
#'   indicating whether the observation occurred during daytime (TRUE) based on
#'   nautical twilight calculations.
#'
#' @details The function performs day/night classification using solar position
#'   calculations with nautical twilight as the threshold. For EcoTaxa data,
#'   the function operates on distinct combinations of position and datetime
#'   to optimize performance, then joins results back. For PRO data, the function
#'   processes all records individually without deduplication. For EcoTaxa data,
#'   the function assumes that date and time are in GMT. For PRO data, it uses
#'   the provided \code{datetime_gmt} column directly.
#'
#' @importFrom dplyr summarize left_join select mutate n distinct
#' @importFrom furrr future_pmap_lgl
#' @importFrom SunCalcMeeus is_daytime
#' @importFrom tibble tibble
#' @importFrom pointblank create_agent col_vals_equal interrogate vars
#'
#' @examples
#' \dontrun{
#'
#' # Set up parallel processing for all ecotaxaLoadR functions
#' setup_parallel_ecotaxa(workers = 4)
#'
#' # Example with EcoTaxa data
#' eco_taxa_df <- data.frame(
#'   cruise = c("cruise1", "cruise1"),
#'   moc = c("moc1", "moc1"),
#'   object_lat = c(34.5, 34.5),
#'   object_lon = c(-120.5, -120.5),
#'   object_date = c("2025-03-26", "2025-03-26"),
#'   object_time = c("12:00:00", "13:00:00")
#' )
#' result_eco <- annotate_daytime(eco_taxa_df)
#'
#' # Example with PRO data
#' pro_data <- data.frame(
#'   lat = c(27.32393, 27.32392),
#'   lon = c(-111.28438, -111.28438),
#'   datetime_gmt = as.POSIXct(c("2024-05-04 18:03:09", "2024-05-04 18:03:11")),
#'   temp = c(23.423, 23.369)
#' )
#' result_pro <- annotate_daytime(pro_data)
#'
#' # Reset when done
#' reset_parallel_ecotaxa()
#' }
#'
#' @export
#'
annotate_daytime <- function(data) {

  # Define required columns for each data source type
  ecotaxa_cols <- c("object_lat", "object_lon", "object_date", "object_time")
  pro_cols <- c("lat", "lon", "datetime_gmt")
  
  # Determine data source type
  has_ecotaxa_cols <- all(ecotaxa_cols %in% colnames(data))
  has_pro_cols <- all(pro_cols %in% colnames(data))
  
  # Validate that we have one of the required column sets
  if (!has_ecotaxa_cols && !has_pro_cols) {
    stop(paste(
      "Input data must contain either:",
      "\n  EcoTaxa format columns:", paste(ecotaxa_cols, collapse = ", "),
      "\n  OR PRO format columns:", paste(pro_cols, collapse = ", "),
      "\n\nProvided columns:", paste(colnames(data), collapse = ", ")
    ))
  }
  
  # If both column sets are present, prioritize EcoTaxa format
  if (has_ecotaxa_cols && has_pro_cols) {
    warning("Both EcoTaxa and PRO format columns detected. Using EcoTaxa format.")
    data_source <- "ecotaxa"
  } else if (has_ecotaxa_cols) {
    data_source <- "ecotaxa"
  } else {
    data_source <- "pro"
  }
  
  message("Detected data source: ", data_source)
  
  nrow_start <- nrow(data)
  
  # Process data based on source type
  if (data_source == "ecotaxa") {
    
    # EcoTaxa data processing (with optimization via distinct values)
    distinct_time_pos <- data |>
      dplyr::distinct(
        object_lat,
        object_lon,
        object_date,
        object_time
      )
    
    distinct_time_pos$is_day <- furrr::future_pmap_lgl(
      list(
        date = as.POSIXct(
          paste(distinct_time_pos$object_date, distinct_time_pos$object_time),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        lat = distinct_time_pos$object_lat,
        lon = distinct_time_pos$object_lon
      ),
      function(date, lat, lon) {
        SunCalcMeeus::is_daytime(
          date = date,
          geocode = tibble::tibble(lat = lat, lon = lon),
          twilight = "nautical"
        )
      }
    )
    
    # Join results back to original data
    data <- data |>
      dplyr::left_join(
        y = distinct_time_pos,
        by = c("object_lat", "object_lon", "object_date", "object_time")
      ) |>
      dplyr::mutate(is_day = as.logical(is_day))
      
  } else {
    
    # PRO data processing (apply to all records, no deduplication)
    data$is_day <- furrr::future_pmap_lgl(
      list(
        date = data$datetime_gmt,
        lat = data$lat,
        lon = data$lon
      ),
      function(date, lat, lon) {
        SunCalcMeeus::is_daytime(
          date = date,
          geocode = tibble::tibble(lat = lat, lon = lon),
          twilight = "nautical"
        )
      }
    )
    
    # Ensure is_day is logical
    data <- data |>
      dplyr::mutate(is_day = as.logical(is_day))
  }
  
  # Data validation - ensure row count hasn't changed
  agent <- pointblank::create_agent(tbl = data) |>
    pointblank::col_vals_equal(
      columns = pointblank::vars(nrow),
      value = nrow_start,
      preconditions = function(x) {
        x |>
          dplyr::summarize(nrow = dplyr::n())
      }
    ) |>
    pointblank::interrogate()

  if (!agent$validation_set$all_passed) {
    stop("The number of rows in the input and output do not match.")
  }
  
  message("Successfully annotated ", nrow(data), " records with day/night classificationn")

  return(data)
}