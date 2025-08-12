#' Pattern definitions for parsing EcoTaxa object_id strings
#'
#' A dataset containing regular expression patterns used to parse object_id 
#' strings from EcoTaxa exports for different instrument types (MOC, FlowCam, UVP).
#' Each pattern is designed to extract specific components like cruise ID, 
#' deployment details, and photo identifiers from the object_id field.
#'
#' @format A data frame with 11 rows and 7 variables:
#' \describe{
#'   \item{type}{Character. Instrument type - one of "moc", "flowcam", or "uvp"}
#'   \item{iteration}{Numeric. Pattern iteration number within each instrument type}
#'   \item{regex}{Character. Regular expression pattern for matching object_id strings}
#'   \item{lab_split}{Logical. TRUE if pattern captures lab split information (a/b), 
#'                    FALSE or NA if no lab split is captured}
#'   \item{notes}{Character. Additional notes about pattern usage and limitations}
#'   \item{examples}{Character. Example object_id string that matches the pattern}
#'   \item{datasets}{Character. Dataset names where this pattern is commonly found}
#' }
#'
#' @details The patterns are used by \code{\link{parse_cruise_id}} to parse
#' EcoTaxa object_id strings into component parts. Different patterns handle
#' different naming conventions.
#' 
#' FlowCam patterns handle numeric cruise identifiers with depth, niskin, 
#' and magnification information.
#' 
#' UVP patterns handle timestamp-based object identifiers.
#' 
#' Note that some patterns require downstream digit extraction as indicated 
#' in the notes column.
#'
#' @source Patterns derived from analysis of EcoTaxa exports from various
#' oceanographic datasets including aggregates, bioscope, and hypoxia.
#'
#' @seealso 
#' \code{\link{parse_cruise_id}} for the function that uses these patterns
#'
#' @note Raw data are available in the package extdata directory, which is the
#' resource that should be edited. As these data are stored also in the package
#' data directory, they can be accessed programmatically via
#' \code{ecotaxaLoadR::pattern_definitions};
#' \code{\link{data-raw\patter_definitions}} should be sourced after any edits
#' to update the package dataset.
#'
"pattern_definitions"
