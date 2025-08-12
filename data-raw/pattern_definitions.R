## code to prepare `pattern_definitions` dataset goes here

data_file <- system.file(
  "extdata",
  "pattern_definitions.csv",
  package  = "ecotaxaLoadR",
  mustWork = TRUE
)

pattern_definitions <- readr::read_csv(data_file)

usethis::use_data(pattern_definitions, overwrite = TRUE)