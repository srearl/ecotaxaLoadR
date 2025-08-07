# ecotaxaLoadR

**Minimally process and format EcoTaxa resources for marine ecological analyses**

[![R-CMD-check](https://github.com/srearl/ecotaxaLoadR/workflows/R-CMD-check/badge.svg)](https://github.com/srearl/ecotaxaLoadR/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

`ecotaxaLoadR` is an R package designed to streamline the processing and analysis of marine plankton data from multiple sources. It provides comprehensive tools for handling EcoTaxa datasets, MOCNESS PRO files, and Particle Image Data (PID) files, with a focus on data validation, temporal annotations, and standardized formatting for downstream ecological analyses.

## Key Features

### 🌊 **EcoTaxa Data Processing**
- Load and validate EcoTaxa TSV files with comprehensive error handling
- Parse cruise identifiers and extract metadata automatically
- Convert measurements to standardized units (area, length, volume)
- Calculate ecological metrics (density, abundance, biomass proxies)
- Data quality validation with detailed reporting

### 🎣 **MOCNESS PRO File Processing**
- Batch process multiple PRO files from MOCNESS deployments
- Extract comprehensive metadata (tow information, instrument calibrations)
- **Enhanced parsing for diverse tow line formats** (handles both "MOC" and "M" prefixes, various vessel name formats)
- Convert decimal day-of-year timestamps to proper datetime objects
- Automatic timezone detection based on geographic coordinates
- **Optional day/night annotation** for temporal ecological analyses
- Depth, temperature, and position data integration

### 🔬 **Particle Image Data (PID) Analysis**
- Parse PID files from zooplankton imaging systems
- **Intelligent SampleId parsing** (automatic extraction of cruise, MOC, and net information)
- **Date format standardization** (converts various date formats to ISO standard)
- Extract image metadata and measurement attributes
- Convert wide-format data to analysis-ready long format
- Batch processing with error handling and progress reporting

### ⏰ **Temporal Analysis Tools**
- Day/night classification based on solar position calculations
- Timezone-aware datetime conversions
- Temporal data validation and consistency checks

## Installation

Install the development version from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install ecotaxaLoadR
devtools::install_github("srearl/ecotaxaLoadR")
```

## Quick Start

### Loading EcoTaxa Data

```r
library(ecotaxaLoadR)

# Load EcoTaxa data with day/night annotation
eco_data <- ecotaxaLoadR::load_eco_taxa(
  file_path = "path/to/ecotaxa_export.tsv",
  daynight = TRUE,
  debug = FALSE
)

# View processed data structure
str(eco_data)
```

### Processing MOCNESS PRO Files

```r
# Process all PRO files in a directory with day/night annotation
pro_data <- ecotaxaLoadR::load_pro_files("path/to/pro/files/", daynight = TRUE)

# Process a single PRO file with day/night annotation
single_pro <- ecotaxaLoadR::ingest_pro_file("MOC1_01A.PRO", daynight = TRUE)

# Works with different filename formats:
# - Traditional: MOC8_08A.PRO, moc5_05A.PRO  
# - Compact: M35_01A.PRO, m12_03B.PRO

# Check processing summary
attr(pro_data, "processing_summary")
```

### Analyzing PID Files

```r
# Batch process PID files
pid_results <- ecotaxaLoadR::load_pid_files("path/to/pid/files/")

# Access different data components
metadata <- pid_results$metadata
image_records <- pid_results$records
attributes <- pid_results$attributes

# View processing summary
cat("Processed", pid_results$file_count, "files\n")

# Check for automatically parsed sample information
parsed_fields <- metadata[metadata$section_name == "parsed_sample", ]
print(parsed_fields)
```

### Day/Night Classification

```r
# Add day/night classification to existing data
annotated_data <- ecotaxaLoadR::annotate_daytime(your_dataframe)

# The function adds an 'is_day' column based on:
# - Geographic coordinates (object_lat, object_lon)
# - Date and time (object_date, object_time)
# - Solar position calculations
```

## Data Formats Supported

### EcoTaxa TSV Files
- Standard EcoTaxa export format
- Automatic detection of MOCNESS data patterns
- Validation of required fields (`object_id`, coordinates, timestamps)

### MOCNESS PRO Files
- Header metadata extraction (tow info, instruments, calibrations)
- **Enhanced tow line parsing** supporting multiple formats:
  - `% Tow: 13 Sally Ride SR2408` (vessel with spaces)
  - `% Tow: 1 AE AE2214` (compact vessel format)
  - `% Tow: 5 R/V Atlantis AT2023` (vessel with special characters)
- **Flexible MOC number extraction** from filenames:
  - Traditional: `MOC8_08A.PRO` → MOC 8
  - Compact: `M35_01A.PRO` → MOC 35
- Tabular data with time, position, depth, and environmental variables
- Automatic timezone detection and conversion

### PID Files (Particle Image Data)
- Section-based metadata parsing
- **Automatic SampleId parsing** extracts:
  - Cruise identifier (e.g., "sr2407", "ab1234")
  - MOC number (e.g., "6", "13") 
  - Net number (e.g., "3", "8")
- **Date standardization** converts formats like "20240504-2248" to "2024-05-04"
- Image measurement attributes
- Support for multiple scanning configurations

## Key Functions

| Function | Purpose | New Features |
|----------|---------|--------------|
| `ecotaxaLoadR::load_eco_taxa()` | Load and process EcoTaxa TSV files | |
| `ecotaxaLoadR::load_pro_files()` | Batch process MOCNESS PRO files | **Added `daynight` parameter** |
| `ecotaxaLoadR::ingest_pro_file()` | Process individual PRO files | **Enhanced filename parsing, daynight support** |
| `ecotaxaLoadR::load_pid_files()` | Batch process PID files | **Automatic SampleId parsing** |
| `ecotaxaLoadR::annotate_daytime()` | Add day/night classification | |

## Data Validation

The package includes comprehensive data validation:

- **Temporal consistency**: Ensures single date per cruise-MOC deployment
- **Spatial consistency**: Validates coordinate consistency within deployments
- **Data completeness**: Checks for required fields and valid ranges
- **Unit conversions**: Automatic conversion based on image resolution settings
- **Pattern validation**: Validates SampleId patterns and date formats in PID files

## Example Workflows

### Complete MOCNESS Analysis Pipeline

```r
# 1. Load EcoTaxa data with day/night annotation
eco_data <- ecotaxaLoadR::load_eco_taxa("ecotaxa_export.tsv", daynight = TRUE)

# 2. Process corresponding PRO files with day/night annotation
pro_data <- ecotaxaLoadR::load_pro_files("pro_files/", daynight = TRUE)

# 3. Process PID files with automatic sample parsing
pid_data <- ecotaxaLoadR::load_pid_files("pid_files/")

# 4. Access parsed sample information
sample_info <- pid_data$metadata %>%
  dplyr::filter(section_name == "parsed_sample") %>%
  dplyr::select(scan_id, key, value) %>%
  tidyr::pivot_wider(names_from = key, values_from = value)

# 5. Combine datasets for integrated analysis
# (your analysis code here)
```

### PID File Analysis with Sample Information

```r
# Process PID files and extract sample metadata
pid_results <- ecotaxaLoadR::load_pid_files("pid_files/")

# View automatically parsed sample fields
sample_metadata <- pid_results$metadata %>%
  dplyr::filter(section_name == "parsed_sample") %>%
  dplyr::select(scan_id, key, value)

# Check for different cruises and deployments
cruise_summary <- sample_metadata %>%
  dplyr::filter(key == "cruise") %>%
  dplyr::count(value, name = "n_scans")

print(cruise_summary)
```

### Quality Control Workflow

```r
# Load data with validation
eco_data <- ecotaxaLoadR::load_eco_taxa("data.tsv")

# Check for any validation issues
# (validation results are reported automatically)

# Filter and clean data as needed
clean_data <- eco_data %>%
  dplyr::filter(!is.na(object_area_mm2)) %>%
  dplyr::filter(object_depth_max > object_depth_min)
```

## Recent Updates

### Version 0.0.0.9000
- **Enhanced PRO file processing**: Added `daynight` parameter to `ecotaxaLoadR::load_pro_files()` 
- **Improved filename parsing**: Support for "M" prefix PRO files (e.g., `M35_01A.PRO`)
- **Flexible tow line formats**: Handle various vessel name and cruise ID formats
- **Automatic SampleId parsing**: Extract cruise, MOC, and net information from PID files
- **Date standardization**: Convert various date formats to ISO standard in PID files
- **Enhanced error handling**: Graceful handling of parsing failures and edge cases

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use `ecotaxaLoadR` in your research, please cite:

```
Earl, S. (2025). ecotaxaLoadR: Minimally process and format EcoTaxa resources. 
R package version 0.0.0.9000. https://github.com/srearl/ecotaxaLoadR
```