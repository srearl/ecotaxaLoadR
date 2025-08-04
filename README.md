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
- Convert decimal day-of-year timestamps to proper datetime objects
- Automatic timezone detection based on geographic coordinates
- Depth, temperature, and position data integration

### 🔬 **Particle Image Data (PID) Analysis**
- Parse PID files from zooplankton imaging systems
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
eco_data <- load_eco_taxa(
  file_path = "path/to/ecotaxa_export.tsv",
  daynight = TRUE,
  debug = FALSE
)

# View processed data structure
str(eco_data)
```

### Processing MOCNESS PRO Files

```r
# Process all PRO files in a directory
pro_data <- load_pro_files("path/to/pro/files/")

# Process a single PRO file
single_pro <- ingest_pro_file("MOC1_01A.PRO")

# Check processing summary
attr(pro_data, "processing_summary")
```

### Analyzing PID Files

```r
# Batch process PID files
pid_results <- load_pid_files("path/to/pid/files/")

# Access different data components
metadata <- pid_results$metadata
image_records <- pid_results$records
attributes <- pid_results$attributes

# View processing summary
cat("Processed", pid_results$file_count, "files\n")
```

### Day/Night Classification

```r
# Add day/night classification to existing data
annotated_data <- annotate_daytime(your_dataframe)

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
- Tabular data with time, position, depth, and environmental variables
- Automatic timezone detection and conversion

### PID Files (Particle Image Data)
- Section-based metadata parsing
- Image measurement attributes
- Support for multiple scanning configurations

## Key Functions

| Function | Purpose |
|----------|---------|
| `load_eco_taxa()` | Load and process EcoTaxa TSV files |
| `load_pro_files()` | Batch process MOCNESS PRO files |
| `ingest_pro_file()` | Process individual PRO files |
| `load_pid_files()` | Batch process PID files |
| `annotate_daytime()` | Add day/night classification |

## Data Validation

The package includes comprehensive data validation:

- **Temporal consistency**: Ensures single date per cruise-MOC deployment
- **Spatial consistency**: Validates coordinate consistency within deployments
- **Data completeness**: Checks for required fields and valid ranges
- **Unit conversions**: Automatic conversion based on image resolution settings

## Example Workflows

### Complete MOCNESS Analysis Pipeline

```r
# 1. Load EcoTaxa data
eco_data <- load_eco_taxa("ecotaxa_export.tsv", daynight = TRUE)

# 2. Process corresponding PRO files
pro_data <- load_pro_files("pro_files/")

# 3. Combine datasets for integrated analysis
# (your analysis code here)
```

### Quality Control Workflow

```r
# Load data with validation
eco_data <- load_eco_taxa("data.tsv")

# Check for any validation issues
# (validation results are reported automatically)

# Filter and clean data as needed
clean_data <- eco_data %>%
  filter(!is.na(object_area_mm2)) %>%
  filter(object_depth_max > object_depth_min)
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use `ecotaxaLoadR` in your research, please cite:

```
Earl, S. (2025). ecotaxaLoadR: Minimally process and format EcoTaxa resources. 
R package version 0.0.0.9000. https://github.com/srearl/ecotaxaLoadR
```

## Support

For questions, bug reports, or feature requests, please [open an issue](https://github.com/srearl/ecotaxaLoadR/issues) on GitHub.