# ecotaxaLoadR

**Minimally process and format EcoTaxa resources for marine ecological analyses**

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

ecotaxaLoadR is an R package designed to streamline the processing and formatting of EcoTaxa data exports for marine ecological analyses. The package provides tools to load, parse, and standardize data from multiple marine imaging instruments including MOCNESS (MOC), FlowCam, and Underwater Vision Profiler (UVP).

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("srearl/ecotaxaLoadR")
```

## Data Formats Supported

### EcoTaxa Exports
- **TSV files**: Tab-separated EcoTaxa export files
- **Object ID parsing**: Automatic extraction of cruise, deployment, and sampling metadata
- **Multi-instrument support**: MOC, FlowCam, and UVP data formats
- **Validation**: Built-in data quality checks and validation

### MOCNESS Files
- **PRO files**: MOCNESS profile data with environmental parameters
- **PID files**: MOCNESS deployment metadata and sampling information
- **Flexible parsing**: Handles various filename formats and cruise conventions

### Pattern Definitions
- **Centralized pattern storage**: CSV file in `inst/extdata/pattern_definitions.csv`
- **Metadata included**: Pattern examples, lab_split indicators, dataset associations
- **Version controlled**: Patterns tracked in package source code
- **Extensible**: Add new patterns by editing CSV and rebuilding package data

## Pattern Recognition System

### Flexible Object ID Parsing
- **Centralized pattern definitions**: All regex patterns stored in `pattern_definitions` dataset
- **Multiple instrument support**: MOC, FlowCam, and UVP pattern recognition
- **Dynamic pattern matching**: Automatically detects appropriate parsing approach
- **Lab split detection**: Identifies samples with a/b laboratory splits
- **Extensible architecture**: Easy to add new patterns without code changes

The package uses a sophisticated pattern recognition system to parse EcoTaxa `object_id` strings:

```r
# View available patterns
data("pattern_definitions")
head(ecotaxaLoadR::pattern_definitions)

# Patterns automatically applied during data loading
eco_data <- ecotaxaLoadR::load_eco_taxa("data.tsv")
```

### Supported Object ID Formats

#### MOC Patterns
- example: `120815_1830_1_5_a_1_12345`
- example: `sr2407_m2_n1_d1_1_1` 
- example: `ae2112_m22_n1_d2_a_1_1`

#### FlowCam Patterns  
- example: `10414_0000_01_1_20x_d_00080`
- example: `10423_0800_22_1_20x_2_d_00116`

#### UVP Patterns
- example: `20120815-183045-123_00001`

## Key Functions

| Function | Purpose | New Features |
|----------|---------|--------------|
| `ecotaxaLoadR::load_eco_taxa()` | Load and process EcoTaxa TSV files | **Pattern-based object_id parsing** |
| `ecotaxaLoadR::parse_cruise_id()` | Parse object_id strings using pattern definitions | **Refactored with centralized patterns** |
| `ecotaxaLoadR::load_pro_files()` | Batch process MOCNESS PRO files | **Added `daynight` parameter** |
| `ecotaxaLoadR::ingest_pro_file()` | Process individual PRO files | **Enhanced filename parsing, daynight support** |
| `ecotaxaLoadR::load_pid_files()` | Batch process PID files | **Automatic SampleId parsing** |
| `ecotaxaLoadR::annotate_daytime()` | Add day/night classification | |
| `pattern_definitions` | Dataset containing all parsing patterns | **New centralized pattern system** |

## Quick Start

### Loading EcoTaxa Data

```r
library(ecotaxaLoadR)

# Load an EcoTaxa TSV file
eco_data <- ecotaxaLoadR::load_eco_taxa(
  file_path = "path/to/ecotaxa_export.tsv",
  daynight = TRUE,   # Add day/night classification
  debug = FALSE      # Set to TRUE for detailed parsing info
)

# View the processed data structure
str(eco_data)
```

### Working with MOCNESS Files

```r
# Load PRO files from a directory
pro_data <- ecotaxaLoadR::load_pro_files(
  file_path = "path/to/pro/files/",
  daynight = TRUE
)

# Load PID files
pid_data <- ecotaxaLoadR::load_pid_files(
  file_path = "path/to/pid/files/"
)
```

### Manual Object ID Parsing

```r
# Parse object_id strings directly
parsed_data <- ecotaxaLoadR::parse_cruise_id(
  ecotaxa_file = your_data,
  debug = TRUE  # Shows pattern matching details
)
```

## Pattern Management

### Viewing Available Patterns

```r
# Load pattern definitions
library(ecotaxaLoadR)

# View all patterns
data("pattern_definitions")
print(ecotaxaLoadR::pattern_definitions)

# View patterns by instrument type
moc_patterns <- ecotaxaLoadR::pattern_definitions[
  ecotaxaLoadR::pattern_definitions$type == "moc", 
]

# Check which patterns detect lab splits
lab_split_patterns <- ecotaxaLoadR::pattern_definitions[
  ecotaxaLoadR::pattern_definitions$lab_split == TRUE, 
]
```

### Adding New Patterns

To add new parsing patterns:

1. **Edit the source file**: `inst/extdata/pattern_definitions.csv`
2. **Add pattern details**: type, iteration, regex, examples, datasets
3. **Rebuild package data**: Run `usethis::use_data(pattern_definitions, overwrite = TRUE)`
4. **Update documentation**: Run `devtools::document()`
5. **Test patterns**: Use debug mode in `parse_cruise_id()`

### Testing Pattern Recognition

```r
# Test pattern matching with debug mode
result <- ecotaxaLoadR::parse_cruise_id(
  ecotaxa_file = your_data,
  debug = TRUE  # Shows detailed pattern matching information
)
```

## Example Workflows

### Standard EcoTaxa Processing

```r
library(ecotaxaLoadR)

# Load and process EcoTaxa data
data <- ecotaxaLoadR::load_eco_taxa(
  file_path = "ecotaxa_export.tsv",
  daynight = TRUE
)

# Check parsing results
table(data$pattern)  # Which patterns were used
summary(data$lab_split)  # Lab split information
```

### MOCNESS Data Integration

```r
# Load MOCNESS profile data
pro_data <- ecotaxaLoadR::load_pro_files("mocness_pro/")

# Load deployment metadata  
pid_data <- ecotaxaLoadR::load_pid_files("mocness_pid/")

# Load EcoTaxa data
eco_data <- ecotaxaLoadR::load_eco_taxa("ecotaxa_export.tsv")

# Combine datasets for analysis
combined_data <- merge(eco_data, pro_data, by = c("cruise", "moc"))
```

## Recent Updates

### Version 0.0.0.9000 (Current Branch)
- **🔄 Major Refactoring**: Centralized pattern definitions system
- **📊 Pattern Definitions Dataset**: All regex patterns now stored in `pattern_definitions` 
- **🏗️ Code Architecture**: Refactored `parse_cruise_id()` for maintainability
- **✅ Enhanced Validation**: Improved debugging and error handling
- **📝 Documentation**: Comprehensive pattern documentation and examples
- **🔧 Maintainability**: Single source of truth for all parsing patterns

### Previous Updates
- **Enhanced PRO file processing**: Added `daynight` parameter to `ecotaxaLoadR::load_pro_files()` 
- **Improved filename parsing**: Support for "M" prefix PRO files (e.g., `M35_01A.PRO`)
- **Flexible tow line formats**: Handle various vessel name and cruise ID formats
- **Automatic SampleId parsing**: Extract cruise, MOC, and net information from PID files
- **Date standardization**: Convert various date formats to ISO standard in PID files
- **Enhanced error handling**: Graceful handling of parsing failures and edge cases

## Data Output Structure

The processed data includes standardized columns:

- **Identification**: `cruise`, `moc`, `net`, `depth`, `lab_split`
- **Spatial**: `object_lat`, `object_lon` 
- **Temporal**: `object_date`, `object_time`, `daynight`
- **Technical**: `pattern` (which parsing pattern was used)
- **Taxonomic**: EcoTaxa classification hierarchy
- **Morphometric**: Object measurements and features

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests on GitHub.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Citation

If you use this package in your research, please cite:

```
Rearl, S. (2024). ecotaxaLoadR: Minimally process and format EcoTaxa resources 
for marine ecological analyses. R package version 0.0.0.9000.
```

## Acknowledgments

This package was developed to support marine ecological research and is part of ongoing efforts to standardize and streamline the processing of marine imaging data.