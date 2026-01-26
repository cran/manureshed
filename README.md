# manureshed 🌾💧

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/manureshed)](https://CRAN.R-project.org/package=manureshed)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/manureshed)](https://cran.r-project.org/package=manureshed)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![pkgdown](https://github.com/Exelegch/manureshed-docs/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Exelegch/manureshed-docs/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

> **Spatiotemporal Nutrient Balance Analysis Across Agricultural and Municipal Systems**

An R package for analyzing integrated agricultural-municipal nutrient flows and watershed management. The **manureshed** framework enables comprehensive analysis of nitrogen and phosphorus balances across multiple spatial scales (county, HUC8, HUC2) with seamless integration of wastewater treatment plant (WWTP) discharge data.

## 📋 Table of Contents
- [Key Features](#-key-features)
- [Quick Links](#-quick-links)
- [Installation](#-installation)
- [Quick Start](#-quick-start)
- [Documentation](#-documentation)
- [Real-World Examples](#-real-world-examples)
- [What Makes manureshed Special](#-what-makes-manureshed-special)
- [Data Sources](#-data-sources)
- [System Requirements](#-system-requirements)
- [Citation](#-citation)
- [Contact](#-contact)
- [License](#-license)
- [Acknowledgments](#-acknowledgments)

## 🔗 Quick Links


- **Documentations**: https://exelegch.github.io/manureshed-docs/
- **CRAN**: https://cran.r-project.org/package=manureshed
- **OSF Data Repository**: https://osf.io/g39xa/
- **GitHub**: https://github.com/cwru-sdle/manureshed
- **Bug Reports**: https://github.com/cwru-sdle/manureshed/issues
- **Methodology Paper**: https://doi.org/10.1016/j.resconrec.2025.108697

## ✨ Key Features

### 🗺️ **Multi-Scale Spatial Analysis**
- **County-level analysis**: 3,000+ US counties
- **HUC8 watersheds**: 2,000+ hydrologic units
- **HUC2 regions**: 18 major water resource regions
- **Seamless scale switching** with consistent methodology

### 🌾 **Comprehensive Nutrient Analysis**
- **Nitrogen balance**: Agricultural surplus/deficit with 0.5 availability factor for manure
- **Phosphorus balance**: Direct calculation of nutrient flows
- **Dual-nutrient analysis**: Analyze both N and P simultaneously
- **Classification system**: Source, Sink Deficit, Sink Fertilizer, Within Watershed/County, Excluded

### 💧 **WWTP Integration**
- **2007-2016 built-in data**: Pre-processed EPA discharge data for both N and P
- **Flexible data loading**: Easy integration of custom WWTP data for any year
- **Unit conversion**: Automatic handling of kg, lbs, pounds, tons
- **Impact analysis**: Before/after comparison with WWTP integration
- **Spatial influence mapping**: Visualize WWTP contribution to nutrient loads

### 📊 **Rich Visualizations**
- **Classification maps**: Spatial distribution of nutrient sources and sinks
- **WWTP facility maps**: Point locations with load-based sizing
- **Influence maps**: Proportion of WWTP contribution to total loads
- **Transition networks**: Spatial probability flows between classifications
- **Comparison plots**: Before/after WWTP integration analysis

### 🔄 **End-to-End Workflows**
- **One-function analysis**: `quick_analysis()` for complete workflow
- **Batch processing**: Multi-year analysis with `batch_analysis_years()`
- **State-specific analysis**: Focus on individual states or regions
- **Custom thresholds**: Flexible cropland exclusion criteria
- **Automated outputs**: Maps, data files, and metadata generation

### ⚡ **Performance & Data Management**
- **On-demand data loading**: Downloads from OSF only when needed
- **Smart caching**: Automatic local caching for repeated use
- **Memory efficient**: Optimized for CONUS-scale analysis
- **Package size <1MB**: Full datasets (~40MB) downloaded separately
- **Reproducible**: Permanent DOI for data versioning

## 📦 Installation

### From CRAN (Recommended)
```r
# Install the stable version from CRAN
install.packages("manureshed")
```

### Development Version from GitHub
```r
# Install development version with latest features
# install.packages("devtools")
devtools::install_github("cwru-sdle/manureshed")
```

### Install Recommended Packages
```r
# Enhanced visualization and spatial analysis
install.packages(c("ggplot2", "sf", "dplyr", "tidyr", "viridis"))

# Optional packages for advanced features
install.packages(c("tigris", "nhdplusTools", "igraph", "cowplot"))
```

## 🚀 Quick Start

### Load Package and Check Data
```r
library(manureshed)

# Check what data is available
check_builtin_data()

# Download all datasets (optional, ~40MB total)
download_all_data()

# Test connection to data repository
test_osf_connection()

# Check package health
health_check()
```

### Basic Analysis Examples
```r
# 1. Quick analysis with automatic visualizations
results <- quick_analysis(
  scale = "county",
  year = 2016,
  nutrients = "nitrogen",
  include_wwtp = TRUE
)

# 2. Comprehensive analysis for both nutrients
results_both <- run_builtin_analysis(
  scale = "huc8",
  year = 2016,
  nutrients = c("nitrogen", "phosphorus"),
  include_wwtp = TRUE,
  output_dir = "analysis_results"
)

# 3. Historical analysis (2007-2016 WWTP available!)
historical <- run_builtin_analysis(
  scale = "county",
  year = 2010,
  nutrients = "nitrogen",
  include_wwtp = TRUE
)

# 4. Multi-year analysis
batch_results <- batch_analysis_years(
  scale = "huc8",
  years = c(2010, 2012, 2014, 2016),
  nutrients = "phosphorus",
  include_wwtp = TRUE
)

# 5. State-specific analysis
ohio <- run_state_analysis(
  state = "OH",
  scale = "county",
  year = 2016,
  nutrients = c("nitrogen", "phosphorus"),
  include_wwtp = TRUE
)
```

## 📖 Documentation

### Vignettes and Tutorials

- [**Interactive Dashboard**](https://exelegch.github.io/manureshed-docs/articles/dashboard-guide.html) - User-friendly interface for non-coders
- [**Getting Started**](https://exelegch.github.io/manureshed-docs/articles/getting-started.html) - Package overview and basic workflows
- [**Data Integration**](https://exelegch.github.io/manureshed-docs/articles/data-integration.html) - Using custom WWTP data
- [**Visualization Guide**](https://exelegch.github.io/manureshed-docs/articles/visualization-guide.html) - Mapping and plotting options
- [**Advanced Features**](https://exelegch.github.io/manureshed-docs/articles/advanced-features.html) - State analysis, custom thresholds, parallel processing

### Function Reference
```r
# View all available functions
help(package = "manureshed")

# Key workflow functions
?run_builtin_analysis
?quick_analysis
?batch_analysis_years
?run_state_analysis

# Data loading functions
?load_builtin_nugis
?load_builtin_wwtp
?load_builtin_boundaries

# Custom data integration
?load_user_wwtp
?wwtp_clean_data

# Visualization functions
?map_agricultural_classification
?map_wwtp_points
?map_wwtp_influence

# Utility functions
?check_builtin_data
?citation_info
?health_check
```

## 🎯 Real-World Examples

### Regional Nutrient Analysis
```r
# Analyze nitrogen flows in the Great Lakes region
great_lakes <- run_builtin_analysis(
  scale = "huc8",
  year = 2016,
  nutrients = "nitrogen",
  include_wwtp = TRUE,
  output_dir = "great_lakes_analysis"
)

# Extract key statistics
summary_stats <- create_classification_summary(
  great_lakes$nitrogen$agri_classified,
  great_lakes$nitrogen$integrated_result
)

# Visualize impact
plot_before_after_comparison(summary_stats)
```

### Watershed Management Planning
```r
# Load watershed boundaries
huc8_boundaries <- load_builtin_boundaries("huc8")

# Analyze nutrient balance
watershed_balance <- run_builtin_analysis(
  scale = "huc8",
  year = 2016,
  nutrients = c("nitrogen", "phosphorus"),
  include_wwtp = TRUE
)

# Calculate transition probabilities
transitions_n <- calculate_transition_probabilities(
  watershed_balance$nitrogen$agri_classified,
  watershed_balance$nitrogen$integrated_result
)

# Visualize network flows
create_network_plot(
  transitions_n,
  nutrient = "nitrogen",
  title = "HUC8 Nitrogen Classification Transitions"
)
```

### Custom WWTP Data Integration
```r
# Load your own WWTP data for any year
custom_wwtp <- load_user_wwtp(
  file_path = "my_wwtp_2018.csv",
  nutrient = "nitrogen",
  column_mapping = list(
    facility = "Facility_Name",
    latitude = "Lat",
    longitude = "Long",
    load = "Total_N_kg"
  ),
  load_units = "kg"
)

# Integrate with agricultural data
custom_results <- run_builtin_analysis(
  scale = "county",
  year = 2016,  # Use 2016 agricultural data
  nutrients = "nitrogen",
  include_wwtp = TRUE,
  custom_wwtp_nitrogen = custom_wwtp
)
```

### Multi-Year Trend Analysis
```r
# Analyze trends across multiple years
trend_results <- batch_analysis_years(
  scale = "county",
  years = seq(2010, 2016, by = 2),
  nutrients = c("nitrogen", "phosphorus"),
  include_wwtp = TRUE,
  output_dir = "trend_analysis"
)

# Available years for WWTP: 2007-2016
list_available_years()
```

### State-Level Analysis
```r
# Quick state analysis with maps
iowa_results <- quick_state_analysis(
  state = "IA",
  scale = "county",
  year = 2016,
  nutrients = "nitrogen"
)

# Multiple states
midwest_states <- c("IA", "IL", "IN", "OH", "MI")
midwest_results <- lapply(midwest_states, function(state) {
  run_state_analysis(
    state = state,
    scale = "county",
    year = 2016,
    nutrients = "nitrogen",
    include_wwtp = TRUE
  )
})
names(midwest_results) <- midwest_states
```

## 🌟 What Makes manureshed Special

### 1. **Integrated Framework**
The manureshed concept represents a paradigm shift from treating agricultural and municipal nutrient systems separately to analyzing them as integrated socio-environmental systems. This enables:
- **Circular economy insights**: Identify opportunities for nutrient recycling
- **Regional planning**: Optimize nutrient flows at watershed scales
- **Policy analysis**: Evaluate impact of nutrient management strategies

### 2. **Comprehensive Spatial Coverage**
- **CONUS-scale**: Analyze entire continental United States
- **Multi-resolution**: Compare patterns across county, watershed, and regional scales
- **Consistent methodology**: Same classification approach across all scales

### 3. **Temporal Depth**
- **30-year agricultural data**: NuGIS 1987-2016
- **10-year WWTP data**: EPA discharge data 2007-2016
- **Trend analysis**: Track changes in nutrient management over time

### 4. **Research-Ready**
Designed specifically for reproducible research:
- **Permanent data archive**: DOI-based OSF repository
- **Version control**: Fixed data versions for reproducibility
- **Complete workflow**: From raw data to publication figures
- **Metadata tracking**: Analysis parameters saved automatically

### 5. **Flexible Data Integration**
- **Built-in datasets**: Ready to use immediately
- **Custom data support**: Easy integration of user data
- **Format flexibility**: Handles varying EPA data formats
- **Unit conversion**: Automatic standardization

### 6. **Rich Analytical Tools**
- **Classification system**: Five-category nutrient balance framework
- **Spatial analysis**: Transition probabilities and network flows
- **Impact assessment**: Quantify WWTP influence on nutrient balance
- **Comparison tools**: Before/after integration analysis

## 📊 Data Sources

### NuGIS Agricultural Data (1987-2016)
**Source**: The Fertilizer Institute (TFI) and Plant Nutrition Canada (PNC)

**Website**: https://nugis.tfi.org/tabular_data

**Components**:
- County-level crop and livestock data (USDA Census of Agriculture)
- Fertilizer use data (AAPFCO)
- Manure production estimates
- Nutrient removal by crops
- Biological nitrogen fixation

**Spatial Scales**: County, HUC8, HUC2

**Citation**: Use `citation_info()` for complete attribution

### EPA WWTP Discharge Data (2007-2016)
**Source**: U.S. Environmental Protection Agency

**System**: Discharge Monitoring Report (DMR) Loading Tool via ECHO

**Website**: https://echo.epa.gov/trends/loading-tool/water-pollution-search

**Parameters**:
- Total nitrogen loads
- Total phosphorus loads
- Facility locations and identifiers
- Permit information

**Pre-processing**: Cleaned and quality-controlled in manureshed package

**License**: Public domain (U.S. Government work)

### Spatial Boundaries
**Sources**: 
- US Census TIGER (counties)
- USGS Watershed Boundary Dataset (HUC8, HUC2)

**Projection**: Albers Equal Area Conic (EPSG:5070)

**Coverage**: Continental United States (CONUS)

## ⚡ Performance

The package is optimized for:

- **Large-scale analysis**: Handle 3,000+ spatial units efficiently
- **Memory efficiency**: On-demand loading and smart caching
- **Cross-platform**: Tested on Windows, macOS, and Linux
- **Batch processing**: Multi-year analysis workflows
- **Quick iterations**: Cached data enables rapid re-analysis

### Performance Tips
```r
# Check package health
health_check()

# Pre-download all data for offline work
download_all_data()

# Clear cache if needed
clear_data_cache()

# Use quick_check to validate results
quick_check(results)

# For large batch analyses, process in chunks
years_chunk1 <- seq(2007, 2011)
years_chunk2 <- seq(2012, 2016)
```

## 🛠 System Requirements

### Required Dependencies
```r
# Core dependencies (automatically installed)
sf (>= 1.0-0)
dplyr (>= 1.0.0)
ggplot2 (>= 3.3.0)
tidyr (>= 1.0.0)
jsonlite
rlang
```

### Recommended Packages
```r
# Install these for full functionality
install.packages(c(
  "viridis",      # Color schemes
  "tigris",       # US boundaries
  "nhdplusTools", # Watershed tools
  "igraph",       # Network analysis
  "cowplot"       # Multi-panel plots
))
```

### Minimum R Version
- **R >= 4.0.0** recommended
- **R >= 3.5.0** minimum

## 📄 Citation

If you use manureshed in your research, please cite:

### Package Citation
```r
citation("manureshed")
```

Akanbi, O. D.; Mandayam, V.; Gupta, A.; Flynn, K. C.; Yarus, J. M.; Barcelos, E. I.; & French, R. H. (2025). manureshed: An Open-Source R Package for Scalable Temporal and Multi-Regional Analysis of Integrated Agricultural-Municipal Nutrient Flows. R package version 0.1.0. OSF Repository: https://osf.io/g39xa/

### Methodology Paper
Akanbi, O. D.; Gupta, A.; Mandayam, V.; Flynn, K. C.; Yarus, J. M.; Barcelos, E. I.; French, R. H. Towards Circular Nutrient Economies: An Integrated Manureshed Framework for Agricultural and Municipal Resource Management. *Resources, Conservation and Recycling*, 2025. https://doi.org/10.1016/j.resconrec.2025.108697

### Data Sources
Use `citation_info()` to display complete citations for:
- NuGIS agricultural data
- EPA WWTP discharge data
- Spatial boundary datasets

## 📧 Contact

- **Maintainer**: Olatunde D. Akanbi (olatunde.akanbi@case.edu)
- **Senior Maintainer**: Roger H. French (roger.french@case.edu)
- **Lead Developer**: Olatunde D. Akanbi
- **Issues & Bug Reports**: https://github.com/cwru-sdle/manureshed/issues

## 🤝 Contributing

We welcome contributions! To contribute:

1. **Report bugs**: Use GitHub Issues with reproducible examples
2. **Suggest features**: Open a discussion issue
3. **Submit pull requests**: Follow our coding standards
4. **Improve documentation**: Help make the package more accessible

### Getting Help

- 📖 **Documentation**: Check our [All Documentation](https://exelegch.github.io/manureshed-docs/)
- 🐛 **Bug Reports**: [GitHub Issues](https://github.com/cwru-sdle/manureshed/issues)
- 💬 **Questions**: Contact maintainers or open a discussion
- 📧 **Email**: olatunde.akanbi@case.edu

## 📝 License

MIT License - see LICENSE file for details.

## 🙏 Acknowledgments

### Funding
This material is based upon financial support by the National Science Foundation, EEC Division of Engineering Education and Centers, NSF Engineering Research Center for Advancing Sustainable and Distributed Fertilizer Production (CASFER), NSF 20-553 Gen-4 Engineering Research Centers award 2133576.

### Contributing Institutions
- **Solar Durability and Lifetime Extension (SDLE) Center**, Case Western Reserve University, Cleveland, Ohio, U.S.A.
- **NSF Engineering Research Center for Advancing Sustainable and Distributed Fertilizer Production (CASFER)**

### Special Thanks
- **Dr. Robert D. Sabo** (U.S. Environmental Protection Agency) for valuable contributions to conceptual development and review
- **The Fertilizer Institute (TFI)** and **Plant Nutrition Canada (PNC)** for NuGIS data
- **U.S. Environmental Protection Agency** for WWTP discharge data
- R Core Team and CRAN maintainers
- Spatial R community (`sf`, `terra`, `ggplot2` developers)

### Development Team
- Olatunde D. Akanbi (Lead Developer, ORCID: 0000-0001-7719-2619)
- Vibha Mandayam (ORCID: 0009-0008-8628-9904)
- Atharva Gupta (ORCID: 0009-0004-5372-0260)
- K. Colton Flynn (ORCID: 0000-0002-5718-1071)
- Jeffrey Yarus (ORCID: 0000-0002-9331-9568)
- Erika I. Barcelos (ORCID: 0000-0002-9273-8488)
- Roger H. French (ORCID: 0000-0002-6162-0532)

---

**Ready to get started?** Install from CRAN with `install.packages("manureshed")` and check out the [Getting Started vignette](https://exelegch.github.io/manureshed-docs/articles/getting-started.html)!

**Questions?** Run `health_check()` to verify your installation, or use `citation_info()` for publication citations.
