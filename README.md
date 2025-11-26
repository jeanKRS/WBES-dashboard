# World Bank Enterprise Surveys Dashboard

## Business Environment Benchmarking

![R Version](https://img.shields.io/badge/R-≥4.1.0-blue)
![Rhino Framework](https://img.shields.io/badge/Rhino-1.9.0-green)
![License](https://img.shields.io/badge/License-Custom-orange)

Enterprise-grade Shiny dashboard for analyzing and benchmarking business environments across 168 economies using World Bank Enterprise Survey data.

**Developed by [Kwiz Computing Technologies](https://kwizresearch.com)** | Nairobi, Kenya

---

## 🎯 Features

### Analytics Modules

| Module | Description |
|--------|-------------|
| **Overview** | Global KPIs, interactive map, regional comparisons |
| **Country Profile** | Deep-dive analysis with radar charts and time series |
| **Cross-Country Benchmark** | Compare up to 10 countries across indicators |
| **Infrastructure** | Power outages, generator usage, productivity impact |
| **Access to Finance** | Credit gaps, collateral analysis, gender disparities |
| **Data Quality** | Complete transparency on data issues and filter logic |

### Technical Highlights

- ✅ **Rhino Framework** - Enterprise-grade modular architecture
- ✅ **Comprehensive Testing** - testthat unit tests with >80% coverage target
- ✅ **Data Quality Documentation** - Full transparency on filtering logic
- ✅ **Responsive Design** - Works on desktop, tablet, and mobile
- ✅ **Reproducible Analysis** - R code provided for all transformations
- ✅ **Publication Ready** - Deploy to shinyapps.io, Posit Connect, or self-host

---

## 🚀 Quick Start

### Prerequisites

```r
# Install required packages
install.packages(c("rhino", "shiny", "bslib", "plotly", "leaflet", "DT",
                   "dplyr", "tidyr", "haven", "waiter", "logger"))
```

### Run Locally

```r
# Clone or download the project
# Navigate to project directory

# Option 1: Using Rhino
rhino::app()

# Option 2: Using shiny directly
shiny::runApp()
```

### With renv (Recommended)

```r
# Restore exact package versions
renv::restore()

# Run app
rhino::app()
```

---

## 📁 Project Structure

```
wbes_dashboard/
├── app/
│   ├── main.R                 # Main app entry point
│   ├── logic/
│   │   └── data_loader.R      # Data loading & processing
│   ├── view/
│   │   ├── mod_overview.R     # Overview module
│   │   ├── mod_country_profile.R
│   │   ├── mod_benchmark.R
│   │   ├── mod_infrastructure.R
│   │   ├── mod_finance_access.R
│   │   ├── mod_data_quality.R # Data quality documentation
│   │   └── mod_about.R
│   ├── styles/
│   │   └── main.scss          # Kwiz Research theme
│   └── static/
│       └── images/
├── tests/
│   └── testthat/
│       └── test-data_loader.R
├── data/                      # Place WBES .dta files here
├── DESCRIPTION
├── rhino.yml
├── dependencies.R
└── README.md
```

---

## 📊 Data Setup

### Option 1: Sample Data (Demo Mode)

The dashboard includes realistic sample data for demonstration. No setup required.

### Option 2: Real WBES Data

1. Download data from [Enterprise Surveys Portal](https://www.enterprisesurveys.org/en/survey-datasets)
2. Place `.dta` files in the `data/` directory
3. Restart the application

Supported file formats:
- Stata `.dta` files (preferred)
- CSV files (alternative)

---

## 🎨 Theme Customization

The dashboard uses the **Kwiz Research** theme with:

| Element | Color |
|---------|-------|
| Primary (Teal) | `#1B6B5F` |
| Secondary (Coral) | `#F49B7A` |
| Success | `#2E7D32` |
| Background | `#FFFFFF` |

Customize in `app/styles/main.scss`:

```scss
$primary-teal: #1B6B5F;
$secondary-coral: #F49B7A;
```

---

## 🔬 Testing

```r
# Run all tests
testthat::test_dir("tests/testthat")

# Run specific test file
testthat::test_file("tests/testthat/test-data_loader.R")

# With Rhino
rhino::test_r()

# Check code style
rhino::lint_r()
```

---

## 📦 Deployment

### shinyapps.io

```r
# Install rsconnect if needed
install.packages("rsconnect")

# Configure your account
rsconnect::setAccountInfo(name="YOUR_ACCOUNT",
                          token="YOUR_TOKEN",
                          secret="YOUR_SECRET")

# Deploy
rsconnect::deployApp()
```

### Posit Connect

```r
rsconnect::deployApp(
  appDir = ".",
  appName = "wbes-dashboard",
  server = "your-connect-server.com"
)
```

### Docker

```dockerfile
FROM rocker/shiny-verse:4.3.0

RUN R -e "install.packages(c('rhino', 'bslib', 'plotly', 'leaflet', 'DT', \
          'haven', 'waiter', 'logger'))"

COPY . /srv/shiny-server/wbes-dashboard

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
```

---

## 📖 Data Quality Philosophy

This dashboard follows the data quality principles outlined in the [Kwiz Research Blog](https://kwizresearch.com/blog):

1. **Transparency** - All data issues are documented
2. **Reproducibility** - R code provided for all filters
3. **Traceability** - Issues linked to specific indicators
4. **Severity Classification** - Clear risk indicators

See the **Data Quality** tab in the dashboard for complete documentation.

---

## 📄 License & Citation

### Data Citation

```
World Bank Group. Enterprise Surveys.
Available at: https://www.enterprisesurveys.org
```

### Dashboard Citation

```
Kwiz Computing Technologies (2025). 
Business Environment Benchmarking Dashboard.
https://kwizresearch.com
```

---

## 🤝 Contact

**Kwiz Computing Technologies**

- 🌐 Website: [kwizresearch.com](https://kwizresearch.com)
- 📧 Email: info@kwizresearch.com
- 💼 LinkedIn: [Jean Victor Kwizera](https://linkedin.com/in/jean-victor-kwizera)

For custom development, consultancy, or enterprise licensing inquiries, please contact us.

---

## 🙏 Acknowledgments

- [World Bank Enterprise Surveys](https://www.enterprisesurveys.org) for making data publicly available
- [Appsilon](https://appsilon.com) for the Rhino framework
- [Posit](https://posit.co) for the R ecosystem

---

*Built with ❤️ in Nairobi, Kenya*
# WBES-dashboard
