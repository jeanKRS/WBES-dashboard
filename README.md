# World Bank Enterprise Surveys Dashboard

## Business Environment Benchmarking

<<<<<<< HEAD
![R](https://img.shields.io/badge/R-≥4.1.0-blue)
![Rhino](https://img.shields.io/badge/Rhino-1.9.0-green)
![License](https://img.shields.io/badge/License-Custom-orange)

Enterprise-grade Shiny dashboard for analyzing business environments across 168 economies using World Bank Enterprise Survey data.
=======
![R Version](https://img.shields.io/badge/R-≥4.1.0-blue)
![Rhino Framework](https://img.shields.io/badge/Rhino-1.9.0-green)
![License](https://img.shields.io/badge/License-Custom-orange)

Enterprise-grade Shiny dashboard for analyzing and benchmarking business environments across 168 economies using World Bank Enterprise Survey data.
>>>>>>> origin/main

**Developed by [Kwiz Computing Technologies](https://kwizresearch.com)** | Nairobi, Kenya

---

<<<<<<< HEAD
## Features
=======
## 🎯 Features

### Analytics Modules
>>>>>>> origin/main

| Module | Description |
|--------|-------------|
| **Overview** | Global KPIs, interactive map, regional comparisons |
<<<<<<< HEAD
| **Country Profile** | Deep-dive with radar charts and time series |
| **Benchmark** | Compare up to 10 countries across indicators |
| **Infrastructure** | Power outages, electricity access, productivity impact |
| **Finance** | Credit gaps, collateral, gender disparities |
| **Data Quality** | Full transparency on data issues and filter logic |
=======
| **Country Profile** | Deep-dive analysis with radar charts and time series |
| **Cross-Country Benchmark** | Compare up to 10 countries across indicators |
| **Infrastructure** | Power outages, generator usage, productivity impact |
| **Access to Finance** | Credit gaps, collateral analysis, gender disparities |
| **Data Quality** | Complete transparency on data issues and filter logic |
>>>>>>> origin/main

### Technical Highlights

- ✅ **Rhino Framework** - Enterprise-grade modular architecture
<<<<<<< HEAD
- ✅ **World Bank API** - Real-time data from Enterprise Surveys API
- ✅ **Comprehensive Testing** - testthat + Cypress e2e tests
- ✅ **Data Quality Documentation** - Full transparency on filtering logic
- ✅ **Responsive Design** - Desktop, tablet, and mobile support

---

## Project Structure

```
.
├── app
│   ├── js
│   │   └── index.js
│   ├── logic
│   │   ├── __init__.R
│   │   └── wbes_data.R        # World Bank API integration
│   ├── static
│   │   └── favicon.ico
│   ├── styles
│   │   └── main.scss          # Kwiz Research theme
│   ├── view
│   │   ├── __init__.R
│   │   ├── overview.R
│   │   ├── country_profile.R
│   │   ├── benchmark.R
│   │   ├── infrastructure.R
│   │   ├── finance_access.R
│   │   ├── data_quality.R
│   │   └── about.R
│   └── main.R
├── tests
│   ├── cypress
│   │   └── e2e
│   │       └── app.cy.js
│   ├── testthat
│   │   └── test-main.R
│   └── cypress.json
├── data                        # Place microdata here (optional)
├── app.R
├── WBESDashboard.Rproj
├── dependencies.R
├── renv.lock
└── rhino.yml
```

---

## Quick Start
=======
- ✅ **Comprehensive Testing** - testthat unit tests with >80% coverage target
- ✅ **Data Quality Documentation** - Full transparency on filtering logic
- ✅ **Responsive Design** - Works on desktop, tablet, and mobile
- ✅ **Reproducible Analysis** - R code provided for all transformations
- ✅ **Publication Ready** - Deploy to shinyapps.io, Posit Connect, or self-host

---

## 🚀 Quick Start
>>>>>>> origin/main

### Prerequisites

```r
<<<<<<< HEAD
install.packages(c("rhino", "shiny", "bslib", "plotly", "leaflet", "DT",
                   "dplyr", "tidyr", "httr", "jsonlite", "haven", 
                   "waiter", "logger", "box"))
=======
# Install required packages
install.packages(c("rhino", "shiny", "bslib", "plotly", "leaflet", "DT",
                   "dplyr", "tidyr", "haven", "waiter", "logger"))
>>>>>>> origin/main
```

### Run Locally

```r
<<<<<<< HEAD
# Option 1: Using Rhino
rhino::app()

# Option 2: Direct Shiny
=======
# Clone or download the project
# Navigate to project directory

# Option 1: Using Rhino
rhino::app()

# Option 2: Using shiny directly
>>>>>>> origin/main
shiny::runApp()
```

### With renv (Recommended)

```r
<<<<<<< HEAD
renv::restore()
=======
# Restore exact package versions
renv::restore()

# Run app
>>>>>>> origin/main
rhino::app()
```

---

<<<<<<< HEAD
## Data Sources

### 1. World Bank API (Default)

The dashboard fetches aggregate indicators from the World Bank API (Source ID 13: Enterprise Surveys). No registration required.

```r
# Fetched indicators include:
# IC.FRM.OUTG.ZS - Power outages obstacle
# IC.FRM.FINA.ZS - Access to finance obstacle
# IC.FRM.CORR.ZS - Corruption obstacle
# ... and 12+ more
```

### 2. Microdata (Optional)

For firm-level analysis, download microdata from [enterprisesurveys.org](https://www.enterprisesurveys.org/en/survey-datasets):

1. Register (free) at the Enterprise Surveys portal
2. Download `.dta` files for your countries of interest
3. Place files in the `data/` directory
4. Restart the application

---

## Testing

```r
# Unit tests
testthat::test_dir("tests/testthat")

# With Rhino
rhino::test_r()

# E2E tests (requires Cypress)
rhino::test_e2e()

# Lint code
=======
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
>>>>>>> origin/main
rhino::lint_r()
```

---

<<<<<<< HEAD
## Deployment
=======
## 📦 Deployment
>>>>>>> origin/main

### shinyapps.io

```r
<<<<<<< HEAD
rsconnect::deployApp(
  appFiles = c("app.R", "app/", "dependencies.R", "rhino.yml", "renv.lock"),
  appName = "wbes-dashboard"
=======
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
>>>>>>> origin/main
)
```

### Docker

```dockerfile
FROM rocker/shiny-verse:4.3.0

<<<<<<< HEAD
RUN R -e "install.packages(c('rhino', 'bslib', 'plotly', 'leaflet', 'DT', 'httr', 'jsonlite', 'haven', 'waiter', 'logger', 'box'))"
=======
RUN R -e "install.packages(c('rhino', 'bslib', 'plotly', 'leaflet', 'DT', \
          'haven', 'waiter', 'logger'))"
>>>>>>> origin/main

COPY . /srv/shiny-server/wbes-dashboard

EXPOSE 3838
<<<<<<< HEAD
=======

>>>>>>> origin/main
CMD ["/usr/bin/shiny-server"]
```

---

<<<<<<< HEAD
## Theme Customization

Edit `app/styles/main.scss`:

```scss
$primary-teal: #1B6B5F;      // Main brand color
$secondary-coral: #F49B7A;    // Accent color
```

---

## License & Citation

### Data

```
World Bank Group. Enterprise Surveys.
https://www.enterprisesurveys.org
```

### Dashboard
=======
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
>>>>>>> origin/main

```
Kwiz Computing Technologies (2025). 
Business Environment Benchmarking Dashboard.
https://kwizresearch.com
```

---

<<<<<<< HEAD
## Contact

**Kwiz Computing Technologies**

- 🌐 [kwizresearch.com](https://kwizresearch.com)
- 📧 info@kwizresearch.com
- 💼 [LinkedIn](https://linkedin.com/in/jean-victor-kwizera)
=======
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
>>>>>>> origin/main

---

*Built with ❤️ in Nairobi, Kenya*
<<<<<<< HEAD
=======
# WBES-dashboard
>>>>>>> origin/main
