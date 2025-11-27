# WBES Data Directory

This directory is for World Bank Enterprise Surveys data files.

## Data Sources

Download WBES data from: https://www.enterprisesurveys.org

## Supported Formats

1. **Stata files (.dta)** - Preferred format
2. **CSV files (.csv)** - Alternative format

## Usage

1. Download WBES data files from the Enterprise Surveys website
2. Place .dta or .csv files in this directory
3. The dashboard will automatically load all files in this directory
4. If no files are found, sample data will be used for demonstration

## File Organization

You can organize files by:
- Country: `Kenya_2023.dta`
- Region: `SubSaharanAfrica_2023.dta`
- Or use the standard WBES file naming convention

## Notes

- Files are loaded automatically when the dashboard starts
- Larger files may take longer to load
- The app will show a loading indicator during data initialization
