# tests/testthat/test-data_loader.R
# Unit tests for data loading and processing module

box::use(
  testthat[describe, it, expect_true, expect_false, expect_equal, 
           expect_type, expect_named, expect_s3_class, expect_gte, expect_lte],
  app/logic/data_loader[load_sample_data, calculate_completeness, generate_quality_metadata]
)

describe("load_sample_data()", {
  
  sample_data <- load_sample_data()
  
  it("returns a list with expected components", {
    expect_type(sample_data, "list")
    expect_named(sample_data, 
                 c("country_panel", "latest", "regional", "countries", 
                   "regions", "years", "metadata", "quality"),
                 ignore.order = TRUE)
  })
  
  it("contains country panel data frame with required columns", {
    panel <- sample_data$country_panel
    expect_s3_class(panel, "data.frame")
    
    required_cols <- c("country", "region", "income_group", "year",
                       "power_outages_per_month", "firms_with_credit_line_pct",
                       "bribery_incidence_pct", "sample_size")
    
    for (col in required_cols) {
      expect_true(col %in% names(panel), 
                  info = paste("Missing column:", col))
    }
  })
  
  it("has valid country coverage", {
    expect_gte(length(sample_data$countries), 20)
    expect_true("Kenya" %in% sample_data$countries)
    expect_true("Nigeria" %in% sample_data$countries)
  })
  
  it("has valid year range", {
    years <- sample_data$years
    expect_true(all(years >= 2019 & years <= 2025))
    expect_gte(length(years), 3)
  })
  
  it("has valid indicator ranges", {
    panel <- sample_data$country_panel
    
    # Power outages should be reasonable
    expect_true(all(panel$power_outages_per_month >= 0, na.rm = TRUE))
    expect_true(all(panel$power_outages_per_month <= 50, na.rm = TRUE))
    
    # Percentages should be 0-100
    pct_cols <- c("firms_with_credit_line_pct", "bribery_incidence_pct",
                  "capacity_utilization_pct", "female_ownership_pct")
    
    for (col in pct_cols) {
      expect_true(all(panel[[col]] >= 0 & panel[[col]] <= 100, na.rm = TRUE),
                  info = paste("Invalid range for", col))
    }
  })
  
  it("includes metadata with source information", {
    meta <- sample_data$metadata
    expect_type(meta, "list")
    expect_true("source" %in% names(meta))
    expect_true("url" %in% names(meta))
    expect_true(grepl("World Bank", meta$source))
  })
  
})

describe("generate_quality_metadata()", {
  
  quality <- generate_quality_metadata()
  
  it("returns a list with issues and filters", {
    expect_type(quality, "list")
    expect_true("issues" %in% names(quality))
    expect_true("filters" %in% names(quality))
    expect_true("methodology_notes" %in% names(quality))
  })
  
  it("documents at least 5 data quality issues", {
    expect_gte(length(quality$issues), 5)
  })
  
  it("each issue has required fields", {
    required_fields <- c("id", "category", "severity", "indicator",
                         "description", "filter_applied", "r_code")
    
    for (issue in quality$issues) {
      for (field in required_fields) {
        expect_true(field %in% names(issue),
                    info = paste("Missing field", field, "in issue", issue$id))
      }
    }
  })
  
  it("severity levels are valid", {
    valid_severities <- c("Low", "Medium", "High")
    
    for (issue in quality$issues) {
      expect_true(issue$severity %in% valid_severities,
                  info = paste("Invalid severity in", issue$id))
    }
  })
  
  it("each filter has R code snippet", {
    for (filter in quality$filters) {
      expect_true("r_code" %in% names(filter))
      expect_true(nchar(filter$r_code) > 50,
                  info = paste("R code too short for", filter$name))
    }
  })
  
})

describe("calculate_completeness()", {
  
  it("returns NULL for NULL input", {
    result <- calculate_completeness(NULL)
    expect_true(is.null(result))
  })
  
  it("calculates completeness metrics correctly", {
    # Create test data with known missing values
    test_data <- data.frame(
      a = c(1, 2, NA, 4, 5),
      b = c(1, NA, NA, 4, 5),
      c = c(1, 2, 3, 4, 5)
    )
    
    result <- calculate_completeness(test_data)
    
    expect_equal(result$total_observations, 5)
    expect_equal(result$total_variables, 3)
    expect_equal(result$missing_cells, 3)  # 1 in 'a', 2 in 'b'
    expect_equal(result$total_cells, 15)
    expect_equal(result$completeness_pct, 80)
    expect_equal(result$complete_cases, 3)  # rows 1, 4, 5
  })
  
})

describe("Data quality documentation completeness", {
  
  quality <- generate_quality_metadata()
  
  it("documents missing data handling", {
    categories <- sapply(quality$issues, function(x) x$category)
    expect_true("Missing Data" %in% categories)
  })
  
  it("documents outlier handling", {
    categories <- sapply(quality$issues, function(x) x$category)
    expect_true("Outliers" %in% categories)
  })
  
  it("documents response bias considerations", {
    categories <- sapply(quality$issues, function(x) x$category)
    expect_true("Response Bias" %in% categories)
  })
  
  it("provides infrastructure analysis filters", {
    filter_names <- sapply(quality$filters, function(x) x$name)
    expect_true("Infrastructure Analysis" %in% filter_names)
  })
  
  it("provides finance analysis filters", {
    filter_names <- sapply(quality$filters, function(x) x$name)
    expect_true("Access to Finance Analysis" %in% filter_names)
  })
  
  it("provides corruption analysis filters", {
    filter_names <- sapply(quality$filters, function(x) x$name)
    expect_true("Corruption Analysis" %in% filter_names)
  })
  
})
