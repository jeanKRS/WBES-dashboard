# tests/testthat.R
# testthat configuration for wbes.dashboard

library(testthat)
library(box)

# Set box module path
options(box.path = file.path(getwd()))

test_check("wbes.dashboard")
