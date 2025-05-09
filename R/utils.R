#' Load custom data from inst/extdata or a user-specified path
#' 
#' This function loads custom data from the inst/extdata directory of the package or
#' a designated path provided by the user.
#'
#' @source Example dataset compiled from the following public sources:
#' - Crude oil supply/demand metrics (FieldProduction, RefinerNetInput, OperableCapacity, Imports, StocksExcludingSPR): https://www.eia.gov
#' - API gravity and Rig Count: Extracted from industry reports
#' - S&P 500 Index (SPX): https://fred.stlouisfed.org/series/SP500
#' - CFTC positioning data (NonCommercialLong, CommercialShort, OpenInterest, etc.): https://www.cftc.gov (NYMEX short format)
#' - TotalLong and TotalShort = NonCommercial (L/S) + Spread (L/S) + Commercial (L/S)
#'
#' @format A data frame with 55 weekly observations and 19 columns, including headers. The first column represents the date in the format MM/DD/YYYY, while all other columns display weekly percentage changes with five-decimal precision.

#' @param data The name of the data file to load (default: "crudeoildata.csv").
#' @param custom_path An optional custom file path. If provided, it overrides the default file path.
#' @param exclude_first_column Logical value indicating whether to exclude the first column from the loaded data (default: FALSE).
#'
#' @return A data frame containing the loaded data.
#'
#' @examples
#' \dontrun{
#' # Load custom data with default options
#' df <- load_custom_data()
#'
#' # Load data from a custom file path and exclude the first column (e.g., a Date column)
#' df <- load_custom_data(
#'   data = "crudeoildata.csv",
#'   custom_path = "path/to/custom/crudeoildata.csv",
#'   exclude_first_column = TRUE
#' )
#'
#' # Load default custom data and exclude the first column
#' df <- load_custom_data(exclude_first_column = TRUE)
#' }
#' @importFrom readxl read_excel
#' @importFrom utils read.csv read.delim read.table
#'
#' @export
load_custom_data <- function(data = "crudeoildata.csv", custom_path = NULL, exclude_first_column = FALSE) {
  # Determine file path
  if (is.null(custom_path)) {
    file_path <- system.file("extdata", data, package = "olr")
  } else {
    file_path <- custom_path
  }
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Load data based on file extension
  extension <- tools::file_ext(file_path)
  data <- switch(extension,
                 "csv" = read.csv(file_path),
                 "xls" = readxl::read_excel(file_path),
                 "xlsx" = readxl::read_excel(file_path),
                 "R" = {
                   e <- new.env()
                   load(file_path, envir = e)
                   get(ls(e)[1], envir = e)
                 },
                 stop(paste("Unsupported file format. Please ensure your data table is stored in one of the following formats: .csv, .xls, .xlsx, or .R:", extension))
  )
  
  # Ensure data was successfully loaded
  if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    stop("The data file was loaded but contains no usable data (zero rows or columns).")
  }
  
  # Exclude first column if requested and safe to do so
  if (exclude_first_column) {
    if (ncol(data) > 1) {
      data <- data[, -1]
    } else {
      warning("exclude_first_column is TRUE but the dataset has only one column. No columns were removed.")
    }
  }
  
  
  return(data)
}
#
# ========================================================================
# 🔍 Model Comparison: Full Model (R-squared) vs Parsimonious Model (Adj R-squared)
# ========================================================================
# | Metric                    | adjr2 = FALSE (All 12 Predictors) | adjr2 = TRUE (Best Subset of 7 Predictors)  |
# |--------------------------|-----------------------------------|---------------------------------------------|
# | **Adjusted R-squared**    | 0.6145                            | **0.6531** ✅ (higher is better)             |
# | **Multiple R-squared**    | 0.7018                            | 0.699                                        |
# | **Residual Std. Error**   | 0.02388                           | **0.02265** ✅ (lower is better)             |
# | **F-statistic (p-value)** | 8.042 (1.88e-07)                  | **15.26 (3.99e-10)** ✅ (stronger model)     |
# | **Model Complexity**      | 12 predictors                     | **7 predictors** ✅ (simpler, more robust)   |
# | **Significant Coeffs**    | 4                                 | **6** ✅ (more signal, less noise)           |
# | **R² Difference**         | —                                 | ~0.003 ❗ (negligible)                       |
#
# ✅ Summary: Even though the full model has a slightly higher raw R², the adjusted R² model:
#   - Achieves a better generalized fit (higher adjusted R²)
#   - Reduces model complexity with fewer predictors
#   - Delivers stronger overall statistical significance
#   - Provides clearer insights with more significant coefficients
#
# 🧠 Interpretation Tip:
#   - **R²** measures total variance explained but always increases with more predictors—even irrelevant ones.
#   - **Adjusted R²** corrects for this by penalizing unnecessary variables, making it better for generalization.
#   - A small decrease in R² (< 0.01) is acceptable if it results in a more stable and interpretable model.
#
# 🛠 Best Practice:
#   - The `olr()` function already **automates the trial-and-error process** by evaluating all possible predictor combinations.
#   - Use `adjr2 = TRUE` to prioritize models that **balance predictive power and simplicity**, avoiding overfitting.
#   - This ensures the best-fitting model is selected without needing to guess which predictors to include.
