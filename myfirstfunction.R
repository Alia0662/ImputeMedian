
data1 <- data.frame(
  Name = c("Alia", "Bob", "Natasya", "Anisa", "Aiman"),
  Age = c(25, NA, 30, 35, NA),
  Salary = c(50000, 60000, NA, 80000, 90000),
  Department = c("HR", NA, "IT", "Finance", "HR")
)

# Display the dataset
print("Original Dataset:")
print(data1)

#' Replace Missing Values with Column Medians
#'
#' This function replaces missing values (NA) in numeric columns of a data frame
#' with the median of the respective column.
#'
#' @param data A data frame. Numeric columns are analyzed for missing values.
#' @return A data frame where NA values in numeric columns are replaced with the column medians.
#' @examples
#' data1 <- data.frame(
#Name = c("Alia", "Bob", "Natasya", "Anisa", "Aiman"),
#Age = c(25, NA, 30, 35, NA),
#Salary = c(50000, 60000, NA, 80000, 90000),
#Department = c("HR", NA, "IT", "Finance", "HR"))
#' print(data1)
#' @export
handle_missing_values <- function(data, method = "median") {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Process numeric columns for median imputation
  if (method == "median") {
    data_clean <- data
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        median_value <- median(data[[col]], na.rm = TRUE)  # Calculate median
        data_clean[[col]][is.na(data[[col]])] <- median_value  # Replace NAs with median
      }
    }
    return(data_clean)
  } else {
    stop("Only 'median' method is supported in this example.")
  }
}

# Apply the function to the dataset
cleaned_data <- handle_missing_values(data1, method = "median")

# Display the cleaned dataset
print("Dataset After Median Imputation:")
print(cleaned_data)
?handle_missing_values
??handle_missing_values
