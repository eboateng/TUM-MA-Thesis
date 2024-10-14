library(reticulate)

# Initialize Python environment in R
# For Windows
use_python("C:/Users/ge78zoz/pyenv/Scripts/python.exe", required = TRUE)

# Check if the correct Python environment is being used
py_config()

pickle <- import("pickle")
py_builtins <- import_builtins()

# Function to read pickle file in Python from R
read_pickle_file <- function(filepath) {
  # Use Python's `open()` to open the file in binary read mode
  file <- py_builtins$open(filepath, "rb")
  
  # Load the pickle data using Python's pickle module
  data <- pickle$load(file)
  
  # Close the file after loading
  file$close()
  
  return(data)
}

# Example usage
pickle_file <- "\\\\nas.ads.mwn.de/ge78zoz/Documents/GitHub/TUM MA Thesis/Data/10-X/2002/10X_2002.pkl"  # Adjust the path accordingly
#pickle_file <- "\\\\nas.ads.mwn.de/ge78zoz/Documents/GitHub/TUM MA Thesis/Data/10-X/1993/10X_1993.pkl"  # Adjust the path accordingly
pickle_data <- read_pickle_file(pickle_file)

# Print the data to verify
#print(pickle_data)
View(pickle_data)
