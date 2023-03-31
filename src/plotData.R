MEAN <- 75
STD_DEV <- 5
SAMPLE_SIZE <- 10000
TRAIT_COUNT <- 10

# Small Data:
# SAMPLE_SIZE = 10000
# TRAIT_COUNT = 10

# Large Data:
# SAMPLE_SIZE = 150000
# TRAIT_COUNT = 50

#'
#' Generate Data
#'  Generate random trait values for SAMPLE_SIZE number of samples
#'  and TRAIT_COUNT number of traits.  Write the data to a csv file.
#'
#' @param output File path to the csv file (default: ./data/traits.csv)
#'
generateData <- function(output = "./data/traits.csv") {
  print(sprintf("Generating data for %i samples by %i traits...", SAMPLE_SIZE, TRAIT_COUNT))
  
  # Generate the names of the individual samples as 'SAMPLE_{n}'
  sample_names <- sapply(
    seq(1:SAMPLE_SIZE), 
    function(x) { paste("SAMPLE", x, sep="_") }
  )
  
  # Generate the names of the individual traits as 'TRAIT_{n}'
  trait_names <- sapply(
    seq(1:TRAIT_COUNT),
    function(x) { paste("TRAIT", x, sep="_") }
  )
  
  # Generate the data
  data <- as.data.frame(
    matrix(
      rnorm(SAMPLE_SIZE*TRAIT_COUNT, mean = MEAN, sd = STD_DEV),
      nrow = SAMPLE_SIZE
    )
  )
  
  # Add a random constant to each trait
  rand <- runif(TRAIT_COUNT, min=0.5, max=2)
  data <- as.data.frame(mapply(`*`, data, rand))
  
  # Set the column names
  names(data) <- trait_names
  
  # Add sample names
  data <- cbind(SAMPLE = sample_names, data)
  
  # Write the data to a .csv file
  print(sprintf("Writing data to %s...", output))
  write.csv(data, output, row.names=F)
}
