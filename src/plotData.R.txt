library(tidyr)
library(ggplot2)
library(RColorBrewer)

# The number of trait categories (up to 11)
TRAIT_BINS <- 5
COLOR_PAL <- "Spectral"

#'
#' Plot Data
#'  Read data from a csv file and plot the trait values in a boxplot
#'
#' @param input File path to the csv file (default: ./data/traits.csv)
#' @param output File path to the plot image file (default: ./plots/{input}.png)
#' 
#' @import tidyr ggplot2 RColorBrewer
#'
plotData = function(input = "./data/traits.csv", output = NULL) {
  print(sprintf("Plotting data from %s...", input))

  # Read generated data from the CSV file
  data_wide <- read.csv(input)
  
  # convert data from wide format (separate column for each trait) 
  # to long format (one column for trait names and one column for trait values)
  data_long <- tidyr::gather(data_wide, trait, value, names(data_wide)[-1])
  
  # Generate the colors to use for the trait categories
  colors <- RColorBrewer::brewer.pal(TRAIT_BINS, COLOR_PAL)
  trait_colors = colors[(as.numeric(gsub(".*([0-9]+)$", "\\1", data_long$trait)) %% TRAIT_BINS) + 1]
  
  # Build the boxplot using the long-formatted data
  #   dependent variable = trait name
  #   independent variable = trait value
  #   sort boxplots by trait value
  #   flip coordinates so the boxplots are vertically arranged
  #   color is determined by the modulo of the trait index in its name
  plot <- ggplot2::ggplot(
      data = data_long, 
      mapping = aes(
        x = reorder(trait, value), 
        y = value,
        fill = trait_colors
      )
    ) + 
    geom_boxplot() + 
    labs(x=NULL, y="Trait Values") + 
    coord_flip() + 
    theme_bw() + 
    theme(legend.position='none')
  
  # Display the plot
  print(plot)
  
  # Save the plot
  if ( is.null(output) ) {
    f <- paste(sub('\\.csv$', '', basename(input)), "png", sep=".") 
    output <- paste(".", "plots", f, sep="/")
  }
  print(sprintf("Saving plot to %s...", output))
  ggplot2::ggsave(output, plot)
}