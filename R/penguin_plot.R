#' Penguin Plot Histogram
#' A function to create a histogram for each species based on specified numeric variable
#' @param data A data frame containing information about different species, with at least one numeric variable and a variable named species to group by
#' @param variable_name A numeric variable to plot, the variable name has to be a string
#' @return A histogram of the containing the summary variables (count, arithmetic mean, standard deviation, minimum and maximum value) for the chosen variable
#' @examples
#' Example usage:
#' Plot the distribution of 'body_mass_g' for each penguin species
#  plot_species_distribution(palmerpenguins::penguins, "body_mass_g")
#' @export
plot_species_distribution <- function(data, variable_name) {

  require(ggplot2)

  ggplot(data, aes_string(x = variable_name, fill = "species")) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~species, scales = "free_y") +
    labs(title = paste("Distribution of", variable_name, "by Species"),
         x = variable_name,
         y = "Frequency") +
    theme_minimal()
}

# Example usage:
# Plot the distribution of 'body_mass_g' for each penguin species
# plot_species_distribution(palmerpenguins::penguins, "body_mass_g")
