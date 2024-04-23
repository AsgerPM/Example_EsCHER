#' Summarisation of species
#'
#' Summarise the count, arithmetic mean, standard deviation, and minimum and maximum values for a specified numeric variable in a dataframe dataset of species
#' @param data A data frame containing information about different species, with at least one numeric variable and a variable named species to group by
#' @param variable A numeric variable to summarise
#' @return A histogram of the containing the summary variables (count, arithmetic mean, standard deviation, minimum and maximum value) for the chosen variable
#' @examples
#' Example usage:
#' summarize_species(data = palmerpenguins::penguins,
#'                variable = flipper_length_mm)
#' @export

# Function to calculate summary statistics for a specified numeric variable by species
summarize_species <- function(data, variable) {

  require(dplyr)

  data %>%
    group_by(species) %>%
    summarise(
      Count = n(),
      Mean = mean({{variable}}, na.rm = TRUE),
      SD = sd({{variable}}, na.rm = TRUE),
      Min = min({{variable}}, na.rm = TRUE),
      Max = max({{variable}}, na.rm = TRUE)
    ) %>%
    ungroup()
}

# Example usage:
summarize_species(data = palmerpenguins::penguins,
                  variable = flipper_length_mm)
