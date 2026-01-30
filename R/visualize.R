#' Visualize Geotagged Photos
#'
#' Creates a simple map of where photos were taken.
#'
#' @param tagged_df The dataframe returned by geotag_photos().
#' @export
visualize_geotags <- function(tagged_df) {

  library(ggplot2)
  library(sf) # If used for background map

  # 1. Filter for valid data
  plot_data <- tagged_df |>
    filter(!is.na(lat), !is.na(lon))

  if (nrow(plot_data) == 0) {
    stop("No geotagged photos to plot.")
  }

  # 2. Plot
  ggplot(plot_data, aes(x = lon, y = lat)) +
    geom_point(color = "red", alpha = 0.7, size = 3) +
    coord_quickmap() +
    theme_minimal() +
    labs(
      title = "Photo Locations",
      subtitle = paste(nrow(plot_data), "photos mapped"),
      x = "Longitude", y = "Latitude"
    )
}
