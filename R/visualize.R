#' Visualize Geotagged Photos
#'
#' Creates an interactive Leaflet map showing where photos were located.
#' Distinguishes between "Raw GPS" (Plan A) and "Semantic Place" (Plan B).
#'
#' @param tagged_df The dataframe returned by geotag_photos().
#' @return A Leaflet map object.
#' @export
visualize_geotags <- function(tagged_df) {

  # Check if leaflet is installed (it's a suggestion, not a hard requirement)
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The 'leaflet' package is required for this function. Please install it.")
  }

  library(leaflet)

  # Filter only successfully tagged images
  plot_data <- tagged_df |>
    filter(!is.na(final_lat)) |>
    mutate(
      color = ifelse(method == "Raw GPS", "blue", "orange"),
      label = paste0(
        "<b>File:</b> ", basename(SourceFile), "<br>",
        "<b>Time:</b> ", local_time, "<br>",
        "<b>Method:</b> ", method
      )
    )

  message("Plotting ", nrow(plot_data), " tagged images...")

  leaflet(plot_data) |>
    addProviderTiles("CartoDB.Positron") |>
    addCircleMarkers(
      ~final_lon, ~final_lat,
      radius = 5,
      color = ~color,
      stroke = FALSE,
      fillOpacity = 0.8,
      popup = ~label
    ) |>
    addLegend(
      position = "bottomright",
      colors = c("blue", "orange"),
      labels = c("Plan A: Raw GPS", "Plan B: Semantic Place"),
      title = "Geotag Method"
    )
}
