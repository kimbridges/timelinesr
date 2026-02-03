#' Visualize Geotagged Photos (Interactive)
#'
#' Creates an interactive map of photo locations using Leaflet.
#'
#' @param tagged_df The dataframe returned by geotag_photos().
#' @return A leaflet map widget.
#' @export
visualize_geotags <- function(tagged_df) {

  # 1. Validation & "Bulletproof" Filtering
  # Use Base R subsetting to guarantee it finds the columns
  # We also force it to be a plain data frame to avoid Tibble quirks in some versions
  plot_data <- as.data.frame(tagged_df)
  plot_data <- plot_data[ !is.na(plot_data$lat) & !is.na(plot_data$lon), ]

  if (nrow(plot_data) == 0) {
    stop("No geotagged photos to plot.")
  }

  message("Mapping ", nrow(plot_data), " photos...")

  # 2. Create Interactive Map
  # The tilde (~) tells leaflet to look INSIDE the dataframe for the column
  leaflet::leaflet(plot_data) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(
      lng = ~lon,  # Ensure this has the tilde (~)!
      lat = ~lat,  # Ensure this has the tilde (~)!
      radius = 5,
      color = "red",
      fillColor = "red",
      fillOpacity = 0.7,
      stroke = FALSE,
      popup = ~paste0(
        "<b>File:</b> ", basename(SourceFile), "<br>",
        "<b>Time:</b> ", timestamp
      )
    )
}
