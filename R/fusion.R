#' Fuse Semantic and Raw Layers (v2)
#'
#' Merges "Place Visits" with "Raw GPS Points".
#' capturing movement before the first visit, between visits, and after the last visit.
#'
#' @param timeline_data The list returned by read_timeline().
#' @return A single dataframe with a 'type' column (visit vs path).
#' @export
fuse_layers <- function(timeline_data) {

  visits <- timeline_data$visits
  raw    <- timeline_data$raw_locations

  if(nrow(visits) == 0) return(NULL)

  message("Fusing layers... (This might take a moment)")

  # Ensure sorted
  visits <- visits |> arrange(start_utc)
  if(!is.null(raw)) raw <- raw |> arrange(timestamp)

  combined_track <- list()

  # --- 1. THE HEAD (Before the first visit) ---
  if (!is.null(raw) && nrow(raw) > 0) {
    first_visit_start <- visits$start_utc[1]
    head_points <- raw |> filter(timestamp < first_visit_start)

    if (nrow(head_points) > 0) {
      combined_track[[length(combined_track) + 1]] <- head_points |>
        transmute(timestamp, lat, lon, type = "path", name = NA, accuracy = accuracy_m)
    }
  }

  # --- 2. THE BODY (Visits + Gaps) ---
  for (i in 1:nrow(visits)) {

    # A. Add the Visit
    current_visit <- visits[i, ]
    combined_track[[length(combined_track) + 1]] <- tibble(
      timestamp = c(current_visit$start_utc, current_visit$end_utc),
      lat       = current_visit$lat,
      lon       = current_visit$lon,
      type      = "visit",
      name      = current_visit$place_name,
      accuracy  = NA
    )

    # B. Add the Gap (if there is a next visit)
    if (i < nrow(visits)) {
      gap_start <- current_visit$end_utc
      gap_end   <- visits$start_utc[i+1]

      if (!is.null(raw) && gap_end > gap_start) {
        gap_points <- raw |> filter(timestamp > gap_start & timestamp < gap_end)
        if (nrow(gap_points) > 0) {
          combined_track[[length(combined_track) + 1]] <- gap_points |>
            transmute(timestamp, lat, lon, type = "path", name = NA, accuracy = accuracy_m)
        }
      }
    }
  }

  # --- 3. THE TAIL (After the last visit) ---
  if (!is.null(raw) && nrow(raw) > 0) {
    last_visit_end <- visits$end_utc[nrow(visits)]
    tail_points <- raw |> filter(timestamp > last_visit_end)

    if (nrow(tail_points) > 0) {
      combined_track[[length(combined_track) + 1]] <- tail_points |>
        transmute(timestamp, lat, lon, type = "path", name = NA, accuracy = accuracy_m)
    }
  }

  # --- Bind and Dedup ---
  final_df <- bind_rows(combined_track) |>
    arrange(timestamp) |>
    distinct(timestamp, .keep_all = TRUE)

  return(final_df)
}
