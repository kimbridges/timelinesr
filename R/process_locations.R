#' Interpolate GPS (Plan A)
#'
#' Performs linear interpolation of GPS coordinates based on timestamps.
#'
#' @param target_times A vector of POSIXct timestamps (from the photos).
#' @param reference_df A dataframe of raw GPS signals (from clean_timeline_signals).
#' @return A tibble with lat, lon, and uncertainty (time difference in seconds).
#' @export
interpolate_gps <- function(target_times, reference_df) {
  if (is.null(reference_df) || nrow(reference_df) == 0) return(NULL)

  ref <- reference_df |> arrange(timestamp_utc)

  lat_est <- approx(ref$timestamp_utc, ref$lat, target_times, rule = 1)$y
  lon_est <- approx(ref$timestamp_utc, ref$lon, target_times, rule = 1)$y

  # Quality Control (Gap Check)
  nearest_idx <- findInterval(target_times, ref$timestamp_utc)
  nearest_idx[nearest_idx == 0] <- 1
  nearest_idx[nearest_idx > nrow(ref)] <- nrow(ref)
  time_diff <- abs(difftime(target_times, ref$timestamp_utc[nearest_idx], units = "secs"))

  tibble(lat = lat_est, lon = lon_est, uncertainty = as.numeric(time_diff))
}

#' Match Semantic Places (Plan B)
#'
#' Matches photos to a "Place Visit" based on time overlap.
#'
#' @param photo_df Dataframe of photos with timestamp_utc.
#' @param semantic_df Dataframe of visits with start_utc and end_utc.
#' @return A tibble with lat_sem and lon_sem.
#' @export
match_semantic_places <- function(photo_df, semantic_df) {
  if (is.null(semantic_df) || nrow(semantic_df) == 0) return(NULL)

  photo_df |>
    rowwise() |>
    mutate(
      idx = which(semantic_df$start_utc <= timestamp_utc &
                    semantic_df$end_utc >= timestamp_utc)[1],
      lat_sem = ifelse(!is.na(idx), semantic_df$lat[idx], NA),
      lon_sem = ifelse(!is.na(idx), semantic_df$lon[idx], NA)
    ) |>
    ungroup() |>
    select(SourceFile, lat_sem, lon_sem)
}

#' Fuse Data (Master Logic)
#'
#' Combines Plan A (Interpolation) and Plan B (Semantic Matching).
#' Prioritizes Plan A if the uncertainty is < 1 hour. Falls back to Plan B otherwise.
#'
#' @param photo_data Dataframe from get_photo_timestamps().
#' @param clean_signals Dataframe from clean_timeline_signals().
#' @param clean_visits Dataframe from clean_semantic_history().
#' @return A comprehensive dataframe with final_lat, final_lon, and method used.
#' @export
fuse_data <- function(photo_data, clean_signals, clean_visits) {

  # Run Plan A
  plan_a <- interpolate_gps(photo_data$timestamp_utc, clean_signals)

  # Run Plan B
  plan_b <- match_semantic_places(photo_data, clean_visits)

  # Merge
  result <- photo_data |>
    bind_cols(plan_a) |>
    left_join(plan_b, by = "SourceFile") |>
    mutate(
      # Strategy: Use Plan A if valid (uncertainty < 3600 seconds), else Plan B
      use_a = !is.na(lat) & uncertainty < 3600,

      final_lat = ifelse(use_a, lat, lat_sem),
      final_lon = ifelse(use_a, lon, lon_sem),
      method    = ifelse(use_a, "Raw GPS", ifelse(!is.na(lat_sem), "Semantic Place", NA))
    )

  return(result)
}
