#' Clean Timeline Raw Signals (Plan A)
#'
#' Extracts high-precision GPS points ("Raw Signals") from the Google Timeline JSON.
#' These are the distinct blue dots on a map, useful for linear interpolation.
#'
#' @param raw_json_data The list object returned by jsonlite::fromJSON().
#' @return A tibble with columns: raw_timestamp, lat, lon, accuracy, altitude, timestamp_utc.
#' @export
clean_timeline_signals <- function(raw_json_data) {
  if (!"rawSignals" %in% names(raw_json_data)) return(NULL)

  as_tibble(raw_json_data$rawSignals) |>
    jsonlite::flatten() |>
    filter(!is.na(position.LatLng)) |>
    select(
      raw_timestamp = position.timestamp,
      latlng_string = position.LatLng,
      accuracy      = position.accuracyMeters,
      altitude      = position.altitudeMeters
    ) |>
    mutate(latlng_clean = str_remove_all(latlng_string, "[° ]")) |>
    separate(latlng_clean, into = c("lat", "lon"), sep = ",", convert = TRUE) |>
    mutate(timestamp_utc = ymd_hms(raw_timestamp)) |>
    arrange(timestamp_utc)
}

#' Clean Semantic History (Plan B)
#'
#' Extracts "Place Visits" (e.g., Hotels, Parks) from the Google Timeline JSON.
#' Used as a fallback when raw GPS signals are missing or sparse.
#'
#' @param raw_json_data The list object returned by jsonlite::fromJSON().
#' @return A tibble with columns: start_utc, end_utc, lat, lon, place_id.
#' @export
clean_semantic_history <- function(raw_json_data) {
  if (!"semanticSegments" %in% names(raw_json_data)) return(NULL)

  sem_df <- as_tibble(raw_json_data$semanticSegments)
  if (!"visit" %in% names(sem_df)) return(NULL)

  sem_df |>
    mutate(
      place_lat = visit$topCandidate$placeLocation$latLng,
      place_id  = visit$topCandidate$placeId
    ) |>
    filter(!is.na(place_lat)) |>
    mutate(latlng_clean = str_remove_all(place_lat, "[° ]")) |>
    separate(latlng_clean, into = c("lat", "lon"), sep = ",", convert = TRUE) |>
    mutate(
      start_utc = ymd_hms(startTime),
      end_utc   = ymd_hms(endTime)
    ) |>
    select(start_utc, end_utc, lat, lon, place_id) |>
    arrange(start_utc)
}

#' Get Photo Timestamps
#'
#' Scans a directory for image files and extracts their "DateTimeOriginal" tags.
#'
#' @param folder_path String. Path to the folder containing images.
#' @param tz_offset String. Timezone of the camera clock (e.g., "Pacific/Honolulu").
#' @return A tibble with columns: SourceFile, local_time, timestamp_utc.
#' @export
get_photo_timestamps <- function(folder_path, tz_offset = "Pacific/Honolulu") {
  files <- list.files(folder_path, pattern = "\\.(jpg|jpeg|arw|tif|tiff)$",
                      ignore.case = TRUE, full.names = TRUE)

  message("Scanning ", length(files), " images...")
  read_exif(files, tags = c("SourceFile", "DateTimeOriginal")) |>
    filter(!is.na(DateTimeOriginal)) |>
    mutate(
      local_time = ymd_hms(DateTimeOriginal, tz = tz_offset),
      timestamp_utc = with_tz(local_time, tzone = "UTC")
    ) |>
    select(SourceFile, local_time, timestamp_utc)
}
