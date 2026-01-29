#' Read Google Timeline JSON
#'
#' High-level wrapper to ingest Google Location History data.
#' Returns both Semantic (Place) visits and Raw GPS signals.
#'
#' @param json_path Path to the JSON file (e.g., "Timelines/History.json").
#' @return A list containing two tibbles: $visits (Semantic) and $raw_locations (GPS).
#' @export
read_timeline <- function(json_path) {
  if (!file.exists(json_path)) stop("File not found: ", json_path)

  message("Reading Timeline JSON (this may take a moment)...")
  raw_json <- jsonlite::fromJSON(json_path)

  # Process Plan B (Semantic Places)
  visits_df <- clean_semantic_history(raw_json)

  # Process Plan C (Raw GPS)
  raw_df <- clean_raw_history(raw_json)

  list(
    visits = visits_df,
    raw_locations = raw_df
  )
}

#' Write Geotags to Files
#'
#' A wrapper that generates the manifest and immediately runs ExifTool.
#'
#' @param tagged_df The dataframe returned by geotag_photos().
#' @param photo_folder String. The folder where images are located.
#' @export
write_geotags <- function(tagged_df, photo_folder) {

  # 1. Create Manifest
  csv_file <- write_gps_manifest(tagged_df)

  # 2. Apply to Images
  apply_gps_to_images(csv_file, photo_folder)

  message("Geotagging complete.")
}


#' Geotag Photos (Master Function)
#'
#' This is the main function for the package. It reads timestamps from images,
#' fuses them with the timeline data, and calculates coordinates.
#'
#' @param photo_folder String. Path to the folder containing images.
#' @param timeline_data The list returned by read_timeline().
#' @param tz String. The time zone where the photos were taken (e.g., "Asia/Tokyo", "Europe/Paris").
#'   Crucial for matching camera time (local) to GPS time (UTC). Default is "Pacific/Honolulu".
#' @return A dataframe of photos with calculated coordinates and method used.
#' @export
geotag_photos <- function(photo_folder, timeline_data, tz = "Pacific/Honolulu") {

  message("Processing photos using timezone: ", tz)

  # 1. Get Photo Clocks
  photos <- get_photo_timestamps(photo_folder, tz_offset = tz)

  if (nrow(photos) == 0) stop("No valid images found in folder.")

  # 2. Fuse
  fused <- fuse_data(photos, timeline_data$signals, timeline_data$visits)

  return(fused)
}
