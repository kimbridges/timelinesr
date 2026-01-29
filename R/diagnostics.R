#' Analyze Place Visit Frequency
#'
#' Summarizes how many semantic "anchor points" exist per day.
#' Useful for determining if Plan B has enough density to improve interpolation.
#'
#' @param timeline_data The list returned by read_timeline().
#' @return A summary dataframe of visits per day.
#' @export
summarize_daily_places <- function(timeline_data) {

  visits <- timeline_data$visits

  daily_stats <- visits |>
    mutate(date = as_date(start_utc)) |>
    group_by(date) |>
    summarise(
      visit_count = n(),
      unique_places = n_distinct(place_name),
      avg_duration_min = round(mean(duration_m, na.rm = TRUE), 1)
    ) |>
    arrange(desc(visit_count))

  # Print a quick console report
  message("--- Semantic Density Report ---")
  message("Total Days with Data: ", nrow(daily_stats))
  message("Average Visits per Day: ", round(mean(daily_stats$visit_count), 1))
  message("Max Visits in one Day: ", max(daily_stats$visit_count))

  return(daily_stats)
}

#' Print Daily Log
#'
#' Prints a readable schedule of where Google thought you were on a specific date.
#'
#' @param timeline_data The list returned by read_timeline().
#' @param target_date String or Date (e.g., "2025-05-20").
#' @param tz String. Timezone for display (default: "Pacific/Honolulu").
#' @export
inspect_day_log <- function(timeline_data, target_date, tz = "Pacific/Honolulu") {

  target <- as_date(target_date)

  visits <- timeline_data$visits |>
    filter(as_date(with_tz(start_utc, tz)) == target) |>
    arrange(start_utc) |>
    mutate(
      start_local = format(with_tz(start_utc, tz), "%H:%M"),
      end_local   = format(with_tz(end_utc, tz), "%H:%M")
    ) |>
    select(start_local, end_local, place_name, duration_m)

  if(nrow(visits) == 0) {
    message("No visits found for date: ", target)
    return(NULL)
  }

  print(knitr::kable(visits, format = "simple", caption = paste("Log for", target)))
}

#' Compare Raw vs Semantic Range
#'
#' Checks if the raw GPS signals show movement that the Semantic layer ignored.
#' Handles Timezone conversion to ensure we catch the right local day.
#'
#' @param timeline_data The list object from read_timeline().
#' @param target_date String (e.g. "2025-11-26").
#' @param tz String. The local timezone to check against (default "Pacific/Honolulu").
#' @export
check_hidden_movement <- function(timeline_data, target_date, tz = "Pacific/Honolulu") {

  target <- as_date(target_date)

  # 1. Get Semantic Spread (with TZ adjustment)
  sem_points <- timeline_data$visits |>
    mutate(date_local = as_date(with_tz(start_utc, tz))) |>
    filter(date_local == target)

  # 2. Get Raw Spread (with TZ adjustment)
  #    We convert the UTC timestamp to Local Time, THEN extract the date.
  if(!"raw_locations" %in% names(timeline_data)) {
    stop("No 'raw_locations' found.")
  }

  raw_points <- timeline_data$raw_locations |>
    mutate(date_local = as_date(with_tz(timestamp, tz))) |>
    filter(date_local == target)

  message("--- Analyzing Date: ", target, " (", tz, ") ---")

  # Calculate Semantic "Movement"
  if(nrow(sem_points) > 0) {
    # Calculate diagonal distance of the bounding box
    sem_dist <- sqrt(diff(range(sem_points$lat))^2 + diff(range(sem_points$lon))^2) * 111
    message("Semantic Spread: ", round(sem_dist, 3), " km (Distinct places: ", n_distinct(sem_points$place_name), ")")
  } else {
    message("Semantic: No data for this local date.")
  }

  # Calculate Raw "Movement"
  if(nrow(raw_points) > 0) {
    raw_dist <- sqrt(diff(range(raw_points$lat))^2 + diff(range(raw_points$lon))^2) * 111
    message("Raw Sensor Spread: ", round(raw_dist, 3), " km (Points: ", nrow(raw_points), ")")

    # BONUS: Show the time range covered
    time_range <- range(with_tz(raw_points$timestamp, tz))
    message("   Raw Coverage: ", format(time_range[1], "%H:%M"), " to ", format(time_range[2], "%H:%M"))
  } else {
    message("Raw: No data for this local date.")
  }
}
