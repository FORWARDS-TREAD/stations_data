source("interlight_api.R")

download_historical_range <- function(start_date, end_date, output_dir = ".") {
  # Parse dates (expecting YYYY-MM-DD)
  start <- as.Date(start_date)
  end <- as.Date(end_date)

  if (start > end) {
    stop("Start date must be before or equal to end date.")
  }

  date_seq <- seq(from = start, to = end, by = "day")

  # Load stations configuration
  stations <- read.csv("stations_config.csv", stringsAsFactors = FALSE)
  stations_to_process <- stations[
    !is.na(stations$api_url) & stations$api_url != "",
  ]

  if (nrow(stations_to_process) == 0) {
    message("No stations with api_url found in stations_config.csv")
    return(invisible(NULL))
  }

  for (i in 1:nrow(stations_to_process)) {
    station <- stations_to_process[i, ]
    message(paste("\n--- Processing station:", station$id, "---"))

    for (d in seq_along(date_seq)) {
      current_date_obj <- date_seq[d]
      api_date_str <- format(current_date_obj, "%d/%m/%Y")

      message(paste("Downloading date:", api_date_str))

      tryCatch(
        {
          download_data(
            api_date_str,
            station$id,
            station$api_url,
            output_dir = output_dir
          )
        },
        error = function(e) {
          warning(paste(
            "Failed to download data for",
            station$id,
            "on",
            api_date_str,
            ":",
            e$message
          ))
        }
      )
    }
  }
}

download_historical_range("2026-03-05", "2026-04-15")
