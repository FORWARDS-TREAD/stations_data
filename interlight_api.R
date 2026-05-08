library(stringr)
library(httr2)
library(jsonlite)
library(dotenv)

# Create GET request with query params
create_get_request <- function(base_url, query_params) {
  request(base_url) |>
    req_headers("Accept" = "application/json") |>
    req_url_query(!!!query_params)
}

# Create POST request with JSON body
create_post_request <- function(base_url, body_list) {
  request(base_url) |>
    req_headers(
      "Accept" = "application/json",
      "Content-Type" = "application/json; charset=utf-8"
    ) |>
    req_body_json(body_list)
}

# Download and parse JSON response
fetch_json <- function(req) {
  req |>
    req_perform() |>
    resp_body_json()
}

# Save JSON to file
save_json_file <- function(data, file_path) {
  json <- toJSON(data, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json, file_path)
}

download_from_api_post <- function(
  api_url,
  body_params,
  station_name,
  output_dir = "."
) {
  # Build and execute POST request
  req <- request(api_url) |>
    req_headers(
      "Accept" = "application/json",
      "Content-Type" = "application/json; charset=utf-8"
    ) |>
    req_body_json(body_params)

  # Execute and parse response
  data <- fetch_json(req)

  # Extract date components from body_params
  date_str <- as.character(body_params$diaMuestra)
  date_parts <- str_split_1(date_str, "/")
  year_month <- str_c(date_parts[3], "-", date_parts[2])
  year_month_day <- str_c(date_parts[3], "-", date_parts[2], "-", date_parts[1])

  # Build directory path: {output_dir}/stations/sensors_data/{station_name}/year-month
  output_path <- file.path(
    output_dir,
    "stations",
    "sensors_data",
    station_name,
    year_month
  )

  # Create directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Generate filename: year-month-day.json
  file_name <- str_c(year_month_day, ".json")
  file_path <- file.path(output_path, file_name)

  save_json_file(data, file_path)

  file_path
}

download_data <- function(
  date,
  station_name,
  api_url,
  output_dir = "."
) {
  download_from_api_post(
    api_url = api_url,
    body_params = list(
      usuario = Sys.getenv("USER"),
      password = Sys.getenv("PASSWORD"),
      diaMuestra = date
    ),
    station_name,
    output_dir = output_dir
  )
}
