#' Build ATP dataset using Jeff Sackman Github data
#'
#' @param path file path for directory of yearly match data csvs
#' @param keep_cols columns to keep after pre-processing
#'
#' @return dataframe of yearly ATP match data with season id
#'
build_atp_data <- function(path, keep_cols) {

  file_paths <- list.files(path = path, pattern = "\\.csv", full.names = TRUE)
  seasons <- stringr::str_extract(file_paths, "\\d{4}")
  file_names <- file_paths |>
    purrr::set_names(nm = seasons)

  purrr::map(all_files, ~ readr::read_csv(.x,
                                                     col_types = readr::cols(
                                                       .default = "?",
                                                       winner_seed = "numeric",
                                                       loser_seed = "numeric"))) |>
    purrr::list_rbind() |>
    dplyr::mutate(season = stringr::str_sub(.data$tourney_date, start = 1L, end = 4L),
                  tourney_date = lubridate::ymd(.data$tourney_date)) |>
    dplyr::select(dplyr::all_of(keep_cols))
}
