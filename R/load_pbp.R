#' Load season play-by-play
#'
#' @param season An integer value or vector of values denoting the end year of the season(s) to scrape.
#' \code{load_pbp} also accepts character strings with more explicit definitions of the
#' season to scrape: '2020-2021', '2020-21', '2020_21' are also acceptable.\cr
#' The default value is the current season, switching to the next year on July 1st when the new league year begins.
#' @param shift_events Logical value; when set to \code{FALSE} this function
#' returns a smaller dataset that excludes specifically shift change events
#'
#' @return A tibble containing all play-by-play data for a given season(s) in
#' the same format as the output of \code{\link{scrape_game}}
#' @export
#'
#' @examples
#' \dontrun{
#' pbp <- load_pbp(2021)
#' }
load_pbp <- function(season = as.numeric(substr(Sys.Date() + 184,1,4)), shift_events = FALSE){

  # Default season switches with new league year on July 1st

  # create df to allow for more inputs
  saved_seasons <- dplyr::tibble(
    season_puckR = seq(2011,as.numeric(substr(Sys.Date() + 184,1,4)),1),
    season_full = glue::glue("{season_puckR-1}-{season_puckR}"),
    season_half = glue::glue("{season_puckR-1}-{substr(season_puckR,3,4)}"),
    season_pbp = glue::glue("{season_puckR-1}_{substr(season_puckR,3,4)}")
  )

  # define seaosns to pull
  to_pull <- saved_seasons %>%
    dplyr::filter(
      season_puckR %in% season |
        season_full %in% season |
        season_half %in% season |
        season_pbp %in% season
    ) %>%
    # get season as used in puckR-data
    dplyr::pull(season_pbp)

  if(length(to_pull) == 0){

    message(glue::glue("No season data found for {season}"))
    pbp_all <- NULL

  } else {

    pbp_all <- NULL
    for(i in to_pull){
      # first check if season data exists (for upcoming seasons)
      season_url <- glue::glue("https://github.com/zbeg/puckR-data/blob/main/data/play_by_play_{i}.rds")
      if(httr::http_status(httr::GET(season_url))$category == "Client error"){
        message(glue::glue("{i} season not available"))
        pbp <- NULL
      } else {
        if(shift_events == FALSE){
          pbp <- readRDS(url(glue::glue("https://github.com/zbeg/puckR-data/raw/main/data/play_by_play_{i}_lite.rds")))
          pbp_all <- dplyr::bind_rows(pbp_all, pbp)
          rm(pbp,i)
        } else {
          pbp <- readRDS(url(glue::glue("https://github.com/zbeg/puckR-data/raw/main/data/play_by_play_{i}.rds")))
          pbp_all <- dplyr::bind_rows(pbp_all, pbp)
          rm(pbp,i)
        }
      }
    }
  }

  return(pbp_all)
}
