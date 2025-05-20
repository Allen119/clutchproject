library(yaml)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)

# Parse a single match file
parse_single_match <- function(file_path) {
  match_data <- yaml::yaml.load_file(file_path)
  
  # Extract match metadata
  match_id <- stringr::str_remove(basename(file_path), "\\.yaml$")
  city <- match_data$info$city %||% NA
  competition <- match_data$info$competition %||% NA
  venue <- match_data$info$venue %||% NA
  winner <- match_data$info$outcome$winner %||% NA
  
  # Create match-level tibble
  match_df <- tibble(
    match_id = match_id,
    city = city,
    competition = competition,
    venue = venue,
    winner = winner
  )
  
  # Loop through innings
  deliveries_df <- map_df(match_data$innings, function(inning) {
    team <- inning[[1]]$team
    deliveries <- inning[[1]]$deliveries
    
    map_df(deliveries, function(delivery) {
      ball_num <- names(delivery)[1]
      over_ball <- as.numeric(strsplit(ball_num, "\\.")[[1]])
      ball_info <- delivery[[1]]
      
      tibble(
        match_id = match_id,
        team = team,
        over = over_ball[1],
        ball = over_ball[2],
        batsman = ball_info$batsman,
        bowler = ball_info$bowler,
        non_striker = ball_info$non_striker,
        runs_batsman = ball_info$runs$batsman,
        runs_extras = ball_info$runs$extras,
        runs_total = ball_info$runs$total,
        extras_type = if (!is.null(ball_info$extras)) paste(names(ball_info$extras), collapse = ",") else NA,
        wicket = if (!is.null(ball_info$wicket)) 1 else 0,
        dismissal_kind = if (!is.null(ball_info$wicket)) ball_info$wicket$kind else NA,
        player_dismissed = if (!is.null(ball_info$wicket)) ball_info$wicket$player_out else NA
      )
    })
  })
  
  # Return both components
  list(
    match = match_df,
    deliveries = deliveries_df
  )
}

# 📂 Example: Read a single match file
parsed <- parse_single_match("C:/Users/Hp/Desktop/data/raw/ipl/829727.yaml")

# View components
View(parsed$match)       # One row
View(parsed$deliveries)  # All ball-level rows
