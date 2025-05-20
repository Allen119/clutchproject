library(yaml)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)

parse_match <- function(file_path) {
  match_data <- yaml::yaml.load_file(file_path)
  
  match_id <- str_remove(basename(file_path), "\\.yaml$")
  city <- match_data$info$city %||% NA
  competition <- match_data$info$competition %||% NA
  venue <- match_data$info$venue %||% NA
  winner <- if (!is.null(match_data$info$outcome$winner)) match_data$info$outcome$winner else NA
  
  # ✅ Extract the season (year) from date
  season <- if (!is.null(match_data$info$dates[[1]])) {
    as.integer(substr(match_data$info$dates[[1]], 1, 4))
  } else {
    NA_integer_
  }
  
  # Match-level metadata
  match_info <- tibble(
    match_id = match_id,
    city = city,
    competition = competition,
    venue = venue,
    winner = winner,
    season = season  # ✅ add to metadata
  )
  
  # Innings-level (ball-by-ball) data
  innings_list <- map(match_data$innings, function(innings_data) {
    team <- innings_data[[1]]$team
    deliveries <- innings_data[[1]]$deliveries
    
    map_df(deliveries, function(delivery) {
      ball_num <- names(delivery)[1]
      over_ball <- as.numeric(strsplit(ball_num, "\\.")[[1]])
      ball_info <- delivery[[1]]
      
      tibble(
        match_id = match_id,  # keep for joining
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
  
  deliveries_df <- bind_rows(innings_list)
  
  return(list(match = match_info, deliveries = deliveries_df))
}

all_files <- list.files("C:/Users/Hp/Desktop/data/raw/ipl", pattern = "\\.yaml$", full.names = TRUE)

parsed_matches <- map(all_files, parse_match)

# Combine into two master tables
matches_df <- bind_rows(map(parsed_matches, "match"))
deliveries_df <- bind_rows(map(parsed_matches, "deliveries"))

view(matches_df)

deliveries_df <- deliveries_df %>%
  mutate(match_id = as.numeric(match_id)) %>%
  arrange(match_id, over, ball)
view(deliveries_df)

deliveries_df <- deliveries_df %>%
  mutate(phase = case_when(
    over >= 0 & over < 6  ~ "Powerplay",
    over >= 6 & over < 15 ~ "Middle",
    over >= 15 & over < 20 ~ "Death",
    TRUE ~ "Other"
  ))

# Batsman stats
batsman_phase_stats <- deliveries_df %>%
  group_by(batsman, phase) %>%
  summarise(
    runs = sum(runs_batsman),
    balls_faced = n(),
    dot_balls = sum(runs_total == 0),
    strike_rate = round(100 * runs / balls_faced, 2),
    .groups = 'drop'
  )

# Bowler stats
bowler_phase_stats <- deliveries_df %>%
  group_by(bowler, phase) %>%
  summarise(
    runs_conceded = sum(runs_total),
    balls_bowled = n(),
    wickets = sum(wicket),
    dot_balls = sum(runs_total == 0),
    economy = round(6 * runs_conceded / balls_bowled, 2),
    .groups = 'drop'
  )
view(deliveries_df)
batsman_phase_stats <- batsman_phase_stats %>%
  mutate(clutchness = round((strike_rate * 0.6 + dot_balls * 0.2 + runs * 0.2), 2))

bowler_phase_stats <- bowler_phase_stats %>%
  mutate(clutchness = round((wickets * 20 + dot_balls * 2 - economy * 5), 2))

view(batsman_phase_stats)
view(bowler_phase_stats)
unique(deliveries_df$phase)

matches_df <- matches_df %>%
  mutate(match_id = as.numeric(match_id))

# Merge deliveries_df with matches_df to get season info
deliveries_df1 <- deliveries_df %>%
  left_join(matches_df %>% select(match_id, season), by = "match_id")

# Add phase column
deliveries_df1 <- deliveries_df1 %>%
  mutate(phase = case_when(
    over >= 0 & over < 6  ~ "Powerplay",
    over >= 6 & over < 15 ~ "Middle",
    over >= 15 & over < 20 ~ "Death"
  ))

normalize <- function(x) {
  normalized_value <- (x-min(x))/(max(x)-min(x))
}

# Batsman stats with normalized clutch score
batsman_phase_season_stats <- deliveries_df1 %>%
  group_by(season, batsman, phase) %>%
  summarise(
    runs = sum(runs_batsman),
    balls_faced = n(),
    dot_balls = sum(runs_total == 0),
    strike_rate = round(100 * runs / balls_faced, 2),
    .groups = 'drop'
  ) %>%
  group_by(season) %>%
  mutate(
    norm_runs = normalize(runs),
    norm_strike_rate = normalize(strike_rate),
    norm_dot_balls = normalize(dot_balls),
    clutch_score = round(norm_strike_rate * 0.6 + norm_runs * 0.3 - norm_dot_balls * 0.1, 4)
  ) %>%
  ungroup()

# Bowler stats with normalized clutch score
bowler_phase_season_stats <- deliveries_df1 %>%
  group_by(season, bowler, phase) %>%
  summarise(
    runs_conceded = sum(runs_total),
    balls_bowled = n(),
    wickets = sum(wicket),
    dot_balls = sum(runs_total == 0),
    economy = round(6 * runs_conceded / balls_bowled, 2),
    .groups = 'drop'
  ) %>%
  group_by(season) %>%
  mutate(
    norm_wickets = normalize(wickets),
    norm_dot_balls = normalize(dot_balls),
    norm_economy = normalize(economy),
    clutch_score = round(norm_wickets * 0.6 + norm_dot_balls * 0.3 - norm_economy * 0.1, 4)
  ) %>%
  ungroup()


View(batsman_phase_season_stats)
View(bowler_phase_season_stats)

library(ggplot2)
library(dplyr)

top_batsmen_death <- batsman_phase_season_stats %>%
  filter(phase == "Death", season >= 2019) %>%
  group_by(batsman) %>%
  summarise(total_clutch = sum(clutch_score), .groups = "drop") %>%
  arrange(desc(total_clutch)) %>%
  slice_head(n = 10)

ggplot(top_batsmen_death, aes(x = reorder(batsman, total_clutch), y = total_clutch)) +
  geom_col(fill = "#2E86AB") +
  coord_flip() +
  labs(title = "Top 10 Clutch Batsmen in Death Overs (2019–2024)",
       x = "Batsman", y = "Total Clutch Score") +
  theme_minimal()

ggplot(filter(bowler_phase_season_stats, bowler == "JJ Bumrah"),
       aes(x = season, y = clutch_score, color = phase)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "JJ Bumrah's Clutch Score by Phase Over Seasons",
       x = "Season", y = "Clutch Score", color = "Phase") +
  theme_minimal()

library(tidyr)

top_bowlers <- bowler_phase_season_stats %>%
  group_by(bowler) %>%
  summarise(total_clutch = sum(clutch_score)) %>%
  arrange(desc(total_clutch)) %>%
  slice_head(n = 5)

heatmap_data <- bowler_phase_season_stats %>%
  filter(bowler %in% top_bowlers$bowler) %>%
  select(bowler, phase, clutch_score)

ggplot(heatmap_data, aes(x = phase, y = reorder(bowler, -clutch_score), fill = clutch_score)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#FF5733") +
  labs(title = "Clutch Score Heatmap by Phase (Top 5 Bowlers)",
       x = "Phase", y = "Bowler") +
  theme_minimal()

ggplot(batsman_phase_stats, aes(x = clutchness, fill = phase)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~phase, scales = "free") +
  labs(title = "Distribution of Clutchness Scores by Phase",
       x = "Clutchness Score", y = "Density") +
  theme_minimal()


write.csv(batsman_phase_season_stats, "batsman_phase_season_stats1.csv", row.names = FALSE)
write.csv(bowler_phase_season_stats, "bowler_phase_season_stats1.csv", row.names = FALSE)
write.csv(batsman_phase_stats, "batsman_phase_stats.csv", row.names = FALSE)
write.csv(bowler_phase_stats, "bowler_phase_stats.csv", row.names = FALSE)










