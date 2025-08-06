library(tidyverse)

# Read match data
set.seed(468)
chou_matches_metadata <- read_csv("match.csv") |> 
  # Filter matches involving CHOU Tien Chen
  filter(winner == "CHOU Tien Chen" | loser == "CHOU Tien Chen") |>
  # Add a match_id column
  mutate(match_id = sample(1000:9999, n(), replace = FALSE)) |>
  select(match_id, tournament, round, year, set, winner, loser, duration, video)

rally_data <- tibble()

for (i in 1:nrow(chou_matches_metadata)) {
  video <- chou_matches_metadata$video[i]
  match_id <- chou_matches_metadata$match_id[i]
  
  match_folder <- file.path(video)
  set_files <- list.files(match_folder, full.names = TRUE)
  
  for (file_path in set_files) {
    set_id <- str_extract(basename(file_path), "\\d+") |>
      as.integer()
    set_data <- read_csv(file_path) |>
      mutate(
        match_id = match_id,
        set_id = set_id
      )
    rally_data <- bind_rows(rally_data, set_data)
  }
}

# Translation table
shot_type_translation <- tibble(
  chinese = c(
    "放小球", "擋小球", "殺球", "點扣", "挑球", "防守回挑", "長球", "平球", 
    "小平球", "後場抽平球", "切球", "過度切球", "推球", "撲球", 
    "防守回抽", "勾球", "發短球", "發長球", "未知球種"
  ),
  english = c(
    "net shot", "return net", "smash", "wrist smash", "lob", "defensive return lob", "clear", "drive", 
    "driven flight", "back-court drive", "drop", "passive drop", "push", "rush", 
    "defensive return drive", "cross-court net shot", "short service", "long service", "unknown"
  )
)

rally_data <- rally_data |> 
  left_join(shot_type_translation, by = c("type" = "chinese")) |>
  select(-type) |>
  rename(type = english)


rally_data <- rally_data |>
  left_join(chou_matches_metadata, by = "match_id") |>
  mutate(
    player_name = if_else(player == "A", winner, loser),
    point_winner_name = if_else(getpoint_player == "A", winner, 
                        if_else(getpoint_player == "B", loser, NA)),
    backhand = if_else(is.na(backhand), 0, backhand),
    chou_score = if_else(winner == "CHOU Tien Chen", roundscore_A, roundscore_B),
    opp_score  = if_else(winner == "CHOU Tien Chen", roundscore_B, roundscore_A)
  ) |>
  select(match_id, set_id, rally, ball_round, time, player_name, backhand, 
         landing_x, landing_y, type, flaw, chou_score,opp_score,
         player_location_x, player_location_y, 
         opponent_location_x, opponent_location_y,
         point_winner_name) 

matches_homography <- read_csv("homography.csv") |>
  left_join(chou_matches_metadata, by = "video") |>
  filter(winner == "CHOU Tien Chen" | loser == "CHOU Tien Chen") 

convert_homography <- function(matrix) {
  as.numeric(
    str_remove_all(matrix, "\\[|\\]") |> 
      str_split(",") |> 
      unlist()) |>
      matrix(nrow = 3, byrow = TRUE)
}

convert_pixels <- function(H,x,y) {
  point <- c(x,y,1)
  transformed_matrix <- H %*% point
  scale_factor <- transformed_matrix[3]
  x <- round(transformed_matrix[1]/scale_factor, 0)
  y <- round(transformed_matrix[2]/scale_factor, 0)
  return (c(x,y))
}

matches_homography <- matches_homography |>
  rowwise() |>
  mutate(
    H = list(convert_homography(homography_matrix))
  ) |>
  select(-homography_matrix)|>
  ungroup()

matches_homography <- matches_homography |>
  rowwise() |>
  mutate(
    t_upleft = list(convert_pixels(H, upleft_x, upleft_y)),
    t_upleft_x = t_upleft[[1]],
    t_upleft_y = t_upleft[[2]],
    t_upright = list(convert_pixels(H, upright_x, upright_y)),
    t_upright_x = t_upright[[1]],
    t_upright_y = t_upright[[2]],
    t_downleft = list(convert_pixels(H, downleft_x, downleft_y)),
    t_downleft_x = t_downleft[[1]],
    t_downleft_y = t_downleft[[2]],
    t_downright = list(convert_pixels(H, downright_x, downright_y)),
    t_downright_x = t_downright[[1]],
    t_downright_y = t_downright[[2]]
  ) |>
  ungroup() 
  
court_coordinates <- matches_homography |>
  select(t_upleft_x, t_upleft_y,
         t_upright_x, t_upright_y,
         t_downleft_x, t_downleft_y,
         t_downright_x, t_downright_y) |>
  slice(1)

matches_homography <- matches_homography |>
  select(match_id, H)

rally_data <- rally_data |>
  mutate(
    chou_location_x = if_else(
      player_name == "CHOU Tien Chen",
      player_location_x, 
      opponent_location_x
    ),
    chou_location_y = if_else(
      player_name == "CHOU Tien Chen",
      player_location_y, 
      opponent_location_y
    ),
    opp_location_x = if_else(
      player_name != "CHOU Tien Chen",
      player_location_x, 
      opponent_location_x
    ),
    opp_location_y = if_else(
      player_name != "CHOU Tien Chen",
      player_location_y, 
      opponent_location_y
    )
  ) |>
  select(-player_location_x,-player_location_y,
         -opponent_location_x, -opponent_location_y)

rally_data <- rally_data |>
  left_join(matches_homography, by = "match_id") |>
  rowwise() |>
  mutate(
    t_shuttle = list(convert_pixels(H, landing_x, landing_y)),
    shuttle_x = t_shuttle[[1]],
    shuttle_y = t_shuttle[[2]],
    t_chou = list(convert_pixels(H, chou_location_x, chou_location_y)),
    chou_x = t_chou[[1]],
    chou_y = t_chou[[2]],
    t_opp = list(convert_pixels(H, opp_location_x, opp_location_y)),
    opp_x = t_opp[[1]],
    opp_y = t_opp[[2]]
  ) |>
  select(-t_shuttle, -t_chou, -t_opp,
         -chou_location_x, -chou_location_y,
         -opp_location_x, -opp_location_y,
         -landing_x, -landing_y,-H) |>
  ungroup() 

chou_matches_metadata <- chou_matches_metadata |>
  select(-video)

court_coordinates <- court_coordinates |>
  rename(
    upleft_x = t_upleft_x,
    upleft_y = t_upleft_y,
    upright_x = t_upright_x,
    upright_y = t_upright_y,
    downleft_x = t_downleft_x,
    downleft_y = t_downleft_y,
    downright_x = t_downright_x,
    downright_y = t_downright_y
  )

rally_data <- rally_data |>
  rename(
    set = set_id,
    stroke = ball_round,         
    current_player = player_name,
    shot_type = type,
    foul = flaw,
    rally_winner = point_winner_name
  )

set_data <- rally_data |>
  filter(
    !is.na(rally_winner),
    chou_score == 21 | opp_score == 21
  ) |>
  select(match_id, set, rally_winner, chou_score, opp_score) |>
  rename(set_winner = rally_winner)

  

write_csv(chou_matches_metadata, "../cleaned data/matches_metadata.csv")
write_csv(court_coordinates, "../cleaned data/court_coordinates.csv")
write_csv(rally_data, "../cleaned data/rally_data.csv")
write_csv(set_data, "../cleaned data/set_data.csv")

