
dat_matrix <- dat_matrix %>%  
  rename(`Defense / Offense` =	offense_defense) %>%
  rename(`Ball start y-axis` =	ball_start_y) %>%
  rename(`Ball end y-axis` =	ball_end_y) %>%
  rename(`Max speed players` =	maxSpeed) %>%
  rename(`Time left in match` =	time_left) %>%
  rename(`Mean speed players` =	meanSpeed) %>%
  rename(`Mean speed ball` =	meanSpeedBall) %>%
  rename(`Max speed ball` =	maxSpeedBall) %>%
  rename(`Mean n passes per player/min` = passes_min_mean) %>%
  rename(`SD n passes per player / minute` =	passes_min_sd) %>%
  rename(`Mean passes per player` =	mean_passes_player) %>%
  rename(`SD passes per player` =	sd_passes_player) %>%
  rename(`Total number of passes` =	n_passes_total) %>%
  rename(`Distance from min x to max x` =	dist_x) %>%
  rename(`Distance from min y to max y` =	dist_y) %>%
  rename(`Convex Hull area` =	chull_area) %>%
  
  # ALIGNMENT
  rename(`Mean speed alignment` =	mean_alignment_speed) %>%
  rename(`SD speed alignment` =	sd_alignment_speed) %>%
  rename(`Max speed alignment` =	max_alignment_speed) %>%
  rename(`Mean directional alignment` =	mean_alignment_dir) %>%
  rename(`SD directional alignment` =	sd_alignment_dir) %>%
  rename(`Max directional alignment` =	max_alignment_dir) %>%
  rename(`Mean absolute directional alignment` =	mean_abs_alignment_dir) %>%
  rename(`Max absolute directional alignment` =	max_abs_alignment_dir) %>%
  rename(`SD absolute directional alignment` =	sd_abs_alignment_dir) %>%
  
  
  # CENTRALITY
  rename(`Mean centrality direction` =	mean_cent_dir) %>%
  rename(`SD centrality direction` =	sd_cent_dir) %>%
  rename(`Max centrality direction` =	max_cent_dir) %>%
  rename(`Mean centrality position` =	mean_cent_pos) %>%
  rename(`SD centrality position` =	sd_cent_pos) %>%
  rename(`Max centrality position` =	max_cent_pos) %>%
  rename(`Mean centrality speed` =	mean_cent_speed) %>%
  rename(`SD centrality speed` =	sd_cent_speed) %>%
  rename(`Max centrality speed` =	max_cent_speed) %>%
  rename(`Mean centrality passes` =	mean_cent_pass) %>%
  rename(`Max centrality passes` =	max_cent_pass) %>%
  rename(`SD centrality passes` =	sd_cent_pass)
