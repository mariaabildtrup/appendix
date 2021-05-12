
# Setting up --------------------------------------------------------------
library(pacman)
p_load(dplyr, lubridate, tidytext, ggraph, tidygraph, visNetwork, tidyr, tidyverse, kableExtra, DT, igraph, scales, raster, grDevices, sp, animation, gganimate, transformr, plotly)

# Load tracking data 
df_tracking <- readRDS("output_data/df_tracking_preprocessing.rds")

# Load event data 
df_event <- readRDS("output_data/event_preprocessing.rds")

# Preprocessing event data
df_event <- df_event %>% 
  arrange(matchId, periodId, time) %>%  
  filter(typeId == 1)

df_event <- subset(df_event, contestantId=="9qsmopgutr7ut5g6workk8w4i")  
df_event <- df_event[, colSums(is.na(df_event)) != nrow(df_event)]

# Add 'ID' column for each unique match_half_phase
df_tracking$ID = paste(df_tracking$matchId, df_tracking$half, df_tracking$phase_id, sep="_")

# Background variables ----------------------------------------------------
home_away <- df_tracking %>%  
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i") %>%  
  distinct(matchId, BIF_home_away=teamHA) %>%  
  mutate(BIF_home_away = ifelse(BIF_home_away==1, "H", "A"))

df_tracking <- df_tracking %>% 
  left_join(home_away, by="matchId")

df_tracking <- df_tracking %>% 
  mutate(offense_defense = ifelse(ballOwnership==BIF_home_away, "offense", "defense"))

rm(home_away)

players_shirtnumber <- df_tracking %>%  
  distinct(playerId, shirtNumber)

df_event_pass <- df_event

df_event_pass <- df_event_pass %>%  
  inner_join(players_shirtnumber, by="playerId")

rm(players_shirtnumber)

temp <- df_event_pass %>% 
  group_by(matchId, periodId, shirtNumber, playerName) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%  
  ungroup() %>%  
  group_by(matchId, periodId) %>%  
  summarize(mean_passes_player = round(mean(n),2), sd_passes_player = round(sd(n),2))

#nrows_df_event_pass = nrow(df_event_pass)

df_event_pass <- df_event_pass %>% 
  group_by(matchId, periodId, min) %>%  
  summarize(n = n()) %>% 
  ungroup() %>% 
  dplyr::select(matchId, periodId,  min, n)

df_event_pass <- df_event_pass %>% 
  group_by(matchId, periodId) %>% 
  summarize(passes_min_mean = mean(n), passes_min_sd = sd(n), n_passes_total = sum(n)) %>% 
  ungroup()

df_event_pass <- merge(df_event_pass, temp, by=c("matchId", "periodId"))

rm(temp)

# Create dataset with basic features 
features <- df_tracking %>%
  ungroup() %>% 
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i") %>% 
  group_by(matchId, half, phase_id) %>% 
  summarize(maxSpeed=max(speed), meanSpeed=mean(speed), meanSpeedBall=mean(speedBall), maxSpeedBall=max(speedBall), ) %>%  
  ungroup()

features <- features %>% 
  left_join(df_tracking[, c("matchId", "half", "phase_id", "offense_defense")], by=c("matchId", "half", "phase_id")) %>% 
  left_join(df_event_pass, by=c("matchId", "half"="periodId")) %>%   
  distinct(matchId, phase_id, half, offense_defense, meanSpeedBall, maxSpeedBall, meanSpeed, maxSpeed, n_passes_total, passes_min_mean, passes_min_sd, mean_passes_player, sd_passes_player) 



#-----Adding start and end of ball-----#
temp = df_tracking %>% 
  mutate(xBall=xBall*direction, yBall=yBall*direction, x=x*direction, y=y*direction) %>% 
  group_by(matchId, half, phase_id) %>% 
  arrange(time) %>%  
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(matchId, half, phase_id, ball_start_x = xBall, ball_start_y = yBall)

features <- left_join(features, temp, by="phase_id")

temp = df_tracking %>% 
  mutate(xBall=xBall*direction, yBall=yBall*direction, x=x*direction, y=y*direction) %>% 
  group_by(matchId, half, phase_id) %>% 
  arrange(desc(time)) %>%  
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(matchId, half, phase_id, ball_end_x = xBall, ball_end_y = yBall)

temp <- temp %>%  
  dplyr::select(phase_id, ball_end_x, ball_end_y)

features <- left_join(features, temp, by="phase_id")

# Time left in match
df_tracking <- df_tracking %>%
  mutate(total_time = ifelse(half==1, min, min+45)) %>%  
  group_by(phase_id) %>%  
  mutate(time_left = 90-min(total_time)) %>% 
  dplyr::select(-total_time) %>%  
  ungroup()

features <- features %>% 
  left_join(df_tracking[,c("phase_id", "time_left")], by=c("phase_id"="phase_id")) %>%  
  distinct()


rm(temp)

# Compactness -------------------------------------------------------------

# Dist on x and y axes 

temp <- df_tracking %>%
  group_by(matchId, half, phase_id) %>% 
  filter(gk==0) %>% 
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i") %>% 
  summarize(dist_x = (max(x)-min(x)), dist_y = (max(y)-min(y))) %>% 
  ungroup()

features <- left_join(features, temp, by=c("matchId", "half", "phase_id"))

# Convex Hull

df_tracking$times = paste(df_tracking$ID, df_tracking$time, sep="_") 

df_hull_input <- df_tracking %>% 
  filter(gk==0) %>%                                         # Exclude goal keeper 
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i") %>%     # Keep only data on BIF players 
  ungroup() 

times <- unique(df_hull_input$times)
chull_areas  <- list()
time_id <- list()
chull_df <- data.frame()

ch_func <- function(time_input) {
  
  i = match(time_input, times)
  
  df_hull <- subset(df_hull_input, times == time_input, select = c("x", "y"))
  
  c.hull <- chull(df_hull)  #Calculates convex hull
  c.hull3 <- c(c.hull, c.hull[1]) #You need five points to draw four line segments, so we add the first set of points at the end
  
  df2 <- as.data.frame(cbind(1,df_hull[c.hull3 ,]$x,df_hull[c.hull3 ,]$y))
  colnames(df2) <- c('ID','x','y')
  
  # Area of the convex hull 
  chull.coords <- df_hull[c.hull3 ,]
  chull.poly <- Polygon(chull.coords, hole=F)  #From the package sp
  
  chull.area <- chull.poly@area
  
  #ch_df <- data.frame(time_input, chull.area)
  #return(ch_df)

  #chull_areas[i] <- chull.area
  #time_id[i] <- time_input
   
  #df_test <- data.frame(chull=chull.area, time=time_input)
  
  return(chull.area)
  
#  gc() # Remove unused memory space to avoid memory error 
}

chull_df_time <- sapply(times, ch_func)

#load("temp_workspace.RData") # DETTE MARKERES 

chull_df_time_test <- t(bind_rows(chull_df_time))
chull_df_time_test <- as.data.frame(chull_df_time_test)
chull_df_time_test$time <- rownames(chull_df_time_test) 
rownames(chull_df_time_test) <- 1:nrow(chull_df_time_test)
chull_df_time_test <- chull_df_time_test %>%  
  rename(chull_area=V1)

chull_df <- chull_df_time_test %>%  
  separate(time, c("matchId", "half", "phase_id", "time"), sep="_") %>% 
  mutate(half = as.numeric(half), time=as.numeric(time), chull_area=as.numeric(chull_area), phase_id=as.numeric(phase_id)) %>% 
  group_by(matchId, half, phase_id) %>%  
  summarize(chull_area = mean(chull_area)) %>%  
  ungroup()

rm(chull_df_time, chull_df_time_test)

features <- features %>% 
  left_join(chull_df, by=c("matchId", "half", "phase_id"))

rm(chull_df)


# Coordination and alignment ----------------------------------------------

# load("temp_image_dir.RData")

#----- DIRECTIONAL ALIGNMENT -----#

load("temp_image_dir.RData")

gc()

temp_df <- df_tracking %>% 
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i")  %>% 
  filter(gk==0) %>% 
  filter(phase_id!=33109 & phase_id!=49898) %>%  # Da disse phase IDs ikke har samme antal spillere
  group_by(shirtNumber, phase_id, ID, time) %>%
  summarize(dir=mean(dir)) %>%  
  ungroup() %>% 
#  distinct(shirtNumber, phase_id, ID, time, dir) 
  select(shirtNumber, phase_id, dir) 

temp_df$dir <-  rescale(temp_df$dir, to = c(1, 10))

#phaseIDs = unique(temp_df$ID)
phaseIDs = unique(temp_df$phase_id)
df_dir = data.frame()
rm(df_dir_2)

# Create function
df_function <- function(phaseID_input) {


  subset <- subset(temp_df, phase_id==phaseID_input) 
  
  temp <- tidybayes::gather_pairs(
    subset,
    shirtNumber,
    dir,
    row = "shirtNumber1",
    col = "shirtNumber2",
    x = "dir_1",
    y = "dir_2",
    triangle = c("lower only", "upper only", "lower", "upper", "both only", "both")) %>% 
    mutate(pair = paste0(shirtNumber1, "_", shirtNumber2)) %>% 
    mutate(phase_id = phaseID_input)
  
#  df_dir <<- rbind(df_dir, temp)
  return(list(shirtNumber1=temp$shirtNumber1, dir_1 = temp$dir_1, shirtNumber2=temp$shirtNumber2, dir_2 = temp$dir_2, pair=temp$pair, phase_id=temp$phase_id))
  
  gc() 
} 

df_dir <- sapply(phaseIDs, df_function)

df_dir <- data.frame(shirtNumber1 = unlist(df_dir[1,]), dir_1 = unlist(df_dir[2,]), shirtNumber2 = unlist(df_dir[3,]), dir_2 = unlist(df_dir[4,]), pair = unlist(df_dir[5,]), phase_id = unlist(df_dir[6,]))

#saveRDS(df_angle_phase_pair, "output_data/df_angle_phase_pair.rds")
#saveRDS(abs_df_angle_phase_pair, "output_data/abs_df_angle_phase_pair.rds")

gc()


# Remove NAs in dir columns 
df_dir <- df_dir %>%
  mutate(phase_id=as.numeric(phase_id), dir_1 = as.numeric(dir_1), dir_2 = as.numeric(dir_2)) %>% 
  filter(!is.na(dir_1)) %>% 
  filter(!is.na(dir_2)) 

# Calculate cor. coeff.
df_angle_phase_pair <- df_dir %>% 
  ungroup() %>% 
  group_by(phase_id, shirtNumber1, shirtNumber2) %>% 
  summarize(cor = cor(dir_1, dir_2, method="spearman")) %>% 
  ungroup()

df_angle_pair <- df_angle_phase_pair %>% 
  group_by(shirtNumber1, shirtNumber2) %>% 
  summarize(cor = mean(cor)) %>% 
  ungroup()

df_angle_phase <- df_angle_phase_pair %>% 
  group_by(phase_id) %>% 
  summarize(max_alignment_dir = max(cor), mean_alignment_dir = mean(cor), sd_alignment_dir = sd(cor))

# Abs. cor. values 
abs_df_angle_phase_pair <- df_dir %>% 
  group_by(matchId, half, phase_id, shirtNumber1, shirtNumber2) %>% 
  summarize(cor = abs(cor(dir_1, dir_2, method="spearman"))) %>% 
  ungroup()

abs_df_angle_phase <- abs_df_angle_phase_pair %>% 
  group_by(matchId, half, phase_id) %>% 
  summarize(max_abs_alignment_dir = max(cor, na.rm=T), mean_abs_alignment_dir = mean(cor, na.rm=T), sd_abs_alignment_dir = sd(cor, na.rm=T))


#--- Merge with features data ---#
features <- features %>% 
  left_join(df_angle_phase, by=c("phase_id")) %>%  
  left_join(abs_df_angle_phase, by=c("phase_id"))

rm(temp, temp_df, df_hull_input, phaseIDs, times)

saveRDS(df_tracking, "output_data/df_tracking.rds")
saveRDS(df_event, "output_data/df_event.rds")
rm(df_tracking, df_event, abs_df_angle_phase, abs_df_angle_pair, abs_df_angle_phase_pair, df_angle_pair, df_angle_phase, df_angle_phase_pair)


#--------- SPEED ALIGNMENT ---------#

#save.image("before_speed.RData")
load("temp_image_dir.RData")
df_tracking <- readRDS("output_data/df_tracking.rds")

temp_df <- df_tracking %>% 
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i")  %>% 
  filter(gk==0) %>% 
  filter(phase_id!=33109 & phase_id!=49898) %>%  # Da disse phase IDs ikke har samme antal spillere
  group_by(shirtNumber, phase_id, time) %>%  
  summarize(speed=mean(speed)) %>%  
  ungroup() %>%  
  dplyr::select(shirtNumber, speed, phase_id) %>%  
  mutate(speed = rescale(speed, to=c(1,10))) 
  
phaseIDs = unique(temp_df$phase_id)
df = data.frame()

# Create function
df_function <- function(phaseID_input) {
  
  #phaseID_input = 0 
  
  subset <- subset(temp_df, phase_id==phaseID_input)
  
  temp <- tidybayes::gather_pairs(
    subset,
    shirtNumber,
    speed,
    row = "shirtNumber1",
    col = "shirtNumber2",
    x = "speed_1",
    y = "speed_2",
    triangle = c("lower only", "upper only", "lower", "upper", "both only", "both")) %>% 
#    mutate(pair = paste0(shirtNumber1, "_", shirtNumber2)) %>% 
    mutate(phase_id = phaseID_input)
  
  return(list(shirtNumber1=temp$shirtNumber1, speed_1 = temp$speed_1, shirtNumber2=temp$shirtNumber2, speed_2 = temp$speed_2, phase_id=temp$phase_id))
  
  gc() 
  
  #rm(temp, subset)
} 

df <- sapply(phaseIDs, df_function)
df_test <- data.frame(shirtNumber1 = unlist(df[1,]), speed_1 = unlist(df[2,]), shirtNumber2 = unlist(df[3,]), speed_2 = unlist(df[4,]), phase_id = unlist(df[5,]))

df_test <- df_test %>%  
  filter(!is.na(speed_1)) %>%  
  filter(!is.na(speed_2)) 

df <- readRDS("output_data/df_speed.rds")

#df_long <- df 

gc()

# Calculate cor. coeff.
memory.limit(20000)

df_cor_phase_pair <- df %>%  
  group_by(phase_id, shirtNumber1, shirtNumber2) %>% 
  #summarize(cor = cor(speed_1, speed_2, method="spearman")) %>% 
  summarize(cor = cor(speed_1, speed_2, method="pearson")) %>% 
  ungroup()

#saveRDS(df, "output_data/df_speed.rds")

df_cor_phase_pair <- df_cor_phase_pair %>%  
  filter(!is.na(cor))

#saveRDS(df_cor_phase_pair, "output_data/df_cor_phase_pair.rds")

df_cor_pair <- df_cor_phase_pair %>% 
  group_by(shirtNumber1, shirtNumber2) %>% 
  summarize(cor = mean(cor)) %>% 
  ungroup()

#df_cor_phase_pair <- readRDS("output_data/df_cor_phase_pair.rds")

df_cor_phase <- df_cor_phase_pair %>% 
  group_by(phase_id) %>% 
  summarize(max_alignment_speed = max(cor), mean_alignment_speed = mean(cor), sd_alignment_speed=sd(cor)) 

features <- features %>% 
  left_join(df_cor_phase, by=c("phase_id"))

save.image("before_leadership.RData")
rm(df, df_cor_phase_pair, df_cor_pair, df_cor_phase)

#saveRDS(features, "output_data/features.rds")
#saveRDS(df_cor_phase_pair, "output_data/df_cor_phase_pair.rds")

# Leadership --------------------------------------------------------------

#------- PASSES ---------#

#df_event <- readRDS("output_data/df_event.rds")

df_pass <- df_event %>%
  filter(typeId == 1) %>%  
  filter(contestantId =="9qsmopgutr7ut5g6workk8w4i") %>% 
  mutate(match_half = paste0(matchId, "_", periodId)) %>% 
  dplyr::select(playerId, id, match_half, outcome)

#turnover variable 
df_pass <- df_pass %>% 
  mutate(turn_over = ifelse(lag(outcome)==0, 1, 0)) %>% 
  mutate(turn_over = ifelse(is.na(turn_over), 0, turn_over))

match_halfs <- unique(df_pass$match_half)
mean_cent_values <- list()
max_cent_values <- list()
sd_cent_values <- list()
match_half <- list()

# Creating function for calculating centrality 
centr_func <- function(match_half_input) {
  
  subset <- subset(df_pass, match_half==match_half_input)
  
  size <- subset %>% 
    group_by(label=playerId) %>% 
    summarize(size=n()) %>% 
    mutate(label=as.character(label))
  
  passings <- data.frame(paste(subset$playerId, collapse = " "))
  names(passings)[1] <- "pass"
  
  passings <- passings %>% 
    unnest_tokens(bigram, pass, token = "ngrams", n = 2) %>% 
    separate(bigram, c("player1", "player2"), sep = " ") %>% 
    rowid_to_column("id") 
  
  passings <- passings %>% 
    left_join(df_pass, by="id") %>% 
    dplyr::select(player1, player2, turn_over) 
  
  passings$turn_over <- passings$turn_over %>% 
    replace_na(0)
  
  # Remove passings before a turnover
  turns <- list()
  for (i in 1:nrow(passings)){
    turn = ifelse(passings$turn_over[i+1]==1, 1, 0)
    turns <- append(turns, turn)
  }
  
  passings$after_turnover <- turns
  
  passings <- passings %>% 
    filter(after_turnover==0) %>% 
    filter(player1!=player2) %>% 
    count(player1, player2, sort = TRUE) %>% 
    rename(weight=n)
  
  players <- passings %>% 
    dplyr::select(player1, player2) %>% 
    t %>% c %>% unique %>% 
    data.frame() 
  
  g <- graph_from_data_frame(passings, directed=FALSE, vertices=players)  
  
  mean_cent_values <- mean(closeness(g))
  max_cent_values <- max(closeness(g))
  sd_cent_values <- sd(closeness(g))
  match_half <- match_half_input
  
  return(list(mean_cent_values, max_cent_values, sd_cent_values, match_half))
  
  gc() 
  
}

df_pass_cent <- sapply(match_halfs, centr_func)

df_pass_cent <- data.frame(mean_cent_pass = unlist(df_pass_cent[1,]), max_cent_pass = unlist(df_pass_cent[2,]), sd_cent_pass = unlist(df_pass_cent[3,]), match_half = unlist(df_pass_cent[4,]))

df_pass_cent <- df_pass_cent %>% 
  separate(match_half, c("matchId", "half"), sep="_") %>% 
  mutate(half=as.numeric(half))

#features <- readRDS("output_data/features.rds")

features <- features %>%  
  left_join(df_pass_cent, by=c("matchId", "half"))

#------- POSITION ---------#

# Load distances data 
distances_df <- readRDS("output_data/df_distances.rds")

# Mean pairwise distance
mean_dist_players <- distances_df %>% 
  group_by(shirtNumber1, shirtNumber2) %>% 
  summarize(meanDistance = mean(dist), sdDistance = sd(dist)) %>% 
  mutate(meanDistance=round(meanDistance, 2), sdDistance=round(sdDistance, 2)) %>% 
  ungroup()

# Mean individual distance 
temp1 <- distances_df %>% 
  dplyr::select(shirtNumber=shirtNumber1, dist)

temp2 <- distances_df %>% 
  dplyr::select(shirtNumber=shirtNumber2, dist)

player_dist <- rbind(temp1, temp2)

player_dist <- player_dist %>% 
  group_by(shirtNumber) %>% 
  summarize(meanDistance = mean(dist), sdDistance = sd(dist)) %>% 
  mutate(meanDistance=round(meanDistance, 2), sdDistance=round(sdDistance, 2)) %>% 
  mutate(shirtNumber=as.character(shirtNumber)) %>% 
  ungroup()


rm(temp1, temp2)

df_tracking <- readRDS("output_data/df_tracking.rds")

df_tracking_join <- df_tracking %>%  
  distinct(matchId, half, time, phase_id)

rm(df_tracking)

subset <- distances_df %>% 
  left_join(df_tracking_join, by=c("matchId", "half", "time")) %>% 
  distinct() 

subset_agg <- subset %>% 
  ungroup() %>%  
  mutate(shirtNumber1 = as.numeric(shirtNumber1), shirtNumber2 = as.numeric(shirtNumber2)) 

subset_agg <- subset_agg %>%    
  group_by(phase_id, shirtNumber1, shirtNumber2) %>% 
  summarize(meanDistCor = mean(dist)) %>%
  ungroup() 

subset <- subset_agg %>%  
  mutate(weight=rescale(-meanDistCor, to = c(1, 5))) %>% 
  ungroup()

rm(subset_agg) 

phaseIDs <- unique(subset$phase_id)
phase_id_list <- list()
mean_cent_pos_list <- list()
max_cent_pos_list <- list()
sd_cent_pos_list <- list()


centr_func <- function(phase_input) {
  
  phase_input = 15671
  
  #i = match(phase_input, phaseIDs)
  
  temp <- subset(subset, phase_id == phase_input) 
  
  players <- temp %>% 
    dplyr::select(shirtNumber1, shirtNumber2) %>% 
    t %>% c %>% unique %>% 
    data.frame() 
  
  relations <- subset(temp, select = c("shirtNumber1", "shirtNumber2", "weight")) 
  
  g <- graph_from_data_frame(relations, directed=FALSE, vertices=players)  
  #is_weighted(g) # Check if g is weighted 
  
  #phase_id_list[i] <<- phase_input
  
  mean_cent_pos_list <- mean(closeness(g))
  max_cent_pos_list <- max(closeness(g))
  sd_cent_pos_list <- sd(closeness(g))
  
  return(list(mean_cent_pos_list, max_cent_pos_list, sd_cent_pos_list, phase_input))
  
  gc()
}  

df_pos <- sapply(phaseIDs, centr_func)
#df_pos <- readRDS("output_data/df_pos.rds")

df_pos <- data.frame(mean_cent_pos = unlist(df_pos[1,]), max_cent_pos=unlist(df_pos[2,]), sd_cent_pos=unlist(df_pos[3,]), phase_id=unlist(df_pos[4,]))
#saveRDS(df_pos, "output_data/df_pos.rds")

#features <- readRDS("output_data/features_w_alignment.rds")

# MERGE WITH FEATURES 
features <- features %>%  
  left_join(df_pos, by=c("phase_id"))

#saveRDS(features, "output_data/features_w_alignment.rds")

#------- DIRECTION ---------#

rm(df_pos)

df_angle_phase_pair <- readRDS("output_data/df_angle_phase_pair.rds")

subset_1 <- df_angle_phase_pair %>% 
  group_by(phase_id, shirtNumber1, shirtNumber2) %>% 
  summarize(meanDirCor = mean(cor)) %>% 
  ungroup() %>% 
  mutate(weight=rescale(-meanDirCor, to = c(1, 5))) 

phaseIDs <- unique(subset_1$phase_id)
cent_mean_list  <- list()
cent_sd_list  <- list()
cent_max_list  <- list()
phase <- list()
phases_cen <- data.frame()

centr_func <- function(phaseID_input) {
  
  #i = match(phaseID_input, phaseIDs)
  
  subset <- subset(subset_1, phase_id==phaseID_input)
  
  players <- subset %>% 
    dplyr::select(shirtNumber1, shirtNumber2) %>% 
    t %>% c %>% unique %>% 
    data.frame() 
  
  relations <- subset(subset, select=c("shirtNumber1", "shirtNumber2", "weight")) 
  
  g <- graph_from_data_frame(relations, directed=FALSE, vertices=players)  
  
  cent_mean_list <- mean(closeness(g))
  cent_sd_list <- sd(closeness(g))
  cent_max_list <- max(closeness(g))
  
  return(list(cent_mean_list, cent_sd_list, cent_max_list, phaseID_input))
  
  gc()
  
  rm(players, subset, relations, g)
}

dir_phases_cen <- sapply(phaseIDs, centr_func)

dir_phases_cen <- data.frame(mean_cent_dir=unlist(dir_phases_cen[1,]), sd_cent_dir=unlist(dir_phases_cen[2,]), max_cent_dir=unlist(dir_phases_cen[3,]), phase_id = unlist(dir_phases_cen[4,]))

rm(cent_mean_list, cent_sd_list, cent_max_list, phase, subset_1, df_angle_phase_pair, dir_phases_cen)


# MERGING WITH FEATURES 
features <- features %>% 
  left_join(dir_phases_cen, by="phase_id")

#saveRDS(features, "output_data/features_w_alignment.rds")


#------- SPEED ---------#

#features <- readRDS("output_data/features_w_alignment.rds")
df_cor_phase_pair <- readRDS("output_data/df_cor_phase_pair.rds")

subset_1 <- df_cor_phase_pair %>% 
  group_by(phase_id, shirtNumber1, shirtNumber2) %>% 
  summarize(meanSpeedCor = mean(cor)) %>% 
  ungroup() %>% 
  mutate(weight=rescale(-meanSpeedCor, to = c(1, 5))) 

rm(df_cor_phase_pair)

phaseIDs <- unique(subset_1$phase_id)
mean_cent_values  <- list()
sd_cent_values  <- list()
max_cent_values  <- list()
phase <- list()
phases_cen <- data.frame()


centr_func <- function(phaseID_input) {
  
  #i = match(phaseID_input, phaseIDs)
  
  subset <- subset(subset_1, phase_id==phaseID_input)
  
  players <- subset %>% 
    dplyr::select(shirtNumber1, shirtNumber2) %>% 
    t %>% c %>% unique %>% 
    data.frame() 
  
  relations <- subset(subset, select=c("shirtNumber1", "shirtNumber2", "weight")) 
  
  g <- graph_from_data_frame(relations, directed=FALSE, vertices=players)  
  
  mean_cent_values <- mean(closeness(g))
  sd_cent_values <- sd(closeness(g))
  max_cent_values <- max(closeness(g))
  
  return(list(mean_cent_values, sd_cent_values, max_cent_values, phaseID_input))
  
}

speed_phases_cen <- sapply(phaseIDs, centr_func)

speed_phases_cen <- data.frame(mean_cent_speed=unlist(speed_phases_cen[1,]),
                               sd_cent_speed=unlist(speed_phases_cen[2,]), max_cent_speed=unlist(speed_phases_cen[3,]), phase_id=unlist(speed_phases_cen[4,]))

rm(mean_cent_values, sd_cent_values, max_cent_values, phase)

# MERGING WITH FEATURES

#saveRDS(speed_phases_cen, "output_data/speed_phases_cen.rds")
#speed_phases_cen <- readRDS("output_data/speed_phases_cen.rds")

features <- features %>% 
  left_join(speed_phases_cen, by="phase_id")

# Performance score -------------------------------------------------------

df_tracking <- readRDS("../df_tracking_0705.rds")

gc()
memory.limit(20000)

df_temp <- df_tracking %>%  
  filter(offense_defense=="offense" & contestantId=="9qsmopgutr7ut5g6workk8w4i" | offense_defense=="defense" & contestantId!="9qsmopgutr7ut5g6workk8w4i") %>% 
  distinct(phase_id, direction)

performance_dat <- df_tracking %>%  
  mutate(pen_area = ifelse(((x > 3622.5 | x < -3622.5) & y <1394 & y>-1394), 1, 0)) %>% 
  group_by(phase_id, time) %>%  
  mutate(count_pen = sum(pen_area)) %>% 
  ungroup() %>% 
  filter(count_pen<16  & zBall<400) 

performance_dat <- performance_dat %>% 
  filter(offense_defense=="offense" & contestantId=="9qsmopgutr7ut5g6workk8w4i" | offense_defense=="defense" & contestantId!="9qsmopgutr7ut5g6workk8w4i")

performance_dat <- performance_dat %>%  
  dplyr::select(-direction) %>%  
  left_join(df_temp, by="phase_id") 

performance_dat <- performance_dat %>%  
  mutate(xBall=xBall*direction, yBall=yBall*direction, x=x*direction, y=y*direction) 

performance_dat <- performance_dat %>%  
  group_by(matchId, half, phase_id, time, playerId, offense_defense) %>%  
  summarize(xBall = mean(xBall), yBall = mean(yBall), x = mean(x), y = mean(y)) %>%  
  ungroup()

# Remove data points where no person is wihtin 2 meters distance from the ball
performance_dat <- performance_dat  %>% 
  group_by(phase_id, time, playerId) %>%  
  mutate(dist_ball = raster::pointDistance(c(x, y), c(xBall, yBall), lonlat=F)) %>%  
  ungroup()

performance_dat <- performance_dat  %>% 
  mutate(dist_meters = dist_ball/100) 

performance_dat <- performance_dat  %>% 
  group_by(phase_id, time) %>%  
  mutate(min_dist = min(dist_meters)) %>%  
  ungroup()

# Filter so a players is always close to the ball (min 2 meters)
performance_dat <- performance_dat  %>% 
  filter(dist_meters <=2)
#-----


performance_dat <- performance_dat %>%  
  group_by(phase_id) %>%
  mutate(perf_score = ifelse(offense_defense=="offense", (max(xBall)*abs(max(xBall))), (min(xBall)*abs(min(xBall))))) %>%
  ungroup() 

performance_dat <- performance_dat %>%  
  group_by(offense_defense) %>% 
  mutate(perf_score = rescale(perf_score, to = c(1, 100))) %>%  
  distinct(phase_id, perf_score) %>%  
  ungroup() 

features <- readRDS("../output_data/features_final.rds")

features_all <- features %>% 
  rename(old_perf = perf_score) %>%  
  inner_join(performance_dat[,c("phase_id", "perf_score")], by=c("phase_id"="phase_id")) %>%  
  #mutate(perf_score = rescale(perf_score, to = c(1, 100))) %>% 
  #mutate(perf_score_log = log(perf_score)) %>% 
  distinct() 

#d <- density(features$perf_score_log) # returns the density data
#plot(d) # plots the results

features <- readRDS("../output_data/features_final.rds")
performance_dat <- readRDS("../output_data/performance_dat.rds") %>%  
  dplyr::select(-offense_defense)

features <- features %>%
  filter(phase_id!=33109 & phase_id!=49898)   # Da disse phase IDs ikke har samme antal spillere
  

features <- features %>%
  dplyr::select(-ball_start_x, -ball_end_x, -ball_start_y, -ball_end_y)   # Da disse phase IDs ikke har samme antal spillere

# Saving files ------------------------------------------------------------

is.num <- sapply(features, is.numeric)
features[is.num] <- lapply(features[is.num], round, 5)

features <- features %>% 
  distinct()

# save dataset 
saveRDS(performance_dat, "../output_data/performance_dat")
saveRDS(features, "../output_data/features_final_1105.rds")
save.image('workspace_feature_engineering_all_data.RData')

#features <- features[rowSums(!is.na(features)) > 0,]

