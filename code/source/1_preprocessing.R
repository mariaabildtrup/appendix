
# SETTING UP ---------------------------------

library(pacman) 
p_load(dplyr, lubridate, tidytext, ggraph, tidygraph, visNetwork, tidyr, tidyverse, kableExtra, igraph, scales, raster)

# Importing data

# Event data 
df_event <- readRDS('df_event.rds') 

df_tracking <- readRDS('df_tracking.rds') %>% 
  dplyr::select(-frameId, -info, -ownGoalLine, -season)

# Event data 

df_event <- df_event %>% 
  mutate(hms = seconds_to_period(min*60+sec))%>%   
  arrange(hms)

gc()
memory.limit(size=20000) 

# Tracking data 

df_tracking <- df_tracking %>% 
  mutate(hms = seconds_to_period(time/25)) %>% 
  mutate(min = minute(hms)) %>%
  mutate(sec = second(hms)) %>% 
  arrange(matchId, half, time)

# Phase ID variable ----------------------------------------------------------

df_tracking <- df_tracking %>% 
  mutate(turnover = paste0(matchId, "_", ballInPlay, "_", ballOwnership, "_", half)) 

df_tracking <- df_tracking %>% 
  mutate(turnover = paste0(matchId, "_", ballInPlay, "_", ballOwnership, "_", half)) 

start_value <- unique(df_tracking$turnover)[1]  

turnovers <- df_tracking$turnover
phases <- list()
phases[1] <- 0

gc()

#start.time <- Sys.time() # Check how much time the code will take to run (try on subset)
for (i in 2:length(turnovers)) {
  phase <- ifelse(turnovers[i]!=turnovers[i-1], (max(unlist(phases))+1), phases[i-1])
  phases[i]=phase
}
#end.time <- Sys.time()
#time.taken <- round(end.time - start.time,2)
#time.taken

df_tracking <- cbind(df_tracking, data.frame(unlist(phases))) %>% 
  rename(phase_id = unlist.phases.) %>% 
  dplyr::select(-turnover) %>% 
  ungroup()

# Remove data when ball is dead
df_tracking <- df_tracking %>% 
  filter(ballInPlay=="Alive")

# Ball on pitch
df_tracking <- df_tracking %>%  
  filter(xBall>=-5250 & xBall<=5250) %>% 
  filter(yBall>=-3400  & yBall<=3400) 

# Create temporary dataset for determining what phases that should be removed (phases with less than two seconds of duration are removed)
temp <- df_tracking %>% 
  group_by(phase_id) %>% 
  summarize(n=n()) %>% 
  mutate(phase_id_upd = ifelse(n<=44, NA, phase_id)) %>% 
  dplyr::select(-n) %>% 
  ungroup()

df_tracking <- df_tracking %>% 
  left_join(temp, by="phase_id") %>% 
  dplyr::select(-phase_id) %>% 
  rename(phase_id=phase_id_upd) %>% 
  filter(!is.na(phase_id)) 

rm(phase, phases)

# Creating dataset with distances -----------------------------------------

gc()

df_temp <- df_tracking %>% 
  ungroup() %>% 
  filter(gk==0) %>%  # Remove goal keeper 
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i") %>%  # Keep only data on BIF players
  mutate(coordinates = paste0(x, ",", y)) %>% 
  mutate(half_match = paste0(half, "_", matchId)) %>% # create variable to loop through (matchId creates memory error)
  dplyr::select(half_match, time, half, shirtNumber, coordinates, matchId)

half_matches = unique(df_temp$half_match) # values to loop through
distances_df = data.frame() # Create empty data frame 

# FUNCTION #
tidybayes_function <- function(time_input) {
  
  subset <- filter(df_temp_match, time==time_input)
  
  temp <- tidybayes::gather_pairs(
    subset,
    shirtNumber,
    coordinates,
    row = "shirtNumber1",
    col = "shirtNumber2",
    x = "coord_1",
    y = "coord_2",
    triangle = c("lower only", "upper only", "lower", "upper", "both only", "both")) %>% 
    mutate(time = time_input)
  
  #df <<- rbind(df, temp)
  
  shirtNumber1 <- temp$shirtNumber1
  shirtNumber2 <- temp$shirtNumber2
  coord_1 <- temp$coord_1
  coord_2 <- temp$coord_2
  time <- temp$time

  return(list(shirtNumber1, shirtNumber2, coord_1, coord_2, time))
  
  rm(temp)
  gc() # Clear unused memory to avoid memory error
  
} 

#for (i in 1:length(half_matches)){
for (i in 1:2){
    
  #df <- data.frame() # Create empty data frame 
  
  df_temp_match = df_temp %>% # Create subset from half_matches values
    filter(half_match==half_matches[i])
  
  times = unique(df_temp_match$time) # values to run through in sapply function
  
  df <- sapply(times, df_function) # Run function that finds each unique pair per time 
  df <- data.frame(shirtNumber1 = unlist(df[1,]), shirtNumber1=unlist(df[2,]), coord_1=unlist(df[3,]), coord_2=unlist(df[4,]), time=unlist(df[5,]))
  
  df$half_match = half_matches[i] # Create half_match column from i value 
  
  distances_df <- rbind(distances_df, df) 
  
  rm(df, df_temp_match)
  
  gc() # Clear unused memory to avoid memory error

}

#for (i in 1:length(half_matches)){
half_match_function <- function(half_match_input) {
  
  #half_match_input = "1_3vpwfv3ylj57e3kdi2ao54416" # test
  
  df_temp_match <<- filter(df_temp, df_temp$half_match==half_match_input)
  
  times = unique(df_temp_match$time) # values to run through in sapply function
  
  df_list <- sapply(times, tidybayes_function) # Run function that finds each unique pair per time 
  #df <- data.frame(shirtNumber1 = unlist(df_list[1,]), shirtNumber2=unlist(df_list[2,]), coord_1=unlist(df_list[3,]), coord_2=unlist(df_list[4,]), time=unlist(df_list[5,]))
  
#  distances_df <- rbind(distances_df, df) 
  
  rm(df_temp_match)
  
  gc() # Clear unused memory to avoid memory error
  
  shirtNumber1 <- unlist(df_list[1,])
  shirtNumber2 <- unlist(df_list[2,])
  coord_1 <- unlist(df_list[4,])
  coord_2 <- unlist(df_list[3,])
  time <- unlist(df_list[5,])
  half_match <- half_match_input

  return(list(shirtNumber1, shirtNumber2, coord_1, coord_2, time, half_match_input))
  
}

df_distance_list <- sapply(half_matches, half_match_function)
distances_df <- data.frame(shirtNumber1=unlist(df_distance_list[1,]), shirtNumber2=unlist(df_distance_list[2,]), coord_1=unlist(df_distance_list[3,]), coord_2=unlist(df_distance_list[4,]), time=unlist(df_distance_list[5,]), half_match=unlist(df_distance_list[6,]))



distances_df <- distances_df %>%
  separate(coord_1, c("x1", "y1"), ",") %>% 
  separate(coord_2, c("x2", "y2"), ",") %>% 
  mutate(x1=as.integer(x1), y1=as.integer(y1), x2=as.integer(x2), y2=as.integer(y2)) %>% 
  distinct()

distances_df <- distances_df %>% 
  group_by(matchId, time, half, shirtNumber1, shirtNumber2) %>% 
  summarize(dist = raster::pointDistance(c(x1, y1), c(x2, y2), lonlat=F)) %>% 
  ungroup()

# Add 'direction' column --------------------------------------------------

# Pythaguras
distance <- function(from, to){
  D <- sqrt((abs(from[,1]-to[,1])^2) + (abs(from[,2]-to[,2])^2))
  return(D)
}

# Angle calculated by sentence calculating angle between vectors
angle <- function(from,to){
  dot.prods <- from$x*to$x + from$y*to$y
  norms.x <- distance(from = `[<-`(from,,,0), to = from)
  norms.y <- distance(from = `[<-`(to,,,0), to = to)
  thetas <- acos(dot.prods / (norms.x * norms.y))
  as.numeric(thetas)
}

# Calculating each player's angle#
temp <- df_tracking %>% 
  #  filter(team_name=="Br?ndby") %>% 
  mutate(player_match_half = paste0(playerId, "_", matchId, "_", half)) %>% 
  dplyr::select(player_match_half, matchId, half, x, y, time, playerId) 

gc()

angles_df <- data.frame()

i_values = unique(temp$player_match_half)

for (i in 1:length(i_values)) {
  
  player_sub <- temp %>% 
    filter(player_match_half==i_values[i]) 
  
  angles <- list()
  time <- list()
  
  times <- unique(player_sub$time)
  
  for (t in 1:length(times)){
    
    From <- data.frame(player_sub$x[t], player_sub$y[t]) 
    names(From)[1] = "x"
    names(From)[2] = "y"
    
    To <- data.frame(player_sub$x[t+1], player_sub$y[t+1])
    names(To)[1] = "x"
    names(To)[2] = "y"
    
    angles[t] = angle(from=From,to=To)
    time[t] = times[t]
    
    gc()
    
  } 
  
  angles <- data.frame(dir = unlist(angles), time=unlist(time), player_match_half=i_values[i])
  angles_df <- rbind(angles_df, angles)
}

# separate player_match_half variable 
angles_df <- angles_df %>%  
  separate(player_match_half, c("playerId", "matchId", "half")) %>% 
  mutate(half = as.numeric(half))

# Merge with tracking data 
df_tracking <- df_tracking %>% 
  left_join(angles_df, by=c("playerId", "matchId", "half", "time"))

rm(angles, player_sub, time, To, From, temp, angles_df)

gc()



# Save workspace ----------------------------------------------------------
save.image('1_preprocessing.RData')

