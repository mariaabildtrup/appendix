
library(pacman)
p_load(dplyr, ggplot2)
features <- readRDS("output_data/features_final.rds")
performance_dat <- readRDS("output_data/performance_dat")
#load('workspace_feature_engineering_all_data.RData')

# n sequences 
temp <- df_tracking %>%  
  group_by(matchId) %>%  
  summarize(n=length(unique(phase_id)))
  
mean(temp$n)
sd(temp$n)

# speed 
temp <- df_tracking %>% 
  filter(contestantId=="9qsmopgutr7ut5g6workk8w4i")  %>%  
  group_by(playerId) %>%  
  summarize(mean_speed = mean(speed), min_speed=min(speed), max_speed=max(speed)) 
  
mean(temp$mean_speed)
sd(temp$mean_speed)
max(temp$max_speed)
min(temp$min_speed)

# Density plot showing distribution of performance values: 

mean(performance_dat$perf_score)
sd(performance_dat$perf_score)

performance_dat_sum <- performance_dat %>%  
  mutate(offense_defense="total")

performance_dat <- rbind(performance_dat, performance_dat_sum)

p <- performance_dat %>%
  ggplot(aes(perf_score, y = ..count.., fill = offense_defense)) + 
  geom_density(alpha = 0.2) +
#  facet_grid(offense_defense ~ .) + 
  guides(fill=guide_legend(title="Defense / Offense")) +
  theme_minimal() + 
  theme(legend.title = element_blank()) +
  labs(y="Count", x = "Performance Score")

ggplotly(p)

p <- performance_dat %>%
  ggplot(aes(perf_score, y = ..count..)) 

p + geom_density(color="grey20", size=1, fill="lightgrey", alpha=.2) +
  labs(y="Count", x = "Performance Score") + 
  geom_density(aes(perf_score, y = ..count.., color=offense_defense), size=1, fill="grey", alpha=.2)+
  theme_minimal() + 
  labs(y="Count", x = "Performance Score", color="Defense / Offense") 






#----- LEADERSHIP ------#
#Movement
mean(features$mean_cent_speed, na.rm=T)
sd(features$mean_cent_speed, na.rm=T)
#min(features$mean_cent_speed, na.rm=T)
#max(features$mean_cent_speed, na.rm=T)

mean(features$max_cent_speed, na.rm=T)
sd(features$max_cent_speed, na.rm=T)

mean(features$sd_cent_speed, na.rm=T)
sd(features$sd_cent_speed, na.rm=T)

#Direction
mean(features$mean_cent_dir, na.rm=T)
sd(features$mean_cent_dir, na.rm=T)
#min(features$mean_cent_dir, na.rm=T)
#max(features$mean_cent_dir, na.rm=T)

mean(features$max_cent_dir, na.rm=T)
sd(features$max_cent_dir, na.rm=T)

mean(features$sd_cent_dir, na.rm=T)
sd(features$sd_cent_dir, na.rm=T)


#Position
mean(features$mean_cent_pos, na.rm=T)
sd(features$mean_cent_pos, na.rm=T)
#min(features$mean_cent_pos, na.rm=T)
#max(features$mean_cent_pos, na.rm=T)

mean(features$max_cent_pos, na.rm=T)
sd(features$max_cent_pos, na.rm=T)

mean(features$sd_cent_pos, na.rm=T)
sd(features$sd_cent_pos, na.rm=T)


#Passes
mean(features$mean_cent_pass, na.rm=T)
sd(features$mean_cent_pass, na.rm=T)
#min(features$mean_cent_pass, na.rm=T)
#max(features$mean_cent_pass, na.rm=T)

mean(features$max_cent_pass, na.rm=T)
sd(features$max_cent_pass, na.rm=T)

mean(features$sd_cent_pass, na.rm=T)
sd(features$sd_cent_pass, na.rm=T)



#-----------ALIGNEMNT--------#

# DIRECTION
mean(features$mean_alignment_dir)
sd(features$mean_alignment_dir)
#max(features$mean_alignment_dir)
#min(features$mean_alignment_dir)

mean(features$sd_alignment_dir)
sd(features$sd_alignment_dir)

mean(features$max_alignment_dir)
sd(features$max_alignment_dir)


# DIRECTION ABS
mean(features$mean_abs_alignment_dir)
sd(features$mean_abs_alignment_dir)
#max(features$mean_abs_alignment_dir)
#min(features$mean_abs_alignment_dir)

mean(features$sd_abs_alignment_dir)
sd(features$sd_abs_alignment_dir)

mean(features$max_abs_alignment_dir)
sd(features$max_abs_alignment_dir)



# SPEED
mean(features$mean_alignment_speed)
sd(features$mean_alignment_speed, na.rm=T)
#max(features$mean_alignment_speed)
#min(features$mean_alignment_speed, na.rm=T)

mean(features$max_alignment_speed)
sd(features$max_alignment_speed, na.rm=T)

mean(features$sd_alignment_speed)
sd(features$sd_alignment_speed, na.rm=T)


#-----COMPACTNESS-----#

mean(features$dist_x)
sd(features$dist_x)

mean(features$dist_y)
sd(features$dist_y)

mean(features$chull_area)
sd(features$chull_area)

# Defend vs. attack 
# X
mean(features[which(features$offense_defense=="offense"),]$dist_x)
sd(features[which(features$offense_defense=="offense"),]$dist_x)

mean(features[which(features$offense_defense=="defense"),]$dist_x)
sd(features[which(features$offense_defense=="defense"),]$dist_x)

 
mean(features[which(features$offense_defense=="offense"),]$dist_y)
sd(features[which(features$offense_defense=="offense"),]$dist_y)

mean(features[which(features$offense_defense=="defense"),]$dist_y)
sd(features[which(features$offense_defense=="defense"),]$dist_y)


# CH
mean(features[which(features$offense_defense=="offense"),]$chull_area)
sd(features[which(features$offense_defense=="offense"),]$chull_area)

mean(features[which(features$offense_defense=="defense"),]$chull_area)
sd(features[which(features$offense_defense=="defense"),]$chull_area)


#-----BASIC-----#

mean(features$maxSpeed)
sd(features$maxSpeed)

mean(features$meanSpeed)
sd(features$meanSpeed)

mean(features$meanSpeedBall)
sd(features$meanSpeedBall)

mean(features$maxSpeedBall)
sd(features$maxSpeedBall)

mean(features$passes_min_mean)
sd(features$passes_min_mean)

mean(features$passes_min_sd)
sd(features$passes_min_sd)

mean(features$mean_passes_player)
sd(features$mean_passes_player)

mean(features$sd_passes_player)
sd(features$sd_passes_player)

mean(features$n_passes_total)
sd(features$n_passes_total)


#-----PERFORMANCE-----#

mean(features$perf_score)
sd(features$perf_score)


