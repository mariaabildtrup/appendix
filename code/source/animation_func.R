
p_load(ggplot2, gganimate)

source('source/addPitchCH.R')

temp <- df_tracking %>% 
  filter(phase_id==phase_id_input) %>%   
  # filter(time==11425) %>% 
  dplyr::select(PlayerID, x=xBall, y=yBall, time, contestantId) %>% 
  mutate(ID="Ball") %>% 
  mutate(ID=as.factor(ID))

data_plot <- df_tracking %>% 
  filter(phase_id==phase_id_input) %>%   
  # filter(time==11425) %>% 
  dplyr::select(PlayerID, x, y, time, contestantId) %>% 
  mutate(ID=as.factor(PlayerID))

data_plot <- rbind(data_plot, temp) %>% 
  mutate(ball = ifelse(ID=="Ball", "Ball", 
                       ifelse(contestantId=="9qsmopgutr7ut5g6workk8w4i", "BIF player", "Opponent"))) %>% 
  mutate(ball=as.factor(ball))

p <- ggplot(
  data_plot, 
  aes(x = x, y=y, group = ID, label = ID, color=ball)
) +
  addPitchCH()+ 
  geom_point(show.legend = FALSE, alpha = 0.7, size=8) +
  scale_colour_manual(values = c("grey30", "#bb9a0e", "coral4")) +
  geom_text(color="black") + 
  labs(x = "x coordinate", y = "y coordinate") +
  # theme(legend.title = element_blank()) +
  transition_time(time) +
  labs(title = "Time: {frame_time}") 

animated_p <- animate(p)


