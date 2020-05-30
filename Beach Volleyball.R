library(tidyverse)
library(skimr)
library(viridis)
library(ggeffects)
library(patchwork)


vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

#create a match ID 
vb_matches <- vb_matches %>%
  mutate(Match_ID = row_number()) %>%
  select(Match_ID, everything())

#look at attack
attacks <- vb_matches %>%
  select(
    Match_ID,
    year, 
    gender,
    country,
    duration,
    w_p1_tot_attacks, 
    w_p2_tot_attacks, 
    l_p1_tot_attacks,
    l_p2_tot_attacks,
  ) %>%
  pivot_longer(cols = c(starts_with("w_"), starts_with("l_")), 
               values_to = "Num_of_Attacks") %>%
  separate(name, c("winner_loser", "player", "name"),
           sep = "_",
           extra = "merge",
           fill = "right") %>% 
  replace_na(replace = list(Num_of_Attacks = 0))

#conduct some exploratory analysis
summary(attacks$Value)
attacks %>%
  ggplot(aes(gender, Num_of_Attacks, fill = winner_loser, colour = winner_loser)) +
  geom_boxplot() +
  ggtitle("Boxplot of Attack Moves in Beach Volleyball By Gender and Winners/Losers") +
  ylab("Number of Attacks") +
  xlab("Gender") +
  theme_bw()

#remove negative value (-6) and high attack value (330). Cannot have negative attacks and 330 attacks in 36 minutes is unrealistic
attacks <- attacks[ !(attacks$Match_ID %in% c(22212, 61151)), ]

#adjust attacks dataframe for analysis
#check probability of attacks by male and females
attacks <- attacks %>%
  mutate(attack = if_else(Num_of_Attacks > 0, 1, 0))

#check class bias
table(attacks$attack) #there is a bias

#create logistic regression model

attack_model <- glm(data = attacks, attack ~ gender*winner_loser, family = "binomial")
attack_model

plot_attack_model <- ggpredict(attack_model, terms = c("winner_loser", "gender")) %>%
  plot() +
  labs(x = "Winner or Loser",
       y = "Probability of Making an Attack Move") +
  ggtitle("Probability of Attack Moves") 
  

#digs analysis

digs <- vb_matches %>%
  select(
    Match_ID,
    year, 
    gender,
    country,
    duration,
    w_p1_tot_digs, 
    w_p2_tot_digs, 
    l_p1_tot_digs,
    l_p2_tot_digs,
  ) %>%
  pivot_longer(cols = c(starts_with("w_"), starts_with("l_")), 
               values_to = "Num_of_Digs") %>%
  separate(name, c("winner_loser", "player", "name"),
           sep = "_",
           extra = "merge",
           fill = "right") %>% 
  replace_na(replace = list(Num_of_Digs = 0))

summary(digs)
digs %>%
  ggplot(aes(gender, Num_of_Digs, fill = winner_loser, colour = winner_loser)) +
  geom_boxplot() +
  ggtitle("Boxplot of Attack Moves in Beach Volleyball By Gender and Winners/Losers") +
  ylab("Number of Digs") +
  xlab("Gender") +
  theme_bw()

digs <- digs %>%
  mutate(dig = if_else(Num_of_Digs > 0, 1, 0))

digs_model <- glm(data = digs, dig ~ gender*winner_loser, family = "binomial")
digs_model

plot_digs_model <- ggpredict(digs_model, terms = c("winner_loser", "gender")) %>%
  plot() +
  labs(x = "Winner or Loser",
       y = "Probability of Making a Dig Move") +
  ggtitle("Probability of Dig Moves")

total_plots <- plot_attack_model + plot_digs_model +
  plot_annotation(title = "Women Have a Greater Probability of Making an Attack Move or Dig Move",
                  subtitle = "Might Women Have a More Aggressive Play Style")

png(filename = "Beach Volleyball Attacks and Digs.png")





          


