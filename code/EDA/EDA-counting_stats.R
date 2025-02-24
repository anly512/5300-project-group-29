library(tidyverse)
library(nnet)
library(hoopR)
library(janitor)

# Load tournament results data
results <- read_csv("../../data/Big_Dance_CSV.csv") %>%
  filter(Year > 2006) %>%
  clean_names()

colnames(results) <- c("season", "round", "region_number", "region_name",
                       "seed_t1", "score_t1", "name_t1",
                       "name_t2", "score_t2", "seed_t2")
tournament_teamnames <- c(results$name_t1, results$name_t2) %>%
  unique() %>%
  sort()

# Load team stats data and subset to only teams who played in the tournament
team_stats <- load_mbb_team_box(seasons = 2007:2024) %>%
  filter(team_short_display_name %in% tournament_teamnames)

regular_teamnames <- unique(team_stats$team_short_display_name) %>%
  unique() %>%
  sort()


# Find summary stats for regular season by team
team_summaries <- team_stats %>%
  summarize(avg_score = mean(team_score),
            avg_assists = mean(assists),
            avg_blocks = mean(blocks),
            avg_dreb = mean(defensive_rebounds),
            avg_fgp = mean(field_goal_pct),
            avg_oreb = mean(offensive_rebounds),
            avg_steals = mean(steals),
            avg_fta = mean(free_throws_attempted),
            avg_3fgp = mean(three_point_field_goal_pct),
            avg_to = mean(turnovers),
            avg_opp_score = mean(opponent_team_score),
            .by = c(team_short_display_name, season)) %>%
  rename(team_name = team_short_display_name)

# Find summary stats for all tournament teams by season
season_summaries <- team_summaries %>%
  summarize(season_avg_score = mean(avg_score),
            season_avg_assists = mean(avg_assists),
            season_avg_blocks = mean(avg_blocks),
            season_avg_dreb = mean(avg_dreb),
            season_avg_fgp = mean(avg_fgp),
            season_avg_oreb = mean(avg_oreb),
            season_avg_steals = mean(avg_steals),
            season_avg_fta = mean(avg_fta),
            season_avg_3fgp = mean(avg_3fgp),
            season_avg_to = mean(avg_to),
            season_avg_opp_score = mean(avg_opp_score),
            .by = c(season))

# Filter tournament results to only teams we have regular season data on
results <- results %>%
  filter(results$name_t1 %in% regular_teamnames & results$name_t2 %in% regular_teamnames) %>%
  
  # Add column indicating the winner of the game and whether the matchup was an upset
  
  mutate(victor = ifelse(score_t1 > score_t2, name_t1, name_t2)) %>%
  mutate(loser = ifelse(score_t1 < score_t2, name_t1, name_t2)) %>%
  mutate(upset = 
           ifelse(victor == name_t1 & seed_t1 > seed_t2 | victor == name_t2 & seed_t2 > seed_t1, 1, 0))

# Create csv with summary stats of individual teams and seasons
# Merge with summary stats to compare
team_summaries_merge <- results %>%
  mutate(victor_seed = 
           ifelse(victor == name_t1, seed_t1, seed_t2)) %>%
  mutate(loser_seed =
           ifelse(victor == name_t1, seed_t2, seed_t1))

winner_stats <- team_summaries %>%
  rename_with(~ paste0("victor_", .x)) %>%
  rename(victor = victor_team_name,
         season = victor_season) %>%
  right_join(team_summaries_merge,
             by = c("season", "victor"))

loser_stats <- team_summaries %>%
  rename_with(~ paste0("loser_", .x)) %>%
  rename(loser = loser_team_name,
         season = loser_season) %>%
  right_join(team_summaries_merge,
             by = c("season", "loser"))

tourney_all_stats <- winner_stats %>%
  full_join(loser_stats,
            by = c(colnames(results), "victor_seed", "loser_seed")) %>%
  left_join(season_summaries,
            by = "season")
tourney_all_stats %>% write_csv("../../data/regular_season_stats_of_all_tournament_team_matchups_2007_2019.csv")
rm(winner_stats, loser_stats, team_summaries_merge)

### UPSET ANALYSIS ###

# Subset to results with upsets
upsets <- results %>%
  filter(upset == 1) %>%
  mutate(victor_seed = 
           ifelse(victor == name_t1 & seed_t1 > seed_t2, seed_t1, seed_t2)) %>%
  mutate(loser_seed =
           ifelse(victor == name_t1 & seed_t1 > seed_t2, seed_t2, seed_t1))

# Merge with summary stats to compare
upset_winner_stats <- team_summaries %>%
  rename_with(~ paste0("victor_", .x)) %>%
  rename(victor = victor_team_name,
         season = victor_season) %>%
  right_join(upsets,
             by = c("season", "victor"))

upset_loser_stats <- team_summaries %>%
  rename_with(~ paste0("loser_", .x)) %>%
  rename(loser = loser_team_name,
         season = loser_season) %>%
  right_join(upsets,
             by = c("season", "loser"))

tourney_stats <- upset_winner_stats %>%
  full_join(upset_loser_stats,
            by = c(colnames(results), "victor_seed", "loser_seed")) %>%
  left_join(season_summaries,
            by = "season")

#tourney_stats %>% write_csv("./data/regular_season_stats_of_upset_teams_2007_2019.csv")
rm(upset_winner_stats, upset_loser_stats, upsets)

# Plot average regular season points of winners and losers of upsets by season
tourney_stats %>%
  drop_na() %>%
  summarize(victor = mean(victor_avg_score),
            loser = mean(loser_avg_score),
            .by = "season") %>%
  pivot_longer(c(victor, loser),
               names_to = "status",
               values_to = "avg_score") %>%
  ggplot(aes(x = season, y = avg_score, color = status)) +
  geom_line() +
  labs(x = "Season", 
       y = "Average Regular Season Point Total",
       title = "Average Regular Season Point Totals \nby Upset Victors and Losers (2007-2019)",
       color = element_blank()) +
  scale_x_continuous(breaks = 2007:2019) +
  theme_bw()
ggsave("../../plots/EDA/upset_score_by_season.png",
       height = 4,
       width = 7)

# Plot average regular season points of winners and losers of upsets by seed
tourney_stats %>%
  drop_na() %>%
  summarize(victor = mean(victor_avg_score),
            num_upsets = n(),
            .by = c("victor_seed")) %>%
  pivot_longer(c(victor),
               names_to = "status",
               values_to = "avg_score") %>%
  mutate(victor_seed = factor(victor_seed)) %>%
  arrange(victor_seed) %>%
  ggplot(aes(x = victor_seed, y = avg_score, size = num_upsets)) +
  geom_point() +
  labs(x = "Victor Seed", 
       y = "Average Regular Season Point Total",
       title = "Average Regular Season Point Totals by Upset Victors (2007-2019)",
       size = "Number of Upsets") +
  scale_y_continuous(expand = c(0,1)) +
  theme_bw()
ggsave("../../plots/EDA/upset_score_by_seed.png",
       height = 4,
       width = 4)

# Plot ten most surprising upsets by regular season scoring
tourney_stats %>%
  mutate(avg_score_diff = loser_avg_score - victor_avg_score) %>%
  select(season, victor, loser, avg_score_diff, victor_seed, loser_seed) %>%
  arrange(-avg_score_diff) %>%
  slice_head(n = 10) %>%
  mutate(plot_string = 
           str_glue("{season} \n{victor} ({victor_seed})\n over \n{loser} ({loser_seed})")) %>%
  ggplot(aes(x = reorder(plot_string, -avg_score_diff), y = avg_score_diff, fill = avg_score_diff)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Matchup",
       y = "Average Regular Season Score Differential \n(losing team average - winning team average)",
       title = paste0("Top 10 Most Surprising Upsets by Scoring (2007-2019)"),
       subtitle = paste0("Losing Team versus Winning Team Regular Season Average Points Scored")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        legend.position = "none")
ggsave("../../plots/EDA/top10_surprising_upsets.png",
       height = 8,
       width = 12)
  
  
  
  
  
  
  
  
