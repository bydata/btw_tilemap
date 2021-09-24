###################################################
#' Run eda.R first
###################################################


#' Calculate t-Test from summary statistics
#' *1 = group 1, *2 = group 2
t_test_from_summary <- function(m1, m2, sd1, sd2, n1, n2, ...) {
  group1 <- scale(1:n1)*sd1 + m1
  group2 <- scale(1:n2)*sd2 + m2
  t.test(group1, group2, ...)
}


# Apply t-test to vote share difference between constituency winner and runner-up
id_cols <- c("wkr", "Wahlkreisname")
bvorh_sig <- bvorh_cleaned %>% 
  group_by(wkr) %>% 
  slice_max(order_by = Stimmenanteil_Mean, n = 2, with_ties = FALSE) %>% 
  mutate(rank = rank(-Stimmenanteil_Mean, ties.method = "first")) %>% 
  ungroup() %>% 
  select(wkr, Wahlkreisname, party, partygroup, rank, Stimmenanteil_Mean, Stimmenanteil_SD, obs) %>% 
  pivot_wider(id_cols = id_cols, 
              names_from = "rank", 
              values_from = c("Stimmenanteil_Mean", "Stimmenanteil_SD", 
                              "party", "partygroup", "obs")) %>% 
  mutate(t_test = pmap(list(m1 = Stimmenanteil_Mean_1, m2 = Stimmenanteil_Mean_2,
                      sd1 = Stimmenanteil_SD_1, sd2 = Stimmenanteil_SD_2,
                      n1 = obs_1, n2 = obs_2, alternative = "greater"), 
                      t_test_from_summary),
         t = map_dbl(t_test, "statistic"),
         p_value = map_dbl(t_test, "p.value"))


# Determine winner in constituency (with a threshold for undecided races)
threshold_percentage <- 1
wk_winner <- bvorh_sig %>% 
  mutate(winner = ifelse(Stimmenanteil_Mean_1 - Stimmenanteil_Mean_2 >= threshold_percentage, 
                         party_1, "undecided"),
         id = as.character(wkr)) %>% 
  select(id, Wahlkreisname, winner, starts_with("Stimmenanteil_"), t, p_value)


bvorh_sig %>% 
  filter(p_value < 0.1)

bvorh_sig %>% 
  filter(t > 1)

wk_topo %>% 
  inner_join(wk2bl, by = c("id" = "wknr")) %>% 
  inner_join(wk_winner, by = "id") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = winner, alpha = -p_value),
          size = 0.1, col = "grey80", 
          # show.legend = FALSE
          ) +
  geom_sf_text(aes(label = id),
               size = 1.75) +
  geom_sf(data = bland_shape,
          aes(geometry = geometry_bland, 
              group = bundesland),
          fill = NA, col = "grey20",
          size = 0.5, show.legend = FALSE) +
  guides(fill = "none", alpha = "none") + 
  scale_fill_manual(values = c(party_colors, c("undecided" = "grey85"))) +
  scale_alpha(range = c(0.1, 0.6)) +
  coord_sf() +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = "white"),
        legend.position = "top",
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.key.height = unit(1, "mm")
        )
ggsave(here("plots", "tilegrid_wk_alpha.png"), width = 1200, height = 1200, units = "px")


