
# init
rm(list = ls())
pacman::p_load(tidyverse, hrbrthemes)

my_oka <- c("#920700", 
            "#EEBD64", 
            "#96B9D0", 
            "#345832")

# ASR counts
asr <-
  read_csv("./data/counts_asr.csv") |>
  summarize(full = n(), .by = "year") |>
  arrange(year)

# DV counts
key <- bind_rows(
  # inequality
  read_csv("./data/dv01_ineq.csv") |>
    summarize(n = n(), .by = "year") |>
    mutate(keyword = "ineq"),
  # solidarity
  read_csv("./data/dv02_sold.csv") |>
    summarize(n = n(), .by = "year") |>
    mutate(keyword = "sold"),
  # capitalism
  read_csv("./data/dv03_caps.csv") |>
    summarize(n = n(), .by = "year") |>
    mutate(keyword = "caps"),
  # secularization
  read_csv("./data/dv04_secu.csv") |>
    summarize(n = n(), .by = "year") |>
    mutate(keyword = "secu")) |> 
  arrange(year) |>
  pivot_wider(names_from = "keyword", 
              values_from = "n") |> 
  mutate_all(~replace(., is.na(.), 0))

# merge
d <- 
  left_join(asr, key, by = "year") |> 
  mutate(ineq = ineq / full,
         sold = sold / full,
         caps = caps / full,
         secu = secu / full) |> select(-full) |>  
  pivot_longer(cols = -c(year),
               names_to = "keyword",
               values_to = "per") |> 
  #### cosmetics
  mutate(keyword = factor(keyword,
                          levels = c("ineq",
                                     "sold",
                                     "caps",
                                     "secu"),
                          labels = c("Inequality",
                                     "Solidarity",
                                     "Capitalism",
                                     "Secularization")))

# plot
png(
  "./output/sociology_DVs.png",
  w = 8,
  h = 6,
  units = "in",
  res = 500
)
d |>
  ggplot(aes(x = year,
             y = per,
             col = keyword)) +
  geom_point(alpha = 0.5, size = 1) + 
  geom_smooth(se = FALSE, 
              linewidth = 0.75) +
  theme_ipsum_rc(grid = "XY") + theme(legend.position = "top") +
  scale_color_manual(values = my_oka) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 9)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  labs(
    title = "Inequality Stands Out in Sociology",
    subtitle = "The % of Articles Using Selected Keywords in the American Sociological Review, 1936-2019",
    caption = "Data compiled from JSTOR, using Constellate.\n 
               Each point represents the percent of keyword usage in a specific year.",
    x = "Year",
    y = "% Usage of the Keyword",
    col = "")
dev.off()
