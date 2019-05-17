library(tidyverse)
library(ggthemes)
library(ggrepel)
library(scales)
library(RColorBrewer)

# Read in data

nobel_winners <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")


# Clean birth countries
nobel_winners <- nobel_winners %>% 
        mutate(birth_con = str_extract(birth_country, "\\(([^()]*)\\)")) %>%
        mutate(birth_country = 
                  if_else(
                        is.na(birth_con),
                        birth_country,
                        gsub("[()]", "", birth_con)
                        )) %>%
        select(-birth_con) %>%
        mutate(birth_country = 
                  if_else(
                        birth_country == "United States of America",
                        "USA",
                        if_else(
                        birth_country == "United Kingdom",
                        "UK",
                        birth_country
                        )
                ))



nobel_winners$birth_country <- as_factor(nobel_winners$birth_country)

## Create plot for ranks of total number of wins per country accumulated on decade basis, 


# Colors
pal1 <- c("#c57c3c", "#e392c2", "#a5e7a8", "#bea3ea", "#d7e298", "#81a4e3", "#a6b16a", "#a7baf2", 
         "#e4c587", "#5ab6e6", "#d6a16d", "#62d9f3", "#eb9189", "#3ec1c8", "#e1a6b6", "#7fe3c5", 
         "#e5b4e2", "#8bba83", "#cd5136", "#84bb9c", "#e1ceeb", "#72b7b0", "#cd9e8c", "#93e7e2", 
         "#ecc0b1", "#7bb1c6", "#d8e8c5", "#acbadd", "#b2b593", "#acd8eb")

nw_rank_decade <- nobel_winners %>%
        mutate(birth_country = fct_lump(birth_country, 11)) %>%
        # mutate(decade = glue::glue("{round(prize_year - 5, -1)}s")) %>%
        mutate(decade = round(prize_year - 5, -1)) %>%
        group_by(birth_country, decade) %>%
        summarize(number_of_wins = n()) %>%
        ungroup() %>% 
        complete(birth_country, decade, fill = list(number_of_wins = 0)) %>%
        filter(birth_country != "Other") %>%
        group_by(birth_country) %>%
        mutate(cumulative_wins = cumsum(number_of_wins)) %>%
        ungroup() %>%
        arrange(decade, desc(cumulative_wins)) %>%
        group_by(decade) %>%
        mutate(rank = row_number())

first_decade <- nw_rank_decade %>%
        filter(decade == 1900)

last_decade <- nw_rank_decade %>%
        filter(decade == 2010)
        




ggplot(data = nw_rank_decade, aes(x = decade, y = rank, group = birth_country)) + 
        geom_line(aes(colour = birth_country), size = 1.5) + 
        geom_point(shape = 21, stroke = 2, size=5, fill = "white", aes(colour = birth_country)) + 
        geom_label_repel(data = first_decade, aes(label = birth_country), size=3, 
                         fontface = "bold", color='#2f2f2f', nudge_x = -5) +
        geom_label_repel(data = last_decade, aes(label = birth_country), size = 3, 
                   fontface = "bold", color = '#2f2f2f', nudge_x = 5) +
        scale_y_reverse(lim = c(11,1), breaks = scales::pretty_breaks(n = 11)) +
        scale_x_continuous(expand = c(.09, .09), breaks = scales::pretty_breaks(n = 11)) +
        labs(x = NULL, y = NULL,
             title = 'Countries Ranked Based On Total Amount of Nobel Prize Wins',
             subtitle = "USA started at 9th but has now been 1st since 1950s, while Netherlands started 4th but are now at 10th.",
             caption = "Visualization by @RoedSimon90") +
        theme_economist() +
        scale_colour_manual(values=pal1) +
        theme(legend.position='none')


setwd("C:/Users/Simon/Desktop/Data Science/R/TidyTuesday/Week 19 - Nobel Prize")
ggsave("CountryRankNobelPrizeWins.png", width = 16, height = 9)
ggsave("CountryRankNobelPrizeWins2.png")

