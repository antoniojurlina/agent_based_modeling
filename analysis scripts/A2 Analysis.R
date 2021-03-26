library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(broom)

csv_b <- "A1 - Antonio Jurlina (b) - BS experiment-table.csv"

output_b <- read_csv(csv_b, skip = 6)

output_b <- output_b[, -3]

colnames(output_b) <- c("run", "spree", "step", "turtles", "min_mnms", "max_mnms")

output_b <- output_b %>%
  arrange(run, step) %>%
  mutate(range = max_mnms - min_mnms)

lm <- output_b %>%
  group_by(turtles, spree, step) %>%
  summarize(min_mnms = round(mean(min_mnms), 0),
            max_mnms = round(mean(max_mnms), 0),
            range = round(mean(range), 0)) %>%
  arrange(spree, turtles, step)

analysis <- output_b %>%
  group_by(turtles, spree) %>%
  summarize(range = max(range)) %>%
  arrange(spree, turtles)

analysis <- analysis[-which(analysis$range == 0),]

regression_no_spree <- analysis %>% 
  filter(spree == "FALSE") %>%
  lm(range ~ turtles, .) %>%
  tidy()

regression_spree <- analysis %>% 
  filter(spree == "TRUE") %>%
  lm(range ~ turtles, .) %>%
  tidy() 

analysis_spree <- analysis[which(analysis$spree == "TRUE"),]
analysis_no_spree <- analysis[-which(analysis$spree == "TRUE"),]
analysis_wide <- bind_cols(analysis_spree, analysis_no_spree)
analysis_wide <- analysis_wide[, -4]

analysis_wide <- analysis_wide %>%
  mutate(diff = range - range1)

difference_regression <- analysis_wide %>%
  lm(diff ~ turtles, .) %>%
  tidy()

analysis_coded <- analysis
analysis_coded[which(analysis_coded$spree == "TRUE"), 2] <- 1
analysis_coded[which(analysis_coded$spree == "FALSE"), 2] <- 0

regression <- analysis_coded %>%
  lm(range ~ turtles + spree, .) %>%
  tidy()
