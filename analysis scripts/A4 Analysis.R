library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
library(ggthemes)
library(extrafont)

csv <- "A4 - Antonio Jurlina experiment-table.csv"

output <- read_csv(csv, skip = 6)

colnames(output) <- c("run", "movement", "location", "color", 
                      "terrain", "q", "step", "clumps", "corridor-width")

output$color <- NULL
output$terrain <- NULL

output <- output %>%
  arrange(run, step)

output_clean <- output %>%
  group_by(movement, location, q, step) %>%
  summarize(clumps = mean(clumps),
            `corridor-width` = mean(`corridor-width`))


plot <- ggplot(output_clean, aes(x = step, y = `corridor-width`, color = factor(q))) +
  geom_line(size = 1) +
  facet_grid(movement ~ location) +
  labs(title = "Changes in Corridor Width",
       subtitle = "(as a function of q)",
       x = "Ticks",
       y = "Corridor width",
       color = "q") +
  scale_color_ptol() +
  theme_minimal()

ggsave("Corridor Widht Plot.jpg", plot, height = 5, width = 7)

regression <- output_clean %>%
  filter(movement == "uphill") %>%
  lm(clumps ~ location + q, .) %>%
  tidy()

regression_2 <- output_clean %>%
  filter(location == "cluster1" & movement == "uphill") %>%
  lm(clumps ~ q, .) %>%
  tidy()

regression_3 <- output_clean %>%
  filter(location == "cluster1" & movement == "any higher neighbor") %>%
  lm(clumps ~ q, .) %>%
  tidy()
    
    
    
    

