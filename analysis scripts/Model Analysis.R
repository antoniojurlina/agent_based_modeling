library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
library(ggthemes)
library(extrafont)

csv <- "Cooperation experiment-table.csv"

output <- read_csv(csv, skip = 6)

output <- output %>%
  select(`[run number]`, `stride-length`, `[step]`, `count greedy-cows`, `count cooperative-cows`)

colnames(output) <- c("run", "stride_length", "step", "greedy", "cooperative")

output <- output %>%
  mutate(diff = greedy - cooperative) %>%
  arrange(run, step)

output_clean <- output %>%
  group_by(stride_length, step) %>%
  summarize(greedy = mean(greedy),
            cooperative = mean(cooperative),
            diff = mean(diff))

regression <- output_clean %>%
  filter(step == 1000) %>%
  lm(diff ~ stride_length, .) %>%
  tidy()

arrow <- tibble (
  x1 = c(0.1),
  x2 = c(0.052),
  y1 = c(-1200),
  y2 = c(-1640)
)

plot1 <- output_clean %>%
  filter(step == 1000) %>%
  ggplot(aes(x = stride_length, y = diff)) +
  geom_point(size = 1.5) +
  geom_line() +
  annotate("text", x = 0.13, y = -1000, size = 4, color = "black",
           label = "at stride length of 0.05 \n greedy - cooperative = -1639") +
  geom_curve(data = arrow, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.6,
             color = "gray20", curvature = -0.3) +
  labs(title = "Stride Length v. Final Cow Numbers",
       subtitle = "(positive y means greedy cows outnumber the cooperative ones)",
       x = "stride length",
       y = "greedy - cooperative",
       caption = "Note: model runs for 1000 ticks") +
  scale_color_ptol() +
  theme_minimal()

ggsave("Model Analysis - Plot 1.jpg", plot1, height = 5, width = 8)

plot2 <- output_clean %>%
  ggplot(aes(x = step, y = diff, color = factor(stride_length))) + 
           geom_point() +
  labs(title = "Cow Numbers Over Time",
       subtitle = "(positive y means greedy cows outnumber the cooperative ones)",
       x = "time",
       y = "greedy - cooperative",
       color = "stride length",
       caption = "Note: model runs for 1000 ticks") +
  theme_minimal()

ggsave("Model Analysis - Plot 2.jpg", plot2, height = 5, width = 8)

plot3 <- output_clean %>%
  ggplot(aes(x = step)) +
    geom_point(aes(y = greedy, color = "Greedy")) +
    geom_point(aes(y = cooperative, color = "Cooperative")) +
  labs(title = "Cow Numbers Over Time",
       x = "time",
       y = "number of cows",
       color = "breed",
       caption = "Note: model runs for 1000 ticks") +
  scale_color_ptol() +
  theme_minimal()

ggsave("Model Analysis - Plot 3.jpg", plot3, height = 5, width = 8)


