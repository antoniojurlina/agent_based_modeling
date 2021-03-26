library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
library(ggthemes)
library(extrafont)

csv <- "A3 - Antonio Jurlina experiment-table.csv"

output <- read_csv(csv, skip = 6)

colnames(output) <- c("run", "spree", "random", "step", 
                      "turtles", "max_mnms", "min_mnms", "gini")

output$random[which(output$random == "TRUE")] <- 1
output$random[which(output$random == "FALSE")] <- 0

output$gini <- output$gini - 0.002

output <- output %>%
  arrange(run, desc(random), spree, step)

output_analysis <- output %>%
  group_by(random, spree, step) %>%
  summarize(n = n(),
            max_mnms_error_margin_99 = 2.576*sd(max_mnms, na.rm = TRUE)/n,
            min_mnms_error_margin_99 = 2.576*sd(min_mnms, na.rm = TRUE)/n,
            gini_error_margin_99 = 2.576*sd(gini, na.rm = TRUE)/n,
            max_mnms = mean(max_mnms, na.rm = TRUE),
            min_mnms = mean(min_mnms, na.rm = TRUE),
            gini = last(gini),
            max_mnms_lower_bound = max_mnms - max_mnms_error_margin_99,
            max_mnms_upper_bound = max_mnms + max_mnms_error_margin_99,
            min_mnms_lower_bound = min_mnms - min_mnms_error_margin_99,
            min_mnms_upper_bound = min_mnms + min_mnms_error_margin_99,
            gini_lower_bound = gini - gini_error_margin_99,
            gini_upper_bound = gini + gini_error_margin_99)

output_regression <- output_analysis %>%
  lm(gini ~ random + spree, .) %>%
  tidy()

output_analysis_plot <- output %>%
  group_by(random, spree) %>%
  summarize(n = n(),
            max_mnms_error_margin_99 = 2.576*sd(max_mnms, na.rm = TRUE)/n,
            min_mnms_error_margin_99 = 2.576*sd(min_mnms, na.rm = TRUE)/n,
            gini_error_margin_99 = 2.576*sd(gini, na.rm = TRUE)/n,
            max_mnms = mean(max_mnms, na.rm = TRUE),
            min_mnms = mean(min_mnms, na.rm = TRUE),
            gini = last(gini),
            max_mnms_lower_bound = max_mnms - max_mnms_error_margin_99,
            max_mnms_upper_bound = max_mnms + max_mnms_error_margin_99,
            min_mnms_lower_bound = min_mnms - min_mnms_error_margin_99,
            min_mnms_upper_bound = min_mnms + min_mnms_error_margin_99,
            gini_lower_bound = gini - gini_error_margin_99,
            gini_upper_bound = gini + gini_error_margin_99)


plot <- ggplot(output_analysis_plot, aes(x = spree, y = gini, color = factor(random))) +
  geom_point(size = 2.1) +
  geom_line(size = 0.9) +
  geom_ribbon(aes(ymin = gini_lower_bound - 0.01,
                  ymax = gini_upper_bound + 0.01,
                  color = factor(random), 
                  fill = factor(random)),
              alpha = 0.1, 
              show.legend = FALSE,
              linetype = 2) +
  labs(title = "Gini Index v. Collecting Spree",
       subtitle = "(wealth distribution among turtles)",
       caption = "Note: shaded region represents 99% confidence interval",
       x = "Collecting spree (n of moves awarded after each successful one)",
       y = "Gini Index",
       color = "Starting position") +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"),
                     labels = c("Center", "Random")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_classic() +
  theme(legend.position = c(0.7, 0.3),
        text = element_text(family = "TT Times New Roman"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"))

plot

ggsave("plot.jpg", plot, width = 7, height = 5)


  
  
  
