library(tidyverse)
library(plotly)

data <- read_csv("IPL 2022.csv")

# Table of number of matches won by each team
table1 <- data %>%
  group_by(match_winner) %>%
  summarise(num_wins = n()) %>%
  arrange(desc(num_wins))

# Bar chart of number of matches won by each team
figure1 <- ggplot(table1, aes(x = match_winner, y = num_wins)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Number of Matches Won in IPL 2022")

# Pie chart of matches won by defending or chasing
data$won_by <- ifelse(data$won_by == "Wickets", "Chasing", "Defending")
table2 <- data %>%
  group_by(won_by) %>%
  summarise(num_matches = n())

figure2 <- plot_ly(table2, labels = ~won_by, values = ~num_matches, type = "pie") %>%
  add_pie(hole = 0.6, marker = list(colors = c("gold", "lightgreen"))) %>%
  layout(title = "Number of Matches Won By Defending Or Chasing", showlegend = FALSE)

# Pie chart of toss decision
table3 <- data %>%
  group_by(toss_decision) %>%
  summarise(num_matches = n())

figure3 <- plot_ly(table3, labels = ~toss_decision, values = ~num_matches, type = "pie") %>%
  add_pie(hole = 0.6, marker = list(colors = c("skyblue", "yellow"))) %>%
  layout(title = "Toss Decision", showlegend = FALSE)

# Bar chart of top scorers
figure4 <- ggplot(data, aes(x = top_scorer)) +
  geom_bar(fill = "purple") +
  labs(title = "Top Scorers in IPL 2022")

# Bar chart of most player of the match awards
figure5 <- ggplot(data, aes(x = player_of_the_match)) +
  geom_bar(fill = "green") +
  labs(title = "Most Player of the Match Awards")

# Bar chart of best bowlers
figure6 <- ggplot(data, aes(x = best_bowling)) +
  geom_bar(fill = "orange") +
  labs(title = "Best Bowlers in IPL 2022")

# Grouped bar chart of first and second innings wickets by venue
figure7 <- ggplot(data, aes(x = venue)) +
  geom_col(aes(y = first_ings_wkts, fill = "First Innings Wickets"), position = "dodge") +
  geom_col(aes(y = second_ings_wkts, fill = "Second Innings Wickets"), position = "dodge") +
  labs(title = "Wickets Taken in First and Second Innings by Venue", fill = "") +
  scale_fill_manual(values = c("gold", "lightgreen"))

# Display all the plots
subplot(figure1, figure2, figure3, figure4, figure5, figure6, figure7, nrows = 4, widths = c(2, 2, 2, 1))
