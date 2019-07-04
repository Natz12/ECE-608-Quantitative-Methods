# ECE608 Assignment 1


library("tidyverse")
library("gridExtra")
library("grid")

bgunder <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv')

bg <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv')

#head(bg)
head(bg)
str(bg)
sum(is.na(bg))

# Scope of the Data Set

p1 <- ggplot(bg, aes(x = year_published, y = average_rating, alpha = users_rated)) + 
  geom_point(color = "red2") +
    labs(x = "Year Published", y = "Average Rating", alpha = "Number of users that\nrated the game")
p2 <- ggplot(bg, aes(x = year_published, y = average_rating, alpha = users_rated)) + 
  geom_jitter(color = "red2") +
  labs(x = "Year Published", y = "Average Rating", alpha = "Number of users that\nrated the game")

fig_cap=textGrob("Figure 1. Scatterplot using geom_point (first) and using geom_jitter (second) for the relation between Average Ranking and the Year of Publication of a board game", 
           gp=gpar(fontsize=9, col="#2b2b2b"), 
           x=unit(0.005, "npc"), just=c("left", "bottom"))

my_layout <- rbind(c(1), c(2), c(3))
grid.arrange(p1,p2,fig_cap, layout_matrix = my_layout, heights=c(0.47, 0.47, 0.06))

# Exploring category type of games: "Dice" and "Cards"

# Filtering the data set to only include the board games that are 'Card Game', 'Dice' or Both type games
bg2 <- bg %>% 
  filter(grepl("Dice", category) | grepl("Card", category)) %>% 
  mutate(type = ifelse(grepl("Dice", category) & grepl("Card", category), "Both", ifelse(grepl("Dice", category), "Dice", "Cards")))

#Visualizing a game with Both type games
#head(bg2$category,14)
#head(bg2$type,14)

#Plotting a scatter plot, box plot and violin plot of the average_rating per type of game

p1 <- ggplot(bg2, aes(x = type, y = average_rating)) + 
  geom_jitter(color = "magenta4")+
   labs(x = "Type of Game", y = "Average Rating")

p2 <- ggplot(bg2, aes(x = type, y = average_rating)) + 
  geom_boxplot(color = "turquoise4")+
   labs(x = "Type of Game", y = "Average Rating")
  
p3 <- ggplot(bg2, aes(x = type, y = average_rating))+ 
  geom_violin(color = "deeppink4")+
   labs(x = "Type of Game", y = "Average Rating")

fig_cap=textGrob("Figure 2. Scatterplot using (left), box plot (center) and violin plot (right) for the relation between Average Rating and the Type of Game of a board game", 
           gp=gpar(fontsize=9, col="#2b2b2b"), 
           x=unit(0.005, "npc"), just=c("left", "bottom"))

my_layout <- rbind(c(1:3), c(4,4,4))
grid.arrange(p1,p2,p3, fig_cap, layout_matrix = my_layout, heights=c(0.95, 0.05))












