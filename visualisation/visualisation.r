library(ggplot2)
library(dplyr)
library(purrr)

data <- read.csv(file = 'gapminder.csv')
print(head(data))

# Plot life expectancy in UK and Burkina Faso

countries  <- c("United Kingdom", "Burkina Faso")

data_uk_bf = data %>% 
  filter(country %in% countries) 

mean_life_exp_ukbf = data_uk_bf %>% 
  group_by(country) %>% 
  summarise(MeanLifeExp=mean(lifeExp))
print(mean_life_exp_ukbf)

ggplot(data_uk_bf, aes(x=year, y=lifeExp, colour=country)) + 
  geom_point() + geom_line()

ggsave(filename = "life_exp_ukbf.png", device = "png", path = "images")

ggplot(data_uk_bf, aes(x=year, y=lifeExp, colour=country)) +
  geom_point() + geom_smooth(method='lm', se=FALSE)

ggsave(filename = "life_exp_ukbf_reg.png", device = "png", path = "images")


# Plot life expectancy in the Americas

data_americas = data[(data$continent == "Americas"),]

ggplot(data_americas, aes(x=year, y=lifeExp, colour=country)) +
  geom_point() + geom_smooth()

ggsave(filename = "life_exp_americas_reg.png", device = "png", path = "images")


# Compare life expectancy in all continents

ggplot(data, aes(x=year, y=lifeExp)) + geom_point() + facet_wrap(~continent) +
  geom_smooth(color="orange")

ggsave(filename = "life_exp_continent_reg.png", device = "png", path = "images")
