## Class Notes 2

#vjust = vertical adjust
#hjust = horizontal adjust

read.delim('Data/DatasaurusDozen.tsv')

dat = read.delim('Data/DatasaurusDozen.tsv')


View(dat)

summary(dat)
dim(dat)

dat |> 
  group_by(dataset) |> 
  summarize

penguins |> 
  drop_na(sex) |> 
  ggplot(aes(x = bill_depth_mm,
                       y = body_mass_g,
                       colour = sex)) +
  geom_point(alpha = 0.75, size = 4) +
  facet_wrap(~species) +
  labs(
    x = "Bill depth (mm)",
    y = "Body mass (g)",
    color = "Sex"
  ) + 
  scale_color_viridis_d(end = 0.8) +
  theme_bw()


ggplot(penguins, aes(x = bill_depth_mm,
                     y = body_mass_g,
                     colour = sex)) +
  geom_point(alpha = 0.75, size = 34) +
  facet_wrap(~species) +
  labs(
    x = "Bill depth (mm)",
    y = "Body mass (g)",
    color = "Sex"
  ) + 
  scale_color_manual(values = c("female" = "purple", "male" = "green")) +
  theme_bw()

install.packages("gapminder")
install.packages("gganimate")
install.packages("gifski")

library(gapminder)
library(ggplot2)
library(gganimate)
library(gifski)
gapminder
dim(gapminder)
View(gapminder)
names(gapminder)
unique(gapminder$continent)
unique(gapminder$country)




## Making a gif plot
ggplot(gapminder, aes(x = lifeExp, 
                      y = gdpPercap,
                      colour = continent)) +
  geom_point(size = 2) +
  geom_text(aes(label = country), vjust = 1.5, size = 3) +
  facet_wrap(~ continent) +
  labs(title = 'Year: {frame_time}') +
  transition_time(time = year)

anim_save("my_anim.gif")






##make it possible for labels to not overlap
install.packages('ggrepel')
library(ggrepel)

##making that gif with non-overlapping labels
ggplot(gapminder, aes(x = lifeExp, 
                      y = gdpPercap,
                      colour = continent)) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = country), vjust = 1.5, size = 3) +
  facet_wrap(~ continent) +
  labs(title = 'Year: {frame_time}') +
  transition_time(time = year)


ggplot(gapminder, aes(x = country, 
                      y = lifeExp,
                      colour = continent)) +
  geom_point(size = 2) +
  facet_wrap(~ continent)


library(tidyverse)

table1
table2

table2 %>% 
  pivot_wider(names_from = type,
              values_from = count)

table3

table3 %>% 
  separate(rate, into = c('cases', 'pop'))
table3 %>% 
  separate(rate, into = c('cases', 'pop'), convert = T)

table5 |> 
  mutate(year = as.numeric(paste0(table5$century, table5$year))) |> 
  select(-century) |> 
  separate(rate, into = c('cases', 'population'), convert = T)

library(readxl)
install.packages("janitor")
library(janitor)




