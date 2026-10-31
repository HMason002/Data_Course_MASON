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




messy_bp = read_excel('Data/messy_bp.xlsx', skip = 3)
view(messy_bp)

# 1. combine months days years of messy bp

?unite
messy_bp |> 
  unite(`Month of birth`, `Day birth`, sep = '-')

## Fix the dates
tidy_date_bp = messy_bp |> 
  clean_names() |> 
  mutate(birthday = paste(year_birth, month_of_birth, day_birth, sep = '-')) |> 
  select(-month_of_birth, -day_birth, -year_birth)

view(tidy_date_bp)

##ignore the hr data and make the bp data tidy
tidy_bp = messy_bp |> 
  clean_names() |> 
  mutate(birthday = paste(year_birth, month_of_birth, day_birth, sep = '-')) |> 
  select(-month_of_birth, -day_birth, -year_birth) |> 
  select(-starts_with("hr")) |> 
  pivot_longer(cols = starts_with("bp"),
               names_to = 'visit',
               values_to = 'bp') |> 
  mutate(visit = case_when(visit =='bp_8' ~ '1',
                           visit =='bp_10' ~ '2',
                           visit =='bp_12' ~ '3')) |> 
  separate(bp, into = c('sys', 'dia'))

##make the hr data tidy
tidy_hr = messy_bp|> 
  clean_names() |> 
  mutate(birthday = paste(year_birth, month_of_birth, day_birth, sep = '-')) |> 
  select(-month_of_birth, -day_birth, -year_birth, -bp_8, -bp_10, -bp_12) |> 
  pivot_longer(cols = starts_with("hr"),
               names_to = 'visit',
               values_to = 'hr') |> 
  mutate(visit = case_when(visit =='hr_9' ~ '1',
                           visit =='hr_11' ~ '2',
                           visit =='hr_13' ~ '3')) |> view(tidy_hr)

##combine the tidy hr data and the tidy bp data
tidy_hr_bp = full_join(tidy_bp, tidy_hr)
view(tidy_hr_bp)

tidy_hr_bp$race |> unique()

## combine and make the race section tidy
tidy_hr_bp |> 
  mutate(race = case_when(race == 'Caucasian' | race == 'WHITE' ~ 'White',
                           TRUE ~ race)) |> view()

## October 29th Class
birds = read_csv('Data/Bird_Measurements.csv')
view(birds)

install.packages('skimr')
library(skimr)

skim(birds)
skim_birds = skim(birds)
view(skim_birds)

## Use skim to tidy the data frame

keep = skim_birds |> 
  filter(complete_rate >= 0.66) |> 
  pluck('skim_variable') 

str(birds)
names(birds)

birds_m = birds |> 
  select(keep) |> 
  select(keep, -starts_with('F_')) |> 
  mutate(sex = 'male')
  
names(birds_m) = names(birds_m) |> str_remove('M_')
view(birds_m)

birds_f = birds |> 
  select(keep) |> 
  select(keep, -starts_with('M_')) |> 
  mutate(sex = 'female')

names(birds_f) = names(birds_f) |> str_remove('F_')
view(birds_f)

full_join(birds_m, birds_f) |> view()






