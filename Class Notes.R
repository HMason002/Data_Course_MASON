getwd()
getwd()
read.csv('Data/')
read.csv('Data/1620_scores.csv')
# practice note #

getwd
getwd()

csvfile = read.csv('Data/1620_scores.csv')

obj1 = 2
is.character(obj1)
is.numeric(obj1)

as.character(obj1)

is.numeric(obj1)

obj1 = as.character(obj1)

is.numeric(obj1)

vec = c(1, 2, 3)
vec
vec + 1

read.csv(wingspan_vs_mass)

read.csv(wingspan_vs_mass)

read.csv(Users/huey/Desktop/Data_Course_MASON/Data/wingspan_vs_mass.csv)
getwd()
read.csv('Data/wingspan_vs_mass.csv')

firstfile = ('Data/wingspan_vs_mass.csv')

birds = c('falcon', 'eagle', 'hawk', 'owl')

for (i in birds) {
  print(i)
}

csv_files = list.files("data", pattern = "\\.csv$")

length(csv_files)

df = read.csv("Data/wingspan_vs_mass.csv")

head(df)

b_files = list.files("Data", pattern = "^b", recursive = TRUE)

for (file in b_files) {
  print(readLines(file, n = 1))
}

getwd

getwd()

csv_files = list.files("data", pattern = "\\.csv$")

length(csv_files)

df = read.csv("Data/wingspan_vs_mass.csv")

head(df)

b_files = list.files("Data", pattern = "^b", recursive = TRUE)

for (file in b_files) {
  print(readLines(file, n = 1))
}

paste0('Data/',)


mtcars

df_car = mtcars

dim(df_car)
ncol(df_car)

head(df_car)
str(df_car)
names(df_car)

df_car[2,3]

df_car[c(1,2,5), 1:3]

View(df_car)

max(df_car$mpg)
min(df_car$mpg)
mean(df_car$mpg)

df_car$mpg

car_mpg = df_car$mpg

max(car_mpg)

df_car$mpg > 20

df_car[df_car$mpg > 20,]

goodmpg = df_car[df_car$mpg > 20,]

View(goodmpg)



## sep 22 notes

getwd()

birds = read.csv('Data/cleaned_bird_data.csv')
?read.csv

dim(birds)

View(birds)

#restart tidyverse every time
library(tidyverse)

##filter the column to remove NA non numeric values
## remember that != means "not equal to" in tidyverse
bird_eggs = birds %>%
  filter(Egg_mass != 'NA') 

##Choose from the egg mass column, get the mean
bird_eggs %>%
  pluck('Egg_mass') %>%
  mean()

## Now filter out all eggs that are smaller than the mean
big_eggs = bird_eggs %>%
  filter(Egg_mass > 21.784)

view(big_eggs)
dim(big_eggs)

##Write and save this as a new folder in my data_course_MASON folder
write.csv(big_eggs, "big_eggs.csv", row.names = F)

##check that the minimum is greater than average
min(big_eggs$Egg_mass)

##Grab out the min, max, and avg of the egg masses when the egg mass is greater
## than 21.784
## it can also be grouped by gender
bird_eggs %>%
  filter(Egg_mass > 21.784) %>%
  group_by(Gender) %>%
  summarise(min_egg = min(Egg_mass),
            max_egg = max(Egg_mass),
            avg_egg = mean(Egg_mass),
            sd = mean(Egg_mass))

## grouping by another column for egg mass sizes
bird_eggs %>%
  filter(Egg_mass > 21.784) %>%
  group_by(Mating_System) %>%
  summarise(min_egg = min(Egg_mass),
            max_egg = max(Egg_mass),
            avg_egg = mean(Egg_mass))

##Sep 24 Notes
getwd()

## Download 'palmerpenguins' package
??palmerpenguins
install.packages("palmerpenguins")
library(palmerpenguins)
view(penguins)

##caluclate average bill length
mean(penguins$bill_length_mm, na.rm = TRUE)

##(bonus) calculate average bill length by species
bill_length %>%
  group_by(species) |> 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE))

## search for NA values
!is.na(penguins$bill_length_mm)
#or you can also 
anyNA(penguins$bill_length_mm)

##notes from what she did
penguins %>%
  group_by(species) |> 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_length = max(bill_length_mm, na.rm = T),
            min_length = min(bill_length_mm, na.rm = T))

##Remove na when the  cell is empty instead of na being a real input.
bill_length_avg = penguins %>%
  filter(!is.na(penguins$bill_length_mm)) 


bill_length_avg |> 
  group_by(species, island) |> 
  summarise(avg_bill_length = mean(bill_length_mm),
            max_length = max(bill_length_mm)) |> 
  arrange(desc(max_length)) |> 
  relocate(species, .after = island)

##practice using arrange() & relocate()

bill_length_avg |> 
  group_by(species) |> 
  summarise(avg_bill_length = mean(bill_length_mm),
            min_length = min(bill_length_mm),
            max_depth = max(bill_depth_mm)) |> 
  arrange(desc(min_length))

## More practice
bill_length_avg |> 
  group_by(species) |> 
  summarise(avg_bill_length = mean(bill_length_mm),
            min_length = min(bill_length_mm),
            max_depth = max(bill_depth_mm)) |> 
  arrange(desc(min_length))|> 
  relocate(max_depth, .before = avg_bill_length)

##new column in tidyverse is called "mutate"

bill_length_avg |> 
  group_by(species) |> 
  summarise(avg_bill_length = mean(bill_length_mm),
            min_length = min(bill_length_mm),
            max_depth = max(bill_depth_mm)) |> 
  arrange(desc(min_length))|> 
  relocate(max_depth, .before = avg_bill_length) |> 
  mutate(avg_and_min = avg_bill_length + min_length, .after = max_depth)

##remove things like a column
#filter- "column namme".... example filter(-min_length)

##Practice removing a column
bill_length_avg |> 
  group_by(species) |> 
  summarise(avg_bill_length = mean(bill_length_mm),
            min_length = min(bill_length_mm),
            max_depth = max(bill_depth_mm)) |> 
  arrange(desc(min_length))|> 
  relocate(max_depth, .before = avg_bill_length) |> 
  mutate(avg_and_min = avg_bill_length + min_length, .after = max_depth) |> 
  select(-max_depth)

## Case_when(condition ~if T, then...)

#add a new column with a condition using case when
bill_length_avg |> 
  group_by(species) |> 
  summarise(avg_bill_length = mean(bill_length_mm),
            min_length = min(bill_length_mm),
            max_depth = max(bill_depth_mm)) |> 
  arrange(desc(min_length))|> 
  relocate(max_depth, .before = avg_bill_length) |> 
  mutate(small_penguin = case_when(min_length < 40 ~ 'smallboi',
                                   TRUE ~ 'Normal'))








