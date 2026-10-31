
# Exam 1

## I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)

covid_data = read.csv(file = "cleaned_covid_data.csv")
head(covid_data)
dim(covid_data)
names(covid_data)
View(covid_data)


## II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)

library(tidyverse)
A_states <- covid_data |> 
  filter(grepl("^A", Province_State))
head(A_states)
dim(A_states)
str(A_states)

## III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)

library(ggplot2)

# Make the Last_update column value a date and not a character
str(covid_data)
covid_data <- covid_data |> 
  mutate(Last_Update = as.Date(Last_Update))
class(covid_data$Last_Update)

A_states = A_states |> 
  mutate(Last_Update = as.Date(Last_Update))
class(A_states$Last_Update)

# Make the plot now that Last_update is a date not a chr
ggplot(A_states, aes(x= Last_Update, y= Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(
    title = "A-States COVID-19 Deaths",
    x = "Date",
    y = "deaths"
  ) +
  theme_minimal()

## IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)

state_max_fatality_rate = covid_data |> 
  filter(!is.na(Case_Fatality_Ratio)) |> 
  group_by(Province_State) |> 
  summarise(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio)
  ) |> 
  arrange(desc(Maximum_Fatality_Ratio))

head(state_max_fatality_rate)
names(state_max_fatality_rate)
view(state_max_fatality_rate)

## V. Use that new data frame from task IV to create another plot. (20 pts)

ggplot(
  state_max_fatality_rate |> 
    mutate(
      Province_State = factor(
        Province_State,
        levels = Province_State
      )
    ),
  aes(x= Province_State, y= Maximum_Fatality_Ratio)
  ) +
  geom_col() +
  labs(
    title = "Maximum COVID-19 Fatality Ratio by State",
    x = "State",
    y = "Maximum Fatality Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

## VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time

US_deaths = covid_data |> 
  group_by(Last_Update) |> 
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE)
  )

head(US_deaths)
view(US_deaths)

ggplot(US_deaths, aes(x= Last_Update, y= Total_Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Cumulative COVID-19 Deaths in the US",
    x = "Date",
    y = "Total Deaths"
  ) +
  theme_minimal()

