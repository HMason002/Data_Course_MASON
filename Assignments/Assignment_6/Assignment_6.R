      #### Assignment 6 ####

# Load librarys of tidyverse, ggplot2, and gganimate, and then read the data set
library(tidyverse)
library(ggplot2)
library(gganimate)

dat <- read.csv("BioLog_Plate_Data.csv")

# View the data set to see what we are working with and plan our tidying steps
head(dat)
dim(dat)
colnames(dat)
str(dat)
view(dat)

## Step 1: Create new column to specify if sample is from soil or water
dat <- dat %>%
  mutate(Sample_Type = case_when(
    Sample.ID == "Clear_Creek" ~ "Water",
    Sample.ID == "Waste_Water" ~ "Water",
    Sample.ID == "Soil_1" ~ "Soil",
    Sample.ID == "Soil_2" ~ "Soil"
  ))

# Check that the new column is there and works
head(dat)
dim(dat)
dat %>%
  distinct(Sample.ID, Sample_Type)

## Step 2: Tidy the column using pivot longer
# Make the (hr_) sample data into a time column and absorbance data in another new column
dat_long <- dat |> 
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  )

#check to make sure the data is longer
head(dat_long)
str(dat_long)
view(dat_long)

## Step 3: Filter for only the 0.1 dilution and generate plots for this dilution
dat_filtered <- dat_long |> 
  filter(Dilution == 0.1)

# Calculate the mean absorbance data for graphs
dat_mean <- dat_filtered %>%
  mutate(Time = readr::parse_number(Time)) %>%
  group_by(Substrate, Sample_Type, Time) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance),
    .groups = "drop"
  )

# Generate the plots for each subrate
ggplot(dat_mean, aes(x = Time, y = Mean_Absorbance,
                     color = Sample_Type,
                     group = Sample_Type)) +
  geom_smooth(method = "loess",
              span = 1.0,
              se = FALSE) +
  facet_wrap(~Substrate, ncol = 6) +
  labs(
    title = "Dilution 0.1",
    x = "Time",
    y = "Absorbance",
    color = "Type"
  )

## Step 4: Make the animated plot for absorbance change of Itaconic Acid
#filter the data
dat_filtered_2 <- dat_long |> 
  mutate(Time = readr::parse_number(Time))

itaconic_acid <- dat_filtered_2 %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(Dilution, Sample.ID, Time) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"
  )

# Make the animation with gganimate

itaconic_acid_animated_plot <- ggplot(itaconic_acid,
                        aes(x = Time,
                            y = Mean_Absorbance,
                            color = Sample.ID,
                            group = Sample.ID)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Dilution) +
  labs(
    title = "Itaconic Acid Absorbance Over Time",
    x = "Time (Hours)",
    y = "Mean Absorbance",
    color = "Sample"
  ) +
  transition_reveal(Time) +
  theme_minimal()

itaconic_acid_animated_plot
