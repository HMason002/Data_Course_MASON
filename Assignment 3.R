## Assignment 3

mtcars
df_mtcars = mtcars


df_mtcars$mpg

goodmpg = df_mtcars[df_mtcars$mpg > 20, ]
View(goodmpg)




df_mpg_20 = df_mtcars[df_mtcars$mpg > 20 & df_mtcars$cyl == 4, ]


names(df_mtcars[,-1])
dim(df_mtcars[, -1])

names(mtcars)

practice_subtract = df_mtcars[,-c(4,6)]

practice_subtract [practice_subtract$mpg > 20 & practice_subtract$cyl != 4, ]

#%>% 
df_mtcars %>%
  names()

mean(df_mtcars$mpg)

practice_tidy = df_mtcars %>%
  filter(mpg > 22, cyl == 4, wt < 3, hp > 90) %>%
  select(-mpg, -cyl)
  
df_mtcars %>%
  filter(mpg > 22, cyl == 4, wt < 3, hp > 90) %>%
  select(-mpg, -cyl)

practice_tidy %>%
  pluck('mpg')

practice_tidy %>%
  pluck('mpg') %>%
  mean()

df_mtcars %>%
  filter(cyl !=4) %>%
  pluck('mpg') %>%
  mean()






practice_filter = df_mtcars[df_mtcars$mpg > 22 & df_mtcars$cyl == 4 & df_mtcars$wt < 3 & df_mtcars$hp > 90, ]
view(practice_tidy)




## install packages
install.packages("qrcode")
library(qrcode)

install.packages("tidyverse")
library(tidyverse)



