##Check to confirm place in computer directory##
getwd()

##Write a command that lists all of the .csv files found in the Data/ directory and stores that list in an object called “csv_files”##
csv_files = list.files(path = 'data', pattern = ".csv")

##Find how many files match that description using the length() function##
length(csv_files)

##Open the wingspan_vs_mass.csv file and store the contents as an R object named “df” using the read.csv() function##
df = read.csv(file = "Data/wingspan_vs_mass.csv")

##Inspect the first 5 lines of this data set using the head() function##
head(df)

##Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)##
b_files = list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)

##Write a command that displays the first line of each of those “b” files (this is tricky… use a for-loop)##
for (i in b_files) {
  print(readLines(i, n = 1))
}

##Do the same thing for all files that end in “.csv”##
csv_files_full = list.files(path = "Data", pattern = ".csv", recursive = TRUE, full.names = TRUE)
for (i in csv_files_full) {
  print(readLines(i, n = 1))
}



