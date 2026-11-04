

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}


BiocManager::install("GEOquery")
no
n

library(GEOquery)

gse <- getGEO("GSE74098", GSEMatrix = TRUE)

capabilities("libcurl")
options(download.file.method = "libcurl")
gse <- getGEO("GSE74098", GSEMatrix = TRUE)

url <- "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE74nnn/GSE74098/matrix/GSE74098_series_matrix.txt.gz"

download.file(
  url,
  destfile = "GSE74098_series_matrix.txt.gz",
  method = "libcurl",
  mode = "wb"
)

gse_data <- read.delim(
  gzfile("GSE74098_series_matrix.txt.gz"),
  comment.char = "!",
  check.names = FALSE
)

head(gse_data)
dim(gse_data)
str(gse_data)


sample_info <- data.frame(
  sample = c(
    "GSM1909854",
    "GSM1909855",
    "GSM1909856",
    "GSM1909857",
    "GSM1909858",
    "GSM1909859"
  ),
  treatment = c(
    "Control",
    "Control",
    "Control",
    "Mastic",
    "Mastic",
    "Mastic"
  )
)

sample_info

readLines(
  gzfile("GSE74098_series_matrix.txt.gz"),
  n = 30
)


url_gpl <- "https://ftp.ncbi.nlm.nih.gov/geo/platforms/GPL16nnn/GPL16933/soft/GPL16933_family.soft.gz"
download.file(
  url_gpl,
  destfile = "GPL16933_family.soft.gz",
  method = "libcurl",
  mode = "wb"
)


platform_lines <- readLines(
  gzfile("GPL16933_family.soft.gz"),
  n = 30
)

platform_lines

grep(
  "platform_table_begin",
  platform_lines
)


grep(
  "platform_table_begin",
  readLines(gzfile("GPL16933_family.soft.gz"))
)

platform_data <- read.delim(
  gzfile("GPL16933_family.soft.gz"),
  skip = 483,
  header = TRUE,
  sep = "\t",
  comment.char = "#",
  check.names = FALSE
)

head(platform_data)

sum(gse_data$ID_REF %in% platform_data$ID)

combined_data <- merge(
  gse_data,
  platform_data,
  by.x = "ID_REF",
  by.y = "ID"
)

dim(combined_data)

sum(duplicated(platform_data$ID))
dim(platform_data)

readLines(
  gzfile("GPL16933_family.soft.gz"),
  n = 490
)[480:490]

platform_data <- read.delim(
  gzfile("GPL16933_family.soft.gz"),
  skip = 484,
  header = TRUE,
  sep = "\t",
  quote = "",
  check.names = FALSE
)

platform_lines <- readLines(
  gzfile("GPL16933_family.soft.gz")
)

length(platform_lines)
tail(platform_lines, 10)
grep(
  "^!platform_table_end",
  platform_lines
)
platform_data <- read.delim(
  gzfile("GPL16933_family.soft.gz"),
  skip = 484,
  nrows = 75213,
  header = TRUE,
  sep = "\t",
  quote = "",
  check.names = FALSE
)

dim(platform_data)
sum(gse_data$ID_REF %in% platform_data$ID)
head(gse_data$ID_REF)
head(platform_data$ID)

names(platform_data)
platform_data <- read.delim(
  text = platform_lines[485:75697],
  sep = "\t",
  header = TRUE,
  quote = "",
  check.names = FALSE
)

head(names(platform_data))
dim(platform_data)

platform_lines[485]

platform_data <- read.delim(
  text = platform_lines[484:75697],
  sep = "\t",
  header = TRUE,
  quote = "",
  check.names = FALSE
)

head(names(platform_data))
dim(platform_data)

sum(gse_data$ID_REF %in% platform_data$ID)

combined_data <- merge(
  gse_data,
  platform_data,
  by.x = "ID_REF",
  by.y = "ID"
)

dim(combined_data)
head(combined_data$gene_assignment, 10)

combined_data$gene_symbol <- sapply(
  strsplit(combined_data$gene_assignment, " /// | // "),
  function(x) {
    if (length(x) >= 2) x[2] else NA
  }
)

head(combined_data[, c("ID_REF", "gene_symbol")], 10)
sum(is.na(combined_data$gene_symbol))

head(
  combined_data$gene_assignment[
    is.na(combined_data$gene_symbol)
  ],
  10
)

clean_data <- combined_data[
  !is.na(combined_data$gene_symbol),
]
dim(clean_data)
names(clean_data)

sample_info
library(tidyverse)

long_data <- clean_data %>%
  pivot_longer(
    cols = starts_with("GSM"),
    names_to = "sample",
    values_to = "expression"
  )
dim(long_data)

long_data <- merge(
  long_data,
  sample_info,
  by = "sample"
)
head(long_data)

table(long_data$treatment, long_data$sample)
length(unique(long_data$gene_symbol))
sum(duplicated(clean_data$gene_symbol))
library(dplyr)

gene_expression <- long_data %>%
  group_by(gene_symbol, sample, treatment) %>%
  summarise(
    average_expression = mean(expression),
    .groups = "drop"
  )
dim(gene_expression)

group_expression <- gene_expression %>%
  group_by(gene_symbol, treatment) %>%
  summarise(
    mean_expression = mean(average_expression),
    .groups = "drop"
  )

head(group_expression)

wide_expression <- group_expression %>%
  pivot_wider(
    names_from = treatment,
    values_from = mean_expression
  )
head(wide_expression)

wide_expression <- wide_expression %>%
  mutate(
    expression_difference = Mastic - Control
  )



