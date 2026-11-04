
# ==========================================
# ASSIGNMENT 4: ZEBRAFISH ESSENTIAL OIL DATA
# GEO Dataset: GSE74098
# ==========================================


# 1. Load packages
# ==========================================

# Install tidyverse if it is not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(tidyverse)


# 2. Download and import GEO expression data
# ==========================================

options(download.file.method = "libcurl")

url <- "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE74nnn/GSE74098/matrix/GSE74098_series_matrix.txt.gz"

if (!file.exists("GSE74098_series_matrix.txt.gz")) {
  download.file(
    url,
    destfile = "GSE74098_series_matrix.txt.gz",
    method = "libcurl",
    mode = "wb"
  )
}

gse_data <- read.delim(
  gzfile("GSE74098_series_matrix.txt.gz"),
  comment.char = "!",
  check.names = FALSE
)

# Check expression data
dim(gse_data)
head(gse_data)


# 3. Create sample information
# ==========================================

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


# 4. Download and import platform annotations
# ==========================================

url_gpl <- "https://ftp.ncbi.nlm.nih.gov/geo/platforms/GPL16nnn/GPL16933/soft/GPL16933_family.soft.gz"

if (!file.exists("GPL16933_family.soft.gz")) {
  download.file(
    url_gpl,
    destfile = "GPL16933_family.soft.gz",
    method = "libcurl",
    mode = "wb"
  )
}

# Read the platform file
platform_lines <- readLines(
  gzfile("GPL16933_family.soft.gz")
)

# Extract only the platform annotation table
platform_data <- read.delim(
  text = platform_lines[484:75697],
  sep = "\t",
  header = TRUE,
  quote = "",
  check.names = FALSE
)

# Check platform data
dim(platform_data)
head(platform_data)


# 5. Combine expression data and annotations
# ==========================================

# Check that the gene IDs match
sum(gse_data$ID_REF %in% platform_data$ID)

# Merge the two datasets
combined_data <- merge(
  gse_data,
  platform_data,
  by.x = "ID_REF",
  by.y = "ID"
)

dim(combined_data)


# 6. Extract gene symbols
# ==========================================

combined_data$gene_symbol <- sapply(
  strsplit(combined_data$gene_assignment, " /// | // "),
  function(x) {
    if (length(x) >= 2) x[2] else NA
  }
)

# Check extracted gene symbols
head(combined_data[, c("ID_REF", "gene_symbol")])

# Remove entries without gene symbols
clean_data <- combined_data[
  !is.na(combined_data$gene_symbol),
]

dim(clean_data)


# 7. Reshape data into long format
# ==========================================

long_data <- clean_data %>%
  pivot_longer(
    cols = starts_with("GSM"),
    names_to = "sample",
    values_to = "expression"
  )

dim(long_data)


# 8. Connect samples to treatment groups
# ==========================================

long_data <- merge(
  long_data,
  sample_info,
  by = "sample"
)

# Check treatment assignments
table(long_data$treatment, long_data$sample)


# 9. Average repeated gene entries
# ==========================================

gene_expression <- long_data %>%
  group_by(gene_symbol, sample, treatment) %>%
  summarise(
    average_expression = mean(expression),
    .groups = "drop"
  )

dim(gene_expression)


# 10. Calculate average expression by treatment
# ==========================================

group_expression <- gene_expression %>%
  group_by(gene_symbol, treatment) %>%
  summarise(
    mean_expression = mean(average_expression),
    .groups = "drop"
  )

head(group_expression)


# 11. Reshape into wide format
# ==========================================

wide_expression <- group_expression %>%
  pivot_wider(
    names_from = treatment,
    values_from = mean_expression
  )

# Check final dataset
dim(wide_expression)
head(wide_expression)


wide_expression <- wide_expression %>%
  mutate(
    expression_difference = Mastic - Control
  )

head(wide_expression)

top_increased <- wide_expression %>%
  arrange(desc(expression_difference)) %>%
  slice_head(n = 10)

top_increased


top_decreased <- wide_expression %>%
  arrange(expression_difference) %>%
  slice_head(n = 10)

top_decreased

top_genes <- bind_rows(
  top_increased,
  top_decreased
)

top_genes

ggplot(top_genes,
       aes(x = reorder(gene_symbol, expression_difference),
           y = expression_difference,
           fill = expression_difference > 0)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "purple"),
    guide = "none"
  ) +
  labs(
    title = "Largest Gene Expression Differences in Zebrafish",
    subtitle = "Mastic Essential Oil Group Compared with Control",
    x = "Gene",
    y = "Expression Difference (Mastic - Control)"
  ) +
  theme_minimal()














