library(dplyr)
library(stringr)
library(readxl)
install.packages("openxlsx")
library(openxlsx)

# Read  Excel file
file_path <- "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/nobles_without_ranks.xlsx"
data_nobles2 <- read_excel(file_path)

# Define lookup: regex patterns for noble ranks
rank_patterns <- c(
  "Kaiser" = "Kaiser",
  "Kaiserin" = "Kaiserin",
  "Erzherzog" = "Erzherzog",
  "Erzherzogin" = "Erzherzogin",
  "Herzog" = "Herzog",
  "Herzogin" = "Herzogin",
  "Fürst" = "Fürst",
  "Fürstin" = "Fürstin",
  "Gräfin" = "Gräfin",
  "(?<![äÄ])Graf" = "Graf",
  "Freiherr" = "Freiherr",
  "Freiin" = "Freiin",
  "Freifrau" = "Freifrau",
  "Baronin" = "Baronin",
  "(?<![iI])Baron" = "Baron",
  "Ritter" = "Ritter",
  "Edler" = "Edler",
  "Edle" = "Edle",
  "Prinzessin" = "Prinzessin"
)

# Function to extract the first matching rank
extract_rank <- function(name) {
  if (is.na(name)) return(NA_character_)  
  for (pattern in names(rank_patterns)) {
    if (str_detect(name, regex(pattern))) {
      return(rank_patterns[[pattern]])
    }
  }
  # special case: if only "von" but no other title
  if (str_detect(name, "\\bvon\\b")) {
    return("von")
  }
  return(NA_character_)
}

# Apply function ONLY to missing Rank rows
data_nobles2 <- data_nobles2 %>%
  mutate(Rank = if_else(
    is.na(Rank) | Rank == "",
    sapply(Name, extract_rank),
    Rank
  ))

# Save back to Excel
write.xlsx(data_nobles2, "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/nobles_with_ranks.xlsx")
