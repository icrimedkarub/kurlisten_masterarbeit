############################
## Load required packages
############################

library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

############################
## Define global settings
############################

# Professional, color-blind friendly palette
color_palette <- list(
  blue   = "blue",
  red    = "red",
  orange = "#E0CA3C",
  darkgreen  = "darkgreen",
  purple = "#6a3d9a",
  salmon   = "salmon",
  navy   = "#2D3047",
  lightblue = "#a6cee3",
  yellow = "yellow",
  darkblue = "darkblue"
)

# Common ggplot theme for all figures
common_theme <- theme_minimal() +
  theme(
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 14),
    axis.title.x     = element_text(size = 16),
    axis.title.y     = element_text(size = 16),
    plot.title       = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title     = element_text(size = 16),
    legend.text      = element_text(size = 14),
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor.y = element_blank()
  )

############################
## Load input data
############################

spa_data_path        <- "../data/spadata.xlsx"
comparison_data_path <- "../data/comparison_data.xlsx"
kurfrequenz_path <- "../data/kurfrequenz_vergleich.xlsx"

spa_data        <- read_excel(spa_data_path)
comparison_data <- read_excel(comparison_data_path)
kurfrequenz_data <- read_excel(kurfrequenz_path)

############################
## Prepare spa visitor data
############################

spa_data <- spa_data %>%
  mutate(
    Date = dmy(Date),
    Year = year(Date)
  )

spa_data_1850s <- spa_data %>%
  filter(Year >= 1850, Year <= 1859)

############################
## Visitor counts by year and gender
############################

visitors_by_year_gender <- spa_data_1850s %>%
  filter(!is.na(Gender)) %>%
  group_by(Year, Gender) %>%
  summarise(
    Visitors = n(),
    .groups  = "drop"
  )

############################
## Top places
############################
top_places <- spa_data_1850s %>%
  filter(!is.na(Place), Place != "Wien") %>%
  group_by(Place) %>%
  summarise(
    Total_Visitors_All_Years = sum(Party, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Visitors_All_Years)) %>%
  slice_head(n = 5)

############################################################
## Plot 1: Total visitors – comparison of data sources
############################################################

# Calculate totals for legend labels
total_official <- sum(comparison_data$OffizielleBesucher, na.rm = TRUE)
total_lists    <- sum(comparison_data$Besucher, na.rm = TRUE)

legend_labels_sources <- c(
  paste0("Official Statistics (", total_official, ")"),
  paste0("Visitor Lists (", total_lists, ")")
)

names(legend_labels_sources) <- c("official", "lists")

# Named color vector for sources
source_colors <- c(
  "Official Statistics" = color_palette$blue,
  "Visitor Lists"       = color_palette$red
)

plot_total_visitors <- ggplot(comparison_data, aes(x = Jahr)) +
  
  geom_vline(
    xintercept = seq(1850, 1859, by = 1),
    color = "gray80",
    linewidth = 0.5
  ) +
  
  geom_line(
    aes(y = OffizielleBesucher, color = "Official Statistics"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = Besucher, color = "Visitor Lists"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = OffizielleBesucher, color = "Official Statistics"),
    size = 2
  ) +
  geom_point(
    aes(y = Besucher, color = "Visitor Lists"),
    size = 2
  ) +
  
  scale_color_manual(
    values = source_colors,
    labels = c(
      paste0("Official Statistics (", total_official, ")"),
      paste0("Visitor Lists (", total_lists, ")")
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(1850, 1859, by = 1),
    labels = number_format(accuracy = 1, big.mark = "")
  ) +
  scale_y_continuous(
    limits = c(6000, 9000),
    breaks = seq(6000, 9000, by = 1000),
    labels = number_format(accuracy = 1, big.mark = "")
  ) +
  
  labs(
    title = "Total Registered Visitors (1850–1859)",
    x     = "Years",
    y     = "Visitors",
    color = "Source"
  ) +
  
  common_theme

print(plot_total_visitors)

############################################################
## Plot 2: Comparison of Spa Visitor Numbers
############################################################

# Reshape from wide to long
kurfrequenz_long <- kurfrequenz_data %>%
  pivot_longer(
    cols = starts_with("18"),  # all columns starting with "18.." are years
    names_to = "Year",
    values_to = "Visitors"
  ) %>%
  mutate(Year = as.numeric(Year))

# Add totals for legend labels
totals_per_city <- kurfrequenz_long %>%
  group_by(Town) %>%
  summarise(Total_Visitors = sum(Visitors, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Visitors))

# Join totals and create legend label
kurfrequenz_long <- kurfrequenz_long %>%
  left_join(totals_per_city, by = "Town") %>%
  filter(Total_Visitors > 0) %>%
  mutate(
    Legend_Label = paste0(Town, " (", Total_Visitors, ")"),
    Legend_Label = factor(Legend_Label,
                          levels = paste0(totals_per_city$Town, " (", totals_per_city$Total_Visitors, ")"))
  )

new_colors <- c(color_palette$blue, color_palette$red, color_palette$lightblue,
                color_palette$purple, color_palette$orange, color_palette$darkgreen)

unique_labels <- unique(kurfrequenz_long$Legend_Label)
colour_map <- setNames(new_colors[seq_along(unique_labels)], unique_labels)

p <- ggplot(
  kurfrequenz_long,
  aes(x = Year, y = Visitors, color = Legend_Label, group = Town)
) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = colour_map) +
  scale_y_continuous(
    limits = c(1500, 9500),
    breaks = seq(1500, 9500, by = 1000)
  ) +
  scale_x_continuous(breaks = 1850:1859) +
  labs(
    title = "Comparison of Visitor Numbers (1850–1859)",
    x = "Years",
    y = "Visitors",
    color = "City"
  ) +
  common_theme +
  theme(
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray80",
      linetype = "dotted"
    )
  )

print(p)

############################################################
## Plot 3: Visitors by gender
############################################################

# Totals for legend
gender_totals <- visitors_by_year_gender %>%
  group_by(Gender) %>%
  summarise(
    Total = sum(Visitors),
    .groups = "drop"
  )

gender_legend_labels <- setNames(
  paste0(gender_totals$Gender, " (", gender_totals$Total, ")"),
  gender_totals$Gender
)

visitors_by_year_gender <- visitors_by_year_gender %>%
  mutate(
    Gender = factor(Gender, levels = c("M", "F"))
  )

plot_gender_visitors <- ggplot(
  visitors_by_year_gender,
  aes(x = Year, y = Visitors, color = Gender, group = Gender)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  scale_color_manual(
    values = c(
      "M" = color_palette$blue,
      "F" = color_palette$red
    ),
    labels = gender_legend_labels
  ) +
  
  scale_x_continuous(breaks = seq(1850, 1859, by = 1)) +
  scale_y_continuous(breaks = seq(900, 1700, by = 100)) +
  
  coord_cartesian(ylim = c(900, 1700)) +
  
  labs(
    title = "Visitors by Gender (1850–1859)",
    x     = "Years",
    y     = "Visitors",
    color = "Gender"
  ) +
  
  common_theme

print(plot_gender_visitors)

############################################################
## Plot 4: Visitors by Month (April–September)
############################################################

monthly_visitors <- spa_data %>%
  filter(!is.na(Date)) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Month = format(Date, "%m")
  ) %>%
  filter(Month %in% sprintf("%02d", 4:9)) %>%
  group_by(Month) %>%
  summarise(Visitors = sum(Party, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Month = factor(
      Month,
      levels = sprintf("%02d", 4:9),
      labels = c("Apr", "Mai", "Jun", "Jul", "Aug", "Sep")
    )
  )

ggplot(monthly_visitors, aes(x = Month, y = Visitors)) +
  geom_bar(stat = "identity", fill = color_palette$navy) +
  labs(
    title = "Total Visitors per Month (April–September, 1850–1859)",
    x = "Months",
    y = "Visitors"
  ) +
  common_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1.5,
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

############################################################
## Plot 5: Monthly distribution (percent of annual total)
############################################################

monthly_distribution <- spa_data %>%
  mutate(
    Year  = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  filter(Month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) %>%
  group_by(Year, Month) %>%
  summarise(
    Total_Visitors = sum(Party, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    Year,
    Month,
    fill = list(Total_Visitors = 0)
  ) %>%
  group_by(Year) %>%
  mutate(
    Year_Total = sum(Total_Visitors),
    Percentage = Total_Visitors / Year_Total * 100
  ) %>%
  ungroup() %>%
  mutate(
    Month = factor(
      Month,
      levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")
    )
  )

month_colors <- c(
  "Apr" = color_palette$red,
  "May" = color_palette$lightblue,
  "Jun" = color_palette$salmon,
  "Jul" = color_palette$navy,
  "Aug" = color_palette$orange,
  "Sep" = color_palette$darkgreen
)

plot_monthly_distribution <- ggplot(
  monthly_distribution,
  aes(x = factor(Year), y = Percentage, fill = Month)
) +
  geom_bar(stat = "identity", width = 0.8) +
  
  scale_fill_manual(
    values = month_colors,
    na.translate = FALSE,
    drop = TRUE
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Monthly Distribution of Visitors by Year (April–September, 1850–1859)",
    x     = "Years",
    y     = "Percentage of Visitors",
    fill  = "Month"
  ) +
  
  common_theme +
  theme(panel.grid.major.x = element_blank())

print(plot_monthly_distribution)

############################################################
## Plot 6: Top 10 countries by visitors (excl. Austria)
############################################################

top_countries_visitors <- spa_data_1850s %>%
  filter(!is.na(Country), Country != "Österreich") %>%
  group_by(Country) %>%
  summarise(
    Total_Visitors = sum(Party, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Visitors)) %>%
  slice_head(n = 10) %>%
  mutate(
    Country = factor(Country, levels = Country)  # preserves descending order
  )

plot_top_countries <- ggplot(
  top_countries_visitors,
  aes(x = Country, y = Total_Visitors)
) +
  geom_col(
    width = 0.7,
    fill  = color_palette$navy
  ) +
  
  scale_y_continuous(
    breaks = seq(0, max(top_countries_visitors$Total_Visitors) + 500, by = 500),
    limits = c(0, max(top_countries_visitors$Total_Visitors))
  ) +
  
  labs(
    title = "Top 10 Countries by Visitors (1850–1859, excl. Austria)",
    x     = "Country",
    y     = "Visitors"
  ) +
  
  common_theme +
  theme(
    axis.text.x = element_text(
      size = 14,
      angle = 45,
      hjust = 1,
      vjust = 1.1
    ),
    panel.grid.major.x = element_blank(),      # no vertical grid
    panel.grid.major.y = element_line(         # horizontal lines every 500
      color = "gray80",
      linetype = "dotted"
    )
  )

print(plot_top_countries)

############################################################
## Plot 7: Top 5 places over time (excl. Vienna)
############################################################

visitors_by_place_year <- spa_data_1850s %>%
  filter(!is.na(Place), Place != "Wien") %>%
  group_by(Place, Year) %>%
  summarise(
    Total_Visitors = sum(Party, na.rm = TRUE),
    .groups = "drop"
  )

# Create a lookup table for legend order (unique!)
legend_levels <- top_places %>%
  arrange(desc(Total_Visitors_All_Years)) %>%
  mutate(
    Legend_Label = paste0(
      Place, " (", Total_Visitors_All_Years, ")"
    )
  ) %>%
  pull(Legend_Label)

visitors_top_places <- visitors_by_place_year %>%
  inner_join(top_places, by = "Place") %>%
  mutate(
    Legend_Label = paste0(
      Place, " (", Total_Visitors_All_Years, ")"
    ),
    Legend_Label = factor(
      Legend_Label,
      levels = legend_levels
    )
  )

plot_top_places_time <- ggplot(
  visitors_top_places,
  aes(
    x     = Year,
    y     = Total_Visitors,
    color = Legend_Label,
    group = Place
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  scale_x_continuous(breaks = seq(1850, 1859, by = 1)) +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, by = 25)
  ) +
  
  scale_color_manual(
    values = c(
      color_palette$blue,
      color_palette$red,
      color_palette$orange,
      color_palette$darkgreen,
      color_palette$purple
    )
  ) +
  
  labs(
    title = "Top 5 Cities by Visitors (1850–1859, excl. Vienna)",
    x     = "Years",
    y     = "Visitors",
    color = "City"
  ) +
  
  common_theme

print(plot_top_places_time)

############################################################
## Plot 8: Visitors by occupation (individual counts)
############################################################

occupation_counts <- spa_data_1850s %>%
  filter(!is.na(Normalised_Categorised_Occupation)) %>%
  count(Normalised_Categorised_Occupation, name = "Individuals") %>%
  arrange(desc(Individuals))

plot_occupation_counts <- ggplot(
  occupation_counts,
  aes(
    x = Individuals,
    y = fct_reorder(
      Normalised_Categorised_Occupation,
      Individuals
    )
  )
) +
  geom_col(fill = color_palette$navy) +
  
  scale_x_continuous(
    breaks = seq(
      0,
      max(occupation_counts$Individuals),
      by = 500
    )
  ) +
  
  labs(
    title = paste0(
      "Visitors by Occupation Category (1850–1859)\n",
      "Total: ",
      sum(occupation_counts$Individuals)
    ),
    x = "Visitors",
    y = "Occupation Category"
  ) +
  
  common_theme +
  theme(
    axis.title.y = element_text(vjust = -1)
  )

print(plot_occupation_counts)

############################################################
## Plot 9: Nobles vs. Commoners
############################################################

nobles_vs_non <- spa_data %>%
  filter(!is.na(Year)) %>%
  mutate(
    Group = factor(
      ifelse(is.na(Rank), "Commoners", "Nobles"),
      levels = c("Nobles", "Commoners")
    )
  ) %>%
  group_by(Year, Group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Year, Group, fill = list(Count = 0)) %>%
  group_by(Year) %>%
  mutate(
    Percentage = (Count / sum(Count)) * 100
  ) %>%
  ungroup()

ggplot(
  nobles_vs_non,
  aes(x = factor(Year), y = Percentage, fill = Group)
) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c(
      "Nobles" = color_palette$orange,
      "Commoners" = color_palette$navy
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Share of Nobles vs. Commoners (1850–1859)",
    x = "Years",
    y = "Percentage of Visitors",
    fill = "Group"
  ) +
  common_theme +
  theme(
    axis.text.x = element_text(vjust = 3.5),
    panel.grid.major.x = element_blank()
  )

############################################################
## Plot 10: Noble Visitors
############################################################
# Prepare data
rank_by_year <- spa_data %>%
  filter(!is.na(Rank), !is.na(Year)) %>%
  group_by(Year, Rank) %>%
  summarise(Count = n(), .groups = "drop")

total_nobles <- rank_by_year %>%
  summarise(Total = sum(Count, na.rm = TRUE)) %>%
  pull(Total)

rank_order_desc <- c(
  "Archduke",
  "Duke",
  "Prince",
  "Count",
  "Baron",
  "Knight",
  "Edler / von"
)

# Totals per rank (for legend labels)
rank_totals <- rank_by_year %>%
  group_by(Rank) %>%
  summarise(Total_AllYears = sum(Count), .groups = "drop")

# Join totals + enforce order
rank_by_year <- rank_by_year %>%
  left_join(rank_totals, by = "Rank") %>%
  mutate(
    Rank = factor(Rank, levels = rank_order_desc),
    Rank_Label = paste0(Rank, " (", Total_AllYears, ")"),
    Rank_Label = factor(
      Rank_Label,
      levels = paste0(
        rank_order_desc, " (",
        rank_totals$Total_AllYears[
          match(rank_order_desc, rank_totals$Rank)
        ],
        ")"
      )
    )
  )

# Color map using global palette
rank_colors <- c(
  "Archduke" = color_palette$red,
  "Duke" = color_palette$yellow,
  "Prince" = color_palette$salmon,
  "Count" = color_palette$orange,
  "Baron" = color_palette$navy,
  "Knight" = color_palette$lightblue,
  "Edler / von" = color_palette$darkgreen
)

rank_color_map <- setNames(
  rank_colors[rank_order_desc],
  levels(rank_by_year$Rank_Label)
)

# Plot
ggplot(
  rank_by_year,
  aes(x = factor(Year), y = Count, fill = Rank_Label)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rank_color_map) +
  scale_y_continuous(
    limits = c(0, 550),
    breaks = seq(0, 550, by = 50)
  ) +
  labs(
    title = "Distribution of Noble Visitors (1850–1859)",
    x = "Years",
    y = "Visitors",
    fill = paste0("Title (Total: ", total_nobles, ")")
  ) +
  common_theme +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(vjust = 3.5)
  )

############################################################
## Plot 11: Gender Differences by Noble Rank (Percentages)
############################################################

# Define hierarchical order of noble ranks (low → high)
rank_order <- c(
  "Edler / von",
  "Knight",
  "Baron",
  "Count",
  "Prince",
  "Duke",
  "Archduke"
)

# Prepare data: count individuals by Rank and Gender
rank_gender_percent <- spa_data %>%
  filter(
    !is.na(Rank),
    !is.na(Gender),
    Rank %in% rank_order
  ) %>%
  group_by(Rank, Gender) %>%
  summarise(
    Individuals = n(),
    .groups = "drop"
  ) %>%
  group_by(Rank) %>%
  mutate(
    Rank_Total = sum(Individuals),
    Percentage = ifelse(
      Rank_Total == 0,
      0,
      (Individuals / Rank_Total) * 100
    )
  ) %>%
  ungroup()

# Apply factor order for ranks
rank_gender_percent <- rank_gender_percent %>%
  mutate(
    Rank = factor(Rank, levels = rank_order)
  )

# Define gender colors
gender_colors <- c(
  "M" = color_palette$navy,
  "F" = color_palette$orange
)

# Plot: stacked percentage bar chart
p <- ggplot(
  rank_gender_percent,
  aes(
    x = Rank,
    y = Percentage,
    fill = Gender
  )
) +
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.7
  ) +
  scale_fill_manual(
    values = gender_colors,
    labels = c("F" = "F", "M" = "M")
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Gender Distribution of Noble Visitors (1850–1859)",
    x = "Titles",
    y = "Percentage of Visitors",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, vjust = 1.5),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted")
  )

print(p)

############################################################
## Plot 12: Visitors by military category (percent per year)
############################################################

military_visitors <- spa_data_1850s %>%
  mutate(
    Military_Group = case_when(
      Person == "Feldwebel abwärts im k. k. Militärbadehaus" ~ "Below Feldwebel",
      Normalised_Categorised_Occupation %in%
        c("Junior Officers", "Senior Officers") ~
        Normalised_Categorised_Occupation,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Military_Group)) %>%
  group_by(Year, Military_Group) %>%
  summarise(
    Total_Visitors = sum(Party, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(Year, Military_Group, fill = list(Total_Visitors = 0)) %>%
  group_by(Year) %>%
  mutate(
    Year_Total = sum(Total_Visitors),
    Percentage = if_else(
      Year_Total == 0,
      0,
      Total_Visitors / Year_Total * 100
    )
  ) %>%
  ungroup()

total_military_visitors <- military_visitors %>%
  summarise(Total = sum(Total_Visitors, na.rm = TRUE)) %>%
  pull(Total)

# Explicit hierarchy: bottom → top of stack
military_levels <- c(
  "Senior Officers",
  "Junior Officers",
  "Below Feldwebel"
)

military_visitors <- military_visitors %>%
  mutate(
    Military_Group = factor(Military_Group, levels = military_levels)
  )

# Totals for legend labels
military_totals <- military_visitors %>%
  group_by(Military_Group) %>%
  summarise(
    Total_All_Years = sum(Total_Visitors),
    .groups = "drop"
  )

military_legend_labels <- setNames(
  paste0(
    military_levels,
    " (",
    military_totals$Total_All_Years[
      match(military_levels, military_totals$Military_Group)
    ],
    ")"
  ),
  military_levels
)

plot_military_distribution <- ggplot(
  military_visitors,
  aes(x = factor(Year), y = Percentage, fill = Military_Group)
) +
  geom_col() +
  
  scale_fill_manual(
    values = c(
      "Senior Officers" = color_palette$red,
      "Junior Officers" = color_palette$orange,
      "Below Feldwebel" = color_palette$navy
    ),
    labels = military_legend_labels
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Distribution of Visitors by Military Category (1850–1859)",
    x     = "Years",
    y     = "Percentage of Visitors",
    fill = paste0(
      "Category (Total: ",
      total_military_visitors,
      ")"
    )
  ) +
  
  common_theme +
  theme(
    panel.grid.major.x = element_blank()
  )

print(plot_military_distribution)

############################################################
## Plot 13: Top 10 by establishment type
############################################################

# Ignore the top 2 addresses and get the next 10
top_addresses_filtered <- spa_data %>%
  filter(!is.na(Address)) %>%
  group_by(Address) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Visitors)) %>%
  filter(!(Address %in% c("Vöslauer Straße 8", "Pelzgasse 32"))) %>%
  slice_head(n = 10)

print(top_addresses_filtered)

# Create mapping table
address_map <- tibble::tibble(
  Address = c(
    "Weilburgstraße 11-13",
    "Scharfeneckweg 10",
    "Johannesgasse 10",
    "Kaiser Franz-Ring 10",
    "Renngasse 27",
    "Antonsgasse 10-12",
    "Grabengasse 17",
    "Renngasse 2",
    "Weilburgstraße 29",
    "Theresiengasse 34"
  ),
  Name = c(
    "Sauerhof",
    "Schloss Weilburg",
    "Todescosches Hospital",
    "Herzoghof",
    "Renngasse 27",
    "Palais Erzherzog Anton",
    "Gasthof zum goldenen Hirsch",
    "Gasthof zum grünen Baum",
    "Marienspital",
    "Theresiengasse 34"
  ),
  Type = c(
    "Hotel",
    "Palace",
    "Charitable Institution",
    "Spa",
    "Private",
    "Private",
    "Inn",
    "Inn",
    "Charitable Institution",
    "Private"
  )
)

# Join mapping with top addresses
top_addresses_categorised <- top_addresses_filtered %>%
  left_join(address_map, by = "Address") %>%
  # If Type is NA, keep original address
  mutate(
    Label = ifelse(is.na(Name), Address, Name),
    Type = ifelse(is.na(Type), "Other", Type)
  )

ggplot(
  top_addresses_categorised,
  aes(
    x = reorder(Label, -Total_Visitors),
    y = Total_Visitors,
    fill = Type,
  )
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "Hotel" = color_palette$navy,
      "Inn" = color_palette$lightblue,
      "Palace" = color_palette$orange,
      "Charitable Institution" = color_palette$darkgreen,
      "Private" = color_palette$salmon,
      "Spa" = color_palette$purple,
      "Other" = "gray50"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 3000),
    breaks = seq(0, 3000, by = 500)
  ) +
  labs(
    title = "Top 10 Establishments (1850–1859, excluding Militärbadehaus & Wohltätigkeitshaus)",
    x = "Establishments",
    y = "Visitors",
    fill = "Type"
  ) +
  common_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1.1
    ),
    plot.title = element_text(hjust = 0.195),
    panel.grid.major.x = element_blank()
  )

############################################################
## Plot 14: Accommodation Types (Percentage Distribution)
############################################################

# Prepare data
accommodation_by_year <- spa_data %>%
  filter(
    !is.na(Year),
    !is.na(Establishment_Type)
  ) %>%
  group_by(Year, Establishment_Type) %>%
  summarise(
    Total_Visitors = sum(Party, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    Year,
    Establishment_Type,
    fill = list(Total_Visitors = 0)
  ) %>%
  group_by(Year) %>%
  mutate(
    Year_Total = sum(Total_Visitors),
    Percentage = if_else(
      Year_Total == 0,
      0,
      Total_Visitors / Year_Total * 100
    )
  ) %>%
  ungroup()

# Totals across all years (for legend labels)
accommodation_totals <- accommodation_by_year %>%
  group_by(Establishment_Type) %>%
  summarise(
    Total_AllYears = sum(Total_Visitors),
    .groups = "drop"
  )

# Total visitors across all years (for legend title)
total_visitors_all <- sum(accommodation_totals$Total_AllYears)

# Order categories
accommodation_order <- accommodation_totals %>%
  arrange((Total_AllYears)) %>%
  pull(Establishment_Type)

accommodation_by_year <- accommodation_by_year %>%
  mutate(
    Establishment_Type = factor(
      Establishment_Type,
      levels = accommodation_order
    )
  )

# Legend labels
legend_labels <- setNames(
  paste0(
    accommodation_order,
    " (",
    accommodation_totals$Total_AllYears[
      match(
        accommodation_order,
        accommodation_totals$Establishment_Type
      )
    ],
    ")"
  ),
  accommodation_order
)

# Named colour map
accommodation_colors <- c(
  "Hotel" = color_palette$navy,
  "Inn" = color_palette$lightblue,
  "Palace" = color_palette$orange,
  "Charitable Institution" = color_palette$darkgreen,
  "Private" = color_palette$salmon,
  "Spa" = color_palette$purple
)


# Plot
plot_accommodation_distribution <- ggplot(
  accommodation_by_year,
  aes(
    x = factor(Year),
    y = Percentage,
    fill = Establishment_Type
  )
) +
  geom_col() +
  
  scale_fill_manual(
    values = accommodation_colors,
    breaks = accommodation_order,
    labels = legend_labels,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Accommodation Types of Visitors (1850–1859)",
    x     = "Years",
    y     = "Percentage of Visitors",
    fill  = paste0(
      "Type (Total: ",
      total_visitors_all,
      ")"
    )
  ) +
  
  common_theme +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 1.5)
  )

print(plot_accommodation_distribution)

############################################################
## Plot 15: Non-listed visitors by Accommodation
############################################################

# Mapping from Person to Category
person_to_category <- c(
  "Untergebracht im Armenhaus errichteten Kinderspital" = "Kinderspital",
  "Untergebracht im Marienspital"                       = "Marienspital",
  "Feldwebel abwärts im k. k. Militärbadehaus"          = "Militärbadehaus",
  "Gefolge"                                             = "Schloss Weilburg",
  "Untergebracht im Todesco'schen Hospital"             = "Todescosches Hospital",
  "Untergebracht im k. k. Wohltätigkeitshaus"           = "Wohltätigkeitshaus"
)

# Create Category first, then filter
occupation_unnamed_subset <- spa_data %>%
  filter(!is.na(Year)) %>%
  mutate(
    Category = ifelse(
      Person %in% names(person_to_category),
      person_to_category[Person],
      NA_character_
    )
  ) %>%
  filter(!is.na(Category)) %>%
  group_by(Year, Category) %>%
  summarise(
    Total_Visitors = sum(Party, na.rm = TRUE),
    .groups = "drop"
  )

total_nonlisted_visitors <- occupation_unnamed_subset %>%
  summarise(Total = sum(Total_Visitors, na.rm = TRUE)) %>%
  pull(Total)


# Totals across all years (for legend labels)
totals_per_category <- occupation_unnamed_subset %>%
  group_by(Category) %>%
  summarise(
    Total_AllYears = sum(Total_Visitors),
    .groups = "drop"
  )

# Join totals + build legend labels
occupation_unnamed_subset <- occupation_unnamed_subset %>%
  left_join(totals_per_category, by = "Category") %>%
  mutate(
    Category_Label = paste0(Category, " (", Total_AllYears, ")")
  )

# Order categories by total visitors (BOTTOM → TOP in stack)
category_order <- totals_per_category %>%
  arrange(Total_AllYears) %>%
  pull(Category)

occupation_unnamed_subset <- occupation_unnamed_subset %>%
  mutate(
    Category = factor(Category, levels = category_order),
    Category_Label = factor(
      Category_Label,
      levels = paste0(
        category_order, " (",
        totals_per_category$Total_AllYears[
          match(category_order, totals_per_category$Category)
        ],
        ")"
      )
    )
  )

# Color map using global palette
category_colors <- c(
  "Kinderspital" = color_palette$red,
  "Marienspital" = color_palette$orange,
  "Militärbadehaus" = color_palette$darkgreen,
  "Schloss Weilburg" = color_palette$salmon,
  "Todescosches Hospital" = color_palette$navy,
  "Wohltätigkeitshaus" = color_palette$lightblue
)

colour_map <- setNames(
  category_colors[category_order],
  levels(occupation_unnamed_subset$Category_Label)
)

# Plot
ggplot(
  occupation_unnamed_subset,
  aes(x = factor(Year), y = Total_Visitors, fill = Category_Label)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colour_map, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 2500),
    breaks = seq(0, 2500, by = 250)
  ) +
  labs(
    title = "Unnamed Visitors by Establishment (1850–1859)",
    x = "Years",
    y = "Visitors",
    fill = paste0(
      "Establishment (Total: ",
      total_nonlisted_visitors,
      ")"
    )
  ) +
  common_theme +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(vjust = 3.5)
  )

############################################################
## Table 1: Average Party Size per Occupation
############################################################

average_party_size_by_occupation <- spa_data %>%
  filter(!is.na(Normalised_Categorised_Occupation)) %>%
  group_by(Normalised_Categorised_Occupation) %>%
  summarise(
    Average_Party_Size = mean(Party, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Average_Party_Size))

print(average_party_size_by_occupation)

############################################################
## Table 2: Top 5 “With” Column
############################################################

top_five_with <- spa_data %>%
  filter(!is.na(With)) %>%
  count(With, sort = TRUE) %>%
  slice_head(n = 5)

print(top_five_with)

############################################################
## Table 3: Visitors per Year
############################################################

visitors_per_year <- spa_data %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop")

print(visitors_per_year)

############################################################
## Table 4: Distribution by Occupation Category (wide table)
############################################################

occupation_table <- spa_data %>%
  filter(!is.na(Normalised_Categorised_Occupation), !is.na(Year)) %>%
  group_by(Year, Normalised_Categorised_Occupation) %>%
  summarise(Total_Visitors = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(
    Year_Total = sum(Total_Visitors, na.rm = TRUE),
    Percentage = round((Total_Visitors / Year_Total) * 100, 2)
  ) %>%
  ungroup() %>%
  select(Year, Normalised_Categorised_Occupation, Percentage) %>%
  pivot_wider(
    names_from = Year,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  arrange(Normalised_Categorised_Occupation)

print(occupation_table)

############################################################
## Share of Officers Staying at Militärbadehaus
############################################################

# Total Junior Officers in dataset
total_junior <- spa_data %>%
  filter(Normalised_Categorised_Occupation == "Junior Officers") %>%
  summarise(Total = sum(Party, na.rm = TRUE)) %>%
  pull(Total)

# Junior Officers at Vöslauer Straße 8
junior_at_address <- spa_data %>%
  filter(
    Normalised_Categorised_Occupation == "Junior Officers",
    Address_2023 == "Vöslauer Straße 8"
  ) %>%
  summarise(Total = sum(Party, na.rm = TRUE)) %>%
  pull(Total)

# Percentage
percentage_junior <- (junior_at_address / total_junior) * 100

# Total Senior Officers in dataset
total_senior <- spa_data %>%
  filter(Normalised_Categorised_Occupation == "Senior Officers") %>%
  summarise(Total = sum(Party, na.rm = TRUE)) %>%
  pull(Total)

senior_at_address <- spa_data %>%
  filter(
    Normalised_Categorised_Occupation == "Senior Officers",
    Address_2023 == "Vöslauer Straße 8"
  ) %>%
  summarise(Total = sum(Party, na.rm = TRUE)) %>%
  pull(Total)

percentage_senior <- (senior_at_address / total_senior) * 100

# Print results
percentage_junior
percentage_senior