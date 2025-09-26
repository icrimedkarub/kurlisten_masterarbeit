# Load necessary libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(tidyr)
library(readr)

# Load the data
file_path <- "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/spadata.xlsx"
data <- read_excel(file_path)

file_path <- "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/comparison_data.xlsx"
comparison_data <- read_excel(file_path)

# Convert the 'Date' column to Date format
data <- data %>%
  mutate(Date = dmy(Date))

# Extract the year from the 'Date' column
data <- data %>%
  mutate(Year = year(Date))

# Filter data for the years 1850-1859
filtered_data <- data %>%
  filter(Year >= 1850 & Year <= 1859)

# Summarize the number of visitors by year and Gender
summary_data <- filtered_data %>%
  group_by(Year, Gender) %>%
  summarise(Visitors = n(), .groups = "drop")

# Filter out rows with NA values in the 'Gender' column and the specified year range
summary_data <- data %>%
  filter(Year >= 1850 & Year <= 1859, !is.na(Gender)) %>%
  group_by(Year, Gender) %>%
  summarise(Visitors = n(), .groups = "drop")

# Define a common theme for consistent text sizes across plots
common_theme <- theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14), # Common x-axis text size
    axis.text.y = element_text(size = 14), # Common y-axis text size
    axis.title.x = element_text(size = 16), # Common x-axis title text size
    axis.title.y = element_text(size = 16), # Common y-axis title text size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Common title text size, bold, centered
    legend.title = element_text(size = 16), # Common legend title size
    legend.text = element_text(size = 14), # Common legend text size
    panel.grid.major.x = element_line(color = "gray80", linetype = "solid"), # Light vertical lines
    panel.grid.minor.x = element_blank(), # Remove minor vertical gridlines
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"), # Subtle horizontal lines
    panel.grid.minor.y = element_blank() # Remove minor horizontal gridlines
  )

################# Plot 1 TOTAL VISITORS
# Calculate totals for each source
total_offizielleBesucher <- sum(comparison_data$OffizielleBesucher, na.rm = TRUE)
total_Besucher <- sum(comparison_data$Besucher, na.rm = TRUE)

# Define legend labels with totals
label_offizielle <- paste0("Official Statistics (", total_offizielleBesucher, ")")
label_Besucher <- paste0("Lists (", total_Besucher, ")")

# Create the plot with updated legend labels
plot <- ggplot(data = comparison_data, aes(x = Jahr)) +
  geom_vline(
    xintercept = seq(1850, 1859, 1),
    color = "gray80",
    linetype = "solid",
    size = 0.5
  ) +
  geom_line(aes(y = OffizielleBesucher, color = label_offizielle), size = 1) +
  geom_line(aes(y = Besucher, color = label_Besucher), size = 1, linetype = "dashed") +
  geom_point(aes(y = OffizielleBesucher, color = label_offizielle), size = 2) +
  geom_point(aes(y = Besucher, color = label_Besucher), size = 2) +
  scale_color_manual(
    values = setNames(c("blue", "red"), c(label_offizielle, label_Besucher))
  ) +
  scale_x_continuous(
    breaks = seq(1850, 1859, 1),
    labels = scales::number_format(accuracy = 1, big.mark = "")
  ) +
  scale_y_continuous(
    limits = c(6000, 9000),
    breaks = seq(6000, 9000, 1000),
    labels = scales::number_format(accuracy = 1, big.mark = "")
  ) +
  labs(
    title = "Total Registered Visitors (1850-1859)",
    x = "Years",
    y = "Visitors",
    color = "Source"
  ) +
  common_theme

# Display the plot
print(plot)

################# Plot 2 VISITORS GENDER
# Calculate total visitors by Gender for the legend
total_visitors <- summary_data %>%
  group_by(Gender) %>%
  summarise(Total = sum(Visitors, na.rm = TRUE)) %>%
  ungroup() # Ensure there is no residual grouping

# Create dynamic labels for the legend including total visitors per sex
legend_labels <- setNames(
  paste0(total_visitors$Gender, " (", total_visitors$Total, ")"),
  total_visitors$Gender
)

# Create the gender balance graph with updated legend
plot <- ggplot(summary_data, aes(x = Year, y = Visitors, color = Gender, group = Gender)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("M" = "blue", "F" = "red"),  # Assign colors
    labels = legend_labels                         # Use dynamic labels with totals
  ) +
  labs(
    title = "Visitors by Gender (1850-1859)",
    x = "Years",
    y = "Visitors",
    color = "Gender"
  ) +
  scale_x_continuous(
    breaks = seq(1850, 1859, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(900, 1700, by = 100)
  ) +
  coord_cartesian(ylim = c(900, 1700)) +
  common_theme

# Display the plot
print(plot)

################# Plot 3 TOTAL IN SEASON PER YEAR
monthly_visitors_by_year <- data %>%
  mutate(
    Month = month(Date, label = TRUE, abbr = TRUE), # Extract month names as abbreviated strings
    Year = year(Date)  # Extract year using lubridate's year() function directly
  ) %>%
  filter(!is.na(Month), Month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) %>% # Filter for high season months
  group_by(Year, Month) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop")

# Check the data structure again
print(monthly_visitors_by_year)
print(head(data$Date))

# Define color scheme for years
colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#b3b3b3", "#a6d854", "#e78ac3", "#e5c494", "#ffd92f", "#1b9e77", "#d95f02")

# Create a bar plot showing monthly visitor numbers by year, with a color scheme for years
plot <- ggplot(monthly_visitors_by_year, aes(x = Month, y = Total_Visitors, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Position bars side by side for each month
  scale_fill_manual(values = colors) +  # Set color scheme for the years
  labs(
    title = "Monthly Visitors by Year (April to September 1850-1859)",
    x = "Months",
    y = "Visitors",
    fill = "Years"
  ) +
  scale_y_continuous(
    limits = c(0, 2550),
    breaks = seq(0, 2500, by = 500)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, vjust = 4.5, margin = margin(t = 10)),  # Adjust text spacing as needed
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18, vjust = 3),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = -3),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),  # Remove major vertical gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical gridlines
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted")  # Keep subtle horizontal lines
  )

# Display the plot
print(plot)

################# Plot 4 TOTAL VISITORS PER COUNTRY EXCL. AUSTRIA
# Calculate total visitors by Country, excluding rows where Country is "Österreich"
top_countries <- data %>%
  filter(Country != "Österreich") %>%
  group_by(Country) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Visitors)) %>%
  slice_head(n = 10) # Get the top 10 countries

# Create a bar plot for the top 10 countries
plot <- ggplot(top_countries, aes(x = reorder(Country, -Total_Visitors), y = Total_Visitors)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.7) +  # Use a single color for all bars
  labs(
    title = "Top 10 Countries (1850-1859, excluding Austria)",
    x = "Countries",
    y = "Visitors"
  ) +
  scale_y_continuous(
    limits = c(0, max(top_countries$Total_Visitors, na.rm = TRUE) + 500),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1.25), # Rotate and size x-axis labels
    axis.text.y = element_text(size = 16), # Larger y-axis text
    axis.title.x = element_text(size = 16, vjust = 2.5), # Larger x-axis title
    axis.title.y = element_text(size = 16), # Larger y-axis title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = -3), # Bold and larger title
    panel.grid.major.x = element_blank(), # Remove vertical gridlines
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted") # Subtle horizontal gridlines
  )

# Display the plot
print(plot)

################# Plot 5 TOTAL VISITORS PER PLACE OVER TIME 5 WITH NUMBERS
# Calculate total visitors per place over all years
places_over_time <- data %>%
  filter(!is.na(Place) & Place != "Wien") %>%
  group_by(Place, Year) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop")

# Identify the top 5 places based on total visitors across all years
top_5_places <- places_over_time %>%
  group_by(Place) %>%
  summarise(Total_Visitors_Sum = sum(Total_Visitors, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Visitors_Sum)) %>%
  slice_head(n = 5) %>%
  pull(Place)

# Filter data to include only the top 5 places
filtered_places_over_time <- places_over_time %>%
  filter(Place %in% top_5_places)

# Calculate total number of visitors for each place for the legend
total_visitors_per_place <- data %>%
  filter(!is.na(Place) & Place != "Wien") %>%
  group_by(Place) %>%
  summarise(Total_Visitors_Sum = sum(Party, na.rm = TRUE), .groups = "drop")

# Join the total visitors data to the filtered places data
filtered_places_over_time <- filtered_places_over_time %>%
  left_join(total_visitors_per_place, by = "Place") %>%
  mutate(
    Legend_Label = paste(Place, " (", Total_Visitors_Sum, ")", sep = "")
  )

# Define a new color palette
new_colors <- c("blue", "red", "lightgreen", "#984ea3", "orange")

# Plot the data
plot <- ggplot(filtered_places_over_time, aes(x = Year, y = Total_Visitors, color = Legend_Label, group = Place)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Top 5 Cities (1850-1859, excluding Vienna)",
    x = "Years",
    y = "Visitors",
    color = "City"
  ) +
  scale_x_continuous(
    breaks = seq(min(filtered_places_over_time$Year, na.rm = TRUE), 
                 max(filtered_places_over_time$Year, na.rm = TRUE), by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 150),                 # set y-axis from 0 to 150
    breaks = seq(0, 150, by = 25)       # tick marks every 25
  ) +
  scale_color_manual(values = new_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_line(color = "gray80", linetype = "solid"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted")
  )

# Display the plot
print(plot)

###################### Plot 6 OCCUPATION BAR PLOT (individuals, not weighted by Party)
# Count number of individuals per occupation (ignoring Party size)
occupation_counts_individuals <- data %>%
  filter(!is.na(Normalised_Categorised_Occupation)) %>%
  group_by(Normalised_Categorised_Occupation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Create the bar plot
plot <- ggplot(occupation_counts_individuals,
               aes(x = Count,
                   y = fct_reorder(Normalised_Categorised_Occupation, Count))) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(
    title = paste("Individual Visitors by Occupation Category 1850-1859\nTotal:", sum(occupation_counts_individuals$Count)),
    x = "Visitors",
    y = "Occupation Category"
  ) +
  scale_x_continuous(
    breaks = seq(0, max(occupation_counts_individuals$Count), by = 500)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18, vjust = -2),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.grid.major.x = element_line(color = "gray80", linetype = "solid"),
    panel.grid.minor.x = element_blank()
  )

# Show the plot
print(plot)


########### Plot 7 Three Military Groups
# Prepare the data
occupation_visitors <- data %>%
  filter(Normalised_Categorised_Occupation %in% c("Below Feldwebel", "Junior Officers", "Senior Officers")) %>%
  group_by(Year, Normalised_Categorised_Occupation) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop") %>%
  complete(Year, Normalised_Categorised_Occupation, fill = list(Total_Visitors = 0)) %>%
  group_by(Year) %>%
  mutate(
    Year_Total = sum(Total_Visitors, na.rm = TRUE),
    Percentage = ifelse(Year_Total == 0, 0, (Total_Visitors / Year_Total) * 100)
  ) %>%
  ungroup()

# Compute totals across all years
totals <- occupation_visitors %>%
  group_by(Normalised_Categorised_Occupation) %>%
  summarise(Total_AllYears = sum(Total_Visitors, na.rm = TRUE), .groups = "drop")

# Order categories by total (largest at top of stack)
occupation_order <- totals %>%
  arrange(desc(Total_AllYears)) %>%
  pull(Normalised_Categorised_Occupation)

occupation_visitors <- occupation_visitors %>%
  mutate(
    Normalised_Categorised_Occupation = factor(Normalised_Categorised_Occupation, levels = occupation_order),
    Category_Label = paste0(Normalised_Categorised_Occupation, " (", totals$Total_AllYears[match(Normalised_Categorised_Occupation, totals$Normalised_Categorised_Occupation)], ")")
  )

# Define colors
colors <- c(
  "Senior Officers" = "darkred",
  "Junior Officers" = "#E0CA3C",
  "Below Feldwebel" = "#2D3047"
)

# Plot
ggplot(occupation_visitors, aes(x = as.factor(Year), y = Percentage,
                                fill = Normalised_Categorised_Occupation)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = colors,
    labels = paste0(levels(occupation_visitors$Normalised_Categorised_Occupation),
                    " (",
                    totals$Total_AllYears[match(
                      levels(occupation_visitors$Normalised_Categorised_Occupation),
                      totals$Normalised_Categorised_Occupation
                    )],
                    ")")
  ) +
  labs(
    title = "Distribution of Visitors by Military Category (1850-1859)",
    x = "Years",
    y = "Percentage of Visitors",
    fill = "Category"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.02))  # small margin above 100%
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, vjust = 1),
    axis.title.y = element_text(size = 16, vjust = -2, margin = margin(r = 15)),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80")
  )

############## Plot 8 Non-listed Visitors
# Filter and prepare the data
occupation_unnamed_subset <- data %>%
  filter(Address %in% c("Marienspital", "Kinderspital",
                                                  "Todescosches Hospital", "Wohltätigkeitshaus",
                                                  "Schloss Weilburg", "Militärbadehaus"),
         !is.na(Year)) %>%
  group_by(Year, Address) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop")

# Compute totals per category across all years
totals_per_category <- occupation_unnamed_subset %>%
  group_by(Address) %>%
  summarise(Total_AllYears = sum(Total_Visitors, na.rm = TRUE), .groups = "drop")

# Join totals back and create labels for the legend
occupation_unnamed_subset <- occupation_unnamed_subset %>%
  left_join(totals_per_category, by = "Address") %>%
  mutate(Category_Label = paste0(Address, " (", Total_AllYears, ")"))

# Define colours for the six categories
chosen_colours <- c("#F4E04D", "#2D3047", "darkgreen", "orange", "darkblue", "darkred")

# Create a named colour map
unique_labels <- unique(occupation_unnamed_subset$Category_Label)
colour_map <- setNames(chosen_colours[seq_along(unique_labels)], unique_labels)

# Plot (stacked version)
p <- ggplot(occupation_unnamed_subset, aes(x = factor(Year), y = Total_Visitors, fill = Category_Label)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colour_map) +
  scale_y_continuous(
    limits = c(0, 2500),
    breaks = seq(0, 2500, by = 250)
  ) +
  labs(
    title = "Non-listed Visitors by Place (1850-1859)",
    x = "Years",
    y = "Visitors",
    fill = "Place"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, margin = margin(t = -15)),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18, vjust = 1), # shift y title slightly left
    plot.title = element_text(size = 18, face = "bold", vjust = -1, hjust = 1),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80")
  )

# Print the plot
print(p)


############ Plot 9 Accommodationby Type
# Ignore the top 2 addresses and get the next 10
top_addresses_filtered <- data %>%
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
    "Hotel",
    "Private",
    "Palace",
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
# Define colors for each type
type_colors <- c(
  "Hotel" = "darkblue", 
  "Inn" = "darkgreen",    
  "Palace" = "#E0CA3C",
  "Charitable Institution" = "darkred",
  "Private" = "#2D3047",
  "Other" = "gray50"
)

# Plot with colors by Type
p <- ggplot(top_addresses_categorised, aes(x = reorder(Label, -Total_Visitors), y = Total_Visitors, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = type_colors) +
  labs(
    title = "Top 10 Establishments (1850-1859, excluding Militärbadehaus & Wohltätigkeitshaus)",
    x = "Establishments",
    y = "Visitors",
    fill = "Type"
  ) +
  scale_y_continuous(
    breaks = seq(0, 3000, by = 500),
    limits = c(0, 3000)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = -1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted")
  )

print(p)

################## Plot 10 Noble Ranks
# Prepare the data ----
rank_by_year <- data %>%
  filter(!is.na(Rank), !is.na(Year)) %>%
  group_by(Year, Rank) %>%
  summarise(Count = n(), .groups = "drop")

# Compute total per Rank across all years ----
rank_totals <- rank_by_year %>%
  group_by(Rank) %>%
  summarise(Total_AllYears = sum(Count, na.rm = TRUE), .groups = "drop")

# Join totals and build legend labels ----
rank_by_year <- rank_by_year %>%
  left_join(rank_totals, by = "Rank") %>%
  mutate(Rank_Label = paste0(Rank, " (", Total_AllYears, ")"))

# Define colors (extend if more ranks exist) ----
chosen_colours <- c("red", "lightblue", "orange", "#2D3047", "darkblue",
                    "darkgreen", "yellow", "#666666")

unique_labels <- unique(rank_by_year$Rank_Label)
colour_map <- setNames(chosen_colours[seq_along(unique_labels)], unique_labels)

# Plot ----
p <- ggplot(rank_by_year, aes(x = factor(Year), y = Count, fill = Rank_Label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colour_map) +
  scale_y_continuous(
    limits = c(0, 550),                # y-axis from 0 to 550
    breaks = seq(0, 550, by = 50)      # tick marks every 50
  ) +
  labs(
    title = "Distribution of Noble Visitors (1850-1859)",
    x = "Years",
    y = "Visitors",
    fill = "Title"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, vjust = 2.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(p)


######## Plot 11 Nobles vs Commoners
# Prepare the data ----
nobles_vs_non <- data %>%
  filter(!is.na(Year)) %>%
  mutate(
    Group = ifelse(is.na(Rank), "Commoners", "Nobles")
  ) %>%
  group_by(Year, Group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Year, Group, fill = list(Count = 0)) %>%  # ensure both groups are present every year
  group_by(Year) %>%
  mutate(
    Year_Total = sum(Count),
    Percentage = (Count / Year_Total) * 100
  ) %>%
  ungroup()

# Plot ----
p <- ggplot(nobles_vs_non, aes(x = factor(Year), y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(
    limits = c(0, 101),
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_manual(values = c("Nobles" = "#E0CA3C", "Commoners" = "#2D3047")) +
  labs(
    title = "Share of Nobles vs. Commoners (1850–1859)",
    x = "Years",
    y = "Percentage of Visitors",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, vjust = 2.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(p)

####### Plot 12 Visitors by Months
# Convert the Date column to Date format and extract Year and Month
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$Year <- format(data$Date, "%Y")
data$Month <- format(data$Date, "%m")

# Summarize the total number of visitors by Year
yearly_visitors <- data %>%
  group_by(Year) %>%
  summarize(TotalVisitors = sum(Party, na.rm = TRUE)) %>%
  ungroup()

# Filter the data to include only months from April to September
data_filtered <- data %>%
  filter(Month %in% sprintf("%02d", 4:9))

# Summarize the number of visitors by Year and Month
monthly_visitors_filtered <- data_filtered %>%
  group_by(Year, Month) %>%
  summarize(Visitors = sum(Party, na.rm = TRUE)) %>%
  ungroup()

# Convert Year and Month to factors for plotting
monthly_visitors_filtered$Year <- factor(monthly_visitors_filtered$Year)
monthly_visitors_filtered$Month <- factor(monthly_visitors_filtered$Month, levels = sprintf("%02d", 4:9))

# BAR PLOT SUBSETTED MONTHS
# Summarize the number of visitors by Month
monthly_visitors_aggregated <- data_filtered %>%
  group_by(Month) %>%
  summarize(Visitors = sum(Party, na.rm = TRUE)) %>%
  ungroup()

# Convert Month to a factor for correct ordering in the plot
monthly_visitors_aggregated$Month <- factor(monthly_visitors_aggregated$Month, levels = sprintf("%02d", 4:9), labels = c("Apr", "Mai", "Jun", "Jul", "Aug", "Sep"))

# Create a regular bar plot
ggplot(data = monthly_visitors_aggregated, aes(x = Month, y = Visitors)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Total Visitors per Month (April to September 1850-1859)",
       x = "Months", y = "Visitors") +
  scale_x_discrete(labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2, size = 14), # Move x-axis labels up a bit, larger text
        axis.text.y = element_text(size = 14), # Larger y-axis labels text
        axis.title.x = element_text(vjust = 1.5, size = 16), # Move x-axis title up a bit, larger text
        axis.title.y = element_text(size = 16), # Larger y-axis title text
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", vjust = -1.5, size = 18)) # Move title down a bit, larger text

###### Plot 13 Comparison of Spa Visitor Numbers
# Read Excel File
kurfrequenz <- read_excel("/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/kurfrequenz_vergleich.xlsx")

# Reshape from wide to long ----
kurfrequenz_long <- kurfrequenz %>%
  pivot_longer(
    cols = starts_with("18"),         # all columns starting with "18.." are years
    names_to = "Year",
    values_to = "Visitors"
  ) %>%
  mutate(
    Year = as.numeric(Year)           # make sure Year is numeric
  )

# Add totals for legend labels ----
totals_per_city <- kurfrequenz_long %>%
  group_by(Town) %>%
  summarise(Total_Visitors = sum(Visitors, na.rm = TRUE), .groups = "drop")

kurfrequenz_long <- kurfrequenz_long %>%
  left_join(totals_per_city, by = "Town") %>%
  filter(Total_Visitors > 0) %>%  # 🔹 remove NA/0 totals
  mutate(Legend_Label = paste0(Town, " (", Total_Visitors, ")"))

# Define colors ----
six_colors <- c("blue", "red", "lightblue", "#984ea3", "orange", "lightgreen")
unique_labels <- unique(kurfrequenz_long$Legend_Label)
colour_map <- setNames(new_colors[seq_along(unique_labels)], unique_labels)

# Plot ----
p <- ggplot(kurfrequenz_long, aes(x = Year, y = Visitors, color = Legend_Label, group = Town)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = colour_map) +
  scale_y_continuous(
    limits = c(1500, 9500),
    breaks = seq(1500, 9500, by = 1000)
  ) +
  scale_x_continuous(
    breaks = seq(1850, 1859, by = 1)
  ) +
  labs(
    title = "Comparison of Visitor Numbers (1850–1859)",
    x = "Years",
    y = "Visitors",
    color = "City"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted")
  )

print(p)

###################### TABLE 1 AVERAGE PARTY SIZE PER CATEGORY
# Calculate and sort the average party size for each occupation category
average_party_size_by_occupation <- data %>%
  filter(!is.na(Normalised_Categorised_Occupation)) %>%  # Exclude rows where the occupation category is NA
  group_by(Normalised_Categorised_Occupation) %>%
  summarise(
    Average_Party_Size = mean(Party, na.rm = TRUE),  # Calculate mean, ignoring NA values in 'Party'
    .groups = 'drop'  # Remove grouping structure from the resulting tibble
  ) %>%
  arrange(desc(Average_Party_Size))  # Order the results by average party size in descending order

# View and export the results
print(average_party_size_by_occupation)
write_csv(average_party_size_by_occupation, "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/average_party_size_by_occupation.csv")

###################### TABLE 2 TOP 5 WITH COLUMN
# Calculate the frequency of each entry in the 'With' column and find the top 5
top_five_with <- data %>%
  filter(!is.na(With)) %>%  # Exclude NA values to focus on valid entries
  count(With, sort = TRUE) %>%  # Count occurrences and sort in descending order
  top_n(5, n)  # Select the top 5 entries

# View the results
print(top_five_with)

###### TABLE 3 visitors per year
# Total visitors per year (with Party size taken into account)
visitors_per_year <- data %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(Total_Visitors = sum(Party, na.rm = TRUE), .groups = "drop")

print(visitors_per_year)
write.csv(visitors_per_year, "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/visitors_per_year.csv", row.names = FALSE)

################# TABLE 4 Distribution by Occupation Category
# Create a wide-format table with percentages per occupation per year (individual-based, no Party weighting)
occupation_table <- data %>%
  filter(!is.na(Normalised_Categorised_Occupation), !is.na(Year)) %>%
  group_by(Year, Normalised_Categorised_Occupation) %>%
  summarise(Total_Visitors = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Year_Total = sum(Total_Visitors, na.rm = TRUE),
         Percentage = round((Total_Visitors / Year_Total) * 100, 2)) %>%
  ungroup() %>%
  select(Year, Normalised_Categorised_Occupation, Percentage) %>%
  pivot_wider(
    names_from = Year,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  arrange(Normalised_Categorised_Occupation)

# View the table
print(occupation_table)

# Optional: show as a nice viewer in RStudio
View(occupation_table)

# Export
# --- Counts table ---
occupation_counts_table <- data %>%
  filter(!is.na(Normalised_Categorised_Occupation), !is.na(Year)) %>%
  group_by(Year, Normalised_Categorised_Occupation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Count, values_fill = 0)

# --- Percentages table (already done, but keeping consistent) ---
occupation_percent_table <- data %>%
  filter(!is.na(Normalised_Categorised_Occupation), !is.na(Year)) %>%
  group_by(Year, Normalised_Categorised_Occupation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup() %>%
  select(Year, Normalised_Categorised_Occupation, Percentage) %>%
  pivot_wider(names_from = Year, values_from = Percentage, values_fill = 0)

# --- Combined table (Count + %) ---
occupation_combined_table <- data %>%
  filter(!is.na(Normalised_Categorised_Occupation), !is.na(Year)) %>%
  group_by(Year, Normalised_Categorised_Occupation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup() %>%
  mutate(Count_Percent = paste0(Count, " (", round(Percentage, 1), "%)")) %>%
  select(Year, Normalised_Categorised_Occupation, Count_Percent) %>%
  pivot_wider(names_from = Year, values_from = Count_Percent, values_fill = "")

# --- Export all three ---
write_csv(occupation_counts_table, "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/occupation_counts_by_year2.csv")
write_csv(occupation_percent_table, "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/occupation_percentages_by_year2.csv")
write_csv(occupation_combined_table, "/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/occupation_counts_percentages_by_year2.csv")
