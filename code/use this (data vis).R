library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

file_path <- "clean data vis data.xlsx"

load_sheets <- function(type = c("births", "deaths")) {
  type <- match.arg(type)
  
  years <- 2018:2023
  sheet_names <- paste(years, type)    # "2018 births", "2018 deaths", etc.
  
  data_list <- lapply(seq_along(sheet_names), function(i) 
    {df <- read_excel(file_path, sheet = sheet_names[i])
    df$Year <- years[i]
    df$Metric <- ifelse(type == "births", "Births", "Deaths")
    df})
  
  bind_rows(data_list)
}

births_all <- load_sheets("births")
deaths_all <- load_sheets("deaths")

clean_region <- function(x) trimws(tolower(x))

births_all <- births_all %>% mutate(REGION = clean_region(REGION))
deaths_all <- deaths_all %>% mutate(REGION = clean_region(REGION))

target_regions <- tolower(c(
  "Yorkshire and the Humber",
  "North East",
  "West Midlands"
))

births_all <- births_all %>% filter(REGION %in% target_regions)
deaths_all <- deaths_all %>% filter(REGION %in% target_regions)

biz_all <- bind_rows(births_all, deaths_all)

stopifnot(nrow(biz_all) > 0)
stopifnot(all(c("Year", "COUNT", "Metric", "REGION") %in% colnames(biz_all)))

ggplot(biz_all, aes(Year, COUNT, colour = Metric, linetype = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  facet_wrap(~ REGION, scales = "free_y") +
  scale_x_continuous(breaks = 2018:2023) +
  scale_colour_manual(values = c("Births" = "#1b9e77", "Deaths" = "#d95f02")) +
  labs(
    title = "Business Births and Deaths (2018–2023)",
    subtitle = "Trends across Yorkshire & Humber, North East and West Midlands",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

# Plot 2: Clustered Bars
ggplot(biz_all, aes(factor(Year), COUNT, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ REGION) +
  scale_fill_manual(values = c("Births" = "#1b9e77", "Deaths" = "#d95f02")) +
  labs(
    title = "Business Births vs Deaths by Year",
    subtitle = "Comparison of yearly births and deaths in each region",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

#  Plot 3: Net Growth 
net_change <- biz_all %>%
  pivot_wider(names_from = Metric, values_from = COUNT) %>%
  mutate(Net_Growth = Births - Deaths)

ggplot(net_change, aes(Year, Net_Growth, colour = REGION)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Net Business Growth (Births – Deaths)",
    subtitle = "Positive = net increase; Negative = net decline",
    x = "Year",
    y = "Net Growth"
  ) +
  theme_minimal(base_size = 13)

#  Plot 4: Heatmap 
heatmap_data <- biz_all %>%
  mutate(Metric = factor(Metric, levels = c("Births", "Deaths")))

ggplot(heatmap_data, aes(Year, REGION, fill = COUNT)) +
  geom_tile() +
  scale_fill_gradient(low = "#e0f3db", high = "#0868ac") +
  facet_wrap(~ Metric) +
  labs(
    title = "Heatmap of Business Births and Deaths",
    subtitle = "Darker shading = higher values",
    x = "Year",
    y = "Region",
    fill = "Count"
  ) +
  theme_minimal(base_size = 13)
