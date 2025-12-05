install.packages("countrycode")
install.packages("magrittr")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(countrycode)
library(stringr)
library(ggthemes)
library(magrittr)

setwd("~/Documents/Imperial/Data science /data sets")
data1 <- read_csv("continents-according-to-our-world-in-data.csv")
data <- read.csv("youth-not-in-education-employment-training.csv")

allowed <- c("Asia","Europe","North America","South America","Africa","Oceania","Antarctica")
df <- data1 %>%
  mutate(Continent = ifelse(Continent %in% allowed, Continent, "Unknown"))

continent_order <- c("Asia","Europe","North America","South America","Africa","Oceania")
df <- data1 %>%
  mutate(Continent = factor(Continent, levels = continent_order)) %>%
  arrange(Continent,Entity,Year)

df <- data1 %>%
  filter(!Continent %in% c("Antarctica","Unknown")) %>%
  mutate(Continent = factor(Continent, levels = continent_order)) %>%
  arrange(Continent, Entity, Year)

df_sorted <- data1 %>% arrange(Continent, Entity, Year)

write_csv(df_sorted, "sorted_by_continent.csv")
View(df_sorted)


library(readr)

old <- read_csv("sorted_by_continent.csv")   
new <- read_csv("youth-not-in-education-employment-training.csv")     

map <- old %>%
  mutate(
    Entity   = str_trim(Entity),
    Code     = str_trim(Code),
    Continent= str_trim(Continent)
  ) %>%
  distinct(Code, Continent)

new2 <- new %>%
  mutate(
    Entity = str_trim(Entity),
    Code   = str_trim(Code)
  ) %>%
  left_join(map, by = "Code")

overrides <- c(
  "Kosovo" = "Europe",
  "Palestine" = "Asia",
  "Hong Kong" = "Asia",
  "Macao" = "Asia",
  "Curacao" = "North America",
  "Channel Islands" = "Europe",
  
  "EU (27)" = "Europe",
  "Europe and Central Asia" = "Europe",
  "North America (WB)" = "North America",
  "South Asia (WB)" = "Asia",
  "Latin America and Caribbean" = "South America"  # or "Americas" if you prefer 5-continent
)

new2 <- new2 %>%
  mutate(Continent = ifelse(is.na(Continent) & Entity %in% names(overrides),
                            overrides[Entity], Continent))

new2 <- new2 %>%
  filter(!Continent %in% c("Antarctica", "Unknown"))

regional_patterns <- c("\\(WB\\)", "^EU \\(\\d+\\)$", "Europe and Central Asia", 
                       "^Latin America", "^North America$", "^South Asia$")
new2 <- new2 %>%
  filter(!str_detect(Entity, paste(regional_patterns, collapse = "|")))

continent_order <- c("Asia","Europe","North America","South America","Africa","Oceania")

df_sorted2 <- new2 %>%
  mutate(Continent = factor(Continent, levels = continent_order)) %>%
  arrange(Continent, Entity, Year) %>%
  
  rename(NEET = `Share of youth not in education, employment or training, total (% of youth population)`)

head(df_sorted2, 20)
write_csv(df_sorted2, "df_sorted_neet.csv")
view(df_sorted2)

agg_patterns <- c("\\(WB\\)", "^EU \\(\\d+\\)$", "Europe and Central Asia", "^Latin America")
neet_clean <- df_sorted2 %>%
  filter(!str_detect(Entity, paste(agg_patterns, collapse = "|")))

summ <- neet_clean %>%
  group_by(Entity, Code) %>%
  arrange(Year,.by_group = TRUE) %>%
  summarise(
    baseline_year = {
      cand <- Year[Year >= 2000 & !is.na(NEET)]
      if (length(cand)) min(cand) else min(Year[!is.na(NEET)])
    },
    baseline_value = {
      y <- NEET[Year == baseline_year]
      if (length(y)) y[1] else NA_real_
    },
    
    target_year = {
      if (any(Year == 2020 & !is.na(NEET))) 2020L else {
        cand_le <- Year[Year <= 2020 & !is.na(NEET)]
        if (length(cand_le)) max(cand_le) else {
          cand_gt <- Year[Year > 2020 & !is.na(NEET)]
          if (length(cand_gt)) min(cand_gt) else NA_integer_
        }
      }
    },
    target_value = {
      y <- NEET[Year == target_year]
      if (length(y)) y[1] else NA_real_
    },
    n_years = sum(!is.na(NEET)),
    slope = if (n_years >= 3) coef(lm(NEET ~ Year))[["Year"]] else NA_real_,.groups = "drop"
  )

neet_clean %>% filter(Code == "ARE") %>% arrange(Year)

neet_clean %>%
  dplyr::filter(Code == "ARE") %>%
  dplyr::summarise(has_pre_2020 = any(Year <= 2020, na.rm = TRUE))
uae <- neet_clean %>% dplyr::filter(Code == "ARE")
any(uae$Year <= 2020, na.rm = TRUE)

neet_clean %>%
  dplyr::filter(Code == "ARE") %>%
  { any(.$Year <= 2020, na.rm = TRUE) }
library(magrittr)
neet_clean %>% dplyr::filter(Code == "ARE") %$% any(Year <= 2020, na.rm = TRUE)
with(subset(neet_clean, Code == "ARE"), any(Year <= 2020, na.rm = TRUE))

neet_clean %>%
  dplyr::group_by(Entity, Code) %>%
  dplyr::summarise(
    has_pre_2020 = any(Year <= 2020, na.rm = TRUE),
    first_year   = min(Year, na.rm = TRUE),
    last_year    = max(Year, na.rm = TRUE),.groups = "drop"
  ) %>%
  dplyr::arrange(!has_pre_2020, Entity)

any(neet_clean$Year <= 2020, na.rm = TRUE)
names(neet_clean)
which(names(neet_clean) == "Year")
sQuote(names(neet_clean))
any("Year"<= 2020, na.rm = TRUE) 
head(summ, 20)
View(summ)                   
View(dplyr::filter(summ, substantial))

