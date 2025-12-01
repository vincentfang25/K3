library(tidyverse)
library(dplyr)
library(purrr)
library(broom)
df_class <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/Classification_by_Income.csv")
df_gdp <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/GDP_Per_Capita.csv")
df_cont <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/continents-according-to-our-world-in-data.csv")

#Cleaning GDP Per Capita File

new_columns <-  2010:2024
old_columns <- paste0("...", 54:68)
df_gdp <- df_gdp %>% rename_with(~as.character(new_columns), .cols = all_of(old_columns))
df_gdp <- df_gdp %>% select(c("Data Source", "World Development Indicators", as.character(2010:2024))) %>%
  filter(`World Development Indicators` %in% df_cont$Code)
df_gdp <- df_gdp[rowSums(is.na(df_gdp)) < 2,] %>% rename("Code" = "World Development Indicators")

df_gdp <- df_gdp %>% gather(, key="Year", value="GDP_Per_Capita", as.character(2010:2024)) %>% 
  arrange(Code)

df_gdp <- df_gdp %>% group_by(Code) %>%
  mutate(Annual_Growth_Rate_PC = ((`GDP_Per_Capita` - lag(`GDP_Per_Capita`)) / lag(`GDP_Per_Capita`)) * 100) %>%
  ungroup()

df_gdp$Year <- as.numeric(df_gdp$Year)


trend_results <- df_gdp %>%
  group_by(Code) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(Annual_Growth_Rate_PC ~ Year, data = .x)),
         results = map(model, tidy)) %>%
  unnest(results) %>%
  filter(term == "Year") %>%
  select(Code, beta = estimate, p_value = p.value)


df_main <- df_gdp %>%
  select(c(2,5)) %>%
  group_by(Code) %>%
  summarise(`Growth Average` = mean(Annual_Growth_Rate_PC, na.rm = TRUE),
            `Standard Deviation` = sd(Annual_Growth_Rate_PC, na.rm = TRUE),
            `Coefficient of Variation` = sd(Annual_Growth_Rate_PC, na.rm = TRUE) / abs(mean(Annual_Growth_Rate_PC))) 
df_main <- left_join(df_main, data_cont[,-3], by = "Code")
df_main <- left_join(df_main, trend_results, by = "Code")

ggplot(data = df_main) + geom_point(mapping = aes(x = `Growth_Average`, y = `Standard Deviation`, color = Continent))

