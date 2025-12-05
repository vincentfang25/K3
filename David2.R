library(tidyverse)
library(dplyr)
library(purrr)
library(broom)
library(reshape2)
df_class <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/Classification_by_Income.csv")
df_gdp <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/GDP_Per_Capita.csv")
df_cont <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/continents-according-to-our-world-in-data.csv")
df_pop <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/Population_Size.csv")

#Cleaning Population file
new_columns <-  2010:2024
old_columns <- paste0("...", 53:67)
df_pop <- df_pop %>% rename_with(~as.character(new_columns), .cols = all_of(old_columns))
df_pop <- df_pop %>% select(c("World Development Indicators", as.character(2010:2024))) %>%
  filter(`World Development Indicators` %in% df_cont$Code)
df_pop <- df_pop[rowSums(is.na(df_pop)) < 2,] %>% rename("Code" = "World Development Indicators")






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
  mutate(Annual_Growth_Rate_PC = ((`GDP_Per_Capita` - lag(`GDP_Per_Capita`)) / lag(`GDP_Per_Capita`)) * 100,
         Growth_Factor = log((`GDP_Per_Capita` / lag(`GDP_Per_Capita`)))) %>%
  ungroup()
df_gdp <- left_join(df_gdp, df_cont[,c(2,4)], by = "Code")
df_gdp <- left_join(df_gdp, df_class[, c(2,4)], by = "Code")
df_gdp$Year <- as.numeric(df_gdp$Year)


trend_results <- df_gdp %>%
  group_by(Code) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(log(GDP_Per_Capita) ~ Year, data = .x)),
         results = map(model, tidy)) %>%
  unnest(results) %>%
  filter(term == "Year") %>%
  select(Code, beta = estimate, p_value = p.value)


df_main <- df_gdp %>%
  select(c(2,5,6)) %>%
  group_by(Code) %>%
  summarise(`Geometric Growth Average` = exp(mean(Growth_Factor, na.rm = TRUE)),
            `Standard Deviation` = sd(Annual_Growth_Rate_PC, na.rm = TRUE),
            `Coefficient of Variation` = sd(Annual_Growth_Rate_PC, na.rm = TRUE) / abs(mean(Annual_Growth_Rate_PC, na.rm = TRUE))) 
df_main <- left_join(df_main, data_cont[,-3], by = "Code")
df_main <- left_join(df_main, df_class[, c(2,4)], by = "Code")
df_main <- left_join(df_main, trend_results, by = "Code")


# (ONE) This plot shows the geometric growth rate against the volatility of this growth by continent
ggplot(data = df_main[df_main$`Coefficient of Variation` < 100, ]) + geom_point(mapping = aes(x = `Geometric Growth Average`, y = `Coefficient of Variation`, color = Continent)) +
  scale_x_continuous(n.breaks = 10) + facet_wrap(~ Continent) + labs(title = "Coefficient of Variation against Geometric Mean") +
  theme_minimal() 

# (TWO) This is another graph I don't completely understand
ggplot(df_main[df_main$beta < 2.0,], aes(x = beta, y = `Geometric Growth Average`,
                                         colour = Continent,
                                         shape = as.vector(
  df_main[df_main$beta < 2.0,]$`Income group`== "Low income" | df_main[df_main$beta < 2.0,]$`Income group` == "Lower middle income"),
  size= as.vector(
    df_main[df_main$beta < 2.0,]$`Income group`== "Low income" | df_main[df_main$beta < 2.0,]$`Income group` == "Lower middle income"))) +
  geom_point(size = 2, alpha = 0.7, show.legend = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth=0.5) +
  geom_hline(yintercept = 1.07, linetype = 2, linewidth=0.5) +
  labs(
    x = "Trend in growth (β)",
    y = "Mean growth",
    title = "Long-run Growth Trend vs Average Growth",
    shape = "Low Income") +
  scale_x_continuous(n.breaks = 15) +
  scale_shape_manual(values = c("TRUE" = 17,
                                "FALSE" = 16)) +
  scale_size_manual(values = c("TRUE" = 4,
                               "FALSE" = 2)) +
  theme_minimal(base_size = 14)


#This is a different graph

ggplot(data = df_gdp) + geom_point(mapping = aes(x = Year, y = `Annual_Growth_Rate_PC`, color = Continent)) +
  facet_wrap( ~ Continent) +
  labs(title = "Annual Volatility of Growth Over Time",
       y= "Annual Change in GDP Per Capita (%)") + theme_minimal(base_size = 14)



#This is another graph
ggplot(data = df_gdp[df_gdp$`Income group` == "Low income" | df_gdp$`Income group` == "Lower middle income",]) +
  geom_line(mapping = aes(x = Year, y = Annual_Growth_Rate_PC, color = "Code"))


#Plot I constructed
ggplot(data = filter(df_gdp, `Income group` == "Lower middle income" | `Income group` == "Low income"),
       aes(x = Year, y = Annual_Growth_Rate_PC, colour = `Data Source`)) +
  geom_line(alpha = 0.5, show.legend = FALSE) +      # line for each country
  geom_point(alpha = 0.7, size = 1.5, show.legend = FALSE) +  # points for each year
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotdash", colour = "black", linewidth = 0.5) +
  labs(title = "Annual GDP Growth by Continent",
       subtitle = "Coloured by Country",
       y = "GDP Growth (%)",
       x = "Year") +
  facet_wrap(~ Continent) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  theme_minimal()


#More plots from the plot I constructed
continent_avg <- df_gdp %>%
  group_by(Continent, Year) %>%
  summarise(
    Average_Growth = mean(Annual_Growth_Rate_PC, na.rm = TRUE),
    n_countries = n()
  ) %>%
  ungroup()

ggplot(continent_avg, aes(x = Year, y = Average_Growth, colour = Continent)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black") +
  labs(
    title = "Average Annual GDP Growth by Continent",
    y = "Average GDP Growth (%)",
    x = "Year"
  ) +
  theme_minimal()
 
#Percentage of the time period for which average growth is above 7%
continent_share_above7 <- continent_avg %>%
  mutate(above7 = Average_Growth >= 7) %>%
  group_by(Continent) %>%
  summarise(share_above7 = mean(above7, na.rm = TRUE))
x




