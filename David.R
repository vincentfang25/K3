install.packages("FactoMineR")
install.packages("corrr")
install.packages("ggcorrplot")
install.packages("factoextra")
install.packages("plotly")
library(tidyverse)
library(mice)
library(FactoMineR)
library(ggcorrplot)
library(factoextra)
library(corrr)
library(plotly)
library(ggplot2)

data_yu <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/youth-not-in-education-employment-training.csv")
data_lp <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/Labor_Participation.csv")
data_ps <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/Population_Size.csv")
data_fm <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/female_male_ratio.csv")
data_cty <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/continents-according-to-our-world-in-data.csv")
data_pdy <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/GDP_Per_Worker.csv")
data_emp <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/Unemployment_Percentage.csv")
data_gdp <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/gdp-per-capita-worldbank.csv")
data_cont <- read_csv("/Users/ade/atunwa/EFDS 1/Introduction to Data Science/SDG_Index/continents-according-to-our-world-in-data.csv")


# Cleaning Youth Unemployment File
data_yu <- arrange(data_yu, Year, Code)
data_yu <- data_yu[, c(3,1,2,4)]


# Cleaning Labour Participation File
old_columns <- paste("...", 5:69, sep="")
new_columns <- 1960:2024

data_lp <- data_lp %>% 
  rename_with(~ as.character(new_columns), .cols = all_of(old_columns))
data_lp <- data_lp %>%
  gather(key="Year", value="Participation_Rate", names(select(data_lp, (as.character(1960:2024)))))


data_lp <- data_lp %>% select(-c("...4", "...3")) %>% filter(`World Development Indicators` %in% as.vector(data_cty$Code))

data_lp <- rename(data_lp, 'Code' = 'World Development Indicators') %>% select(-"Data Source")

data_lp$Year <- as.numeric(data_lp$Year)
main <- inner_join(data_yu, data_lp, by = c("Year", "Code"))


#Cleaning Population Size File
new_columns <- 1960:2024
old_columns <- paste0("...", 3:67)


data_ps <- data_ps %>% 
  rename_with(~ as.character(new_columns), .cols = all_of(old_columns))
data_ps <- data_ps %>%
  gather(key="Year", value="Population_Size", names(select(data_ps, (as.character(1960:2024)))))
data_ps <- rename(data_ps, 'Code' = 'World Development Indicators') 
data_ps <- data_ps %>% filter(`Code` %in% as.vector(data_cty$Code)) %>% select(-"...2")
data_ps$Year <- as.numeric(data_ps$Year)

main <- inner_join(main, data_ps, by = c("Year", "Code"))


#Cleaning Female to Male Proportion of Labor Force File

old_columns <- paste("...", 5:69, sep="")
new_columns <- 1960:2024

data_fm <- data_fm %>% rename_with(~as.character(new_columns), .cols= all_of(old_columns))

data_fm <- data_fm %>%
  gather(key="Year", value="Female_Male_Labour_Ratio", names(select(data_fm, (as.character(1960:2024)))))
data_fm <- rename(data_fm, 'Code' = 'World Development Indicators') 
data_fm <- data_fm %>% filter(`Code` %in% as.vector(data_cty$Code)) %>% select(-c("...3", "Data Source"))
data_fm <- select(data_fm, -"...4")
data_fm$Year <- as.numeric(data_fm$Year)

main <- inner_join(main, data_fm, by = c("Year", "Code"))

#Cleaning Labor Productivity File

old_columns <- paste("...", 5:69, sep="")
new_columns <- 1960:2024

data_pdy <- data_pdy %>% rename_with(~as.character(new_columns), .cols= all_of(old_columns))

data_pdy <- data_pdy %>%
  gather(key="Year", value="GDP_Per_Worker", names(select(data_pdy, (as.character(1960:2024)))))
data_pdy <- rename(data_pdy, 'Code' = 'World Development Indicators') 
data_pdy <- data_pdy %>% filter(`Code` %in% as.vector(data_cty$Code)) %>% select(-c("...3", "...4", "Data Source"))
data_pdy$Year <- as.numeric(data_pdy$Year)

main <- inner_join(main, data_pdy, by = c("Year", "Code"))


#Cleaning Unemployment File

old_columns <- paste("...", 5:69, sep="")
new_columns <- 1960:2024

data_emp <- data_emp %>% rename_with(~as.character(new_columns), .cols= all_of(old_columns))

data_emp <- data_emp %>%
  gather(key="Year", value="Unemployment_As_A_Percentage", names(select(data_emp, (as.character(1960:2024)))))
data_emp <- rename(data_emp, 'Code' = 'World Development Indicators') 
data_emp <- data_emp %>% filter(`Code` %in% as.vector(data_cty$Code)) %>% select(-c("...3", "...4", "Data Source"))
data_emp$Year <- as.numeric(data_emp$Year)

main <- inner_join(main, data_emp, by = c("Year", "Code"))


#Cleaning GDP Per Capita File

data_gdp <- rename(data_gdp, GDP_Per_Capita = `GDP per capita, PPP (constant 2017 international $)`)

main <- left_join(main, data_gdp[, c(2,4)])

#Classifying the countries by income 



#Handling the missing data

main <- mice(main, m = 3, maxit = 5, method = 'cart', seed = 123)
main <- complete(main, action = 1)

#standardising features

standardize <- function(x) {
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return (z)
}

main[, c(-1:-3)] <- main[, c(-1:-3)] %>% apply(2, standardize)

#performing PCA

pca <- princomp(main[,-c(1:3)])
summary(pca)

#scree plot
scree_plot <- fviz_eig(pca, addLabels = TRUE)

#biplot of the attributes
fviz_pca_var(pca, col.var = "black")

#contribution of each variable
fviz_cos2(pca, choice = "var", axes = 1:2)


#not exactly sure what this does yet
pca_coordinates <- pca$scores[,1:2]
pca_coordinates <- data.frame(
  Year = main$Year,
  Country = main$Entity,
  PC1 = pca_coordinates[,1],
  PC2 = pca_coordinates[,2],
  PC3 = pca$scores[,3]
)

#K-means clustering
set.seed(123)
km <- kmeans(pca_coordinates[, c("PC1", "PC2")], centers = 3, nstart = 25)
pca_coordinates$cluster <- as.factor(km$cluster)

#Visualising clusters
fviz_cluster(
  list(data = pca_coordinates[, c("PC1", "PC2")], cluster = km$cluster),
  geom = "point",
  ellipse = TRUE,
  stand = FALSE
) + theme_minimal()

#Visualising in 3D!
plot_ly(
  x = ~pca$scores[,1],
  y = ~pca$scores[,2],
  z = ~pca$scores[,3],
  color = ~pca_coordinates$cluster,
  type = "scatter3d",
  mode = "markers"
)

pca_coordinates <- left_join(pca_coordinates, data_cont[,c(1,4)], by = c("Country" = "Entity"))
ggplot(pca_coordinates, mapping = aes(x = PC1, y = PC2, group = Country, color = Continent)) +
  geom_path(alpha = 0.7) +
  geom_point(size = 1) +
  theme_minimal() +
  facet_wrap(~ Continent) +
  labs(title = "Country trajectories in PCA space over time")


plot_ly(pca_coordinates,
        x = ~PC1, y = ~PC2, z = ~PC3,
        color = ~Continent,
        type = 'scatter3d',
        mode = 'lines+markers',
        split = ~Country)

centroids <- pca_coordinates %>%
  group_by(Continent, Year) %>%
  summarise(PC1 = mean(PC1), 
            PC2 = mean(PC2),
            PC3 = mean(PC3))










