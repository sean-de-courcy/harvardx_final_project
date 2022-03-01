#pkgs <- c("factoextra",  "NbClust")
#install.packages(pkgs)

library(factoextra)
library(NbClust)
library(tidyverse)
unemployment <- read.csv("output.csv")
unemployment <- unemployment %>% mutate(County = paste(County, State, sep = ", ") )
unemployment <- unemployment %>% select(-State) %>% group_by(Year, County) %>% summarise(Rate = mean(Rate)) %>% ungroup() %>% data.frame()
minimum_wage <- read.csv("Minimum Wage Data.csv")
minimum_wage <- minimum_wage %>% select(Year, State, Effective.Minimum.Wage.2020.Dollars) %>% filter(Year >= 1990 & Year <= 2016) %>% rename(Min.Wage = Effective.Minimum.Wage.2020.Dollars)

yearlyNationalAvgs <- unemployment %>% na.omit() %>% group_by(Year) %>% summarise(Avg = mean(Rate))
unemployment.kmeans.data <- unemployment %>% na.omit() %>% mutate(Delta = Rate - yearlyNationalAvgs[Year - 1989,]$Avg) %>% select(-Rate)
unemployment.kmeans.data <- unemployment.kmeans.data %>% pivot_wider(names_from = Year, values_from = Delta) %>% select(-County) %>% na.omit() %>% data.frame()

fviz_nbclust(unemployment.kmeans.data, kmeans, nstart = 25, method = "gap_stat", k.max = 15, nboot = 50) + labs(subtitle = "Gap statistic for kmeans")
fviz_nbclust(unemployment.kmeans.data, kmeans, method = "wss") + labs(subtitle = "Elbow method for kmeans")
fviz_nbclust(unemployment.kmeans.data, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method for kmeans")

unemployment.kmeans <- kmeans(unemployment.kmeans.data, centers = 7, nstart = 25)
unemployment.kmeans.data$Cluster <- unemployment.kmeans$cluster
unemployment.kmeans.data$County <- unemployment %>% na.omit() %>% mutate(Delta = Rate - yearlyNationalAvgs[Year - 1989,]$Avg) %>% select(-Rate) %>% pivot_wider(names_from = Year, values_from = Delta) %>% na.omit() %>% .$County

unemployment <- unemployment.kmeans.data %>% pivot_longer(!c(County, Cluster), names_to = "Year") %>% rename(Delta = value) %>% transform(Year = as.numeric(str_extract(Year, "(?<=X)[:digit:]+")))

grouped_unemployment <- unemployment %>% group_by(Year, Cluster) %>% summarise(Delta = mean(Delta)) %>% ungroup() %>% data.frame()

unemployment %>% ggplot(aes(x = Year, y = Delta, group = County)) + geom_line(alpha = 0.2) + facet_wrap(~Cluster) + geom_line(data = grouped_unemployment, aes(x = Year, y = Delta, group = FALSE), color = "Red")
