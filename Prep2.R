library(dplyr)
library(ggplot2)
library(shiny)
library(ggthemes)

## Loading Data
#Rank15 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2015.csv")
#Rank16 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2016.csv")
#Rank17 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2017.csv")

Rank15 <- read.csv("2015.csv")
Rank16 <- read.csv("2016.csv")
Rank17 <- read.csv("2017.csv")

## Added Year column
Rank152 <- mutate(Rank15, Year = 2015)
colnames(Rank152) <- c("Country", "Region", "Rank", "Score","Standard.Error","Economy","Family","Life.Expectency","Freedom","Trust","Generosity","Dystopia", "Year")

Rank162 <- mutate(Rank16, Year = 2016)
colnames(Rank162) <- c("Country", "Region", "Rank", "Score","Confidence1","Confidence2", "Economy", "Family", "Life.Expectency", "Freedom", "Trust", "Generosity", "Dystopia", "Year")

Rank172 <- mutate(Rank17, Year = 2017)
colnames(Rank172) <- c("Country", "Rank", "Score","Confidence1","Confidence2", "Economy", "Family", "Life.Expectency", "Freedom", "Generosity","Trust", "Dystopia", "Year")


##Combining data of 2015,2016,2017

df153 <- rbind(Rank152[,c(1,3,4,6:11,13)], Rank162[,c(1,3,4,7:12,14)])
df154 <- rbind(df153[,], Rank172[,c(1:3,6:11,13)])



## Added a column continent

df154$Continent <- NA

df154$Continent[which(df154$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                   "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                   "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                   "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                   "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                   "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                   "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
df154$Continent[which(df154$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                   "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                   "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                   "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                   "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                   "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus","Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                   "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                   "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
df154$Continent[which(df154$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                   "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                   "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                   "Haiti"))] <- "North America"
df154$Continent[which(df154$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                   "Colombia", "Ecuador", "Bolivia", "Peru",
                                                   "Paraguay", "Venezuela"))] <- "South America"
df154$Continent[which(df154$Country %in% c("New Zealand", "Australia"))] <- "Australia"
df154$Continent[which(is.na(df154$Continent))] <- "Africa"


# Moving the continent column's position in the dataset to the second column

Happiness2 <- df154  %>% select(Country,Continent, everything()) 




## Ranks change between 2015 and 2016
##countries that had significant increase or decrease in happiness

#creating a new data frame to show ranks for each year & then change of ranks

df1 <- merge(Rank15[,c(1,3,4)], Rank16[,c(1,3,4)], by.x = "Country", by.y = "Country")
colnames(df1) <- c("Country", "Rank2015", "Happiness.Score2015", "Rank2016","Happiness.Score2016")
Happiness <- merge(df1[,], Rank17[,c(1,2,3)])
colnames(Happiness) <- c("Country", "Rank2015", "Happiness.Score2015", "Rank2016","Happiness.Score2016", "Rank2017", "Happiness.Score2017")
Happiness <- Happiness %>%   mutate(`Rank Change 2015-2016`=`Rank2015`-`Rank2016`, `Rank Change 2016-2017`=`Rank2016`-`Rank2017`)







## Top 20 Countries ranking highest in overall happiness 


Plot1 <-  ggplot(data = head(Happiness2,20), aes(x= Country, y= Rank)) + 
                  geom_segment(aes(x= Country, xend = Country, y = 0, yend = Rank), color = "red") +
                  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21) +
                  theme_light() + theme(panel.grid.major.x = element_blank(),panel.border = element_blank(),axis.ticks.x = element_blank()) +
                  ggtitle("Happy Countries - 2015")



Plot2 <- ggplot(data = head(filter(Happiness2,Year == '2016'),20), aes(x= Country, y= Rank)) + 
  geom_segment(aes(x= Country, xend = Country, y = 0, yend = Rank), color = "orange") +
  geom_point(size=5, color="orange", fill=alpha("yellow", 0.3), alpha=0.7, shape=21) +
  theme_light() + theme(panel.grid.major.x = element_blank(),panel.border = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Happy Countries - 2016")



Plot3 <- ggplot(data = head(filter(Happiness2,Year == '2017'),20), aes(x= Country, y= Rank)) + 
  geom_segment(aes(x= Country, xend = Country, y = 0, yend = Rank), color = "orange") +
  geom_point(size=5, color="orange", fill=alpha("lightgreen", 0.3), alpha=0.7, shape=21) +
  theme_light() + theme(panel.grid.major.x = element_blank(),panel.border = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Happy Countries - 2017")





##Contribution of each factor to happiness for each continent in 2016
#Economy
colnames(Rank16) <- c("Country", "Region", "Happiness.Rank", "Happiness.Score", "Lower.Confidence", "Upper.Confidence", "Economy", "Family", "Life.Expectency", "Freedom", "Corruption", "Generosity", "Dystopia")
Plot4 <- ggplot(data = Rank16, aes(x= Economy, y= Happiness.Score)) + 
  geom_point(aes(color=Region), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = Region, fill = Region), method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Economy role in Happiness")







#Top10 Countries with increase in Happiness
Rankchange3 <- head(arrange(Happiness, desc(`Rank Change 2015-2016`)), 10)
Rankchange4 <- head(arrange(Happiness, desc(`Rank Change 2016-2017`)), 10)


Rankchange3$Country <- factor(Rankchange3$Country, levels = Rankchange3$Country[order(Rankchange3$`Rank Change 2015-2016`)])


Plot12 <- ggplot(data=Rankchange3, aes(x=Country, y=`Rank Change 2015-2016`)) + 
  geom_bar(aes(fill=Country),stat = "identity") + theme_bw() +
  ggtitle("Significant increase in Rank: 2015-2016")

Rankchange4$Country <- factor(Rankchange4$Country, levels = Rankchange4$Country[order(Rankchange4$`Rank Change 2016-2017`)])


Plot13 <- ggplot(data=Rankchange4, aes(x=Country, y=`Rank Change 2016-2017`)) + 
  geom_bar(aes(fill=Country),stat = "identity") + theme_bw() +
  ggtitle("Significant increase in Rank: 2016-2017")



#Top10 Countries with decrease in Happiness
Rankchange1 <- head(arrange(Happiness, `Rank Change 2015-2016`), 10)
Rankchange2 <- head(arrange(Happiness, `Rank Change 2016-2017`), 10)


Rankchange1$Country <- factor(Rankchange1$Country, levels = Rankchange1$Country[order(Rankchange1$`Rank Change 2015-2016`)])

Plot10 <- ggplot(data=Rankchange1, aes(x= Country, y= -(`Rank Change 2015-2016`))) + 
  geom_bar(aes(fill=Country), stat = "identity") + theme_bw() +
  ggtitle("Significant decrease in Rank: 2015-2016")

Rankchange2$Country <- factor(Rankchange2$Country, levels = Rankchange2$Country[order(Rankchange2$`Rank Change 2016-2017`)])

Plot11 <- ggplot(data=Rankchange2, aes(x=Country,y= -(`Rank Change 2016-2017`))) + 
  geom_bar(aes(fill=Country),stat = "identity") + theme_bw() +
  ggtitle("Significant decrease in Rank: 2016-2017")





