library(dplyr)
library(ggplot2)
library(shiny)
library(ggthemes)
library(plotly)

## Loading Data
#Rank15 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2015.csv")
#Rank16 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2016.csv")
#Rank17 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2017.csv")

Rank15 <- read.csv("2015.csv")
Rank16 <- read.csv("2016.csv")
Rank17 <- read.csv("2017.csv")


df1 <- merge(Rank15[,c(1,3,4)], Rank16[,c(1,3,4)], by.x = "Country", by.y = "Country")
colnames(df1) <- c("Country", "Rank2015", "Score2015", "Rank2016","Score2016")
Happiness <- merge(df1[,], Rank17[,c(1,2,3)])
colnames(Happiness) <- c("Country", "Rank2015", "Score2015", "Rank2016","Score2016", "Rank2017", "Score2017")
Happiness <- Happiness %>%   mutate(`Rank Change 2015-2016`=`Rank2015`-`Rank2016`, `Rank Change 2016-2017`=`Rank2016`-`Rank2017`)


## Top 20 Countries ranking highest in overall happiness 

#2015
#Top2015 <- head(arrange(Rank15,Happiness.Rank), 20) #Sorting top20 Countries
Plot1 <- ggplot(data = head(arrange(Happiness,Rank2015), 20), aes(x= Score2015, y= Country)) + 
         geom_segment(aes(x= 0, y = Country, xend = Score2015, yend = Country), color = "red") +
         geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21) +
         theme_light() + theme(panel.grid.major.y = element_blank(),panel.border = element_blank(),axis.ticks.y = element_blank()) +
         ggtitle("Happy Countries - 2015")

#2016
#Top2016 <- head(arrange(Rank16,Happiness.Rank), 20) #Sorting top20 Countries

Plot2 <- ggplot(data = head(arrange(Happiness,Rank2016), 20), aes(x= Score2016, y= Country)) + 
          geom_segment(aes(x= 0, y = Country, xend = Score2016, yend = Country), color = "orange") +
          geom_point(size=5, color="orange", fill=alpha("yellow", 0.3), alpha=0.7, shape=21) +
          theme_light() + theme(panel.grid.major.y = element_blank(),panel.border = element_blank(),axis.ticks.y = element_blank()) +
          ggtitle("Happy Countries - 2016")


#2017
#Top2017 <- head(arrange(Rank17,Happiness.Rank), 20) #Sorting top20 Countries
Plot3 <- ggplot(data = head(arrange(Happiness,Rank2017), 20), aes(x= Score2017, y= Country)) + 
          geom_segment(aes(x= 0, y = Country, xend = Score2017, yend = Country), color = "orange") +
          geom_point(size=5, color="orange", fill=alpha("lightgreen", 0.3), alpha=0.7, shape=21) +
          theme_light() + theme(panel.grid.major.y = element_blank(),panel.border = element_blank(),axis.ticks.y = element_blank()) +
          ggtitle("Happy Countries - 2017")




##Contribution of each factor to happiness for each region in 2016
#Economy
colnames(Rank16) <- c("Country", "Region", "Happiness.Rank", "Happiness.Score", "Lower.Confidence", "Upper.Confidence", "Economy", "Family", "Life.Expectency", "Freedom", "Corruption", "Generosity", "Dystopia")
Plot4 <- ggplot(data = Rank16, aes(x= Economy, y= Happiness.Score)) + 
        geom_point(aes(color=Region), size = 3, alpha = 0.8) +
        geom_smooth(aes(color = Region, fill = Region), method = "lm", fullrange = TRUE) +
        facet_wrap(~Region) +
        theme_bw() + labs(title = "Economy role in Happiness")


#Family
Plot5 <- ggplot(data = Rank16, aes(x= Family, y= Happiness.Score)) + 
  geom_point(aes(color=Region), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = Region, fill = Region), method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Family role in Happiness")

#Life Expectency
Plot6 <- ggplot(data = Rank16, aes(x= Life.Expectency, y= Happiness.Score)) + 
  geom_point(aes(color=Region), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = Region, fill = Region), method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Life expectency role in Happiness")

#Freedom

Plot7 <- ggplot(data = Rank16, aes(x= Freedom, y= Happiness.Score)) + 
  geom_point(aes(color=Region), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = Region, fill = Region), method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Freedom role in Happiness")


#Corruption
Plot8 <- ggplot(data = Rank16, aes(x= Corruption, y= Happiness.Score)) + 
  geom_point(aes(color=Region), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = Region, fill = Region), method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Corruption role in Happiness")

#Generosity
Plot9 <- ggplot(data = Rank16, aes(x= Generosity, y= Happiness.Score)) + 
  geom_point(aes(color=Region), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = Region, fill = Region), method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Role of Generosity in Happiness")





## Ranks change between 2015 and 2016
##countries that had significant increase or decrease in happiness

#creating a new data frame to show ranks for each year & then change of ranks

#df1 <- merge(Rank15[,c(1,3,4)], Rank16[,c(1,3,4)], by.x = "Country", by.y = "Country")
#colnames(df1) <- c("Country", "Rank2015", "Happiness.Score2015", "Rank2016","Happiness.Score2016")
#Happiness <- merge(df1[,], Rank17[,c(1,2,3)])
#colnames(Happiness) <- c("Country", "Rank2015", "Happiness.Score2015", "Rank2016","Happiness.Score2016", "Rank2017", "Happiness.Score2017")
#Happiness <- Happiness %>%   mutate(`Rank Change 2015-2016`=`Rank2015`-`Rank2016`, `Rank Change 2016-2017`=`Rank2016`-`Rank2017`)

#Top10 Countries with decrease in Happiness
Rankchange1 <- head(arrange(Happiness, `Rank Change 2015-2016`), 10)
Rankchange2 <- head(arrange(Happiness, `Rank Change 2016-2017`), 10)

#Top10 Countries with increase in Happiness
Rankchange3 <- head(arrange(Happiness, desc(`Rank Change 2015-2016`)), 10)
Rankchange4 <- head(arrange(Happiness, desc(`Rank Change 2016-2017`)), 10)

Plot10 <- ggplot(data=Rankchange1, aes(x=`Rank Change 2015-2016`)) + 
          geom_histogram(aes(fill=Country),binwidth = 1) + theme_bw() +
          ggtitle("Significant decrease in Rank: 2015-2016")

Plot11 <- ggplot(data=Rankchange2, aes(x=`Rank Change 2016-2017`)) + 
           geom_histogram(aes(fill=Country),binwidth = 1) + theme_bw() +
           ggtitle("Significant decrease in Rank: 2016-2017")


Plot12 <- ggplot(data=Rankchange3, aes(x=`Rank Change 2015-2016`)) + 
          geom_histogram(aes(fill=Country),binwidth = 1) + theme_bw() +
          ggtitle("Significant increase in Rank: 2015-2016")

Plot13 <- ggplot(data=Rankchange4, aes(x=`Rank Change 2016-2017`)) + 
          geom_histogram(aes(fill=Country),binwidth = 1) + theme_bw() +
          ggtitle("Significant increase in Rank: 2016-2017")


Plot20 <- ggplot(ggplot(data=Rankchange1, aes(x=country, y= `Rank Change 2015-2016`)) + 
         geom_bar(aes(fill=Country)) + theme_bw() +
         ggtitle("Significant decrease in Rank: 2015-2016"))












