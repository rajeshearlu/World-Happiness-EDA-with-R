library(dplyr)
library(ggplot2)
library(shiny)
library(ggthemes)
library(DT)
library(rsconnect)


## Loading Data
#Rank15 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2015.csv")
#Rank16 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2016.csv")
#Rank17 <- read.csv("C:/Users/rajes/Desktop/NYDS/Data Analysis & Visualization with R/Shiny/Happy Countries/data/2017.csv")

Rank15 <- read.csv("2015.csv")
Rank16 <- read.csv("2016.csv")
Rank17 <- read.csv("2017.csv")

## Added Year column
Rank152 <- mutate(Rank15, Year = 2015)
colnames(Rank152) <- c("Country", "Region", "Rank", "Score","Standard.Error","Economy","Family","Life","Freedom","Trust","Generosity","Dystopia", "Year")

Rank162 <- mutate(Rank16, Year = 2016)
colnames(Rank162) <- c("Country", "Region", "Rank", "Score","Confidence1","Confidence2", "Economy", "Family", "Life", "Freedom", "Trust", "Generosity", "Dystopia", "Year")

Rank172 <- mutate(Rank17, Year = 2017)
colnames(Rank172) <- c("Country", "Rank", "Score","Confidence1","Confidence2", "Economy", "Family", "Life", "Freedom", "Generosity","Trust", "Dystopia", "Year")


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

Happiness2 <- df154 %>% select(Country,Continent, everything()) 




#creating a new data frame to show ranks for each year & then change of ranks

df1 <- merge(Rank15[,c(1,3,4)], Rank16[,c(1,3,4)], by.x = "Country", by.y = "Country")
colnames(df1) <- c("Country", "Rank2015", "Happiness.Score2015", "Rank2016","Happiness.Score2016")
Happiness <- merge(df1[,], Rank17[,c(1,2,3)])
colnames(Happiness) <- c("Country", "Rank2015", "Happiness.Score2015", "Rank2016","Happiness.Score2016", "Rank2017", "Happiness.Score2017")
Happiness <- Happiness %>%   mutate(`Rank Change 2015-2016`=`Rank2015`-`Rank2016`, `Rank Change 2016-2017`=`Rank2016`-`Rank2017`)


#Top10 Countries with increase in Happiness
Rankchange3 <- head(arrange(Happiness, desc(`Rank Change 2015-2016`)), 10)
Rankchange4 <- head(arrange(Happiness, desc(`Rank Change 2016-2017`)), 10)


#Top10 Countries with descrease in Happiness
Rankchange1 <- head(arrange(Happiness, (`Rank Change 2015-2016`)), 10)
Rankchange2 <- head(arrange(Happiness, (`Rank Change 2016-2017`)), 10)






##Server O/P
#=============

shinyServer(function(input, output) {
  
  
  #Ranking Page
  #=====================================================================================================================
  
  Happiness_reactive <- reactive({
    #Happiness2[Happiness2$Year==input$Year,]
    Happiness2 %>% 
      filter(Year == input$Year) %>% 
      arrange(Rank) 
      # head(20)
    
  })
  
  output$Ranking = renderPlot({
    
    Happiness3 = Happiness_reactive()
    
    Happiness3$Country <- factor(Happiness3$Country, levels = Happiness3$Country[order(Happiness3$Rank)])
    
    
    ggplot(data = head(Happiness3, 20), aes(x= Country, y= Rank)) +
      geom_segment(aes(x= Country, xend = Country, y = 0, yend = Rank), color = "red") +
      geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21) +
      theme_light() + 
      theme(panel.grid.major.x = element_blank(),panel.border = element_blank(),axis.ticks.x = element_blank()) +
      ggtitle("Happy Countries")
    
    
    
    
  })
  
  
  
  
  
  #Factors contributing to Happiness
  #============================================================================================================
  
  
  #Economy
  #=========
  
  output$Economy = renderPlot({
    
    Happiness4 <- Happiness2[Happiness2$Continent==input$Continent2,]
    
    ggplot(data = Happiness4, aes(x= Economy, y= Score)) + 
      geom_point(aes(color=Continent), size = 3, alpha = 0.8) +
      geom_smooth(aes(color = Continent, fill = Continent), method = "lm", fullrange = TRUE) +
      facet_wrap(~Year) +
      theme_bw() + labs(title = "Economy role in Happiness")
  })
  
  #Family
  #=========
  
  output$Family = renderPlot({
    
    Happiness5 <- Happiness2[Happiness2$Continent==input$Continent3,]
    
    ggplot(data = Happiness5, aes(x= Family, y= Score)) + 
      geom_point(aes(color=Continent), size = 3, alpha = 0.8) +
      geom_smooth(aes(color = Continent, fill = Continent), method = "lm", fullrange = TRUE) +
      facet_wrap(~Year) +
      theme_bw() + labs(title = "Family role in Happiness")
    
  })
  
  #Life Expectency
  #=================
  
  
  output$Life = renderPlot({
    
    Happiness4 <- Happiness2[Happiness2$Continent==input$Continent4,]
    
    ggplot(data = Happiness4, aes(x= Life, y= Score)) + 
      geom_point(aes(color=Continent), size = 3, alpha = 0.8) +
      geom_smooth(aes(color = Continent, fill = Continent), method = "lm", fullrange = TRUE) +
      facet_wrap(~Year) +
      theme_bw() + labs(title = "Life expectency role in Happiness")
    
  })
  
  #Freedom
  #========
  
  output$Freedom = renderPlot({
    
    Happiness4 <- Happiness2[Happiness2$Continent==input$Continent5,]
    
    ggplot(data = Happiness4, aes(x= Freedom, y= Score)) + 
      geom_point(aes(color=Continent), size = 3, alpha = 0.8) +
      geom_smooth(aes(color = Continent, fill = Continent), method = "lm", fullrange = TRUE) +
      facet_wrap(~Year) +
      theme_bw() + labs(title = "Freedom role in Happiness")
  })
  
  #Trust
  #===========
  
  output$Trust = renderPlot({
    
    Happiness4 <- Happiness2[Happiness2$Continent==input$Continent6,]
    
    ggplot(data = Happiness4, aes(x= Trust, y= Score)) + 
      geom_point(aes(color=Continent), size = 3, alpha = 0.8) +
      geom_smooth(aes(color = Continent, fill = Continent), method = "lm", fullrange = TRUE) +
      facet_wrap(~Year) +
      theme_bw() + labs(title = "Corruption role in Happiness")
    
  })
  
  
  
  #Generosity
  #===========
  
  output$Generosity = renderPlot({
    
    Happiness4 <- Happiness2[Happiness2$Continent==input$Continent7,]
    
    ggplot(data = Happiness4, aes(x= Generosity, y= Score)) + 
      geom_point(aes(color=Continent), size = 3, alpha = 0.8) +
      geom_smooth(aes(color = Continent, fill = Continent), method = "lm", fullrange = TRUE) +
      facet_wrap(~Year) +
      theme_bw() + labs(title = "Role of Generosity in Happiness")
    
  })
  
  ##Significant Rank Changes
  #==========================================================================================
  
  
  #Top10 Countries with increase in Happiness
  
  
  output$change1 = renderPlot({
    
    
    
    Rankchange3$Country <- factor(Rankchange3$Country, levels = Rankchange3$Country[order(Rankchange3$`Rank Change 2015-2016`)])
    
    ggplot(data=Rankchange3, aes(x=Country, y=`Rank Change 2015-2016`)) + 
      geom_bar(aes(fill=Country),stat = "identity") + theme_bw() +
      ggtitle("Significant increase in Rank: 2015-2016")
    
  })
  
  output$change2 = renderPlot({
    
    Rankchange4$Country <- factor(Rankchange4$Country, levels = Rankchange4$Country[order(Rankchange4$`Rank Change 2016-2017`)])
    
    
    ggplot(data=Rankchange4, aes(x=Country, y=`Rank Change 2016-2017`)) + 
      geom_bar(aes(fill=Country),stat = "identity") + theme_bw() +
      ggtitle("Significant increase in Rank: 2016-2017")
    
    
  })
  
  output$change3 = renderPlot({
    
    Rankchange1$Country <- factor(Rankchange1$Country, levels = Rankchange1$Country[order(Rankchange1$`Rank Change 2015-2016`)])
    
    
    ggplot(data=Rankchange1, aes(x= Country, y= -(`Rank Change 2015-2016`))) + 
      geom_bar(aes(fill=Country), stat = "identity") + theme_bw() +
      ggtitle("Significant decrease in Rank: 2015-2016")
    
  })
  
  
  output$change4 = renderPlot({
    
    Rankchange2$Country <- factor(Rankchange2$Country, levels = Rankchange2$Country[order(Rankchange2$`Rank Change 2016-2017`)])
    
    ggplot(data=Rankchange2, aes(x=Country,y= -(`Rank Change 2016-2017`))) + 
      geom_bar(aes(fill=Country),stat = "identity") + theme_bw() +
      ggtitle("Significant decrease in Rank: 2016-2017")
    
  })
  
  
  ##Data Table
  #==========================================================================================
  
  
  output$table1 <- DT::renderDataTable({
    datatable(data=Happiness2, rownames=FALSE) 
    
  })
  
  output$table2 <- DT::renderDataTable({
    datatable(data=Happiness, rownames=FALSE) 
    
  })
  
  
  
})

