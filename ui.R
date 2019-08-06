library(shinydashboard)
# dashboardPage(header,sidebar,body)
shinyUI(dashboardPage(
  skin = 'blue',
  
  #1 Header
  dashboardHeader(title = "Analysis of Happy Countries"),
  
  
  #2 Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("App Info", tabName = "intro", icon = icon("info")),
      br(),
      menuItem("Happiness Ranking", tabName = "Ranking", icon = icon("signal")),
      br(),
      menuItem("Contributing Factors", tabName = 'Factors', icon = icon("th-list")),
      menuSubItem("Economy", tabName = 'Economy'),
      menuSubItem("Family", tabName = "Family"),
      menuSubItem("Life Expectency", tabName = "Life"),
      menuSubItem("Freedom", tabName = "Freedom"),
      menuSubItem("Trust", tabName = "Trust"),
      menuSubItem("Generosity", tabName = "Generosity"),
      br(),
      menuItem("Significant Variations", tabName = "change", icon = icon("tags")),
      br(),
      menuItem("Data Table", tabName = "data", icon = icon("database")),
      br(),
      br(),
      br(),
      br(),
      menuItem('GitHub', icon = icon('github'),
               href = 'https://github.com/rajeshearlu/World-Happiness-Report')
    )),
  
  
  
  ## Body
  dashboardBody(
    tabItems(
      
      #Intro Page
      #=======================
      tabItem(tabName = 'intro',
              fluidRow(box(background = 'aqua',
                           h1('Introduction', style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h3("The World Happiness Report is a landmark survey of the state of global happiness.The report continues to gain global recognition as 
                              governments, organizations and civil society increasingly use happiness indicators to inform their policy-making decisions. Leading experts across 
                              fields - economics, psychology, survey analysis, national statistics, health, public policy and more - describe how measurements of well-being 
                              can be used effectively to assess the progress of nations. The reports review the state of happiness in the world today and show how the new science 
                              of happiness explains personal and national variations in happiness.", style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h1("Motivation",  style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h3("The purpose of choosing this work is to find out which factors are more important to live a happier life. As a result, people and countries 
                              can focus on the more significant factors to achieve a higher happiness level. There are three parts to my report as follows:", style = "font-family: 'times'; font-si16pt"),
                           
                           h3("1. Cleaning Data",style = "font-family: 'times'; font-si16pt"),
                           h3("2. Visualization",style = "font-family: 'times'; font-si16pt"),
                           
                           h3("The questions I'm going to answer here are:",style = "font-family: 'times'; font-si16pt"),
                           h3("What are the top 20 countries or regions rank the highest in overall happiness for each year ?",style = "font-family: 'times'; font-si16pt"),
                           h3("What are the factors contributing to happiness ?",style = "font-family: 'times'; font-si16pt"),
                           h3("How did country ranks or scores change between the 2015 and 2016 as well as the 2016 and 2017 reports and which country experienced a significance
                              increase or decrease in happiness ?",style = "font-family: 'times'; font-si16pt"),
                           em(h3("Let's begin for happiness!!!",style = "font-family: 'times'; font-si18pt", align = "center")),
                           
                           
                           
                           
                           
                           width =10))),
      
      
      
      
      
      
      
      
      
      
      
      #Ranking Page
      #==========================
      
      tabItem(tabName = 'Ranking',
              fluidRow(box(background = 'aqua',
                           selectInput('Year',label = h3('Select  Year'),
                                       choices = list("2015"=2015, "2016"=2016, "2017"=2017),
                                       selected = 2015, width = 180)),
                       br(),
                       br(),
                       br(),
                       br(),
                       box(plotOutput("Ranking"), width = 12, length = 30))),
      
      
      ##Contributing Factors
      #======================
      
      #Economy
      #=========
      
      tabItem(tabName = 'Economy',
              fluidRow(box(background = 'aqua',
                           selectInput("Continent2", label = h3('Select Continent'),
                                       choices = list("Asia" = "Asia", "Europe" = "Europe", "North America" = "North America", "South America" = "South America", "Australia" = "Australia", "Africa" = "Africa"),
                                       selected = 1, width = 'auto')),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       box(plotOutput("Economy"),  width = 12, length = 30 ))),
      
      #Family
      #========
      
      tabItem(tabName = 'Family',
              fluidRow(box(background = 'aqua',
                           selectInput("Continent3", label = h3('Select Continent'),
                                       choices = list("Asia" = "Asia", "Europe" = "Europe", "North America" = "North America", "South America" = "South America", "Australia" = "Australia", "Africa" = "Africa"),
                                       selected = "Asia", width = 'auto')),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       box(plotOutput("Family"),  width = 12, length = 30 ))),
      
      #Life Expectency
      #=================
      
      tabItem(tabName = 'Life',
              fluidRow(box(background = 'aqua',
                           selectInput("Continent4", label = h3('Select Continent'),
                                       choices = list("Asia" = "Asia", "Europe" = "Europe", "North America" = "North America", "South America" = "South America", "Australia" = "Australia", "Africa" = "Africa"),
                                       selected = 1, width = 'auto')),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       box(plotOutput("Life"),  width = 12, length = 30 ))),
      
      #Freedom
      #========
      
      tabItem(tabName = 'Freedom',
              fluidRow(box(background = 'aqua',
                           selectInput("Continent5", label = h3('Select Continent'),
                                       choices = list("Asia" = "Asia", "Europe" = "Europe", "North America" = "North America", "South America" = "South America", "Australia" = "Australia", "Africa" = "Africa"),
                                       selected = 1, width = 'auto')),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       box(plotOutput("Freedom"),  width = 12, length = 30 ))),
      
      #Trust
      #========
      
      tabItem(tabName = 'Trust',
              fluidRow(box(background = 'aqua',
                           selectInput("Continent6", label = h3('Select Continent'),
                                       choices = list("Asia" = "Asia", "Europe" = "Europe", "North America" = "North America", "South America" = "South America", "Australia" = "Australia", "Africa" = "Africa"),
                                       selected = 1, width = 'auto')),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       box(plotOutput("Trust"),  width = 12, length = 30 ))),
      
      #Generosity
      #============
      
      tabItem(tabName = 'Generosity',
              fluidRow(box(background = 'aqua',
                           selectInput("Continent7", label = h3('Select Continent'),
                                       choices = list("Asia" = "Asia", "Europe" = "Europe", "North America" = "North America", "South America" = "South America", "Australia" = "Australia", "Africa" = "Africa"),
                                       selected = 1, width = 'auto')),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       box(plotOutput("Generosity"),  width = 12, length = 30 ))),
      
      
      
      ##Significant Rank Changes
      #==========================================================================================
      
      tabItem(tabName = 'change',
              
              
              tabBox( 
                tabPanel("Increase 2015-2016", 
                         p(h4("Below are the countries which had significant increase in ranks or scores of happiness between 2015 and 2016")),
                         fluidRow(
                           box(plotOutput ("change1"), width = "auto", length = "auto"))),
                
                
                
                tabPanel("Increase 2016-2017", 
                         p(h4("Below are the countries which had significant increase in ranks or scores of happiness between 2016 and 2017")),
                         fluidRow(
                           box(plotOutput ("change2"), width = "auto", length = "auto"))),
                
                
                
                tabPanel("Decrease 2015-2016",
                         
                         p(h4("Below are the countries which had significant decline in ranks or scores of happiness between 2015 and 2016")),
                         fluidRow(
                           box(plotOutput ("change3"), width = "auto", length = "auto"))),
                
                
                
                tabPanel("Decrease 2016-2017",
                         p(h4("Below are the countries which had significant decline in ranks or scores of happiness between 2016 and 2017")),
                         fluidRow(
                           box(plotOutput ("change4"), width = "auto", length = "auto"))), width = 12), width = 12),
      
      
      
      
      ##Data Table
      #==========================================================================================
      
      tabItem(tabName = 'data',
              p(h3("Dataset used for Ranking & Factors")),
              fluidRow(box(DT::dataTableOutput("table1"), width = 12),
                       p(h3("Dataset used for Significant Rank Changes")),
                       fluidRow(box(DT::dataTableOutput("table2"), width = 12)))#changing width to 12 makes it take up whole page
      )
      
      
      
      
      
      
              ))
  
  
  
  
  
  
  
  
  ))