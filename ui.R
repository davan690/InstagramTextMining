library(shiny)


shinyUI(
  fluidPage(
    # Application title
    tags$meta(name="description", content="Explore comments and post in instagram & faceboook, find your friends on instagram & faceboook in your neighbourhood, reach to your consumer with social marketing content, build real customer relationships with strategies from social media"),
    tags$meta(name="keywords",content="instagram,facebook,social media, marketing tools,friedfinder,christmas, application, learn Python, spark, scala, google , API, data mining, text mining,social media marketing,twitter, advertisment,information technology,Social Networking, Social Media, News, Web, Technology, Web 2.0, Tech, Information, Blog, Facebook, YouTube, Google, Top"),
    tags$meta(name="author",content="Ali Moridani"),
    titlePanel("EXPLORE INSTAGRAM IN YOUR NEIGHBORHOOD"),         hr(),
    includeScript("mycode.js"),
    tags$head(includeScript("google.js")),

# sidebarLayout(
# Sidebar with a slider and selection inputs

    fluidRow(column(3,
      h4("Latitude & Longtitude"),      
      verbatimTextOutput("GEObox"),
      hr(),
      h4("Your Address"),       
      verbatimTextOutput("addressbox"),
      helpText(div( strong("Note:"), "You need to grant browser to access your location upon request. Wait to see your aproximate location in the address box.          The most frequent  keywords in your locality will apear on the plot.\n The Shiny free service supports only one instant at a time, if link is dead you need to refresh it. Tested on Android & Windows OS. Support only English & Persian. \n It is a demo app using R & js. \n  any comment: MORIDANI@Gmail or ",a("LinkedIn.com/in/moridani",  target="_blank",   href="http://www.linkedin.com/in/moridani"),strong("\n UPDATE: INSTAGRAM rejects access to API's endpoint currently & it is under review for access!!!"),             style = "color:blue") ) ),
      
      column(2,   h4("Word MAP [Cloud]"), sliderInput("freq","Minimum Frequency:", min = 1,  max = 50, value = 2),
           sliderInput("max","Maximum Number of Words:", min = 1,  max = 100,  value = 35)),
    
      # Show Word Cloud
      column(5, plotOutput("plot"))),
                  
    fluidRow(dataTableOutput("view"))
  ))
