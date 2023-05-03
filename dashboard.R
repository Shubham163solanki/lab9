# Import necessary libraries
library(shiny)
library(httr)
library(jsonlite)
library(shinythemes)
library(plotly)


ui <- fluidPage(

  
  titlePanel("Cat Breeds Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("breed", "Select Breed", choices = c("Abyssinian", "Bengal", "Persian", "Siamese")),
      actionButton("getBreedDetails", "Get Breed Details")
    ),
    mainPanel(
      tags$h4("Breed Details"),
      tags$div(id = "breedDetails",
               tags$h4("Name:"),
               textOutput("breedName"),
               tags$h4("Description:"),
               textOutput("breedDescription"),
               tags$h4("Temperament:"),
               textOutput("breedTemperament"),
               tags$h4("Origin:"),
               textOutput("breedOrigin"),
               tags$h4("Life Span:"),
               textOutput("breedLifeSpan"),
               tags$h4("Attributes:"),
               plotlyOutput("breedAttributes")
      )
    )
  )
)


server <- function(input, output) {

  getBreedDetails <- function() {
    breedName <- input$breed
    
    response <- GET(paste0("https://api.thecatapi.com/v1/breeds/search?q=", breedName))
    breedData <- content(response, "text")
    breed <- fromJSON(breedData)
    
    breedDetails <- breed[1, c("name", "description", "temperament", "origin", "life_span", "affection_level", "intelligence", "adaptability", "stranger_friendly", "dog_friendly", "health_issues")]
    breedDetails
  }
  

  observeEvent(input$getBreedDetails, {
    breedDetails <- getBreedDetails()
    
    output$breedName <- renderText(breedDetails$name)
    output$breedDescription <- renderText(breedDetails$description)
    output$breedTemperament <- renderText(breedDetails$temperament)
    output$breedOrigin <- renderText(breedDetails$origin)
    output$breedLifeSpan <- renderText(breedDetails$life_span)
    
    output$breedAttributes <- renderPlotly({
      attributes <- c("Affection Level", "Intelligence", "Adaptability", "Stranger Friendly", "Dog Friendly", "Health Issues")
      values <- c(breedDetails$affection_level, breedDetails$intelligence, breedDetails$adaptability, breedDetails$stranger_friendly, breedDetails$dog_friendly, breedDetails$health_issues)
      
      df <- data.frame(Attribute = attributes, Value = values)
      
      fig <- plot_ly(df, type = "scatterpolar", mode = "lines",
                     theta = ~Attribute, r = ~Value, fill = "toself",
                     line = list(color = "lightblue"), hoverinfo = "none")
      
      fig <- fig %>% layout(title = "Breed Attributes",
                            polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
                            showlegend = FALSE)
      
      fig
    })
  })
}
shinyApp(ui = ui, server = server)

