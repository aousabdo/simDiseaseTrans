
library(shiny)
library(markdown)
library(rmarkdown)

shinyUI(pageWithSidebar(
  
  headerPanel("Disease-Transmission Simulation"),
  sidebarPanel(
    br(),
    wellPanel(
      sliderInput("N","Number of Simulated Personas:", min = 100, max = 10000, value = 1000,step = 100 ),
      #     sliderInput("encounter","Number of Simulated Encounters:", min = 1, max = 10, value = 2,step = 1),
      br(),
      sliderInput("ELminor", "% of Minor exposure level", min = 0, max = 100, value = 33, step = 1),
      sliderInput("ELmoderate", "% of Moderate exposure level", min = 0, max = 100, value = 33, step = 1),
      sliderInput("ELhigh", "% of High exposure level", min = 0, max = 100, value = 33, step = 1)
    ), 
    br(),
    downloadButton('downloadData', 'Download Simulated Data'),
    helpText("Download button is not yet working.")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("About", includeMarkdown("About.md")),
      tabPanel("Distributions of Personas and Exposure Levels", plotOutput("distsPlot", width = 1100, height=850)), 
      tabPanel("Post-Exposure Status of Recipients", plotOutput("postExpPlot", width = 1100, height=850)),
      tabPanel("Simulated Data (Table)", dataTableOutput("dataTable")),
      tabPanel("Transmission Probability Matrix", tableOutput("transMatrix"))
    )
  )
)
)
