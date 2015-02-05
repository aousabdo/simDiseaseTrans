
library(shiny)
library(markdown)
library(rmarkdown)

shinyUI(pageWithSidebar(
  
  headerPanel("Disease-Transmission Simulation"),
  sidebarPanel(
    br(),
    wellPanel(
      sliderInput("N","Number of Simulated Personas:", min = 100, max = 10000, value = 1000,step = 100 )
      #     sliderInput("encounter","Number of Simulated Encounters:", min = 1, max = 10, value = 2,step = 1),
    ),
    wellPanel(
      helpText("Exposure Level: The three sliders provided below allow the user to adjust the percentages of the three
               different levels of exposure."),
      helpText(HTML("<p><font color=\"red\"> <b>Please note that percentages selected have to add up to a 100.</b></font></p>")),
      sliderInput("exp.level.minor", "% of Minor exposure level", min = 1, max = 98, value = 30, step = 1),
      sliderInput("exp.level.moderate", "% of Moderate exposure level", min = 1, max = 98, value = 40, step = 1),
      sliderInput("exp.level.high", "% of High exposure level", min = 1, max = 98, value = 30, step = 1)
    ),
    
    wellPanel(
      p("Click the button to generate the simulations."),
      actionButton("goButton", "Run!")
    ),
    
    wellPanel(
      downloadButton('downloadData', 'Download Simulated Data'),
      helpText("Download button is not yet working.")
    )
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
