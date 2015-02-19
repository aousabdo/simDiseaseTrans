
library(shiny)
library(markdown)
library(rmarkdown)

shinyUI(pageWithSidebar(
  
  headerPanel("Disease-Transmission Simulation"),
  sidebarPanel(
    br(),
    wellPanel(
      sliderInput("N","Number of Simulated Personas:", min = 100, max = 1000, value = 150, step = 20),
      br(),
      sliderInput("encounter","Number of Simulated Encounters:", min = 1, max = 20, value = 3, step = 1),
      br(),
      sliderInput("encounter_ratio","Percentage of Population having N Encounters 
                  (defaults to a 100 when selected number of encounters is 1): ", min = 10, max = 100, value = 50, step = 10),
      br(),
      checkboxInput("adjustMatrix", label = "Adjust Percentages in Matrix", value = FALSE),
      conditionalPanel("input.adjustMatrix == true",
                       sliderInput("adjustMatrixValue", "Subtract the following value from non-zero 
                                   values in the Transmission Probabilty Matrix:", 
                                   min = 5, max = 80, value = 20, step = 5))
    ),
    wellPanel(
      helpText("Exposure Level: The three sliders provided below allow the user to adjust the percentages of the three
               different levels of exposure."),
      helpText(HTML("<p><font color=\"red\"> <b>Please note that percentages selected have to add up to a 100.</b></font></p>")),
      sliderInput("exp.level.minor", "% of Minor exposure level"      , min = 10, max = 80, value = 30, step = 1),
      sliderInput("exp.level.moderate", "% of Moderate exposure level", min = 10, max = 80, value = 40, step = 1),
      sliderInput("exp.level.high", "% of High exposure level"        , min = 10, max = 80, value = 30, step = 1)
    ),
    
    wellPanel(
      p("Click the button to generate the simulations."),
      actionButton("goButton", "Run!")
    ),
    
    wellPanel(
      helpText("Download Simulted Data: "),
      br(),
      downloadButton('downloadPopData', 'Download Simulated Population'),
      br(),
      br(),
      downloadButton('downloadIntData', 'Download Simulated Interactions'),
      br(),
      br(),
      downloadButton('downloadMatrix', 'Download Transmission Probability Matrix')
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("About", includeMarkdown("About.md")),
      tabPanel("Distributions of Personas and Exposure Levels", plotOutput("distsPlot2", width = 1100, height=850)),
      tabPanel("Population Plots", plotOutput("popPlots", width = 1100, height=850)),
      tabPanel("Post-Exposure Status of Recipients", plotOutput("postExpPlot", width = 1100, height=850)),
      tabPanel("Simulated Population", dataTableOutput("population")),
      tabPanel("Simulated Interactions", dataTableOutput("dataTable2")),
      tabPanel("Transmission Probability Matrix", tableOutput("transMatrix"))
    )
  )
)
)
