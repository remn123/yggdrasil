# Define server logic required to draw a histogram
server <- function(input, output) {
  callModule(yggModule, "tree")
  
  callModule(selectWithBarsModule, "select_x", train, test)
  
}