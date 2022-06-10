source("R/functions.R")

ui <- fluidPage(
  phyOutput("tree")
)

server <- function(input, output, session) {
  set.seed(37)
  
  tree_df <- random_tree_df(10)
  
  vals <- reactiveValues()

  phyOutputServer("tree", tree_df, source = "phy")
  click_data <- reactive(event_data("plotly_click", source = "phy"))
  
  observeEvent(click_data(),{
    click_df <- click_data()
    node <- click_df$customdata
    if(!is.null(node)){
     clade_df <- offspring(tree_df, node)
     print(clade_df)
    }
   })
}

shinyApp(ui, server)
