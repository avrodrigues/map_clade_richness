source("R/functions.R")

ui <- fluidPage(
  phyOutput("tree")
)

server <- function(input, output, session) {
  
  vals <- reactiveValues()
  set.seed(37)
  vals$tree_df <- random_tree_df(8)
  
  observeEvent(vals$tree_df, {
    phyOutputServer("tree", vals$tree_df, source = "phy")

  })
  click_data <- reactive(event_data("plotly_click", source = "phy"))
  
  observeEvent(click_data(), {
    click <- click_data()
    if(!is.null(click) & !is.null(click$customdata)){
      vals$tree_df <- select_clade(vals$tree_df, click)
      print(vals$tree_df)
    }
  })
  
}

shinyApp(ui, server)
