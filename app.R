source("R/functions.R")

ui <- fluidPage(
  column(
    6, 
    phyOutput("tree")
    ), 
  column(
    6, 
    richnessUI("richness")
  )
  
)

server <- function(input, output, session) {
  
  vals <- reactiveValues()
  n = 100
  set.seed(1006)
  vals$tree_df <- generate_tree_df(n = n)
  set.seed(1006)
  t_df <- generate_tree_df(n = n)
  occ_df_rdm <- random_occ_df(t_df)
  
  observeEvent(vals$tree_df, {
    phyOutputServer("tree", vals$tree_df, source = "phy")
    
    #set.seed(52)
    
    richnessServer("richness", vals$tree_df, occ_df_rdm)

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
