library(shiny)


phyOutput <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("tree"))
  )
}


phyOutputServer <- function(id, tree_df, source = "phy") {
  stopifnot(!is.reactive(tree_df))
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$tree <- renderPlotly({
        
        g <- ggplot(tree_df, aes(x, y)) +
        geom_point(
          aes(
            text = paste("Node:", node),
            customdata = node
            )
          ) +
          geom_tree() +
          theme_tree2()
          
        
       ggplotly(g, source = source, tooltip = "text") %>%   
         layout(dragmode = "select") %>%
         event_register("plotly_selecting")
       
      })
    }
  )
}