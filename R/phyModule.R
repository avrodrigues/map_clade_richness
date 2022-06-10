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
        
        g <- ggplot(tree_df, aes(x, y, color = parent_color)) +
          geom_tree() +
          theme_tree2() +
        geom_point(
          aes(
            color = node_color,
            text = paste("Node:", node),
            customdata = node
            ),
          size = 2
          ) +
          scale_color_manual(
            values = c(unselected = "black", selected = "#6194A5") 
          ) +
          
          theme(
            legend.position = 'none'
          )
          
        
       ggplotly(g, source = source, tooltip = "text") %>%   
         layout(dragmode = "select") %>%
         event_register("plotly_selecting")
       
      })
    }
  )
}





  

