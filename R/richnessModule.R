richnessUI <- function(id) {
  ns <- NS(id)
  tagList(
  plotlyOutput(ns("map"))
  )
}

richnessServer <- function(id, tree_df, occ_df) {
  stopifnot(!is.reactive(tree_df))
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$map <- renderPlotly({
       
        max_rich <- occ_df %>% 
          count(x, y, name = "rich") %>% 
          pull(rich) %>% 
          max()
        
        limits <- list(x = range(occ_df$x), y = range(occ_df$y))
        
        selected_tips <- tree_df %>% 
          filter(isTip, node_color == "selected") %>% 
          pull(label)
        
        if(length(selected_tips) != 0){
          gg <- 
            occ_df %>% 
            filter(species %in% selected_tips) %>% 
            count(x, y, name = "rich") %>% 
            ggplot(aes(x, y, fill = rich)) +
            geom_raster() +
            scale_fill_continuous(
              name = "Richness",
              type = "viridis", 
              limits = c(0, max_rich)) +
            coord_equal(xlim = limits$x, ylim = limits$y) +
            theme_bw() 
          
          ggplotly(gg)
        }
        
    })
    }
  )
}