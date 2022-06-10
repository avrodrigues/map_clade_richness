#' @export
generate_tree_df <- function(tree = NULL, n = NULL){
  
  require(ape)
  require(ggtree)
  require(treeio)
  
  
  if(is.null(tree)){
    if(is.null(n)) stop("provide a number of tips")
    tree <- ape::rtree(n)
    tree <- treeio::as.ultrametric(tree)
  }

  
  phy_plot <- ggtree(tree)
  
  phy_plot$data %>% 
    mutate(
      x = x-max(x),
      node_color = "unselected", 
      parent_color = "unselected"
    ) 
}


select_clade <- function(tree_df, click_data) {
  stopifnot(!is.reactive(tree_df))
  stopifnot(!is.reactive(click_data))
  
  tree_df <- 
    tree_df %>% 
    mutate(
      node_color = "unselected"
    ) 
  
  if(is.null(click_data) | is.null(click_data$customdata)){
    clade_df <- NULL
    clicked_node <- NULL
  }
  

  if(!is.null(click_data) & !is.null(click_data$customdata)){
    clicked_node <- click_data$customdata
    clade_df <- offspring(tree_df, clicked_node)
      
  }
  
  
  tree_df <- 
    tree_df %>% 
    mutate(
      node_color = if_else(
        node %in% c(clade_df$node, clicked_node), 
        "selected", 
        node_color
        )
    )
  
  root_node <- sum(tree_df$isTip) + 1
  
  tree_df <- 
    tree_df %>% 
    mutate(
      parent_color = map_chr(node, function(x){ 
        if(x == root_node) {
          return(tree_df %>% filter(node == root_node) %>% pull(node_color))
        }
        parent(tree_df, x) %>% 
          pull(node_color)
      })
    )
  
  tree_df
}
