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


random_occ_df <- function(tree_df){
  require(dplyr)
  
  n <- sum(tree_df$isTip)
  sp <- tree_df$label[tree_df$isTip]
  
  n_grp <- round(n/5)
  n_grp <- ifelse(n_grp > 1, n_grp, 2)
  
  k_grp <- kmeans(cophenetic(as.phylo(tree_df)), n_grp)$cluster
  
  k_list <- lapply(unique(k_grp), function(i) names(k_grp)[k_grp == i])
  
  l_occ_df_rdm <- list()
  idx <- 1
  for(k in seq_along(k_list)){
    x <- runif(1, 1, 20)
    y <- runif(1, 1, 20)
    
    for(i in seq_along(k_list[[k]])){
      
      sp <- k_list[[k]][i]
      x_norm <- rnorm(75, x, runif(1, 1, 5)) |> round(0)
      y_norm <- rnorm(75, y, runif(1, 1, 5)) |> round(0) 
      
      l_occ_df_rdm[[idx]] <- data.frame(x = x_norm, y = y_norm, species = sp)
      idx <- idx + 1
    }
  }

  
  l_occ_df_rdm %>%
    bind_rows() %>% 
    filter(x %in% 1:20, y %in% 1:20)
  
}
