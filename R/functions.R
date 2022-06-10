#' @export
random_tree_df <- function(n){
  require(ape)
  require(ggtree)
  require(treeio)
  
  tree <- ape::rtree(n)
  tree <- as.ultrametric(tree)
  
  phy_plot <- ggtree(tree)
  
  phy_plot$data
}
