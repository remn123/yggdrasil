yggUI <- function(id, label = NULL)
{
  ns <- NS(id)
  tagList(
    visNetworkOutput(ns("net"))
  )
}

yggModule <- function(input, output, session)
{
  output$net <- renderVisNetwork({
    visNetwork(nodes, 
               edges, 
               main = "Create your tree", 
               submain = "Subtitle") %>%
      visExport() %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE, selected = "1"),
                 manipulation = TRUE) %>%
      visEdges(shadow = TRUE,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
               color = list(color = "lightblue", highlight = "red")) %>%
      visHierarchicalLayout() %>% 
      #visInteraction(navigationButtons = TRUE)
      visInteraction(keyboard = TRUE, tooltipDelay = 0)
    
  })
}