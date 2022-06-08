#' Visnetwork from config
#' 
#' Creat 

visconfig <- function(graph) {

    visnetwork <- visNetwork::visNetwork(
        graph$nodes, graph$edges
      )
    visnetwork <- visNetwork::visEdges(
        visnetwork,
        arrows = "to", width = 2,
        smooth = list(
            type = "cubicBezier",
            forceDirection = "horizontal"
          )
    )

    visnetwork <- visNetwork::visHierarchicalLayout(visnetwork, direction = "LR", sortMethod = "directed")
    visnetwork <- visNetwork::visOptions(visnetwork, highlightNearest = FALSE)
    visNetwork::visLegend(visnetwork, useGroups = FALSE)

    invisible(visnetwork)
}