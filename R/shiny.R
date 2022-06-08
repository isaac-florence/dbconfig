#' Shiny app up
#' 
#' Launch the shiny app
#' 
#' @param config file path
#' 
#' @export 
shiny_up <- function(config_file) {

    message("Loading `shiny` and `visNetwork` packages")
    library(shiny)
    library(visNetwork)
    server <- function(input, output) {

        # TODO refresh parsed config on hitting refresh in UI
        graph <- eventReactive(input$refresh, {
            db_objects(config_file) ## from shiny up
        })

        # TODO add filtering based on UI selection
        db <- reactive({input$db_using})

        output$db_graph <- renderVisNetwork({
            g <- list()
            incl_nodes <- c()

            db_nodes_f <- graph()$nodes |>
                dplyr::filter(db_alias == db())

            incl_nodes <- c(db_nodes_f$name)
            for (node in db_nodes_f$name) {
            
                parents <- graph()$edges |>
                    dplyr::filter(to == node)

                while(nrow(parents > 0)) {
                    incl_nodes <- c(incl_nodes, parents$from)

                    parents <- graph()$edges |>
                        dplyr::filter(to == parents$from & !is.na(from))
                }
            }

            g$nodes <- graph()$nodes |>
                dplyr::filter(name %in% incl_nodes) |>
                dplyr::mutate(
                    color = dplyr::case_when(
                        db_alias == db() ~ "#003B5c",
                        db_alias != db() ~ "#1D57A5"
                    )
                )

            g$edges <- graph()$edges |>
                dplyr::filter(from %in% incl_nodes)

            vis <- visconfig(g)
        })
    }

    ui <- fluidPage(
        theme = bslib::bs_theme(
            bootswatch = "flatly",
            primary = "#007C91",
            secondary = "#003B5C",
            base_font = "arial",
            code_font = "monospace",
            heading_font = "helvetica"
        ),
        titlePanel("DB configuration"),
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    "db_using",
                    "Which database are using:",
                    get_dbs(config_file) ## from shiny up
                ),
                hr(),
                helpText("Click 'Refresh graph' to first see the graph.",
                    "If you return to the page after a config update,",
                    "refreshing the graph will update it."
                ),
                actionButton(
                    "refresh",
                    "Refresh graph",
                    icon("redo")
                ),
                width = 3
            ),
            mainPanel(
                visNetworkOutput("db_graph", height = "720px", width = "1000px")
            )
        )
    )

    shinyApp(ui = ui, server = server)

}