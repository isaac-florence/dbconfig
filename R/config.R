#' DB objects
#' 
#' Turn config into graph to be displayed by the app
db_objects <- function(config_file) {

    config <- yaml::read_yaml(config_file)

    # empty df for binding - not sure rbind should work this way
    db_specs <- data.frame()
    for (db in config$database) {

        db_objs <- data.frame()
        for (object in db$objects) {
            db_objs <- dplyr::bind_rows(data.frame(rbind(object)), db_objs)
            rownames(db_objs) <- NULL
        }

        #populate upper-loop vals into collected df
        db_objs$db_name <- db$name
        db_objs$db_alias <- db$alias
        # bind each db's objects
        db_specs <- dplyr::bind_rows(db_specs, db_objs)
    }

    # allow multiple dependencies
    db_specs <- db_specs |>
        tidyr::unnest_longer(dependencies) |>
        tidyr::unnest_wider(dependencies)

    # disallow remaining list cols
    db_specs <- db_specs |> 
        dplyr::mutate(dplyr::across(where(is.list), as.character))

    # create edges df
    db_edges <- db_specs |>
        dplyr::rename(
            to = "name",
            from = "depname",
            label = "why"
        ) |>
        dplyr::select(from, to, label, db_name)

    # dedupe nodes df
    db_specs <- db_specs |>
        dplyr::select(-depname, -why) |>
        dplyr::distinct()

    # create visnetwork required cols
    db_specs <- db_specs |>
        dplyr::mutate(
            id = name,
            label = name,
            shape = dplyr::case_when(
                kind == "view" ~ "diamond",
                kind == "table" ~ "square"
            ),
            title = paste0("DB name: ", db_name)
        )

    # get groupings of related objects
    components <- igraph::graph_from_data_frame(
        dplyr::filter(
            db_edges,
            !is.na(from),
            !is.na(to)
        ),
        vertices = db_specs,
        directed = TRUE
    ) |>
        igraph::clusters()

    db_obj_groups <- data.frame(
        name     = names(components$membership),
        group_id = seq_along(components$csize),
        group_sz = components$csize
    )
    db_specs <- dplyr::left_join(
        db_specs,
        db_obj_groups,
        "name"
    )

    # create graph obj
    graph <- list(
        nodes = db_specs,
        edges = db_edges
    )

    return(graph)
}

#' Get DBs
#' 
#' Get char vector of database alias names for inclusion in the UI
get_dbs <- function(config_file){
    dbs <- c()

    for (i in yaml::read_yaml(config_file)[["database"]]) {
        dbs <- c(dbs, i$alias)
    }

    return(dbs)
}