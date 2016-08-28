#' table output.
#' @param data data with radios embeded.
#' @export
output_table <- function(data) {
   # colnames(data) = col_names
    options_table <- list(dom = 't', paging = FALSE, ordering = FALSE)
    table <- DT::renderDataTable(
        data, escape = FALSE, selection = 'none', server = FALSE,
        options = options_table
    )
    return(table)
}
