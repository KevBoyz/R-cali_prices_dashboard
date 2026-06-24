estatistica_ui <- function() {
  div(
    class = "stats-page",
    h2("Estatística Descritiva"),
    p("Resumo estatístico de todas as variáveis do dataset."),
    div(class = "stats-table-wrapper",
        tableOutput("tabela_estatistica"))
  )
}

estatistica_server <- function(input, output, session) {
  output$tabela_estatistica <- renderTable(
    build_stats_table(housing),
    rownames = TRUE, bordered = TRUE,
    sanitize.text.function = identity
  )
}
