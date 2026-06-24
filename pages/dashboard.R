dashboard_ui <- function() {
  div(
    class = "stats-page",
    style = "padding-bottom: 10px;",
    h2("California Housing Prices"),
    p("Um clássico dataset para treinamento de modelos de regressão"),
    div(class = "stats-table-wrapper",
        tableOutput("tabela_colunas"))
  )
}

dashboard_server <- function(input, output, session) {
  output$tabela_colunas <- renderTable(
    col_desc,
    rownames = FALSE, bordered = TRUE,
    sanitize.text.function = identity
  )
}
