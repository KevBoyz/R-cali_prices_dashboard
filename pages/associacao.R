discretizar <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  q <- unique(quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
  if (length(q) < 2) return(as.character(x))
  as.character(cut(x, breaks = q, include.lowest = TRUE,
                   labels = paste0("Q", seq_len(length(q) - 1))))
}

assoc_block <- function(sidebar_content, main_content, titulo) {
  div(
    class = "viz-block",
    div(class = "viz-sidebar", h4(titulo), sidebar_content),
    div(class = "viz-plot box", main_content)
  )
}

associacao_ui <- function() {
  div(
    class = "stats-page",
    h2("Análise de Associação"),
    p("Explore relações entre variáveis do dataset Housing."),

    # 1. Gráfico de Correlação
    assoc_block(
      tagList(
        checkboxGroupInput("corr_vars", "Variáveis:",
                           choices = num_vars, selected = num_vars),
        radioButtons("corr_method", "Método:",
                     choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                     inline = TRUE),
        checkboxInput("corr_values", "Exibir valores nas células", value = TRUE)
      ),
      plotOutput("plot_corr", height = "420px"),
      "Gráfico de Correlação"
    ),

    # 2. Tabela de Contingência
    assoc_block(
      tagList(
        selectInput("cont_varA", "Variável A:", choices = all_vars, selected = all_vars[1]),
        selectInput("cont_varB", "Variável B:", choices = all_vars, selected = all_vars[2]),
        checkboxInput("cont_total",   "Linha e coluna de Total", value = TRUE),
        checkboxInput("cont_heatmap", "Mapa de calor",           value = FALSE),
        tags$small(style = "color:#888;",
                   "Variáveis numéricas são discretizadas em quartis (Q1–Q4).")
      ),
      uiOutput("tabela_contingencia"),
      "Tabela de Contingência"
    ),

    # 3. Teste Qui-Quadrado
    assoc_block(
      tagList(
        selectInput("chi_varA", "Variável A:", choices = all_vars, selected = all_vars[1]),
        selectInput("chi_varB", "Variável B:", choices = all_vars, selected = all_vars[2]),
        selectInput("chi_alpha", "Nível de significância (α):",
                    choices = c("1%  (α = 0.01)" = "0.01",
                                "5%  (α = 0.05)" = "0.05",
                                "10% (α = 0.10)" = "0.10"),
                    selected = "0.05"),
        tags$small(style = "color:#888;",
                   "Premissa: frequências esperadas ≥ 5 em todas as células.")
      ),
      uiOutput("resultado_chi"),
      "Teste Qui-Quadrado"
    )
  )
}

associacao_server <- function(input, output, session) {

  output$plot_corr <- renderPlot({
    req(input$corr_vars, input$corr_method)
    vars <- input$corr_vars
    if (length(vars) < 2) {
      plot.new()
      text(0.5, 0.5, "Selecione ao menos 2 variáveis.", cex = 1.4)
      return()
    }
    method_label <- if (input$corr_method == "pearson") "Pearson" else "Spearman"
    corr_mat <- cor(housing[, vars, drop = FALSE],
                    use = "complete.obs", method = input$corr_method)
    pal <- colorRampPalette(c("#d73027", "#f7f7f7", "#4575b4"))(100)
    nr  <- nrow(corr_mat)
    nc  <- ncol(corr_mat)
    par(mar = c(8, 8, 4, 2))
    image(1:nc, 1:nr, t(corr_mat)[, nr:1],
          col = pal, zlim = c(-1, 1), axes = FALSE,
          main = paste("Correlação de", method_label), xlab = "", ylab = "")
    axis(1, at = 1:nc, labels = colnames(corr_mat), las = 2, cex.axis = 0.85)
    axis(2, at = 1:nr, labels = rev(rownames(corr_mat)), las = 1, cex.axis = 0.85)
    if (isTRUE(input$corr_values)) {
      for (i in seq_len(nr)) for (j in seq_len(nc)) {
        val <- corr_mat[i, j]
        text(j, nr + 1 - i, labels = round(val, 2),
             col = if (abs(val) > 0.5) "white" else "#333333", cex = 0.85)
      }
    }
  })

  output$tabela_contingencia <- renderUI({
    req(input$cont_varA, input$cont_varB)

    vA  <- discretizar(housing[[input$cont_varA]])
    vB  <- discretizar(housing[[input$cont_varB]])
    tab <- table(A = vA, B = vB)

    if (isTRUE(input$cont_total)) tab <- addmargins(tab)

    nr      <- nrow(tab); nc <- ncol(tab)
    row_nms <- rownames(tab); col_nms <- colnames(tab)

    body_vals <- tab[row_nms != "Sum", col_nms != "Sum", drop = FALSE]
    max_val   <- max(body_vals, 1)

    cell_bg <- function(val, is_total) {
      if (!isTRUE(input$cont_heatmap) || is_total) return("")
      intensity <- val / max_val
      r <- round(255 - intensity * 100)
      g <- round(255 - intensity * 100)
      sprintf("background-color:rgb(%d,%d,255);", r, g)
    }

    th_style <- "padding:5px 10px; border:1px solid #ddd; text-align:center;"
    td_style <- "padding:5px 10px; border:1px solid #ddd; text-align:center;"

    header <- paste0(
      "<thead><tr>",
      sprintf('<th style="%s"></th>', th_style),
      paste(sprintf('<th style="%s">%s</th>', th_style, col_nms), collapse = ""),
      "</tr></thead>"
    )

    body_rows <- sapply(seq_len(nr), function(i) {
      is_total_row <- row_nms[i] == "Sum"
      row_bg       <- if (is_total_row) "background:#f0f0f0;" else ""
      row_fw       <- if (is_total_row) "font-weight:bold;" else ""

      cells <- sapply(seq_len(nc), function(j) {
        is_total_col <- col_nms[j] == "Sum"
        is_total     <- is_total_row || is_total_col
        col_fw       <- if (is_total) "font-weight:bold;" else ""
        bg           <- cell_bg(tab[i, j], is_total)
        sprintf('<td style="%s%s%s">%s</td>',
                td_style, col_fw, bg,
                format(tab[i, j], big.mark = ","))
      })

      sprintf('<tr style="%s%s"><th style="%s%s%s">%s</th>%s</tr>',
              row_bg, row_fw,
              th_style, row_bg, row_fw,
              row_nms[i], paste(cells, collapse = ""))
    })

    HTML(paste0(
      '<div style="overflow-x:auto; padding:8px;">',
      sprintf('<p style="font-size:0.82em; color:#888; margin-bottom:6px;">A: %s &nbsp;|&nbsp; B: %s</p>',
              input$cont_varA, input$cont_varB),
      '<table style="border-collapse:collapse; font-size:0.84em; width:100%;">',
      header, "<tbody>", paste(body_rows, collapse = ""), "</tbody>",
      "</table></div>"
    ))
  })

  output$resultado_chi <- renderUI({
    req(input$chi_varA, input$chi_varB, input$chi_alpha)
    alpha <- as.numeric(input$chi_alpha)

    # Variáveis iguais
    if (input$chi_varA == input$chi_varB) {
      return(div(style = "padding:14px;",
                 div(style = "border-left:4px solid #b45309; padding:10px;
                              background:#fffbeb; border-radius:4px; color:#92400e;",
                     tags$strong("⚠ Selecione duas variáveis diferentes."))))
    }

    vA  <- discretizar(housing[[input$chi_varA]])
    vB  <- discretizar(housing[[input$chi_varB]])
    tab <- table(A = vA, B = vB)

    expected <- tryCatch(
      suppressWarnings(chisq.test(tab, correct = FALSE)$expected),
      error = function(e) NULL
    )

    if (is.null(expected)) {
      return(div(style = "padding:14px; color:#b91c1c;",
                 "Erro ao calcular frequências esperadas. Verifique as variáveis selecionadas."))
    }

    n_invalid   <- sum(expected < 5)
    premissa_ok <- n_invalid == 0

    if (!premissa_ok) {
      pct <- round(100 * n_invalid / length(expected), 1)
      return(div(
        style = "padding:14px;",
        div(style = "border-left:4px solid #b91c1c; padding:10px;
                     background:#fef2f2; border-radius:4px; color:#991b1b; font-size:0.88em;",
            tags$strong("✗ Premissa não atendida — teste não executado."),
            tags$br(), tags$br(),
            sprintf("%d célula(s) (%.1f%%) com frequência esperada < 5.", n_invalid, pct),
            tags$br(), tags$br(),
            tags$em("Sugestão: agrupe categorias raras ou escolha outras variáveis."))
      ))
    }

    result   <- suppressWarnings(chisq.test(tab, correct = FALSE))
    chi2     <- round(result$statistic, 4)
    df_val   <- result$parameter
    p_val    <- result$p.value
    rejeitar <- p_val < alpha

    cor_borda <- if (rejeitar) "#166534" else "#991b1b"
    bg_cor    <- if (rejeitar) "#f0fdf4"  else "#fef2f2"
    conclusao <- if (rejeitar) {
      sprintf("Rejeita-se H₀ (p = %.4f < α = %.2f): há evidência de associação entre as variáveis.",
              p_val, alpha)
    } else {
      sprintf("Não se rejeita H₀ (p = %.4f ≥ α = %.2f): sem evidência de associação.",
              p_val, alpha)
    }

    td1 <- "padding:5px 8px; font-weight:bold; border-bottom:1px solid #eee;"
    td2 <- "padding:5px 8px; border-bottom:1px solid #eee;"
    tr_alt <- "background:#fafafa;"

    div(
      style = "padding:14px; font-size:0.9em;",
      div(style = "color:#166534; font-weight:bold; margin-bottom:12px;",
          "✔ Premissa atendida"),
      tags$table(
        style = "border-collapse:collapse; width:100%; margin-bottom:14px;",
        tags$tr(
          tags$td(style = td1, "Estatística χ²"),
          tags$td(style = td2, chi2)
        ),
        tags$tr(style = tr_alt,
                tags$td(style = td1, "Graus de liberdade"),
                tags$td(style = td2, df_val)
        ),
        tags$tr(
          tags$td(style = td1, "p-valor"),
          tags$td(style = td2, formatC(p_val, format = "e", digits = 4))
        ),
        tags$tr(style = tr_alt,
                tags$td(style = td1, "Nível de significância (α)"),
                tags$td(style = td2, alpha)
        ),
        tags$tr(
          tags$td(style = gsub("border-bottom:1px solid #eee;", "", td1), "N (observações)"),
          tags$td(style = gsub("border-bottom:1px solid #eee;", "", td2),
                  format(sum(tab), big.mark = ","))
        )
      ),
      div(style = sprintf("border-left:4px solid %s; padding:10px; background:%s;
                           border-radius:4px; color:%s; font-size:0.88em;",
                          cor_borda, bg_cor, cor_borda),
          conclusao)
    )
  })
}
