# ── Visualizações ──────────────────────────────────────────────────────────
# Seis gráficos interativos: histograma, boxplot, barras, dispersão, setores e heatmap.

# ── Helpers de layout (usados só aqui) ────────────────────────────────────
viz_block <- function(sidebar_content, plot_id, titulo) {
  div(
    class = "viz-block",
    div(class = "viz-sidebar", h4(titulo), sidebar_content),
    div(class = "viz-plot box", plotOutput(plot_id, height = "340px"))
  )
}

grp_label <- function(prefix) {
  paste(prefix, if (!is.null(group_var)) group_var else "—")
}

# ── UI ──────────────────────────────────────────────────────────────────────
visualizacoes_ui <- function() {
  div(
    class = "stats-page",
    h2("Visualizações"),
    p("Explore os dados do dataset Housing com diferentes tipos de gráficos."),

    # 1. Histograma
    viz_block(
      tagList(
        selectInput("hist_var",   "Variável:",       choices = num_vars, selected = num_vars[1]),
        sliderInput("hist_bins",  "Número de bins:", min = 3, max = 60, value = 15),
        selectInput("hist_color", "Cor:",            choices = color_choices),
        checkboxInput("hist_density", "Mostrar curva de densidade", value = FALSE)
      ),
      "plot_hist", "Histograma"
    ),

    # 2. Boxplot
    viz_block(
      tagList(
        selectInput("box_var",   "Variável Y:", choices = num_vars, selected = num_vars[1]),
        checkboxInput("box_group", grp_label("Agrupar por"), value = FALSE),
        selectInput("box_color", "Cor:", choices = color_choices),
        checkboxInput("box_notch", "Notch (IC da mediana)", value = FALSE)
      ),
      "plot_box", "Boxplot"
    ),

    # 3. Gráfico de Barras
    viz_block(
      tagList(
        selectInput("bar_var",  "Variável numérica:", choices = num_vars, selected = num_vars[1]),
        selectInput("bar_stat", "Estatística:",
                    choices = c("Média" = "mean", "Mediana" = "median",
                                "Máximo" = "max",  "Mínimo" = "min")),
        selectInput("bar_color", "Cor:", choices = color_choices),
        checkboxInput("bar_horiz", "Horizontal", value = FALSE)
      ),
      "plot_bar", "Gráfico de Barras"
    ),

    # 4. Gráfico de Dispersão
    viz_block(
      tagList(
        selectInput("disp_x", "Eixo X:", choices = num_vars, selected = num_vars[1]),
        selectInput("disp_y", "Eixo Y:", choices = num_vars, selected = num_vars[2]),
        checkboxInput("disp_group", grp_label("Colorir por"), value = !is.null(group_var)),
        checkboxInput("disp_lm",  "Linha de regressão", value = FALSE),
        sliderInput("disp_cex", "Tamanho dos pontos:", min = 0.3, max = 2, value = 0.6, step = 0.1)
      ),
      "plot_disp", "Gráfico de Dispersão"
    ),

    # 5. Gráfico de Setores
    viz_block(
      tagList(
        selectInput("pie_var",  "Variável numérica:", choices = num_vars, selected = num_vars[1]),
        selectInput("pie_stat", "Estatística por categoria:",
                    choices = c("Soma" = "sum", "Média" = "mean", "Contagem" = "n")),
        checkboxInput("pie_pct", "Mostrar porcentagens", value = TRUE)
      ),
      "plot_pie", "Gráfico de Setores"
    ),

    # 6. Heatmap de Correlação
    viz_block(
      tagList(
        checkboxGroupInput("heat_vars", "Variáveis:", choices = num_vars, selected = num_vars),
        selectInput("heat_color", "Paleta:",
                    choices = c("Vermelho-Azul" = "RdBu", "Verde-Vermelho" = "RdYlGn",
                                "Roxo-Verde"    = "PRGn", "Laranja-Roxo"   = "PuOr")),
        checkboxInput("heat_values", "Exibir valores", value = TRUE)
      ),
      "plot_heat", "Heatmap de Correlação"
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
visualizacoes_server <- function(input, output, session) {

  # 1. Histograma
  output$plot_hist <- renderPlot({
    req(input$hist_var, input$hist_bins, input$hist_color)
    x <- housing[[input$hist_var]]
    hist(x, breaks = input$hist_bins, col = input$hist_color, border = "white",
         main = paste("Histograma de", input$hist_var),
         xlab = input$hist_var, ylab = "Frequência", las = 1)
    if (isTRUE(input$hist_density)) {
      d <- density(x, na.rm = TRUE)
      par(new = TRUE)
      plot(d, axes = FALSE, ann = FALSE, col = "#1a2847", lwd = 2)
    }
  })

  # 2. Boxplot
  output$plot_box <- renderPlot({
    req(input$box_var, input$box_color)
    if (isTRUE(input$box_group) && !is.null(group_var)) {
      n_grp <- length(unique(housing[[group_var]]))
      cols  <- group_palette[seq_len(n_grp)]
      par(mar = c(8, 4, 4, 2))
      boxplot(as.formula(paste(input$box_var, "~", group_var)),
              data  = housing, col = cols, border = "#333333",
              notch = isTRUE(input$box_notch),
              main  = paste("Boxplot de", input$box_var, "por", group_var),
              xlab  = "", ylab = input$box_var, las = 2)
    } else {
      boxplot(housing[[input$box_var]],
              col = input$box_color, border = "#333333",
              notch = isTRUE(input$box_notch),
              main  = paste("Boxplot de", input$box_var),
              ylab  = input$box_var, las = 1)
    }
  })

  # 3. Gráfico de Barras
  output$plot_bar <- renderPlot({
    req(input$bar_var, input$bar_stat, input$bar_color)
    if (is.null(group_var)) {
      plot.new(); text(0.5, 0.5, "Nenhuma variável categórica disponível.", cex = 1.4); return()
    }
    fn   <- switch(input$bar_stat, mean = mean, median = median, max = max, min = min)
    vals <- tapply(housing[[input$bar_var]], housing[[group_var]], fn, na.rm = TRUE)
    par(mar = c(9, 5, 4, 2))
    barplot(vals,
            col   = input$bar_color, border = "white",
            horiz = isTRUE(input$bar_horiz),
            main  = paste(input$bar_stat, "de", input$bar_var, "por", group_var),
            xlab  = if (!isTRUE(input$bar_horiz)) "" else input$bar_var,
            ylab  = if (!isTRUE(input$bar_horiz)) input$bar_var else "",
            las   = 2)
  })

  # 4. Gráfico de Dispersão
  output$plot_disp <- renderPlot({
    req(input$disp_x, input$disp_y, input$disp_cex)
    if (isTRUE(input$disp_group) && !is.null(group_var)) {
      cats    <- sort(unique(housing[[group_var]]))
      n_cats  <- length(cats)
      pal     <- setNames(group_palette[seq_len(n_cats)], cats)
      col_vec <- adjustcolor(pal[housing[[group_var]]], alpha.f = 0.5)
    } else {
      col_vec <- adjustcolor("#2563eb", alpha.f = 0.4)
      pal     <- NULL
    }
    plot(housing[[input$disp_x]], housing[[input$disp_y]],
         col = col_vec, pch = 19, cex = input$disp_cex,
         main = paste("Dispersão:", input$disp_x, "×", input$disp_y),
         xlab = input$disp_x, ylab = input$disp_y, las = 1)
    if (isTRUE(input$disp_lm)) {
      abline(lm(housing[[input$disp_y]] ~ housing[[input$disp_x]]),
             col = "#1a2847", lwd = 2, lty = 2)
    }
    if (!is.null(pal)) {
      legend("topright", legend = names(pal), col = pal,
             pch = 19, bty = "n", pt.cex = 1.2)
    }
  })

  # 5. Gráfico de Setores
  output$plot_pie <- renderPlot({
    req(input$pie_var, input$pie_stat)
    if (is.null(group_var)) {
      plot.new(); text(0.5, 0.5, "Nenhuma variável categórica disponível.", cex = 1.4); return()
    }
    vals <- if (input$pie_stat == "n") {
      table(housing[[group_var]])
    } else {
      fn <- if (input$pie_stat == "sum") sum else mean
      tapply(housing[[input$pie_var]], housing[[group_var]], fn, na.rm = TRUE)
    }
    n_cats <- length(vals)
    cores  <- group_palette[seq_len(n_cats)]
    labels <- names(vals)
    if (isTRUE(input$pie_pct)) {
      pcts   <- round(100 * vals / sum(vals), 1)
      labels <- paste0(labels, "\n", pcts, "%")
    }
    pie(vals, labels = labels, col = cores,
        main = paste("Setores:",
                     if (input$pie_stat == "n") "Contagem" else paste(input$pie_stat, "de", input$pie_var)))
  })

  # 6. Heatmap de Correlação
  output$plot_heat <- renderPlot({
    req(input$heat_vars, input$heat_color)
    vars <- input$heat_vars
    if (length(vars) < 2) {
      plot.new(); text(0.5, 0.5, "Selecione ao menos 2 variáveis.", cex = 1.4); return()
    }
    corr_mat <- cor(housing[, vars, drop = FALSE], use = "complete.obs")
    pal <- switch(input$heat_color,
                  "RdBu"   = colorRampPalette(c("#d73027", "#f7f7f7", "#4575b4"))(100),
                  "RdYlGn" = colorRampPalette(c("#d73027", "#ffffbf", "#1a9850"))(100),
                  "PRGn"   = colorRampPalette(c("#762a83", "#f7f7f7", "#1b7837"))(100),
                  "PuOr"   = colorRampPalette(c("#b35806", "#f7f7f7", "#542788"))(100),
                  colorRampPalette(c("#d73027", "#f7f7f7", "#4575b4"))(100)
    )
    nr <- nrow(corr_mat); nc <- ncol(corr_mat)
    par(mar = c(8, 8, 4, 2))
    image(1:nc, 1:nr, t(corr_mat)[, nr:1],
          col = pal, zlim = c(-1, 1), axes = FALSE,
          main = "Heatmap de Correlação", xlab = "", ylab = "")
    axis(1, at = 1:nc, labels = colnames(corr_mat), las = 2, cex.axis = 0.85)
    axis(2, at = 1:nr, labels = rev(rownames(corr_mat)), las = 1, cex.axis = 0.85)
    if (isTRUE(input$heat_values)) {
      for (i in 1:nr) for (j in 1:nc) {
        val <- corr_mat[i, j]
        text(j, nr + 1 - i, labels = round(val, 2),
             col = if (abs(val) > 0.5) "white" else "#333333", cex = 0.9)
      }
    }
  })
}
