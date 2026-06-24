library(shiny)

# ── Dados ──────────────────────────────────────────────────────────────────
housing <- read.csv("housing.csv", stringsAsFactors = FALSE)

# ── Helpers ────────────────────────────────────────────────────────────────
calc_mode <- function(x) {
  tab      <- table(x)
  max_freq <- max(tab)
  if (max_freq <= 1) return("Sem moda")
  modes <- names(tab[tab == max_freq])
  if (length(modes) == length(tab)) return("Sem moda")
  paste(modes, collapse = ", ")
}

fmt <- function(x) format(round(x, 4), nsmall = 4)

build_stats_table <- function(df) {
  stat_fns <- list(
    "Média"              = function(x) if (is.numeric(x)) fmt(mean(x, na.rm = TRUE))                          else "—",
    "Mediana"            = function(x) if (is.numeric(x)) fmt(median(x, na.rm = TRUE))                        else "—",
    "Moda"               = function(x) calc_mode(x),
    "Variância"          = function(x) if (is.numeric(x)) fmt(var(x, na.rm = TRUE))                           else "—",
    "Desvio Padrão"      = function(x) if (is.numeric(x)) fmt(sd(x, na.rm = TRUE))                            else "—",
    "Coef. Variação (%)" = function(x) if (is.numeric(x)) fmt(sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE) * 100)  else "—",
    "Q1 (25%)"           = function(x) if (is.numeric(x)) fmt(quantile(x, .25, na.rm = TRUE))                 else "—",
    "Q2 (50%)"           = function(x) if (is.numeric(x)) fmt(quantile(x, .50, na.rm = TRUE))                 else "—",
    "Q3 (75%)"           = function(x) if (is.numeric(x)) fmt(quantile(x, .75, na.rm = TRUE))                 else "—",
    "P10"                = function(x) if (is.numeric(x)) fmt(quantile(x, .10, na.rm = TRUE))                 else "—",
    "P25"                = function(x) if (is.numeric(x)) fmt(quantile(x, .25, na.rm = TRUE))                 else "—",
    "P75"                = function(x) if (is.numeric(x)) fmt(quantile(x, .75, na.rm = TRUE))                 else "—",
    "P90"                = function(x) if (is.numeric(x)) fmt(quantile(x, .90, na.rm = TRUE))                 else "—"
  )
  mat <- sapply(stat_fns, function(fn) sapply(df, fn))
  as.data.frame(t(mat), stringsAsFactors = FALSE)
}

# ── Variáveis compartilhadas ───────────────────────────────────────────────
num_vars  <- names(housing)[sapply(housing, is.numeric)]
cat_vars  <- names(housing)[!sapply(housing, is.numeric)]
all_vars  <- names(housing)
group_var <- if (length(cat_vars) > 0) cat_vars[1] else NULL  # "ocean_proximity"

# ── Paletas ────────────────────────────────────────────────────────────────
group_palette <- c("#2563eb", "#e63946", "#2a9d8f", "#f4a261",
                   "#6d28d9", "#0ea5e9", "#f59e0b", "#10b981")

color_choices <- c(
  "Azul"       = "#2563eb",
  "Azul Claro" = "#4a90d9",
  "Marinho"    = "#1a2847",
  "Ciano"      = "#0ea5e9",
  "Laranja"    = "#ff8c00",
  "Rosa"       = "#ff69b4"
)

# ── Descrição das colunas ──────────────────────────────────────────────────
col_desc <- data.frame(
  `Variável` = c(
    "longitude", "latitude", "housing_median_age",
    "total_rooms", "total_bedrooms", "population",
    "households", "median_income", "median_house_value", "ocean_proximity"
  ),
  `Descrição` = c(
    "Longitude geográfica do bloco habitacional",
    "Latitude geográfica do bloco habitacional",
    "Idade mediana das residências no bloco (em anos)",
    "Número total de cômodos no bloco",
    "Número total de quartos no bloco",
    "População total residente no bloco",
    "Número de domicílios (famílias) no bloco",
    "Renda mediana dos domicílios (em dezenas de milhares de USD)",
    "Valor mediano dos imóveis no bloco (em USD)",
    "Categoria de proximidade ao oceano (NEAR BAY, INLAND, etc.)"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
