# =============================================================================
# app.R — BCN Habitatge · Shiny Dashboard
# 5 pàgines: Inici · Evolució · Mapa · Accessibilitat · Hipòtesis
# =============================================================================
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(DT)

source("utils.R")
source("modules/mod_evolucio.R")
source("modules/mod_mapa.R")
source("modules/mod_renda.R")
source("modules/mod_hipotesis.R")

# Càrrega de dades (una sola vegada a l'inici)
df_hab   <- load_data()
ipc_data <- load_ipc()

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",

  # ── Header ──
  dashboardHeader(
    title = tags$span(
      style = "font-family:'DM Sans','Inter',sans-serif; font-weight:700; font-size:15px; letter-spacing:-.3px;",
      tags$span(style = "color:#3b82f6;", "BCN"),
      tags$span(style = "color:#f8fafc;", " Habitatge")
    ),
    titleWidth = 230
  ),

  # ── Sidebar ──
  dashboardSidebar(
    width = 230,
    tags$div(
      class = "sidebar-section-label",
      "Anàlisi"
    ),
    sidebarMenu(
      id = "menu",
      menuItem("Inici",          tabName = "inici",      icon = icon("house")),
      menuItem("Evolució",       tabName = "evolucio",   icon = icon("chart-line")),
      menuItem("Mapa de preus",  tabName = "mapa",       icon = icon("map-location-dot")),
      menuItem("Accessibilitat", tabName = "renda",      icon = icon("scale-balanced")),
      menuItem("Hipòtesis",      tabName = "hipotesis",  icon = icon("flask"))
    ),
    tags$div(
      style = "padding:12px 14px; margin-top:8px;",
      tags$div(
        style = "background:rgba(139,92,246,.12); border:1px solid rgba(139,92,246,.2); border-radius:10px; padding:10px 12px;",
        tags$div(style = "font-size:10px; font-weight:700; text-transform:uppercase; letter-spacing:.08em; color:#7c3aed; margin-bottom:6px;",
                 tags$i(class = "fa-solid fa-flask", style = "margin-right:4px;"),
                 "Hipòtesis"),
        tags$div(style = "font-size:11px; color:#8b5cf6; line-height:1.5;",
          tags$div("H1: Preu >> IPC"),
          tags$div("H2: Bretxa territorial"),
          tags$div("H3: Nou vs. Usat")
        )
      )
    ),
    tags$div(
      style = "position:absolute; bottom:0; left:0; right:0; padding:14px; border-top:1px solid #1e293b;",
      tags$div(
        style = "font-size:10px; color:#475569; line-height:1.7;",
        "Font: Ajuntament de Barcelona", tags$br(),
        "Registre de la Propietat · INE", tags$br(),
        tags$span(style = "color:#334155;", "2014 – 2025 Q3")
      )
    )
  ),

  # ── Body ──
  dashboardBody(
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700;800&display=swap"),
      tags$style(HTML(CSS_GLOBAL))
    ),
    tabItems(
      tabItem("inici",     mod_inici_ui(df_hab, ipc_data)),
      tabItem("evolucio",  mod_evolucio_ui("evolucio")),
      tabItem("mapa",      mod_mapa_ui("mapa")),
      tabItem("renda",     mod_renda_ui("renda")),
      tabItem("hipotesis", mod_hipotesis_ui("hipotesis"))
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  mod_evolucio_server("evolucio",  df_hab)
  mod_mapa_server("mapa",          df_hab)
  mod_renda_server("renda",        df_hab)
  mod_hipotesis_server("hipotesis", df_hab, ipc_data)
}

shinyApp(ui, server)
