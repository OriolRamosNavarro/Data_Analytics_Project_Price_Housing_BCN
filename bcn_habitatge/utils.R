# =============================================================================
# utils.R — Helpers, paletes, CSS global i pàgina d'inici
# =============================================================================
library(dplyr); library(readr); library(ggplot2); library(scales)

# ── Càrrega de dades ──────────────────────────────────────────────────────────
load_data <- function() {
  p <- "data/processed/habitatge_long.csv"
  if (!file.exists(p)) stop("No s'ha trobat: ", p)
  read_csv(p, show_col_types = FALSE) %>%
    mutate(any = as.integer(any), trimestre = as.integer(trimestre), codi = as.double(codi))
}

load_renda <- function() {
  path <- "data/renda_barris_real.csv"
  if (!file.exists(path)) return(NULL)
  tryCatch(
    read_csv(path, show_col_types = FALSE) %>%
      mutate(any = as.integer(any), codi_barri = as.integer(codi_barri)),
    error = function(e) NULL
  )
}

load_ipc <- function() {
  path <- "data/ipc_barcelona.csv"
  if (!file.exists(path)) return(NULL)
  tryCatch(
    read_csv(path, show_col_types = FALSE) %>%
      mutate(any = as.integer(any)),
    error = function(e) NULL
  )
}

# ── Etiquetes ─────────────────────────────────────────────────────────────────
METRICA_LABEL <- c(
  compravendes = "Compravendes (nre. operacions)",
  superficie   = "Superfície mitjana (m²)",
  preu_total   = "Preu total (milers EUR/op.)",
  preu_m2      = "Preu per m² (EUR/m²)"
)

TIPUS_LABEL <- c(
  total      = "Total",
  nou        = "Habitatge nou",
  nou_lliure = "Nou lliure",
  protegit   = "Protegit (VPO)",
  usat       = "Habitatge usat"
)

TIPUS_PER_METRICA <- list(
  compravendes = c("total", "nou_lliure", "protegit", "usat"),
  superficie   = c("total", "nou_lliure", "protegit", "usat"),
  preu_total   = c("total", "nou", "usat"),
  preu_m2      = c("total", "nou", "usat")
)

# ── Paleta de colors unificada ────────────────────────────────────────────────
# Regla d'or: un color per districte, sempre el mateix a tota l'app.
# Noms amb accents exactes tal com surten al CSV i al BARRI_DISTRICTE_MAP.
# "BCN total" sempre en negre (#111827) per destacar com a referència global.

COLOR_BCN   <- "#111827"   # Negre quasi-pur — reservat per "Barcelona total"

# 10 districtes: noms exactes del CSV (amb accents)
COLORS_DIST <- c(
  "Ciutat Vella"        = "#E63946",  # vermell viu
  "Eixample"            = "#2196F3",  # blau mig
  "Sants-Montjuïc"      = "#FF9800",  # taronja
  "Les Corts"           = "#9C27B0",  # violeta
  "Sarrià-Sant Gervasi" = "#009688",  # verd fosc
  "Gràcia"              = "#F06292",  # rosa
  "Horta-Guinardó"      = "#795548",  # marró
  "Nou Barris"          = "#607D8B",  # gris blavós
  "Sant Andreu"         = "#4CAF50",  # verd mig
  "Sant Martí"          = "#FFC107"   # groc ambre
)

# COLORS_DIST_ALT = còpia idèntica (per compatibilitat amb scale_color_manual)
COLORS_DIST_ALT <- COLORS_DIST

# Funció per obtenir color d'un districte (tolerant a variants d'accent)
color_districte <- function(nom) {
  if (nom %in% names(COLORS_DIST)) return(unname(COLORS_DIST[nom]))
  # Fallback sense accents
  nom_clean  <- iconv(nom, to = "ASCII//TRANSLIT")
  keys_clean <- iconv(names(COLORS_DIST), to = "ASCII//TRANSLIT")
  idx <- match(nom_clean, keys_clean)
  if (!is.na(idx)) return(unname(COLORS_DIST[idx]))
  "#94a3b8"
}

# Paleta per a categories semàntiques fixes (H1, H3, renda, etc.)
COLOR_NOMINAL   <- "#E63946"   # vermell  → preu nominal / encariment
COLOR_REAL      <- "#2196F3"   # blau     → preu real / deflactat
COLOR_IPC       <- COLOR_BCN    # negre → IPC de referència (igual que BCN total, per coherència)
COLOR_NOU       <- "#E63946"   # vermell  → habitatge nou (més car = "alerta")
COLOR_USAT      <- "#2196F3"   # blau     → habitatge usat
COLOR_RENDA_ALT <- "#009688"   # verd fosc → renda alta
COLOR_RENDA_MIG <- "#FF9800"   # taronja  → renda mitjana
COLOR_RENDA_BAX <- "#E63946"   # vermell  → renda baixa
COLOR_MES_CAR   <- "#E63946"   # vermell  → línia extrems car
COLOR_MES_ECO   <- "#4CAF50"   # verd     → línia extrems econòmic

# PALETTE_MAIN per a barris arbitraris (negre primer = BCN)
PALETTE_MAIN <- c(COLOR_BCN, unname(COLORS_DIST))

BARRI_DISTRICTE_MAP <- c(
  rep("Ciutat Vella",        4),
  rep("Eixample",            6),
  rep("Sants-Montjuïc",      8),
  rep("Les Corts",           3),
  rep("Sarrià-Sant Gervasi", 6),
  rep("Gràcia",              5),
  rep("Horta-Guinardó",     11),
  rep("Nou Barris",         13),
  rep("Sant Andreu",         7),
  rep("Sant Martí",         10)
)
names(BARRI_DISTRICTE_MAP) <- as.character(1:73)

BARRI_COORDS <- data.frame(
  codi = 1:73,
  lat  = c(41.3826,41.3826,41.3791,41.3840,41.3906,41.4013,41.3948,41.3877,
           41.3826,41.3786,41.3712,41.3567,41.3500,41.3625,41.3718,41.3740,
           41.3727,41.3749,41.3823,41.3860,41.4049,41.4193,41.4112,41.4026,
           41.4079,41.4010,41.4089,41.4118,41.4119,41.4083,41.4022,41.4046,
           41.4070,41.4129,41.4110,41.4168,41.4216,41.4264,41.4313,41.4337,
           41.4298,41.4248,41.4242,41.4310,41.4368,41.4400,41.4449,41.4420,
           41.4484,41.4435,41.4393,41.4355,41.4486,41.4563,41.4622,41.4655,
           41.4393,41.4370,41.4287,41.4310,41.4254,41.4261,41.4220,41.4126,
           41.4101,41.3987,41.3870,41.4001,41.4046,41.4126,41.4065,41.4143,41.4245),
  lon  = c(2.1685,2.1756,2.1891,2.1820,2.1856,2.1737,2.1642,2.1543,
           2.1534,2.1641,2.1683,2.1510,2.1568,2.1631,2.1575,2.1499,
           2.1368,2.1337,2.1225,2.1140,2.1009,2.1034,2.1157,2.1259,
           2.1324,2.1437,2.1477,2.1570,2.1612,2.1630,2.1581,2.1655,
           2.1731,2.1763,2.1836,2.1836,2.1770,2.1820,2.1882,2.1803,
           2.1729,2.1696,2.1651,2.1565,2.1531,2.1600,2.1703,2.1476,
           2.1379,2.1441,2.1423,2.1465,2.1499,2.1539,2.1496,2.1557,
           2.1918,2.2013,2.2010,2.1880,2.1945,2.1881,2.1905,2.1927,
           2.1941,2.1939,2.1999,2.2030,2.2115,2.2200,2.2070,2.2034,2.2108)
)

DISTRICTE_COORDS <- data.frame(
  nom = c("Ciutat Vella", "Eixample", "Sants-Montjuïc", "Les Corts",
          "Sarrià-Sant Gervasi", "Gràcia", "Horta-Guinardó",
          "Nou Barris", "Sant Andreu", "Sant Martí"),
  lat = c(41.383,41.392,41.370,41.388,41.405,41.401,41.422,41.440,41.430,41.408),
  lon = c(2.177,2.162,2.163,2.118,2.125,2.158,2.174,2.170,2.195,2.205)
)

# ── Helper: eix temporal (any, no trimestre) ──────────────────────────────────
# Converteix "2014-Q1" → 2014.0, "2014-Q2" → 2014.25, etc. per a scale_x_continuous
periode_a_num <- function(p) {
  any_ <- as.integer(substr(p, 1, 4))
  q_   <- as.integer(substr(p, 7, 7))
  any_ + (q_ - 1) / 4
}

# Genera breaks nets (un per any) i labels "2014", "2015"...
breaks_anuals <- function(periodes_vec) {
  nums  <- periode_a_num(periodes_vec)
  anys  <- seq(floor(min(nums)), floor(max(nums)), by = 1)
  # Retorna els valors numèrics corresponents al Q1 de cada any
  anys  # es mapen directament (Q1 = .0)
}

# Converteix el vector de periodes a numèric i afegeix la columna
add_t_num <- function(df) {
  df %>% dplyr::mutate(t_num = periode_a_num(periode))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

get_periodes <- function(df) sort(unique(df$periode))
get_noms     <- function(df, niv) sort(unique(df$nom[df$nivell == niv]))

filter_serie <- function(df, niv, noms, met, tip) {
  df %>% filter(nivell == niv, nom %in% noms, metrica == met, tipus_habitatge == tip) %>%
    arrange(nom, any, trimestre)
}

fmt_valor <- function(v, metrica) {
  if (is.na(v) || v == 0) return("—")
  switch(metrica,
    compravendes = format(round(v), big.mark = ".", scientific = FALSE),
    superficie   = paste0(format(round(v, 1), nsmall = 1), " m²"),
    preu_total   = paste0(format(round(v, 1), big.mark = ".", nsmall = 1), " kEUR"),
    preu_m2      = paste0(format(round(v), big.mark = "."), " EUR/m²"),
    as.character(round(v, 2))
  )
}

# ── Tema ggplot2 ──────────────────────────────────────────────────────────────
tema_hab <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      text                = element_text(family = "sans", color = "#1e293b"),
      plot.title          = element_text(size = base + 1, face = "bold", color = "#0f172a", margin = margin(b = 4)),
      plot.subtitle       = element_text(size = base - 2, color = "#64748b", margin = margin(b = 8)),
      plot.caption        = element_text(size = base - 3, color = "#94a3b8", hjust = 0, margin = margin(t = 8)),
      axis.title          = element_text(size = base - 2, color = "#64748b"),
      axis.text           = element_text(size = base - 3, color = "#94a3b8"),
      axis.text.x         = element_text(angle = 45, hjust = 1),
      panel.grid.major.y  = element_line(color = "#f1f5f9", linewidth = .6),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor    = element_blank(),
      panel.background    = element_rect(fill = "white", color = NA),
      plot.background     = element_rect(fill = "white", color = NA),
      legend.position     = "bottom",
      legend.key.size     = unit(10, "pt"),
      legend.text         = element_text(size = base - 3),
      legend.title        = element_text(size = base - 2, face = "bold"),
      strip.text          = element_text(size = base - 1, face = "bold"),
      strip.background    = element_rect(fill = "#f8fafc", color = NA),
      plot.margin         = margin(10, 25, 8, 8)
    )
}
theme_set(tema_hab())

# ── Pàgina d'inici ────────────────────────────────────────────────────────────
mod_inici_ui <- function(df, ipc_data = NULL) {
  periodes   <- get_periodes(df)
  p_last     <- tail(periodes, 1)

  bcn_preu   <- df %>% filter(nivell == "barcelona", metrica == "preu_m2",
                               tipus_habitatge == "total", periode == p_last) %>% pull(valor)
  bcn_preu14 <- df %>% filter(nivell == "barcelona", metrica == "preu_m2",
                               tipus_habitatge == "total", periode == "2014-Q1") %>% pull(valor)
  bcn_cv     <- df %>% filter(nivell == "barcelona", metrica == "compravendes",
                               tipus_habitatge == "total", periode == p_last) %>% pull(valor)
  preu_yoy_p <- tail(periodes[grep("Q3", periodes)], 2)
  bcn_now    <- df %>% filter(nivell == "barcelona", metrica == "preu_m2",
                               tipus_habitatge == "total", periode == preu_yoy_p[2]) %>% pull(valor)
  bcn_prev   <- df %>% filter(nivell == "barcelona", metrica == "preu_m2",
                               tipus_habitatge == "total", periode == preu_yoy_p[1]) %>% pull(valor)
  yoy        <- round((bcn_now - bcn_prev) / bcn_prev * 100, 1)
  creix_tot  <- round((bcn_preu - bcn_preu14) / bcn_preu14 * 100, 1)

  # Creixement IPC per comparació ràpida
  ipc_creix <- if (!is.null(ipc_data)) {
    ipc_fi  <- ipc_data %>% filter(any == max(any)) %>% pull(ipc_index)
    ipc_ini <- ipc_data %>% filter(any == min(any)) %>% pull(ipc_index)
    if (length(ipc_fi) > 0 && length(ipc_ini) > 0 && ipc_ini > 0)
      round((ipc_fi - ipc_ini) / ipc_ini * 100, 1)
    else NA
  } else NA

  kpi_hero <- function(val, label, sub, color = "#3b82f6", icon_cls = "fa-house") {
    tags$div(class = "kpi-hero",
      tags$div(class = "kpi-hero-icon",
               style = paste0("background:", color, "18; color:", color, ";"),
               tags$i(class = paste("fa-solid", icon_cls))
      ),
      tags$div(
        tags$div(class = "kpi-hero-val", val),
        tags$div(class = "kpi-hero-label", label),
        tags$div(class = "kpi-hero-sub", sub)
      )
    )
  }

  hip_card <- function(id, titol, desc, veredicte, col_bg, col_txt, icon_cls, tab_dest) {
    tags$div(
      class = "hip-card",
      style = paste0("border-left: 4px solid ", col_txt, ";"),
      tags$div(class = "hip-card-head",
        tags$div(class = "hip-badge",
                 style = paste0("background:", col_bg, "; color:", col_txt, ";"),
                 tags$i(class = paste("fa-solid", icon_cls)),
                 paste0(" ", id)
        ),
        tags$div(class = "hip-veredicte",
                 style = paste0("background:", col_bg, "; color:", col_txt, ";"),
                 veredicte)
      ),
      tags$div(class = "hip-titol", titol),
      tags$div(class = "hip-desc", desc),
      tags$div(class = "hip-link",
               onclick = paste0("$('a[data-value=\"hipotesis\"]').click();"),
               "Veure anàlisi complet →"
      )
    )
  }

  fluidRow(column(12,
    # ── Capçalera ──
    tags$div(class = "page-header",
      tags$div(class = "page-header-left",
        tags$div(class = "page-badge", "Mercat immobiliari · Barcelona"),
        tags$h1(class = "page-title", "Habitatge a Barcelona"),
        tags$p(class = "page-subtitle",
          "Sèrie trimestral de compravendes inscrites al Registre de la Propietat.",
          tags$br(),
          "Cobrim ", tags$strong("73 barris"), " i ", tags$strong("10 districtes"),
          " des del 2014 fins al 2025 Q3."
        )
      ),
      tags$div(class = "page-header-right",
        tags$div(class = "update-badge",
          tags$i(class = "fa-solid fa-circle-check", style = "color:#10b981; margin-right:6px;"),
          "Darrera actualització: ", tags$strong(p_last)
        )
      )
    ),

    # ── KPIs ──
    tags$div(class = "kpi-hero-row",
      kpi_hero(paste0(format(round(bcn_preu), big.mark = "."), " EUR/m²"),
               "Preu mitjà Barcelona",
               paste0("Q3 2025 · +", yoy, "% vs any anterior"),
               "#3b82f6", "fa-chart-line"),
      kpi_hero(paste0("+", creix_tot, "%"),
               "Pujada des del 2014",
               paste0(format(round(bcn_preu14), big.mark = "."), " → ",
                      format(round(bcn_preu), big.mark = "."), " EUR/m²"),
               "#ef4444", "fa-arrow-trend-up"),
      kpi_hero(format(round(bcn_cv), big.mark = "."),
               "Compravendes",
               paste0("Trimestre ", p_last),
               "#10b981", "fa-handshake"),
      if (!is.na(ipc_creix))
        kpi_hero(paste0("+", ipc_creix, "% IPC vs +", creix_tot, "% preu"),
                 "Preu supera la inflació",
                 "Diferència acumulada des del 2014",
                 "#f59e0b", "fa-scale-unbalanced")
      else
        kpi_hero("73 barris · 10 districtes",
                 "Cobertura geogràfica",
                 "84 àrees · 47 trimestres",
                 "#8b5cf6", "fa-map-location-dot")
    ),

    # ── Hipòtesis del treball ──
    tags$div(class = "hip-section",
      tags$div(class = "hip-section-head",
        tags$div(class = "hip-section-title",
          tags$i(class = "fa-solid fa-flask", style = "color:#8b5cf6; margin-right:8px;"),
          "Hipòtesis del treball"
        ),
        tags$div(class = "hip-section-sub",
          "Tres preguntes que guien l'anàlisi · fes clic a qualsevol per veure l'anàlisi complet"
        )
      ),
      tags$div(class = "hip-grid",
        hip_card("H1", "El preu ha crescut molt per sobre de la inflació des del 2014",
                 "Comparem l'evolució del preu nominal i real (deflactat per IPC) de Barcelona amb l'índex de preus al consum.",
                 "CONFIRMADA", "#fee2e2", "#991b1b", "fa-fire", "hipotesis"),
        hip_card("H2", "Desigualtat territorial significativa entre districtes",
                 "Analitzem la bretxa de preu entre Sarrià-Sant Gervasi i Nou Barris, i la dispersió general entre els 10 districtes.",
                 "CONFIRMADA", "#fef3c7", "#92400e", "fa-map-location-dot", "hipotesis"),
        hip_card("H3", "L'habitatge nou s'ha encarit més que el de segona mà des del 2015",
                 "Comparem la trajectòria del preu per m² de l'habitatge nou vs. usat per veure quin ha crescut més ràpidament.",
                 "CONFIRMADA", "#f0fdf4", "#166534", "fa-house-circle-check", "hipotesis")
      )
    ),

    # ── Guia de navegació ──
    tags$div(class = "guide-section-title",
      tags$i(class = "fa-solid fa-compass", style = "color:#64748b; margin-right:8px;"),
      "Com navegar per l'aplicació"
    ),
    tags$div(class = "guide-row",
      tags$div(class = "guide-card",
        tags$div(class = "guide-icon", style = "background:#eff6ff;",
                 tags$i(class = "fa-solid fa-chart-line", style = "color:#3b82f6; font-size:22px;")),
        tags$div(class = "guide-text",
          tags$div(class = "guide-title", "Evolució"),
          tags$div(class = "guide-desc",
            "Compara l'evolució de preus, compravendes o superfície al llarg del temps. ",
            "Selecciona fins a 5 àrees simultàniament; si se superposen, activa les ", tags$em("facetes"), " per veure-les separades.")
        )
      ),
      tags$div(class = "guide-card",
        tags$div(class = "guide-icon", style = "background:#f0fdf4;",
                 tags$i(class = "fa-solid fa-map-location-dot", style = "color:#10b981; font-size:22px;")),
        tags$div(class = "guide-text",
          tags$div(class = "guide-title", "Mapa de preus"),
          tags$div(class = "guide-desc",
            "Visualitza geogràficament el preu per m² per trimestre. ",
            "Mida i color dels cercles reflecteix el preu relatiu. Inclou llegenda de gradient i ranking.")
        )
      ),
      tags$div(class = "guide-card",
        tags$div(class = "guide-icon", style = "background:#faf5ff;",
                 tags$i(class = "fa-solid fa-scale-balanced", style = "color:#8b5cf6; font-size:22px;")),
        tags$div(class = "guide-text",
          tags$div(class = "guide-title", "Accessibilitat"),
          tags$div(class = "guide-desc",
            "Creua preu de l'habitatge amb renda familiar disponible per barri. ",
            "Índex d'esforç = mesos de renda bruta per comprar 1 m².")
        )
      ),
      tags$div(class = "guide-card",
        tags$div(class = "guide-icon", style = "background:#fefce8;",
                 tags$i(class = "fa-solid fa-flask", style = "color:#ca8a04; font-size:22px;")),
        tags$div(class = "guide-text",
          tags$div(class = "guide-title", "Hipòtesis"),
          tags$div(class = "guide-desc",
            "Una pàgina dedicada a contrastar les tres hipòtesis del treball, amb gràfiques específiques i veredicte de cada una.")
        )
      )
    ),

    # ── Nota metodològica ──
    tags$div(class = "metodologia",
      tags$i(class = "fa-solid fa-circle-info", style = "color:#64748b; margin-right:6px;"),
      tags$strong("Metodologia: "),
      "Les dades provenen del Departament d'Estadística de l'Ajuntament de Barcelona (Registre de la Propietat). ",
      "Les cel·les amb '...' a l'origen (confidencialitat estadística) apareixen com a valors absents. ",
      "L'IPC correspon a la Província de Barcelona (INE, base 2014=100)."
    )
  ))
}

# ── CSS global ────────────────────────────────────────────────────────────────
CSS_GLOBAL <- "
/* ── Reset i base ─────────────────────────────────────────────────────── */
*, *::before, *::after { box-sizing: border-box; }
body, .content-wrapper, .right-side {
  background: #f1f5f9 !important;
  font-family: 'DM Sans', 'Inter', sans-serif !important;
}

/* ── Header ───────────────────────────────────────────────────────────── */
.skin-black .main-header .logo,
.skin-black .main-header .logo:hover { background: #0f172a !important; border-bottom: 1px solid #1e293b !important; }
.skin-black .main-header .navbar { background: #0f172a !important; border-bottom: 1px solid #1e293b !important; }
.skin-black .main-header .navbar .sidebar-toggle { color: #64748b !important; }
.skin-black .main-header .navbar .sidebar-toggle:hover { color: #f8fafc !important; background: transparent !important; }

/* ── Sidebar ──────────────────────────────────────────────────────────── */
.skin-black .main-sidebar { background: #0f172a !important; border-right: 1px solid #1e293b !important; }
.skin-black .sidebar-menu > li > a {
  color: #64748b !important; font-size: 13px !important; font-weight: 500 !important;
  padding: 10px 16px !important; border-left: 3px solid transparent !important;
  transition: all .15s ease !important;
}
.skin-black .sidebar-menu > li > a:hover {
  color: #e2e8f0 !important; background: rgba(255,255,255,.05) !important;
  border-left-color: #475569 !important;
}
.skin-black .sidebar-menu > li.active > a {
  color: #f8fafc !important; background: rgba(59,130,246,.15) !important;
  border-left-color: #3b82f6 !important;
}
.skin-black .sidebar-menu > li > a .fa { width: 20px !important; font-size: 14px !important; }
.main-sidebar .sidebar { padding-top: 8px; }
.sidebar-section-label {
  padding: 14px 16px 4px; color: #475569; font-size: 10px;
  font-weight: 700; letter-spacing: .1em; text-transform: uppercase;
}

/* ── Layout ───────────────────────────────────────────────────────────── */
.content-wrapper { padding: 20px !important; }
.tab-content { padding: 0 !important; }

/* ── Boxes ────────────────────────────────────────────────────────────── */
.box { border: none !important; border-radius: 14px !important; box-shadow: 0 1px 4px rgba(0,0,0,.07) !important; background: white !important; margin-bottom: 18px !important; }
.box-header { border-radius: 14px 14px 0 0 !important; padding: 14px 18px 12px !important; background: white !important; border-bottom: 1px solid #f1f5f9 !important; }
.box-header .box-title { font-size: 13px !important; font-weight: 600 !important; color: #0f172a !important; }
.box-header.with-border { border-bottom: 1px solid #f1f5f9 !important; }
.box-body { padding: 16px 18px !important; }
.box::before { display: none !important; }

/* ── Inputs ───────────────────────────────────────────────────────────── */
.selectize-input { border: 1px solid #e2e8f0 !important; border-radius: 8px !important; font-size: 12px !important; padding: 7px 10px !important; box-shadow: none !important; }
.selectize-input.focus { border-color: #3b82f6 !important; box-shadow: 0 0 0 3px rgba(59,130,246,.12) !important; }
.selectize-dropdown { border: 1px solid #e2e8f0 !important; border-radius: 8px !important; box-shadow: 0 8px 24px rgba(0,0,0,.1) !important; font-size: 12px !important; }
.selectize-dropdown-content .option { padding: 8px 12px !important; }
.selectize-dropdown-content .option.active { background: #eff6ff !important; color: #1d4ed8 !important; }
.form-control { border: 1px solid #e2e8f0 !important; border-radius: 8px !important; font-size: 12px !important; }
.control-label { font-size: 11px !important; font-weight: 600 !important; color: #475569 !important; text-transform: uppercase !important; letter-spacing: .07em !important; margin-bottom: 5px !important; }
.btn-group-container-sw .btn { font-size: 12px !important; font-weight: 500 !important; }

/* Slider */
.irs--shiny .irs-bar { background: #3b82f6 !important; border-top: 1px solid #3b82f6 !important; border-bottom: 1px solid #3b82f6 !important; }
.irs--shiny .irs-handle { border: 2px solid #3b82f6 !important; background: white !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #3b82f6 !important; font-size: 10px !important; }

/* Switch */
.material-switch > input[type='checkbox']:checked + .material-switch-container { background: #3b82f6 !important; }

/* ── Pàgina inici ─────────────────────────────────────────────────────── */
.page-header {
  display: flex; justify-content: space-between; align-items: flex-start;
  margin-bottom: 20px; padding: 24px 28px; background: white;
  border-radius: 16px; box-shadow: 0 1px 4px rgba(0,0,0,.07);
}
.page-badge { display: inline-block; background: #eff6ff; color: #1d4ed8; font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: .08em; padding: 4px 10px; border-radius: 20px; margin-bottom: 10px; }
.page-title { font-size: 26px; font-weight: 800; color: #0f172a; margin: 0 0 8px 0; letter-spacing: -.5px; line-height: 1.2; }
.page-subtitle { font-size: 13px; color: #64748b; margin: 0; line-height: 1.6; }
.update-badge { background: #f0fdf4; border: 1px solid #bbf7d0; color: #15803d; font-size: 12px; font-weight: 500; padding: 8px 14px; border-radius: 20px; white-space: nowrap; }

.kpi-hero-row { display: grid; grid-template-columns: repeat(4,1fr); gap: 14px; margin-bottom: 20px; }
.kpi-hero { display: flex; align-items: center; gap: 14px; background: white; border-radius: 14px; padding: 18px 20px; box-shadow: 0 1px 4px rgba(0,0,0,.07); }
.kpi-hero-icon { width: 48px; height: 48px; border-radius: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0; font-size: 20px; }
.kpi-hero-val { font-size: 19px; font-weight: 800; color: #0f172a; line-height: 1.2; letter-spacing: -.5px; }
.kpi-hero-label { font-size: 12px; font-weight: 600; color: #374151; margin-top: 2px; }
.kpi-hero-sub { font-size: 11px; color: #94a3b8; margin-top: 2px; }

/* Hipòtesis a l'inici */
.hip-section { background: white; border-radius: 16px; padding: 20px 22px; margin-bottom: 20px; box-shadow: 0 1px 4px rgba(0,0,0,.07); }
.hip-section-head { margin-bottom: 16px; }
.hip-section-title { font-size: 15px; font-weight: 700; color: #0f172a; margin-bottom: 4px; }
.hip-section-sub { font-size: 12px; color: #94a3b8; }
.hip-grid { display: grid; grid-template-columns: repeat(3,1fr); gap: 14px; }
.hip-card { background: #f8fafc; border-radius: 12px; padding: 16px 18px; border-left: 4px solid #e2e8f0; }
.hip-card-head { display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; }
.hip-badge { font-size: 11px; font-weight: 700; padding: 4px 10px; border-radius: 20px; display: inline-flex; align-items: center; gap: 5px; }
.hip-veredicte { font-size: 10px; font-weight: 700; padding: 3px 9px; border-radius: 20px; letter-spacing: .04em; }
.hip-titol { font-size: 13px; font-weight: 700; color: #1e293b; margin-bottom: 6px; line-height: 1.4; }
.hip-desc { font-size: 11px; color: #64748b; line-height: 1.6; margin-bottom: 10px; }
.hip-link { font-size: 11px; font-weight: 600; color: #3b82f6; cursor: pointer; }
.hip-link:hover { text-decoration: underline; }

/* Guia de navegació */
.guide-section-title { font-size: 13px; font-weight: 700; color: #374151; margin-bottom: 12px; }
.guide-row { display: grid; grid-template-columns: repeat(4,1fr); gap: 12px; margin-bottom: 18px; }
.guide-card { background: white; border-radius: 14px; padding: 18px; box-shadow: 0 1px 4px rgba(0,0,0,.07); display: flex; gap: 14px; }
.guide-icon { width: 48px; height: 48px; border-radius: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0; }
.guide-title { font-size: 13px; font-weight: 700; color: #0f172a; margin-bottom: 5px; }
.guide-desc { font-size: 11px; color: #64748b; line-height: 1.6; }

.metodologia { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 10px; padding: 12px 16px; font-size: 12px; color: #64748b; line-height: 1.6; }

/* ── KPI cards ────────────────────────────────────────────────────────── */
.kpi-strip { display: grid; gap: 12px; margin-bottom: 16px; }
.kpi-card { background: white; border-radius: 12px; padding: 14px 16px; box-shadow: 0 1px 3px rgba(0,0,0,.06); border-left: 3px solid #3b82f6; }
.kpi-label { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: .08em; color: #94a3b8; margin-bottom: 4px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.kpi-value { font-size: 18px; font-weight: 800; color: #0f172a; letter-spacing: -.5px; line-height: 1.2; }
.kpi-delta { font-size: 11px; font-weight: 600; margin-top: 3px; }
.kpi-delta-up   { color: #ef4444; }
.kpi-delta-down { color: #10b981; }
.kpi-sub { font-size: 10px; color: #cbd5e1; margin-top: 2px; }

/* ── Panel de controls ────────────────────────────────────────────────── */
.control-panel { background: white; border-radius: 14px; padding: 18px; box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 14px; }
.control-section-label { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: .1em; color: #94a3b8; margin-bottom: 10px; display: block; }
.control-divider { border: none; border-top: 1px solid #f1f5f9; margin: 14px 0; }
.info-tip { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 12px; font-size: 11px; color: #64748b; line-height: 1.5; margin-top: 10px; }

/* ── Plot container ───────────────────────────────────────────────────── */
.plot-container { background: white; border-radius: 14px; box-shadow: 0 1px 4px rgba(0,0,0,.07); padding: 18px 18px 10px; margin-bottom: 14px; }
.plot-header { margin-bottom: 12px; }
.plot-title { font-size: 14px; font-weight: 700; color: #0f172a; }
.plot-subtitle { font-size: 11px; color: #94a3b8; margin-top: 2px; }
.plot-footer { font-size: 10px; color: #cbd5e1; margin-top: 6px; padding-top: 6px; border-top: 1px solid #f8fafc; }

/* ── Mapa container ───────────────────────────────────────────────────── */
.map-container { background: white; border-radius: 14px; box-shadow: 0 1px 4px rgba(0,0,0,.07); overflow: hidden; margin-bottom: 14px; }
.map-header { padding: 14px 18px; border-bottom: 1px solid #f1f5f9; display: flex; justify-content: space-between; align-items: center; }

/* ── Hipòtesi cards (pàgina hipòtesis) ───────────────────────────────── */
.h-section { background: white; border-radius: 16px; padding: 22px 24px; margin-bottom: 20px; box-shadow: 0 1px 4px rgba(0,0,0,.07); }
.h-header { display: flex; align-items: flex-start; gap: 16px; margin-bottom: 18px; padding-bottom: 16px; border-bottom: 1px solid #f1f5f9; }
.h-badge { font-size: 22px; font-weight: 800; color: white; width: 52px; height: 52px; border-radius: 14px; display: flex; align-items: center; justify-content: center; flex-shrink: 0; }
.h-title { font-size: 16px; font-weight: 700; color: #0f172a; margin-bottom: 4px; }
.h-subtitle { font-size: 12px; color: #64748b; line-height: 1.6; }
.h-veredicte-badge { display: inline-flex; align-items: center; gap: 6px; font-size: 12px; font-weight: 700; padding: 5px 14px; border-radius: 20px; margin-top: 8px; }
.h-conclusio { background: #f8fafc; border-radius: 10px; padding: 14px 16px; font-size: 12px; color: #374151; line-height: 1.7; margin-top: 14px; border-left: 3px solid #e2e8f0; }

/* ── Taula DT ─────────────────────────────────────────────────────────── */
.dataTables_wrapper { font-size: 12px; }
.dataTables_wrapper .dataTables_filter input { border: 1px solid #e2e8f0 !important; border-radius: 8px !important; padding: 5px 10px !important; font-size: 12px !important; }
table.dataTable thead th { background: #f8fafc !important; color: #374151 !important; font-weight: 600 !important; font-size: 11px !important; text-transform: uppercase !important; letter-spacing: .05em !important; border-bottom: 1px solid #e2e8f0 !important; }
table.dataTable tbody tr:hover { background: #f8fafc !important; }
table.dataTable tbody td { font-size: 12px !important; color: #374151 !important; }

/* ── Responsive ───────────────────────────────────────────────────────── */
@media (max-width: 1200px) {
  .kpi-hero-row  { grid-template-columns: repeat(2,1fr); }
  .guide-row     { grid-template-columns: repeat(2,1fr); }
  .hip-grid      { grid-template-columns: repeat(1,1fr); }
}

/* ── Desplegables (details/summary) ──────────────────────────────────────── */
details > summary { list-style: none; }
details > summary::-webkit-details-marker { display: none; }
details[open] > summary {
  border-radius: 14px 14px 0 0 !important;
  box-shadow: 0 1px 4px rgba(0,0,0,.07) !important;
  margin-bottom: 0 !important;
}
details > div.plot-container { border-radius: 0 0 14px 14px !important; }

/* ── Scrollbar ────────────────────────────────────────────────────────── */
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: #f1f5f9; }
::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: #94a3b8; }
"
