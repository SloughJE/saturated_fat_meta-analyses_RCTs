library(dplyr)
library(tibble)
library(meta)
library(grid)
library(ggplot2)
library(stringr)

#######################

# -------------------------
# 1) Master study-level data
# -------------------------
# One row per study per outcome
# outcome_key identifies which Steen figure/outcome the row belongs to

steen_dat <- tribble(
  ~outcome_key,  ~figure_no, ~outcome_label,            ~study,                         ~replace_type, ~event.e, ~n.e,  ~event.c, ~n.c,  ~study_order,
  "allcause",    1,          "All-cause mortality",     "Minnesota Coronary, 1989",     "PUFA",            269, 4541,      248, 4516,  1,
  "allcause",    1,          "All-cause mortality",     "DART, 1989",                   "PUFA",            111, 1018,      113, 1015,  2,
  "allcause",    1,          "All-cause mortality",     "Lyon Diet Heart, 1994",        "PUFA",              8,  302,       20,  303,  3,
  "allcause",    1,          "All-cause mortality",     "MRC, 1968",                    "PUFA",             28,  199,       31,  194,  4,
  "allcause",    1,          "All-cause mortality",     "Rose et al (corn oil), 1965",  "PUFA",              5,   28,        1,   13,  5,
  "allcause",    1,          "All-cause mortality",     "Veterans Admin, 1969",         "PUFA",            174,  424,      177,  422,  6,
  "allcause",    1,          "All-cause mortality",     "STARS, 1992",                  "PUFA",              1,   27,        3,   28,  7,
  "allcause",    1,          "All-cause mortality",     "Sydney Diet-Heart, 1978",      "PUFA",             39,  221,       28,  237,  8,
  "allcause",    1,          "All-cause mortality",     "Oslo Diet-Heart, 1966",        "PUFA",             48,  206,       65,  206,  9,
  "allcause",    1,          "All-cause mortality",     "Black et al., 1994",           "CHO",               1,   66,        2,   67, 10,
  "allcause",    1,          "All-cause mortality",     "Rose et al (olive oil), 1965", "MUFA",              3,   26,        1,   26, 11,
  "allcause",    1,          "All-cause mortality",     "Ley et al., 2004",             "CHO",               2,   88,        6,   88, 12,
  "allcause",    1,          "All-cause mortality",     "WHI, 2006",                    "CHO",             989,19541,     1520,29294, 13,
  "allcause",    1,          "All-cause mortality",     "WINS, 2006",                   "CHO",              64,  975,      107, 1462, 14
  

)

steen_fig_meta <- list(
  allcause = list(
    title_text = "Steen Figure 1\nAll-cause mortality\nPUFA replacement versus other macronutrients",
    xlim = c(0.01, 100),
    at   = c(0.01, 0.1, 1, 10, 100)
  )
)


steen_fig_meta <- list(
  allcause = list(
    title_text = "Steen Figure 1: All-cause mortality\nPUFA replacement versus other macronutrients",
    xlim = c(0.01, 100),
    at   = c(0.01, 0.1, 1, 10, 100)
  )
)

plot_steen <- function(outcome_name,
                       exclude_rose_olive = FALSE,
                       subgroup_mode = c("combined", "split"),
                       save_png = TRUE,
                       filename = NULL,
                       width = 3200,
                       height = 2400,
                       res = 300) {
  
  subgroup_mode <- match.arg(subgroup_mode)
  
  dat <- steen_dat %>%
    dplyr::filter(.data$outcome_key == outcome_name)
  
  if (exclude_rose_olive) {
    dat <- dat %>%
      dplyr::filter(.data$study != "Rose et al (olive oil), 1965")
  }
  
  dat <- dat %>%
    dplyr::mutate(
      replace_group = dplyr::case_when(
        subgroup_mode == "combined" & .data$replace_type == "PUFA" ~ "PUFA replacement",
        subgroup_mode == "combined" & .data$replace_type != "PUFA" ~ "Other replacement",
        subgroup_mode == "split"    & .data$replace_type == "PUFA" ~ "PUFA replacement",
        subgroup_mode == "split"    & .data$replace_type == "CHO"  ~ "CHO replacement",
        subgroup_mode == "split"    & .data$replace_type == "MUFA" ~ "MUFA replacement"
      )
    ) %>%
    dplyr::arrange(.data$study_order) %>%
    dplyr::mutate(
      replace_group = factor(.data$replace_group, levels = unique(.data$replace_group)),
      study = factor(.data$study, levels = .data$study)
    ) %>%
    droplevels()
  
  meta_row <- steen_fig_meta[[outcome_name]]
  
  if (is.null(filename)) {
    suffix <- if (exclude_rose_olive) "_no_rose_olive" else ""
    filename <- paste0("steen_", outcome_name, "_", subgroup_mode, suffix, ".png")
  }
  
  if (save_png) {
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(
        filename = filename,
        width = width,
        height = height,
        units = "px",
        res = res
      )
    } else {
      png(
        filename = filename,
        width = width,
        height = height,
        res = res,
        type = "cairo"
      )
    }
  }
  
  m <- meta::metabin(
    event.e = event.e,
    n.e     = n.e,
    event.c = event.c,
    n.c     = n.c,
    studlab = study,
    data    = dat,
    sm      = "RR",
    method  = "MH",
    common  = FALSE,
    random  = TRUE,
    subgroup = replace_group,
    tau.common = FALSE,
    method.tau = "DL",
    Q.Cochrane = TRUE,
    method.random.ci = FALSE,
    prediction = FALSE
  )
  
  meta::forest(
    m,
    leftcols  = "studlab",
    leftlabs  = "Study",
    rightcols = c("effect", "ci", "w.random"),
    rightlabs = c("RR", "95% CI", "Weight"),
    
    overall = TRUE,
    subgroup = TRUE,
    overall.hetstat = TRUE,
    subgroup.hetstat = TRUE,
    test.overall = FALSE,
    test.subgroup = TRUE,
    
    print.subgroup.labels = TRUE,
    print.subgroup.name = FALSE,
    
    smlab = "Risk ratio",
    xlab  = "Favors Lower SFA      Favors Higher SFA",
    xlog  = TRUE,
    xlim  = meta_row$xlim,
    at    = meta_row$at,
    
    digits = 2,
    digits.weight = 1,
    addrows.below.overall = 4,
    rows.gr = 2,
    
    text.random = "Random effects model",
    
    colgap.forest.left  = "10mm",
    colgap.forest.right = "8mm",
    
    col.square = "grey35",
    col.square.lines = "grey35",
    col.diamond = "grey75",
    col.diamond.lines = "grey35",
    col.study = "black"
  )
  
  grid::grid.text(
    meta_row$title_text,
    x = 0.5,
    y = unit(0.965, "npc"),
    just = c("center", "top"),
    gp = grid::gpar(fontsize = 16, fontface = "bold", lineheight = 1)
  )
  
  if (save_png) {
    dev.off()
    message("Saved: ", filename)
  }
  
  invisible(m)
}


# regular Steen Figure 1
# plot_steen(
#   "allcause",
#   exclude_rose_olive = TRUE,
#   filename = "output/steen_fig1_no_rose_olive.png"
# )

plot_steen("allcause", save_png = TRUE,  filename = "output/steen_fig1.png")
# same plot, but drop Rose olive oil entirely
#plot_steen("allcause", exclude_rose_olive = TRUE, subgroup_mode = "combined")




# Steen Figure 2 (Cardiovascular mortality)
# Note:
# The individual row counts and subgroup totals appear internally consistent.
# However, the overall participant total implied by the subgroup totals is 63,109,
# whereas the paper reports 63,083 participants for cardiovascular mortality.
# The 26-participant difference is explained by Rose et al. (1965), where the same
# 26-person control arm is used in both the corn oil and olive oil comparisons.
# So: row counts are fine for reproducing the figure, but the overall total in the
# subgrouped figure double-counts the shared Rose control arm.

steen_dat <- bind_rows(
  steen_dat,
  tribble(
    ~outcome_key,       ~figure_no, ~outcome_label,              ~study,                         ~replace_type, ~event.e, ~n.e,  ~event.c, ~n.c,  ~study_order,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Minnesota Coronary, 1989",     "PUFA",            157, 4541,      147, 4516,  1,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "DART, 1989",                   "PUFA",            101, 1018,      100, 1015,  2,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Lyon Diet Heart, 1994",        "PUFA",              3,  302,       16,  303,  3,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "MRC, 1968",                    "PUFA",             27,  199,       25,  194,  4,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Oslo Diet-Heart, 1966",        "PUFA",             38,  206,       52,  206,  5,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Rose et al (corn oil), 1965",  "PUFA",              5,   28,        1,   26,  6,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "STARS, 1992",                  "PUFA",              1,   27,        3,   28,  7,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Veterans Admin, 1969",         "PUFA",             57,  424,       81,  422,  8,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Sydney Diet-Heart, 1978",      "PUFA",             37,  221,       25,  237,  9,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Black et al., 1994",           "CHO",               0,   66,        2,   67, 10,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Rose et al (olive oil), 1965", "MUFA",              3,   26,        1,   26, 11,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "Ley et al., 2004",             "CHO",               1,   88,        4,   88, 12,
    "cvd_mortality",    2,          "Cardiovascular mortality",  "WHI, 2006",                    "CHO",             213,19541,      320,29294, 13
  )
)

steen_fig_meta$cvd_mortality <- list(
  title_text = "Steen Figure 2: Cardiovascular mortality\nPUFA replacement versus other macronutrients",
  xlim = c(0.01, 100),
  at   = c(0.01, 0.1, 1, 10, 100)
)

plot_steen("cvd_mortality", filename = "output/steen_fig2.png")
#plot_steen("cvd_mortality", exclude_rose_olive = TRUE, filename = "output/steen_fig2_no_rose_olive.png")


# Steen Figure 3 (Nonfatal MI)
# Note:
# The published Figure 3 appears to contain two individual row-total typos.
# Using the paper + supplement, the internally consistent values are:
# - Veterans Admin, 1969: control total = 422 (not 427)
# - Moy et al., 2001: intervention total = 117 (not 177)
# These corrected values match the printed subgroup totals and the printed RR for Moy.

steen_dat <- bind_rows(
  steen_dat,
  tribble(
    ~outcome_key,   ~figure_no, ~outcome_label, ~study,                         ~replace_type, ~event.e, ~n.e,  ~event.c, ~n.c,  ~study_order,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "DART, 1989",                   "PUFA",             35, 1018,       47, 1015,  1,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "Lyon Diet Heart, 1994",        "PUFA",              5,  302,       17,  303,  2,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "MRC, 1968",                    "PUFA",             25,  199,       25,  194,  3,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "Oslo Diet-Heart, 1966",        "PUFA",             24,  206,       31,  206,  4,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "Rose et al (corn oil), 1965",  "PUFA",              7,   28,        5,   26,  5,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "Veterans Admin, 1969",         "PUFA",             13,  424,       21,  427,  6,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "Moy et al., 2001",             "CHO",               2,  177,        1,  118,  7,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "Rose et al (olive oil), 1965", "MUFA",              6,   26,        5,   26,  8,
    "nonfatal_mi_typos",  3,          "Nonfatal MI",  "WHI, 2006",                    "CHO",             459,19541,      684,29294,  9
  )
)

steen_dat <- bind_rows(
  steen_dat,
  tribble(
    ~outcome_key,   ~figure_no, ~outcome_label, ~study,                         ~replace_type, ~event.e, ~n.e,  ~event.c, ~n.c,  ~study_order,
    "nonfatal_mi",  3,          "Nonfatal MI",  "DART, 1989",                   "PUFA",             35, 1018,       47, 1015,  1,
    "nonfatal_mi",  3,          "Nonfatal MI",  "Lyon Diet Heart, 1994",        "PUFA",              5,  302,       17,  303,  2,
    "nonfatal_mi",  3,          "Nonfatal MI",  "MRC, 1968",                    "PUFA",             25,  199,       25,  194,  3,
    "nonfatal_mi",  3,          "Nonfatal MI",  "Oslo Diet-Heart, 1966",        "PUFA",             24,  206,       31,  206,  4,
    "nonfatal_mi",  3,          "Nonfatal MI",  "Rose et al (corn oil), 1965",  "PUFA",              7,   28,        5,   26,  5,
    "nonfatal_mi",  3,          "Nonfatal MI",  "Veterans Admin, 1969",         "PUFA",             13,  424,       21,  422,  6,
    "nonfatal_mi",  3,          "Nonfatal MI",  "Moy et al., 2001",             "CHO",               2,  117,        1,  118,  7,
    "nonfatal_mi",  3,          "Nonfatal MI",  "Rose et al (olive oil), 1965", "MUFA",              6,   26,        5,   26,  8,
    "nonfatal_mi",  3,          "Nonfatal MI",  "WHI, 2006",                    "CHO",             459,19541,      684,29294,  9
  )
)

steen_fig_meta$nonfatal_mi_typos <- list(
  title_text = "Steen Figure 3: Nonfatal MI (displayed row-total typos)\nPUFA replacement versus other macronutrients",
  xlim = c(0.01, 100),
  at   = c(0.01, 0.1, 1, 10, 100)
)

steen_fig_meta$nonfatal_mi <- list(
  title_text = "Steen Figure 3: Nonfatal MI (typos fixed)\nPUFA replacement versus other macronutrients",
  xlim = c(0.01, 100),
  at   = c(0.01, 0.1, 1, 10, 100)
)

#plot_steen("nonfatal_mi_typos", filename = "output/steen_fig3_typos.png")
#plot_steen("nonfatal_mi", exclude_rose_olive = TRUE, filename = "output/steen_fig3_no_rose_olive.png")
plot_steen("nonfatal_mi", filename = "output/steen_fig3_fixed.png")


# Steen Figure 4 (Stroke: fatal and non-fatal)
# Note:
# Unlike Figure 3, Figure 4 appears internally consistent.
# The row counts sum exactly to the printed subgroup totals and overall totals.
# One subtle point: the supplement's CHO-only subgroup summary is smaller because
# Figure 4's "other macronutrients" group also includes Moy et al. (classified as "Unclear"),
# whereas the CHO summary excludes that trial.

steen_dat <- bind_rows(
  steen_dat,
  tribble(
    ~outcome_key, ~figure_no, ~outcome_label,               ~study,                        ~replace_type, ~event.e, ~n.e,  ~event.c, ~n.c,  ~study_order,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "Minnesota Coronary, 1989",    "PUFA",              5, 4541,        8, 4516,  1,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "STARS, 1992",                 "PUFA",              0,   27,        1,   28,  2,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "Lyon Diet Heart, 1994",       "PUFA",              0,  302,        4,  303,  3,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "MRC, 1968",                   "PUFA",              2,  199,        0,  194,  4,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "Oslo Diet-Heart, 1966",       "PUFA",              2,  206,        1,  206,  5,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "Veterans Admin, 1969",        "PUFA",             13,  424,       22,  422,  6,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "Moy et al., 2001",            "CHO",               1,  117,        1,  118,  7,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "Ley et al., 2004",            "CHO",               1,   88,        5,   88,  8,
    "stroke",     4,          "Stroke (fatal and non-fatal)", "WHI, 2006",                   "CHO",             435,19541,      634,29294,  9
  )
)

steen_fig_meta$stroke <- list(
  title_text = "Steen Figure 4: Stroke (fatal and non-fatal)\nPUFA replacement versus other macronutrients",
  xlim = c(0.01, 100),
  at   = c(0.01, 0.1, 1, 10, 100)
)

plot_steen("stroke", filename = "output/steen_fig4.png")
#plot_steen("stroke", exclude_rose_olive = TRUE, filename = "output/steen_fig4_no_rose_olive.png")


#################
# sensitivity analysis


steen_outcomes <- c("allcause", "cvd_mortality", "nonfatal_mi", "stroke")

prep_steen_pufa_data <- function(outcome_name) {
  steen_dat %>%
    filter(
      outcome_key == outcome_name,
      replace_type == "PUFA"
    ) %>%
    arrange(study_order) %>%
    mutate(
      study = factor(study, levels = study)
    ) %>%
    droplevels()
}

fit_steen_pufa_meta <- function(dat, tau = c("REML", "DL")) {
  tau <- match.arg(tau)
  
  metabin(
    event.e = event.e,
    n.e     = n.e,
    event.c = event.c,
    n.c     = n.c,
    studlab = study,
    data    = dat,
    sm      = "RR",
    method  = "MH",
    common  = FALSE,
    random  = TRUE,
    method.tau = tau,
    Q.Cochrane = identical(tau, "DL"),
    method.random.ci = FALSE,
    prediction = FALSE
  )
}

analyze_steen_pufa_loo <- function(outcome_name, tau = c("REML", "DL")) {
  tau <- match.arg(tau)
  
  dat <- prep_steen_pufa_data(outcome_name)
  m   <- fit_steen_pufa_meta(dat, tau = tau)
  inf <- metainf(m, pooled = "random")
  
  figure_label <- paste0(
    "Figure ", unique(dat$figure_no), ": ", unique(dat$outcome_label)
  )
  
  loo_tbl <- tibble(
    figure       = figure_label,
    omitted      = as.character(inf$studlab),
    RR           = exp(inf$TE),
    lower        = exp(inf$lower),
    upper        = exp(inf$upper),
    I2           = inf$I2,
    tau2         = inf$tau2,
    type         = "Leave-one-out"
  ) %>%
    left_join(
      dat %>% select(study, study_order),
      by = c("omitted" = "study")
    ) %>%
    rename(display_order = study_order)
  
  overall_tbl <- tibble(
    figure        = figure_label,
    omitted       = "Full dataset",
    RR            = exp(m$TE.random),
    lower         = exp(m$lower.random),
    upper         = exp(m$upper.random),
    I2            = m$I2,
    tau2          = m$tau2,
    type          = "Overall",
    display_order = 0
  )
  
  bind_rows(overall_tbl, loo_tbl)
}

plot_steen_pufa_loo_all <- function(tau = c("REML", "DL")) {
  tau <- match.arg(tau)
  
  plot_dat <- bind_rows(
    lapply(steen_outcomes, analyze_steen_pufa_loo, tau = tau)
  ) %>%
    mutate(
      omitted_clean = case_when(
        type == "Overall" ~ "Full dataset",
        TRUE ~ omitted %>%
          str_remove("^Omitting\\s+") %>%
          str_remove(",\\s*\\d{4}$") %>%
          str_remove("\\s*\\d{4}$") %>%
          str_replace_all("\\bet al\\.?\\b", "") %>%
          str_replace_all("\\bDiet-Heart\\.?\\b", "") %>%
          str_replace_all("\\bDiet Heart\\.?\\b", "") %>%
          str_replace("^Minnesota Coronary$", "Minnesota") %>%
          str_replace("^Veterans", "Vet") %>%
          str_squish()
      )
    ) %>%
    group_by(figure) %>%
    arrange(display_order, .by_group = TRUE) %>%
    mutate(
      omitted_facet = paste(figure, omitted_clean, sep = "___")
    ) %>%
    ungroup()
  
  plot_dat$omitted_facet <- factor(
    plot_dat$omitted_facet,
    levels = rev(unique(plot_dat$omitted_facet))
  )
  
  ggplot(
    plot_dat,
    aes(x = RR, y = omitted_facet, color = type)
  ) +
    geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
    geom_point(size = 2) +
    scale_x_log10() +
    scale_y_discrete(
      labels = function(x) sub("^.*___", "", x)
    ) +
    scale_color_manual(
      values = c(
        "Leave-one-out" = "#F8766D",
        "Overall" = "#00BFC4"
      )
    ) +
    facet_wrap(~ figure, scales = "free_y", ncol = 2) +
    labs(
      x = "Risk ratio",
      y = NULL,
      title = paste0(
        "Steen PUFA replacement: leave-one-out sensitivity analysis (", tau, ")"
      ),
      subtitle = "Row shows pooled RR after leaving out the named study; 'Full dataset' is the original"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none"
    )
}

steen_loo_reml <- bind_rows(
  lapply(steen_outcomes, analyze_steen_pufa_loo, tau = "REML")
)

steen_loo_dl <- bind_rows(
  lapply(steen_outcomes, analyze_steen_pufa_loo, tau = "DL")
)

#plot_steen_pufa_loo_all("REML")
plot_steen_pufa_loo_all("DL")
