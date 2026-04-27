###
# recreate Yamada with RR

library(dplyr)
library(tibble)
library(meta)
library(grid)
library(ggplot2)
library(stringr)

plot_yamada_rr <- function(dat, fig_no, outcome, xlim, at) {
  m <- metabin(
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
    method.tau = "REML",
    method.random.ci = FALSE,
    prediction = FALSE
  )
  
  forest(
    m,
    leftcols  = "studlab",
    leftlabs  = "Study",
    rightcols = c("effect", "ci", "w.random"),
    rightlabs = c("RR", "95% CI", "Weight"),
    
    overall = TRUE,
    overall.hetstat = TRUE,
    test.overall = FALSE,
    
    smlab = "Risk ratio",
    xlab  = "Favors Lower SFA      Favors Higher SFA",
    xlog  = TRUE,
    xlim  = xlim,
    at    = at,
    
    digits = 2,
    digits.weight = 1,
    addrows.below.overall = 4,
    
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
    paste0(
      "Yamada Figure ", fig_no,
      "\nSaturated fatty acid reduction trials on ", outcome
    ),
    x = 0.5, y = 0.9,
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
  
  invisible(m)
}

study_label_map <- c(
  "Ramsden CE 2013"    = "Ramsden CE 2013 (1966)",
  "Burr ML 1989"       = "Burr ML 1989 (1983)",
  "Frantz ID 1989"     = "Frantz ID 1989 (1968)",
  "Leren P 1966"       = "Leren P 1966 (1957)",
  "MRC 1968"           = "MRC 1968 (1960)",
  "Rose GA 1965"       = "Rose GA 1965 (c.1963)",
  "Dayton S 1969"      = "Dayton S 1969 (1959)",
  "Vijayakumar M 2016" = "Vijayakumar M 2016 (c.2009)",
  "Watts GF 1992"      = "Watts GF 1992 (1988)"
)

relabel_yamada_studies <- function(dat) {
  old_order <- as.character(dat$study)
  new_order <- unname(study_label_map[old_order])
  
  dat %>%
    mutate(
      study = dplyr::recode(as.character(study), !!!study_label_map),
      study = factor(study, levels = unique(new_order))
    )
}

# -------------------------
# Figure 2: cardiovascular mortality
# -------------------------

dat_yamada_fig2_rr <- tribble(
  ~study,               ~event.e, ~n.e, ~event.c, ~n.c,
  "Ramsden CE 2013",          34,  221,       26,  237,
  "Burr ML 1989",             97, 1018,       97, 1015,
  "Frantz ID 1989",           83, 4541,       75, 4516,
  "Leren P 1966",             38,  206,       52,  206,
  "MRC 1968",                 27,  199,       25,  194,
  "Rose GA 1965",              8,   54,        1,   26,
  "Dayton S 1969",            57,  424,       81,  422,
  "Vijayakumar M 2016",        0,   99,        0,   99,
  "Watts GF 1992",             1,   27,        3,   28
) %>%
  mutate(
    study = factor(
      study,
      levels = c(
        "Ramsden CE 2013",
        "Burr ML 1989",
        "Frantz ID 1989",
        "Leren P 1966",
        "MRC 1968",
        "Rose GA 1965",
        "Dayton S 1969",
        "Vijayakumar M 2016",
        "Watts GF 1992"
      )
    )
  ) %>%
  arrange(study)

dat_yamada_fig2_rr <- relabel_yamada_studies(dat_yamada_fig2_rr)

plot_yamada_rr(
  dat     = dat_yamada_fig2_rr,
  fig_no  = 2,
  outcome = "cardiovascular mortality",
  xlim    = c(0.1, 10),
  at      = c(0.1, 0.5, 1, 2, 10)
)


# -------------------------
# Figure 3: all-cause mortality
# -------------------------

dat_yamada_fig3_rr <- tribble(
  ~study,               ~event.e, ~n.e, ~event.c, ~n.c,
  "Ramsden CE 2013",          35,  221,       28,  237,
  "Burr ML 1989",            111, 1018,      113, 1015,
  "Frantz ID 1989",          269, 4541,      248, 4516,
  "Leren P 1966",             41,  206,       55,  206,
  "MRC 1968",                 28,  199,       31,  194,
  "Rose GA 1965",              8,   54,        1,   26,
  "Dayton S 1969",           174,  424,      177,  422,
  "Vijayakumar M 2016",        0,   99,        2,   99,
  "Watts GF 1992",             1,   27,        3,   28
) %>%
  mutate(
    study = factor(
      study,
      levels = c(
        "Ramsden CE 2013",
        "Burr ML 1989",
        "Frantz ID 1989",
        "Leren P 1966",
        "MRC 1968",
        "Rose GA 1965",
        "Dayton S 1969",
        "Vijayakumar M 2016",
        "Watts GF 1992"
      )
    )
  ) %>%
  arrange(study)

dat_yamada_fig3_rr <- relabel_yamada_studies(dat_yamada_fig3_rr)

plot_yamada_rr(
  dat     = dat_yamada_fig3_rr,
  fig_no  = 3,
  outcome = "all-cause mortality",
  xlim    = c(0.01, 100),
  at      = c(0.01, 0.1, 1, 10, 100)
)


# -------------------------
# Figure 4: myocardial infarction
# -------------------------

dat_yamada_fig4_rr <- tribble(
  ~study,               ~event.e, ~n.e, ~event.c, ~n.c,
  "Burr ML 1989",            132, 1018,      144, 1015,
  "Leren P 1966",             34,  206,       54,  206,
  "MRC 1968",                 40,  199,       39,  194,
  "Rose GA 1965",             16,   54,        5,   26,
  "Dayton S 1969",            36,  424,       44,  422,
  "Vijayakumar M 2016",        0,   99,        0,   99
) %>%
  mutate(
    study = factor(
      study,
      levels = c(
        "Burr ML 1989",
        "Leren P 1966",
        "MRC 1968",
        "Rose GA 1965",
        "Dayton S 1969",
        "Vijayakumar M 2016"
      )
    )
  ) %>%
  arrange(study)

dat_yamada_fig4_rr <- relabel_yamada_studies(dat_yamada_fig4_rr)

plot_yamada_rr(
  dat     = dat_yamada_fig4_rr,
  fig_no  = 4,
  outcome = "myocardial infarction",
  xlim    = c(0.2, 5),
  at      = c(0.2, 0.5, 1, 2, 5)
)


# -------------------------
# Figure 5: any coronary artery event
# -------------------------

dat_yamada_fig5_rr <- tribble(
  ~study,               ~event.e, ~n.e, ~event.c, ~n.c,
  "Ramsden CE 2013",          32,  221,       24,  237,
  "Burr ML 1989",            132, 1018,      144, 1015,
  "Frantz ID 1989",           61, 4541,       54, 4516,
  "Leren P 1966",             42,  206,       74,  206,
  "MRC 1968",                 62,  199,       74,  194,
  "Rose GA 1965",             21,   54,       10,   26,
  "Dayton S 1969",            47,  424,       54,  422,
  "Vijayakumar M 2016",        0,   99,        0,   99,
  "Watts GF 1992",             2,   26,        5,   24
) %>%
  mutate(
    study = factor(
      study,
      levels = c(
        "Ramsden CE 2013",
        "Burr ML 1989",
        "Frantz ID 1989",
        "Leren P 1966",
        "MRC 1968",
        "Rose GA 1965",
        "Dayton S 1969",
        "Vijayakumar M 2016",
        "Watts GF 1992"
      )
    )
  ) %>%
  arrange(study)

dat_yamada_fig5_rr <- relabel_yamada_studies(dat_yamada_fig5_rr)

plot_yamada_rr(
  dat     = dat_yamada_fig5_rr,
  fig_no  = 5,
  outcome = "any coronary artery event",
  xlim    = c(0.1, 10),
  at      = c(0.1, 0.5, 1, 2, 10)
)


#########################################

# Published Yamada Figure 4
dat_yamada_mi_pub <- tribble(
  ~study,                ~event.e, ~n.e, ~event.c, ~n.c,
  "Burr ML 1989",             132, 1018,      144, 1015,
  "Leren P 1966",              34,  206,       54,  206,
  "MRC 1968",                  40,  199,       39,  194,
  "Rose GA 1965",              16,   54,        5,   26,
  "Dayton S 1969",             36,  424,       44,  422,
  "Vijayakumar M 2016",         0,   99,        0,   99
)

# 1) Minimal internal repair: remove Burr/DART
dat_yamada_mi_no_burr <- dat_yamada_mi_pub %>%
  filter(study != "Burr ML 1989")

# 4) Optional appendix only:
# non-fatal MI substitution for Burr from DART
# useful as a sensitivity analysis, but not a preferred main repair
dat_yamada_mi_dart_nfmi <- tribble(
  ~study,                ~event.e, ~n.e, ~event.c, ~n.c,
  "Burr ML 1989",              35, 1018,       47, 1015,
  "Leren P 1966",              34,  206,       54,  206,
  "MRC 1968",                  40,  199,       39,  194,
  "Rose GA 1965",              16,   54,        5,   26,
  "Dayton S 1969",             36,  424,       44,  422,
  "Vijayakumar M 2016",         0,   99,        0,   99
)

datasets <- list(
  "Published Yamada Figure 4" = dat_yamada_mi_pub,
  "Minimal repair: Burr removed" = dat_yamada_mi_no_burr,
  "Appendix only: Burr replaced with DART non-fatal MI 35/47" = dat_yamada_mi_dart_nfmi
)

dat_yamada_mi_pub <- relabel_yamada_studies(dat_yamada_mi_pub)
dat_yamada_mi_no_burr <- relabel_yamada_studies(dat_yamada_mi_no_burr)
dat_yamada_mi_dart_nfmi <- relabel_yamada_studies(dat_yamada_mi_dart_nfmi)


models <- lapply(datasets, fit_or)

# summaries
lapply(models, summary)

############

# published plot
# plot_yamada_rr(
#   dat     = dat_yamada_mi_pub,
#   fig_no  = 4,
#   outcome = "myocardial infarction\npublished",
#   xlim    = c(0.2, 5),
#   at      = c(0.2, 0.5, 1, 2, 5)
# )

# plot without Burr

# plot_yamada_rr(
#   dat     = dat_yamada_mi_no_burr,
#   fig_no  = 4,
#   outcome = "myocardial infarction\nBurr removed",
#   xlim    = c(0.2, 5),
#   at      = c(0.2, 0.5, 1, 2, 5)
# )
# 
# 
# plot_yamada_rr(
#   dat     = dat_yamada_mi_dart_nfmi,
#   fig_no  = 4,
#   outcome = "myocardial infarction\nnon-fatal MI substitution for Burr from DART",
#   xlim    = c(0.2, 5),
#   at      = c(0.2, 0.5, 1, 2, 5)
# )



# dat_yamada_mi_pub_no_rose <- dat_yamada_mi_pub %>%
#   filter(study != "Rose GA 1965 (c.1963)")
# 
# 
# dat_yamada_fig2_rr_no_ram = dat_yamada_fig2_rr %>%
#   filter(study!= "Ramsden CE 2013 (1966)")
# 
# dat_yamada_fig3_rr_no_ram = dat_yamada_fig3_rr %>%
#   filter(study!= "Ramsden CE 2013 (1966)")
# 
# dat_yamada_fig4_rr_no_ram = dat_yamada_fig4_rr %>%
#   filter(study!= "Ramsden CE 2013 (1966)")
# 
# dat_yamada_fig5_rr_no_ram = dat_yamada_fig5_rr %>%
#   filter(study!= "Ramsden CE 2013 (1966)")



# plot_yamada_rr(
#   dat     = dat_yamada_fig5_rr_no_ram,
#   fig_no  = 4,
#   outcome = "All cause\nnon-fatal MI substitution for Burr from DART",
#   xlim    = c(0.2, 5),
#   at      = c(0.2, 0.5, 1, 2, 5)
# )



########
# LOO Sensitivity Analysis

datasets <- list(
  "Figure 2: cardiovascular mortality" = dat_yamada_fig2_rr,
  "Figure 3: all-cause mortality"      = dat_yamada_fig3_rr,
  "Figure 4: myocardial infarction"    = dat_yamada_fig4_rr,
  "Figure 5: any coronary event"       = dat_yamada_fig5_rr
)

fit_rr_meta <- function(dat, tau = "REML") {
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
    method.random.ci = FALSE,
    prediction = FALSE
  )
}

analyze_rr_dataset <- function(dat, fig_label, tau = "REML") {
  main_fit <- fit_rr_meta(dat, tau = tau)
  loo_obj  <- metainf(main_fit, pooled = "random")
  
  loo_tbl <- bind_rows(
    tibble(
      figure  = fig_label,
      omitted = "Full dataset",
      RR      = exp(main_fit$TE.random),
      lower   = exp(main_fit$lower.random),
      upper   = exp(main_fit$upper.random),
      I2      = main_fit$I2,
      tau2    = main_fit$tau2,
      type    = "Overall"
    ),
    tibble(
      figure  = fig_label,
      omitted = as.character(loo_obj$studlab),
      RR      = exp(loo_obj$TE),
      lower   = exp(loo_obj$lower),
      upper   = exp(loo_obj$upper),
      I2      = loo_obj$I2,
      tau2    = loo_obj$tau2,
      type    = "Leave-one-out"
    )
  )
  
  loo_tbl
}

make_loo_data <- function(datasets, tau = "REML") {
  out <- bind_rows(
    lapply(names(datasets), function(nm) {
      analyze_rr_dataset(datasets[[nm]], nm, tau = tau)
    })
  )
  
  out %>%
    mutate(
      omitted_clean = case_when(
        type == "Overall" ~ "Full dataset",
        TRUE ~ omitted %>%
          str_remove("^Omitting\\s+") %>%
          str_remove("\\s*\\([^)]*\\)$") %>%
          str_remove("\\s+[0-9]{4}$") %>%
          str_replace("\\s+.*$", "")
      )
    ) %>%
    group_by(figure) %>%
    mutate(
      omitted_clean = factor(omitted_clean, levels = rev(unique(omitted_clean)))
    ) %>%
    ungroup()
}

plot_loo <- function(loo_plot, title_text) {
  ggplot(
    loo_plot,
    aes(x = RR, y = omitted_clean, color = type)
  ) +
    geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
    geom_point(size = 2) +
    scale_x_log10() +
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
      title = title_text,
      subtitle = "Row shows pooled RR after leaving out the named study; 'Full dataset' is the original"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none"
    )
}

# REML version
loo_reml <- make_loo_data(datasets, tau = "REML")
plot_loo(loo_reml, "Yamada: Leave-one-out sensitivity analysis (REML)")

# DL version
#loo_dl <- make_loo_data(datasets, tau = "DL")
#plot_loo(loo_dl, "Yamada: Leave-one-out sensitivity analysis (DL)")
