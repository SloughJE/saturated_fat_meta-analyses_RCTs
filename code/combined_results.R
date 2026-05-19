library(dplyr)
library(tibble)
library(meta)
library(ggplot2)
library(forcats)



########
# MUST run yamada and steen _forst_plots.R first
# -----------------------------
# 1) Helpers
# -----------------------------

fit_rr_yamada <- function(dat, tau = "REML") {
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

fit_rr_steen <- function(dat, tau = "REML") {
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
    #Q.Cochrane = identical(tau, "REML"),
    method.random.ci = FALSE,
    prediction = FALSE
  )
}

extract_rr_row <- function(m, outcome_row, meta_analysis, group_label, figure_label) {
  tibble(
    outcome_row   = outcome_row,
    meta_analysis = meta_analysis,
    group_label   = group_label,
    figure_label  = figure_label,
    RR            = exp(m$TE.random),
    lower         = exp(m$lower.random),
    upper         = exp(m$upper.random)
  )
}

blank_rr_row <- function(outcome_row, meta_analysis, group_label = "Overall", figure_label = "") {
  tibble(
    outcome_row   = outcome_row,
    meta_analysis = meta_analysis,
    group_label   = group_label,
    figure_label  = figure_label,
    RR            = NA_real_,
    lower         = NA_real_,
    upper         = NA_real_
  )
}

# -----------------------------
# 2) Yamada pooled results
# -----------------------------

make_yamada_summary <- function(tau = "REML") {
  bind_rows(
    extract_rr_row(
      fit_rr_yamada(dat_yamada_fig3_rr, tau = tau),
      outcome_row   = "All-cause mortality\nSteen Fig 1\nYamada Fig 3",
      meta_analysis = paste0("Yamada (", tau, ")"),
      group_label   = "Overall",
      figure_label  = "Fig 3"
    ),
    extract_rr_row(
      fit_rr_yamada(dat_yamada_fig2_rr, tau = tau),
      outcome_row   = "Cardiovascular mortality\nSteen Fig 2\nYamada Fig 2",
      meta_analysis = paste0("Yamada (", tau, ")"),
      group_label   = "Overall",
      figure_label  = "Fig 2"
    ),
    extract_rr_row(
      fit_rr_yamada(dat_yamada_fig4_rr, tau = tau),
      outcome_row   = "MI outcome\nSteen Fig 3: nonfatal MI\nYamada Fig 4: all MI",
      meta_analysis = paste0("Yamada (", tau, ")"),
      group_label   = "Overall",
      figure_label  = "Fig 4"
    ),
    blank_rr_row(
      outcome_row   = "Stroke\nSteen Fig 4",
      meta_analysis = paste0("Yamada (", tau, ")"),
      group_label   = "Overall",
      figure_label  = ""
    ),
    extract_rr_row(
      fit_rr_yamada(dat_yamada_fig5_rr, tau = tau),
      outcome_row   = "Any coronary artery event\nYamada Fig 5",
      meta_analysis = paste0("Yamada (", tau, ")"),
      group_label   = "Overall",
      figure_label  = "Fig 5"
    )
  )
}

# -----------------------------
# 3) Steen pooled results
#    PUFA vs Other only
# -----------------------------

make_steen_summary_one <- function(outcome_key, outcome_row, figure_label, tau = "REML") {
  dat_all <- steen_dat %>%
    filter(outcome_key == !!outcome_key) %>%
    mutate(group2 = if_else(replace_type == "PUFA", "PUFA", "Other"))
  
  out_overall <- extract_rr_row(
    fit_rr_steen(dat_all, tau = tau),
    outcome_row   = outcome_row,
    meta_analysis = paste0("Steen (", tau, ")"),
    group_label   = "Overall",
    figure_label  = figure_label
  )
  
  out_subgroups <- bind_rows(
    lapply(c("PUFA", "Other"), function(g) {
      dat_g <- dat_all %>% filter(group2 == g)
      
      extract_rr_row(
        fit_rr_steen(dat_g, tau = tau),
        outcome_row   = outcome_row,
        meta_analysis = paste0("Steen (", tau, ")"),
        group_label   = g,
        figure_label  = figure_label
      )
    })
  )
  
  bind_rows(out_overall, out_subgroups)
}

make_steen_summary <- function(tau = "REML") {
  bind_rows(
    make_steen_summary_one(
      outcome_key  = "allcause",
      outcome_row  = "All-cause mortality\nSteen Fig 1\nYamada Fig 3",
      figure_label = "Fig 1",
      tau = tau
    ),
    make_steen_summary_one(
      outcome_key  = "cvd_mortality",
      outcome_row  = "Cardiovascular mortality\nSteen Fig 2\nYamada Fig 2",
      figure_label = "Fig 2",
      tau = tau
    ),
    make_steen_summary_one(
      outcome_key  = "nonfatal_mi",
      outcome_row  = "MI outcome\nSteen Fig 3: nonfatal MI\nYamada Fig 4: all MI",
      figure_label = "Fig 3",
      tau = tau
    ),
    make_steen_summary_one(
      outcome_key  = "stroke",
      outcome_row  = "Stroke\nSteen Fig 4",
      figure_label = "Fig 4",
      tau = tau
    ),
    blank_rr_row(
      outcome_row   = "Any coronary artery event\nYamada Fig 5",
      meta_analysis = paste0("Steen (", tau, ")"),
      group_label   = "Overall",
      figure_label  = ""
    )
  )
}

# -----------------------------
# 4) Combined summary table
# -----------------------------

make_summary_chart_data <- function(yamada_tau = "REML", steen_tau = "REML") {
  bind_rows(
    make_yamada_summary(tau = yamada_tau),
    make_steen_summary(tau = steen_tau)
  ) %>%
    mutate(
      result = if_else(
        is.na(RR),
        "",
        sprintf("%.2f [%.2f, %.2f]", RR, lower, upper)
      ),
      subgroup_type = if_else(group_label == "Overall", "Overall", "Replacement subgroup"),
      outcome_row = factor(
        outcome_row,
        levels = c(
          "All-cause mortality\nSteen Fig 1\nYamada Fig 3",
          "Cardiovascular mortality\nSteen Fig 2\nYamada Fig 2",
          "MI outcome\nSteen Fig 3: nonfatal MI\nYamada Fig 4: all MI",
          "Stroke\nSteen Fig 4",
          "Any coronary artery event\nYamada Fig 5"
        )
      ),
      meta_analysis = factor(
        meta_analysis,
        levels = c(
          paste0("Yamada (", yamada_tau, ")"),
          paste0("Steen (", steen_tau, ")")
        )
      ),
      group_label = factor(
        group_label,
        levels = c(
          "Other",
          "PUFA",
          "Overall"
        )
      )
    )
}

summary_tbl <- make_summary_chart_data(
  yamada_tau = "REML",
  steen_tau  = "REML"
)

summary_tbl_overall = summary_tbl %>%
  filter(group_label == "Overall")

plot_summary_compact <- function(summary_tbl,
                                 x_display_min = 0.70,
                                 x_display_max = 1.25) {
  
  plot_dat <- summary_tbl %>%
    filter(!is.na(RR)) %>%
    mutate(
      meta_short = case_when(
        grepl("^Yamada", meta_analysis) ~ "Yamada",
        grepl("^Steen",  meta_analysis) ~ "Steen"
      ),
      meta_short = factor(meta_short, levels = c("Yamada", "Steen")),
      outcome_short = case_when(
        grepl("^All-cause mortality", outcome_row) ~ "All-cause mortality",
        grepl("^Cardiovascular mortality", outcome_row) ~ "Cardiovascular mortality",
        grepl("^MI outcome", outcome_row) ~ "Yamada: all MI\nSteen: non-fatal MI",
        grepl("^Any coronary artery event", outcome_row) ~ "Any coronary artery event",
        grepl("^Stroke", outcome_row) ~ "Stroke"
      )
    )
  
  outcome_levels <- c(
    "All-cause mortality",
    "Cardiovascular mortality",
    "Yamada: all MI\nSteen: non-fatal MI",
    "Any coronary artery event",
    "Stroke"
  )
  
  plot_dat <- plot_dat %>%
    mutate(
      outcome_short = factor(outcome_short, levels = rev(outcome_levels)),
      outcome_id    = as.numeric(outcome_short),
      y             = outcome_id + if_else(meta_short == "Yamada", 0.16, -0.16),
      xmin_plot     = pmax(lower, x_display_min),
      xmax_plot     = pmin(upper, x_display_max),
      trunc_left    = lower < x_display_min,
      label_x       = pmin(xmax_plot * 1.015, x_display_max * 1.03)
    )
  
  left_trunc_dat <- plot_dat %>% filter(trunc_left)
  
  x_text <- x_display_max * 0.975
  x_max  <- x_display_max * 1.06
  
  ggplot(plot_dat, aes(y = y, color = meta_short)) +
    geom_hline(
      yintercept = seq(1.5, length(outcome_levels) - 0.5, by = 1),
      color = "grey85",
      linewidth = 1
    ) +
    geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
    
    geom_segment(
      aes(x = xmin_plot, xend = xmax_plot, yend = y),
      linewidth = 1.5
    ) +
    
    geom_segment(
      data = left_trunc_dat,
      aes(
        x = x_display_min + 0.02,
        xend = x_display_min,
        y = y,
        yend = y
      ),
      linewidth = 1,
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      show.legend = FALSE
    ) +
    
    geom_point(aes(x = RR), size = 4) +
    
    geom_text(
      aes(x = x_text, label = result),
      hjust = 0,
      size = 4.5,
      color = "grey20",
      show.legend = FALSE
    ) +
    
    scale_x_log10(
      breaks = c(0.7, 0.8, 0.9, 1.0, 1.1, 1.2)
    ) +
    
    scale_y_continuous(
      breaks = seq_along(rev(outcome_levels)),
      labels = rev(outcome_levels)
    ) +
    
    scale_color_manual(
      breaks = c("Yamada", "Steen"),
      values = c(
        "Yamada" = "#B85C2E",
        "Steen"  = "#0B6B61"
      )
    ) +
    
    labs(
      x = "Risk ratio",
      y = NULL,
      title = "Yamada and Steen Summary of Overall Pooled Estimates",
      subtitle = "meta-analyses refit using Risk Ratio and REML for direct comparison",
      caption = "Outcome definitions may vary / "
      
    ) +
    
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      plot.margin = margin(5.5, 20, 5.5, 20)
    ) +
    coord_cartesian(
      xlim = c(x_display_min, x_max),
      clip = "off"
    )
}

plot_summary_compact(summary_tbl_overall)
