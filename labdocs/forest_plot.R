library(tidyverse)      # for data wrangling
library(forestploter)   # for the forest plot
library(grid)           # grid and gridExtra to glue a couple plots together
library(gridExtra)

# some summary data calculated elsewhere
mace <- tribble(
  ~Subgroup, ~est, ~low.ci, ~high.ci, ~p.interaction,
  #  Subgroup, point estimate, low end of 95% CI, high end of 95% CI, p-value for interaction
  "Sex", NA, NA, NA, 0.664,
  "Male", 1.02, 0.95, 1.09, NA,
  "Female", 1.04, 0.96, 1.12, NA,
  "Race", NA, NA, NA, 0.203,
  "White", 1.00, 0.94, 1.06, NA,
  "Black", 1.12, 1.01, 1.26, NA,
  "Other", 1.01, 0.81, 1.26, NA,
  "Age group", NA, NA, NA, 0.244,
  "<65 y", 1.08, 0.98, 1.18, NA,
  "≥65 y", 1.01, 0.95, 1.07, NA,
  "History of ESA", NA, NA, NA, 0.745,
  "No", 1.00, 0.86, 1.17, NA,
  "Yes", 1.03, 0.98, 1.09, NA
)

death <- tribble(
  ~Subgroup, ~est, ~low.ci, ~high.ci, ~p.interaction,
  #  Subgroup, point estimate, low end of 95% CI, high end of 95% CI, p-value for interaction
  "Sex", NA, NA, NA, 0.476,
  "Male", 0.93, 0.87, 1.00, NA,
  "Female", 0.89, 0.82, 0.96, NA,
  "Race", NA, NA, NA, 0.289,
  "White", 0.91, 0.86, 0.97, NA,
  "Black", 0.94, 0.84, 1.06, NA,
  "Other", 0.76, 0.60, 0.98, NA,
  "Age group", NA, NA, NA, 0.971,
  "<65 y", 0.93, 0.84, 1.04, NA,
  "≥65 y", 0.91, 0.86, 0.96, NA,
  "History of ESA", NA, NA, NA, 0.836,
  "No", 0.90, 0.75, 1.05, NA,
  "Yes", 0.92, 0.87, 0.97, NA
)


mace <- mace |> 
  mutate(Subgroup = if_else(is.na(est), Subgroup, paste0("    ", Subgroup)),
         se = (log(high.ci) - log(est))/1.96,
         ` ` = paste(rep(" ", 35), collapse = " "),
         `HR (95% CI)` = if_else(is.na(se), "", sprintf("%.2f (%.2f - %.2f)", est, low.ci, high.ci)),
         `p-interaction` = if_else(is.na(p.interaction), "", paste(p.interaction))) |> 
  select(-p.interaction)

death <- death |> 
  mutate(Subgroup = if_else(is.na(est), Subgroup, paste0("    ", Subgroup)),
         se = (log(high.ci) - log(est))/1.96,
         ` ` = paste(rep(" ", 35), collapse = " "),
         `HR (95% CI)` = if_else(is.na(se), "", sprintf("%.2f (%.2f - %.2f)", est, low.ci, high.ci)),
         `p-interaction` = if_else(is.na(p.interaction), "", paste(p.interaction))) |> 
  select(-p.interaction)

# forest plot theme
tm <- forest_theme(base_size = 10,
                   refline_col = "black",
                   arrow_type = "closed")

# create forest plots
p1 <- forest(mace[,c(1, 6:8)],
             est = mace$est,
             lower = mace$low.ci, 
             upper = mace$high.ci,
             sizes = mace$se,
             ci_column = 2,
             ref_line = 1,
             x_trans = "log",
             arrow_lab = c("Long-acting Better", "Short-acting Better"),
             xlim = c(0.5, 1.5),
             ticks_at = c(0.5, 0.75, 1, 1.25, 1.5),
             theme = tm)

p2 <- forest(death[,c(1, 6:8)],
              est = death$est,
              lower = death$low.ci, 
              upper = death$high.ci,
              sizes = death$se,
              ci_column = 2,
              ref_line = 1,
              x_trans = "log",
              arrow_lab = c("Long-acting Better", "Short-acting Better"),
              xlim = c(0.5, 1.5),
              ticks_at = c(0.5, 0.75, 1, 1.25, 1.5),
              theme = tm) 

p1p <- forest(mace[,c(1, 6:8)],
            est = mace$est,
            lower = mace$low.ci, 
            upper = mace$high.ci,
            sizes = mace$se,
            ci_column = 2,
            ref_line = 1,
            x_trans = "log",
            arrow_lab = c("Long-acting Better", "Short-acting Better"),
            xlim = c(0.5, 1.5),
            ticks_at = c(0.5, 0.75, 1, 1.25, 1.5),
            theme = tm,
            title = "A. Major Adverse Cardiovascular Events") 

p2p <- forest(death[,c(1, 6:8)],
            est = death$est,
            lower = death$low.ci, 
            upper = death$high.ci,
            sizes = death$se,
            ci_column = 2,
            ref_line = 1,
            x_trans = "log",
            arrow_lab = c("Long-acting Better", "Short-acting Better"),
            xlim = c(0.5, 1.5),
            ticks_at = c(0.5, 0.75, 1, 1.25, 1.5),
            theme = tm,
            title = "B. All-cause Mortality") 

# glue plots together
# gridExtra::grid.arrange(p, p2, ncol=1, nrow=2)

# panel figure
g <- arrangeGrob(p1p, p2p, nrow=2) 

ggsave(file = "/Users/stevensmith/Downloads/ESA_ms_forestplot_panel.svg",
       g,
       device = "svg",
       width = 7,
       height = 8.5,
       units = "in")
ggsave(file = "/Users/stevensmith/Downloads/ESA_ms_forestplot_panel.png",
       g,
       device = "png",
       width = 7,
       height = 8.5,
       units = "in")

# individual figures
plot(p1)
dev.copy(svg, "/Users/stevensmith/Downloads/ESA_ms_forestplot_mace.svg")
dev.off()
dev.copy(png, "/Users/stevensmith/Downloads/ESA_ms_forestplot_mace.png")
dev.off()

plot(p2)
dev.copy(svg, "/Users/stevensmith/Downloads/ESA_ms_forestplot_death.svg")
dev.off()
dev.copy(png, "/Users/stevensmith/Downloads/ESA_ms_forestplot_death.png")
dev.off()

