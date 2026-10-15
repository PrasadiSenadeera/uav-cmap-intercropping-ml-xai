rm(list=ls())
setwd("Z:/transfer/projects/TRIO/R/2024/AnalyseMedicinal")

library(readxl); library(caret); library(conflicted); library(desplot); library(emmeans); library(easystats)
library(equatiomatic); library(extrafont); library(forcats); library(ggh4x); library(ggplot2); library(ggpubr)
library(ggtext); library(gridExtra); library(hrbrthemes); library(lme4); library(lmerTest); library(MetBrewer)
library(multcomp); library(multcompView); library(olsrr); library(plotly); library(qqplotr); library(scales)
library(stringr); library(viridis); library(dplyr)

base_path <- "./results/caraway/stats_M/for_lme_perMLperinputconfig/"
RMSE=RMSE

df_ph_rf <- read_excel(paste0(base_path, "height_RF.xlsx"))
df_sp_rf <- read_excel(paste0(base_path, "spectral_RF.xlsx"))
df_spph_rf <- read_excel(paste0(base_path, "spectral_height_RF.xlsx"))

df_ph_svm <- read_excel(paste0(base_path, "height_SVM.xlsx"))
df_sp_svm <- read_excel(paste0(base_path, "spectral_SVM.xlsx"))
df_spph_svm <- read_excel(paste0(base_path, "spectral_height_SVM.xlsx"))

df_ph_plsr <- read_excel(paste0(base_path, "height_PLSR.xlsx"))
df_sp_plsr <- read_excel(paste0(base_path, "spectral_PLSR.xlsx"))
df_spph_plsr <- read_excel(paste0(base_path, "spectral_height_PLSR.xlsx"))

df <- rbind(
  df_ph_rf, df_sp_rf, df_spph_rf,
  df_ph_svm, df_sp_svm, df_spph_svm,
  df_ph_plsr, df_sp_plsr, df_spph_plsr
)

df <- df %>% 
  mutate(
    rsvariables = fct_relevel(rsvariables, "SP", "PH", "PH_SP"),
    mlvariables = fct_relevel(mlvariables, "RF", "SVM", "PLSR"),
    Resample = as.factor(Resample),
    nRMSE_range = as.integer(round(nRMSE_range * 100, 0))
  )

model <- lme4::lmer(nRMSE_range ~ rsvariables * mlvariables + (1|Resample), data = df, REML = FALSE)

mean_comp <- model %>%
  emmeans(specs = ~ rsvariables | mlvariables, lmer.df = "kenward-roger") %>%
  cld(adjust = "Tukey", Letters = letters, alpha = 0.05)

model_means_cld <- mean_comp %>%
  mutate(rsvariables = fct_relevel(rsvariables, "SP", "PH", "PH_SP"))

df <- df %>%
  mutate(rsvariables = fct_relevel(rsvariables, "SP", "PH", "PH_SP"))

df$mlvariables <- factor(df$mlvariables, levels = c("RF", "SVM", "PLSR"),
                         labels = c("RF", "SVR", "PLSR"))

model_means_cld$mlvariables <- factor(model_means_cld$mlvariables,
                                      levels = c("RF", "SVM", "PLSR"),
                                      labels = c("RF", "SVR", "PLSR"))

plot_RMSE <- ggplot() +
  geom_boxplot(
    data = df,
    aes(x = rsvariables, y = nRMSE_range),
    width = 0.2,
    color = "black",
    alpha = 1
  ) +
  stat_summary(
    data = df,
    aes(x = rsvariables, y = nRMSE_range, label = round(..y.., 2)),
    fun = median,
    geom = "text",
    vjust = 0.5,
    hjust = 1.5,
    size = 3.5,
    color = "blue"
  ) +
  labs(
    x = "Remote sensing variables for modelling dry biomass of Caraway",
    y = expression(nRMSE[cv])
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(labels = c("SP" = "MS", "PH" = "CH", "PH_SP" = "MS+CH")) +
  geom_point(
    data = df,
    aes(y = nRMSE_range, x = rsvariables, fill = rsvariables),
    shape = 21,
    color = "black",
    alpha = 0.8,
    position = position_nudge(x = -0.2)
  ) +
  scale_fill_viridis(discrete = TRUE) +
  geom_point(
    data = model_means_cld,
    aes(y = emmean, x = rsvariables),
    position = position_nudge(x = 0.2),
    size = 2,
    color = "deeppink4"
  ) +
  geom_errorbar(
    data = model_means_cld,
    aes(ymin = lower.CL, ymax = upper.CL, x = rsvariables),
    width = 0.1,
    position = position_nudge(x = 0.2),
    color = "deeppink4"
  ) +
  geom_text(
    data = model_means_cld,
    aes(
      y = emmean,
      x = rsvariables,
      label = str_trim(.group)
    ),
    position = position_nudge(x = 0.3),
    hjust = 0,
    color = "deeppink4"
  ) +
  theme_ipsum(
    base_family = "Bahnschrift",
    base_size = 11.5,
    plot_title_size = 18,
    plot_title_face = "bold",
    plot_title_margin = 10,
    subtitle_size = 12,
    subtitle_face = "plain",
    subtitle_margin = 15,
    strip_text_size = 12,
    strip_text_face = "plain",
    caption_size = 9,
    caption_face = "italic",
    caption_margin = 10,
    axis_text_size = 12,
    axis_title_size = 14,
    axis_title_face = "plain",
    axis_title_just = "c",
    grid_col = "#cccccc",
    grid = TRUE,
    axis_col = "#cccccc",
    axis = FALSE,
    ticks = FALSE
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggh4x::facet_grid2(. ~ mlvariables)

plot_RMSE

ggplot2::ggsave(
  filename = "Z:/transfer/projects/TRIO/R/2024/AnalyseMedicinal/results/Caraway/stats_M/for_lme_perMLperinputconfig/caraway_model.png",
  plot = plot_RMSE,
  width = 8.5,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

summary_table <- df %>%
  group_by(mlvariables, rsvariables) %>%
  summarise(
    Median_R2cv = median(Rsquared, na.rm = TRUE),
    Median_RMSEcv = median(RMSE, na.rm = TRUE),
    Median_nRMSErange = median(nRMSE_range, na.rm = TRUE)
  ) %>%
  arrange(mlvariables, rsvariables)

summary_table