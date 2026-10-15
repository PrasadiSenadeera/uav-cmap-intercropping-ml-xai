rm (list = ls())

library("plyr")
library("dplyr")
library("readr")
library("ggplot2")
library("tidyr")
library("ggpubr")
library("lme4")
library("nlme")
library("emmeans")
library("multcompView")
library("openxlsx")
library("viridis")
library("hrbrthemes")
library("ggtext")
library("extrafont")
library("ggh4x")
library("scales")


# Define site and read data
site <- "DFH"
df <- read_csv(("Z:/transfer/projects/TRIO/R/2024/01_StatisticalAnalysis/Predicted/predictions_by_ZE_caraway_DFH.csv")) %>% 
  mutate(purity = factor(purity, levels = c("pure", "mixed")), 
         block = substr(split_id, 1, 7))
df





########## Using all growth stages data #################

df_long <- df %>%
  pivot_longer(
    cols = starts_with("pred_ZE"),
    names_to = "Stage",
    values_to = "pred_DB"
  ) %>% 
  mutate(Stage = case_when(Stage == "pred_ZE1" ~ "GS1", 
                           Stage == "pred_ZE2" ~ "GS2", 
                           Stage == "pred_ZE3" ~ "GS3"), 
         Stage = factor(Stage)) %>% 
  mutate(Date = case_when(Stage == "GS1" ~ "2024-06-10", 
                          Stage == "GS2" ~ "2024-07-08", 
                          Stage == "GS3" ~ "2024-07-24"), 
         Date = as.Date(Date), 
         BBCH = case_when(Stage == "GS1" ~ 30, 
                          Stage == "GS2" ~ 60, 
                          Stage == "GS3" ~ 70)) %>% 
  dplyr::filter(BBCH != 30) %>% 
  droplevels()

df_long

# Fit the linear mixed model with (anisotropic) covariance structure
model <- lme(
  fixed = pred_DB ~ purity * Stage,
  random = ~ 1 | block,  # Plot as random effect
  correlation = corSpatial(
    form = ~ X_centroid + Y_centroid + BBCH,  # Coordinates and time
    type = "spherical",  # Power model
    metric = "euclidean",  # Euclidean distance
    nugget = TRUE  # Allow for nugget effect
  ),
  
  # correlation = corGaus(form = ~ x + y + date, nugget = TRUE),
  data = df_long,
  method = "REML",  # Use REML estimation
  control = lmeControl(opt = "optim", msMaxIter = 1000)
)

# Model summary
summary(model)

# Type III tests for fixed effects
car::Anova(model, type = "III")


# Estimated marginal means for loc × cut
emm <- emmeans(model, ~ Stage * purity)
emm_df <- as.data.frame(emm)

# Pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "sidak")


# Get compact letter display
cld_emm <- multcomp::cld(emm, Letters = letters, adjust = "sidak")

# Convert to data frame for plotting
cld_df <- as.data.frame(cld_emm)
cld_df <- cld_df %>%
  rename(emmean = emmean, SE = SE, lower.CL = lower.CL, upper.CL = upper.CL, label = .group)


ggplot(cld_df, aes(x = purity, y = emmean, color = Stage, group = Stage)) +
  geom_point(size = 3) +                                      # points for EMMs
  geom_line(linewidth = 1) +                                       # connect points by location
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +  # CI bars
  geom_text(aes(label = label), vjust = -0.5, size = 4) +     # letters above points
  labs(
    y = "Estimated marginal mean of wheat DB",
    x = "Pure vs Mixed",
    color = "Location",
    title = "EMMs of DB by Pure vs Mixed"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

df_scales <- df_long %>%
  group_by(Stage) %>%
  summarise(
    ymin = min(pred_DB, na.rm = TRUE),
    ymax = max(pred_DB, na.rm = TRUE)
  ) %>%
  rename(Panel = Stage) %>%
  split(.$Panel)

scales <- lapply(df_scales, function(x) {
  scale_y_continuous(limits = c(x$ymin, x$ymax))
})


a= ggplot() +
    geom_boxplot(
      data = df_long,
      aes(x = purity, y = pred_DB),
      width = 0.3,
      fill = "white",
      color = "black",
      linewidth = 0.4
    )+ 
    stat_summary(
      data= df_long,
      aes(x = purity, y = pred_DB, label = round(after_stat(y), 1)),
      fun = mean,
      geom = "text",
      vjust = 0,
      hjust = 1.5,
      size = 3.5,
      color = "blue"
    ) +
    labs(x = "Cropping system for Caraway",
         y = expression("Predicted caraway DB (gm"^-2*")"))+
    scale_x_discrete(labels = c("mixed" = " Intercropped \nwith Wheat", "pure" = "Pure")) +
    # geom_point(
    #   data = df_long,
    #   aes(y = pred_DB, x = purity),
    #   shape = 1,
    #   color = "black",
    #   alpha = 0.8,
    #   position = position_nudge(x = -0.2)
    # ) + 
    geom_point(
      data = cld_df,
      aes(y = emmean, x = purity),
      position = position_nudge(x = 0.2),
      size = 2,
      color = "deeppink4"
    ) +
    geom_errorbar(
      data = cld_df,
      aes(ymin = lower.CL, ymax = upper.CL, x = purity),
      width = 0.1,
      position = position_nudge(x = 0.2),
      color = "deeppink4"
    ) +
    geom_text(
      data = cld_df,
      aes(
        y = emmean,
        x = purity,
        label = label
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
      axis_text_size = 10,
      axis_title_size = 12,
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
      plot.title = element_text(size = 15)
    ) + 
    ggh4x::facet_grid2(. ~ Stage, labeller = label_parsed)+
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 13),
      axis.text.x = element_text(size = 10),
      panel.spacing = unit(1, "lines"),
      panel.grid.major = element_line(color = "grey80", linetype = "dashed", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5)
    )

ggplot2::ggsave(
  filename = "Z:/transfer/projects/TRIO/R/2024/01_StatisticalAnalysis/RPlots/caraway_predict.png",
  plot = a,
  width = 6, height = 7, units = "in",
  dpi = 600, bg = "white"
)
