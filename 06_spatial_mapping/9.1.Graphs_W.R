library(tidyverse)
library(plotly)
library(openxlsx)
library(ggplot2)

# Example: load data
# site <- "GGE"
# station<- "Großgerau"


site <- "DFH"
station<- "Frankenhausen"
df <- read.xlsx(paste0("Z:/transfer/projects/TRIO/R/PredictRF/results/predict/timeSeries/predictions_by_ZE_wheat_", site, ".xlsx"))


#### Add model identifier for easier long-format plotting ####
# df_long <- df %>%
#   select(split, pred_ZE1, pred_ZE2, pred_ZE3) %>%
#   pivot_longer(cols = starts_with("pred_"), names_to = "model", values_to = "biomass") %>%
#   mutate(model = case_when(
#     model == "pred_ZE1" ~ "ZE1",
#     model == "pred_ZE2" ~ "ZE2",
#     model == "pred_ZE3" ~ "ZE3"
#   ))
# 
# # Assign numeric value for model to use as z-axis
# df_long$model_z <- as.numeric(factor(df_long$model, levels = c("ZE1", "ZE2", "ZE3")))
# 
# # Create 3D line plot
# fig <- plot_ly(df_long, x = ~split, y = ~biomass, z = ~model_z,
#                color = ~model, colors = c("red", "green", "blue"),
#                type = 'scatter3d', mode = 'lines+markers')
# 
# fig <- fig %>%
#   layout(scene = list(
#     xaxis = list(title = 'Split'),
#     yaxis = list(title = 'Predicted Biomass'),
#     zaxis = list(title = 'Model (ZE)', tickvals = 1:2, ticktext = c("ZE1", "ZE2", "ZE3"))
#   ))
# 
# fig

#### Scatter plot - Predicted Biomass Across Growth Stages ####

library(ggplot2)
library(dplyr)
library(tidyr)

df_long <- df %>%
  pivot_longer(cols = starts_with("pred_ZE"),
               names_to = "ZE",
               values_to = "biomass") %>%
  mutate(ZE = recode(ZE,
                     "pred_ZE1" = "Shooting",
                     "pred_ZE2" = "Flowering",
                     "pred_ZE3" = "Milk ripening"),
         ZE = factor(ZE, levels = c("Shooting", "Flowering", "Milk ripening")))

means <- df_long %>%
  group_by(ZE) %>%
  summarise(mean_biomass = mean(biomass, na.rm = TRUE),
            label = paste0(round(mean_biomass, 2), "~~gm^{-2}"))

ggplot(df_long, aes(x = ZE, y = biomass, color = ZE)) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.8) +
  geom_text(data = means, aes(x = ZE, y = mean_biomass, label = label),
            vjust = -0.5, size = 3, fontface = "bold", color = "black", show.legend = FALSE,
            parse = TRUE) +
  scale_color_manual(values = c("Shooting" = "#1b9e77", "Flowering" = "#d95f02", "Milk ripening" = "#7570b3")) +
  labs(title = "Predicted biomass across growth stages",
       subtitle = paste("for wheat at",station),
       x = "Growth Stage",
       y = expression("Predicted biomass (g/m"^2*")"),
       color = "Growth Stage") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray70", size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray60", linewidth = 0.6),
    axis.text= element_text(size=6),
    axis.title = element_text(size = 7),
    plot.title = element_text(face = "bold", size = 8, hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "none",
    # legend.spacing = unit(0.01, "cm"),
    # legend.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"),
    # legend.title = element_text(face = "bold",size= 6),
    # legend.text = element_text(size = 6)
  ) 

ggsave(paste0("results/graphs/wheat/predicted_biomass_scatterPlot_", site, ".jpeg"),
       width = 3, height = 3, dpi = 300)



#### predicted biomass 3D ####
# 
# library(tidyverse)
# library(plotly)
# 
# # Clean and reshape
# df <- df %>%
#   mutate(across(starts_with("pred_ZE"), ~ as.numeric(str_replace(., ",", "."))))
# 
# df_long <- df %>%
#   pivot_longer(cols = starts_with("pred_ZE"),
#                names_to = "ZE_stage",
#                names_prefix = "pred_",
#                values_to = "predicted_biomass") %>%
#   mutate(split = as.numeric(split),
#          ZE_stage_num = case_when(
#            ZE_stage == "ZE1" ~ 1,
#            ZE_stage == "ZE2" ~ 2,
#            ZE_stage == "ZE3" ~ 3
#          ))
# 
# # 3D mesh with color based on biomass (intensity)
# plot_ly(df_long,
#         x = ~split,
#         y = ~ZE_stage_num,
#         z = ~predicted_biomass,
#         intensity = ~predicted_biomass,
#         colors = colorRamp(c("blue", "green", "yellow", "red")),
#         type = "mesh3d") %>%
#   layout(
#     scene = list(
#       xaxis = list(title = "Split"),
#       yaxis = list(title = "Growth Stage", tickvals = c(1, 2, 3), ticktext = c("ZE1", "ZE2", "ZE3")),
#       zaxis = list(title = "Predicted Biomass")
#     ),
#     title = "3D Mesh: Biomass by Split and Growth Stage (Color = Biomass)"
#   )




#### mean predicted biomass ####


df <- df %>%
  mutate(across(starts_with("pred_"), ~ as.numeric(gsub(",", ".", .))))

df_long <- df %>%
  pivot_longer(cols = starts_with("pred_"), names_to = "growth_stage", values_to = "predicted_biomass") %>%
  mutate(
    growth_stage = case_when(
      growth_stage == "pred_ZE1" ~ "Shooting",
      growth_stage == "pred_ZE2" ~ "Flowering",
      growth_stage == "pred_ZE3" ~ "Milk ripening",
      TRUE ~ growth_stage
    ),
    growth_stage = factor(growth_stage, levels = c("Shooting", "Flowering", "Milk ripening")),
    purity = case_when(
      purity == "pure" ~ "pure plots",
      purity == "mixed" ~ "mixed plots",
      TRUE ~ purity
    )
  )

df_summary <- df_long %>%
  group_by(purity, growth_stage) %>%
  summarise(mean_biomass = mean(predicted_biomass, na.rm = TRUE), .groups = "drop")

ggplot(df_summary, aes(x = growth_stage, y = mean_biomass, color = purity, group = purity)) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 1.2) +
  geom_text(aes(label = round(mean_biomass, 3)), vjust = -1.5,hjust=1.5, size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("pure plots" = "#1f78b4", "mixed plots" = "#33a02c"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Mean predicted biomass across growth stages",
    subtitle = "Comparison between pure and mixed plots",
    x = "Growth Stage",
    y = "Mean predicted biomass (grams per sq.meter)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    axis.title = element_text(size = 12, face="bold"),
    axis.text = element_text(color = "gray20"),
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    axis.line = element_line(color = "gray40"),
    plot.margin = margin(5, 5, 5, 5)
  ) 

ggsave(paste0("results/graphs/wheat/pure_mixed_", site, ".jpeg"),
       width = 8, height = 6, dpi = 300)


#### Mean Predicted Biomass by Mixing Partner ####

#### Mean Predicted Biomass by Mixing Partner (excluding Wheat) ####

df_mixedwith <- df %>%
  mutate(across(starts_with("pred_"), ~ as.numeric(gsub(",", ".", .)))) %>%
  pivot_longer(cols = starts_with("pred_"), names_to = "growth_stage", values_to = "predicted_biomass") %>%
  mutate(
    growth_stage = case_when(
      growth_stage == "pred_ZE1" ~ "Shooting",
      growth_stage == "pred_ZE2" ~ "Flowering",
      growth_stage == "pred_ZE3" ~ "Milk ripening",
      TRUE ~ growth_stage
    ),
    growth_stage = factor(growth_stage, levels = c("Shooting", "Flowering", "Milk ripening")),
    mixedwith = factor(mixedwith)
  ) %>%
  filter(!is.na(mixedwith), mixedwith != "", mixedwith != "wheat")

df_summary_mixed <- df_mixedwith %>%
  group_by(mixedwith, growth_stage) %>%
  summarise(mean_biomass = mean(predicted_biomass, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    vjust_label = if_else(mixedwith == "coriander", -.5, -2,5)
  )

ggplot(df_summary_mixed, aes(x = growth_stage, y = mean_biomass, color = mixedwith, group = mixedwith)) +
  geom_line(size = 1.1) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  geom_text(aes(label = round(mean_biomass, 2), vjust = vjust_label), size = 4, show.legend = FALSE) +
  scale_color_manual(
    values = c("coriander" = "#1f78b4", "caraway" = "#33a02c"),
    name = "Mixed With",
    labels = c("caraway" = "Caraway", "coriander" = "Coriander")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # adds 20% space above
  labs(
    title = "Mean Predicted biomass across growth stages",
    subtitle = "Wheat mixed with caraway and coriander",
    x = "Growth Stage",
    y = "Mean predicted biomass (grams per sq.meter)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    axis.line = element_line(color = "gray40"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted")
  )

ggsave(paste0("results/graphs/wheat/mixedwith_comparison_", site, ".jpeg"),
       width = 8, height = 7, dpi = 300)
