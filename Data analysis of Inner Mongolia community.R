#### Data analysis of Inner Mongolia community
#######################################################################################
# 1.data import
library(ggplot2)
library(dplyr)
library(purrr)
library(gridExtra)

load("data_for_process.RData")

data0731 <- data1

years <- unique(data0731$SampleYear)

species_all1 <- unique(data0731$AccSpeciesName)
data0731$plot_unique <- paste0(data0731$SampleDate, data0731$Plot)

na_matrix <- matrix(NA, nrow = 41, ncol = 1)
result_data_frame <- as.data.frame(na_matrix)
colnames(result_data_frame) <- c("year")
result_data_frame$year <- years
result_data_frame$year <- as.numeric(result_data_frame$year)

#######################################################################################
# 2.calculate GSP、GST
library(xlsx)
qihou <- read.xlsx("climate_data_1981_2024.xlsx" ,sheetIndex = 1)
qihou <- qihou[qihou$Year %in% years, ]

result_data_frame$GSP <- rowSums(qihou[, 18:21], na.rm = TRUE)
result_data_frame$GST <- rowMeans(qihou[, 6:9], na.rm = TRUE)
result_data_frame$GSP_scaled <- scale(result_data_frame$GSP)
result_data_frame$GST_scaled <- scale(result_data_frame$GST)

#######################################################################################
# 3.Observed Species Competition Strength
library(ggplot2)
library(dplyr)
library(gvlma)

# Group by sampling time and species name, and count the number of sample squares that appeared for each species at each sampling time
species_count <- data0731 %>%
  group_by(SampleYear, AccSpeciesName) %>%
  summarise(PlotCount = n_distinct(plot_unique), .groups = "drop")

# Group by sampling time to identify the species that appear in more than four sample plots and the remaining species
result1 <- species_count %>%
  group_by(SampleYear) %>%
  group_map(~ {
    top_species <- .x$AccSpeciesName[.x$PlotCount > 4]
    list(top_species = top_species)
  })

# Extract the list corresponding to each sampling time
top_species_list <- lapply(result1, function(x) x$top_species)

spe_comp_all <- sort(unique(unlist(top_species_list, use.names = FALSE)))
spe_comp_matrix <- matrix(NA, nrow = length(spe_comp_all), ncol = 41)
rownames(spe_comp_matrix) <- spe_comp_all
colnames(spe_comp_matrix) <- years

result_data_frame$nega_mean_obs_interspec_competition <- NA
result_data_frame$abs_cv_obs_interspec_competition <- NA

plot_list1 <- list()
for(zz in 1:41){
  
  curr_spe <- c()
  other_spe <- c()
  spe_name <- c()
  
  top_species_list1 <- top_species_list[[zz]]
  nega_obs_interspec_competition <- c()
  
  years1 <- years[zz]
  data2 <- data0731[data0731$SampleYear %in% years1, ]
  unique_species <- unique(data2$AccSpeciesName)
  
  unique_species <- unique_species[unique_species %in% top_species_list1]
  # Traverse every species
  for (sp in unique_species) {
    # Filter out the data of the current species
    species_data <- data2[data2$AccSpeciesName == sp, ]
    other_species_biomass <- c()
    current_species_biomass <- c()
    
    # For each quadrat
    for (q in unique(species_data$plot_unique)) {
      # Filter out the data of the current sample quadrat
      quadrat_data <- data2[data2$plot_unique == q, ]
      
      # Calculate the total biomass of all species except the current one in the current sample quadrat
      other_species_biomass <- c(other_species_biomass, sum(quadrat_data$WeightDry[quadrat_data$AccSpeciesName != sp]))
      
      # Extract the biomass of the current species in the current sample plot
      current_species_biomass <- c(current_species_biomass, quadrat_data$WeightDry[quadrat_data$AccSpeciesName == sp])
    }
    
    model <- lm(current_species_biomass ~ other_species_biomass)
    summary_model <- summary(model)
    nega_obs_interspec_competition <- c(nega_obs_interspec_competition, summary_model$coefficients["other_species_biomass", "Estimate"])
    
    curr_spe <- c(curr_spe, current_species_biomass)
    other_spe <- c(other_spe, other_species_biomass)
    spe_name <- c(spe_name, rep(sp, length(species_data$plot_unique)))
    spe_comp_matrix[sp,zz] <- summary_model$coefficients["other_species_biomass", "Estimate"]
  }
  if(is.null(nega_obs_interspec_competition)){
    result_data_frame$nega_mean_obs_interspec_competition[zz] <- NA
    result_data_frame$abs_cv_obs_interspec_competition[zz] <- NA
    
  }else{
    result_data_frame$nega_mean_obs_interspec_competition[zz] <- mean(nega_obs_interspec_competition, na.rm = TRUE)
    result_data_frame$abs_cv_obs_interspec_competition[zz] <- abs(sd(nega_obs_interspec_competition, na.rm = TRUE)/result_data_frame$nega_mean_obs_interspec_competition[zz])
    
  }
  
  spe_jz <- data.frame(curr_spe,other_spe,spe_name)
  
  
  p <- ggplot(spe_jz, aes(x = other_spe, y = curr_spe, color = spe_name)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(
      aes(group = spe_name),
      method = "lm", se = FALSE, linewidth = 0.8, alpha = 0.5
    ) +  
    annotate("text", x = min(spe_jz$other_spe), y = max(spe_jz$curr_spe), 
             label = paste0("Year = ", years[zz]), hjust = 0, vjust = 1, size = 9) +
    labs(
      x = "Dry Biomass Sum of Other Species", 
      y = "Dry Biomass of Each Species",
      color = "Species Name"
    ) +
    
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  
    theme_minimal(base_family = "Arial") +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      aspect.ratio = 1,  
      panel.grid = element_blank(),  
      text = element_text(size = 15),  # Base text size
      axis.title = element_text(size = 20),  # Axis titles
      axis.text = element_text(size = 15),  # Axis labels
      legend.title = element_text(size = 15),  # Legend title
      legend.text = element_text(size = 15),  # Legend items
      axis.ticks = element_line(color = "black"),  # Axis ticks
      axis.ticks.length = unit(0.2, "cm")  # Tick length
    ) +
    guides(color = guide_legend(ncol = 1))
  
  plot_list1[[zz]] <- p
}

nrow_per_page <- 8
ncol_per_page <- 3
plots_per_page <- nrow_per_page * ncol_per_page

total_pages <- ceiling(length(plot_list1) / plots_per_page)


for (page in 1:total_pages) {
  
  start_index <- (page - 1) * plots_per_page + 1
  end_index <- min(page * plots_per_page, length(plot_list1))
  current_plots <- plot_list1[start_index:end_index]
  
  combined_plot <- grid.arrange(
    grobs = current_plots,
    nrow = nrow_per_page,
    ncol = ncol_per_page,
    padding = unit(1, "lines"))
  
  ggsave(
    filename = paste0("calcualte_spe_comp_inte_", page, ".png"),
    plot = combined_plot,
    width = 32,  
    height = 60,
    dpi = 300,    
    limitsize = FALSE
  )
}
plot_list1 <- list()


#######################################################################################
# 4.species pool size, interspecific biomass variability, biomass dominance by the 3 most abundant species

# species pool size
unique(data$AccSpeciesName)
result <- data %>%
  group_by(SampleYear) %>%
  filter(!AccSpeciesName %in% paste0("sp", 1:18)) %>%
  summarise(species_pool = n_distinct(AccSpeciesName))
result_data_frame$SampleYear <- as.character(result_data_frame$year)
result_data_frame <- result_data_frame %>%
  left_join(result, by = "SampleYear")

result_data_frame$species_pool_scaled <- scale(result_data_frame$species_pool)
result_data_frame$log_species_pool <- log(result_data_frame$species_pool)
result_data_frame$log_species_pool_scaled <- scale(result_data_frame$log_species_pool)

# interspecific biomass variability
cv_species_biomass_year <- data0731 %>%
  filter(
    !is.na(SampleYear),
    !is.na(Checked_Name),
    !is.na(WeightDry)
  ) %>%
  
  # Step 1: calculate total dry biomass of each species in each year
  group_by(SampleYear, Checked_Name) %>%
  summarise(
    Species_WeightDry = sum(WeightDry, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Step 2: calculate CV among species within each year
  group_by(SampleYear) %>%
  summarise(
    N_species = n(),
    Mean_species_biomass = mean(Species_WeightDry, na.rm = TRUE),
    SD_species_biomass = sd(Species_WeightDry, na.rm = TRUE),
    CV_species_biomass = SD_species_biomass / Mean_species_biomass,
    .groups = "drop"
  )

result_data_frame$CV_species_biomass <- cv_species_biomass_year$CV_species_biomass

# biomass dominance by the 3 most abundant species
top3_species_year <- data0731 %>%
  filter(
    !is.na(Checked_Name),
    !is.na(WeightDry)
  ) %>%
  
  # Calculate annual biomass of each species
  group_by(SampleYear, Checked_Name) %>%
  summarise(
    Species_WeightDry = sum(WeightDry, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Calculate total community biomass for each year
  group_by(SampleYear) %>%
  mutate(
    Total_WeightDry = sum(Species_WeightDry),
    Biomass_proportion = Species_WeightDry / Total_WeightDry
  ) %>%
  
  # Select the top 3 species in biomass for each year
  slice_max(
    order_by = Species_WeightDry,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  # Rank species within each year
  arrange(SampleYear, desc(Species_WeightDry)) %>%
  mutate(
    Rank = row_number()
  ) %>%
  
  group_by(SampleYear) %>%
  summarise(
    Biomass_proportion = sum(Biomass_proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

result_data_frame$top3_species_Biomass_proportion <- top3_species_year$Biomass_proportion

#######################################################################################
# 5.NiD & FiD
library(dplyr)
library(tidyr)
library(scales)

data_for_lmer <- data0731 %>%
  group_by(SampleYear, Plot) %>%
  summarise(TotalWeightDry = sum(WeightDry, na.rm = TRUE))
data_for_lmer <- data_for_lmer %>%
  left_join(result_data_frame, by = "SampleYear")

result_list <- list()

# each year and quadrat
for(year in colnames(spe_comp_matrix)){
  
  # Data for the current year
  df_year <- data0731 %>% filter(SampleYear == year)
  
  for(plot in unique(df_year$Plot)){
    
    # Data for the current quadrat
    df_plot <- df_year %>% filter(Plot == plot)
    
    # species list of the quadrat
    species_in_plot <- df_plot$AccSpeciesName
    
    # Find the corresponding year rows and columns of these species in spe_comp_matrix
    species_in_matrix <- intersect(species_in_plot, rownames(spe_comp_matrix))
    
    if(length(species_in_matrix) > 0){
     
      comp_values <- spe_comp_matrix[species_in_matrix, as.character(year), drop = FALSE]
      
      mean_val <- mean(comp_values, na.rm = TRUE)
      sd_val <- abs(sd(comp_values, na.rm = TRUE)/mean_val)
    } else {
      mean_val <- NA
      sd_val <- NA
    }
    
    result_list[[length(result_list) + 1]] <- data.frame(
      SampleYear = year,
      Plot = plot,
      Comp_Mean = mean_val,
      Comp_SD = sd_val
    )
  }
}

result_df <- do.call(rbind, result_list)
colnames(result_df)[3:4] <- c("NiD_plot","FiD_plot")
data_for_lmer <- data_for_lmer %>%
  left_join(result_df, by = c("SampleYear", "Plot"))

result1 <- data0731 %>%
  group_by(SampleYear, Plot) %>%
  summarise(real_div = n(), .groups = 'drop')
result1$log_real_div <- log(result1$real_div)
data_for_lmer <- data_for_lmer %>%
  left_join(result1, by = c("SampleYear", "Plot"))
data_for_lmer$log_real_div_scaled <- scale(data_for_lmer$log_real_div)

result2 <- result1 %>%
  group_by(SampleYear) %>%
  summarise(real_div_mean = mean(real_div, na.rm = TRUE))
result2$log_real_div_mean <- log(result2$real_div_mean)
data_for_lmer <- data_for_lmer %>%
  left_join(result2, by = "SampleYear")
data_for_lmer$log_real_div_mean_scaled <- scale(data_for_lmer$log_real_div_mean)



# removed sample quadrats with the outliers of NiD and FiD
data_for_lmer$NiD_plot_scaled <- scale(data_for_lmer$NiD_plot)
data_for_lmer$FiD_plot_scaled <- scale(data_for_lmer$FiD_plot)
data_for_lmer1 <- data_for_lmer[(data_for_lmer$FiD_plot_scaled>-3)&(data_for_lmer$FiD_plot_scaled<3),]
data_for_lmer1$NiD_plot_scaled <- scale(data_for_lmer1$NiD_plot)
data_for_lmer1$FiD_plot_scaled <- scale(data_for_lmer1$FiD_plot)
data_for_lmer1 <- data_for_lmer1[(data_for_lmer1$NiD_plot_scaled>-3)&(data_for_lmer1$NiD_plot_scaled<3)&(data_for_lmer1$FiD_plot_scaled>-3)&(data_for_lmer1$FiD_plot_scaled<3),]
data_for_lmer1$NiD_plot_scaled <- scale(data_for_lmer1$NiD_plot_scaled)
data_for_lmer1$FiD_plot_scaled <- scale(data_for_lmer1$FiD_plot_scaled)
data_for_lmer1 <- data_for_lmer1[(data_for_lmer1$NiD_plot_scaled>-3)&(data_for_lmer1$NiD_plot_scaled<3)&(data_for_lmer1$FiD_plot_scaled>-3)&(data_for_lmer1$FiD_plot_scaled<3),]
data_for_lmer1$NiD_plot_scaled <- scale(data_for_lmer1$NiD_plot_scaled)
data_for_lmer1$FiD_plot_scaled <- scale(data_for_lmer1$FiD_plot_scaled)
data_for_lmer1 <- data_for_lmer1[(data_for_lmer1$NiD_plot_scaled>-3)&(data_for_lmer1$NiD_plot_scaled<3)&(data_for_lmer1$FiD_plot_scaled>-3)&(data_for_lmer1$FiD_plot_scaled<3),]
data_for_lmer1$NiD_plot_scaled <- scale(data_for_lmer1$NiD_plot_scaled)
data_for_lmer1$FiD_plot_scaled <- scale(data_for_lmer1$FiD_plot_scaled)
# 741 quadrats left out of 806
plot(data_for_lmer1$NiD_plot_scaled)
plot(data_for_lmer1$FiD_plot_scaled)


result <- data_for_lmer1 %>%
  group_by(SampleYear) %>%
  summarise(NiD = mean(NiD_plot_scaled, na.rm = TRUE),
            FiD = mean(FiD_plot_scaled, na.rm = TRUE),
            dry_weight = mean(TotalWeightDry, na.rm = TRUE),
            dry_weight_var = var(TotalWeightDry, na.rm = TRUE),
            n = n())
result_data_frame <- result_data_frame %>%
  left_join(result, by = "SampleYear")
result_data_frame$dry_weight_var <- result_data_frame$dry_weight_var/result_data_frame$n
result_data_frame$NiD_scaled <- scale(result_data_frame$NiD)
result_data_frame$FiD_scaled <- scale(result_data_frame$FiD)
result_data_frame1 <- result_data_frame
#######################################################################################
# 6.temperate trend of GSP, GST, NiD, FiD, biodiversity and biomass

result_data_frame1$year <- as.numeric(result_data_frame1$SampleYear)
# GSP
summary(lm(GSP~year, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = GSP, x = year)) +
  geom_point(size = 2) +
  labs(y = "GSP(mm)", x = "Year") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "black", fill = "#CCCCCC",linetype="blank") +
  annotate("text", x = min(result_data_frame1$year), y = max(result_data_frame1$GSP), 
           label = "R²<0.001, p=0.97", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )
ggsave("GSP_time.png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)

# GST
summary(lm(GST~year, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = GST, x = year)) +
  geom_point(size = 2) +
  labs(y = "GST(℃)", x = "Year") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "black", fill = "#CCCCCC") +
  annotate("text", x = min(result_data_frame1$year), y = max(result_data_frame1$GST), 
           label = "R²=0.31, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )
ggsave("GST_time.png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)

# NiD
summary(lm(NiD_scaled~year, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = NiD_scaled, x = year)) +
  geom_point(size = 2) +
  labs(y = "Niche Difference(scaled)", x = "Year") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "black", fill = "#CCCCCC") +
  annotate("text", x = min(result_data_frame1$year), y = max(result_data_frame1$NiD_scaled), 
           label = "R²=0.30, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )
ggsave("NiD_time.png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)


# FiD
summary(lm(FiD_scaled~year, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = FiD_scaled, x = year)) +
  geom_point(size = 2) +
  labs(y = "Fitness Difference(scaled)", x = "Year") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "black", fill = "#CCCCCC") +
  annotate("text", x = min(result_data_frame1$year), y = max(result_data_frame1$FiD_scaled), 
           label = "R²=0.26, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )
ggsave("FiD_time.png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)

# species pool size
summary(lm(species_pool~year, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = species_pool, x = year)) +
  geom_point(size = 2) +
  labs(y = "Species Pool Size", x = "Year") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "black", fill = "#CCCCCC") +
  annotate("text", x = min(result_data_frame1$year), y = max(result_data_frame1$species_pool), 
           label = "R²=0.25, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )
ggsave("Spe_pool_time.png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)

# biomass
summary(lm(dry_weight~year, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = dry_weight, x = year)) +
  geom_point(size = 2) +
  labs(y = "Quadrat Dry Biomass(g/m²)", x = "Year") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "black", fill = "#CCCCCC") +
  annotate("text", x = min(result_data_frame1$year), y = max(result_data_frame1$dry_weight), 
           label = "R²=0.17, p=0.007", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )
ggsave("Eco_Fun_time.png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)

#######################################################################################
# 7.Possible influencing factors of NiD and FiD
# GSP and GST non-significant
summary(lm(NiD_scaled~GSP_scaled+GST_scaled,data = result_data_frame1))
summary(lm(FiD_scaled~GSP_scaled+GST_scaled,data = result_data_frame1))

# FiD with interspecific biomass variability non-significant
summary(lm(FiD_scaled~CV_species_biomass, data = result_data_frame1))

# FiD with biomass dominance by the 3 most abundant species non-significant
summary(lm(FiD_scaled~top3_species_Biomass_proportion, data = result_data_frame1))


#######################################################################################
# 8.Drivers of Biodiversity–Ecosystem Functioning Relationships

# linear mixed-effects models
library(nlme)
library(performance)
library(dplyr)

plot(data_for_lmer1$TotalWeightDry)
plot(data_for_lmer1$log_species_pool_scaled)
plot(data_for_lmer1$NiD_plot_scaled)
plot(data_for_lmer1$FiD_plot_scaled)
plot(data_for_lmer1$GSP_scaled)
plot(data_for_lmer1$GST_scaled)



# main model
model_lme_bef <- lme(
  TotalWeightDry ~ log_species_pool_scaled + GSP_scaled + GST_scaled + NiD_plot_scaled + FiD_plot_scaled +
    GSP_scaled:NiD_plot_scaled + GST_scaled:NiD_plot_scaled + 
    log_species_pool_scaled:GSP_scaled + log_species_pool_scaled:GST_scaled + log_species_pool_scaled:NiD_plot_scaled + log_species_pool_scaled:FiD_plot_scaled + 
    log_species_pool_scaled:GSP_scaled:NiD_plot_scaled+ 
    log_species_pool_scaled:GST_scaled:NiD_plot_scaled,
  random = ~1 | SampleYear,
  correlation = corCompSymm(form = ~ 1 | SampleYear),
  weights = varIdent(form = ~1 | SampleYear),
  data = data_for_lmer1,
  control = lmeControl(maxIter = 1000, msMaxIter = 1000, niterEM = 50)
)
summary(model_lme_bef)
r2(model_lme_bef)

# Copy the table to the clipboard
sm <- summary(model_lme_bef)
fixef <- as.data.frame(sm$tTable)
colnames(fixef) <- c("Estimate", "SE", "DF", "t", "p")
fixef$Term <- rownames(fixef)
fixef$Estimate <- round(fixef$Estimate, 2)
fixef$SE       <- round(fixef$SE, 2)
fixef$t        <- round(fixef$t, 2)
fixef$p        <- round(fixef$p, 3)
fixef$Sig <- cut(
  fixef$p,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)
fixef_out <- fixef[, c("Term", "Estimate", "SE", "DF", "t", "p", "Sig")]
write.table(
  fixef_out,
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)


# Model hypothesis testing
# no heteroscedasticity
plot(model_lme_bef)
# No significant temporal autocorrelation
data_for_lmer1$res <- resid(model_lme_bef, type = "normalized")
res_year <- data_for_lmer1 %>%
  group_by(year) %>%
  summarise(mean_res = mean(res))
acf(res_year$mean_res)
# No exterme residual
which(abs(resid(model_lme_bef, type = "normalized")) > 3)
# No problematic multicollinearity
collinearity_result <- check_collinearity(model_lme_bef)
collinearity_table <- as.data.frame(collinearity_result)
collinearity_table <- collinearity_table %>%
  dplyr::mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )
write.table(
  collinearity_table,
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

library(ggplot2)
library(nlme)

# plot Model hypothesis testing
# Extract fitted values and Pearson residuals
diag_data <- data.frame(
  fitted = fitted(model_lme_bef),
  residual = residuals(model_lme_bef, type = "pearson")
)

# Plot residuals vs fitted values
p <- ggplot(diag_data, aes(x = fitted, y = residual)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  labs(
    x = "Fitted values",
    y = "Pearson residuals"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(
      fill = NA,
      linewidth = 0.7
    )
  )
ggsave(
  filename = "Fig_S9_heteroscedasticity_checks.png",
  plot = p,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

# Draw graphs of normality and temporal autocorrelation
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)

data_for_lmer1$res <- resid(model_lme_bef, type = "normalized")

# data for normality
plot_list_data <- list(
  "Dry Biomass"         = data_for_lmer1$TotalWeightDry,
  "species pool size(scaled)"= data_for_lmer1$log_species_pool_scaled,
  "NiD(scaled)"         = data_for_lmer1$NiD_plot_scaled,
  "FiD(scaled)"         = data_for_lmer1$FiD_plot_scaled,
  "GSP(scaled)"             = data_for_lmer1$GSP_scaled,
  "GST(scaled)"            = data_for_lmer1$GST_scaled,
  "Normalized residuals"   = data_for_lmer1$res
)

panel_letters <- letters[1:7]

theme_nee <- theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 11, colour = "black"),
    axis.text  = element_text(size = 10, colour = "black"),
    plot.title = element_text(size = 11, face = "plain", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(8, 8, 8, 8)
  )

make_hist_plot <- function(x, var_name, letter_label) {
  
  df <- data.frame(value = x) %>%
    filter(!is.na(value))
  
  ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 10,
                   fill = "grey80",
                   colour = "black",
                   linewidth = 0.4) +
    geom_density(linewidth = 0.8, colour = "black", adjust = 1) +
    labs(
      title = var_name,
      x = NULL,
      y = "Density"
    ) +
    annotate(
      "text",
      x = -Inf, y = Inf,
      label = letter_label,
      hjust = -0.25, vjust = 1.4,
      size = 5,
      fontface = "bold"
    ) +
    theme_nee
}

hist_plots <- mapply(
  FUN = make_hist_plot,
  x = plot_list_data,
  var_name = names(plot_list_data),
  letter_label = panel_letters,
  SIMPLIFY = FALSE
)

fig_normality <- wrap_plots(hist_plots, ncol = 3) +
  plot_annotation(
    title = "Normality checks for response, predictors, and model residuals",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
    )
  )
ggsave(
  filename = "Fig_S8_normality_checks.png",
  plot = fig_normality,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)


# graphs of temporal autocorrelation
# Calculate the average residuals aggregated by year and perform the ACF
res_year <- data_for_lmer1 %>%
  group_by(year) %>%
  summarise(mean_res = mean(res, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)
acf_obj <- acf(res_year$mean_res, plot = FALSE)

acf_df <- data.frame(
  lag = as.numeric(acf_obj$lag),
  acf = as.numeric(acf_obj$acf)
)

n_obs <- nrow(res_year)
ci <- 1.96 / sqrt(n_obs)

fig_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-ci, ci), linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  geom_segment(aes(xend = lag, y = 0, yend = acf), linewidth = 0.7, colour = "black") +
  geom_point(size = 2) +
  scale_x_continuous(breaks = acf_df$lag) +
  labs(
    title = "Temporal autocorrelation of annual mean normalized residuals",
    x = "Lag",
    y = "ACF"
  ) +
  theme_nee

ggsave(
  filename = "Fig_S7_temporal_acf.png",
  plot = fig_acf,
  width = 6.5,
  height = 4.8,
  units = "in",
  dpi = 300
)

# fully factorial linear mixed-effects model (SI Appendix)
model_lme_bef1 <- lme(
  TotalWeightDry ~ log_species_pool_scaled + GSP_scaled + GST_scaled + NiD_plot_scaled + FiD_plot_scaled +
    GSP_scaled:NiD_plot_scaled + GSP_scaled:FiD_plot_scaled + GST_scaled:NiD_plot_scaled + GST_scaled:FiD_plot_scaled + 
    log_species_pool_scaled:GSP_scaled + log_species_pool_scaled:GST_scaled + log_species_pool_scaled:NiD_plot_scaled + log_species_pool_scaled:FiD_plot_scaled + 
    log_species_pool_scaled:GSP_scaled:NiD_plot_scaled + log_species_pool_scaled:GSP_scaled:FiD_plot_scaled + 
    log_species_pool_scaled:GST_scaled:NiD_plot_scaled + log_species_pool_scaled:GST_scaled:FiD_plot_scaled,
  random = ~1 | SampleYear,
  correlation = corCompSymm(form = ~ 1 | SampleYear),
  weights = varIdent(form = ~1 | SampleYear),
  data = data_for_lmer1,
  control = lmeControl(maxIter = 1000, msMaxIter = 1000, niterEM = 50)
)
summary(model_lme_bef1)

# Model hypothesis testing
r2(model_lme_bef1)
# No problematic multicollinearity
check_collinearity(model_lme_bef1)
# no heteroscedasticity
plot(model_lme_bef1)
# No significant temporal autocorrelation
data_for_lmer1$res <- resid(model_lme_bef1, type = "normalized")
res_year <- data_for_lmer1 %>%
  group_by(year) %>%
  summarise(mean_res = mean(res))
acf(res_year$mean_res)
# Copy the table to the clipboard
sm <- summary(model_lme_bef1)
fixef <- as.data.frame(sm$tTable)
colnames(fixef) <- c("Estimate", "SE", "DF", "t", "p")
fixef$Term <- rownames(fixef)
fixef$Estimate <- round(fixef$Estimate, 2)
fixef$SE       <- round(fixef$SE, 2)
fixef$t        <- round(fixef$t, 2)
fixef$p        <- round(fixef$p, 3)
fixef$Sig <- cut(
  fixef$p,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)
fixef_out <- fixef[, c("Term", "Estimate", "SE", "DF", "t", "p", "Sig")]
write.table(
  fixef_out,
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# plot the figures to show the liner mixed effect model
library(nlme)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggnewscale)

# model without triple interaction(Fig 4 a-d)
model_lme_bef_simple <- lme(
  TotalWeightDry ~ log_species_pool_scaled + GSP_scaled + GST_scaled +
    NiD_plot_scaled + FiD_plot_scaled +
    log_species_pool_scaled:GSP_scaled +
    log_species_pool_scaled:GST_scaled +
    log_species_pool_scaled:NiD_plot_scaled +
    log_species_pool_scaled:FiD_plot_scaled,
  random = ~1 | SampleYear,
  correlation = corCompSymm(form = ~ 1 | SampleYear),
  weights = varIdent(form = ~1 | SampleYear),
  data = data_for_lmer1,
  control = lmeControl(maxIter = 1000, msMaxIter = 1000, niterEM = 50)
)

summary(model_lme_bef_simple)

x_var <- "log_species_pool_scaled"
y_var <- "TotalWeightDry"

mod_vars <- c(
  "GSP_scaled",
  "GST_scaled",
  "NiD_plot_scaled",
  "FiD_plot_scaled"
)

mod_labels <- c(
  GSP_scaled = "GSP",
  GST_scaled = "GST",
  NiD_plot_scaled = "NiD",
  FiD_plot_scaled = "FiD"
)

x_seq <- seq(
  min(data_for_lmer1[[x_var]], na.rm = TRUE),
  max(data_for_lmer1[[x_var]], na.rm = TRUE),
  length.out = 100
)

species_pool_key <- data_for_lmer1 %>%
  filter(
    !is.na(log_species_pool_scaled),
    !is.na(species_pool)
  ) %>%
  group_by(log_species_pool_scaled) %>%
  summarise(
    species_pool = mean(species_pool),
    .groups = "drop"
  ) %>%
  arrange(log_species_pool_scaled)

scaled_to_species_pool <- approxfun(
  x = species_pool_key$log_species_pool_scaled,
  y = species_pool_key$species_pool,
  rule = 2
)

make_pred_data <- function(mod) {
  
  newdat <- expand.grid(
    log_species_pool_scaled = x_seq,
    mod_level = c("-SD", "+SD")
  )
  
  # Other explanatory variables are fixed at the mean
  newdat$GSP_scaled <- 0
  newdat$GST_scaled <- 0
  newdat$NiD_plot_scaled <- 0
  newdat$FiD_plot_scaled <- 0
  
  newdat[[mod]] <- ifelse(newdat$mod_level == "-SD", -1, 1)
  
  newdat$SampleYear <- data_for_lmer1$SampleYear[1]
  
  newdat$Moderator <- mod_labels[[mod]]
  
  return(newdat)
}

pred_dat <- map_dfr(mod_vars, make_pred_data)

X <- model.matrix(
  delete.response(terms(model_lme_bef_simple)),
  pred_dat
)

beta <- fixef(model_lme_bef_simple)
V <- vcov(model_lme_bef_simple)

X <- X[, names(beta)]

pred_dat$fit <- as.numeric(X %*% beta)
pred_dat$se <- sqrt(diag(X %*% V %*% t(X)))
pred_dat$lwr <- pred_dat$fit - 1.96 * pred_dat$se
pred_dat$upr <- pred_dat$fit + 1.96 * pred_dat$se


plot_dat <- map_dfr(mod_vars, function(mod) {
  data_for_lmer1 %>%
    mutate(
      Moderator = mod_labels[[mod]],
      mod_level = ifelse(.data[[mod]] < 0, "-SD", "+SD")
    )
})

sig_code <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*",
                       ifelse(p < 0.1, ".", "ns"))))
}

tt <- summary(model_lme_bef_simple)$tTable

get_int_name <- function(mod) {
  int1 <- paste0(x_var, ":", mod)
  int2 <- paste0(mod, ":", x_var)
  
  if (int1 %in% rownames(tt)) {
    return(int1)
  } else {
    return(int2)
  }
}

ann_dat <- map_dfr(mod_vars, function(mod) {
  
  p_x <- tt[x_var, "p-value"]
  p_m <- tt[mod, "p-value"]
  p_int <- tt[get_int_name(mod), "p-value"]
  
  tibble(
    Moderator = mod_labels[[mod]],
    x = min(data_for_lmer1[[x_var]], na.rm = TRUE) +
      0.08 * diff(range(data_for_lmer1[[x_var]], na.rm = TRUE)),
    y = max(data_for_lmer1[[y_var]], na.rm = TRUE) -
      0.12 * diff(range(data_for_lmer1[[y_var]], na.rm = TRUE)),
    label = paste0(
      "SR", sig_code(p_x), "\n",
      mod_labels[[mod]], sig_code(p_m), "\n",
      "SR:", mod_labels[[mod]], sig_code(p_int)
    )
  )
})

plot_dat <- plot_dat %>%
  mutate(
    point_group = ifelse(mod_level == "-SD", "< Mean", "> Mean")
  )

pred_dat <- pred_dat %>%
  mutate(
    line_group = ifelse(mod_level == "-SD", "Mean - SD", "Mean + SD")
  )

p_all <- ggplot() +

  geom_point(
    data = plot_dat,
    aes(
      x = log_species_pool_scaled,
      y = TotalWeightDry,
      color = point_group,
      shape = point_group
    ),
    size = 2.3,
    alpha = 0.75,
    stroke = 0.9
  ) +
  scale_color_manual(
    name = "Points\n(moderator relative to mean)",
    values = c("< Mean" = "#4C78A8", "> Mean" = "#F58518")
  ) +
  scale_shape_manual(
    name = "Points\n(moderator relative to mean)",
    values = c("< Mean" = 1, "> Mean" = 2)
  ) +
  
  ggnewscale::new_scale_color() +
  
  geom_line(
    data = pred_dat,
    aes(
      x = log_species_pool_scaled,
      y = fit,
      color = line_group,
      linetype = line_group
    ),
    linewidth = 1.2
  ) +
  scale_color_manual(
    name = "Lines\n(moderator fixed at mean ± SD)",
    values = c("Mean - SD" = "#4C78A8", "Mean + SD" = "#F58518")
  ) +
  scale_linetype_manual(
    name = "Lines\n(moderator fixed at mean ± SD)",
    values = c("Mean - SD" = "dashed", "Mean + SD" = "solid")
  ) +
  
  geom_text(
    data = ann_dat,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1,
    vjust = 1.1,
    fontface = "bold",
    size = 5,
    inherit.aes = FALSE
  ) +
  
  facet_wrap(~ Moderator, ncol = 2) +
  guides(
    shape = guide_legend(order = 1),
    `colour_ggnewscale_1` = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    linetype = guide_legend(order = 2)
  ) +
  scale_x_continuous(
    name = expression(log[2]~"(species pool, scaled)"),
    sec.axis = dup_axis(
      name = "Actual species pool size",
      labels = function(x) round(scaled_to_species_pool(x))
    )
  ) +
  labs(
    y = "TotalWeightDry",
    title = "Effects of species pool under different moderator levels"
  ) +
  theme_classic(base_size = 15) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

for (m in unique(pred_dat$Moderator)) {
  
  p_single <- ggplot() +

    geom_point(
      data = filter(plot_dat, Moderator == m),
      aes(
        x = log_species_pool_scaled,
        y = TotalWeightDry,
        color = point_group,
        shape = point_group
      ),
      size = 2.5,
      alpha = 0.75,
      stroke = 0.9
    ) +
    scale_color_manual(
      name = "Points\n(moderator relative to mean)",
      values = c("< Mean" = "#4C78A8", "> Mean" = "#F58518")
    ) +
    scale_shape_manual(
      name = "Points\n(moderator relative to mean)",
      values = c("< Mean" = 1, "> Mean" = 2)
    ) +
    
    ggnewscale::new_scale_color() +
    
    geom_line(
      data = filter(pred_dat, Moderator == m),
      aes(
        x = log_species_pool_scaled,
        y = fit,
        color = line_group,
        linetype = line_group
      ),
      linewidth = 1.3
    ) +
    scale_color_manual(
      name = "Lines\n(moderator fixed at mean ± SD)",
      values = c("Mean - SD" = "#4C78A8", "Mean + SD" = "#F58518")
    ) +
    scale_linetype_manual(
      name = "Lines\n(moderator fixed at mean ± SD)",
      values = c("Mean - SD" = "solid", "Mean + SD" = "solid")
    ) +
    
    geom_text(
      data = filter(ann_dat, Moderator == m),
      aes(x = -Inf, y = Inf, label = label),
      hjust = -0.1,
      vjust = 1.1,
      fontface = "bold",
      size = 5.5,
      inherit.aes = FALSE
    ) +
    
    guides(
      shape = guide_legend(order = 1),
      `colour_ggnewscale_1` = guide_legend(order = 1),
      colour = guide_legend(order = 2),
      linetype = guide_legend(order = 2)
    ) +
    scale_x_continuous(
      name = "ln(species pool size), scaled",
      sec.axis = dup_axis(
        name = "Actual species pool size",
        labels = function(x) round(scaled_to_species_pool(x))
      )
    ) +
    labs(
      y = expression("Quadrat Dry Biomass (g "*m^{-2}*")")
    ) +
    theme_classic(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none",
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    )
  
  print(p_single)
  
  ggsave(
    filename = paste0("interaction_", m, ".png"),
    plot = p_single,
    width = 5.5,
    height = 5,
    dpi = 300
  )
}


p_single <- ggplot() +

  geom_point(
    data = filter(plot_dat, Moderator == m),
    aes(
      x = log_species_pool_scaled,
      y = TotalWeightDry,
      color = point_group,
      shape = point_group
    ),
    size = 2.5,
    alpha = 0.75,
    stroke = 0.9
  ) +
  scale_color_manual(
    name = "Points\n(moderator relative to mean)",
    values = c("< Mean" = "#4C78A8", "> Mean" = "#F58518")
  ) +
  scale_shape_manual(
    name = "Points\n(moderator relative to mean)",
    values = c("< Mean" = 1, "> Mean" = 2)
  ) +
  
  ggnewscale::new_scale_color() +
  
  geom_line(
    data = filter(pred_dat, Moderator == m),
    aes(
      x = log_species_pool_scaled,
      y = fit,
      color = line_group,
      linetype = line_group
    ),
    linewidth = 1.3
  ) +
  scale_color_manual(
    name = "Lines\n(moderator fixed at mean ± SD)",
    values = c("Mean - SD" = "#4C78A8", "Mean + SD" = "#F58518")
  ) +
  scale_linetype_manual(
    name = "Lines\n(moderator fixed at mean ± SD)",
    values = c("Mean - SD" = "solid", "Mean + SD" = "solid")
  ) +
  
  geom_text(
    data = filter(ann_dat, Moderator == m),
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1,
    vjust = 1.1,
    fontface = "bold",
    size = 5.5,
    inherit.aes = FALSE
  ) +
  
  guides(
    shape = guide_legend(order = 1),
    `colour_ggnewscale_1` = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    linetype = guide_legend(order = 2)
  ) +
  labs(
    x = "ln(species pool size), scaled",
    y = "Quadrat Dry Biomass (g/m²)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = paste0("interaction_1_", m, ".png"),
  plot = p_single,
  width = 12,
  height = 5,
  dpi = 300
)


# model with triple interaction
library(ggplot2)
library(dplyr)
library(grid)   # for unit()

summary(data_for_lmer1$NiD_plot_scaled)
summary(data_for_lmer1$FiD_plot_scaled)
summary(data_for_lmer1$GSP_scaled)
summary(data_for_lmer1$GST_scaled)

summary(model_lme_bef)
line_colors <- c(
  "Low (-1)"    = "#4C78A8",
  "High (1)"    = "#F58518"
)

line_types <- c(
  "Low (-1)"    = "solid",
  "High (1)"    = "solid"
)

theme_my <- theme_classic(base_size = 15) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(color = "black", size = 13),
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )

# GSP × NiD
x1_seq <- seq(-1.8, 2.6, length.out = 100)
x2_levels <- c(1, -1)
plot_data <- expand.grid(x1 = x1_seq, x2 = x2_levels)

GSP_BEF <- function(NiD, GSP) {
  18.65611 + 16.97618 * NiD - 16.89862 *GSP  + 14.87988*NiD*GSP 
}

plot_data$y_pred <- GSP_BEF(plot_data$x2, plot_data$x1)
plot_data$x2_group <- factor(
  plot_data$x2,
  levels = c(-1, 1),
  labels = c("Low (-1)", "High (1)")
)

p1 <- ggplot(plot_data, aes(x = x1, y = y_pred,
                            color = x2_group, linetype = x2_group)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = line_colors, name = "NiD (scaled)") +
  scale_linetype_manual(values = line_types, name = "NiD (scaled)") +
  labs(
    x = "GSP(scaled)",
    y = "predicted slope of BEF relationships"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "SR:GSP:NiD**",
    hjust = -0.1,
    vjust = 1.1,
    fontface = "bold",
    size = 5.5
  ) +
  theme_my


ggsave(
  "interaction_GSP_BEF.png",
  plot = p1,
  width = 5.5,
  height = 5,
  dpi = 300
)

# GST × NiD
x1_seq <- seq(-2.3, 2, length.out = 100)
x2_levels <- c(1, -1)
plot_data <- expand.grid(x1 = x1_seq, x2 = x2_levels)

GST_BEF <- function(NiD, GST) {
  18.65611 + 16.97618 * NiD -17.85643*GST + 8.33039*NiD*GST
}

plot_data$y_pred <- GST_BEF(plot_data$x2, plot_data$x1)
plot_data$x2_group <- factor(
  plot_data$x2,
  levels = c(-1, 1),
  labels = c("Low (-1)", "High (1)")
)

p2 <- ggplot(plot_data, aes(x = x1, y = y_pred,
                            color = x2_group, linetype = x2_group)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = line_colors, name = "NiD (scaled)") +
  scale_linetype_manual(values = line_types, name = "NiD (scaled)") +
  labs(
    x = "GST(scaled)",
    y = "predicted slope of BEF relationships"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "SR:GST:NiD*",
    hjust = -0.1,
    vjust = 1.1,
    fontface = "bold",
    size = 5.5
  ) +
  theme_my


ggsave(
  "interaction_GST_BEF.png",
  plot = p2,
  width = 5.5,
  height = 5,
  dpi = 300
)

#######################################################################################
# 10.BEF of realized species diversity
# realized species diversity
model_lme_bef_realdiv <- lme(
  TotalWeightDry ~ log_real_div_scaled+ NiD_plot_scaled + FiD_plot_scaled  + GSP_scaled + GST_scaled +
    GSP_scaled:NiD_plot_scaled + GST_scaled:NiD_plot_scaled + 
    log_real_div_scaled:NiD_plot_scaled + log_real_div_scaled:FiD_plot_scaled +log_real_div_scaled:GSP_scaled + log_real_div_scaled:GST_scaled +  
    log_real_div_scaled:GSP_scaled:NiD_plot_scaled  + 
    log_real_div_scaled:GST_scaled:NiD_plot_scaled ,
  random = ~1 | SampleYear,
  correlation = corCompSymm(form = ~ 1 | SampleYear),
  weights = varIdent(form = ~1 | SampleYear),
  data = data_for_lmer1,
  control = lmeControl(maxIter = 1000, msMaxIter = 1000, niterEM = 50)
)
summary(model_lme_bef_realdiv)
r2(model_lme_bef_realdiv)

sm <- summary(model_lme_bef_realdiv)
fixef <- as.data.frame(sm$tTable)
colnames(fixef) <- c("Estimate", "SE", "DF", "t", "p")
fixef$Term <- rownames(fixef)
fixef$Estimate <- round(fixef$Estimate, 2)
fixef$SE       <- round(fixef$SE, 2)
fixef$t        <- round(fixef$t, 2)
fixef$p        <- round(fixef$p, 3)
fixef$Sig <- cut(
  fixef$p,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "ns")
)
fixef_out <- fixef[, c("Term", "Estimate", "SE", "DF", "t", "p", "Sig")]
write.table(
  fixef_out,
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# annual mean of realized species diversity
model_lme_bef_realdiv_mean <- lme(
  TotalWeightDry ~ log_real_div_mean_scaled+ NiD_plot_scaled + FiD_plot_scaled  + GSP_scaled + GST_scaled +
    GSP_scaled:NiD_plot_scaled + GST_scaled:NiD_plot_scaled + 
    log_real_div_mean_scaled:NiD_plot_scaled + log_real_div_mean_scaled:FiD_plot_scaled +log_real_div_mean_scaled:GSP_scaled + log_real_div_mean_scaled:GST_scaled +  
    log_real_div_mean_scaled:GSP_scaled:NiD_plot_scaled  + 
    log_real_div_mean_scaled:GST_scaled:NiD_plot_scaled ,
  random = ~1 | SampleYear,
  correlation = corCompSymm(form = ~ 1 | SampleYear),
  weights = varIdent(form = ~1 | SampleYear),
  data = data_for_lmer1,
  control = lmeControl(maxIter = 1000, msMaxIter = 1000, niterEM = 50)
)
summary(model_lme_bef_realdiv_mean)
r2(model_lme_bef_realdiv_mean)

sm <- summary(model_lme_bef_realdiv_mean)
fixef <- as.data.frame(sm$tTable)
colnames(fixef) <- c("Estimate", "SE", "DF", "t", "p")
fixef$Term <- rownames(fixef)
fixef$Estimate <- round(fixef$Estimate, 2)
fixef$SE       <- round(fixef$SE, 2)
fixef$t        <- round(fixef$t, 2)
fixef$p        <- round(fixef$p, 3)
fixef$Sig <- cut(
  fixef$p,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "ns")
)
fixef_out <- fixef[, c("Term", "Estimate", "SE", "DF", "t", "p", "Sig")]
write.table(
  fixef_out,
  file = "clipboard",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)



#######################################################################################
# 11.temperate trend of BEF predicted

library(ggplot2)
library(viridis) 
library(grid)
calculate_BEF_all <- function(NiD, FiD, GSP, GST) {18.65611 + 16.97618 * NiD - 11.35040  * FiD - 16.89862*GSP -17.85643*GST + 14.87988*NiD*GSP + 8.33039*NiD*GST+ 0 *FiD*GSP - 0*FiD*GST}
calculate_BEF_clim <- function(NiD, FiD, GSP, GST) {- 16.89862*GSP -17.85643*GST+ 14.87988*NiD*GSP + 8.33039*NiD*GST+ 0 *FiD*GSP - 0*FiD*GST}
calculate_BEF_comm <- function(NiD, FiD) { 16.97618 * NiD - 11.35040  * FiD}
# calculate_BEF_baseline <- 18.65611

result_data_frame1$bef_all_predicted <- calculate_BEF_all(
  result_data_frame1$NiD, 
  result_data_frame1$FiD, 
  result_data_frame1$GSP_scaled, 
  result_data_frame1$GST_scaled
)

result_data_frame1$bef_clim_predicted <- calculate_BEF_clim(
  result_data_frame1$NiD, 
  result_data_frame1$FiD, 
  result_data_frame1$GSP_scaled, 
  result_data_frame1$GST_scaled
)

result_data_frame1$bef_comm_predicted <- calculate_BEF_comm(
  result_data_frame1$NiD, 
  result_data_frame1$FiD
)


result_data_frame1$bef_baseline <- 18.65611
summary(lm(bef_all_predicted~year, data = result_data_frame1))
summary(lm(bef_clim_predicted~year, data = result_data_frame1))
summary(lm(bef_comm_predicted~year, data = result_data_frame1))

p <- ggplot(result_data_frame1, aes(x = year)) +
  
  geom_line(aes(y = bef_baseline, color = "BEF_baseline"), linewidth = 0.5) +
  geom_line(aes(y = bef_clim_predicted, color = "BEF_clim"), linewidth = 0.5) +
  geom_line(aes(y = bef_comm_predicted, color = "BEF_comm"), linewidth = 0.5) +
  geom_line(aes(y = bef_all_predicted, color = "BEF_all"), linewidth = 1.5) +
  
  scale_color_manual(
    name = "predicted slope of pool-based BEF relationship",
    values = c("BEF_all" = "black", 
               "BEF_comm" = "#E44C34",
               "BEF_clim" = "#4F6DB0", 
               "BEF_baseline" = "gray60"),
    labels = c("BEF_all" = "BEF slope", 
               "BEF_comm" = "species-coexistence contribution", 
               "BEF_clim" = "climatic contribution",
               "BEF_baseline" = "BEF baseline"),
    limits = c("BEF_all","BEF_comm", "BEF_clim",  "BEF_baseline")
  ) +
  
  labs(
    x = "Year",
    y = "Slope of BEF relationships"
  ) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )

ggsave("predicted_BEF_time_lme.png", 
       plot = p,
       width = 10,  
       height = 5, 
       dpi = 300)



calculate_BEF_NiD <- function(NiD) {16.97618 * NiD}
calculate_BEF_FiD <- function(FiD) { - 11.35040  * FiD}
result_data_frame1$bef_NiD_predicted <- calculate_BEF_NiD(
  result_data_frame1$NiD
)
result_data_frame1$bef_FiD_predicted <- calculate_BEF_FiD(
  result_data_frame1$FiD
)

calculate_BEF_GSP <- function(NiD, FiD, GSP) {- 16.89862*GSP+ 14.87988*NiD*GSP + 0 *FiD*GSP}
calculate_BEF_GST <- function(NiD, FiD, GST) {-17.85643*GST+ 8.33039*NiD*GST - 0*FiD*GST}
result_data_frame1$bef_GSP_predicted <- calculate_BEF_GSP(
  result_data_frame1$NiD, 
  result_data_frame1$FiD,
  result_data_frame1$GSP_scaled
)
result_data_frame1$bef_GST_predicted <- calculate_BEF_GST(
  result_data_frame1$NiD, 
  result_data_frame1$FiD,
  result_data_frame1$GST_scaled
)


p_comm <- ggplot(result_data_frame1, aes(x = year)) +
  geom_line(aes(y = bef_NiD_predicted, color = "NiD"), linewidth = 0.5) +
  geom_line(aes(y = bef_FiD_predicted, color = "FiD"), linewidth = 0.5) +
  geom_line(aes(y = bef_comm_predicted, color = "BEF_comm"), linewidth = 1.5) +
  scale_color_manual(
    name = "species-coexistence-associated contribution",
    values = c(
      "BEF_comm" = "#D94841",
      "NiD" = "#F2A7A0",
      "FiD" = "#8C2D1C"
    ),
    labels = c(
      "BEF_comm" = "BEF predicted by NiD & FiD",
      "NiD" = "NiD contribution",
      "FiD" = "FiD contribution"
    ),
    breaks = c("BEF_comm", "NiD", "FiD")
  )+
  labs(
    x = "Year",
    y = "Slope of BEF relationships"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )

ggsave(
  "predicted_BEF_comm_decomposition.png",
  plot = p_comm,
  width = 10,
  height = 5,
  dpi = 300
)


p_clim <- ggplot(result_data_frame1, aes(x = year)) +
  geom_line(aes(y = bef_GSP_predicted, color = "GSP"), linewidth = 0.5) +
  geom_line(aes(y = bef_GST_predicted, color = "GST"), linewidth = 0.5) +
  geom_line(aes(y = bef_clim_predicted, color = "BEF_clim"), linewidth = 1.5) +
  scale_color_manual(
    name = "climate-associated contribution",
    values = c(
      "BEF_clim" = "#4C78A8",
      "GSP" = "#9BBCE0",
      "GST" = "#1D3F66"
    ),
    labels = c(
      "BEF_clim" = "BEF predicted by GSP, GST and NiD",
      "GSP" = "GSP contribution",
      "GST" = "GST contribution"
    ),
    breaks = c("BEF_clim", "GSP", "GST")
  ) +
  labs(
    x = "Year",
    y = "Slope of BEF relationships"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 15),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )

ggsave(
  "predicted_BEF_clim_decomposition.png",
  plot = p_clim,
  width = 10,
  height = 5,
  dpi = 300
)

# Sum of squares decomposition
# all
# BEF predicted by clim
var(result_data_frame1$bef_clim_predicted)/var(result_data_frame1$bef_all_predicted)
# BEF influenced by NiD&FiD
var(result_data_frame1$bef_comm_predicted)/var(result_data_frame1$bef_all_predicted)
# cov
2*cov(result_data_frame1$bef_clim_predicted, result_data_frame1$bef_comm_predicted)/var(result_data_frame1$bef_all_predicted)

# NiD&FiD
# BEF predicted by NiD
var(result_data_frame1$bef_NiD_predicted)/var(result_data_frame1$bef_comm_predicted)
# BEF influenced by FiD
var(result_data_frame1$bef_FiD_predicted)/var(result_data_frame1$bef_comm_predicted)
# cov
2*cov(result_data_frame1$bef_NiD_predicted, result_data_frame1$bef_FiD_predicted)/var(result_data_frame1$bef_comm_predicted)

# clim
# BEF predicted by GSP
var(result_data_frame1$bef_GSP_predicted)/var(result_data_frame1$bef_clim_predicted)
# BEF influenced by GST
var(result_data_frame1$bef_GST_predicted)/var(result_data_frame1$bef_clim_predicted)
# cov
2*cov(result_data_frame1$bef_GSP_predicted, result_data_frame1$bef_GST_predicted)/var(result_data_frame1$bef_clim_predicted)




#######################################################################################
# 11.Others

# The sampling time for the maximum Dry Biomass period each year
library(ggplot2)
library(lubridate)

weightmax_day <- data.frame(Date = ymd(unique(data0731$SampleDate)),
                            SampleYear = as.numeric(unique(data0731$SampleYear)))
weightmax_day$DayOfYear <- yday(weightmax_day$Date)

p <- ggplot(weightmax_day, aes(x = SampleYear, y = DayOfYear)) +
  geom_point(size = 2) +
  labs(y = "Day of the year", x = "Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(limits = c(0, 366)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),  
    text = element_text(size = 15),  # Base text size
    axis.title = element_text(size = 25),  # Axis titles
    axis.text = element_text(size = 15),  # Axis labels
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 15),  # Legend items
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Tick length
  )
ggsave("maxdryweight_time.png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)


# Changes in quadrat Dry Biomass
library(dplyr)
library(ggplot2)
library(lubridate)

data$Date <- ymd(data$SampleDate)
data$DayOfYear <- yday(data$Date)

plot_sum <- data %>%
  group_by(SampleYear, SampleDate, Date, DayOfYear, Plot) %>%
  summarise(
    PlotWeight = sum(WeightDry, na.rm = TRUE),
    .groups = "drop"
  )

date_summary <- plot_sum %>%
  group_by(SampleYear, SampleDate, Date, DayOfYear) %>%
  summarise(
    MeanWeight = mean(PlotWeight),
    SE = sd(PlotWeight),
    .groups = "drop"
  )

year_list <- split(date_summary, date_summary$SampleYear)

plot_list1 <- lapply(year_list, function(df_year) {
  
  year0 <- unique(df_year$SampleYear)
  
  tick_dates <- ymd(paste0(
    year0,
    c("0501","0601","0701","0801","0901","1001")
  ))
  
  tick_days <- yday(tick_dates)
  
  tick_labels <- c("0501","0601","0701","0801","0901","1001")
  
  
  p <- ggplot(df_year,
              aes(x = DayOfYear, y = MeanWeight)) +
    
    geom_line(size = 1) +
    geom_point(size = 2) +
    
    geom_errorbar(
      aes(
        ymin = MeanWeight - SE,
        ymax = MeanWeight + SE
      ),
      width = 2,
      alpha = 0.7
    ) +
    
    scale_x_continuous(
      breaks = tick_days,
      labels = tick_labels,
      limits = c(121, 275)
    )+
    
    scale_y_continuous(limits = c(0, 350)) +
    annotate("text", x = 121, y = 340, 
             label = paste0("Year = ", year0), hjust = 0, vjust = 1, size = 9) +
    
    labs(
      x = "Sampling Date",
      y = "Quadrat Dry Biomass(g/m²)"
    ) +
    
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      aspect.ratio = 1,  
      panel.grid = element_blank(),  
      panel.background = element_rect(fill = "white"),  
      text = element_text(size = 15),  # Base text size
      axis.title = element_text(size = 20),  # Axis titles
      axis.text = element_text(size = 15),  # Axis labels
      legend.title = element_text(size = 15),  # Legend title
      legend.text = element_text(size = 15),  # Legend items
      axis.ticks = element_line(color = "black"),  # Axis ticks
      axis.ticks.length = unit(0.2, "cm")  # Tick length
    ) 
  
  return(p)
})
nrow_per_page <- 9
ncol_per_page <- 5
plots_per_page <- nrow_per_page * ncol_per_page

total_pages <- ceiling(length(plot_list1) / plots_per_page)


for (page in 1:total_pages) {
  start_index <- (page - 1) * plots_per_page + 1
  end_index <- min(page * plots_per_page, length(plot_list1))
  current_plots <- plot_list1[start_index:end_index]
  
  combined_plot <- grid.arrange(
    grobs = current_plots,
    nrow = nrow_per_page,
    ncol = ncol_per_page,
    padding = unit(1, "lines"))
  
  ggsave(
    filename = paste0("Fig_S2_dry_weight_change", page, ".png"),
    plot = combined_plot,
    width = 25,  
    height = 45, 
    dpi = 300,    
    limitsize = FALSE
  )
}
plot_list1 <- list()


# Export the data table used for the analysis
library(xlsx)
write.xlsx(x = as.data.frame(data_for_lmer), file =  "Data for Drivers of Biodiversity–Ecosystem Functioning Relationships in Natural Communities.xlsx", sheetName ="data for liner mixed effect model", row.names = F)
write.xlsx(x = as.data.frame(result_data_frame1), file =  "Data for Drivers of Biodiversity–Ecosystem Functioning Relationships in Natural Communities.xlsx", sheetName ="data for Fig. 5", row.names = F, append = TRUE)
write.xlsx(x = as.data.frame(date_summary), file =  "Data for Drivers of Biodiversity–Ecosystem Functioning Relationships in Natural Communities.xlsx", sheetName ="data for SI Appendix Figs. S1 and S2", row.names = F, append = TRUE)
