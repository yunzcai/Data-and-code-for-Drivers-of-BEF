#### Data analysis of Lotka-Volterra community
#######################################################################################
# 1. Generate simulated communities using the LV model

library(parallel)

# Define the discrete update function for the Lotka-Volterra model (with a disturbance term)
LV_model_discrete <- function(state, parameters, disturbance_sd = 0.1) {
  with(as.list(c(state, parameters)), {
    # Extract parameters
    n_species <- length(state)
    r <- parameters$r
    K <- parameters$K
    alpha <- parameters$alpha
    
    # Calculate the growth rate and competitive effect for each species
    dN_dt <- numeric(n_species)
    
    # Set populations below 0.0001 to 0 (extinction)
    state[state < 0.0001] <- 0
    
    for (i in 1:n_species) {
      if (state[i] > 0) {  # Calculate only if the species is still present
        competition_effect <- 0
        for (j in 1:n_species) {
          if (state[j] > 0) {  # Consider competition only from extant species
            competition_effect <- competition_effect + alpha[i, j] * state[j]
          }
        }
        # Add a normally distributed disturbance term
        disturbance <- rnorm(1, 0, disturbance_sd)
        dN_dt[i] <- r[i] * state[i] * (1 - competition_effect / K[i]) + disturbance * state[i]
      } else {
        dN_dt[i] <- 0
      }
    }
    
    return(dN_dt)
  })
}

# simulation function (randomly select 4/5 of the species for simulation)
run_simulation <- function(n_species, competition_strength, sim_id, parameters_list = NULL, disturbance_sd = 0.1) {
  r_values <- parameters_list$r_values
  K_values <- parameters_list$K_values
  alpha <- parameters_list$alpha_matrix
  
  # Randomly select 4/5 of the species
  n_selected <- round(n_species * 4/5)
  selected_species <- sample(1:n_species, n_selected, replace = FALSE)
  selected_species <- sort(selected_species)  # 排序以便后续处理
  
  # Extract parameters corresponding to the selected species
  r_values_selected <- r_values[selected_species]
  K_values_selected <- K_values[selected_species]
  alpha_selected <- alpha[selected_species, selected_species, drop = FALSE]
  
  # Initial conditions - assign initial values only to selected species; set all others to 0
  state_full <- rep(0, n_species)  # Full species vector, initialized to 0
  state_full[selected_species] <- 0.1  # Assign initial values only to selected species
  names(state_full) <- paste0("N", 1:n_species)
  
  # State vector of selected species (used for simulation)
  state_selected <- state_full[selected_species]
  
  # Parameter list (containing only selected species)
  parameters_selected <- list(
    r = r_values_selected, 
    K = K_values_selected, 
    alpha = alpha_selected,
    species_mapping = selected_species  # Store the species mapping
  )
  
  # Time series
  times <- seq(0, 1000, by = 1)
  n_steps <- length(times) - 1
  
  # Run the simulation using the discrete algorithm
  tryCatch({
    
    # Simulation history of selected species
    population_history_selected <- matrix(0, nrow = n_steps + 1, ncol = n_selected)
    population_history_selected[1, ] <- state_selected
    
    for (step in 1:n_steps) {
      # Calculate the rate of change at the current time step (selected species only)
      dN_dt_selected <- LV_model_discrete(state_selected, parameters_selected, disturbance_sd)
      
      # Use the discrete update: state[i+1] = state[i] + dN_dt[i]
      state_selected <- state_selected + dN_dt_selected
      
      # Handle extinction events: set population size to 0 when it falls below 0.0001
      state_selected[state_selected < 0.0001] <- 0
      
      # Record population history
      population_history_selected[step + 1, ] <- state_selected
      
    }
    
    # Handle extinction events: set population size to 0 when it falls below 0.0001
    final_populations_selected <- state_selected
    
    extinct_species_selected <- which(final_populations_selected < 0.0001)
    
    final_populations_selected[final_populations_selected < 0.0001] <- 0
    
    # Calculate community characteristics (based on the full species set)
    surviving_species <- sum(final_populations_selected >= 0.0001)
    total_individuals <- sum(final_populations_selected[final_populations_selected >= 0.0001])
    
    # Return results
    return(list(
      sim_id = sim_id,
      n_species = n_species,
      n_selected_species = n_selected,
      selected_species = selected_species,
      competition_strength = competition_strength,
      surviving_species = surviving_species,
      total_individuals = total_individuals,
      r_values = r_values,
      K_values = K_values,
      alpha_matrix = alpha,
      r_values_selected = r_values_selected,
      K_values_selected = K_values_selected,
      alpha_matrix_selected = alpha_selected,
      final_populations = final_populations_selected,
      population_history = population_history_selected,
      species_mapping = selected_species 
    ))
  }, error = function(e) {
    return(list(
      sim_id = sim_id,
      n_species = n_species,
      n_selected_species = n_selected,
      selected_species = selected_species,
      competition_strength = competition_strength,
      surviving_species = NA,
      total_individuals = NA,
      r_values = r_values,
      K_values = K_values,
      alpha_matrix = alpha,
      r_values_selected = r_values_selected,
      K_values_selected = K_values_selected,
      alpha_matrix_selected = alpha_selected,
      final_populations = rep(NA, n_selected),
      population_history = NA,
      species_mapping = selected_species
    ))
  })
}

# Parallel version of the simulation function
run_simulation_parallel <- function(args) {
  # args is a list containing all parameters
  return(run_simulation(args$n_species, args$competition_strength, args$sim_id, 
                        args$parameters, args$disturbance_sd))
}

# Main simulation procedure (parallel version)
main_simulation_parallel <- function(disturbance_sd = 0.1) {
  # Define the sequence of species-pool sizes
  species_pool_sizes <- seq(10, 100, by = 2)
  competition_strengths <- as.character(c(1:50)/50)
  n_replicates <- 100
  
  # Pre-generate parameters for each species-number-by-competition-strength combination
  parameters_pool <- list()
  
  for (n_species in species_pool_sizes) {
    for (comp_strength in competition_strengths) {
      key <- paste0("s", n_species, "_c", comp_strength)
      
      # Generate parameters
      r_values <- runif(n_species, 0.01, 0.5)
      K_values <- runif(n_species, 1, 150)
      
      # Generate the competition coefficient matrix according to competition strength
      alpha <- matrix(0, nrow = n_species, ncol = n_species)
      mu <- as.numeric(comp_strength)
      sigma <- 0.4
      
      for (i in 1:n_species) {
        for (j in 1:n_species) {
          if (i == j) {
            alpha[i, j] <- 1
          } else {
            aji <- rnorm(1, mu, sigma)
            aji <- max(0.001, min(1.2, aji))
            alpha[i, j] <- aji
          }
        }
      }
      
      parameters_pool[[key]] <- list(
        r_values = r_values,
        K_values = K_values,
        alpha_matrix = alpha
      )
    }
  }
  
  # Create the parameter list for all simulation tasks
  sim_tasks <- list()
  sim_counter <- 1
  
  for (n_species in species_pool_sizes) {
    for (comp_strength in competition_strengths) {
      key <- paste0("s", n_species, "_c", comp_strength)
      base_parameters <- parameters_pool[[key]]
      
      for (rep in 1:n_replicates) {
        sim_tasks[[sim_counter]] <- list(
          n_species = n_species,
          competition_strength = comp_strength,
          sim_id = sim_counter,
          parameters = base_parameters,
          disturbance_sd = disturbance_sd
        )
        sim_counter <- sim_counter + 1
      }
    }
  }
  
  # Set the number of parallel cores
  n_cores <- 30 # detectCores() <- 256
  cat("use", n_cores, "cores to calcualte...\n")
  
  # Create the cluster
  cl <- makeCluster(n_cores)
  
  # Export required functions and packages to worker nodes
  clusterExport(cl, c("run_simulation", "LV_model_discrete"))
  
  # Display progress information
  cat("start simulate, ", length(sim_tasks), "tasks in total...\n")
  
  # Run the parallel computation
  results_list <- parLapply(cl, sim_tasks, run_simulation_parallel)
  
  # Stop the cluster
  stopCluster(cl)
  
  # Process results
  results <- data.frame()
  parameter_list <- list()
  population_history_list <- list()
  
  for (sim_result in results_list) {
    # Store results
    results <- rbind(results, data.frame(
      sim_id = sim_result$sim_id,
      n_species = sim_result$n_species,
      competition_strength = sim_result$competition_strength,
      replicate = NA,  # To be reassigned in subsequent calculations
      surviving_species = sim_result$surviving_species,
      total_individuals = sim_result$total_individuals
    ))
    
    # Store parameters
    parameter_list[[sim_result$sim_id]] <- list(
      r_values = sim_result$r_values,
      K_values = sim_result$K_values,
      alpha_matrix = sim_result$alpha_matrix,
      r_values_selected = sim_result$r_values_selected,
      K_values_selected = sim_result$K_values_selected,
      alpha_matrix_selected = sim_result$alpha_matrix_selected
    )
    
    population_history_list[[sim_result$sim_id]] <- sim_result$population_history
  }
  
  # Reassign replicate IDs
  results <- do.call(rbind, lapply(split(results, 
                                         list(results$n_species, 
                                              results$competition_strength)), 
                                   function(df) {
                                     df$replicate <- 1:nrow(df)
                                     return(df)
                                   }))
  
  # Reorder by sim_id
  results <- results[order(results$sim_id), ]
  rownames(results) <- NULL
  
  return(list(results = results, parameters = parameter_list, population_history = population_history_list))
}

# Run the simulation
cat("start...\n")
start_time <- Sys.time()
simulation_results <- main_simulation_parallel(disturbance_sd = 0.1)
end_time <- Sys.time()

cat("finish! time spend:", round(end_time - start_time, 2), "seconds\n")
# Save results
results_df <- simulation_results$results
parameters <- simulation_results$parameters
population_history <- simulation_results$population_history

# Basic statistical analysis
print(summary(results_df))

# Save results to files
saveRDS(parameters, "LV_model_parameters_discrete_parallel.rds")
save(population_history, file = "LV_model_population_history_parallel.Rdata")


library(dplyr)

# Calculate the total number of rows and groups
n_total <- nrow(results_df)
n_groups <- ceiling(n_total / 100)

results_df <- results_df %>%
  mutate(
    Community = cut(row_number(), 
                    breaks = seq(0, n_total, by = 100),
                    labels = 1:n_groups,
                    include.lowest = TRUE)
  ) 

# Create a new list, population_history_last, retaining only the last row of each corresponding matrix

population_history_last <- list()
for (i in 1:230000){
  last_row <- population_history[[i]][1001, ]
  spec <- which(parameters[[i]]$r_values %in% parameters[[i]]$r_values_selected)
  names(last_row) <- paste0("V",spec)
  population_history_last[[i]] <- last_row
}

df_list <- list()
# Iterate over each matrix in population_history_last
for (i in 1:length(population_history_last)) {
  # Get the current matrix
  current_matrix <- population_history_last[[i]]
  
  # Convert the matrix to a data frame
  # First column: AccSpeciesName (matrix column names)
  # Second column: WeightDry (matrix values)
  current_df <- data.frame(
    AccSpeciesName = names(current_matrix),
    WeightDry = as.numeric(current_matrix)
  )
  
  # Add the third column, Community (from the corresponding Community in results_df)
  current_df$Community <- results_df$Community[i]
  
  current_df$Plot <- i
  
  # Add the current data frame to the list
  df_list[[i]] <- current_df
  
}

# Combine all data frames into one long-format data frame
data_of_LV <- do.call(rbind, df_list)

data_of_LV <- data_of_LV[data_of_LV$WeightDry>=0.0001,]

save(data_of_LV,file = "data_of_LV.Rdata")
save(population_history_last, file = "LV_model_population_history_last.Rdata")
rm(population_history)

# Directly extract alpha_matrix objects at all specified positions
indices <- seq(1, length(parameters), by = 100)
alpha_matrices <- sapply(indices, function(i) parameters[[i]]$alpha_matrix, simplify = FALSE)
save(alpha_matrices, file="alpha_matrices.Rdata")
library(purrr)
library(dplyr)

use_plots <- results_df$surviving_species>2
sum(use_plots) #219,004 observations

#######################################################################################
# 2. Calculate niche differences and fitness differences (aij) of the initial species at the plot level
library(parallel)

n_cores <- 5
cl <- makeCluster(n_cores)
clusterExport(cl, c("parameters","use_plots"))

# Process each community using parallel computation
results <- parLapply(cl, 1:230000, function(i) {
  if(use_plots[i]){
    alpha_mat <- parameters[[i]]$alpha_matrix_selected
    n_species <- nrow(alpha_mat)
    
    # Get the population history of the current community
    surviving_species <- c(1:nrow(parameters[[i]]$alpha_matrix_selected))
    
    niche_diff_pair <- c()   # Niche differences for species pairs in the current community
    fitness_diff_pair <- c() # Fitness differences for species pairs in the current community
    
    # Define surviving species pairs as all species pairs (avoid duplication by calculating only the upper triangle)
    surviving_pairs <- combn(surviving_species, 2)
    
    for (pair_idx in 1:ncol(surviving_pairs)) {
      j <- surviving_pairs[1, pair_idx]
      k <- surviving_pairs[2, pair_idx]
      
      # Calculate niche difference: 1 - sqrt(alpha_ij * alpha_ji)
      niche_diff <- 1 - sqrt(alpha_mat[j, k] * alpha_mat[k, j])
      
      # Calculate fitness difference: sqrt(alpha_ij / alpha_ji) (assuming alpha_ij != 0)
      if (alpha_mat[j, k] == 0 || alpha_mat[k, j] == 0) {
        warning(paste("in community", i, ", interspecific competitive interaction strength between species", j, " and ", k, "is 0, FiD is NA"))
        fitness_diff <- NA
      } else {
        fitness_diff <- sqrt(alpha_mat[j, k] / alpha_mat[k, j])
        if(fitness_diff < 1){
          fitness_diff <- 1/fitness_diff
        }
      }
      
      niche_diff_pair <- c(niche_diff_pair, niche_diff)
      fitness_diff_pair <- c(fitness_diff_pair, fitness_diff)
    }
  }else{
    niche_diff_pair <- NA
    fitness_diff_pair <- NA
  }
  
  
  
  # Return results for the current community
  return(list(
    niche_diff = niche_diff_pair,
    fitness_diff = fitness_diff_pair,
    niche_avg = ifelse(length(niche_diff_pair) > 0, mean(niche_diff_pair, na.rm = TRUE), NA),
    fitness_avg = ifelse(length(fitness_diff_pair) > 0, mean(fitness_diff_pair, na.rm = TRUE), NA)
  ))
})

# Stop the cluster
stopCluster(cl)

# Extract results
niche_differences <- lapply(results, function(x) x$niche_diff)
fitness_differences <- lapply(results, function(x) x$fitness_diff)
community_niche_avg <- sapply(results, function(x) x$niche_avg)
community_fitness_avg <- sapply(results, function(x) x$fitness_avg)

# Store results in the data frame
results_df$community_niche_avg_orig <- community_niche_avg
results_df$community_fitness_avg_orig <- community_fitness_avg


save(niche_differences,fitness_differences,file = "1103NiDFiD(plot_spe_more_than_3).Rdata")
write.csv(results_df, "LV_model_simulation_results_discrete_parallel.csv", row.names = FALSE)

#######################################################################################
# 3. observed interspecific competition strength among community species
load("LV_model_population_history_last.Rdata")
load("data_of_LV.Rdata")
results_df <- read.csv("LV_model_simulation_results_discrete_parallel.csv")

library(gvlma)
library(dplyr)
library(tidyr)

species_all1 <- unique(data_of_LV$AccSpeciesName)
na_matrix <- matrix(NA, nrow = 2300, ncol = 1)
result_data_frame <- as.data.frame(na_matrix)
colnames(result_data_frame) <- c("community")
result_data_frame$community <- c(1:2300)
communitys <- c(1:2300)
result_data_frame$comp_stren_aij <- rep(c(1:50)/50,46)
result_data_frame$species_pool <- rep(seq(10, 100, by = 2), each = 50)
result_data_frame$species_pool_scaled <- scale(result_data_frame$species_pool)
result_data_frame$log_species_pool <- log(result_data_frame$species_pool)
result_data_frame$log_species_pool_scaled <- scale(result_data_frame$log_species_pool)


species_count <- data_of_LV %>%
  group_by(Community, AccSpeciesName) %>%
  summarise(PlotCount = n_distinct(Plot), .groups = "drop")

# Group by sampling occasion and identify species occurring in more than four plots
result1 <- species_count %>%
  group_by(Community) %>%
  group_map(~ {
    top_species <- .x$AccSpeciesName[.x$PlotCount > 4]
    list(top_species = top_species)
  })

# Extract the list corresponding to each sampling occasion
top_species_list <- lapply(result1, function(x) x$top_species)

spe_comp_all <- sort(unique(unlist(top_species_list, use.names = FALSE)))
spe_comp_matrix <- matrix(NA, nrow = length(spe_comp_all), ncol = 2300)
rownames(spe_comp_matrix) <- spe_comp_all
colnames(spe_comp_matrix) <- 1:2300

result_data_frame$nega_mean_obs_interspec_competition <- NA
result_data_frame$abs_cv_obs_interspec_competition <- NA


for(zz in 1:2300){
  
  curr_spe <- c()
  other_spe <- c()
  spe_name <- c()
  
  top_species_list1 <- top_species_list[[zz]]
  nega_obs_interspec_competition <- c()
  
  communitys1 <- communitys[zz]
  data2 <- data_of_LV[data_of_LV$Community %in% communitys1, ]
  unique_species <- unique(data2$AccSpeciesName)
  # Identify elements of vec1 that also occur in vec2
  unique_species <- unique_species[unique_species %in% top_species_list1]
  # Iterate over each species
  for (sp in unique_species) {
    # Filter data for the current species
    species_data <- data2[data2$AccSpeciesName == sp, ]
    other_species_biomass <- c()
    current_species_biomass <- c()
    
    # For each plot
    coun <- 0
    for (q in unique(species_data$Plot)) {
      # Filter data for the current plot
      quadrat_data <- data2[data2$Plot == q, ]
      
      if(length(quadrat_data$WeightDry)>2){
        coun <- coun+1
        # Calculate the total biomass of all other species in the current plot
        other_species_biomass <- c(other_species_biomass, sum(quadrat_data$WeightDry[quadrat_data$AccSpeciesName != sp]))
        
        # Extract the biomass of the focal species in the current plot
        current_species_biomass <- c(current_species_biomass, quadrat_data$WeightDry[quadrat_data$AccSpeciesName == sp])
      }
      
    }
    if(length(unique(other_species_biomass))>4){
      model <- lm(current_species_biomass ~ other_species_biomass)
      summary_model <- summary(model)
      nega_obs_interspec_competition <- c(nega_obs_interspec_competition, summary_model$coefficients["other_species_biomass", "Estimate"])
      
      curr_spe <- c(curr_spe, current_species_biomass)
      other_spe <- c(other_spe, other_species_biomass)
      spe_name <- c(spe_name, rep(sp, coun))
      spe_comp_matrix[sp,zz] <- summary_model$coefficients["other_species_biomass", "Estimate"]
    }
    
  }
  if(is.null(nega_obs_interspec_competition)){
    result_data_frame$nega_mean_obs_interspec_competition[zz] <- NA
    result_data_frame$abs_cv_obs_interspec_competition[zz] <- NA
    
  }else{
    result_data_frame$nega_mean_obs_interspec_competition[zz] <- mean(nega_obs_interspec_competition, na.rm = TRUE)
    result_data_frame$abs_cv_obs_interspec_competition[zz] <- abs(sd(nega_obs_interspec_competition, na.rm = TRUE)/result_data_frame$nega_mean_obs_interspec_competition[zz])
    
  }
  
}


library(ggplot2)
library(dplyr)
library(purrr)
library(gridExtra)

comm_to_plot <- unlist(lapply(seq(5, 40, 5), function(x) seq(x, 2040, 1000)))
plot_list1 <- list()
colnames(data_of_LV)[3:4] <- c("Community", "Plot")

for(zz in comm_to_plot){
  
  curr_spe <- c()
  other_spe <- c()
  spe_name <- c()
  
  top_species_list1 <- top_species_list[[zz]]
  nega_obs_interspec_competition <- c()
  
  communitys1 <- communitys[zz]
  data2 <- data_of_LV[data_of_LV$Community %in% communitys1, ]
  unique_species <- unique(data2$AccSpeciesName)
  # Identify elements of vec1 that also occur in vec2
  unique_species <- unique_species[unique_species %in% top_species_list1]
  # Iterate over each species
  for (sp in unique_species) {
    # Filter data for the current species
    species_data <- data2[data2$AccSpeciesName == sp, ]
    other_species_biomass <- c()
    current_species_biomass <- c()
    
    # For each plot
    coun <- 0
    for (q in unique(species_data$Plot)) {
      # Filter data for the current plot
      quadrat_data <- data2[data2$Plot == q, ]
      
      if(length(quadrat_data$WeightDry)>2){
        coun <- coun+1
        # Calculate the total biomass of all other species in the current plot
        other_species_biomass <- c(other_species_biomass, sum(quadrat_data$WeightDry[quadrat_data$AccSpeciesName != sp]))
        
        # Extract the biomass of the focal species in the current plot
        current_species_biomass <- c(current_species_biomass, quadrat_data$WeightDry[quadrat_data$AccSpeciesName == sp])
      }
      
    }
    if(length(unique(other_species_biomass))>4){
      
      curr_spe <- c(curr_spe, current_species_biomass)
      other_spe <- c(other_spe, other_species_biomass)
      spe_name <- c(spe_name, rep(sp, coun))
    }
  }
  spe_jz <- data.frame(curr_spe,other_spe,spe_name)
  
  
  p <- ggplot(spe_jz, aes(x = other_spe, y = curr_spe, color = spe_name)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(
      aes(group = spe_name),
      method = "lm", se = FALSE, linewidth = 0.8, alpha = 0.5
    ) +  
    annotate("text", x = min(spe_jz$other_spe), y = max(spe_jz$curr_spe), 
             label = paste0("Pool = ", result_data_frame$species_pool[zz],", Comp = ", result_data_frame$comp_stren_aij[zz]), hjust = 0, vjust = 1, size = 9) +
    labs(
      x = "Dry Weight Sum of Other Species", 
      y = "Dry Weight of Each Species",
      color = "Species Name"
    ) +
    # Add axis breaks and tick marks
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
  
  plot_list1 <- c(plot_list1, list(p))
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
    filename = paste0("calculate competitive interactions among species_", page, ".png"),
    plot = combined_plot,
    width = 32,  
    height = 60, 
    dpi = 300,   
    limitsize = FALSE
  )
}
plot_list1 <- list()
rm(list = c("current_plots", "combined_plot"))
gc()

#######################################################################################
# 4. Calculate realized-diversity BEF and species-pool size
use_plots <- results_df$surviving_species>2
sum(use_plots) #219,004 observations
slope_results <- results_df %>%
  group_by(Community) %>%
  summarise(
    bef_log_realized = if(all(surviving_species <= 2)) {
      NA_real_
    } else if(n_distinct(surviving_species) > 1) {
      model <- lm(total_individuals ~ log(surviving_species))
      coef(model)[2]
    } else {
      NA_real_
    },
    se_bef_log_realized = if(all(surviving_species <= 2)) {
      NA_real_
    } else if(n_distinct(surviving_species) > 1) {
      model <- lm(total_individuals ~ log(surviving_species))
      summary(model)$coefficients[2, 2]
    } else {
      NA_real_
    },
    n_obs = n(),
    .groups = "drop"
  )
colnames(result_data_frame)[1] <- c("Community")
slope_results <- slope_results[,-4]
colnames(slope_results)[1] <- c("Community")
result_data_frame <- result_data_frame %>%
  left_join(slope_results, by = "Community")


#######################################################################################
# 5. Calculate NiD and FiD

colnames(data_of_LV)[4] <- "Plot" 
result_list <- list()

for(community in colnames(spe_comp_matrix)){
  df_community <- data_of_LV %>% filter(Community == community)
  for(plot in unique(df_community$Plot)){
    df_plot <- df_community %>% filter(Plot == plot)
    species_in_plot <- df_plot$AccSpeciesName
    # Identify the corresponding community column and rows for these species in spe_comp_matrix
    species_in_matrix <- intersect(species_in_plot, rownames(spe_comp_matrix))
    if(length(species_in_matrix) > 2){
      # Extract the corresponding submatrix
      comp_values <- spe_comp_matrix[species_in_matrix, as.character(community), drop = FALSE]
      
      # Calculate the mean and coefficient of variation (ignoring NA values)
      mean_val <- mean(comp_values, na.rm = TRUE)
      sd_val <- abs(sd(comp_values, na.rm = TRUE)/mean_val)
    } else {
      mean_val <- NA
      sd_val <- NA
    }
    
    # Save results
    result_list[[length(result_list) + 1]] <- data.frame(
      Community = community,
      Plot = plot,
      NiD_plot = mean_val,
      FiD_plot = sd_val
    )
  }
}

# Combine all results
result_df1 <- do.call(rbind, result_list)
result_df1$Community <- as.numeric(result_df1$Community)
colnames(result_df1)[3:4] <- c("NiD_plot","FiD_plot")

data_for_lmer <- data_of_LV %>%
  group_by(Community, Plot) %>%
  summarise(TotalWeightDry = sum(WeightDry, na.rm = TRUE))
data_for_lmer$Community <- as.numeric(data_for_lmer$Community)

data_for_lmer <- data_for_lmer %>%
  left_join(result_df1, by = c("Community", "Plot"))

colnames(results_df)[1] <- "Plot"
results_df0 <- results_df[,-7]
data_for_lmer <- data_for_lmer %>%
  left_join(results_df0, by = "Plot")

data_for_lmer$log_species_pool_scaled <- scale(log(data_for_lmer$n_species))
write.csv(results_df, "LV_model_redults_df.csv", row.names = FALSE)

#######################################################################################
# 6. Relationships between the two NiD metrics, two FiD metrics, and two measures of interspecific competition strength

data_for_lmer1 <- na.omit(data_for_lmer) #218,831 observations retained out of approximately 230,000
data_for_lmer1$NiD_plot_scaled <- scale(data_for_lmer1$NiD_plot)
data_for_lmer1$FiD_plot_scaled <- scale(data_for_lmer1$FiD_plot)
data_for_lmer1 <- data_for_lmer1[(data_for_lmer1$FiD_plot_scaled>-3)&(data_for_lmer1$FiD_plot_scaled<3),]
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
data_for_lmer1$community_niche_diff_orig_scaled <- scale(data_for_lmer1$community_niche_avg_orig)
data_for_lmer1$community_fitness_diff_orig_scaled <- scale(data_for_lmer1$community_fitness_avg_orig)
data_for_lmer1 <- data_for_lmer1[(data_for_lmer1$community_niche_diff_orig_scaled>-3)&(data_for_lmer1$community_niche_diff_orig_scaled<3)&(data_for_lmer1$community_fitness_diff_orig_scaled>-3)&(data_for_lmer1$community_fitness_diff_orig_scaled<3),]
data_for_lmer1$community_niche_diff_orig_scaled <- scale(data_for_lmer1$community_niche_diff_orig_scaled)
data_for_lmer1$community_fitness_diff_orig_scaled <- scale(data_for_lmer1$community_fitness_diff_orig_scaled)
data_for_lmer1$NiD_plot_scaled <- scale(data_for_lmer1$NiD_plot_scaled)
data_for_lmer1$FiD_plot_scaled <- scale(data_for_lmer1$FiD_plot_scaled)

summary(data_for_lmer1$community_niche_diff_orig_scaled)
summary(data_for_lmer1$community_fitness_diff_orig_scaled)
summary(data_for_lmer1$NiD_plot_scaled)
summary(data_for_lmer1$FiD_plot_scaled)



# 209,640 observations remain
NiDFiD_df <- data_for_lmer1 %>%
  group_by(Community) %>%
  summarise(community_fitness_diff_orig = mean(community_fitness_diff_orig_scaled, na.rm = TRUE),
            community_niche_diff_orig = mean(community_niche_diff_orig_scaled, na.rm = TRUE),
            dry_weight = mean(TotalWeightDry, na.rm = TRUE),
            dry_weight_var = var(TotalWeightDry, na.rm = TRUE),
            n = n(),
            NiD = mean(NiD_plot_scaled, na.rm = TRUE),
            FiD = mean(FiD_plot_scaled, na.rm = TRUE))

result_data_frame1 <- result_data_frame[which(result_data_frame$Community %in% unique(data_for_lmer1$Community)),]
result_data_frame1 <- result_data_frame1 %>%
  left_join(NiDFiD_df, by = "Community")

result_data_frame1$dry_weight_var <- result_data_frame1$dry_weight_var/result_data_frame1$n
result_data_frame1$community_niche_diff_orig_scaled <- scale(result_data_frame1$community_niche_diff_orig)
result_data_frame1$community_fitness_diff_orig_scaled <- scale(result_data_frame1$community_fitness_diff_orig)
result_data_frame1$NiD_scaled <- scale(result_data_frame1$NiD)
result_data_frame1$FiD_scaled <- scale(result_data_frame1$FiD)

summary(result_data_frame1$community_niche_diff_orig_scaled)
summary(result_data_frame1$community_fitness_diff_orig_scaled)

model_bef_pool1 <- lm(
  dry_weight ~ log_species_pool_scaled*NiD_scaled+log_species_pool_scaled*FiD_scaled,
  data = result_data_frame1,weights = 1 / dry_weight_var
)
summary(model_bef_pool1)


model_bef_pool2 <- lm(
  dry_weight ~ log_species_pool_scaled*community_niche_diff_orig_scaled+log_species_pool_scaled*community_fitness_diff_orig_scaled,
  data = result_data_frame1,weights = 1 / dry_weight_var
)
summary(model_bef_pool2)


# 2288个
library(ggplot2)
# Two NiD metrics (community level)
summary(lm(NiD_scaled~community_niche_diff_orig_scaled, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = NiD_scaled, x = community_niche_diff_orig_scaled)) +
  geom_point(size = 2,alpha = 0.2,colour = "#444444") +
  labs(y = "Niche Difference obs(scaled)", x = "Niche Difference α(scaled)") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "#444444", fill = "#CCCCCC") +
  annotate("text", x = min(result_data_frame1$community_niche_diff_orig_scaled), y = max(result_data_frame1$NiD_scaled), 
           label = "R²=0.75, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave("Two NiD metrics (community level).png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)


# Two FiD metrics (community level)
summary(lm(FiD_scaled~community_fitness_diff_orig_scaled, data = result_data_frame1))
p <- ggplot(result_data_frame1, aes(y = FiD_scaled, x = community_fitness_diff_orig_scaled)) +
  geom_point(size = 2,alpha = 0.2,colour = "#444444") +
  labs(y = "Fitness Difference obs(scaled)", x = "Fitness Difference α(scaled)") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, colour = "#444444", fill = "#CCCCCC") +
  annotate("text", x = min(result_data_frame1$community_fitness_diff_orig_scaled), y = max(result_data_frame1$FiD_scaled), 
           label = "R²=0.12, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave("Two FiD metrics (community level).png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)


# Two NiD metrics (quadart level)
summary(lm(NiD_plot_scaled~community_niche_diff_orig_scaled, data = data_for_lmer1))
p <- ggplot(data_for_lmer1, aes(y = NiD_plot_scaled, x = community_niche_diff_orig_scaled)) +
  geom_point(size = 2,alpha = 0.01,colour = "#555555") +
  labs(y = "Niche Difference obs(scaled)", x = "Niche Difference α(scaled)") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "#555555", fill = "#CCCCCC") +
  annotate("text", x = min(data_for_lmer1$community_niche_diff_orig_scaled), y = max(data_for_lmer1$NiD_plot_scaled), 
           label = "R²=0.65, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave("Two NiD metrics (quadart level).png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)


# Two FiD metrics (quadart level)
summary(lm(FiD_plot_scaled~community_fitness_diff_orig_scaled, data = data_for_lmer1))
p <- ggplot(data_for_lmer1, aes(y = FiD_plot_scaled, x = community_fitness_diff_orig_scaled)) +
  geom_point(size = 2,alpha = 0.01,colour = "#555555") +
  labs(y = "Fitness Difference obs(scaled)", x = "Fitness Difference α(scaled)") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "#555555", fill = "#CCCCCC") +
  annotate("text", x = min(data_for_lmer1$community_fitness_diff_orig_scaled), y = max(data_for_lmer1$FiD_plot_scaled)+0.2, 
           label = "R²=0.06, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave("Two FiD metrics (quadart level).png", 
       plot = p,
       width = 5,  
       height = 5, 
       dpi = 300)

# Relationship between the two measures of interspecific competition strength
load("alpha_matrices.Rdata")
comp_aij <- sapply(alpha_matrices, function(mat) {
  # Calculate the number of off-diagonal elements
  diag_elements <- ncol(mat)
  non_diag_elements <- diag_elements*diag_elements - diag_elements
  # Calculate the sum of off-diagonal elements
  total_sum <- sum(mat)
  diag_sum <- sum(diag(mat))
  non_diag_sum <- total_sum - diag_sum
  # Calculate the mean
  non_diag_sum / non_diag_elements
})
spe_comp_df <- data.frame(comp_aij = comp_aij,
                          comp_obs = -colMeans(spe_comp_matrix,na.rm = TRUE))
spe_comp_df$comp_aij_scaled <- scale(spe_comp_df$comp_aij)
spe_comp_df$comp_obs_scaled <- scale(spe_comp_df$comp_obs)
spe_comp_df <- na.omit(spe_comp_df)
spe_comp_df <- spe_comp_df[(spe_comp_df$comp_obs_scaled>-3)&(spe_comp_df$comp_obs_scaled<3),]
spe_comp_df$comp_aij_scaled <- scale(spe_comp_df$comp_aij)
spe_comp_df$comp_obs_scaled <- scale(spe_comp_df$comp_obs)
spe_comp_df <- spe_comp_df[(spe_comp_df$comp_aij_scaled>-3)&(spe_comp_df$comp_aij_scaled<3),]
spe_comp_df$comp_aij_scaled <- scale(spe_comp_df$comp_aij)
spe_comp_df$comp_obs_scaled <- scale(spe_comp_df$comp_obs)

summary(lm(comp_obs_scaled~comp_aij_scaled, data=spe_comp_df))
summary(lm(comp_obs~comp_aij, data=spe_comp_df))
cor(spe_comp_df$comp_aij,spe_comp_df$comp_obs)
cor.test(spe_comp_df$comp_aij,spe_comp_df$comp_obs)
# 2,285 observations
p <- ggplot(spe_comp_df, aes(y = comp_obs, x = comp_aij)) +
  geom_point(size = 2,alpha = 0.2,colour = "#444444") +
  labs(y = "Interspecific Competition obs", x = "Interspecific Competition α") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2, color = "#444444", fill = "#CCCCCC") +
  annotate("text", x = min(spe_comp_df$comp_aij), y = max(spe_comp_df$comp_obs)+0.2, 
           label = "R²=0.75, p<0.001", hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    aspect.ratio = 1,
    panel.grid = element_blank(),
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave("competitive interactions among species (community level).png", 
       plot = p,
       width = 6,  
       height = 5, 
       dpi = 300)
