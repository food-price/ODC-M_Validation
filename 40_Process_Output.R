# Process output from ODC-M model
# Current output: means and variances across individuals for each cycle and simulation

setwd()

library(abind)

# Settings
vars <- c("Obesity", "Diabetes", "CVD_history", "QALY", "HCE", "QALY_disc", "cost_disc", 
          "All-cause mortality", "Incident Stroke", "Incident CHD")
races <- c("all", "NHW", "NHB", "Hisp")
# seeds = c("1234", "2345", "23456", "234567")
seeds = c("513")
date = "2022-02-16"
year1 = 2001
n.cycle = 15
n.sim = 50
output_path = "final_output.csv"

# Initialize final output (means and 95% CIs for each variable, race, and cycle - long format)
final_out = data.frame()
for (race in races) {
  # Combine output from several runs
  for (seed in seeds) {
    if (seed == seeds[1]) {
      model_out_means = readRDS(paste("means", race, "No_Policy", "SEED", seed, date, ".rda", sep = "_"))
      model_out_vars = readRDS(paste("vars", race, "No_Policy", "SEED", seed, date, ".rda", sep = "_"))
    } else {
      model_out_means = abind(model_out_means, 
                              readRDS(paste("means", race, "No_Policy", "SEED", seed, date, ".rda", sep = "_")))
      model_out_vars = abind(model_out_vars,
                             readRDS(paste("vars", race, "No_Policy", "SEED", seed, date, ".rda", sep = "_")))
    }
  }
  for (i_var in vars) {
    temp.means = model_out_means[i_var,1:n.cycle,]
    temp.within.var = model_out_vars[i_var,1:n.cycle,]
    temp_out <- data.frame(Year = year1:(year1+n.cycle-1))
    temp_out$Outcome <- i_var
    temp_out$RE <- race
    # Mean of means across simulations (final mean per cycle)
    temp_out$mean = apply(temp.means, 1, mean)
    # Mean of variances across simulations (final within-simulation variance per cycle)
    within.var <- apply(temp.within.var, 1, mean)
    # Variance (SE^2) of means across simulations (final between-simulation variance per cycle)
    between.var <- apply(temp.means, 1, function(x) sd(x)^2/n.sim)
    # Final variance calculation per cycle (Rubin's rule formula)
    # Source: Dakin et al. Accurately Reflecting Uncertainty When Using Patient-Level Simulation Models to Extrapolate Clinical Trial Data. MDM. 2020.
    final.var <- within.var + (1 + (1/n.sim)) * between.var
    final.se <- sqrt(final.var)
    # Calculate lower limit of 95% CI
    temp_out$LL = temp_out$mean - 1.96 * final.se
    # Calculate upper limit of 95% CI
    temp_out$UL = temp_out$mean + 1.96 * final.se
    final_out = rbind(final_out, temp_out)
  }
}

# Save output
write.csv(final_out, file = output_path, row.names = F)
