# Sample R code for SCA----

# Create example data
age <- 1:10
N <- c(100, 150, 200, 180, 160, 120, 90, 80, 50, 30)
F <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
Z <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1)

# Calculate catch
C <- N * F * (1 - exp(-Z))

# Create dataframe
data <- data.frame(Age = age, Catch = C)

# Plot catch by age
plot(data$Age, data$Catch, type = "b", xlab = "Age", ylab = "Catch", main = "Statistical Catch at Age")


# Sample R code for VPA----

# Create example data
year <- 1:5
N <- matrix(c(100, 150, 200, 180, 160, 120, 90, 80, 50, 30), ncol = 5)
F <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), ncol = 5)
Z <- matrix(c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1), ncol = 5)

# Calculate total catch
C <- apply(N * F * (1 - exp(-Z)), 2, sum)

# Create dataframe
data <- data.frame(Year = year, Catch = C)

# Plot total catch by year
plot(data$Year, data$Catch, type = "b", xlab = "Year", ylab = "Total Catch", main = "Virtual Population Analysis")


## Sample R code for YPR----

# Set parameter values
F <- seq(0.1, 1, by = 0.1)
R <- 1000
Z <- 0.8
M <- 0.2

# Calculate yield-per-recruit
YPR <- (F * R * exp(-Z)) / (Z + M)

# Create dataframe
data <- data.frame(Fishing_Mortality = F, YPR = YPR)

# Plot yield-per-recruit
plot(data$Fishing_Mortality, data$YPR, type = "l", xlab = "Fishing Mortality", ylab = "Yield-per-Recruit", main = "Yield-Per-Recruit")


# Sample R code for Beverton and Holt model----

# Set parameter values
alpha <- 0.2
SSB <- seq(0, 1000, by = 100)

# Calculate recruitment
R <- (4 * alpha * SSB) / (alpha * SSB + 1)

# Create dataframe
data <- data.frame(SSB = SSB, Recruitment = R)

# Plot recruitment as a function of SSB
plot(data$SSB, data$Recruitment, type = "l", xlab = "Spawning Stock Biomass (SSB)", ylab = "Recruitment", main = "Beverton and Holt Model")


# Sample R code for Ricker model----

# Set parameter values
alpha <- 0.2
beta <- 0.01
SSB <- seq(0, 1000, by = 100)

# Calculate recruitment
R <- alpha * SSB * exp(-beta * SSB)

# Create dataframe
data <- data.frame(SSB = SSB, Recruitment = R)

# Plot recruitment as a function of SSB
plot(data$SSB, data$Recruitment, type = "l", xlab = "Spawning Stock Biomass (SSB)", ylab = "Recruitment", main = "Ricker Model")


# Sample R code for length-based catch curve----

# Assuming L is a vector of length bins and C is a vector of catch corresponding to each length bin

L <- c(20, 30, 40, 50, 60)
C <- c(100, 80, 50, 30, 10)

# Calculate cumulative catch
cumulative_Catch <- cumsum(C)

# Create dataframe
data <- data.frame(Length = L, Cumulative_Catch = cumulative_Catch)

# Plot cumulative catch by length
plot(data$Length, data$Cumulative_Catch, type = "b", xlab = "Length", ylab = "Cumulative Catch", main = "Length-based Catch Curve")


# Sample R code for VBGF----

# Set parameter values
L_inf <- 100
k <- 0.2
t <- 1:20

# Calculate length at age
L <- L_inf * (1 - exp(-k * t))

# Create dataframe
data <- data.frame(Age = t, Length = L)

# Plot length by age
plot(data$Age, data$Length, type = "b", xlab = "Age", ylab = "Length", main = "Von Bertalanffy Growth Function")

# Sample R code for surplus production model----

# Assuming CPUE is a vector of catch-per-unit-effort values and Effort is a vector of corresponding effort values

CPUE <- c(100, 80, 50, 40, 30)
Effort <- c(10, 15, 20, 25, 30)

# Calculate surplus production
surplus_production <- CPUE * Effort

# Create dataframe
data <- data.frame(Effort = Effort, Surplus_Production = surplus_production)

# Plot surplus production by effort
plot(data$Effort, data$Surplus_Production, type = "b", xlab = "Effort", ylab = "Surplus Production", main = "Surplus Production Model")


# Sample R code for stock assessment model----

# Assuming biomass is a vector of biomass values and year is a vector of corresponding years

biomass <- c(1000, 1200, 800, 600, 900)
year <- 2010:2014

# Create dataframe
data <- data.frame(Year = year, Biomass = biomass)

# Plot biomass over time
plot(data$Year, data$Biomass, type = "b", xlab = "Year", ylab = "Biomass", main = "Stock Assessment Model")



# Set parameter values---
R <- 1000  # Recruitment
age <- 1:10  # Age classes

# Varying values of Z
Z_values <- c(0.2, 0.4, 0.6)

# Create dataframe to store results
results <- data.frame(Age = age)

# Calculate cohort size for each Z value
for (Z in Z_values) {
  N <- R * exp(-Z * age)
  results <- cbind(results, N)
}

# Plot cohort decline with varying Z values
matplot(results$Age, results[, -1], type = "l", xlab = "Age", ylab = "Cohort Size",
        main = "Cohort Decline with Varying Z (Total Mortality)",
        col = rainbow(length(Z_values)), lty = 1:length(Z_values),
        legend = legend("topright", legend = Z_values, col = rainbow(length(Z_values)), lty = 1:length(Z_values)))


# Set parameter values----
R <- 1000  # Recruitment
age <- 1:10  # Age classes

# Varying values of F
F_values <- c(0.2, 0.4, 0.6)

# Create dataframe to store results
results <- data.frame(Age = age)

# Calculate cohort size for each F value
for (F in F_values) {
  Z <- 0.8  # Constant value of Z (total mortality)
  M <- 0.2  # Constant value of M (natural mortality)
  N <- R * exp(-(Z + M + F) * age)
  results <- cbind(results, N)
}

# Plot cohort decline with varying F values
matplot(results$Age, results[, -1], type = "l", xlab = "Age", ylab = "Cohort Size",
        main = "Cohort Decline with Varying F (Fishing Mortality)",
        col = rainbow(length(F_values)), lty = 1:length(F_values),
        legend = legend("topright", legend = F_values, col = rainbow(length(F_values)), lty = 1:length(F_values)))



# Set parameter values----
R <- 1000  # Recruitment
age <- 1:10  # Age classes

# Varying values of M
M_values <- c(0.1, 0.3, 0.5)

# Create dataframe to store results
results <- data.frame(Age = age)

# Calculate cohort size for each M value
for (M in M_values) {
  Z <- 0.8  # Constant value of Z (total mortality)
  F <- 0.2  # Constant value of F (fishing mortality)
  N <- R * exp(-(Z + M + F) * age)
  results <- cbind(results, N)
}

# Plot cohort decline with varying M values
matplot(results$Age, results[, -1], type = "l", xlab = "Age", ylab = "Cohort Size",
        main = "Cohort Decline with Varying M (Natural Mortality)",
        col = rainbow(length(M_values)), lty = 1:length(M_values),
        legend = legend("topright", legend = M_values, col = rainbow(length(M_values)), lty = 1:length(M_values)))


# Set parameter values----
R <- 1000  # Recruitment
age <- 1:10  # Age classes
F_values <- seq(0, 1, by = 0.01)  # Range of F values

# Calculate yield for each F value
yield <- numeric(length(F_values))
for (i in seq_along(F_values)) {
  Z <- 0.8  # Total mortality rate
  M <- 0.2  # Natural mortality rate
  N <- R * exp(-(Z + M + F_values[i]) * age)
  catch <- N * F_values[i]
  yield[i] <- sum(catch)
}

# Find Fmax and F0.1
Fmax <- F_values[which.max(yield)]
F0.1 <- F_values[which(yield >= 0.1 * max(yield))[1]]

# Plot yield vs. F values
plot(F_values, yield, type = "l", xlab = "F (Fishing Mortality)", ylab = "Yield",
     main = "Yield vs. Fishing Mortality",
     col = "blue", lwd = 2)
abline(v = Fmax, lty = 2, col = "red", lwd = 2, lwd.ticks = 2)
abline(v = F0.1, lty = 3, col = "green", lwd = 2, lwd.ticks = 2)
legend("topright", legend = c("Yield", paste0("Fmax = ", Fmax), paste0("F0.1 = ", F0.1)),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), lwd = c(2, 2, 2))
