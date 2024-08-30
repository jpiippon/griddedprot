# Distribution for proper use factors
library(truncnorm)

# Define values
vals_min <- c(0.25, 0.30, 0.20, 0.25, 0.125, 0.20, 0.47, 0.12, 0.20)
vals_max <- c(0.60, 0.45, 0.80, 0.60, 0.25, 0.50, 0.66, 0.50, 0.70)
vals_average <- c(0.3182, 0.375, 0.4768, 0.4333, 0.3198, 0.3786, 0.5733, 0.2711, 0.475)

# Calculate standard deviation
all_vals <- c(vals_min, vals_max, vals_average)
vals_sd <- sd(all_vals)


# Generate random values using the truncated normal distribution
PUF_dist <- rtruncnorm(1000, 
                       a = min(vals_min), # 0.12
                       b = max(vals_max), # 0.80
                       mean = mean(vals_average),
                       sd = vals_sd)

# Calculate the median of generated values
(PUF_median <- median(PUF_dist)) # 42%
(PUF_mean <- mean(PUF_dist)) # 42%
(PUF_cv <- sd(PUF_dist) / PUF_mean * 100) # 37%
# Coefficient of Variation (CV) measures the relative variability of a dataset.
#In this case, a CV of 36% for PUF_dist indicates that the spread of values around the mean (42%) 
# is about 36% of the mean value. A higher CV suggests greater variability and potential uncertainty in the distribution.

# Check the distribution of generated values
hist(PUF_dist, 
     main="Truncated Normal Distribution for PUF", 
     xlab="Value", 
     col="skyblue", 
     border="black")



# References for derived PUF values:
# Amiri, F., Shariff, A.R.B.M., Tabatabaie, T., 2012. Monitoring Land Suitability for Mixed Livestock Grazing Using Geographic Information System (GIS). Application of Geographic Information Systems. https://doi.org/10.5772/47939
# De Leeuw, P.N., Tothill, J.C., 1990. The concept of rangeland carrying capacity in sub-Saharan Africa: Myth or reality. Overseas Development Institute, Pastoral Development Network London.
# Harris, P.S., 2000. Grassland resource assessment for pastoral systems. FAO plant production and protection paper.
# Holechek, J.L., 1988. An approach for setting the stocking rate. Rangelands Archives 10, 10–14.
# Meehan, M.A., Sedivec, K.K., Printz, J.L., Brummer, F.A., 2018. Determining carrying capacity and stocking rates for range and pasture in North Dakota. NDSU Extension, North Dakota State University.
# Meshesha, D.T., Moahmmed, M., Yosuf, D., 2019. Estimating carrying capacity and stocking rates of rangelands in Harshin District, Eastern Somali Region, Ethiopia. Ecology and Evolution 9, 13309–13319.
# Qin, P., Sun, B., Li, Z., Gao, Z., Li, Y., Yan, Z., Gao, T., 2021. Estimation of Grassland Carrying Capacity by Applying High Spatiotemporal Remote Sensing Techniques in Zhenglan Banner, Inner Mongolia, China. Sustainability 13, 3123. https://doi.org/10.3390/su13063123
# Valentine, K.A., 1947. Distance from Water as a Factor in Grazing Capacity of Rangeland. JOURNAL OF FORESTRY 6.
# Vallentine, J.F., 2016. Grazing Management. Academic Press.
