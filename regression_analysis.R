install.packages("plm")

cleaned_data <- read_csv("~/Desktop/Semester 8/MAT Probability Statistics 2/Project/cleaned_data.csv")
View(cleaned_data)
dim(cleaned_data)

unique_data <- cleaned_data[!duplicated(cleaned_data[c('Country', 'Year')]),] 
View(unique_data) 
dim(unique_data)


##################################################################### 2005
data_2005 <- subset(cleaned_data, Year == 2005)
View(data_2005)

# Exploratory analysis
plot(density(data_2005$Access_Improved_Sanitation, kernel = "gaussian"))
plot(density(data_2005$Rural_Population, kernel = "gaussian"))
plot(density(data_2005$ICT_Infrastructure, kernel = "gaussian"))

ggplot(data_2005, aes(x=ICT_Infrastructure, y=Access_Improved_Sanitation)) + geom_point()
ggplot(data_2005, aes(x=Rural_Population, y=Access_Improved_Sanitation)) + geom_point()
ggplot(data_2005, aes(x=Age_Dependency_Ratio, y=Access_Improved_Sanitation)) + geom_point()
ggplot(data_2005, aes(x=ICT_Infrastructure, y=Access_Improved_Sanitation)) + geom_point()

model_2005 <- lm(Access_Improved_Sanitation ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
                   Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
                   Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = data_2005)
summary(model_2005)
plot(model_2005)

# log transformation: bad Normal
model_2005_log <- lm(log(Access_Improved_Sanitation) ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
                   Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
                   Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = data_2005)
summary(model_2005_log)
plot(model_2005_log)

# sqrt transformation
model_2005_sqrt <- lm(sqrt(Access_Improved_Sanitation) ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
                       Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
                       Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = data_2005)
summary(model_2005_sqrt)
plot(model_2005_sqrt)

# square transformation
model_2005_sqr <- lm((Access_Improved_Sanitation)^2 ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
                        Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
                        Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = data_2005)
summary(model_2005_sqr)
plot(model_2005_sqr)

# square transformation
model_2005_sqr <- lm((Access_Improved_Sanitation)^2 ~ (Food_Import_Dependency) + Rural_Population + log(ICT_Infrastructure) + 
                       log(Education) + (Dependency_External_Health_Services) + (Water_Dependency_Ratio) + (Political_Stability) + 
                       log(Doing_Business) + log(Age_Dependency_Ratio) + (Ecological_Footprint), data = data_2005)
summary(model_2005_sqr)
plot(model_2005_sqr)

##################################################################### 2010
data_2010 <- subset(cleaned_data, Year == 2010)
View(data_2010)

model_2010 <- lm((Access_Improved_Sanitation) ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
                   Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
                   Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = data_2010)
summary(model_2010)
plot(model_2010)

# square transformation
model_2010_sqr <- lm((Access_Improved_Sanitation)^2 ~ (Food_Import_Dependency) + Rural_Population + log(ICT_Infrastructure) + 
                       log(Education) + (Dependency_External_Health_Services) + (Water_Dependency_Ratio) + (Political_Stability) + 
                       log(Doing_Business) + log(Age_Dependency_Ratio) + (Ecological_Footprint), data = data_2010)
summary(model_2010_sqr)
plot(model_2010_sqr)

# cube transformation
model_2010_cube <- lm((Access_Improved_Sanitation)^3 ~ (Food_Import_Dependency) + Rural_Population + (ICT_Infrastructure) + 
                        (Education) + (Dependency_External_Health_Services) + (Water_Dependency_Ratio) + (Political_Stability) + 
                        (Doing_Business) + (Age_Dependency_Ratio) + (Ecological_Footprint), data = data_2010)
summary(model_2010_cube)
plot(model_2010_cube)

# cube log transformation
model_2010_cube_log <- lm((Access_Improved_Sanitation)^3 ~ (Food_Import_Dependency) + Rural_Population + log(ICT_Infrastructure) + 
                       log(Education) + (Dependency_External_Health_Services) + (Water_Dependency_Ratio) + (Political_Stability) + 
                       log(Doing_Business) + log(Age_Dependency_Ratio) + (Ecological_Footprint), data = data_2010)
summary(model_2010_cube_log)
plot(model_2010_cube_log)

##################################################################### 2015
data_2015 <- subset(cleaned_data, Year == 2015)
View(data_2015)

model_2015 <- lm((Access_Improved_Sanitation) ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
                   Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
                   Doing_Business + Age_Dependency_Ratio + (Ecological_Footprint)^2, data = data_2015)
summary(model_2015)
plot(model_2015)

# square transformation
model_2015_sqr <- lm((Access_Improved_Sanitation)^2 ~ (Food_Import_Dependency) + Rural_Population + log(ICT_Infrastructure) + 
                       log(Education) + (Dependency_External_Health_Services) + (Water_Dependency_Ratio) + (Political_Stability) + 
                       log(Doing_Business) + log(Age_Dependency_Ratio) + (Ecological_Footprint), data = data_2015)
summary(model_2015_sqr)
plot(model_2015_sqr)

# cube log transformation
model_2015_cube_log <- lm((Access_Improved_Sanitation)^3 ~ (Food_Import_Dependency) + Rural_Population + log(ICT_Infrastructure) + 
                            log(Education) + (Dependency_External_Health_Services) + (Water_Dependency_Ratio) + (Political_Stability) + 
                            log(Doing_Business) + log(Age_Dependency_Ratio) + (Ecological_Footprint), data = data_2015)
summary(model_2015_cube_log)
plot(model_2015_cube_log)


#####################################################################
# # Exploratory analysis
# plot(density(panel_data$Access_Improved_Sanitation, kernel = "gaussian"))
# plot(density(panel_data$Rural_Population, kernel = "gaussian"))
# plot(density(panel_data$ICT_Infrastructure, kernel = "gaussian"))
# 
# ggplot(panel_data, aes(x=ICT_Infrastructure, y=Access_Improved_Sanitation)) + geom_point()
# ggplot(panel_data, aes(x=Rural_Population, y=Access_Improved_Sanitation)) + geom_point()
# ggplot(panel_data, aes(x=Age_Dependency_Ratio, y=Access_Improved_Sanitation)) + geom_point()
# ggplot(panel_data, aes(x=ICT_Infrastructure, y=Access_Improved_Sanitation)) + geom_point()
# 
# summary(unique_data$ICT_Infrastructure)
# summary(unique_data$Education)
# summary(unique_data$Political_Stability)
# summary(unique_data$Doing_Business)
# 
# ######## Variable selection: 
# # Checking correlation coefficients
# x_y_variable <- subset(unique_data, select=c(Food_Import_Dependency, Rural_Population, ICT_Infrastructure,
#                                            Education, Dependency_External_Health_Services, Water_Dependency_Ratio, 
#                                            Political_Stability, Doing_Business, Age_Dependency_Ratio, Ecological_Footprint, 
#                                            Access_Improved_Sanitation))
# cor(x_y_variable) # none of the variables had |correlation| > 0.8 
# 
# ######################################################## Linear Models 
# ######## OLS
# 
# 
# lm_model <- lm(Access_Improved_Sanitation ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
#                  Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
#                  Doing_Business + Age_Dependency_Ratio + Ecological_Footprint + Year, data = data_2005)
# summary(lm_model)
# plot(lm_model)
# 
# ######## Country Fixed Effects
# panel_data <- pdata.frame(unique_data, index=c("Country","Year"), drop.index=TRUE, row.names=TRUE)
# # model
# within_model <- plm(Access_Improved_Sanitation ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
#                       Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
#                       Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = panel_data, model = "within",
#                     effect = 'individual', index = c('Country'))
# summary(within_model)
# 
# ######## Country and Time Fixed Effects
# # model
# twoway_model <- plm(Access_Improved_Sanitation ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
#                       Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability +
#                       Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = panel_data, 
#                     model = "within", effect = "twoways", index = c('Country', 'Year'))
# summary(twoway_model)
# 
# ######## First Differences (across countries)
# # model
# fd_model <- plm(Access_Improved_Sanitation ~ Food_Import_Dependency + Rural_Population + ICT_Infrastructure + 
#                       Education + Dependency_External_Health_Services + Water_Dependency_Ratio + Political_Stability + 
#                       Doing_Business + Age_Dependency_Ratio + Ecological_Footprint, data = panel_data, model = "fd")
# summary(fd_model)
# 
# ######## Robustness Checks
# ## normality
# qqnorm(residuals(within_model), ylab = 'Residuals')
# qqline(residuals(within_model))
# 
# ## constant variance
# fitted.values <- as.numeric(panel_data$Access_Improved_Sanitation - residuals(within_model))
# plot(fitted.values, within_model$residuals)
# 
# ## outliers
# x_variable <- subset(unique_data, select=c(Food_Import_Dependency, Rural_Population, ICT_Infrastructure,
#                                            Education, Dependency_External_Health_Services, Water_Dependency_Ratio, 
#                                            Political_Stability, Doing_Business, Age_Dependency_Ratio))
# 
# X <- as.matrix(x_variable)
# P = X %*% solve(t(X) %*% X) %*% t(X)
# halfnorm(diag(P), labs = 1:1969, ylab = 'Leverages', nlab = 1)
# 
# ######################################################## GMM
# install.packages("mixtools")
# library("mixtools")
# 
# kernlab, MASS, segmented, stats, survival
# 
# plot_mix_comps <- function(x, mu, sigma, lam) {
#   lam * dnorm(x, mu, sigma)
# }
# 
# set.seed(1)
# sanitation <- panel_data$Access_Improved_Sanitation
# mixmdl <- normalmixEM(sanitation, k = 2)
# 
# data.frame(x = mixmdl$x) %>%
#   ggplot() +
#   geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
#                  fill = "white") +
#   stat_function(geom = "line", fun = plot_mix_comps,
#                 args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
#                 colour = "red", lwd = 1.5) +
#   stat_function(geom = "line", fun = plot_mix_comps,
#                 args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
#                 colour = "blue", lwd = 1.5) +
#   ylab("Density")
# 
# mixmdl$mu
# 
# mixmdl$sigma
# 
# post.df <- as.data.frame(cbind(x = mixmdl$x, mixmdl$posterior))
# 
# plot(density(panel_data$Access_Improved_Sanitation))
# plot(density(panel_data$Doing_Business))
# plot(density(panel_data$Political_Stability))
