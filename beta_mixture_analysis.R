
cleaned_data <- read_csv("Desktop/Semester 8/MAT Probability Statistics 2/Project/cleaned_data.csv")
unique_data <- cleaned_data[!duplicated(cleaned_data[c('Country', 'Year')]),] 
panel_data <- plm::pdata.frame(unique_data, index=c("Country","Year"), drop.index=TRUE, row.names=TRUE)
sanitation <- panel_data$Access_Improved_Sanitation

#beta fitting
beta_mixture_dens <- function(x, mixture_param, alpha_1, beta_1, alpha_2, beta_2) {
  first_comp = mixture_param*dbeta(x, alpha_1, beta_1)
  second_comp = (1-mixture_param)*dbeta(x, alpha_2, beta_2)
  return(log(first_comp + second_comp))
}

likelihood <- function(theta, x) {
  if (any(theta <= 0)) {
    NA
  } else {
    beta_mixture_dens(x, theta[1], theta[2], theta[3], theta[4], theta[5])
  }
}

m1 <- maxLik::maxLik(likelihood, start=c(mixture_param = 0.5, alpha_1 = 2, beta_1 = 2, alpha_2 = 2, beta_2 = 2), x = (sanitation-0.1)/100)
x = seq(0.001, 1, by = 0.001)
y = exp(beta_mixture_dens(x, m1$estimate[1], m1$estimate[2], m1$estimate[3], m1$estimate[4], m1$estimate[5]))

plot(density(sanitation/100, kernel = 'gaussian'))
lines(x, y, col = 'red')
