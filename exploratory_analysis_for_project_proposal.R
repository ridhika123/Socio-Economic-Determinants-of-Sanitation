## Code for Initial Proposal
sanitation_score_nona <- drop_na(sanitation_score)
par(mfrow=c(1,3))
plot(density(sanitation_score_nona$`1995`, kernel = "gaussian"), main = "Sanitation Score 1995")
plot(density(sanitation_score_nona$`2000`, kernel = "gaussian"))
plot(density(sanitation_score_nona$`2017`, kernel = "gaussian"))

slum_pop <- drop_na(slum_pop_score)
plot(density(slum_pop$`1995`, kernel = "gaussian"))
plot(density(slum_pop$`2000`, kernel = "gaussian"))
plot(density(slum_pop$`2017`, kernel = "gaussian"))

doing_business <- drop_na(doing_business_score)
plot(density(doing_business$`1995`, kernel = "gaussian"), main = "Doing Business 1995")
plot(density(doing_business$`2000`, kernel = "gaussian"))
plot(density(doing_business$`2017`, kernel = "gaussian"))

political_stability <- drop_na(political_stability_score)
plot(density(political_stability$`1995`, kernel = "gaussian"), main = "Political Stability 1995")
plot(density(political_stability$`2000`, kernel = "gaussian"))
plot(density(political_stability$`2017`, kernel = "gaussian"))

social_inequality <- drop_na(social_inequality_score)
plot(density(social_inequality$`1995`, kernel = "gaussian"))
plot(density(social_inequality$`2000`, kernel = "gaussian"))
plot(density(social_inequality$`2017`, kernel = "gaussian"))