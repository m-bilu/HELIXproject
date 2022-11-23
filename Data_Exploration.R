load("C:/Users/Onion/Downloads/exposome_NA.RData")

covariatesNA
codebook

phenotypeNA
exposomeNA
posts = codebook$variable_name[which(codebook$period == "Postnatal" & codebook$family == "Lifestyle")]
pregs = codebook$variable_name[which(codebook$period == "Pregnancy" & codebook$family == "Lifestyle")]
lifestyle = codebook$variable_name[which(codebook$family == "Lifestyle")]


n = length(posts)

for (i in 1:n) {
  posts[i] = toString(posts[i])
}

# creating a lifestyle codebook
exposomeNA[,posts]
lifestyle_mom <- intersect(lifestyle, colnames(exposomeNA))
lifestyle_mom
lifestyle_data <- exposomeNA[,lifestyle_mom]
lifestyle_codebook <- codebook[lifestyle_mom,]

# found that no variables in lifestyle exist in cov_data
cov_data <- intersect(lifestyle, colnames(covariatesNA))
cov_data
cov_codebook <- codebook[cov_data,]


# trying to fit all the lifestyle variables to raven score
lifestyle_data$hs_correct_raven <- phenotypeNA$hs_correct_raven
lifestyle_fit <- lm(data = lifestyle_data, formula = hs_correct_raven ~ .)

# investigating interval type data
# identifying data type and finding the range of different values
typeof(lifestyle_data$h_cereal_preg_Ter)
plot(lifestyle_data$h_cereal_preg_Ter)
plot(lifestyle_data$h_dairy_preg_Ter)
plot(lifestyle_data$h_meat_preg_Ter)
plot(lifestyle_data$hs_beverages_Ter)

summary(lifestyle_fit)
