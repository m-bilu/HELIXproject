data2<-load("exposome_NA.RData")


codebook<-codebook
exposome<-exposomeNA

lifestyles_df<-codebook[codebook$domain=='Lifestyles',]
postnatal_lifestyle<-lifestyles_df[lifestyles_df$period=='Postnatal',]
pregnancy_lifestyle<-lifestyles_df[lifestyles_df$period=='Pregnancy',]
df_pregnancy_lifestyle<-pregnancy_lifestyle
#pregnancy
#how many different subfamilies are there->12
losub<-c(pregnancy_lifestyle$subfamily)
length(losub)
#which subfamily do we have the most data on
df1<-data.frame(losub)
names(which.max(table(df1$losub)))
#DIET most number of data->8


#Some sort of relationship between the diet cov and the outcome
#df with just diet

df_diet<-df_pregnancy_lifestyle[!(df_pregnancy_lifestyle$subfamily=="Physical activity" |
                           df_pregnancy_lifestyle$subfamily=="Folic acid consumption"
                           |df_pregnancy_lifestyle$subfamily=="Prenatal Alcohol" 
                         ),]


lov_diet<-c(df_diet$variable_name)

#contains values of cov that are under the subfamily of diet
diet_data<-subset(exposome,select = lov_diet)

#postnatal and prenatal behavior of the mother and how it affects the 
#raven score

     
     