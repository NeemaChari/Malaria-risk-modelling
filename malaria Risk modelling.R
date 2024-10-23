#install.packages("readxl")

#Reading excel file
library(readxl)
malariaPR <- read_excel("C:/Users/user/Documents/Malaria4/malariaPR.xlsx")
View(malariaPR)

library(dplyr)
#removing duplicated rows
malariaPR<-unique(malariaPR)
print(colnames(malariaPR))

#Renaming variables
Malaria <- malariaPR %>%
  rename(Region = hv024,Age =hc1, RDTresult = hml35,
         Microscopy = hml32, ITN = hml10, LLN = hml20, IRS = sh119a)


#Creating dataframes using the select function
malariaPR1<-select(Malaria,Region,shcounty,shzone,Age,Microscopy,RDTresult,ITN,LLN,IRS)

#Omitting NAs in the data
clean_data <- na.omit(malariaPR1)


# Replace "60+" with a numeric value representing the upper limit of the age range
clean_data  <- clean_data  %>%
  mutate(Age = ifelse(Age == "60+", 60, Age))

# Convert the Age column to numeric
clean_data$Age <- as.numeric(clean_data$Age)


# Filtering the dataframe to include only children aged between 6 to 59 months and tested for malaria
filtered_df <- filter(clean_data, Age >= 6 & Age <= 59, !is.na(Microscopy) | !is.na(RDTresult))
View(filtered_df)


#Creating a df that shows sums of children tested +ve and those tested in both RDT and Microscopy and the total use of interventions
malaria_sums<- filtered_df %>%
  group_by(Region,shzone, shcounty) %>%
  summarize(
    rdt_positive = sum(RDTresult == "positive"),
    rdt_tested = sum(!is.na(RDTresult)),
    micro_positive = sum(Microscopy == "positive"),
    micro_tested = sum(!is.na(Microscopy)),
    Use_ITN = sum(ITN == "yes"),
    total_ITN = sum(!is.na(ITN)),
    Use_LLN = sum(LLN == "yes"),
    total_LLN = sum(!is.na(LLN)),
    Use_IRS = sum(IRS == "yes"),
    total_IRS = sum(!is.na(IRS))
  )


#getting the proportion of the use of each malaria control interventions 
malaria_data1 <- malaria_sums %>%
  mutate(
    ITN_prop = (Use_ITN/total_ITN),
    LLN_prop = (Use_LLN/total_LLN),
    IRS_prop = (Use_IRS/total_IRS),
    
  )

#renaming the column 'old_column' to 'new_column'
malaria_data1 <- malaria_data1 %>%
  rename(county = shcounty)


# Mutate the columns so that we could merge using the shapefile
malaria_data1 <- malaria_data1 %>%
  mutate(ID_2 = case_when(
    county == "kiambu" ~ 1,
    county == "kirinyag" ~ 2,
    county == "machakos" ~ 3,
    county == "muranga" ~ 4,
    county == "nyandaru" ~ 5,
    county == "nyeri" ~ 6,
    county == "kilifi" ~ 7,
    county == "kwale" ~ 8,
    county == "lamu" ~ 9,
    county == "mombasa" ~ 10,
    county == "taita ta" ~ 11,
    county == "tana riv" ~ 12,
    county == "embu" ~ 13,
    county == "isiolo" ~ 14,
    county == "kitui" ~ 15,
    county == "machakos" ~ 16,
    county == "makueni" ~ 17,
    county == "marsabit" ~ 18,
    county == "meru" ~ 19,
    county == "tharaka" ~ 20,
    county == "nairobi" ~ 22,
    county == "garissa" ~ 23,
    county == "mandera" ~ 24,
    county == "wajir" ~ 25,
    county == "homa bay" ~ 26,
    county == "kisii" ~ 27,
    county == "kisumu" ~ 28,
    county == "migori" ~ 29,
    county == "nyamira" ~ 30,
    county == "siaya" ~ 31,
    county == "baringo" ~ 32,
    county == "bomet" ~ 33,
    county == "elgeyo m" ~ 34,
    county == "kericho" ~ 35,
    county == "laikipia" ~ 36,
    county == "nakuru" ~ 37,
    county == "nandi" ~ 38,
    county == "narok" ~ 39,
    county == "samburu" ~ 40,
    county == "trans-nz" ~ 41,
    county == "turkana" ~ 42,
    county == "uasin gi" ~ 43,
    county == "west pok" ~ 44,
    county == "bungoma" ~ 45,
    county == "busia" ~ 46,
    county == "kakamega" ~ 47,
    county == "vihiga" ~ 48,
    TRUE ~ NA_integer_  # If none of the conditions match, return NA
  ))

#install.packages("sf")
library(sf)
#Read the shapefile
shpfile<-st_read("C:/Users/user/Desktop/shapefiles/KEN_adm2/KEN_adm2.shp")


#merging the malaria data to shapefile
library(tidyverse)
merged_datax<-inner_join(shpfile,malaria_data1,relationship ="many-to-many" ,by= c("ID_2"="ID_2"))

#visualization
library(ggplot2)
#Mapping Malaria cases identified through RDT testing in each county.
ggplot() +
  geom_sf(data = merged_datax, aes(fill = rdt_positive)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") +  # Using a red color gradient
  theme_minimal()+
  ggtitle("Malaria Cases Distribution using RDT")
 

#Mapping malaria cases using microscopy result
ggplot() +
  geom_sf(data = merged_datax, aes(fill = micro_positive)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") +  # Using a red color gradient
  theme_minimal()+
  ggtitle("Malaria Cases Distribution using Microscopy")


#getting expected cases from the observed cases RDT test using spatialepi package
library(SpatialEpi)  
expected_cases <-expected(population = merged_datax$rdt_tested,cases =merged_datax$rdt_positive,n.strata = 1 )
merged_datax$expected_cases<-expected_cases


#Expected cases using Microscopy test
expected_Micro <-expected(population = merged_datax$micro_tested,cases =merged_datax$micro_positive,n.strata = 1 )
merged_datax$expected_Micro<-expected_Micro


# obtaining the neighbourhood matrix using spdep package
library(spdep)
nb<-poly2nb(merged_datax)
head(nb)

# Convert neighbors list to a file compatible with R-INLA
library(INLA)
nb2INLA("merged_datax.adj",nb)

# Read the file using inla.read.graph() and store it in the object g
g<-inla.read.graph(filename = "merged_datax.adj")


#modeling random effects
re_u<-1:nrow(merged_datax)
re_v<-1:nrow(merged_datax)


#Model 1 using RDT test as the response variable without interventions
Model1 <- rdt_positive ~ f(re_u, model="besag", graph=g, scale.model=TRUE) + f(re_v, model="iid")
result<-inla(Model1,family = "poisson",data = merged_datax,E = expected_cases,control.predictor = list(compute = TRUE)
)
summary(result)
head(result$summary.fitted.values)

#add results to merged_datax
merged_datax$RRwithout<-result$summary.fitted.values[,"mean"]
merged_datax$LLwithout<-result$summary.fitted.values[,"0.025quant"]
merged_datax$ULwithout<-result$summary.fitted.values[,"0.975quant"]

#mapping malaria disease risk without interventions
ggplot() +
  geom_sf(data = merged_datax, aes(fill = RRwithout)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") +  # Using a red color gradient
  theme_minimal()+
  ggtitle("Malaria Disease Risk map RDT without interventions")

#Model 2 incorporating interventions
formula2 <- rdt_positive ~ ITN_prop + LLN_prop + IRS_prop + f(re_u, model="besag", graph=g, scale.model=TRUE) + f(re_v, model="iid")
results<-inla(formula2,family = "poisson",data = merged_datax,E = expected_cases,control.predictor = list(compute = TRUE)
)
summary(results)
head(results$summary.fitted.values)

#plot the posterior distribution of the ITN coefficient 
library(ggplot2)

#calculate smoothing of marginal distribution of the coefficient using inla.s()
marginal<-inla.smarginal(results$marginals.fixed$ITN_prop)
marginal<-data.frame(marginal)

#using ggplot
ggplot(marginal,aes(x = x,y = y,)) + geom_line() + labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


#plotting the posterior distribution of LLN coefficient
marginal<-inla.smarginal(results$marginals.fixed$LLN_prop)
marginal<-data.frame(marginal)
ggplot(marginal,aes(x = x,y = y,)) + geom_line() + labs(x = expression(beta[2]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()

#ploting the posterior distribution of IRS coefficient
marginal<-inla.smarginal(results$marginals.fixed$IRS_prop)
marginal<-data.frame(marginal)
ggplot(marginal,aes(x = x,y = y,)) + geom_line() + labs(x = expression(beta[3]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()

#add results to merged_datax
merged_datax$RR<-results$summary.fitted.values[,"mean"]
merged_datax$LL<-results$summary.fitted.values[,"0.025quant"]
merged_datax$UL<-results$summary.fitted.values[,"0.975quant"]

#mapping malaria disease risk
ggplot() +
  geom_sf(data = merged_datax, aes(fill = RR)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") +  # Using a red color gradient
  theme_minimal()+
  ggtitle("Malaria Disease Risk map (RDT)")

#Model 3 Microscopy without interventions
Model3 <- micro_positive ~ f(re_u, model="besag", graph=g, scale.model=TRUE) + f(re_v, model="iid")
res<-inla(Model1,family = "poisson",data = merged_datax,E = expected_Micro,control.predictor = list(compute = TRUE)
)
summary(res)
head(res$summary.fitted.values)

#add results to merged_datax
merged_datax$RRMicro<-res$summary.fitted.values[,"mean"]
merged_datax$LLMicro<-res$summary.fitted.values[,"0.025quant"]
merged_datax$ULMicro<-res$summary.fitted.values[,"0.975quant"]

#mapping malaria disease risk without intervetions
ggplot() +
  geom_sf(data = merged_datax, aes(fill = RRMicro)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") +  # Using a red color gradient
  theme_minimal()+
  ggtitle("Malaria Disease Risk map (Micro) without interventions")

#Model 4 Microscopy with interventions
formula3 <- micro_positive ~ ITN_prop + LLN_prop + IRS_prop + f(re_u, model="besag", graph=g, scale.model=TRUE) + f(re_v, model="iid")
results3<-inla(formula3,family = "poisson",data = merged_datax,E = expected_Micro,control.predictor = list(compute = TRUE)
)
summary(results3)
head(results3$summary.fitted.values)

#plotting the posterior distribution of ITN coefficient
marginal<-inla.smarginal(results3$marginals.fixed$ITN_prop)
marginal<-data.frame(marginal)
ggplot(marginal,aes(x = x,y = y,)) + geom_line() + labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()

#plotting the posterior distribution of LLN coefficient
marginal<-inla.smarginal(results3$marginals.fixed$LLN_prop)
marginal<-data.frame(marginal)
ggplot(marginal,aes(x = x,y = y,)) + geom_line() + labs(x = expression(beta[2]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


#plotting the posterior distribution of IRS coefficient
marginal<-inla.smarginal(results3$marginals.fixed$IRS_prop)
marginal<-data.frame(marginal)
ggplot(marginal,aes(x = x,y = y,)) + geom_line() + labs(x = expression(beta[3]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()

#add results to merged_datax
merged_datax$RR_Micro<-results3$summary.fitted.values[,"mean"]
merged_datax$LL_Micro<-results3$summary.fitted.values[,"0.025quant"]
merged_datax$UL_Micro<-results3$summary.fitted.values[,"0.975quant"]

#mapping malaria disease risk
ggplot() +
  geom_sf(data = merged_datax, aes(fill = RR_Micro)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") +  # Using a red color gradient
  theme_minimal()+
  ggtitle("Malaria Disease Risk map (Microscopy)")
