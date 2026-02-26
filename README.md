# GLMM_for_IRR
## Generalized Linear Mixed Models for determining Incidence Risk Ratio for household associated factors leading to increase or decrease of mosquito density indoor


```CDCLTF<-read.csv("D:/Msc Research Docs/Thesis/CDC-LT Data/CDCLT-FULLDATA.csv")
View(CDCLTF)
names(CDCLTF)
install.packages("Hmisc")
install.packages("psych")
library(Hmisc)
library(psych)
library(ggplot2)
library(sjPlot)
library(lme4)
library(ggpubr)
library(sjlabelled)
library(sjmisc)
library(janitor)
describe(CDCLTF)


CDCLTF$todays_date.1<-as.Date(CDCLTF$todays_date.1, format = "%d/%m/%Y")
str(CDCLTF$todays_date.1)
View(CDCLTF)

##CDCLTF$num_members<-factor(CDCLTF$num_members)
CDCLTF$sleep_net<-factor(CDCLTF$sleep_net)
CDCLTF$geo_cluster_num<-factor(CDCLTF$geo_cluster_num)
CDCLTF$hhid.1<-factor(CDCLTF$hhid.1)
CDCLTF$todays_date.1<-factor(CDCLTF$todays_date.1)
CDCLTF$sleep_net<-factor(CDCLTF$sleep_net)
##CDCLTF$num_nets<-factor(CDCLTF$num_nets)
CDCLTF$animals_indoors<-factor(CDCLTF$animals_indoors)
CDCLTF$wall<-factor(CDCLTF$wall)
CDCLTF$roof<-factor(CDCLTF$roof)
CDCLTF$eaves<-factor(CDCLTF$eaves)
#CDCLTF$windows<-factor(CDCLTF$windows)

str(CDCLTF)

## ******************Anopheles gambiae descriptives*******************##

###people<-ggplot(CDCLTF, aes(sum_gambiae, fill = num_members)) + 
  geom_histogram(binwidth = 1) + facet_grid(num_members ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
#people

sleep_net<-ggplot(CDCLTF, aes(sum_gambiae, fill = sleep_net)) + 
  geom_histogram(binwidth = 1) + facet_grid(sleep_net ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
sleep_net

##num_nets<-ggplot(CDCLTF, aes(sum_gambiae, fill = num_nets)) + 
  geom_histogram(binwidth = 1) + facet_grid(num_nets ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
##num_nets


animals_indoors<-ggplot(CDCLTF, aes(sum_gambiae, fill = animals_indoors)) + 
  geom_histogram(binwidth = 1) + facet_grid(animals_indoors ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
animals_indoors

wall<-ggplot(CDCLTF, aes(sum_gambiae, fill = wall)) + 
  geom_histogram(binwidth = 1) + facet_grid(wall ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
wall

roof<-ggplot(CDCLTF, aes(sum_gambiae, fill = roof)) + 
  geom_histogram(binwidth = 1) + facet_grid(roof ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
roof

eaves<-ggplot(CDCLTF, aes(sum_gambiae, fill = eaves)) + 
  geom_histogram(binwidth = 1) + facet_grid(eaves ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
eaves

eaves<-ggplot(CDCLTF, aes(sum_gambiae, fill = eaves)) + 
  geom_histogram(binwidth = 1) + facet_grid(eaves ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
eaves


##windows<-ggplot(CDCLTF, aes(sum_gambiae, fill = windows)) + 
  geom_histogram(binwidth = 1) + facet_grid(windows ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
windows


##num_rooms<-ggplot(CDCLTF, aes(sum_gambiae, fill = num_rooms)) + 
  geom_histogram(binwidth = 1) + facet_grid(num_rooms ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
num_rooms

## ******************Anopheles funestus descriptives*******************##

##peoplef<-ggplot(CDCLTF, aes(sum_funestus, fill = num_members)) + 
  geom_histogram(binwidth = 1) + facet_grid(num_members ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
##peoplef

sleep_netf<-ggplot(CDCLTF, aes(sum_funestus, fill = sleep_net)) + 
  geom_histogram(binwidth = 1) + facet_grid(sleep_net ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
sleep_netf

##num_netsf<-ggplot(CDCLTF, aes(sum_funestus, fill = num_nets)) + 
  geom_histogram(binwidth = 1) + facet_grid(num_nets ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
##num_netsf


animals_indoorsf<-ggplot(CDCLTF, aes(sum_funestus, fill = animals_indoors)) + 
  geom_histogram(binwidth = 1) + facet_grid(animals_indoors ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
animals_indoorsf

wallf<-ggplot(CDCLTF, aes(sum_funestus, fill = wall)) + 
  geom_histogram(binwidth = 1) + facet_grid(wall ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
wallf

rooff<-ggplot(CDCLTF, aes(sum_funestus, fill = roof)) + 
  geom_histogram(binwidth = 1) + facet_grid(roof ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
rooff

eavesf<-ggplot(CDCLTF, aes(sum_funestus, fill = eaves)) + 
  geom_histogram(binwidth = 1) + facet_grid(eaves ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
eavesf

###windowsf<-ggplot(CDCLTF, aes(sum_funestus, fill = windows)) + 
  geom_histogram(binwidth = 1) + facet_grid(windows ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
windowsf


##num_roomsf<-ggplot(CDCLTF, aes(sum_funestus, fill = num_rooms)) + 
  geom_histogram(binwidth = 1) + facet_grid(num_rooms ~ 
                                              ., margins = TRUE, 
                                            scales = "free")
num_roomsf

#########*******changing variable names*********************#####
CDCLTF$wall<-gsub(pattern = "blocks_cement mud","Mixed",CDCLTF$wall)
CDCLTF$wall<-gsub(pattern = "blocks_mud","Mixed",CDCLTF$wall)
CDCLTF$wall<-gsub(pattern = "mud palm_leaves","Mixed",CDCLTF$wall)
tabyl(CDCLTF$wall)

##CDCLTF$windows<-gsub(pattern = "mixed","screened",CDCLTF$windows)
##tabyl(CDCLTF$windows)



#CDCLTF$num_nets<-gsub(pattern = "1","Below_two",CDCLTF$num_nets)
#CDCLTF$num_nets<-gsub(pattern = "2","Below_two",CDCLTF$num_nets)
#CDCLTF$num_nets<-gsub(pattern = "3","Above_two",CDCLTF$num_nets)
#CDCLTF$num_nets<-gsub(pattern = "4","Above_two",CDCLTF$num_nets)
#CDCLTF$num_nets<-gsub(pattern = "5","Below_two",CDCLTF$num_nets)
#CDCLTF$num_nets<-gsub(pattern = "6","Below_two",CDCLTF$num_nets)
#tabyl(CDCLTF$num_nets)

#CDCLTF$num_members<-gsub(pattern = "1","Below_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "2","Below_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "3","Below_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "4","Below_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "5","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "6","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "7","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "8","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "9","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "10","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "11","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "13","Above_five",CDCLTF$num_members)

#CDCLTF$num_members<-gsub(pattern = "Below_five0","Above_five",CDCLTF$num_members)
#CDCLTF$num_members<-gsub(pattern = "Below_fiveBelow_five","Above_five",CDCLTF$num_members)

#CDCLTF$num_rooms<-gsub(pattern = "1","Below_five",CDCLTF$num_rooms)
#CDCLTF$num_rooms<-gsub(pattern = "2","Below_five",CDCLTF$num_rooms)
#CDCLTF$num_rooms<-gsub(pattern = "3","Below_five",CDCLTF$num_rooms)
#CDCLTF$num_rooms<-gsub(pattern = "4","Below_five",CDCLTF$num_rooms)
#CDCLTF$num_rooms<-gsub(pattern = "5","Above_five",CDCLTF$num_rooms)
#CDCLTF$num_rooms<-gsub(pattern = "6","Above_five",CDCLTF$num_rooms)
#CDCLTF$num_rooms<-gsub(pattern = "7","Above_five",CDCLTF$num_rooms)

#tabyl(CDCLTF$roof)
str(CDCLTF)
write.csv(CDCLTF, "CDCLTF_CLEANED_04122024.csv")
getwd()
#######********Setting reference category *****************#####
#CDCLTF$num_members <- relevel(as.factor(CDCLTF$num_members), ref = "Below_five")
CDCLTF$sleep_net <- relevel(as.factor(CDCLTF$sleep_net), ref = "yes")
##CDCLTF$windows <- relevel(as.factor(CDCLTF$window), ref = "no_window")

#####******How to Calculate the Sum by Group*****#####
aggregate(CDCLTF$sum_gambiae, list(CDCLTF$sleep_net), FUN=sum) 



#######******GLMM**********#############
library(lme4)
m1<-glmer.nb(sum_gambiae~sleep_net+animals_indoors
             +wall+roof+eaves+
                        (1|geo_cluster_num/todays_date.1/hhid.1),
                      data = CDCLTF)
md1<-glmer.nb(sum_funestus~sleep_net+animals_indoors
              +wall+roof+eaves+
                (1|geo_cluster_num/todays_date.1/hhid.1),
              data = CDCLTF)


summary(m1)
###plotting mixed models for An.gambiae s.l
sjPlot::plot_model(m1, show.reflvl = TRUE, 
                    p.style = "asterisk",
                    show.p = TRUE,
                    show.values = TRUE,       
                    transform = "exp",        
                    value.offset = 0.4,       
                    value.size = 5.0)

####plotting mixed models for An. funestus s.l
sjPlot::plot_model(md1, show.reflvl = TRUE, 
                    p.style = "asterisk",
                    show.p = TRUE,
                    show.values = TRUE,       
                    transform = "exp",        
                    value.offset = 0.4,       
                    value.size = 5.0)

#### plotting mixed models for Both Funestus and Gambiae
sjPlot::tab_model(m1,md1, show.reflvl = TRUE, show.ngroups = TRUE)
sjPlot::tab_model(m1,md1,
                  file = "Results_04122024.xls")


p1<- sjPlot::plot_models(m1,md1, show.reflvl = TRUE, 
                 p.style = "asterisk",
                 show.p = TRUE,
                 show.values = TRUE,       # Show IRR values
                 transform = "exp",        # Exponentiate coefficients to get IRRs
                 value.offset = 0.5,       # Adjust label position
                 value.size = 3.5)          # Adjust label size

sjPlot::plot_model(md1, show.reflvl = TRUE, 
                         p.style = "asterisk",
                         show.p = TRUE,
                         show.values = TRUE,       # Show IRR values
                         transform = "exp",        # Exponentiate coefficients to get IRRs
                         value.offset = 0.5,       # Adjust label position
                         value.size = 3.5) 
sjPlot::plot_models(m1,md1, show.values = TRUE,
                    value.offset = 0.6,       # Push labels further from points
                    value.size = 3.2,         # Slightly reduce font size
                    transform = "exp",        # If you're plotting IRRs
                    show.p = TRUE,            # Show significance asterisks
                    p.style = "asterisk",     # Use asterisk style for p-values
                    show.reflvl = TRUE)       # Show reference levels

                    
tabyl(CDCLTF$windows)
sjPlot::plot_models(
  m1, md1,
  show.values = TRUE,       # Show IRR or coefficient values
  value.offset = 0.5,       # Push labels away from points
  value.size = 3.5,         # Adjust font size for clarity
  transform = "exp",        # If you're plotting IRRs
  show.p = TRUE,            # Show significance asterisks
  p.style = "asterisk",     # Use asterisk style for p-values
  show.reflvl = TRUE        # Show reference levels
)```
