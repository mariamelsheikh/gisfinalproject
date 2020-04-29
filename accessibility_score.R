#PACKAGES
library(tidyverse)
library(sf)

#Data
healthcenters <- read_sf("healthcenters.shp")
sociodemo <- read_sf("sociodemo.shp")


names(sociodemo) <- c("Shape Area","Type","Dwellings","Households","GeoUID","Population",
                      "Adjusted Population (previous Census)","PR_UID","CMA_UID","CSD_UID",
                      "CD_UID","province_code","cms_code","census_tract","population",
                      "population_densitykm2","average_age","average household size","knowledge_eng",
                      "knowledge_french","kowledge_both","knowledge_neither","first_english","first_french",
                      "first_both","first_neither","median income of households in 2015 ($)",
                      "median after-tax income of households in 2015","number household in 2015",
                      "under 5000","5000 to 9999","10000 to 14999","15000 to 19999",
                      "20000 to 24999","25000 to 29999","30000 to 34999","35000 to 39999",
                      "40000 to 44999","45000 to 49999","50000 to 59999","60000 to 69999",
                      "0000 to 79999","80000 to 89999","90000 to 99999","100000 and over",
                      "average after-tax income of households in 2015","low income LIM-AT", 
                      "prevalence of low income LIM-AT","low income LICO-AT","prevalence LICO-AT",
                      "knowledge of languages for the population in private households",
                      "knowledge_official language_private households",
                      "knowledge_eng_private households","knowledge_french_private households", 
                      "knowledge_non-official language_private households",
                      "Total - Citizenship for the population in private households",
                      "canadian citizens","Not Canadian citizens", 
                      "Total - Immigrant status for the population in private households",
                      "non-immigrants", "immigrants", "Non-permanent residents", "Immigrants_2011 to 2016", 
                      "Immigrants_2006 to 2010", "Immigrants_2001 to 2005", "Immigrants_2001 to 2010", 
                      "Immigrants_1991 to 2000", "Immigrants_1981 to 1990", "Immigrants_Before 1981", 
                      "Total - Generation status for the population in private households", 
                      "First generation", "Second generation", "Third generation or more", 
                      "Admission category for the immigrant population in private households", 
                      "Economic immigrants", "Immigrants sponsored by family", "Refugees", 
                      "other immigrants", "Total - Aboriginal identity for the population in private households", 
                      "Non-Aboriginal identity", "Aboriginal identity", 
                      "Total - Visible minority for the population in private households", 
                      "Total visible minority population","Not a visible minority",
                      "Total - Private households by tenure",
                      "Total - diploma or degree for the population aged 15 years and over in private households",
                      "No certificate, diploma or degree", "Secondary", "Postsecondary", "Employment rate",
                      " Unemployment rate", "Total - Population aged 15 years and over by Labour force status", 
                      "In the labour force", "Not in the labour force","geometry")

########### 2SFCA
#first step, the 2SFCA places a buffer, or catchment, around a point of 
#health care supply and calculates a provider-to-population ratio within it
hc_buf <- st_buffer(healthcenters,3000)
hc_buf <- hc_buf %>% mutate(hc_id = c(1:nrow(hc_buf)))
ct_centroid <- st_centroid(sociodemo)
supply <- st_join(hc_buf,ct_centroid, left = F) %>% 
  group_by(hc_id) %>% 
  summarize(pop = sum(Population),
            ppr_s = 1/pop)

# second step, it places a second buffer around a point of population demand 
# and sums the ratios from all provider points within that second buffer

ct_cent_buf <- st_buffer(ct_centroid,3000)

demand <- st_join(ct_cent_buf, supply, left = F) %>% 
  group_by(GeoUID) %>% 
  summarize(sum_ratio = sum(ppr_s),
            sum_ratio_1000 = sum_ratio*1000)

#The third step of our 3SFCA method involves generating an access ratio 
#at the neighbourhood level by averaging the 2SFCA access ratios for 
#all CTs falling within a neighbourhood. In this research CTs included in 
#a neighbourhood’s access ratio are those with centroids falling 
#within a neighbourhood’s boundaries

access_ratio_nbh <- st_join(nbh_to,demand, left = F) %>% 
  group_by(id) %>% 
  summarize(n_ct = length(GeoUID),
            avg_ratio = sum(sum_ratio)/n_ct,
            avg_ratio_1000 = avg_ratio*1000)