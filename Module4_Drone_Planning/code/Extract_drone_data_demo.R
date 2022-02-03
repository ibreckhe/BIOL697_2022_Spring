##Script to extract crown temperatures from UAS data.

##Loads required packages.
library(sf)
library(raster)
library(exactextractr)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(readr)

##Set working directory.
setwd("~/code/BIOL697_2022_Spring/Module4_Drone_Planning/")

##Loads data.
crown_poly <- st_read("./data/ResearchMeadow_Sapflux_Crown_Polygons_2021_12_03.gpkg")
crown_poly$area_sqm <- st_area(crown_poly)

mosaic_f1 <- brick("./data/GothicTownsite_Altum_2021_07_27_flight1_calibrated_subset.tif")
mosaic_f2 <- brick("./data/GothicTownsite_Altum_2021_07_27_flight2_calibrated_subset.tif")
mosaic_f3 <- brick("./data/GothicTownsite_Altum_2021_07_27_flight3_calibrated_subset.tif")

##Thresholds mosaics to extract sunlit vs shaded crowns.
sunlit_f1 <- mosaic_f1[[4]] > 0.12
sunlit_f2 <- mosaic_f2[[4]] > 0.12
sunlit_f3 <- mosaic_f3[[4]] > 0.12

##Extracts sunlit temps.
sunlit_temp_f1 <- sunlit_f1 * mosaic_f1[[6]]
sunlit_temp_f1[sunlit_temp_f1==0] <- NA
sunlit_temp_f2 <- sunlit_f2 * mosaic_f2[[6]]
sunlit_temp_f2[sunlit_temp_f2==0] <- NA
sunlit_temp_f3 <- sunlit_f3 * mosaic_f3[[6]]
sunlit_temp_f3[sunlit_temp_f3==0] <- NA

##Extracts shaded temps.
shaded_temp_f1 <- (!(sunlit_f1)) * mosaic_f1[[6]]
shaded_temp_f1[shaded_temp_f1==0] <- NA
shaded_temp_f2 <- (!(sunlit_f2)) * mosaic_f2[[6]]
shaded_temp_f2[shaded_temp_f2==0] <- NA
shaded_temp_f3 <- (!(sunlit_f3)) * mosaic_f3[[6]]
shaded_temp_f3[shaded_temp_f3==0] <- NA

##Computes proportion of the crown that is sunlit for each crown polygon.
sun_brick <- brick(sunlit_f1,sunlit_f2,sunlit_f3)
names(sun_brick) <- c("sunlit_20210727090528","sunlit_20210727112654","sunlit_20210727134223")
prop_sun <- exact_extract(sun_brick,crown_poly,fun="mean",append_cols=c("TreeLabel","area_sqm"))

##Reshapes extracted data and gets date from the layer names.
prop_sun_long <- pivot_longer(prop_sun,cols=contains("sunlit"),names_to="label",values_to="prop_sunlit")
prop_sun_date <- str_split_fixed(prop_sun_long$label,pattern=fixed("_"),n=2)[,2]
prop_sun_date_format <- as.POSIXct(prop_sun_date,format="%Y%m%d%H%M%S")
prop_sun_long_format <- data.frame(tree_label=prop_sun_long$TreeLabel,
                                   crown_area=prop_sun_long$area_sqm,
                                   datetime=prop_sun_date_format,
                                   prop_sunlit=prop_sun_long$prop_sunlit)

##Extracts temperatures for sunlit parts of each crown.
sunlit_temp_brick <- brick(sunlit_temp_f1,sunlit_temp_f2,sunlit_temp_f3)
names(sunlit_temp_brick) <- c("suntemp_20210727090528","suntemp_20210727112654","suntemp_20210727134223")
temp_sunlit <- exact_extract(sunlit_temp_brick,crown_poly,fun="quantile",append_cols=c("TreeLabel"),
                             quantiles=c(0.1,0.25,0.5,0.75,0.9))

##Reshapes data and gets date from the layer name.
temp_sunlit_long <- pivot_longer(temp_sunlit,cols=contains("suntemp"),names_to="label",values_to="sunlit_temp")
temp_quantile <- str_split_fixed(temp_sunlit_long$label,pattern=fixed("."),n=2)[,1]
temp_date <- str_split_fixed(temp_sunlit_long$label,pattern=fixed("_"),n=2)[,2]
temp_date_format <- as.POSIXct(temp_date,format="%Y%m%d%H%M%S")

##Extracts temperature data for the shaded parts of each crown.
shaded_temp_brick <- brick(shaded_temp_f1,shaded_temp_f2,shaded_temp_f3)
names(shaded_temp_brick) <- c("shadetemp_20210727090528","shadetemp_20210727112654","shadetemp_20210727134223")
temp_shaded <- exact_extract(shaded_temp_brick,crown_poly,fun="quantile",append_cols=c("TreeLabel"),
                             quantiles=c(0.1,0.25,0.5,0.75,0.9))
temp_shaded_long <- pivot_longer(temp_shaded,cols=contains("shadetemp"),names_to="label",values_to="shaded_temp")

##Combines data frames for output.
temp_long <- data.frame(tree_label=temp_sunlit_long$TreeLabel,
                        datetime=temp_date_format,
                        quantile=temp_quantile,
                        sunlit_temp=temp_sunlit_long$sunlit_temp,
                        shaded_temp=temp_shaded_long$shaded_temp)

temp_long_prop <- left_join(temp_long,prop_sun_long_format)
temp_wide_prop <- pivot_wider(temp_long_prop,names_from="quantile",values_from=c("sunlit_temp","shaded_temp"))

##Writes summaries to disk.
write_csv(temp_wide_prop,"./data/crown_temp_summaries_20210727_v2.csv")

##Plots median sunlit and shaded temps.
legend_colors <- c("sunlit"="black","shaded"="grey50")
spp_colors <- c("AL10"="black","PT3"="grey50")


pdf("./plots/crown_temperature_20210727_v2.pdf",width=6,height=8)
ggplot(temp_wide_prop)+
  geom_linerange(aes(x=datetime-200,ymin=sunlit_temp_q10,ymax=sunlit_temp_q90,color="sunlit"))+
  geom_linerange(aes(x=datetime-200,ymin=sunlit_temp_q25,ymax=sunlit_temp_q75,color="sunlit"),lwd=1.5)+
  geom_point(aes(x=datetime-200,y=sunlit_temp_q50,color="sunlit"),shape=21,fill="white",size=2.5)+
  geom_linerange(aes(x=datetime+200,ymin=shaded_temp_q10,ymax=shaded_temp_q90,color="shaded"))+
  geom_linerange(aes(x=datetime+200,ymin=shaded_temp_q25,ymax=shaded_temp_q75,color="shaded"),lwd=1.5)+
  geom_point(aes(x=datetime+200,y=shaded_temp_q50,color="shaded"),shape=21,fill="white",size=2.5)+
  scale_x_datetime("Local Time (MDT)")+
  scale_y_continuous("Crown Radiometric Temperature (C)")+
  scale_color_manual(name=" ",values=legend_colors,guide=guide_legend())+
  facet_wrap(facets=~tree_label,as.table=TRUE,ncol=2)+
  theme_bw()
dev.off()

pdf("./plots/crown_temperature_simple.pdf",width=3,height=2.1)
ggplot(filter(temp_wide_prop,tree_label %in% c("AL10","PT3")))+
  geom_linerange(aes(x=datetime-200,ymin=sunlit_temp_q10,ymax=sunlit_temp_q90,color=tree_label))+
  geom_linerange(aes(x=datetime-200,ymin=sunlit_temp_q25,ymax=sunlit_temp_q75,color=tree_label),lwd=1.5)+
  geom_point(aes(x=datetime-200,y=sunlit_temp_q50,color=tree_label),shape=21,fill="white",size=2.5)+
  scale_x_datetime("Local Time (MDT)")+
  scale_y_continuous("Radiometric Tleaf (?C)")+
  scale_color_manual(name=" ",values=spp_colors,guide=guide_legend())+
  #facet_wrap(facets=~tree_label,as.table=TRUE,ncol=2)+
  theme_bw()+
  theme(legend.position=c(0.8,0.3),
        legend.background=element_blank())
dev.off()

