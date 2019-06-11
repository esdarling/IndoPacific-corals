# Created by:    Emily S. darling
# Created:       3 April 2016
# Last modified: 3 April 2016
# Purpose:       global Pacific-centred map in R as base layer for ggplot maps

#updated Pacific-centred map code
#http://stackoverflow.com/questions/28001212/making-a-world-map-with-plots-based-on-the-density-using-ggmap

library(rgdal)
library(rgeos)
library(httr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(countrycode)
library(geosphere)

# Get Pacific-centered map & remove Antarctica ----------------------------
#world_0_360_geojson <- "https://gist.githubusercontent.com/hrbrmstr/1caee1f5e95cc8fa70c2/raw/f4cdd7f34d3a4512cb1c66345d9a5d6149c05c7c/world_0_360.json"
#stop_for_status(GET(world_0_360_geojson, write_disk("world_0_360.geojson"), progress())) 

world <- readOGR(here("analysis", "fig-1-map","world_0_360.geojson"), "OGRGeoJSON")
world <- world[!world$iso_a2 %in% c("AQ"),]
world_map <- fortify(world)
head(world_map)

# Transform our country data ----------------------------------------------

proportions <- "Japan 3137 China 542 Korea 499 VietNam 423 Indonesia 261 Thailand 222 SriLanka 60 Taiwan 56 Taiwan 60 Bangladesh 51 Nepal 43 India 37 Mongolia 26 Myanmar 21 Philippines 16 Singapore 15 Cambodia 11 Malaysia 10 Pakistan 9 Lao_People_Democratic_Republic 7 Brunei_Darussalam 3 Afghanistan 10 Iran 2 Yemen 2 United_Arab_Emirates 2 Lebanon 1 Israel 1 Kenya 9 Botswana 7 Ethiopia 3 Nigeria 2 Mozambique 2 Uganda 2 Morocco 1 Ghana 1 South_Africa 1 Zimbabwe 1 America 58 Canada 5 UnitedMexicanStates 5 Brazil 2 Guyana 2 AntiguaandBarbuda 1 Cuba 1 Nicaragua 1 Fiji 11 Australia 6 Tonga 6 Samoa 2 PapuaNewGuinea 1 Uzbekistan 106 Norway 10 KyrgyzRepublic 9 Germany 7 Fracne 6 Tajikistan 6 Austria 5 Italy 5 UK 5 Belgium 4 Denmark 4 Sweden 4 Finland 4 Estonia 3 Lithuania 3 Russia 3 Georgia 1 Netherlands 1 Portuguese 1 Iceland 1 Kazakhstan 1 Moldova 1 Poland 1 Spain 1 SwissConfedeartion 1 Ukraine 1"

proportions %>%
  strsplit(" ") %>%
  extract2(1) %>%
  matrix(ncol=2, byrow=TRUE) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  dplyr::select(country=1, value=2) %>%
  mutate(value=as.numeric(value),
         iso_2c=countrycode(country, "country.name", "iso2c")) %>%
  left_join(data.frame(gCentroid(world, byid=TRUE), iso_2c=world@data$iso_a2)) -> pts

# Plot our world map ----------------------------------------------
gg <- ggplot() + 
  geom_map(map=world_map, data=world_map, 
           aes(map_id=id, x=long, y=lat), 
           fill = "grey70", colour = "grey70") +
  #scale_x_continuous(limits = c(20,300), expand=c(0,0)) +
  #scale_y_continuous(limits = c(-45,45), expand=c(0,0)) +
  labs(x=NULL, y=NULL) +
  coord_equal() +
  theme_bw(base_size = 14) + 
  theme(panel.border=element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        panel.background = element_rect(fill = "grey96"), 
        panel.grid = element_blank())
