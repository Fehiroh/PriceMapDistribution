if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(tidyverse, RCurl, janitor, summarytools, leaflet, 
       tidytext, igraph, ggraph, tweenr, shiny)  



x <- getURL("https://raw.githubusercontent.com/Fehiroh/TexasAirbnb/master/Airbnb_Texas_Rentals.csv")
og_df <- read.csv(text = x)

redcd_avg_and_bdc_df <- og_df %>% 
  select(-X) %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  mutate(average_rate_per_night = as.numeric(str_extract(average_rate_per_night, "\\d+")), 
         bedrooms_count = as.numeric(if_else(bedrooms_count == "Studio", "0", bedrooms_count))) %>% 
  filter(!is.na(bedrooms_count), bedrooms_count <= 5, average_rate_per_night <= 600) 

city_as_factor <- redcd_avg_and_bdc_df %>% 
  mutate(city = as.factor(city))

city_count <- city_as_factor %>% 
  count(city) %>% 
  arrange(desc(n)) 

cities_to_group_into_rural <- city_count %>% 
  filter(n < 20) %>% 
  select(city) %>%
  unlist() %>% 
  as.character()

cities_to_group_into_mid_size <- city_count %>% 
  filter(n >= 20, n <= 50) %>% 
  select(city) %>%
  unlist() %>% 
  as.character()

cities_to_group_into_sizable <- city_count %>% 
  filter(n > 50, n <= 100) %>% 
  select(city) %>%
  unlist() %>% 
  as.character()


city_or_rural <- city_as_factor %>% 
  mutate(city_or_rural = if_else(as.character(city) %in% cities_to_group_into_rural,
                                 "Cities with < 20 Postings", 
                                 if_else(as.character(city) %in% cities_to_group_into_mid_size, 
                                         "Cities with 20 - 50 Postings", 
                                         if_else(as.character(city) %in% cities_to_group_into_sizable, 
                                                 "Cities with 50 - 100 Postings", 
                                                 as.character(city)))))

city_freq_2 <- freq(city_or_rural$city_or_rural, nmax = 5, header = TRUE, order = "freq") 


city_or_rural_to_keep <- row.names(city_freq_2)[1-10]


definitive_cities <- city_or_rural %>% 
  filter(city_or_rural %in% city_or_rural_to_keep) %>% 
  select(-url) %>% 
  mutate(index = row_number(), 
         city_or_rural = as.factor(city_or_rural))


# looking at the titles, it appears that there is some sort of issue with the  "/"  symbol 
# as everytime the titleends with one, it appears to cut off the sentence, likely to do with 
# how this was scraped. 



definitive_cities$price_quartile <- ntile(definitive_cities$average_rate_per_night, 4)

# creating a datafrme with reference colours 
color_data_frame <- definitive_cities %>% 
  mutate(price_colours = if_else(price_quartile == "1", "aliceblue", 
                                 if_else(price_quartile == "2", "lightskyblue", 
                                         if_else(price_quartile == "3", "blue", "midnightblue"))))

pal <- colorFactor(pallete = c())

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title_1 <- tags$div(
  tag.map.title, HTML("Airbnb Price Distriution Map")
)  

# Map
qpal <- colorQuantile("Greys", color_data_frame$price_quartile, n = 4)

price_map <- leaflet(color_data_frame) %>%
  
  addTiles() %>%
  
  addCircleMarkers(color = ~qpal(price_quartile), 
                   label =  color_data_frame$description)  %>% 
  
  addControl(title_1, position = "topleft", className="map-title") %>%
  
  addLegend(position = "bottomright", pal = qpal, values = ~price_quartile)

price_map




