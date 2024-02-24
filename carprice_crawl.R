# Crawl U-Car car price information
# Author: hanktsai404
# Created on Feb.20 2024

library(rvest)
library(dplyr)
library(stringr)

car_df = data.frame()

url = "https://newcar.u-car.com.tw/newcar/search?bodytype=0&minprice=&maxprice=&size=0&displacement=0&fueltype=0"
raw_web = read_html(url)

for(idx in c(2:373)){
  print(paste("Now crawling car model number:", idx))
  
  brand_sel = paste0("body > div.out_wrap > div.row > div > div > div.span9.article-main > div.channels_list_area > div > div:nth-child(",
   idx, ") > div.newcar_range_ideal_r_m > div.title_brand")
  brand_nm = raw_web %>% html_element(brand_sel) %>% html_text()
  
  sel = paste0("body > div.out_wrap > div.row > div > div > div.span9.article-main > div.channels_list_area > div > div:nth-child(",
               idx, ") > div.newcar_range_ideal_r_m > h2")
  model_url = raw_web %>% html_element(sel) %>% 
    html_children() %>% toString()
  model_url = str_split_i(model_url, pattern = "\"", i = 2)
  model_url = paste0("https://newcar.u-car.com.tw", model_url)
  
  raw_model = read_html(model_url)
  raw_car = raw_model %>%
    html_element("body > div.out_wrap > div.wrap > table") %>%
    html_text()
  raw_car = str_remove_all(raw_car, "\t")
  raw_car = str_replace_all(raw_car, c("\r\n" = "\t", " " = ""))
  raw_car = str_remove_all(raw_car, ",")
  
  car_arr = str_split(raw_car, pattern = "\t")[[1]]
  car_arr = car_arr[-length(car_arr)]
  car_arr = append(car_arr, "dummy1", after = 3)
  car_arr = append(car_arr, "dummy2", after = 5)
  
  row_number = length(car_arr) / 12 - 1
  # Is row_number an integer?
  if(!(row_number %% 1 == 0)){print(paste("Failed URL:", model_url))}
  
  mod_df = data.frame(matrix(ncol = 12, nrow = 0))
  colnames(mod_df) = car_arr[1:12]
  
  for(i in c(1:row_number)){
    mod_df[i,] = car_arr[(12*i + 1):(12*(i+1))]
  }
  mod_df = mod_df %>% select(!c("dummy1", "dummy2"))
  mod_df = mod_df %>% mutate(brand = brand_nm) %>%
    relocate(brand)
  
  car_df = rbind(car_df, mod_df)
  Sys.sleep(2) # Prevent from being blocked by the website
}

write.csv(car_df, "car_price.csv", row.names = F,
          fileEncoding = "UTF-8")

car_df = read.csv("car_price.csv", encoding = "UTF-8")

# Let us make the data frame cleaner
colnames(car_df) = c("brand", "model", "price", "cyl_cap",
                     "fuel_type", "engine_type", "gear_tp",
                     "drivetrain", "max_horsepower",
                     "fuel_consumption_km_l", "tax")

car_df = car_df %>% 
  mutate(price = str_split_fixed(price, pattern = "~", n = 2)[,1])
car_df = car_df %>%
  mutate(price = as.numeric(str_extract(price, "[0-9].")) * 10000)

car_df = car_df %>%
  mutate(tax = str_remove_all(tax, "[燃牌]")) %>%
  mutate(fuel_charge = as.numeric(str_split_fixed(tax, pattern = "/", n = 2)[,1]),
         license_tax = as.numeric(str_split_fixed(tax, pattern = "/", n = 2)[,2])) %>%
  select(!c("tax"))

car_df = car_df %>%
  mutate(fuel_consumption_km_l = str_remove_all(fuel_consumption_km_l, "km/l")) %>%
  mutate(fuel_consumption_km_l = as.numeric(fuel_consumption_km_l))

car_df = car_df %>% arrange(fuel_type, brand, cyl_cap, price)

write.csv(car_df, "cleaner_carprice.csv", row.names = F,
          fileEncoding = "UTF-8")



