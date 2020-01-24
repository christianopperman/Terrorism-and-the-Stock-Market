###Define a function to calculate change from previous day
get_change = function(db, db_price_col){
   i = 1
   closing_price = c(0)
   for(i in 2:nrow(db)){
     closing_price = c(closing_price, db_price_col[i]-db_price_col[i-1])
   }
   return(closing_price)
 }

###Create database of daily S&P500 and VIX movement
volatility_db = fread(file = "./data/^VIX.csv", stringsAsFactors = F) %>% 
  select(., date = Date, vix_close = Close)

volatility_db = volatility_db %>% mutate(., vix_price_change = get_change(volatility_db, volatility_db$vix_close))

sandp_db = fread(file = "./data/^GSPC.csv", stringsAsFactors = F) %>% 
  select(., date = Date, sandp_close = Close)

sandp_db = sandp_db %>% mutate(., sandp_price_change = get_change(sandp_db, sandp_db$sandp_close))

vix_sandp_db = inner_join(volatility_db, sandp_db, by = "date")

write.csv(vix_sandp_db, file = "./data/vix_sandp_db.csv", row.names = F)



###Create database of terrorist attacks in 1990-2017, with stock market and volatility index closing prices on day of attack

terror_db =
  fread(file = "./data/globalterrorismdb.csv", stringsAsFactors = F) %>%
  mutate(., date = paste(iyear, imonth, iday, sep = "-")) %>%
  select(.,
    eventid,
    date,
    doubt_terrorism = doubtterr,
    country = country_txt,
    region = region_txt,
    provstate,
    city,
    latitude,
    longitude,
    attacktype = attacktype1_txt,
    suicide,
    weapontype = weaptype1_txt,
    weaponsubtype = weapsubtype1_txt,
    targettype = targtype1_txt,
    targetsubtype = targsubtype1_txt,
    nationality = natlty1_txt,
    perpname = gname,
    nkill,
    nwound
  )

terror_db = terror_db %>% 
  full_join(., volatility_db, by = "date") %>% 
  full_join(., sandp_db, by = "date") %>% 
  filter(., doubt_terrorism != -9)

terror_db$weapontype = 
  ifelse(terror_db$weapontype == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", 
         "Vehicle",
         terror_db$weapontype)

write.csv(terror_db, file = "./data/terror_db.csv", row.names = F)


