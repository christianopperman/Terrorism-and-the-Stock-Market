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
volatility_db = fread(file = "./data/^VIX.csv", stringsAsFactors = F) %>% select(., date = Date, vix_close = Close)
sandp_db = fread(file = "./data/^GSPC.csv", stringsAsFactors = F) %>% select(., date = Date, gspc_close = Close)

terror_db = terror_db %>% 
  inner_join(., volatility_db, by = "date") %>% 
  inner_join(., sandp_db, by = "date") #%>% 
  filter(.,doubt_terrorism != -9)

terror_db$weapontype = 
  ifelse(terror_db$weapontype == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", 
         "Vehicle",
         terror_db$weapontype)

write.csv(terror_db, file = "./data/terror_db.csv", row.names = F)


