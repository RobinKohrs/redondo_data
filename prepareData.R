library(giscoR)
library(sf)
library(here)
library(glue)
library(rajudas)
library(lwgeom)
library(geojsonio)
library(tidyverse)
library(eurostat)

# download eu countries ---------------------------------------------------
eu_countries = eurostat::get_eurostat_geospatial()

# only get nuts level 0 ---------------------------------------------------
eu = eu_countries %>%
  filter(
    LEVL_CODE == 0
  ) %>% 
  janitor::clean_names() %>% 
  mutate(nuts_name = str_to_title(nuts_name)) %>% 
  st_transform(3857)

# split countries ---------------------------------------------------------
countriesEUSplit = split(eu, eu$fid)

# set cache dir
force = F
baseDir = here("static/geodata/")


walk(countriesEUSplit, function(c){
  
  cli::cli_h2(glue("Processing data for: {c$name_latn}"))

  # country  ------------------------------------------------------
  op_country_data = file.path(baseDir, "all_communes_country", glue("{c$cntr_code}.geojson"))
  dir_country_data = dirname(op_country_data)
  
  if(!file.exists(op_country_data)){
    
    dir.create(dir_country_data, recursive = T)
    country_communes = giscoR::gisco_get_communes(epsg="3857", country = c$fid)
    write_sf(country_communes, op_country_data)
    
  }else{
    country_communes = read_sf(op_country_data)
  }
  
  if(!"COMM_NAME" %in% names(country_communes)){
    return()
  }
  
  # get the first three letters for each  ------------------------------------------------------
  data_country = country_communes %>%
    mutate(fl = str_sub(COMM_NAME, 1, 2) %>% str_to_lower,
           fl_nchar = nchar(fl)) %>%
    select(COMM_ID, COMM_NAME, FID, fl, fl_nchar)
  
  # split for each commune  ------------------------------------------------------
  each_commune = data_country %>%
    split(.$FID) %>% 
    unname()
  
  iwalk(each_commune, function(co, i){
    
    cat(paste0(i, "/", length(each_commune), "\r"))
    
    # op for that commune  ------------------------------------------------------ 
    name_clean = co$COMM_NAME %>% janitor::make_clean_names()
    op = file.path(baseDir, "communes_single", co$fl, glue("{name_clean}.geojson"))
    dir_file = dirname(op)
    
    if(file.exists(op)) return()
    if(!dir.exists(dir_file)) dir.create(dir_file, recursive = T)
    
    # select only the biggest one  ------------------------------------------------------
    poly_max = co %>%
      st_cast("POLYGON") %>% 
      mutate(area = st_area(.)) %>% 
      filter(area == max(area)) %>% 
      mutate(
        type = "outline"
      ) 
    
    # get the circle  ------------------------------------------------------
    min_circle = st_minimum_bounding_circle(poly_max) %>% 
      mutate(
        type = "circle"
      )
    
    # compute the area  ------------------------------------------------------ 
    area_commune = as.numeric(st_area(poly_max %>% st_transform(3035)) / 1e6) %>% round(2)
    area_circle = as.numeric(st_area(min_circle %>% st_transform(3035)) / 1e6) %>% round(2)
    
    poly_max[["area"]] = area_commune 
    min_circle[["area"]] = area_circle 
    poly_max[["circliness"]] = (100 * (area_commune / area_circle)) %>% round(2)
    
   
    # bind together the polygon and the cirlce  ------------------------------------------------------
    bind_rows(
      poly_max,
      min_circle
    ) %>% 
      select(COMM_NAME, type, fl, fl_nchar, area, circliness) -> final_data
    
    
    # set the rownames to NULL
    rownames(final_data) = NULL
   
    # create the entry in the index file 
    index_file = file.path(dir_file, "index.csv")
    df_index_entry = data.frame(
      name_clean = name_clean,
      name_real = co$COMM_NAME
    )
    
    if(!file.exists(index_file)){
      cat("name_clean,name_real\n", file=index_file)
      cat(paste(name_clean, co$COMM_NAME, sep=","), file=index_file, append = T, fill = T)
    }else{
      cat(paste(name_clean, co$COMM_NAME, sep=","), file=index_file, append = T, fill=T)
    }
    
    
    
    write_sf(final_data, op)
    
      
  })
  
  
   
})
