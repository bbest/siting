# TODO
# - read in cetaceans, add png of cetacean over time

# load libraries ----
suppressPackageStartupMessages({
  library(shiny)
  library(raster)
  library(sf)
  library(dplyr)
  library(leaflet)
  library(readr)
  library(ggplot2)
  library(plotly)
  library(rnaturalearth)
  library(RColorBrewer)
  library(DT)
})
source('functions.R')

# load data ----

# utility table
d_sum = read_csv('data/utility/utility_birds-vs-industry8_v2_data.csv') %>%
  select(
    rank,
    utility = u_avg,
    bird    = x,
    npv     = y,
    key     = i) %>%
  mutate(
    utility = round(utility, 3),
    bird    = round(bird, 3),
    npv     = round(npv, 3))
  
# NOTE: reading rasters does not work with save() & load() of *.Rdata
redo_rasters = F

# utility raster
r_u = raster(
  'data/utility/utility_birds-vs-industry8_v2_raster.grd')

# color palettes
pal = colorNumeric(
  brewer.pal(11, 'Spectral'), values(r_u), na.color='transparent')
pal_rev = colorNumeric(
  rev(brewer.pal(11, 'Spectral')), values(r_u), na.color='transparent')

# id raster
r_i = r_u
values(r_i) = 1:ncell(r_i)

# big rasters
if (file.exists('~/github/consmap-prep') & redo_rasters){
  # not available on shiny.env.duke.edu, so do this locally on laptop
  
  # bird raster
  r_b = raster(stack('~/github/consmap-prep/data/birds/spp_birds_aea.grd'), 'BIRDS_nw') %>%
    crop_na()
  writeRaster(r_b, 'data/birds/spp_birds_aea_BIRDS_nw.grd', overwrite=T)
  
  # cetacean stack, just 12 months of composite values, rescaled 0 to 1
  s_c = stack('~/github/consmap-prep/data/species/spp_EC_nzw_aea.grd') %>%
    subset(sprintf('ALL_nfzw_%02d', 1:12)) %>%
    crop(r_b) %>%
    mask(r_b)
  s_min = min(cellStats(s_c, 'min'))
  s_max = max(cellStats(s_c, 'max'))
  s_c = (s_c - s_min) / (s_max - s_min)
  writeRaster(s_c, 'data/cetaceans/spp_EC_nzw_aea_ALL_nfzw_1to12_scaled0to1.grd', overwrite=T)
  
} else {
  
  # bird raster
  r_b = raster('data/birds/spp_birds_aea_BIRDS_nw.grd')
  
  # cetacean stack
  s_c = stack('data/cetaceans/spp_EC_nzw_aea_ALL_nfzw_1to12_scaled0to1.grd')
}
  

# get USA states
us = rnaturalearth::ne_states(iso_a2 = "US", returnclass = "sf") %>% 
  mutate(
    ctr = st_centroid(geometry),
    lon = st_coordinates(ctr)[,1],
    lat = st_coordinates(ctr)[,2],
    code = c(setNames(state.abb, state.name), 'District of Columbia'='DC')[name])

# st_coordinates(us$ctr)
# # get East Coast states
# ec = subset(us,
#             code %in% 
#               c('ME','VT', # NE
#                 'NY','MA','RI','CT','PA','NJ','MD','DE','DC','WV','VA','NC', # Mid-Atlantic
#                 'SC','GA','FL')) # South
# ec@data %>%
#   arrange(desc(pt_lat)) %>%
#   .$code %>% paste0("','") %>% cat(sep='')
# get Mid-Atlantic states
# ma_gcs = subset(us,
#                 code %in% 
#                   c('NY','MA','RI','CT','PA','NJ','MD','DE','DC','WV','VA','NC')) # Mid-Atlantic
# # project to aea
# ma = spTransform(ma_gcs, crs(r_b))
#ma_gadm <- ma
#plot(ma_gadm)
#plot(ma)

# get plotting coordinates
# ma@data = ma@data %>%
#   mutate(
#     x = coordinates(ma)[,1],
#     y = coordinates(ma)[,2])
# ma_pts_manual = list( # get from plotting raster and using click()
#   NY=c(318279.0, 861861.0),
#   MA=c(583509.0, 900670.0),
#   PA=c(176331.0, 753784.4),
#   NJ=c(302205.0, 697670.0),
#   DE=c(218600.4, 532496.3),
#   MD=c(116658.7, 604601.4),
#   VA=c(87289.93, 403240.4), #58407.0, 403619.0),
#   NC=c(142765.7, 194348.2))
# for (code in names(ma_pts_manual)){
#   xy = ma_pts_manual[[code]]
#   ma@data$x[ma@data$code==code] = xy[1]
#   ma@data$y[ma@data$code==code] = xy[2]
# }


# ui: user interface ----
ui <- fluidPage(
  navbarPage(title='Conservation',
    tabPanel(
      'Siting', icon = icon('map-marker'),
      fluidRow(
        column(
          7,
          leafletOutput('map')),
        column(
          5,
          plotlyOutput('plot'))),
      fluidRow(
        column(
          12,
          dataTableOutput('table')))
    )))

server <- function(input, output, session){
                
  # output$plot ----
  output$plot = renderPlotly({
    s = input$table_rows_selected
    key <- d_sum$key
    p = ggplot(d_sum, aes(bird, npv, colour=utility, key=key)) +
      geom_point() +
      coord_equal(xlim=c(0,1), ylim=c(0,1), expand=F) +
      scale_x_continuous(
        name = 'Bird Sensitivity', trans='reverse') +
      scale_y_continuous(
        name = 'Wind Profitablity ($NPV)') +
      theme(axis.title.y=element_text(margin=margin(0,20,0,0))) +
      scale_colour_gradientn(colours = brewer.pal(11, 'Spectral'), name='u') +
      geom_vline(xintercept = quantile(d_sum$bird, probs=(1-0.2), na.rm=T), linetype=2, col='red') +
      geom_vline(xintercept = quantile(d_sum$bird, probs=(1-0.6), na.rm=T), linetype=2, col='blue') +
      geom_hline(yintercept = quantile(d_sum$npv, probs=   0.2 , na.rm=T), linetype=2, col='red') +
      geom_hline(yintercept = quantile(d_sum$npv, probs=   0.6 , na.rm=T), linetype=2, col='blue') + 
      theme(legend.position="none")
    if (length(s)){
      p = p + geom_point(aes(bird, npv, key=key), data = d_sum[s, drop = F], pch = 19, cex = 2, color='black')
    }

    ggplotly(p) %>% layout(dragmode='select')  
  })
  
  r_sel = reactive({
    # get rows selected in plot
    sel_plot = event_data('plotly_selected')
    if(is.null(sel_plot)){
      # get rows selected in table
      sel_table = input$table_rows_selected
      if (length(sel_table)){
        return(mask(r_u, raster::`%in%`(r_i, d_sum$key[sel_table]), maskvalue=0))
      } else{
        return(r_u)
      }
    } else {
      return(mask(r_u, raster::`%in%`(r_i, sel_plot$key), maskvalue=0))
    }
  })
  
  # output$map ----
  output$map = renderLeaflet({

    # get USA states
    # us = rnaturalearth::ne_states(iso_a2 = "US", returnclass = "sf") %>% 
      # mutate(
      #   ctr = st_centroid(geometry, of_largest_polygon = T),
      #   lon = st_coordinates(ctr)[,1],
      #   lat = st_coordinates(ctr)[,2],
      #   code = c(setNames(state.abb, state.name), 'District of Columbia'='DC')[name])
      # filter(code != "DC") %>% 
      # left_join(
      #   tribble( # get from plotting raster and using click()
      #     ~code, ~lon, ~lat
      #     "NY", 318279.0, 861861.0),
      #     "MA", 583509.0, 900670.0),
      #     "PA", 176331.0, 753784.4),
      #     "NJ", 302205.0, 697670.0),
      #     "DE", 218600.4, 532496.3),
      #     "MD", 116658.7, 604601.4),
      #     "VA", 87289.93, 403240.4), #58407.0, 403619.0),
      #     "NC", 142765.7, 194348.2))
      #   
        
      #)
      
      #mapedit::editFeatures()    

      # tribble( # get from plotting raster and using click()
      # code, lon, lat
      # NY=c(318279.0, 861861.0),
      # MA=c(583509.0, 900670.0),
      # PA=c(176331.0, 753784.4),
      # NJ=c(302205.0, 697670.0),
      # DE=c(218600.4, 532496.3),
      # MD=c(116658.7, 604601.4),
      # VA=c(87289.93, 403240.4), #58407.0, 403619.0),
      # NC=c(142765.7, 194348.2))
    
    # m <- 
    # us %>%
    #   filter(
    #     code %in% c('NY','MA','RI','CT','PA','NJ','MD','DE','WV','VA','NC')) %>% # Mid-Atlantic
    #   leaflet() %>%
    #   #addTiles() %>%
    #   addPolygons(
    #     color = "#4D4D4D", fill = FALSE, weight=2, opacity=1) %>%
    #   addLabelOnlyMarkers(
    #     lng = ~lon, lat = ~lat, label = ~code,
    #     labelOptions = labelOptions(
    #       noHide = T, direction = 'top', textOnly = T)) #%>%
    #   mapedit::editMap()
    # data = us, stroke = T, color = "gray80", fill=F) %>%
    #fitBounds()
    # gplots::col2hex("gray90") # "#E5E5E5"
    # gplots::col2hex("gray10") # "#1A1A1A"
    # gplots::col2hex("gray30") # "#1A1A1A"
      
    bb <- projectExtent(r_u, crs=leaflet:::epsg4326) %>% extent() %>% as.vector()
        
    # leaflet(data = quakes[1:20,]) %>% addTiles() %>%
    #   addLabelOnlyMarkers(~long, ~lat, label =  ~as.character(mag), 
    #                       labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
    # 
    
   us %>%
      filter(
        !code %in% c("HI", "AK", "DC")) %>% 
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, group="Esri ocean") %>%
      addProviderTiles(providers$Stamen.TonerLite, group="Stamen b&w") %>%
      addPolygons(
        group="States",
        color = "#4D4D4D", fill = FALSE, weight=2, opacity=1) %>% 
      addLabelOnlyMarkers(
        lng = ~lon, lat = ~lat, label = ~code, group="States",
        labelOptions = labelOptions(
          noHide = T, direction = 'top', textOnly = T)) %>% 
      addLayersControl(
        baseGroups    = c("Stamen b&w", "Esri ocean", "None"),
        overlayGroups = c("States", "Utility")) %>% 
      addRasterImage(r_u    , colors=pal, opacity=0.3, layerId='r_u', group="Utility") %>%
      addRasterImage(r_u, colors=pal, opacity=1  , layerId='r_sel', group="Utility") %>%
      addLegend('bottomright', pal=pal_rev, values=values(r_u),
                title = 'Utility', opacity = 1,
                labFormat = labelFormat(transform = function(x) rev(x))) %>% 
     hideGroup("States") %>% 
     fitBounds(bb[1], bb[3], bb[2], bb[4])
   
  })
  
  # draw opaque utility raster of selected sites in tradeoff plot
  observe({
    leafletProxy('map') %>%
      removeImage('r_sel') %>%
      addRasterImage(r_sel(), colors=pal, opacity=1, layerId='r_sel', group="Utility")
  })
  
  # add popup on map of site values
  observe({
    req(input$map_click)
    
    click = input$map_click

    isolate({
      leafletProxy('map') %>% clearPopups()
      click_gcs = SpatialPoints(
        matrix(unlist(click[c('lng','lat')]), ncol=2), 
        proj4string=CRS(leaflet:::epsg4326))
      click_aea = spTransform(click_gcs, crs(r_u))
      i = extract(r_i, click_aea)
      u = extract(r_u, click_aea)
      
      if (!is.na(u)){
        
        # get cetaceans over time image
        png_path = i_cetacean_time_png(i, r_i, s_c)
        #png_path = 'tmp/i_cetacean_time_1459713210.png'
        
        row = d_sum %>% filter(key==i)
        leafletProxy('map') %>%
          addPopups(
            click$lng, click$lat, 
            sprintf(
              #   'key: %d<br>utility:<strong>%0.3f</strong><br>bird: %0.3f<br>npv: %0.3f<br>',
              # i, row$utility, row$bird, row$npv), 
              paste(
                'key: %d<br>utility:<strong>%0.3f</strong><br>bird: %0.3f<br>npv: %0.3f<br>',
                '<img src="%s">'),
              i, row$utility, row$bird, row$npv, png_path),
            layerId = 'click')
      }
    })
  })
  
  # Reactive that returns the whole dataset if there is no brush
  d_sel = reactive({
    # get rows selected in plot
    d = event_data('plotly_selected')
    if(is.null(d)){
      d_sum
    } else {
      #browser()
      d_sum %>%
        semi_join(
          d %>% 
            mutate(key = as.integer(key)), by='key')
    }
  })
  
  # pts_sel = reactive({
  #   # get rows selected in table
  #   s = input$table_rows_selected
  #   if (length(s)) points(cars[s, , drop = FALSE], pch = 19, cex = 2)
  #   
  #   if (length(sel_table)){
  #     return(mask(r_u, raster::`%in%`(r_i, d_sum$key[sel_table]), maskvalue=0))
  #   } else{
  #     return(r_u)
  #   }
  #   } else {
  #     return(mask(r_u, raster::`%in%`(r_i, sel_plot$key), maskvalue=0))
  #   }
  # })
  
  
  # output$table ----
  output$table = renderDataTable({
    d_sel()
  }, rownames=F, options = list(pageLength = 10, dom = 'tip'))

}

shinyApp(ui, server)