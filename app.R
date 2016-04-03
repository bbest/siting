# TODO
# - read in cetaceans, add png of cetacean over time

# load libraries ----
suppressPackageStartupMessages({
  library(shiny)
  library(raster)
  library(dplyr)
  library(leaflet)
  library(readr)
  library(ggplot2)
  library(plotly)
  library(RColorBrewer)
  library(DT)
})
source('functions.R')

# load data ----
Rdata = 'data/dat.Rdata'
if (!file.exists(Rdata)){
  # if loading new data in block below, delete Rdata and run again to regenerate

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
  
  # big rasters
  if (file.exists('~/github/consmap-prep')){
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
  
  # save all vars to disk
  save(r_u, pal, pal_rev, r_i, d_sum, r_b, s_c, file=Rdata)
} else {
  # speed up start app time by loading Rdata
  load(Rdata)
}
  
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

    ggplotly(p) %>% layout(dragmode='select')  
  })
  
  r_sel = reactive({
    # get rows selected in plot
    d = event_data('plotly_selected')
    if(is.null(d)){
      # get rows selected in table
      rows = input$table_rows_selected
      if (length(rows)){
        return(mask(r_u, raster::`%in%`(r_i, d_sum$key[rows]), maskvalue=0))
      } else{
        return(r_u)
      }
    } else {
      return(mask(r_u, raster::`%in%`(r_i, d$key), maskvalue=0))
    }
  })
  
  # output$map ----
  output$map = renderLeaflet({
    
    leaflet() %>%
      addProviderTiles('Stamen.TonerLite') %>%
      addRasterImage(r_u    , colors=pal, opacity=0.3, layerId='r_u') %>%
      addRasterImage(r_u, colors=pal, opacity=1  , layerId='r_sel') %>%
      addLegend('bottomright', pal=pal_rev, values=values(r_u),
                title = 'u', opacity = 1,
                labFormat = labelFormat(transform = function(x) rev(x)))
    
  })
  
  # draw opaque utility raster of selected sites in tradeoff plot
  observe({
    leafletProxy('map') %>%
      removeImage('r_sel') %>%
      addRasterImage(r_sel(), colors=pal, opacity=1  , layerId='r_sel')
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
      d_sum %>%
        semi_join(d, by='key')
    }
  })
  
  # output$table ----
  output$table = renderDataTable({
    d_sel()
  }, rownames=F, options = list(pageLength = 10, dom = 'tip'))

}

shinyApp(ui, server)