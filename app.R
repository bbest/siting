# global ----
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

# raster of utility
r_u = raster(
  'data/utility/utility_birds-vs-industry8_v2_raster.grd')

# raster of ids
r_i = r_u
values(r_i) = 1:ncell(r_i)

# color palettes
pal = colorNumeric(
  brewer.pal(11, 'Spectral'), values(r_u), na.color='transparent')
pal_rev = colorNumeric(
  rev(brewer.pal(11, 'Spectral')), values(r_u), na.color='transparent')

# data frame of utility values
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
    d = event_data('plotly_selected')
    if(is.null(d)){
      return(r_u)
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
        row = d_sum %>% filter(key==i)
        leafletProxy('map') %>%
          addPopups(
            click$lng, click$lat, 
            sprintf(
              'key: %d<br>utility:<strong>%0.3f</strong><br>bird: %0.3f<br>npv: %0.3f', 
              i, row$utility, row$bird, row$npv), 
            layerId = 'click')
      }
    })
  })
  
  # Reactive that returns the whole dataset if there is no brush
  d_sel = reactive({
    d = event_data('plotly_selected')
    if(is.null(d)){
      return(d_sum)
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