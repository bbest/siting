crop_na = function(r){
  # crop out NA margins of raster
  m <- is.na(as.matrix(r))
  cols = which(colSums(m) != nrow(m))
  rows = which(rowSums(m) != ncol(m))
  crop(
    r,
    extent(
      r, 
      rows[1], rows[length(rows)],
      cols[1], cols[length(cols)]))
}

i_cetacean_time_png = function(i, r_i, s_c){
  # @parameters
  #   i = index of cell
  #   r_i = raster of indices corresponding with s_c
  #   s_c = stack of cetacean rasters
  # @return: path to png
  
  # delete old images
  unlink(list.files(path='www/tmp', pattern='i_cetacean_time_.*\\.png', full.names=T))
  
  v = data_frame(
    mo = 1:12,
    val = s_c[i] %>% as.vector())

  dir.create('www/tmp', showWarnings = F)
  png_tmp = sprintf('www/tmp/i_cetacean_time_%d.png', as.integer(Sys.time()))
  png_www = sprintf(    'tmp/i_cetacean_time_%d.png', as.integer(Sys.time()))
  
  # get month with minimum value
  mo_min = which.min(v$val)
  
  # TODO: approximate lines so continuous color, like http://stackoverflow.com/questions/31706390/varying-gradient-using-ggplot2-in-r
  p = ggplot(v, aes(x=mo, y=val, color=val)) + 
    geom_line(size=3) +
    coord_cartesian(xlim = c(1, 12)) + 
    xlab('Month') + ylab('Cetacean') +
    scale_x_continuous(breaks=c(1,4,7,10,12), minor_breaks=seq(1,12,by=1), labels=month.abb[c(1,4,7,10,12)]) +
    geom_point(aes(x=mo_min, y=v$val[mo_min], color=v$val[mo_min]), cex=10) + 
    scale_colour_distiller(palette='Spectral') +
    labs(colour = 'Sensitivity')
    theme_grey(base_size=15)
  dpi = 72
  ggsave(basename(png_tmp), p, path=dirname(png_tmp), width=400/dpi, height=200/dpi, dpi=dpi)

  return(png_www)
}