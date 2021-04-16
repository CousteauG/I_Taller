get_variable2 <- function(base_polygon, Lon, Lat, Var)
{
  
  newLon = Lon[ Lon >= range(base_polygon$x)[1] & Lon <= range(base_polygon$x)[2]]
  newLat = Lat[ Lat >= range(base_polygon$y)[1] & Lat <= range(base_polygon$y)[2]]
  
  
  tmp = expand.grid(x = newLon, y = newLat)
  pos = inout(pts = tmp, poly = base_polygon)
  tmp2 = tmp[pos, ]
  
  posLon = match(tmp2$x, Lon)
  posLat = match(tmp2$y, Lat)
  
  output = list(Values = mean(Var[posLon, posLat], na.rm = TRUE),
                Positions = tmp2)
  
  return(output)
  
}


get_variable <- function(base_data, Lon, Lat, Var)
{
  
  output = NULL
  for(i in 1:nrow(base_data)){
    
    posLon = which.min(abs(Lon - base_data[i,1]))
    posLat = which.min(abs(Lat - base_data[i,2]))
    
    selLon = Lon[posLon]
    selLat = Lat[posLat]
    
    tmp = data.frame(var = Var[posLon, posLat], lon_env = selLon, lat_env = selLat)
    
    output = rbind(output, tmp)
    
  }
  
  return(output)
  
}