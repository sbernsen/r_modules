# Plotting functions to make life easy
library(caTools)
library(colorRamps)
library(matlab)


pm_polygon_fill <- function(x, y, shift , line_fill) 
{

  pos_x <- c(x, rev(x) )
  pos_y <- ifelse(y >= shift, y, shift) 
  pos_y <- c(pos_y, rep(shift, length(pos_y) ) )
  
  neg_x <- pos_x
  neg_y <- ifelse(y <= shift, y, shift) 
  neg_y <- c(neg_y, rep(shift, length(neg_y) ) )
  

  polygon(pos_y, pos_x, col = line_fill[1], lwd = 0.5)
  polygon(neg_y, neg_x, col = line_fill[2], lwd = 0.5)
  
}

ts_norm <- function(df, k) 
{
  n <- dim(df)[2]
  for( i in 1:n)
  {
    df[,i] <- df[,i]/runsd(df[,i], k, endrule = "sd")
  }
  
  return(df)
}

ts_yshifts <- function( df )
{
  m <- dim(df)[1]
  n <- dim(df)[2]
  
  mns <- apply(abs(df), 2, max, na.rm = T )
  mns <- t(matrix(mns, n, m ) ) 
  df <- df/mns
  
  amp_shift <- seq(1, n)
  
  df <- t( matrix( amp_shift, n, m) ) + df
  
  return( list(amp_shift = amp_shift, ts_shift = df) )
  
}


## -------------------------------------------------------------------------------- ##
plot_geophone_wiggles <- function( time_vec, df, line_fill, vert)
{
  # You must add your own axis labels  
  df <- ts_yshifts(df) 
  amp_shift <- df$amp_shift
  df <- df$ts_shift 
  par(bg = NA, col = "black", col.lab = "black", col.axis = "black", col.sub = "black")

  da <- diff(amp_shift)[1]
  if( vert )
  {
    plot( df[,1], time_vec, type = "l", xlim = c(min(amp_shift)-da, max(amp_shift)+da ),
         ylim = c(max(time_vec), min(time_vec) ), xlab = "", ylab = "Travel Time (s)", xaxt = "n", 
         lwd = 1.5, xaxs = "i" )

    if(line_fill[1] != "none" )
    {
      pm_polygon_fill(time_vec, df[,1], amp_shift[1], line_fill)
      for(i in 2:length(amp_shift) )
      {
        lines( df[,i], time_vec, lwd = 1.5)
        pm_polygon_fill(time_vec, df[,i], amp_shift[i], line_fill)
      }
    }else 
    { 
      for(i in 2:length(amp_shift) )
      {
        lines( df[,i], time_vec, lwd = 1.5)
      }
    }
  }else
  {
    plot( time_vec, df[,1], type = "l", ylim = c(min(amp_shift), max(amp_shift) ),
         xlim = c(min(time_vec), max(time_vec) ), yaxt = "n" , xlab = "", ylab = "",
         lwd = 1.5, xaxs = "i")
  
    if(line_fill[1] != "none" )
    {
      pm_polygon_fill(time_vec, df[,1], amp_shift[1], line_fill)
      for(i in 2:length(amp_shift) )
      {
        lines( time_vec, df[,i], lwd = 1.5)
        pm_polygon_fill(time_vec, df[,i], amp_shift[i], line_fill)
      }
    }else 
    {  
      for(i in 2:length(amp_shift) )
      {
        lines( time_vec, df[,i], lwd = 1.5)
      }
    
    }
  }

#if( !is.null(direct_wave) && vert )
#  {
#    lines( amp_shift, direct_wave, col = "green4", lwd = 1.5)
#  }else if( !is.null(direct_wave) )
#  {
#    lines( direct_wave, amp_shift, col = "green4", lwd = 1.5) 
#  }
  return(amp_shift)
}



plot_stransform <- function(ts, stran, f, tv)
# This is a quick and dirty plot for a stacked subplot of the time series being analyzed and the S-transform 
# Input:
#   ts - the m-by-1 amplitude vector
#   tv - the m-by-1 time vector as seconds
#   f - frequency as hertz
# 
#
{

  layout( matrix( c(1, 1, 2, 2, 2, 2), nrow = 3, ncol = 2, byrow = TRUE) )

  plot( tv, ts, type = "l", xlab = "Time (s)", xaxs = "i")

  imagesc(tv, f, abs(stran), col = matlab.like(50), ylab = "Frequency (Hz)", xlab = "Time (s)" )

}

plot_cwt <- function(ts, cwt, period, time_vec) 
{
  layout( matrix( c(1, 1, 2, 2, 2, 2), nrow = 3, ncol = 2, byrow = TRUE) )

  plot( time_vec, ts, type = "l", xlab = "Time (s)", xaxs = "i")

  image(time_vec, period, abs(cwt), col = matlab.like(50), ylab = "Period (s)", xlab = "Time (s)")

}

plot_radarco <- function(x, z, im)
{
  gs = gray.colors(50) 
  
}



single_pick <- function(X, tv, time_id)
{

  n <- dim(X)[2] 

  time_id <- matrix(0, n, 3) 

  for(i in 1:n) 
  {
    plot(tv, X[,i], type = "l")
    ind[i] <-  identify(tv, X[,i])
    points( tv[ ind[i] ], X[1,i] , pch = "-" )
    key_press <- readline(prompt = "Press [x] to redo [s] to save")

    while(key_press == "x")
    {
        plot(tv, X[,i], type = "l")
        points( tv[ ind[i] ], X[1,i] , pch = "-" )

        ind[i] <-  identify(tv, X[,i])
        points( tv[ind[i]], X[1,i] , pch = "-" )

        key_press <- readline(prompt = "Press [x] to redo, [] to save")
    
      
    }

  }

}


array_pick <- function(X, tv, line_fill, time_id)
{


  X <- ts_yshifts(X) 
  amp_shift <- X$amp_shift
  X <- X$ts_shift 
  par(bg = NA, col = "black", col.lab = "black", col.axis = "black", col.sub = "black")

  plot( X[,1], tv, type = "l", ylim = c( max(tv), min(tv) ), xlim = c(min(X), max(X) ), xlab = "", ylab = "Travel Time (s)",  xaxt = "n", 
         lwd = 1.5, xaxs = "i" )

  pm_polygon_fill(tv, X[,1], amp_shift[1], line_fill)
  for(i in 2:length(amp_shift) )
  {
    lines( X[,i], tv, lwd = 1.5)
    pm_polygon_fill(tv, X[,i], amp_shift[i], line_fill)
  }


  n <- dim(X)[2] 
  x <- X[1,]

  lines(x, time_id[,2], col = "red", lwd = 3)

  for(i in 1:n) 
  {

    lines(X[,i], tv, col = "black", cex = 1.5)
    print("Pick a point\n")
    ind <-  as.numeric( identify( X[,i], tv, n=1, plot = F) )
    points( x[i] , tv[ ind ], pch = "-", cex = 2 )
    key_press <- readline(prompt = "Press [x] to redo, [n] for next, [q] to quit, [0-9] to save a pick id number\n")
    
#    if( key_press != "x" || as.numeric(key_press) > 9 )
#    {
#      key_press <- "x"
#    }  
    if(key_press == "n")
    {
      print("Moving on to the next trace.\n\n")
      next
    }

    if(key_press == "q")
    {
      break
    }

    while(key_press == "x" )
    {
        
      ind <-  as.numeric(identify( X[,i], tv, n=1, plot = F))
      points( X[1,i] , tv[ ind ], pch = "-", cex = 2 )
      key_press <- readline(prompt = "Press [x] to redo [0-9] to save a pick id number\n")

    }

    time_id[i,] <- c( as.numeric(ind), as.numeric(tv[ind]), as.numeric(key_press) ) 

    plot( X[,1], tv, type = "l", xlim = c(min(X), max(X) ),
         ylim = c(max(tv), min(tv) ), xlab = "", ylab = "Travel Time (s)", xaxt = "n", 
         lwd = 1.5, xaxs = "i" )

    pm_polygon_fill(tv, X[,1], amp_shift[1], line_fill)
    for(i in 2:length(amp_shift) )
    {
      lines( X[,i], tv, lwd = 1.5)
      pm_polygon_fill(tv, X[,i], amp_shift[i], line_fill)
    }

    lines(x, time_id[,2], col = "red", lwd = 3)

  }

  return(time_id)

}