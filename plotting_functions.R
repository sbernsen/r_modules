# Plotting functions to make life easy
library(caTools)

pm_polygon_fill <- function(x, y, shift , line_fill) 
{
  ind <- which(y >= shift)
  pos_x <- c(x[ind], rev(x[ind]) )
  pos_y <- c(y[ind], rep(shift, length(ind) ) )
  
  neg_x <- c(x[-ind], rev(x[-ind]) )
  neg_y <- c(y[-ind], rep(shift, length(y[-ind]) ) )
  
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

ts_shifts <- function( df )
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

plot_geophone_wiggles <- function(time_vec, df, line_fill )
{
  
  df <- ts_shifts(df) 
  amp_shift <- df$amp_shift
  df <- df$ts_shift 
  
  dev.new(width = 8, height = 12, units = "in")
  plot( df[,1], time_vec, type = "l", xlim = c(min(amp_shift), max(amp_shift) ),
       ylim = ylims, xlab = "", ylab = "", xaxt = "n", 
       lwd = 0.5 )
  pm_polygon_fill(time_vec, df[,1], amp_shift[1], line_fill)
  
  for(i in 2:length(amp_shift) )
  {
    lines( df[,i], time_vec, lwd = 0.5)
    pm_polygon_fill(time_vec, df[,i], amp_shift[i], line_fill)
  }
  
}

