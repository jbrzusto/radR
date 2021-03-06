## svn $Id: rawarch.conf.R 574 2010-05-11 02:07:15Z john $
##                                                         
##   RAWARCH  INTERFACE  CONFIG
##                                                         
##  These are parameters for the raw interface plugin.     
##  This plugin is used to define generic methods for the  
##  the other interface plugins (which define class        
##  methods for their object class)                        
##                                                         
##  DO NOT EDIT THIS FILE WHILE RUNNING radR, because      
##  it is overwritten with new values when you exit        
##  radR.                                                  
##                                                         
##  Lines beginning with "#" and blank lines are preserved 
##  but comments at the ends of lines are not.             
##                                                         
##  Make sure to include a comma at the end of each item   
##  in a list.  A list definition should look like this:   
##                                                         
##     what.ever = list (                                  
##        something = 45,                                  
##        something.else = 55                              
##     )                                                   
##                                                         
##  The first and last lines must not have any actual list 
##  items on them, and all items must be named             
##  (i.e. NAME = THING, instead of just THING)             


enabled = TRUE

## should we compress data when creating a raw archive?
## This probably should not be enabled unless substantial
## portions of the scan are zero, either due to exclusion
## zones, or noise-threshold filtering.

## This uses the mini LZO library (http://www.oberhumer.com/opensource/lzo/)
## which is lossless, fast on compression, and very fast on decompression.

compress = TRUE

