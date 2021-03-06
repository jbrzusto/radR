## svn $Id$
##                                                        
##     ZONE SPECIFICATION FILE
##                                                        
##  These are parameters describing exclusion and special zones.
##                                                        
##  DO NOT EDIT THIS FILE WHILE RUNNING radR, because     
##  it may be overwritten with new values when you quit radR.
##                                                        
##  Lines beginning with "#" and blank lines are preserved
##  but comments at the ends of lines are not.            
##                                                        
##  Make sure to include a comma at the end of each item  
##  in a list, except the last.                           
##                                                        
##  NOTE:  all elements of lists must be given names      
##         e.g.   list(x=1, b=2) instead of list(1, 2)    
##                                                        
##

## Description (for humans) of what zones this file defines.

desc = "a set of zones for testing"

## The list of defined zones.  Tags can be any words, but "exclusion" is treated
## specially to mean a zone where radR should not look for targets.
## Each zone is a strictenv with items as shown below:

zones = list ( 

  exclusion = list ( 

    ## is this zone enabled?
    enabled = FALSE,

    ## is this zone visible
    visible = FALSE,

    ## A region is a union of segments, each of which is described by
    ## inner and outer radii, an angle, and an angular extent.  The
    ## segment is treated as extending between the two radii and from
    ## the angle clockwise for the number of degrees given by the
    ## extent.

    ## The default zone has just one segment, so the following items
    ## are vectors of length one.
    
    ## the inner and outer radii for segments (in metres)
    
    r = list ( 
      r1 = 0,
      r2 = 500 
      ),

    ## the first and second angles for segments (in degrees clockwise from North)
    ## Note that for extent, 0 and 360 have different meanings.
    
    a = list ( 
      start = 0,
      extent = 360 
      ),

    colour = "red"
    ),

  ## a special zone
  
  special1 = list (
    
    enabled = TRUE,
    visible = TRUE,
    r = list ( 
      r1 = c(477.700837635625, 252.199842206248),
      r2 = c(955.40167527125, 502.199842206248) 
      ),
    a = list ( 
      start = c(297.816730152382, 343.031411220996),
      extent = c(117.816730152382, 30) 
      ),
    colour = "blue"
    ),

  ## another special zone
  
  special2 = list ( 
    enabled = TRUE,
    visible = TRUE,
    r = list ( 
      r1 = c(516.000242248005, 516.000242248005, 761.433680631478),
      r2 = c(999.062969938753, 766.000242248005, 1011.43368063148) 
      ),
    a = list ( 
      start = c(121.750719451679, 234.127784673492, 91.683514162352),
      extent = c(111.572709060049, 23.3225978219588, 30) 
      ) ,
    colour = "yellow"
    ),

  ## a third special zone
  
  special3 = list ( 
    enabled = TRUE,
    visible = TRUE,
    r = list ( 
      r1 = c(1487.71006920031, 1487.71006920031, 1487.71006920031),
      r2 = c(1992.81365159917, 1992.81365159917, 1992.81365159917) 
      ),
    a = list ( 
      start = c(301.705475338762, 184.899859154952, 45.6298840372247),
      extent = c(111.572709060049, 111.572709060049, 111.572709060049) 
      ) ,
    colour = "orange"
    ) 
  ) 




