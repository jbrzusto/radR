## svn $Id$
##                                                         
##  Configuration for Simrad plugin.
##                                                         
##  DO NOT EDIT THIS FILE WHILE RUNNING radR, because      
##  it is overwritten with new values when you exit        
##  radR.                                                  

## is this plugin currently enabled when loaded?
enabled = TRUE 

## The following flag says that this plugin is enabled whenever
## it is loaded.  TRUE here makes the "enabled" parameter redundant.
always.enabled = TRUE

## pulses to capture per sweep
desired.pulses = 2048

## multicast address for obtaining radar data
multicast.addr = "236.6.7.8"

## multicast port for obtaining radar data
multicast.port = 6678
