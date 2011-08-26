## This is a seascan radar description file, which describes the operating and
## sampling modes available for a particular radar accessed via SeaScan.

## The name for this radar

name = "Furuno Model 1824C"

## The mode table for this radar (explained below) The last 2 columns are
## ignored because of the '##' but are provided here for convenience.
## This table may contain up to 31 rows.

mode.table.string = "

      PRF.Hz  PLEN.ns   NPULSE   RES.m    NCELL        ##   RANGE.m   MEGABYTES
                                                       ##
 1     2100     80       4096     2.5      256         ##      640        2   
 2     2100     80       4096     2.5      512         ##     1280        4        
 3     2100     80       4096     2.5      768         ##     1920        6       
 4     2100     80       4096     2.5     1024         ##     2560        8        
 5     2100     80       4096     5.0      256         ##     1280        2        
 6     2100     80       4096     5.0      512         ##     2560        4        
 7     2100     80       4096     5.0      768         ##     3840        6        
 8     2100     80       4096     5.0     1024         ##     5120        8   
 9     2100     80       4096     7.5      256         ##     1920        2        
10     2100     80       4096     7.5      512         ##     3840        4       
11     2100     80       4096    15.0      256         ##     2560        2   
12     2100     80       4096    15.0      512         ##     5120        4   
13     1200    300       3072     5.0      256         ##     1280        1.5 
14     1200    300       3072     5.0      512         ##     2560        3   
15     1200    300       3072     5.0      768         ##     3840        4.5 
16     1200    300       3072     5.0     1024         ##     5120        6   
17     1200    300       3072     5.0     1280         ##     6400        7.5 
18     1200    300       3072    15.0      256         ##     2560        1.5 
19     1200    300       3072    15.0      512         ##     5120        3   
20     1200    300       3072    15.0      768         ##     7680        4.5 
21     1200    300       3072    15.0     1024         ##    10240        6   
22     1200    300       3072    15.0     1280         ##    12800        7.5 
23      600    800       1536     5.0      256         ##     1280        0.75
24      600    800       1536     5.0      512         ##     2560        1.5 
25      600    800       1536     5.0      768         ##     3840        2.25
26      600    800       1536    15.0      512         ##     5120        1.5 
27      600    800       1536    15.0      768         ##     7680        2.25
28      600    800       1536    15.0     1024         ##    10240        3   
29      600    800       1536    20.0      512         ##    10240        1.5 
30      600    800       1536    20.0      768         ##    15360        2.25
31      600    800       1536    20.0     1024         ##    20480        3   

"

## Column variables:
##    PRF.Hz:     nominal pulse repetition frequency in Hz
##    PLEN.ns:    pulse length in nanoseconds
##    NPULSE:     number of pulses digitized per scan
##    RES.m:      resolution (i.e. range cell size) in metres
##    NCELLS:     number of range cells per pulse
##    RANGE.m:    total range in metres
##                IGNORED since it equals  RES.m * NCELLS
##    MEGABYTES:  total memory required per scan, in megabytes
##                IGNORED since it equals NPULSE * NCELL * 2;
##                these parameters must be such that MEGABYTES <= 8.0


## The Furuno 1824 has three nominal PRFs:  600, 1200, 2100
## and three pulse lengths: 80, 300, 800 nanoseconds.
##
## The Furuno control unit allows these combinations:

##   RANGE     PRF    PLEN
## (n.miles)    Hz   nanosecs 
##   0.125     2100    80
##   0.250     2100    80
##   0.5       2100    80
##   0.75      2100    80
##   1.0       2100    80  *
##   1.5       2100    80  
##   1.5       1200   300  
##   2.0       1200   300  *
##   3.0       1200   300 
##   3.0        600   800  
##   4.0        600   800  *
##   6.0        600   800
##   8.0        600   800
##  12.0        600   800
##  16.0        600   800
##  24.0        600   800
##  36.0        600   800
##  48.0        600   800
##  72.0        600   800

## The rows marked * are sufficient to generate all the
## prf/plen combinations for the Furuno:  use the Range +/-
## button to choose one of them.

## Note that the RANGE value chosen on the Furuno unit does not
## directly affect the range of data obtained by Seascan.  That's what
## the table below is about.

## For SeaScan, the following constraints apply:
## 
##    RES.m: seascan allows these sampling rates and corresponding
##           range cell sizes:
##
##	Sampling Rate:       2.5,  5,  7.5,  10,  20,  30,   40,   60   Mhz 
##	Range Cell Size:     60,   30 , 20 , 15 , 7.5 , 5   3.75 , 2.5  m  

##    NCELLS: seascan allows these numbers of range cells (i.e. samples per pulse):
##            256, 512, 768, ..., 4096 (i.e. multiples of 256)

##    RANGE.m: this must equal RES.m * NRANGE and is included for convenience.
##             It is recalculated by radR.

##    MEGABYTES: this equals 2 * NCELLS * NPULSE, and is recalculated by radR.

##    NPULSE: seascan allows 256, 512, 768, ..., 4096 (i.e. multiples of 256)

##    Seascan's maximum gated image size is 8 megabytes (or smaller,
##    if set in SeaScan0.ini), therefore any mode which requires more
##    than that is getting duplicated pulse data as seascan.
##    i.e. NPULSE * RANGE.m / RES.m * 2 should be <= 2^23 == 8388608
##    (the factor of 2 is due to 12-bit samples requiring 2 bytes for storage)

##    For each value of PRF, exactly one value of PLEN.ns and NPULSE is allowed:
##    i.e. all rows having the same PRF must also have the same PLEN.ns and NPULSE
##    If you need more choices than that, you can generate multiple SeaScan0.bin
##    files and switch between them manually by restarting SeaScan.

## What follows is a space-delimited table, read by R

## Each row is a sampling mode; ie. a permitted combination of values for
## the column variables.

## You may choose any combinations of RES.m and RANGE.m for each of the three
## allowed combinations of PRF.HZ and PLEN.ns, so long as the total number
## of combinations (i.e. sampling modes) is <= 31

## I've set up what seem like reasonable combinations to start with, using
## longer pulses for greater ranges, and trying to get as close to the
## full set of pulses per scan (for a 25 rpm antenna) as Seascan allows.

