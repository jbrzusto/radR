library(png)
library(lubridate)

dyn.load("./ocr.so")
for (f in dir("/mnt/wind/home/data/benoit_gineste/RADAR_1_1930_1950/", pattern="^2014.*.png$", full.names=TRUE)) {
    x = readPNG(f, native=TRUE)
    dim(x)=rev(dim(x))
    tsi = x[1100:1280,990:1024]
    print(strptime(.Call("ocr", tsi, as.integer(c(35, 181, 1, 3, 2, 0x00ff0000)))[[1]], "%d.%m.%y %H:%M:%S"))
}


