## testing biglist module
library(biglist)

bl <- biglist("mylistfile.dat", names=c("id", "numbers"), overwrite=TRUE, cache.size=5)
for (i in 20:1) {
  x <- seq(length=i)
  bl[[i]] <- list(id=paste("id", i, sep=""), numbers=x)
}
print(bl)
flush(bl)
print(sprintf("Size before writing element 21     : %d\n", file.info("mylistfile.dat")$size))
c(bl, list(a=1:5,b=c("test", "me", "again"), e=34+5i))

for (i in 4*(1:10)) {
  print(paste("bl[[", i, "]]="))
  print(bl[[i]])
}
print(length(bl))
flush(bl)
rm(bl)
invisible(gc())
bl <- biglist("mylistfile.dat", overwrite=FALSE)
for (i in 3*(1:10)) {
  print(paste("bl[[", i, "]]="))
  print(bl[[i]])
}

print(bl[[101]])

print(names(bl))
flush(bl)
print(length(bl))
bl[[98]]<-"I am element 98"
flush(bl)
print(sprintf("Size before writing elements 103,104: %d\n", file.info("mylistfile.dat")$size))
for (i in 103:104)
  bl[[i]] <- cos(1:10)
flush(bl)
print(sprintf("Size after writing elements 103,104: %d\n", file.info("mylistfile.dat")$size))
bl[[102]] <- sin(1:20)
flush(bl)
print(sprintf("Size after writing element 102     : %d\n", file.info("mylistfile.dat")$size))
for (i in 105:106)
  bl[[i]] <- tan(1:10)
flush(bl)
print(sprintf("Size after writing elements 105,106: %d\n", file.info("mylistfile.dat")$size))
rm(bl)
bl <- biglist("mylistfile.dat", cache.size = 7)
length(bl) <- 101
flush(bl)
print(sprintf("Size after resetting length to 101:  %d\n", file.info("mylistfile.dat")$size))
bl[[99]] <- 1:1000
flush(bl)
print(sprintf("Size after rewriting elt 99:  %d\n", file.info("mylistfile.dat")$size))
drop.items(bl, c(99, 101))
flush(bl)
print(sprintf("Size after dropping elt 99, 101:  %d\n", file.info("mylistfile.dat")$size))

