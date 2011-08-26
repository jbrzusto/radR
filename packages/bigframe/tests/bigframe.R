## tests for the bigframe package

library("bigframe")

x <- data.frame(a=1:10, b=sin(1:10), c=1==1:10, d=complex(r=-(1:10), i=10:1), s1=I(paste("i", as.character(10:1), sep="")), s2=I(paste("jj", as.character(1000*(10:1)), sep="")))
fr <- bigframe("myfile.dat", x, list(info="this is my object", weird=cos(10:1)), cache.size = 15)
print(fr[])
fr[13,] <- data.frame(a=3:1, b=sin(3:1), c=rep(TRUE, 3), d=complex(r=3:1,i=1:3),s1=I("testing"), s2=I("moreover"))
print(fr[])
fr[20,] <- data.frame(a=22:20, b=sin(22:20), c=rep(TRUE, 3), d=complex(r=22:20,i=-(22:20)),s1=I("zxcv"), s2=I("qwerty"))
rbind.bigframe(fr, data.frame(50:55, cos(50:55), rep(NA, 6), complex(r=55:50, i = 50:55), I("google"), I("yahoo.com")))
print(fr[])
savefr<-fr[]
rm(fr)
fr <- bigframe("myfile.dat")
print(attr(fr, "header"))
print(dim(fr))
print(fr[])
print(identical(fr[], savefr))
