### vectors, data, matrices, subsetting
x=c(2,7,5)
x
y=seq(from=4,length=3,by=3)
?seq
y
x+y
x/y
x^y
x[2]
x[2:3]
x[-2]
x[-c(1,2)]
z=matrix(seq(1,12),4,3)
z
z[3:4,2:3]
z[,2:3]
z[,1]
z[,1,drop=FALSE]
dim(z)
ls()
rm(y)
ls()

#################
#      Plot 
################

### Generating random data, graphics
x=runif(50)
y=rnorm(50)
plot(x,y)
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue")
plot(x,y)

#################
# PAR,Layout
################

attach(mtcars)
#render image using par , layout
par(mfrow=c(3,2)) #place  images and show  in 3 rows and 2 columns
hist(wt)
hist(mpg)
hist(disp)

#layout (mat )is a matrix object specifying the location of the N figures to plot.
# One figure in row 1 and two figures in row 2
# row 1 is 1/3 the height of row 2
# column 2 is 1/4 the width of the column 1 
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), 
       widths=c(1,1), heights=c(1,1))
hist(wt)
hist(mpg)
hist(disp)
        
#################
#      PAIRS
################
d1<-c(1,2,3)
d2<-c(4,5,6)
d3<-c(7,1,9)
df<-data.frame(d1,d2,d3)
pairs(df,col="brown")

################
#   CPAIRS
###############
dta <- mtcars[c(1,3,5,6)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

#################
# SCATTERPLOT
#################
# Another Spinning 3d Scatterplot
library(Rcmdr)
attach(mtcars)
scatter3d(wt, disp, mpg)


#################
#      DEV OFF
################

jpeg("images/lesson1.png")
plot(x,y)
dev.off()#disable  output to the terminal it returns image result to lesson1.png.
source("input/sample.R")#to refer another R package script
x=runif(50)

            #logger implementation 

#################
#      LOGGER
################

#first give logger file to log
#
sink()#revert output to console rather than logger.txt
y=rnorm(50)
#wont print in console it log to outpupt/logger.txt
# output directed to myfile.txt in cwd. output is appended
# to existing file. output also send to terminal. 
sink("output/logger.txt", append=TRUE, split=TRUE)
x
sink()
x