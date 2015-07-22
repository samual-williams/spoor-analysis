# Script to bootstrap spoor frequency data, written by Phil Stephens (http://community.dur.ac.uk/philip.stephens/welcome.htm)
rm(list=ls())	# clears everything from memory
machine <- 3	# 1 for PS external drive, 2 for PS PC, 3 for other, 4 for PS new folder

# set working directory
sources <-c (	"E:/Phil/Teaching, Durham/Projects and lit reviews/2009-10/Henderson/R code",
              "C:/Documents/Teaching, Durham/Projects and lit reviews/2009-10/Henderson/R code/Examples",
              "/Users/Sam/R_working_local/spoor_paper",
              'C:\\Users\\Phil\\Documents\\Statistics\\Sam Williams')	# the 3rd one could be modified and used with source=3

setwd(sources[machine])
# the working directory is now set to the directory specified by number "machine" in the vector "sources"
Distance.driven=695.7		
reps=20000		# reps is the number of bootstrap samples to be used.  
			# 5000 usually does the trick but to get the smoothest lines, you might 
			# want to use more (e.g. 10000 or 20000 - but remember, they'll take a long time
			# to run and, hence, to create the figures).

# set the resolution at which you want to assess sample size (currently a sequence
# from 10 to 200 in jumps of 10).
spoors <- seq(2,100,2)

# import data
data <- read.csv("All spp data.csv",header=T)

# make a list of the unique species in the data file
Spp <- unique(data[,2])

# apply a function to each species and return the Sample sizes with low and high confidence intervals
results <- lapply(Spp, function(S){
  dat <- subset(data, Species==S)  	# identify the data relevant to the focal species
  Dist <- max(dat[,1])					# find highest distance at which spoor was found
  dat[,1] <- dat[,1]+Distance.driven-Dist	# add the "missing" distance onto the 1st data point
  n <- dim(dat)[1]					# count the number of data points
  starts <- c(0,dat[1:(n-1),1])			# make an offset vector, starting at zero
  intervals <- dat[,1]-starts			# create a table of inter-spoor intervals
  true.mean <- mean(intervals)			# find the mean spoor frequency
  CIs <- lapply(spoors, function(sample.size){
    ###! Now the key line that actually does the bootstrapping !###
    BS <- replicate(reps,mean(sample(intervals,sample.size,replace=T)))
    return(data.frame(sample_size_ci=sample.size, low_ci=quantile(BS,0.025), high_ci=quantile(BS,0.975), species=as.character(S)))
  })
  CIs <- do.call('rbind', CIs)
  row.names(CIs) <- NULL
  return(CIs)
})

results <- do.call('rbind', results)
write.table(results,'ci_combined.csv',row.names=F,sep=',')

# Make plot 

library(ggplot2)
ci_combined <- read.csv("ci_combined.csv")
spoor_frequencies_combined <- read.csv("spoor_frequencies_combined.csv")

frequency_plot <- ggplot(spoor_frequencies_combined, aes(x=sample_size_freq, y=spoor_frequency)) + xlab("Sample size") + ylab("Spoor frequency") + theme_bw() 
frequency_plot <- frequency_plot + geom_point(size=1.5) + xlim(0,100) +
        facet_wrap( ~ species, scales="free_y", ) + geom_line(aes(x=sample_size_ci, y=high_ci), data=ci_combined) + 
        geom_line(aes(x=sample_size_ci, y=low_ci), data=ci_combined)
frequency_plot # prints plot 
