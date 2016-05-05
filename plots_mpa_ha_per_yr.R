# Coral Triangle Project
# goal: plot hectares/yr protected by mpas 
# by: mikaela provost
# last modified: May 4, 2016

library(stringr)
data <- read.csv("C:/Users/provo/Documents/GitHub/Coral_Triangle/data/mpa_profiles.csv", header=T)
head(data)
summary(data)
data[data==""] <- NA # put 'NA' into empty cells
data <- data[-which(is.na(data$Date.Established)), ]


#split date column into 3 columns: yr, month, day
date <- data.frame(str_split_fixed(data$Date.Established, "/", 3))
colnames(date) <- c("mo","day","yr")
data <- cbind(data, date)

# plot total hectares for each year (all countries)
    # two columns: yr, total hectares 
    t <- subset(data, select=c("yr","Reported.Area_ha"))
    yr.totals <- aggregate(t$Reported.Area_ha, by=list(t$yr), FUN=sum)

    # need to fill out yr column so that have every yr, even if no mpas built
    y <- seq(from=1900, to=2015, by=1)
    area.added <- rep(0,length.out = length(y))
    all.y <- data.frame(cbind(y,area.added))
    all.y$area.added <- yr.totals$x[match(all.y$y, yr.totals$Group.1)]
    all.y[is.na(all.y)] <- 0 #if no mpas were built, put zero for that yr
    
    # get regression line between yr and hectares
    reg1 <- lm(all.y$area.added ~ all.y$y)

    # plot
    #remove points showing zero hectares 
    plot(all.y$y, all.y$area.added, ylab="hectares / yr", xlab="year", 
     main="all CT countries")
    abline(reg1)
    reg1

  


