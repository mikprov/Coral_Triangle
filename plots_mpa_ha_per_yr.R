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
all.y <- data.frame(cbind(y,area.added))
all.y$area.added <- yr.totals$x[match(all.y$y, yr.totals$Group.1)]
all.y[is.na(all.y)] <- "" #if no mpas were built, put zero for that yr


pdf(file="C:/Users/provo/Documents/GitHub/Coral_Triangle/all_CT_countries_5.6.2016.pdf")
plot(all.y$y, all.y$area.added, ylab="hectares / yr", xlab="year",
           main="all CT countries")
abline(reg1)
dev.off()

# create table of hectares added per country
# columes: yr, indonesia, Malaysia, Papua New Guinea,
# Philippines, Solomon Islands, Timor-Leste
# Indonesia
i <- subset(data[data$Country == "Indonesia",], select=c("yr","Reported.Area_ha"))
i.totals <- aggregate(i$Reported.Area_ha, by=list(i$yr), FUN=sum)

# Malayasia
m <- subset(data[data$Country == "Malaysia",], select=c("yr","Reported.Area_ha"))
m.totals <- aggregate(m$Reported.Area_ha, by=list(m$yr), FUN=sum)

# Papua New Guinea
p <- subset(data[data$Country == "Papua New Guinea",], select=c("yr","Reported.Area_ha"))
p.totals <- aggregate(p$Reported.Area_ha, by=list(p$yr), FUN=sum)

# Philippines
pp <- subset(data[data$Country == "Philippines",], select=c("yr","Reported.Area_ha"))
pp.totals <- aggregate(pp$Reported.Area_ha, by=list(pp$yr), FUN=sum)

# Solomon Islands
s <- subset(data[data$Country == "Solomon Islands",], select=c("yr","Reported.Area_ha"))
s.totals <- aggregate(s$Reported.Area_ha, by=list(s$yr), FUN=sum)

# Timor-Leste
t <- subset(data[data$Country == "Timor-Leste",], select=c("yr","Reported.Area_ha"))
t.totals <- aggregate(t$Reported.Area_ha, by=list(t$yr), FUN=sum)

# need to fill out yr column so that have every yr, even if no mpas built
Indonesia <- rep(0,length.out = length(y))
Malaysia <- rep(0,length.out = length(y))
PapuaNewGuinea <- rep(0,length.out = length(y))
Philippines <- rep(0,length.out = length(y))
SolomonIslands <- rep(0,length.out = length(y))
TimorLeste <- rep(0,length.out = length(y))

all.countries <- data.frame(cbind(y,Indonesia, Malaysia,
                                  PapuaNewGuinea, Philippines,
                                  SolomonIslands, TimorLeste))

all.countries$Indonesia <- i.totals$x[match(all.countries$y, i.totals$Group.1)]
all.countries$Malaysia <- m.totals$x[match(all.countries$y, m.totals$Group.1)]
all.countries$PapuaNewGuinea <- p.totals$x[match(all.countries$y, p.totals$Group.1)]
all.countries$Philippines <- pp.totals$x[match(all.countries$y, pp.totals$Group.1)]
all.countries$SolomonIslands <- s.totals$x[match(all.countries$y, s.totals$Group.1)]
all.countries$TimorLeste <- t.totals$x[match(all.countries$y, t.totals$Group.1)]


all.countries[is.na(all.countries)] <- 0 #if no mpas were built, put zero for that yr
write.csv(all.countries, file ="C:/Users/provo/Documents/GitHub/Coral_Triangle/all_CT_countries_5.6.2016.csv")
