require(ggplot2)
require(MASS)

source("fit-histograms.R")

LoadPrudentialData <- function(saveFileName) {
	# Load the prudential histogram source.
	prudentialDataText <- readLines(file("~/Documents/prudential-stickers/web-source.html"))
	getAgeCount <- function(age, text = prudentialDataText) {
		str <- paste0("<div class=\"bdot_age\">", age, "</div>")
		str <- paste0("] = \"", age, "\";")
		return(length(grep(str, text)))
	}
	prudData <- data.frame(age = 1:150)
	prudData$count <- sapply(prudData$age, getAgeCount)
	# Trim the data to not include the zeros.
	dataRange <- range(prudData[prudData[["count"]] > 0,"age"])
	prudData <- subset(prudData, age >= dataRange[1] & age <= dataRange[2])
	save(file = saveFileName, list = c("prudData"))
	return(prudData)
}

#----------------------------------------------
# Plot the prudential data histogram.
#----------------------------------------------
prudFile <- "~/Documents/prudential-stickers/prudential.Rdata"
if (file.exists(prudFile)) {
	load(prudFile)
} else {
	prudData <- LoadPrudentialData(prudFile)
}


#----------------------------------------------
# Load french data.
#----------------------------------------------
frenchData <- read.csv("~/Documents/prudential-stickers/2010populationFrance.csv")
colnames(frenchData)[1] <- "age"
# Plot the distribution.
dev.new()
ggplot() + geom_histogram(data = data.frame(age = rep(frenchData$age, frenchData[["X2010"]])), aes(x = age, y = ..count..), alpha = 0.25, fill = "blue", color = "darkblue", binwidth = 1)
#ggsave("~/Documents/prudential-stickers/french-age-hist.jpg", width = 9, height = 9)

#----------------------------------------------
# Fit the data.
#----------------------------------------------
ageData <- french[ ,c("age", "X2010")]
colnames(ageData)[2] <- "prob"
empData <- data.frame(age = rep(prudData$age, prudData$count))
# Fit the exponential
N <- 1e5
M <- 10
saveFileName <- sprintf("~/Documents/prudential-stickers/fitResults_N1e%d_M%d.Rdata", log10(N), M)
if (file.exists(saveFileName)) {
	load(saveFileName)
} else {
	fitResults <- GenerateFits(saveFileName, N, M)
}

#----------------------------------------------
# Plot the prudential data histogram.
#----------------------------------------------
dev.new()
ggplot() + geom_histogram(data = data.frame(age = rep(prudData$age, prudData$count)), aes(x = age, y = ..count..), alpha = 0.25, fill = "blue", color = "darkblue", binwidth = 1)
#ggsave("~/Documents/prudential-stickers/prud-hist.jpg", width = 9, height = 9)


#----------------------------------------------
# Create the k and maxAge data frame
#----------------------------------------------
distributions <- unique(unlist(fitResults[ ,"distribution"]))
randData <- data.frame()
ageData <- data.frame()
for (dist in c("delta", "exponential", "lognormal")) {
	temp <- fitResults[distributions == dist, ]
	randData <- rbind(randData, data.frame(k = temp$k, distribution = dist))
	ageData <- rbind(ageData, data.frame(maxAge = temp$maxAge, distribution = dist))
}

#----------------------------------------------
# Plot the k distributions
#----------------------------------------------
dev.new()
ggplot(data = randData) + geom_density(aes(x = k, y = ..density.., fill = distribution, color = distribution), alpha = 0.25, kernel = "cosine") + coord_cartesian( xlim = c(0, 1000))# + facet_wrap(~ distribution, ncol = 2)#  + coord_trans(xtrans = "log10", limx = c(1, 1e4)) #xlim(0, 1000)
ggsave("~/Documents/prudential-stickers/probability-densities.jpg", width = 9, height = 9)

dev.new()
ggplot(data = subset(randData, distribution %in% c("exponential", "lognormal"))) + geom_density(aes(x = k, y = ..density.., fill = distribution, color = distribution), alpha = 0.25, kernel = "cosine") + coord_cartesian( xlim = c(0, 1000))# + facet_wrap(~ distribution, ncol = 2)#  + coord_trans(xtrans = "log10", limx = c(1, 1e4)) #xlim(0, 1000)
ggsave("~/Documents/prudential-stickers/probability-densities.jpg", width = 9, height = 9)

#----------------------------------------------
# Plot the maximum age distributions
#----------------------------------------------
dev.new()
ggplot(data = subset(ageData, maxAge > 65), aes(depth, fill = distribution)) + geom_density(aes(x = maxAge, y = ..density.., fill = distribution, color = distribution), alpha = 0.5, binwidth = 1, kernel = "cosine") + geom_histogram(data = data.frame(age = rep(prudData$age, prudData$count)), aes(x = age, y = ..density..), alpha = 0.25, fill = "black", color = "black", binwidth = 1)
ggsave("~/Documents/prudential-stickers/age-fits.jpg", width = 9, height = 9)

