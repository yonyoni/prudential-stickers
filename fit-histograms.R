#---------------------------------------
# Create distributions
#---------------------------------------
GetRandomNumbers <- function(N, m, shape, distribution) {
	# Return a random sample with mean and shape params.
	switch(distribution,
	delta = {
		return(rep(m, N))
	},
	normal = {
		return(rnorm(N, mean = m, sd = shape))
	},
	normalpositive = {
		un <- rnorm(N, mean = m, sd = shape)
		# make sure there are no negative numbers
		repeat {
			nNeg <- sum(un <= 0)
			if(nNeg == 0) {
				break;
			}
			un[un <= 0] <- rnorm(nNeg, mean = m, sd = shape)
		}
		# Normalize the variable shape
		un <- shape * un / sd(un)
		return(un)
	},
	lognormal = {
		return(rlnorm(N, meanlog = m, sdlog = shape))
	},
	exponential = {
		return(rexp(N, rate = 1 / m))
	},
	powerlaw = {
		upl <- runif(N)^(-1 / shape)
		return(upl * m / mean(upl))
	},
	return(c()))
}

PlotRandomNumbers <- function(data, logstr = "xy") {
	temp <- hist(data, plot = FALSE)
	plot(temp$mids, temp$density, log = logstr)
}
#---------------------------------------
# Likelihood measures
#---------------------------------------

CalculateLogLikelihoodUsingHist <- function(data1, xlabel, ylabel, data2) {
	# Calculated the log likelihood of the fitted data given a histogram of the emprical data.
	# First estimate the density.
	rData <- range(prudData[ data1[[ylabel]] > 0, xlabel]) 
    dataHist <- hist(rep(data1[["xlabel"]], data1[["ylabel"]]), breaks = rData[1] : rData[2], plot = FALSE)
    # Get the estimated density.
    x = dataHist$mids
    y = dataHist$density
    # Estimate the density of the data2 using (linear) interpolation.
    fittedDensity <- approx(x, y, data2)
    return(log(prod(fittedDensity)))
}

CalculateLogLikelihoodUsingDensity <- function(empDensity, data) {
	# Calculated the log likelihood of the fitted data given the emprical density.
	# Estimate the density of the data using (linear) interpolation.
	x <- empDensity[["x"]]
	y <- empDensity[["y"]]
    fittedDensity <- approx(x = x, y = y, xout = as.numeric(data), yleft = min(y), yright = min(y))
    return(sum(log(fittedDensity$y)))
}

CalculateLogLikelihoodUsingEmpData <- function(data1, data2) {
	# Calculated the log likelihood of data2 given the density of data1.
	empDensity <- density(data1)
	return(CalculateLogLikelihoodUsingDensity(empDensity, data2))
}

#---------------------------------------
# Create the age distribution.
#---------------------------------------
getMaxAge <- function(N, age = french$age , prob = french$X2010) {
	prob <- prob / sum(prob)
	return(max(sample(x = age, size = ceiling(N), prob = prob, replace = TRUE)))
}

#---------------------------------------
# Fit data.
#---------------------------------------

FitData <- function(ageData, empData, N, meanVals, shapeVals, distribution, fineGrid = TRUE, maxK = 1e4) {
	# Do this the non "R" way using loops instead of apply
	# Args:
	#   ageData: The age distribution to sample from. It has teh fields "age" and "prob".
	#   empData: The empirical age counts we are fitting to. It has a field "age" of observations.
	mOpt = 0
	sOpt = 0
	lOpt = -Inf
	maxAgeOpt <- c()
	kOpt <- c()
	for (m in meanVals) {
		for (s in shapeVals) {
			# Get the number of aqauintances for this distribution
			k <- GetRandomNumbers(N = N, m = m, shape = s, distribution = distribution)
			# Ensure the max number
			k <- sapply(k, function(x) min(maxK, x))
			maxAge <- sapply(k, getMaxAge, age = ageData$age, prob = ageData$prob)
			# Get the log likelihood of the data for this.
			l <- CalculateLogLikelihoodUsingEmpData(maxAge, as.integer(empData$age))
			# Check if this set of variables improves the fit.
			if (is.infinite(lOpt) || l > lOpt) {
				lOpt <- l
				mOpt <- m
				sOpt <- s
				kOpt <- k
				maxAgeOpt <- maxAge
			}
		}
	}
	# Check if to do a finer grid.
	if (fineGrid) {
		mIndex <- which(meanVals == mOpt)
		Nm <-length(meanVals)
		meanVals = seq(meanVals[max(mIndex - 1, 1)], meanVals[min(mIndex + 1, Nm)], length.out = Nm)
		sIndex <- which(meanVals == sOpt)
		Ns <- length(shapeVals)
		shapeVals = seq(shapeVals[max(sIndex - 1, 1)], shapeVals[min(sIndex + 1, Ns)], length.out = Ns)
		# A one time recursion.
		return(FitData(ageData, empData, N, meanVals, shapeVals, distribution, fineGrid = FALSE))
	}
	return(list(l=lOpt, m=mOpt, s=sOpt, distribution = distribution, maxAge = maxAgeOpt, k = kOpt))
}

GenerateFits <- function(saveFileName, N, M) {
	meanVals <- seq(1, 500, length.out = M)
	shapeVals <- seq(1, 500, length.out = M)
	fitResults <- FitData(ageData = ageData, empData = empData, N = N, meanVals = meanVals, shapeVals = c(1), distribution = "delta")
	cat("delta")
	fitResults <- rbind(fitResults, FitData(ageData = ageData, empData = empData, N = N, meanVals = meanVals, shapeVals = c(1), distribution = "exponential"))
	cat("exponential")
	fitResults <- rbind(fitResults, FitData(ageData = ageData, empData = empData, N = N, meanVals = meanVals, shapeVals = shapeVals, distribution = "normalpositive"))
	cat("normalpositive")
	fitResults <- rbind(fitResults, FitData(ageData = ageData, empData = empData, N = N, meanVals = log(meanVals), shapeVals = log(shapeVals), distribution = "lognormal"))
	cat("lognormal")
	fitResults <- rbind(fitResults, FitData(ageData = ageData, empData = empData, N = N, meanVals = meanVals, shapeVals = seq(0.5, 4.5, length.out = M), distribution = "powerlaw"))
	cat("powerlaw")
	# Save the data.
	save(file = saveFileName, list = c("fitResults", "meanVals", "shapeVals", "N", "M"))
	return(fitResults)
}