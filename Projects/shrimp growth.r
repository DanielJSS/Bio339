# R script to estimate von Bertalanffy growth parameters for shrimp
# based on ELEFAN methods from the TropFishR package

### 0. Preliminary steps

# set working directory
setwd("./UiB/BIO339/growth exercise/")
getwd()

### 1a. Load necessary packages
library(TropFishR)
library(openxlsx) # other packages are also possible, but this works best here

### 1b. read in the data and put in the suitable format

# The data are in xlsx format. If you have library(openxlsx) working, then using it is the simplest approach, detailed below.
# Alternatively, you can open the file in Excel, save it in CSV format, and read in with read.csv().

# The length frequency data
# sheet 1 = "Raw Observations"
d0<-read.xlsx("BIO 339 Lenght Data shrimp.xlsx",sheet=1,detectDates=TRUE) 

# It is always to good to have a look on the data, to check that everything is looking OK
# Aggregate all data into a single length distribution for the sake of illustration
d0.agg<-apply(d0[,-1],1,sum)
barplot(d0.agg,names.arg=d0$Length.classes)

# Creating list object with dates of observations, length classes, and the length frequency data
# This is what ELEFAN in TropFishR uses
d<-list(dates = as.Date(names(d0)[2:ncol(d0)]),		# dates, should be of class "Date" 
	midLengths = d0$Length.classes, 				# name of the column with length class midpoints)
	catch = as.matrix(d0[, -1]))					# all except the first column

class(d) <- "lfq" 				# giving the list an "lfq setting"
d$catch[is.na(d$catch)] <- 0	# this would fill empty cells with zeros - not relevant here

# have a look:
str(d)

# What you see should like this:
# List of 3
 # $ dates     : Date[1:9], format: "1997-02-20" "1997-03-11" ...
 # $ midLengths: num [1:57] 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5 ...
 # $ catch     : num [1:57, 1:9] 0 0 0 0 0 0 0 0 0 0 ...
  # ..- attr(*, "dimnames")=List of 2
  # .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  # .. ..$ : chr [1:9] "1997-02-20" "1997-03-11" "1997-04-16" "1997-05-09" ...
 # - attr(*, "class")= chr "lfq"


# plotting the data with TropFishR. The parameter hist.sc affects the horizontal "stretch" of each histogram
plot(x=d, 
	Fname = "catch",
	date.axis = "modern", 
	hist.sc = 0.9, 
	ylim = c(8, 30), 
	ylab = "length (mm)", # naming y-axis
	xlab = "") # naming x-axis


### 2. Restructuring data into the ELEFAN 1 points system with TropFishR

#After creating lfq data, the data has to be restructured into the ELEFAN 1 points system. For more information regarding this points system see the paper by Pauly and David (1981): http://legacy.seaaroundus.s3.amazonaws.com/doc/Researcher+Publications/dpauly/PDF/1981/Journal+Articles/ELEFAN_I_BasicProgramObjectiveExtractionGrowthParametersLeng.pdf.

# Restructuring data and applying MA (moving average). 
# The MA value should be about the same as the bin width of the smallest cohort. Look at the plot and select an appropriate moving average. It must be an odd number. 
MA.window<-7
d.MA <- lfqRestructure(d,
	MA=MA.window, 
	addl.sqrt = FALSE)

#Plotting the restructured data set
plot(d.MA, 
	Fname="rcounts", 
	date.axis="modern", 
	ylim = c(8, 30))


### 3. Estimating initial input-values for the input parameters in the ELEFAN analysis 

#The asymptotic length (Linf)
#Using the Lmax approach for estimating Linf, where the biggest value for L is used for estimation. This is done by taking the largest individual and timing it by 0.95, giving us the average of the maximum theoretical fish. 
(Linf.ini<-0.95*max(d0$Length.classes))

# We do not have information on K. If we assume that shrimp approach thei maximum size quite fast, then we should chooce a relatively high K. 
(K.ini<- 0.5)

# t_anchor should correspond to peak spawning as a fraction of year. In the Barents Sea
(t_anchor.ini =0.75)

### 4. The ELEFAN analysis

# running analysis with simulated annealing (SA) [a particular optimization method]
d.SA <- ELEFAN_SA(d.MA, seasonalised = FALSE, 
	init_par = list(Linf = Linf.ini, K = K.ini, t_anchor = t_anchor.ini ),
	SA_time = 60*1,
	SA_temp = 1e+05, maxit = 750, MA = MA.window, addl.sqrt
	= FALSE, agemax = NULL, flagging.out = TRUE)
show(d.SA)

# Estimated parameters:
d.SA$par
plot(d.SA,ylim = c(0, 30))


### 5. Scrutinizing
# You should try to see to what extent the smoothing window and the starting values matter
# You should try to see to what extent the results are biologically reasonable

# The analysis seems to find an optimum that corresponds to Linf that are is relatively low.
# If tweaking the controls does not change the situation, and you are convinced that Linf should be higher, then:
# We can constrain the analysis - notice the new input "low_par", which specifies minimum accepted values
d.SA <- ELEFAN_SA(d.MA, seasonalised = FALSE, 
	init_par = list(Linf = Linf.ini, K = K.ini, t_anchor = t_anchor.ini ),
	low_par = list(Linf = Linf.ini*0.9, K = K.ini*0.5, t_anchor = 0),
	SA_time = 60*1,
	SA_temp = 1e+05, maxit = 750, MA = MA.window, addl.sqrt
	= FALSE, agemax = NULL, flagging.out = TRUE)

d.SA$par
plot(d.SA,ylim = c(0, 30))


# This gives a higher Linf and lower K (not so surprising, given that Linf and K are generally negatively correlated
# Another way to potentially improve the model is to allow for seasonality in growth.


# Using ELEFAN is an expert process!
# You will always get a result. You should use your knowledge about the system to evaluate the results and steer them towards reasonable.