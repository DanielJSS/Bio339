#I already loaded the packages tropfish and openxlx----

# Now I will read in the exek file, calling it tiger  
Tiger<-read.xlsx("Tiger-Kariba 1 cm.xlsx",sheet=1,detectDates=TRUE) 
show(Tiger)


#I will aggregate the data and make a simple barplot looking at it 

Tiger.agg<-apply(Tiger[,-1],1,sum)
barplot(Tiger.agg,names.arg=Tiger$Length.classes)

#Here we can see that there is alot of smaller fish, and the max size seems to be 62 cm. 


# Creating list object with dates of observations, length classes, and the length frequency data
# This is what ELEFAN in TropFishR uses
Tiger_d<-list(dates = as.Date(names(Tiger)[2:ncol(Tiger)]),		# dates, should be of class "Date" 
        midLengths = Tiger$Length.classes, 				# name of the column with length class midpoints)
        catch = as.matrix(Tiger[, -1]))					# all except the first column

class(Tiger_d) <- "lfq" 				# giving the list an "lfq setting"
Tiger_d$catch[is.na(Tiger_d$catch)] <- 0	# this would fill empty cells with zeros - not relevant here

# have a look:
str(Tiger_d)


#Now we can plot this 
plot(x=Tiger_d, 
     Fname = "catch",
     date.axis = "modern", 
     hist.sc = 0.9, 
     ylim = c(8, 30), 
     ylab = "length (mm)", # naming y-axis
     xlab = "") # naming x-axis

#Following the lines of this plot is apperse to be about 2 or 3 differnt cohorts/age gorupsgroups 


### 2. Restructuring data into the ELEFAN 1 points system with TropFishR----

#After creating lfq data, the data has to be restructured into the ELEFAN 1 points system. For more information regarding this points system see the paper by Pauly and David (1981): http://legacy.seaaroundus.s3.amazonaws.com/doc/Researcher+Publications/dpauly/PDF/1981/Journal+Articles/ELEFAN_I_BasicProgramObjectiveExtractionGrowthParametersLeng.pdf.

# The MA value should be about the same as the bin width of the smallest cohort. Look at the plot and select an appropriate moving average. It must be an odd number. 
MA.window<-7
Tiger_d.MA <- lfqRestructure(Tiger_d,
                       MA=MA.window, 
                       addl.sqrt = FALSE)
#Plotting the restructured data set
plot(Tiger_d.MA, 
     Fname="rcounts", 
     date.axis="modern", 
     ylim = c(8, 30))


# Now we can estimate the differt values---- 

### 3. Estimating initial input-values for the input parameters in the ELEFAN analysis 

#The asymptotic length (Linf)
#Using the Lmax approach for estimating Linf, where the biggest value for L is used for estimation. This is done by taking the largest individual and timing it by 0.95, giving us the average of the maximum theoretical fish. 
#The largest fish here was 62 cm, so we multiply with 0.95
(Linf.ini<-0.95*max(Tiger$Length.classes))
#We then get 58.9 cm as the maximum Linf 

# We do not have information on K. If we assume that shrimp approach thei maximum size quite fast, then we should chooce a relatively high K. 
(K.ini<- 0.327)
#here we can also try differnt starting values of k to see what is better, since these fish are not so large nayvbe k needs to be higer 

# t_anchor should correspond to peak spawning as a fraction of year. 
#I tried to look up peak spaning seaon of tigherfish in africa and i found that it occurs between september and november 
#Which means its eihter 0.75 0.833 or 0.916 so we can try differnt ones and see
(t_anchor.ini =0.916)



#The ELEFAN analysis----

# running analysis with simulated annealing (SA) [a particular optimization method]
Tiger.SA <- ELEFAN_SA(Tiger_d.MA, seasonalised = FALSE, 
                  init_par = list(Linf = Linf.ini, K = K.ini, t_anchor = t_anchor.ini ),
                  SA_time = 60*1,
                  SA_temp = 1e+05, maxit = 750, MA = MA.window, addl.sqrt
                  = FALSE, agemax = NULL, flagging.out = TRUE)
# Estimated parameters:
Tiger.SA$par
plot(Tiger.SA,ylim = c(0, 50))

#This is the output 
#$Linf
#[1] 57.19784

#$K
#[1] 0.06450569 #THis does not seem right 

#$t_anchor
#[1] 0.6939415

#$phiL
#[1] 2.324357


### 5. Scrutinizing
# You should try to see to what extent the smoothing window and the starting values matter
# You should try to see to what extent the results are biologically reasonable

# The analysis seems to find an optimum that corresponds to Linf that are is relatively low.
# If tweaking the controls does not change the situation, and you are convinced that Linf should be higher, then:
# We can constrain the analysis - notice the new input "low_par", which specifies minimum accepted values
Tiger.SA <- ELEFAN_SA(Tiger_d.MA, seasonalised = FALSE, 
                  init_par = list(Linf = Linf.ini, K = K.ini, t_anchor = t_anchor.ini ),
                  low_par = list(Linf = Linf.ini*0.9, K = K.ini*0.5, t_anchor = 0),
                  SA_time = 60*1,
                  SA_temp = 1e+05, maxit = 750, MA = MA.window, addl.sqrt
                  = FALSE, agemax = NULL, flagging.out = TRUE)

Tiger.SA$par
plot(Tiger.SA,ylim = c(0, 50))


###Result from k=0.5 and Lanchor =0.75
#$Linf
#[1] 56.66826

#$K
#[1] 0.2984602

#$t_anchor
#[1] 0.6349158

#$phiL
#[1] 2.981566

###Result from k =0.7 and t_anchor is 0.83
#$Linf
#[1] 58.16107

#$K
#[1] 0.3802656

#$t_anchor
#[1] 0.9864921

#$phiL
#[1] 3.109352


###result from k=0.4
#$Linf
#[1] 61.18941

#$K
#[1] 0.2555769

#$t_anchor
#[1] 0.5430696

#$phiL
#[1] 2.980874


###FINAL ANSWERE
##Result from k=0.3 seems to give the best result, the L-infinite seems to fit, the k is rather low, and the plot seems to hit where it is supoosed to

#$Linf
#[1] 61.33736

#$K
#[1] 0.2401412
#Given how they become about 41 (105 cm) inches at max size and grow 4-6 inches a year this might be appropriate sinze they dont grow that fast 

#$t_anchor
#[1] 0.3871596

#$phiL
#[1] 2.955917





