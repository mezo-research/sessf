#------------------------------------------------------------------
# SS Outputs and Diagnostics (R Script):
#
# This script can:
# (1) Read in SS output files;
# (2) Create results summary table, with RBCs;
# (3) Create output plots and save to file;
# (4) Create comparisons of multiple models (tables and plots).
#
# Configured for SESSF Stock Assessment 2015 
# By Athol Whitten, 2015, athol.whitten@mezo.com.au
#------------------------------------------------------------------

# Get latest (master) version of R4SS from Github:
# devtools::install_github("r4ss/r4ss")

# Open r4ss package:
library(r4ss)
library(knitr)

# Specify working folder (directory where subdirectories are model folders):
folder <- "C:/~/Morwong"

# Enter assessment year (year is model_end_year+1, RBC calculations are made for year+1):
year <- 2015

# Which plot groups from SS plots function required? (all = 1:24):
which_plots <- (1:24)

# Specify model names (folder names for each set of model files), set base case as first model:
model_names <- c("BC", "BC_EstM", "BC_R3", "BC_H2")

# Set max and min values for absolute correlation between estimated parameters, R4SS will report parameters with correlations above or below this level:
cormax <- 0.95
cormin <- 0.01

# Specify if summary tables of model comparisons should be saved to file (this summary shown to screen by default):
save_tables <- TRUE

# Specify if model comparison plots are required:
# NOTE: Plots shown to screen, if print_compare is TRUE, then plots are also printed to file).
compare_plots <- TRUE
print_compare <- TRUE

# Specify last year displayed for comparison plots and summary stats (may wish to ignore forecast period for example):
# NOTE: This is also the year for which the Long Term RBC will be calculated.
last_year <- 2030

# For spawning depletion plots, specify biomass target and biomass minimum threshold:
biomass_target    <- 0.48
biomass_threshold <- 0.20

# Enter TRUE/FALSE (1/0) for output options for each of the models listed above (same number of entries as number of models):
include <- c(1,1,1,1) 		    # Include these models as part of general outputs (plots and summary/comparison tables)
compare <- c(1,1,0,1)       	# Include these models in comparison plots

hessian  <- c(1,1,1,1) 		    # Which of the listed models have estimation turned on (and a positive definite hessian)?
forecast <- c(1,1,1,1)		    # Which of the listed models have forecasting turned on? 

show_plots  <- c(0,0,0,0)		# Show plots on screen for these models
print_plots <- c(1,1,1,1)    	# Print plots to file for these models

#-------------------------------------------------------------------
# End user controls, start main script:
#-------------------------------------------------------------------

# Create model directory array and print information to screen:
model_dirs <- array()

for(i in 1:length(model_names)){
	next_dir   <- paste(folder, model_names[i], sep="/")
	model_dirs <- append(model_dirs, next_dir)
}

model_dirs   <- model_dirs[-1]
model_string <- "Output sought from these model directories: \n"

for(i in which(include==TRUE)){
	next_dir     <- paste(folder, model_names[i], sep="/")
	model_string <- cat(model_string, next_dir, "\n")
}

nmods <- length(model_names)

# Create input/output control data frame and print to screen for reference:
iocontrol <- data.frame(cbind(model_names, include, forecast, hessian, show_plots, print_plots, compare))
print(iocontrol)

# Create tables to compare models, 'mdata' for model summaries, and 'ldata' for likelihood summaries:
mdata <- as.data.frame(as.list(1:8))
names(mdata) <- c("SB0", "SB_Curr", "CurrDepl", "RBC", "RBCLong", "N_Est_Pars", "LogLik", "LLDiff")

ldata <- data.frame()
tslist <- list()

# Make plots from SS report file/s and create tables to compare models:
for(i in which(include==TRUE)){
	replist     <- SS_output(dir=model_dirs[i], covar=hessian[i], forecast=forecast[i], cormax=cormax, cormin=cormin, printhighcor=50)
	n_est_pars 	<- replist$N_estimated_parameters
	log_lik	    <- round(replist$likelihoods_used[1,1], 0)
	
	if(i==1) base_ll <- log_lik
	ll_diff <- log_lik - base_ll

	time_series <- replist$timeseries
	SB0 <- round(replist$SBzero, 0)

	ts_SB     <- time_series[ ,c("Yr","SpawnBio")]
	ts_SB     <- cbind(ts_SB, "Depletion"=ts_SB$SpawnBio/(ts_SB$SpawnBio[1]))

	if(forecast[i]==TRUE){
		
		SB_curr   <- round(ts_SB[ts_SB$Yr==year + 1, "SpawnBio"])
	  	curr_depl <- round(ts_SB[ts_SB$Yr==year + 1, "Depletion"], 2)

		next_year <- replist$timeseries[replist$timeseries$Yr==(year+1), ] 
		long_term <- replist$timeseries[replist$timeseries$Yr==last_year, ]
	
		dead_bio  <- agrep("dead(B):_X", names(next_year))
		rbc 	    <- round(sum(next_year[dead_bio]))

		dead_bio_long <- agrep("dead(B):_X", names(long_term))
		rbc_long <- round(sum(long_term[dead_bio_long]))
	
	} else { 		
		SB_curr   <- NA
		curr_depl <- NA
		rbc       <- NA
		rbc_long  <- NA
	}

	mtrow <- as.list(c(SB0, SB_curr, curr_depl, rbc, rbc_long, n_est_pars, log_lik, ll_diff))
	mdata <- rbind(mdata, mtrow)
	row.names(mdata)[i+1] <- model_names[i]

	report_likes <- replist$likelihoods_used[1]

	if(i==1) ldata  <- report_likes else ldata <- cbind(ldata, report_likes)
	names(ldata)[i] <- model_names[i]

	tslist <- c(tslist, time_series)
	names(tslist)[i] <- model_names[i]

	if(show_plots[i]==TRUE){
		SS_plots(replist=replist, btarg=biomass_target, minbthresh=biomass_threshold, uncertainty=hessian[i], plot=which_plots, forecastplot=forecast[i], png=FALSE)
	}
	
	if(print_plots[i]==TRUE){
		SS_plots(replist=replist, btarg=biomass_target, minbthresh=biomass_threshold, uncertainty=hessian[i], plot=which_plots, forecastplot=forecast[i], png=TRUE)
	}
}

# Tidy model comparison dataframe (mdata) and component likelihoods dataframe (ldata), then print to screen:
mdata <- mdata[-1,]
ldata <- round(ldata, 2)
print(mdata)
print(ldata)

# Specify and create folders into which summary/comparison tables and plots should be directed (if required):
if(save_tables==TRUE){
	tables_folder <- paste(folder, model_names[1], "tables", sep="/")
	dir.create(tables_folder)
}

if(print_compare==TRUE){
	compare_folder <- paste(folder, model_names[max(which(compare==TRUE))], "compare", sep="/")
	dir.create(compare_folder)
}

# Save summary/comparison tables (mdata and ldata) to files (text and markdown files):
if(save_tables==TRUE){
 capture.output(mdata, file=paste(tables_folder, "/model_compare.txt", sep=""))
 capture.output(kable(mdata), file=paste(tables_folder, "/model_compare.md", sep=""))
 capture.output(ldata, file=paste(tables_folder, "/loglik_compare.txt", sep=""))
 capture.output(kable(ldata), file=paste(tables_folder, "/loglik_compare.md", sep=""))
}

#-------------------------------------------------------------------
# Model plot comparison section:
#-------------------------------------------------------------------

# Only get covariance files if hessian is available for all models required for comparison:
ifelse(all(hessian[which(compare==TRUE)]==TRUE), get_covar <- TRUE, get_covar <- FALSE)   

# Get number of models to compare:
ncomp_mods <- sum(compare)

# Get output and do plots for all models required for comparison:
if(compare_plots==TRUE){

	# Get all report files into a single list object and make each report list a global variable for plotting features:
	compare_list <- SSgetoutput(dirvec=model_dirs[which(compare==TRUE)], forecast=TRUE, verbose=TRUE, underscore=TRUE, listlists=TRUE, getcovar=get_covar)

	# Summarise the contents of 'compare_list' using the SSsummarize function:
	compare_summary <- SSsummarize(compare_list)

	# Get, print, and optionally save, a summary of the main estimated quantities (rounded):
	compare_quants <- cbind(round(compare_summary$quants[1:ncomp_mods], 2), compare_summary$quants[(ncomp_mods+1):(ncomp_mods+2)])
	names(compare_quants)[1:ncomp_mods] <- model_names[which(compare==TRUE)]
	capture.output(compare_quants, file=paste(compare_folder, "/output_compare.txt", sep=""))
	capture.output(kable(compare_quants), file=paste(compare_folder, "/output_compare.md", sep=""))

	#Compare plots of interest using SSplotcomparisons:
	SSplotComparisons(compare_summary, btarg=biomass_target, endyrvec=last_year, minbthresh=biomass_threshold, subplots=1:20, spacepoints=5, staggerpoints=1, plot=TRUE, print=print_compare, legendlabels=model_names[which(compare==TRUE)], plotdir=compare_folder)
}

#---------------------------
# EOF