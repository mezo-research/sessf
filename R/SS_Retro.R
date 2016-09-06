#------------------------------------------------------------------
# SS Retrospective Analysis (R Script):
#
# Configured for SESSF Stock Assessment 2015 
# By Athol Whitten, 2015, athol.whitten@mezo.com.au
#------------------------------------------------------------------

# Get latest (master) version of R4SS from Github:
# devtools::install_github("r4ss/r4ss")

# Open r4ss package:
library(r4ss)

# Specify working folder (directory where each folder contains a set of model files, base-case or otherwise):
folder <- "C:/~/Morwong/"

# Enter the name of the model folder that retrospective analysis will be performed on:
model <- "BC"

# List of relative years over which to run retrospective tests (should be 0 to -Y, e.g. 0:-5, or seq(0, -10, -2)):
years <- 0:-3

# Number of recent years/cohorts of recruitment estimates to plot in relation to main model (for 'squid' plot):
n_cohorts <- 10

# Include hessian calculation in model runs?
hessian <- TRUE

# Call to system or shell to run SS3? (Choice depends on how you’re running R. Default is "system"):
call_type <- "system"

# Print retrospective plots to a folder and/or show on screen?
show_plots <- FALSE
print_plots <- TRUE

# For spawning depletion plots, specify biomass target and biomass minimum threshold:
biomass_target    <- 0.48
biomass_threshold <- 0.20


#-------------------------------------------------------------------
# End user controls, start main script:
#-------------------------------------------------------------------

# Run retrospective model tests:
SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir="Retrospectives", subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)

# Collect output information for each of the retrospective models:
retro_mods <- SSgetoutput(dirvec=paste0(folder, 'Retrospectives/Retro', years), getcovar=hessian)

# Summarise information collected above:
retro_summary <- SSsummarize(retro_mods)

# Create a list of end years, one for for each model:
end_years <- retro_mods[[1]]$endyr + years

# Create a list of cohorts, relative to last year with recruitment estimation:
end_rec_year <- retro_mods[[1]]$recruit$year[max(which(retro_mods[[1]]$recruit$era=="Main"))]
retro_cohorts <- (end_rec_year - n_cohorts):end_rec_year

# Create a folder to print compare plots:
plot_dir <- file.path(folder, "Retrospectives/Plots") 
dir.create(plot_dir)

# Create list of retrospective model names:
retro_names <- paste0(model, "_", end_years)

# Print compare plots as PNG files, and to screen:
SSplotComparisons(retro_summary, 
	endyrvec=end_years, 
	btarg=biomass_target, 
	minbthresh=biomass_threshold, 
	subplots=1:20, 
	spacepoints=5, 
	staggerpoints=1, 
	plot=show_plots, 
	print=print_plots, 
	new=TRUE, 
	legendlabels=retro_names, 
	plotdir=plot_dir
)

# Print retrospective recruitment plot (a 'Squid Plot') as a PNG file:
png(file.path(plot_dir, "/Retro_Recruit_Devs.png"), width=7, height=7, units="in", res=300)

SSplotRetroRecruits(retro_summary,
	endyrvec=end_years,
	cohorts=retro_cohorts,
	relative=TRUE,
	labelyears=TRUE,
	legend=FALSE,
)

dev.off()

#------------------------------------------------------------------
# EOF.