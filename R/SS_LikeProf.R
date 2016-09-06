#------------------------------------------------------------------
# SS Likelihood Profiling (R Script):
#
# Configured for SESSF Stock Assessment 2015 
# By Athol Whitten, 2015, athol.whitten@mezo.com.au
#------------------------------------------------------------------

# Get latest (master) version of R4SS from Github:
# devtools::install_github("r4ss/r4ss")

# Open r4ss package:
library(r4ss)

# Specify working folder (directory where subdirectories are model folders):
folder <- "C:/~/Morwong/"

# Control file name:
ctl_file <- "morwong"

#------------------------------------------------------------------
# Set objects for each parameter of interest and run commands for 
# likelihood profile testing, summarising, and plotting.

#============================
# Template:
#============================

model_name      <- "BC_LPX"
BC_value		<- 0.20
profile_vec     <- seq(0.10, 0.50, 0.05)
ss_line_no      <- 00
profile_string  <- "String"
profile_label   <- "Label"

#----------------------------
profile_out <- SS_profile(dir=paste0(folder, model_name), model="ss3", extras="-nox -nohess", masterctlfile=paste0(ctl_file, ".ctl"), newctlfile=paste0(ctl_file, "_mod.ctl"), linenum=ss_line_no, profilevec=profile_vec)
show(profile_out)

profile_list <- SSgetoutput(dirvec=paste0(folder, model_name), keyvec=as.character(1:length(profile_vec)), forecast=FALSE, getcovar=FALSE, getcomp=FALSE, verbose=TRUE)
summary_list <- SSsummarize(profile_list)

SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)

# Save copy of plot to working directory:
png(paste0("SS_", model_name, ".png")) 
SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)
dev.off()

#------------------------------------------------------------------
# EXAMPLES

#============================
# For M (on Base Case Model):
#============================

model_name      <- "BC_LPM"
BC_value		<- 0.15
profile_vec     <- seq(0.10, 0.40, 0.05)
ss_line_no      <- 31
profile_string  <- "NatM_p_1_Fem"
profile_label   <- "Natural Mortality (M)"

#----------------------------
profile_out <- SS_profile(dir=paste0(folder, model_name), model="ss3", extras="-nox -nohess", masterctlfile=paste0(ctl_file, ".ctl"), newctlfile=paste0(ctl_file, "_mod.ctl"), linenum=ss_line_no, profilevec=profile_vec)
show(profile_out)

profile_list <- SSgetoutput(dirvec=paste0(folder, model_name), keyvec=as.character(1:length(profile_vec)), forecast=FALSE, getcovar=FALSE, getcomp=FALSE, verbose=TRUE)
summary_list <- SSsummarize(profile_list)

SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)

# Save copy of plot to working directory:
png(paste0("SS_", model_name, ".png")) 
SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)
dev.off()


#============================
# For h (on Base Case Model):
#============================

model_name		<- "BC_LPh"
BC_value 		<- 0.70
profile_vec     <- seq(0.65, 0.75, 0.05)
ss_line_no      <- 58
profile_string  <- "steep"
profile_label   <- "SR steepness (h)"

#----------------------------
profile_out <- SS_profile(dir=paste0(folder, model_name), model="ss3", extras="-nox -nohess", masterctlfile=paste0(ctl_file, ".ctl"), newctlfile=paste0(ctl_file, "_mod.ctl"), linenum=ss_line_no, profilevec=profile_vec)
show(profile_out)

profile_list <- SSgetoutput(dirvec=paste0(folder, model_name), keyvec=as.character(1:length(profile_vec)), forecast=FALSE, getcovar=FALSE, getcomp=FALSE, verbose=TRUE)
summary_list <- SSsummarize(profile_list)

SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)

# Save copy of plot to working directory:
png(paste0("SS_", model_name, ".png")) 
SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)
dev.off()


#============================
# For R0 (on Base Case Model):
#============================

model_name		<- "BC_LPR0"
BC_value 		<- 8.37
profile_vec     <- seq(8, 9, 0.5)
ss_line_no      <- 57
profile_string  <- "SR_LN"
profile_label   <- "Ln(R0)"

#----------------------------
profile_out <- SS_profile(dir=paste0(folder, model_name), model="ss3", extras="-nox -nohess", masterctlfile=paste0(ctl_file, ".ctl"), newctlfile=paste0(ctl_file, "_mod.ctl"), linenum=ss_line_no, profilevec=profile_vec)
show(profile_out)

profile_list <- SSgetoutput(dirvec=paste0(folder, model_name), keyvec=as.character(1:length(profile_vec)), forecast=FALSE, getcovar=FALSE, getcomp=FALSE, verbose=TRUE)
summary_list <- SSsummarize(profile_list)

SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)

# Save copy of plot to working directory:
png(paste0("SS_", model_name, ".png"))
SSplotProfile(summary_list, plot=TRUE, models="all", profile.string=profile_string, profile.label=profile_label)
abline(v=BC_value, lty=2)
dev.off()

#------------------------------------------------------------------
# EOF.