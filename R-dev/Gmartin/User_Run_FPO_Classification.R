
##-----------------------------------------------------------------------------------##
##           Script to use the function FPO_classification
##-----------------------------------------------------------------------------------##



# Author: Sophie de Grissac - DioMed&a Science
# COMPANY N° (SIRET): 882 532 245 00017
# last update: 29/03/2020
# email: s.degrissac@gmail.com
# Contact me for any update, question, problem related to this function.


##-----------------------------------------------------------------------------------##


###   Enter below in between the "" the chosen path to files and folder then RUN each line.
###   SEE INSTRUCTIONS IN THE WORD DOCUMENT PRODIVED

###   1) csv file with harbour locations; 
path_harbour <- file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/VMS_Sophie/",
                          "FPO classification program/HarbourLocs/harbourLoc.csv")

###   2) csv file with vms locations to classify; 
path_vessel <- file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/VMS_Sophie/",
                         "FPO classification program/Data/Eilidh C_modelledActivity_SdG240320.csv")

###   3) The folder in which you want all the resulting files
path_folder_result <- file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/VMS_Sophie/",
                                "FPO classification program/Results")

###   4) Path to the folder where the FPO_classification_function.R is stored.
path_function <- file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/VMS_Sophie/",
                           "FPO classification program")

###  RUN THE FOLLOWING LINES
source (paste0(path_function,"/",'FPO_classification_function.R',sep=""))
FPO_classification(path_vessel, path_harbour, path_folder_result)



######### NOTES ON DATA INPUT ########

# - The provided dataset with fishing vessel GPS location + date/time and vessel identity (code):

#                   * must be a CSV (coma separation) with the first 4 columns as follows:
#                   * vessel identity / Date+time / longitude / latitude (see instructions). 
#                   * The names of the column doesn't matter
#                   * The format of the Date+Time MUST BE dd/dd/yyy hh:mm:ss
#                   * Ping rates can typically be 5, 3 or 1 min. Different ping rates are allowed in the same dataset
#                   * Classifcation problem will arise for vessels that have changed their ping rate during a trip (they can change in between trips) or for which pings are very irregular
#                   * Therefore, the regularity of the ping rate is tested for each trip and trips with to much irregularity (standard error compared to median of ming rate) are removed
#                   * Trips that are removed from the analyse are kept and saved in a separate file for the record.
