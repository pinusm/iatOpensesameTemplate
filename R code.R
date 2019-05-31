#Michael Pinus, pinusm@post.bgu.ac.il

#D score calculations done by the IAT packeage (Dan Martin), based on Greenwald, Banaji & Nosek (2003).
#Here D2 is used (can be altered below in the cleanIAT calls, see ?cleanIAT for details).
#Internal consistency was calculated as per Bar-Anan and Nosek (2014) suggested procedure.

#Positive iatD values indicate faster RTs when cat1/att1 and cat2/att2 shared the response keys.
#Negative iatD values indicate faster RTs when cat2/att1 and cat1/att2 shared the response keys.
#thus, a positive iatD score indicates a stronger association of cat1 with att1, over att2,
#or/and
#a stronger association of cat2 with att2, over att1.

#Output is the data-frame iatD, which is a row-per-participat table where D scores are coupled with subject numbers.
#Optionally, creates a CSV file


####################### THERE'S NO NEED TO EDIT THIS ###################################
####################### ALL THE INFO THE SCRIPT NEEDS #############################################
####################### CAN BE PROVIDED WITH DIALOG BOXES #########################################
####################### SO YOU CAN JUST RUN IT NOW, AS-IS #########################################


# Setting 'interactive_usage = "yes"' means you will be prompted to choose the path of the of the raw opensesame csv files,
# and the path of the output file. Setting 'interactive_usage = "no"' means you will need to hardcode both raw and output paths, below.
# 'interactive_usage = "yes"' is for inexperienced or casual R users. If you need nothing from R, other than getting the D scores,
# leave this setting set to "yes". valid values are "yes"/"no".
interactive_usage = "yes"

# create CSV file with the results? 'outputCSV = "yes"' is for inexperienced or casual R users. If you need nothing from R,
# other than getting the D scores, leave this setting set to "yes".
# valid values are "yes"/"no".
outputCSV = "yes"

# create CSV file with the results? 'outputCSV = "yes"' is for inexperienced or casual R users. If you need nothing from R,
# other than getting the D scores, leave this setting set to "yes".
# valid values are "yes"/"no".
outputTXT = "yes"

# The next setting is IGNORED if 'interactive_usage = "yes"'.
# insert here the path to the raw files. make sure not to use Hebrew (or any
# non-English) characters in your path, to use double slashes, and a valid
# Windows path (if you're on Linux, you probably know how to tweak to code
# accordingly)
# takes a double-quoted string, such as:
# rawPath = "C:\\my_iat\\RawData\\"
rawPath = "C:\\Users\\micha\\Downloads\\IAT\\"


# The next setting is IGNORED if 'interactive_usage = "yes"'.
# insert here the path to the output CSV file. make sure to
# use double slashes, and a complete and valid windows path (if you're on linux,
# you probably know how to tweak to code accordingly).
# If not defined, the script will try to use the parent folder of the rawPath folder.
# If the parent folder is write-protected, the working directory (where this script
# is saved) will be used.
# takes a double-quoted string, such as:
# csvPath = "C:\\my_iat\\"
csvPath = "C:\\Users\\micha\\Downloads\\"

#if CSV exists, should it be overwritten (with warning)? valid values are "yes"/"no"
overwriteCSV = "yes"

# The next setting is IGNORED if 'interactive_usage = "yes"'.
# insert here the path to the output TXT file for the internal consistency result. make sure to
# use double slashes, and a complete and valid windows path (if you're on linux,
# you probably know how to tweak to code accordingly).
# If not defined, the script will try to use the parent folder of the rawPath folder.
# If the parent folder is write-protected, the working directory (where this script
# is saved) will be used.
# takes a double-quoted string, such as:
# txtPath = "C:\\my_iat\\"
txtPath = "C:\\Users\\micha\\Downloads\\"

#if TXT exists, should it be overwritten (with warning)? valid values are "yes"/"no"
overwriteTXT = "yes"

# This will tell the script to notify you when the CSV or TXT files where overwritten.
# 'alertOnWarning = "yes"' is for inexperienced or casual R users. If you need nothing from R,
# other than getting the D scores, leave this setting set to "yes".
# valid values are "yes"/"no".
alertOnWarning = "yes"

####################### DO NOT EDIT BELOW THIS LINE ############################
#################### UNLESS YOU KNOW WHAT YOU'RE DOING #########################

# We require several packages. This will make sure they're installed before we proceed.
# running this if the packages are already installed won't do anything (i.e., it won't do any harm, and it won't look for updates).
if (!require("Hmisc")) {install.packages("Hmisc")}
if (!require("psych")) {install.packages("psych")}
if (!require("IAT")) {install.packages("IAT")}
if (!require("tidyverse")) {install.packages("tidyverse")}


# check if the users want to interactively pick the paths, and let them do so
if (interactive_usage == "yes"){
    # make sure Rstudio is running
    if (!rstudioapi::isAvailable()) {stop("For interactive usage, you must use Rstudio v1.2 or later.")}
    rstudioapi::showDialog(title = "Raw Files Path", message = "Hello there! On the next popup window, please pick the the path of the raw files created by OpenSesame")
    rawPath <-  rstudioapi::selectDirectory()
    rstudioapi::showDialog(title = "Output CSV Path", message = "Now, pick a location and file name for the output CSV file.")
    csvPath <- rstudioapi::selectFile(caption = "Save File",
                                         label = "Save",
                                         existing = FALSE,
                                         path = rawPath)
    if (!endsWith(csvPath, ".csv")) {csvPath <- paste0(csvPath, ".csv")}
    rstudioapi::showDialog(title = "Output TXT Path", message = "Last one. Pick a location and file name for the internal consistency TXT file.")
    txtPath <- rstudioapi::selectFile(caption = "Save File",
                                      label = "Save",
                                      existing = FALSE,
                                      path = rawPath)
    # append file extension, if missing
    if (!endsWith(csvPath, ".csv")) {csvPath <- paste0(csvPath, ".csv")}
    if (!endsWith(txtPath, ".txt")) {txtPath <- paste0(txtPath, ".txt")}
}

# cleanup the paths, incase the user hardcoded them, with forward slashes
rawPath <- gsub("\\\\", "/", rawPath)
csvPath <- gsub("\\\\", "/", csvPath)
txtPath <- gsub("\\\\", "/", txtPath)

# Import OpenSesame data ####
RawOpenSesame <- list.files(path = rawPath, pattern = "*.csv",full.names = TRUE) %>%
    purrr::map_df(~readr::read_csv(., col_types = readr::cols(.default = "c")))


## make sure all subjects got in.
length(unique(RawOpenSesame$subject_nr))
## keep only needed variables ####
keepVars <- c("blocknumber", "subject_nr", "correct_first_chance_response", "response_time_first_chance_response", "response_time_second_chance_response", "first_loc")
cleanOpenSesame <- RawOpenSesame %>%
    dplyr::select(dplyr::one_of(keepVars)) %>%
    readr::type_convert() # guess the column types. This guess should be pretty safe, as we've kept only the
# variables we need, and we know what they are.


## get ready for IATD. this is a multistep process.. ####
### create the err variable
errors <- cleanOpenSesame %>% dplyr::mutate(
    err = dplyr::case_when(
        correct_first_chance_response == 0 ~ 1,
        correct_first_chance_response == 1 ~ 0,
        TRUE ~ NA_real_
    )
)


### compute _full_ trial latency
latencies <- errors %>% dplyr::mutate(
    response_time_second_chance_response = dplyr::if_else(err == 0, 0 ,response_time_second_chance_response),
    trial_latency = response_time_first_chance_response + response_time_second_chance_response
)

### add Block number based on block content

blocks <- latencies %>%
    dplyr::mutate(
        block_number = dplyr::case_when(
            blocknumber == 3 & first_loc == "right" ~  "B3",
            blocknumber == 3 & first_loc == "left"  ~  "B6",
            blocknumber == 6 & first_loc == "right" ~  "B6",
            blocknumber == 6 & first_loc == "left"  ~  "B3",
            blocknumber == 4 & first_loc == "right" ~  "B4",
            blocknumber == 4 & first_loc == "left"  ~  "B7",
            blocknumber == 7 & first_loc == "right" ~  "B7",
            blocknumber == 7 & first_loc == "left"  ~  "B4",
            TRUE ~ NA_character_
        )
    )

### get only trials where block_number isn't NA
iat <- blocks %>%
    dplyr::filter(!is.na(block_number))

## prepare for IC analysis ####
modulo <- 1:nrow(iat)
iat <- iat %>% dplyr::mutate(
    modulo = modulo%%3 ### nth cycle, here every 3
    )

### make sure every subject has same number of trials with valid
### block_number.
iat %>%
    dplyr::group_by(subject_nr) %>%
    dplyr::filter(!is.na(block_number)) %>%
    dplyr::count()

### make sure numbers of trials with valid block_number make sense. note:
### B6,B7 are should be twice as long as B3,B4
iat %>%
    dplyr::group_by(block_number) %>%
    dplyr::filter(!is.na(block_number)) %>%
    dplyr::count()

# breakdown the valid trials by blocks, for each subject. this could get
# lengthy for big datasets.
iat %>%
    dplyr::group_by(subject_nr,block_number) %>%
    dplyr::filter(!is.na(block_number)) %>%
    dplyr::count()


# now same tests, by modulo, to make sure the division for the IC analysis
# worked as expected

iat %>%
    dplyr::group_by(modulo, subject_nr) %>%
    dplyr::filter(!is.na(block_number)) %>%
    dplyr::count()

iat %>%
    dplyr::group_by(modulo,block_number) %>%
    dplyr::filter(!is.na(block_number)) %>%
    dplyr::count()

iat %>%
    dplyr::group_by(modulo,subject_nr,block_number) %>%
    dplyr::filter(!is.na(block_number)) %>%
    dplyr::count()

iatmod0 <- iat %>% dplyr::filter(modulo == 0)
iatmod1 <- iat %>% dplyr::filter(modulo == 1)
iatmod2 <- iat %>% dplyr::filter(modulo == 2)

## compute IATD scores ####

iatD <- IAT::cleanIAT(iat , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                 trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 )

iatD <- dplyr::select(iatD,subject_nr,IAT)

### for IC measure
iatDmod0 <- IAT::cleanIAT(iatmod0 , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                     trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 ) %>% as_tibble()
iatDmod1 <- IAT::cleanIAT(iatmod1 , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                     trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 ) %>% as_tibble()
iatDmod2 <- IAT::cleanIAT(iatmod2 , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                     trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 ) %>% as_tibble()

iatDmod0 <- iatDmod0 %>%
    dplyr::rename(iatmod0 = IAT) %>%
    dplyr::select(subject_nr,iatmod0)
iatDmod1 <- iatDmod1 %>%
    dplyr::rename(iatmod1 = IAT) %>%
    dplyr::select(subject_nr,iatmod1)
iatDmod2 <- iatDmod2 %>%
    dplyr::rename(iatmod2 = IAT) %>%
    dplyr::select(subject_nr,iatmod2)


iatDmods2 <- iatDmod0 %>% dplyr::full_join(iatDmod1, by = "subject_nr") %>%
    dplyr::full_join(iatDmod2, by = "subject_nr")

iatIC <- iatDmods %>% dplyr::select(-subject_nr) %>% psych::alpha()

# write output to CSV ##########################################################
if (outputCSV == "yes") {
    if (!dir.exists(normalizePath(dirname(csvPath)))) { #check if the directory of csvPath exists. will warn if not valid.
        #if csvPath not defined, goto parent of rawPath
        csvPath <- file.path(dirname(rawPath),"iatD.csv")
    }
    if (overwriteCSV == "yes") {
        if (file.exists(csvPath)) {
            file.remove(csvPath)
            warning("CSV file overwritten. Let's hope that's OK with you...")
            if (alertOnWarning == "yes"){
                rstudioapi::showDialog(title = "CSV file overwritten", "CSV file overwritten. Let's hope that's OK with you...")
            }

        }
    }
    else {
        if (file.exists(csvPath)) {
            stop("CSV file exists. Remove/rename it and try again")
            if (alertOnWarning == "yes"){
                rstudioapi::showDialog(title = "CSV file exists", "CSV file exists. Remove/rename it and try again")
            }
        }
    }
    write.csv(iatD, file = csvPath, na="", row.names = FALSE)
    print(paste0("CSV file saved in ", csvPath))
}

# write IC output to TXT ##########################################################
if (outputTXT == "yes") {
    if (!dir.exists(normalizePath(dirname(txtPath)))) { #check if the directory of txtPath exists. will warn if not valid.
        #if csvPath not defined, goto parent of rawPath
        txtPath <- file.path(dirname(rawPath),"iatIC.txt")
    }
    if (overwriteTXT == "yes") {
        if (file.exists(txtPath)) {
            file.remove(txtPath)
            warning("TXT file overwritten. Let's hope that's OK with you...")
            if (alertOnWarning == "yes"){
                rstudioapi::showDialog(title = "TXT file overwritten", "TXT file overwritten. Let's hope that's OK with you...")
            }

        }
    }
    else {
        if (file.exists(txtPath)) {
            stop("TXT file exists. Remove/rename it and try again")
            if (alertOnWarning == "yes"){
                rstudioapi::showDialog(title = "TXT file exists", "TXT file exists. Remove/rename it and try again")
            }
        }
    }
    reportIC <- paste0("Raw Cronbach's Alpha for the IAT was ", round((iatIC$total$raw_alpha),2), ". See Bar-Anan & Nosek (2014) for details on how it was computed.")

    cat(reportIC, file= txtPath, sep="n", append=FALSE)
    print(paste0("TXT file saved in ", txtPath))
}

print(reportIC)

