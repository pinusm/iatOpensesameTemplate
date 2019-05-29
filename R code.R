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
#Optionally, creates a CSV file named 'iatD.csv'.

#requires several packages. If errors occur, uncomment and run these next lines once.
# (running it multiple times won't do any harm, but will be redundant)
# install.packages("Hmisc")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("reshape2")
# install.packages("IAT")


####################### EDIT THIS ##############################################
#insert here the path to the raw files. make sure not to use Hebrew (or any 
#non-English) characters in your path, to use double slashes, and a valid
#Windows path (if you're on Linux, you probably know how to tweak to code 
#accordingly)
#takes a double-quoted string, such as:
#rawPath = "C:\\my_iat\\RawData\\"
rawPath = "C:\\Users\\micha\\Downloads\\IAT\\"

#create iat.CSV with the results? valid values are "yes"/"no"
outputCSV = "yes"

#insert here the path to the output CSV file. make sure to
#use double slashes, and a complete and valid windows path (if you're on linux,
#you probably know how to tweak to code accordingly).
#If not defined, the script will try to use the parent folder of the rawPath folder.
#If the parent folder is write-protected, the working directory (where this script
#is saved) will be used.
#takes a double-quoted string, such as:
#csvPath = "C:\\my_iat\\"
csvPath = "C:\\Users\\micha\\Downloads\\"

#if iat.CSV exists, should it be overwritten (with warning)? valid values are "yes"/"no"
overwriteCSV = "yes"
####################### DO NOT EDIT BELOW THIS LINE ############################
#################### UNLESS YOU KNOW WHAT YOU'RE DOING #########################
# load packages #################################################
library("Hmisc")
library("plyr")
library("dplyr")
library("psych")
library("IAT")
# Import OpenSesame data ####
old.dir <- getwd()
setwd(gsub("\\\\", "/", rawPath))
# save as tbl, following dplyr docs suggestion
RawOpenSesame <- tbl_df(ldply(list.files(pattern="*.csv"),function(filename) {
    dum=read.csv(filename,encoding = "UTF-8")
    dum$filename=filename
    return(dum)
}))
setwd(old.dir)
rm(old.dir)

## make sure all subjects got in.
length(unique(RawOpenSesame$subject_nr))
## keep only needed variables ####
keepVars <- c("blocknumber", "subject_nr", "correct_first_chance_response", "response_time_first_chance_response", "response_time_second_chance_response", "first_loc")
cleanOpenSesame <- RawOpenSesame[ , names(RawOpenSesame) %in% keepVars]
## get ready for IATD. this is a multistep process.. ####
### create the err variable 
errors <- cleanOpenSesame
errors[!is.na(errors$correct_first_chance_response) & errors$correct_first_chance_response == 0, "err"] = 1
errors[!is.na(errors$correct_first_chance_response) & errors$correct_first_chance_response == 1, "err"] = 0
rm(cleanOpenSesame)
### compute _full_ trial latency
latencies <- errors
latencies[latencies$err == 0 , "response_time_second_chance_response"] = 0
latencies[, "trial_latency"] = latencies$response_time_first_chance_response + latencies$response_time_second_chance_response
rm(errors)

### add Block number based on block content, only for abs(ZerrPercent) < 2.5
blocks <- latencies
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 3 & blocks$first_loc == "right", "block_number"] = "B3"
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 3 & blocks$first_loc == "left", "block_number"] = "B6"
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 6 & blocks$first_loc == "right", "block_number"] = "B6"
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 6 & blocks$first_loc == "left", "block_number"] = "B3"
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 4 & blocks$first_loc == "right", "block_number"] = "B4"
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 4 & blocks$first_loc == "left", "block_number"] = "B7"
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 7 & blocks$first_loc == "right", "block_number"] = "B7"
blocks[!is.na(blocks$blocknumber) & blocks$blocknumber == 7 & blocks$first_loc == "left", "block_number"] = "B4"
rm(latencies)

### get only trials where block_number isn't NA
iat <- subset(blocks, !is.na(blocks$block_number))
rm(blocks)

## prepare for IC analysis ####
modulo <- 1:nrow(iat)
iat$modulo <- modulo%%3   ### nth cycle, here every 3
### make sure every subject has same number of trials with valid block_number. uncomment to run test:
#ddply(iat,.(subject_nr), dplyr::summarise, count = length(block_number))
### make sure numbers of trials with valid block_number make sense. note: B6,B7 are should be twice as long as B3,B4
#ddply(iat,.(block_number), dplyr::summarise, count = length(block_number))
# breakdown the valid trials by blocks, for each subject. this could get lengthy for big datasets.
#ddply(iat,.(subject_nr,block_number), dplyr::summarise, count = length(block_number))

# now same tests, by modulo, to make sure the division for the IC analysis worked as expected
#ddply(iat,.(modulo, subject_nr), dplyr::summarise, count = length(block_number))
#ddply(iat,.(modulo, block_number), dplyr::summarise, count = length(block_number))
#in the next one modulo 0 has more trials than modulo 1,2. this is because the total trail number (120) is nicely divided (modulo = 0) of 3.
#ddply(iat,.(modulo, subject_nr,block_number), dplyr::summarise, count = length(block_number))

iatmod0 <- iat[iat$modulo==0 , ]  
iatmod1 <- iat[iat$modulo==1 , ]
iatmod2 <- iat[iat$modulo==2 , ]
## compute IATD scores ####

iatD <- cleanIAT(iat , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                 trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 )

iatD <- select(iatD,subject_nr,IAT)

### for IC measure
iatDmod0 <- cleanIAT(iatmod0 , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                     trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 )
iatDmod1 <- cleanIAT(iatmod1 , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                     trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 )
iatDmod2 <- cleanIAT(iatmod2 , block_name = "block_number" , trial_blocks = c("B3", "B4", "B6", "B7") , session_id = "subject_nr" , trial_latency= "trial_latency",
                     trial_error ="err" , v_error = 1 , v_extreme = 2 , v_std = 1 )
rm(iatmod0,iatmod1,iatmod2)

iatDmod0$iatmod0 <- iatDmod0$IAT
iatDmod1$iatmod1 <- iatDmod1$IAT
iatDmod2$iatmod2 <- iatDmod2$IAT

iatDmod0 <- select(iatDmod0,subject_nr,iatmod0)
iatDmod1 <- select(iatDmod1,subject_nr,iatmod1)
iatDmod2 <- select(iatDmod2,subject_nr,iatmod2)

iatDmods <- Merge(iatDmod0, iatDmod1, iatDmod2, id = ~ subject_nr , verbose = FALSE)
rm(iatDmod0,iatDmod1,iatDmod2)
iatIC <- psych::alpha(iatDmods[ , -1])

# write output to CSV ##########################################################
old.dir <- getwd()
if (outputCSV == "yes") {
    if (nchar(csvPath) > 0) { #only if csvPath is defined. will warn if not valid.
        setwd(gsub("\\\\", "/", csvPath)) #if this err'd, WD will be restored.
    } else { #if csv not defined, goto parent of rawPath
        setwd(gsub("\\\\", "/", rawPath))
        t2 <- try(setwd('..')) #try to setWD to parent folder.
        if("try-error" %in% class(t2)) setwd(old.dir)   #if this err'd, WD will be restored.
    } 
    if (overwriteCSV == "yes") {
        if (file.exists("iatD.csv")) {
            file.remove("iatD.csv")
            warning("iatD.csv overwritten. Let's hope that's OK...")
        }
    }
    else {
        if (file.exists("iatD.csv")) {
            stop("iatD.csv exists. Remove/rename it and try again")
        }
    }
    write.csv(iatD, file = "iatD.csv", na="", row.names = FALSE)
    print(paste0("iatD.csv saved in ", csvPath))
}
setwd(old.dir)
rm(old.dir)
print(paste0("Raw Cronbach's Alpha for the IAT was ", round((iatIC$total$raw_alpha),2), ". see Bar-Anan & Nosek (2014) for details on how it was computed."))

