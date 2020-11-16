

#       _______.___________.    ___      .______     .___________.
#      /       |           |   /   \     |   _  \    |           |
#     |   (----`---|  |----`  /  ^  \    |  |_)  |   `---|  |----`
#      \   \       |  |      /  /_\  \   |      /        |  |     
#  .----)   |      |  |     /  _____  \  |  |\  \----.   |  |     
#  |_______/       |__|    /__/     \__\ | _| `._____|   |__|     
  
####################--------Instructions Below------------------######################################

#Before you run the program load the data set to be assessed into the folder for the region the data set 
#applies toand rename the data set something unique.
#Once R is open you will need to install the pacakges plyr, dplyr, and lubridate
#Then change the working directory file path to match the location of ReLEP on your computer
#make sure to use double forward slashes in the file path name
#change the name of the file in the "Load Data Set" command to match the file you want assessed
#Change the working directory file pat command at the bottom to match the location of the output 
#folder on your computer once the program has been run check the output folder for the LOEs and 
#the missing rows files the missing rows contain the data that could not run through the program because
#of mismatched names (station, analyte, waterbody, etc.)
#check those files to make sure that all data that should have been assessed WAS assessed

#######################-----------------------------------------#######################################

#This section loads nexeccary pagages for assessment
#try not to add unecessary pacages here because
#sometimes packages conflict with eachother
#or they will mask the ommands and cause unpredicable behavior in the code

library(readxl)
library(dplyr,warn.conflicts=TRUE)
library(tidyr)
library(data.table, warn.conflicts=TRUE)
library(tidyverse)
library(lubridate,warn.conflicts=TRUE)
library(rio)


#Sets path to use Rtools to zip up xls/xlsx file with rio
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")

#Sets path to use rJava for package (unsure if explicitly needed)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')

#Removes all defined variables this is only imporant when you run the
#code multiple times in one session.  it removes previously defined variables
rm(list=ls(all=TRUE))

#saves system time to display how long the code takes to run at the end
Start_Time<- Sys.time()


########################################################################################################
########################################################################################################
####################_______________  Begin Water Module _____________________________###################
########################################################################################################
########################################################################################################

########################################################################################################
###################___ Enter the working location of the ReLEP folder Here _________####################
########################################################################################################
################                                                                        ################
#############                                                                              #############
###########                                                                                  ###########
#########                                                                                      #########
#######                                                                                          #######
#####                                                                                              #####
####                                                                                                #### 
###                                                                                                  ###
##                                                                                                    ##
#                                                                                                      #


#>>>>>     Change the values input in Region and Author to get the program to run        <<<<<<<<<<<<<#
#>>>>>     The region should be the region number you are assessing and the author       <<<<<<<<<<<<<#
#>>>>>     should be the username login you use to log into your computer                <<<<<<<<<<<<<#

Region<-"3"                                                                    #<<<<<<<<<<<-<<<<<<<<<<#

Author<-"Jkaplan"                                                              #<<<<<<<<<<<-<<<<<<<<<<#

ReLEP_Version<-"Version 1.1"								                                  #<<<<<<<<<<<-<<<<<<<<<<#

Write_Data_Viz_Table<-"No"								#Must be "Yes" to write table       #<<<<<<<<<<<-<<<<<<<<<<#

######You will want to change the name of the below file name to match the name of the data file you
#####are trying to assess.  The file name is currently "Region 2 export.txt" but you could change
####it to any text file.  Just make sure the field names match.  Do the same thing with each of the
###tables below.  Each table is used by the program but is specific to each Region. Make sure the file
##ends in the .txt extension, and put the name in quotations like the examples below.Put the name to the right
#of the "gets" symbol <-


FileNameInQuotations<-"R3_Water_Sediment_And_Field_Data.txt"



WorkingDirectory<-paste0("C:\\Users\\",Author,"\\Desktop\\Final_ReLEP\\Region ",Region,"\\Water_Module")

#Set the directory so that relative paths work for loading the files
setwd(WorkingDirectory)

SitesTable<-paste0("R",Region,"_Sites_FINAL.txt")

BeneficialUseTable<-paste0("R",Region,"_Beneficial_Use_FINAL.txt")

ObjectivesTable<-paste0("R",Region,"_Analytes_Water_FINAL.txt")

SummingPollutantsTable<-"SummingPollutants.txt"

PAHTable<-"PAH_TEFs.txt"

DataReferencesTable<-paste0("R",Region,"_Data_Used_DRAFT.txt")

SSOTable<-paste0("R",Region,"_Site_Specifics_FINAL.txt")

SampleTypeNameTable<-paste0("R",Region,"_Acceptable_SampleTypeNames.txt")


#                                                                                                      #
##                                                                                                    ##
#####                                                                                               ####
#######                                                                                            #####
#########                                                                                         ######
###########                                                                                      #######
#############                                                                                   ########
###############                                                                               ##########
#################                                                                           ############
###################                                                                       ##############
#####################                                                                   ################
########################################################################################################

# Load Region specific Data file, remove unecessary fields, and convert to tbl_df
data<-tbl_df(read.delim(FileNameInQuotations,header=TRUE,stringsAsFactors=FALSE))
data$ProjectName<-data$ParentProjectName
names(data)[names(data)=="Latitude"]<-"TargetLatitude"
names(data)[names(data)=="Longitude"]<-"TargetLongitude"
#Change negative results with ResQualCodes of ND or DNQ to be equal to zero
#the negative results are negative MDLs which means they will eather be clean samples
#or quantitation discards depending on the objective used so zero is appropriate
data$Result[which(data$Result<0 & (data$ResQualCode=="ND"|data$ResQualCode=="DNQ"))]<-0 #perfect scenario
ExportedData<-data
data<-data[which(data$MatrixName=="samplewater"|data$MatrixName=="Surface Water"|data$MatrixName==""|data$MatrixName=="samplewater, <1.2 um"),]
#redefine "samplewater, <1.2 um" as simply "samplewater" because they are equivalent for our purposes
data$MatrixName[which(data$MatrixName=="samplewater, <1.2 um")]<-"samplewater"
ExportedData$SampleDate<-ymd(ExportedData$SampleDate)
ExportedData<-filter(ExportedData,!(ExportedData$MatrixName=="air"|ExportedData$MatrixName=="sediment"|ExportedData$MatrixName=="sediment, <63 um"))
data<-subset(data,select=c("ProjectName","StationCode","SampleDate","MatrixName","AnalyteName","FractionName"
,"UnitName","Result","MDL","RL","ResQualCode","QACode","SampleTypeName","TargetLatitude","TargetLongitude","DataQuality","DataQualityIndicator"))
data$SampleDate<-ymd(data$SampleDate)
data$FractionName[which(data$FractionName=="Recoverable"|data$FractionName=="Total Recoverable")]<-"Total"


BadMatrixName<-subset(ExportedData,!(MatrixName=="samplewater"|MatrixName=="Surface Water"|MatrixName==""|MatrixName=="samplewater, <1.2 um"))
BadMatrixName$Issue<-"Check MatrixName"
AllExportedData<-tbl_df(BadMatrixName)


#Load the acceptable SampleTypeName List and filter for "acceptable" sample types
AcceptableSampleTypes<-tbl_df(read.delim(SampleTypeNameTable,header=TRUE,stringsAsFactors=FALSE))
AcceptableSampleTypes<-AcceptableSampleTypes[AcceptableSampleTypes$Acceptable=="Yes",]
AcceptableSampleTypes$Acceptable<-NULL


#Add data with results < 0 and ResQualCode of "=" to tossed data
LessThanZero<-ExportedData[which(ExportedData$ResQualCode=="="&ExportedData$Result<0),]
LessThanZero$Issue<-"Result less than Zero AND ResQualCode of '=' Check data for errors"
AllExportedData<-rbind.data.frame(AllExportedData,LessThanZero)

#Remove data with results < 0 and ResQualCode of "=" to tossed data
data<-data[which(!(data$ResQualCode=="="&data$Result<0&!is.na(data$Result))),]



#Create table of data with bad QA Codes to be exported and reviewed
BadQACodes<-filter(ExportedData,DataQuality!="Passed QC")
BadQACodes$Issue<-"Check DataQuality Field"
AllExportedData<-tbl_df(rbind.data.frame(AllExportedData,BadQACodes))


#Now do the opposite command on the data to get the remaining rows of data with acceptable QACodes
data<-filter(data,DataQuality=="Passed QC")
data$DataQuality<-NULL
data$DataQualityIndicator<-NULL


#Change negative results with ResQualCodes of ND or DNQ to be equal to zero
#the negative results are neative MDLs which means they will eather be clean samples
#or quantitation discards depending on the objective used so zero is appropriate
data$Result[which(data$Result<0 & (data$ResQualCode=="ND"|data$ResQualCode=="DNQ"))]<-0 #perfect scenario



#tbl_df is a great command for coding
#it allows you to see only the top ten rows of data, but it shows you 
#a count of the number of rows, and the names and data types for all
#of the columns i use it all the time in the code below

data<-tbl_df(data)

#only water data moves on beyond this point

data<-tbl_df(merge(data,AcceptableSampleTypes))
data$SampleTypeName<-NULL

BadSampleType<-anti_join(ExportedData,AcceptableSampleTypes)
BadSampleType$Issue<-"Check Sample type"
AllExportedData<-rbind.data.frame(AllExportedData,BadSampleType)

#Create a table of bad ResQualCodes
MissingResQualCode<-filter(AllExportedData, ((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode))&(RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))
MissingResQualCode$Issue<-"Missing ResQualCode"
AllExportedData<-rbind.data.frame(AllExportedData,MissingResQualCode)

#Remove data with bad res qual codes from the main data table
data<-filter(data, !((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode))&(RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))


#Add ND and DNQ data that does not have MDL and RL information to exported data table
MissingLimits<-subset(ExportedData,((ResQualCode=="ND"|ResQualCode=="DNQ")&(RL=="NA"|RL==" "|RL==""|is.na(RL))))
MissingLimits$Issue<-"ND or DNQ Sample Missing RL"
AllExportedData<-rbind.data.frame(AllExportedData,MissingLimits)

#Remove ND and DNQ data that does not have MDL and RL information from main data table
data<-subset(data,!((ResQualCode=="ND"|ResQualCode=="DNQ")&(RL=="NA"|RL==" "|RL==""|is.na(RL))))


#Multiply MDL by 3.18 to get RL if RL is missing as per quantitaiton guidance
data$RL[which((is.na(data$RL)|data$RL==""|data$RL==" ")&(data$MDL!="NA"|data$MDL!=""|data$MDL!=
" "))]<-data$MDL[which((is.na(data$RL)|data$RL==""|data$RL==" ")&(data$MDL!="NA"|data$MDL!=""|data$MDL!=" "))]*3.18

####################################################################################################################
#########################OTHER TABLE LOADING SECTION###############################################################


# Load Region sites table and convert to tbl_df
Sites<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Sites<-Sites[Sites$STATUS=="Completed",]
Sites<-subset(Sites,select=c(Waterbody,StationCode,WBID))
names(Sites)<-c("Waterbody","StationCode","WBID")
Sites<-tbl_df(Sites)

#"Marine" represents salt water NOT covered under the ocean plan more specifically than
#it represents "marine" waters which mean "M" also includes to inland saline lakes
Ocean_Marine<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Ocean_Marine<-Ocean_Marine[Ocean_Marine$STATUS=="Completed",]
Ocean_Marine<-subset(Ocean_Marine,select=c(StationCode,FreshMarine))
Ocean_Marine<-Ocean_Marine[which(Ocean_Marine$FreshMarine=="O"|Ocean_Marine$FreshMarine=="M"),]

Fresh_Fresh<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Fresh_Fresh<-Fresh_Fresh[Fresh_Fresh$STATUS=="Completed",]
Fresh_Fresh<-subset(Fresh_Fresh,select=c(StationCode,FreshMarine))
Fresh_Fresh<-Fresh_Fresh[Fresh_Fresh$FreshMarine=="F",]

#Ocean stations covered under the ocean plan
Ocean_Waters<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors = FALSE))
Ocean_Waters<-Ocean_Waters[Ocean_Waters$STATUS=="Completed",]
Ocean_Waters<-subset(Ocean_Waters,select=c(StationCode,FreshMarine))
Ocean_Waters<-Ocean_Waters[Ocean_Waters$FreshMarine=="O",]

#All stations and their salinity type
SiteSalinity<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors = FALSE))
SiteSalinity<-SiteSalinity[SiteSalinity$STATUS=="Completed",]
SiteSalinity<-subset(SiteSalinity,select=c(StationCode,FreshMarine))
SiteSalinity<-distinct(SiteSalinity)

#Load Beneficial Uses table and convert to tbl_df
Beneficial_Uses<-read.delim(BeneficialUseTable,sep="\t",header=TRUE,stringsAsFactors=FALSE)
Beneficial_Uses<-subset(Beneficial_Uses,select=c(Waterbody,WBID,Wbtype,BeneficialUse))
Beneficial_Uses<-na.omit(Beneficial_Uses)
Beneficial_Uses<-tbl_df(Beneficial_Uses)


# Load Region objecives table and convert to tbl_df
Analytes<-read.delim(ObjectivesTable,header=TRUE,stringsAsFactors=FALSE)
#The NA omit command that follows may be a problem because the code for one of the 
#beneficial uses is NA and so I think the code will remove it.  There should be some 
#workaround for this.
#Analytes<-na.omit(Analytes)
Analytes$Alias<-NULL
Analytes$CAS_Number<-NULL
Analytes<-subset(Analytes,select=c(AnalyteName,UnitName,BeneficialUse,FreshMarine,Objective,AveragingPeroid,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number))
Analytes$Objective_Ref_Number<-as.character(Analytes$Objective_Ref_Number)
Analytes$Eval_Ref_Number<-as.character(Analytes$Eval_Ref_Number)
Analytes<-tbl_df(Analytes)

#Create table of Ocean water specific objectives
Ocean_Analytes<-Analytes[Analytes$FreshMarine=="O",]
Ocean_Analytes$FreshMarine<-NULL

#Remove ocean water specific objectives from the main analyte table
Analytes<-Analytes[Analytes$FreshMarine!="O",]
Analytes$FreshMarine<-NULL

#Each AnalyteName may apprear multiple times, but each row should be unique
SummingPollutants<-read.delim(SummingPollutantsTable,header=TRUE,stringsAsFactors=FALSE)
SummingPollutants<-tbl_df(SummingPollutants[SummingPollutants$Matrix=="Water",])
SummingPollutants<-subset(SummingPollutants,select=c(AnalyteName,SummingName))


#Table contains TEFs for PAHs and TCDDs
PAH_TEFs<-read.delim(PAHTable,header=TRUE,stringsAsFactors=FALSE)
PAH_TEFs<-tbl_df(PAH_TEFs)

#Contains CalWQA ref numbers for the data set and QAPPs
DataReferences<-tbl_df(read.delim(DataReferencesTable,header=TRUE,stringsAsFactors=FALSE))
DataReferences<-subset(DataReferences,select=c(ParentProjectName,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO,DATA_SOURCE,Assess))
names(DataReferences)[names(DataReferences)=="ParentProjectName"]<-"ProjectName"
DataReferences<-DataReferences[DataReferences$Assess=="Yes",]
DataReferences$Assess<-NULL

#Contains site specific objectives information
SSOs<-tbl_df(read.delim(SSOTable,header=TRUE,stringsAsFactors=FALSE))
SSOs$Waterbody<-NULL
SSOs<-SSOs[SSOs$Units!="mg/Kg"|SSOs$Units!="mg/kg",]


#Contains conversion of ReLEP AnalyteName to CalWQA names
ReLEP_to_CalWQA_Lookup<-tbl_df(read.delim("ReLEP_to_CalWQA_Lookup.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE))
names(ReLEP_to_CalWQA_Lookup)[names(ReLEP_to_CalWQA_Lookup)=="ReLEP_AnalyteName"]<-"POLLUTANT"

#############_______________End Data Loading Section___________###############################
##############################################################################################
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################
##############################################################################################
######################Begin table joining and data splitting section##########################

# Add the beneficial uses to the waterbodies table
Waterbodies<-merge(Sites,Beneficial_Uses,by=c("Waterbody","WBID"))
Waterbodies<-tbl_df(Waterbodies)


# Add waterbody names and beneficial uses to the data
W_Waterbodies<-merge(data,Waterbodies,by="StationCode")
W_Waterbodies<-tbl_df(W_Waterbodies)

#Create a table of data from stations that do not have waterbody 
#information assigned in the sties tables
Missing_Stations<-anti_join(ExportedData,Waterbodies,by="StationCode")
Missing_Stations$Issue<-"Missing waterbody information"
AllExportedData<-rbind.data.frame(AllExportedData,Missing_Stations)


#Split out pH and Temperatue for ammonia calculations
#Remove blank reuslts, blank temp not possible
temp<-W_Waterbodies[W_Waterbodies$AnalyteName=="Temperature",]
names(temp)[names(temp)=="Result"]<-"TempResult"
temp<-filter(temp,!is.na(TempResult))
temp$Fraction<-NULL
temp$AnalyteName<-NULL
temp$MDL<-NULL
temp$RL<-NULL
temp$UnitName<-NULL
temp$ResQualCode<-NULL
temp$QACode<-NULL
temp$FractionName<-NULL

#Convert temp in celsius to Kelvin
KelvinTemp<-temp
KelvinTemp<-mutate(KelvinTemp,Kelvin=TempResult+273.15)
KelvinTemp$TempResult<-NULL


#Split out pH for pentachlorophenol identify result as being pH result, remove all fields
#unecessary for unique join
#Remove blank reuslts, blank pH not valid
PentapH<-W_Waterbodies[W_Waterbodies$AnalyteName=="pH",]
names(PentapH)[names(PentapH)=='Result']<-"pHResult"
PentapH<-filter(PentapH,!is.na(pHResult))
PentapH$Fraction<-NULL
PentapH$AnalyteName<-NULL
PentapH$MDL<-NULL
PentapH$RL<-NULL
PentapH$UnitName<-NULL
PentapH$ResQualCode<-NULL
PentapH$QACode<-NULL
PentapH$FractionName<-NULL

Salinity<-tbl_df(W_Waterbodies[W_Waterbodies$AnalyteName=="Salinity",])
names(Salinity)[names(Salinity)=='Result']<-"SalinityResult"
Salinity$Fraction<-NULL
Salinity$AnalyteName<-NULL
Salinity$MDL<-NULL
Salinity$RL<-NULL
Salinity$UnitName<-NULL
Salinity$ResQualCode<-NULL
Salinity$QACode<-NULL
Salinity$FractionName<-NULL


#Split out DOC and TOC data for use with Pyrethroids
OrganicCarbon<-W_Waterbodies[which(W_Waterbodies$AnalyteName=="Dissolved Organic Carbon"
                                   |W_Waterbodies$AnalyteName=="Total Organic Carbon"),]


#Determine the presence of SSOs and export data if present 
W_SSOs<-tbl_df(merge(ExportedData,Waterbodies))
W_SSOs<-tbl_df(merge(W_SSOs,SSOs))
W_SSOs_for_Export<-W_SSOs
W_SSOs_for_Export$BeneficialUse<-NULL
W_SSOs_for_Export<-unique(W_SSOs_for_Export)

W_SSOs<-subset(W_SSOs,select=c(WQID,ProgramCode,ProgramName,ParentProjectCode,ParentProjectName,ProjectCode,ProjectName,ProjectDescr
,QAPPCode,QAPPName,PublicRelease,SampleDate,StationCode,StationName,StationDescr,TargetLatitude,TargetLongitude,Datum,RegionalBoardID
,WaterBodyType,SampleAgency,SampleAgencyCode,SampleComments,CollectionTime,PositionWaterColumn,LabCollectionComments,CollectionMethodName
,SampleTypeCode,SampleTypeName,Replicate,CollectionDeviceCode,CollectionDeviceName,CollectionDepth,UnitCollectionDepth,LabAgencyCode,LabAgencyName
,SubmittingAgency,LabSubmissionCode,LabBatchComments,MatrixCode,MatrixName,MethodCode,MethodName,AnalyteCode,AnalyteName,FractionCode,FractionName
,UnitCode,UnitName,MDL,RL,QACode,LabBatch,AnalysisDate,LabReplicate,Result,ResQualCode,LabResultComments,SampleLatitiude,SampleLongitude,SampleDatum
,SampleElevation,SampleUnitElevation,DataQuality,DataQualityIndicator,FieldWater))
W_SSOs$Issue<-"Data row has SSO for one or more beneficial uses that apply to it"
W_SSOs<-unique(W_SSOs)
AllExportedData<-tbl_df(rbind.data.frame(AllExportedData,W_SSOs))


SSOs$BeneficialUse<-NULL
#Remove data for watebody/pollutant/BU combinations that have SSOs
W_Waterbodies<-anti_join(W_Waterbodies,SSOs)

#Add data from projects that are missing QA information to the exproted data table
No_Ref_Codes<-anti_join(ExportedData,DataReferences)
No_Ref_Codes$Issue<-"Parent Project Not in Data Used Table or marked as Do Not Assess."
AllExportedData<-tbl_df(rbind.data.frame(AllExportedData,No_Ref_Codes))


########## ____________Begin Freshwater Total Ammonia to unionized ammonia conversion________________#######
##########   Currently this section throws out all data that does not have coresponding pH and temp data   #
##########   I am not sure if this is the correct course of action or not                       ############
##########   The other option would be to join the data with pH and temp data and then fill in the blanks ##
##########   (rows without coresponding pH and temp data)  								######
##########   with default values described in the EPA criteria document.  I am not sure if this          ###
##########   is appropriate though. If no default numbers are used, do we want to track the            #####
##########   rows that are tossed?                                                                   #######

#Split out Total Ammonia for sample days that did not have un-ionized ammonia also reported
UnIonized<-W_Waterbodies[which(W_Waterbodies$AnalyteName=="Ammonia as N, Unionized"|W_Waterbodies$AnalyteName=="Ammonia as NH3, Unionized"|W_Waterbodies$AnalyteName=="Ammonia as NH3"),]
UnIonized<-distinct(select(UnIonized,StationCode,SampleDate))

#Do antijoin with the main data frame to get list of staion days that only reported Total Ammonia
TotalAmmoniaOnly<-anti_join(W_Waterbodies,UnIonized,by=c("StationCode","SampleDate"))
TotalAmmoniaOnly<-TotalAmmoniaOnly[TotalAmmoniaOnly$AnalyteName=="Ammonia as N",]

#Filter out Total Ammonia data that is from saline water
#It needs to be assessed using a different formula.
FreshTotalAmmoniaOnly<-anti_join(TotalAmmoniaOnly,Ocean_Marine)
TotalAmmoniaOnly<-FreshTotalAmmoniaOnly

#Merge Ammonia Data with Temp and pH data.  Any row that does not have a coresponding pH value will be thrown out.
TotalAmmoniaOnly_W_pH<-tbl_df(merge(TotalAmmoniaOnly,PentapH,by=c("StationCode","ProjectName","SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaOnlyJoin<-subset(TotalAmmoniaOnly,select=c(StationCode,ProjectName,SampleDate,MatrixName,AnalyteName,FractionName,Result,UnitName,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude))
AmmoniaNopH<-tbl_df(merge(ExportedData,TotalAmmoniaOnlyJoin))
AmmoniaNopH<-anti_join(AmmoniaNopH,PentapH)
AmmoniaNopH<-unique(AmmoniaNopH)
AmmoniaNopH$Issue<-"Ammonia missing pH data"
AllExportedData<-rbind.data.frame(AllExportedData,AmmoniaNopH)


#Merge Ammonia data with temp data.  Any row that does not have a coresponding pH value will be thrown out.
TotalAmmonia_For_Conversion<-tbl_df(merge(TotalAmmoniaOnly_W_pH,temp,by=c("StationCode","ProjectName","SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaOnlyJoin<-subset(TotalAmmoniaOnly,select=c(StationCode,ProjectName,SampleDate,MatrixName,AnalyteName,FractionName,Result,UnitName,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude))
TotalAmmoniaOnlyJoin<-unique(TotalAmmoniaOnlyJoin)
AmmoniaNoTemp<-tbl_df(merge(ExportedData,TotalAmmoniaOnlyJoin))
AmmoniaNoTemp<-anti_join(AmmoniaNoTemp,temp)
AmmoniaNoTemp<-unique(AmmoniaNoTemp)
AmmoniaNoTemp$Issue<-"Ammonia as N data missing temp data"
AllExportedData<-rbind.data.frame(AllExportedData,AmmoniaNoTemp)


#Convert Total Ammonia to unionized ammonia using formula and then remove unecessary fields
Converted_Ammonia<-mutate(TotalAmmonia_For_Conversion,pKA=.09018+(2729.92/(273.2+TempResult)))
Converted_Ammonia<-mutate(Converted_Ammonia,UnionizedAmmoniaResult=(14/17)*Result*(1/(1+10^((.09018+(2729.92/(273.2+TempResult)))-pHResult))))
Converted_Ammonia$AnalyteName<-"Ammonia as N, Unionized"
Converted_Ammonia$Result<-NULL
Converted_Ammonia$pKA<-NULL
Converted_Ammonia$pHResult<-NULL
Converted_Ammonia$TempResult<-NULL
names(Converted_Ammonia)[names(Converted_Ammonia)=="UnionizedAmmoniaResult"]<-"Result"
Converted_Ammonia$Result[which(is.na(Converted_Ammonia$Result))]<-0

###_____Converted ammonia added back to W_Waterbodies between hardness conversion
###_____and the unit conversion sections                                         

############_______________________End Freshwater Conversion____________________###########
###########################################################################################

#####################____Begin Saltwater Ammonia calculation____###########################

#Create Table of Salty Total Ammonia for non ocean waterbodies
SaltyAmmoniaOnly<-W_Waterbodies[W_Waterbodies$AnalyteName=="Ammonia as N",]
SaltyAmmoniaOnly<-anti_join(SaltyAmmoniaOnly,Fresh_Fresh)


#Toss data that is missing coresponding temperature data
SaltyAmmoniaJoin<-unique(subset(SaltyAmmoniaOnly,select=c(StationCode,ProjectName,SampleDate,MatrixName,AnalyteName,FractionName,Result,UnitName,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude)))
SaltyAmmoniaMissingTemp<-tbl_df(merge(ExportedData,SaltyAmmoniaJoin))
SaltyAmmoniaMissingTemp<-anti_join(SaltyAmmoniaMissingTemp,KelvinTemp)
SaltyAmmoniaMissingTemp$Issue<-"Ammonia data missing coresponding temp"
AllExportedData<-rbind.data.frame(AllExportedData,SaltyAmmoniaMissingTemp)

#Toss data that is missing corresponding salinity data
SaltyAmmoniaMissingSalt<-tbl_df(merge(ExportedData,SaltyAmmoniaJoin))
SaltyAmmoniaMissingSalt<-anti_join(SaltyAmmoniaMissingSalt,Salinity)
SaltyAmmoniaMissingSalt$Issue<-"Ammonia data missing coresponding salinity"
AllExportedData<-rbind.data.frame(AllExportedData,SaltyAmmoniaMissingSalt)

#Removereplicates from alinity and kelvin temp
Salinity<-tbl_df(as.data.table(Salinity)[,mean(SalinityResult),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype)])
	names(Salinity)[names(Salinity)=="V1"]<-"SalinityResult"
KelvinTemp<-tbl_df(as.data.table(KelvinTemp)[,mean(Kelvin),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype)])
	names(KelvinTemp)[names(KelvinTemp)=="V1"]<-"Kelvin"
pH<-tbl_df(as.data.table(PentapH)[,mean(pHResult),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype)])
	names(pH)[names(pH)=="V1"]<-"pH"


#Calculate pKa for objective formula
pKaSalinity<-tbl_df(merge(Salinity,KelvinTemp))
pKaSalinity<-tbl_df(merge(pKaSalinity,pH))
pKaSalinity<-mutate(pKaSalinity,I=((19.9273*SalinityResult)/(1000-(1.005109*SalinityResult))))
pKaSalinity<-mutate(pKaSalinity,pKa=(9.245+.116*I))
pKaSalinity<-mutate(pKaSalinity,fNH3=(1/(1+(10^(pKa+(.0324*(298-Kelvin))+(.0415/Kelvin)-pH)))))
pKaSalinity<-mutate(pKaSalinity,Objective=((.035/fNH3)*1000))

#Create Objective table
SalinityObjectives<-tbl_df(pKaSalinity)
SalinityObjectives$SalinityResult<-NULL
SalinityObjectives$Kelvin<-NULL
SalinityObjectives$I<-NULL
SalinityObjectives$pH<-NULL
SalinityObjectives$pKa<-NULL
SalinityObjectives$fNH3<-NULL

#Add objective language, and reference codes
SalinityObjectives$Evaluation_Guideline<-""
SalinityObjectives$Eval_Ref_Number<-""
SalinityObjectives$Objective_Language<-paste0("The Total Ammonia criterion continuous concentration (expressed as a 4-day average) to protect aquatic life in saltwater is temperature, pH and salinity dependent; and was calculated according to the formula listed in the Ambient Water QualityCriteria for Ammonia (Saltwarer) - 1989 document.")
SalinityObjectives$Objective_Ref_Number<-"4529"
SalinityObjectives$AveragingPeroid<-4
SalinityObjectives<-tbl_df(SalinityObjectives)


SalinityObjectives$Region<-substr(SalinityObjectives$WBID,4,4)
SalinityObjectives<-filter(SalinityObjectives,!(SalinityObjectives$Region=="5"|SalinityObjectives$Region=="6"|SalinityObjectives$Region=="7"))
SalinityObjectives$Region<-NULL
SalinityAmmoniaObjectives<-SalinityObjectives[which(SalinityObjectives$BeneficialUse=="ES"|SalinityObjectives$BeneficialUse=="MA"),]
SalinityAmmoniaObjectives$AnalyteName<-"Ammonia as N"

#Remove objectives for stations/waterbodies that the objective does not apply to
SalinityAmmoniaObjectives<-anti_join(SalinityAmmoniaObjectives,Fresh_Fresh)

###########################################################################################

###########################################################################################
###################___________\/________Begin_Hardness_____\/_______#######################


#Metals Assessments
HardnessSubset<-subset(W_Waterbodies,select=c("ProjectName","StationCode","SampleDate"
	,"MatrixName","AnalyteName","UnitName","Result", "BeneficialUse")) 
Hardness<-tbl_df(HardnessSubset)
Hardness<-Hardness[Hardness$AnalyteName=="Hardness as CaCO3",]
Hardness<-Hardness[which(Hardness$BeneficialUse=="CO"
	|Hardness$BeneficialUse=="WA"|Hardness$BeneficialUse=="ES"|Hardness$BeneficialUse=="MA"),]

###Create a table of rows of hardness that have units other than mg/L 
##because they will need to be converted. This is a very unlikely 
###thing to happen so it does not make sense to program the conversion in here
###But, in the unlikely event it does happen. This table will flag 
###the issue and show you the rows that need to be changed
HardnessUnitIssues<-ExportedData[ExportedData$UnitName!="mg/L",]
HardnessUnitIssues<-subset(HardnessUnitIssues,(AnalyteName=="Hardness as CaCO3"
	|AnalyteName=="Alkalinity as CaCO3"))
HardnessUnitIssues$Issue<-"Hardness data with weird units"
AllExportedData<-rbind.data.frame(AllExportedData,HardnessUnitIssues)


#Convert hardness value to number (if it isnt already) then assign values 
###of 400 if hardness is greater than 400 and hardness value of 100 
###if hardness if the result field is empty
Hardness$Result<-as.numeric(as.character(Hardness$Result))
Hardness$Result[Hardness$Result>400]<-400
Hardness$Result[Hardness$Result=="NA"]<-100


#Remove replicates (samples collected on the same day) of the hardness data
Hardness<-tbl_df(as.data.table(Hardness)[,mean(Result),list(AnalyteName,MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,UnitName)])
	names(Hardness)[names(Hardness)=="V1"]<-"Result"


#Create tables of objectives by date and station separated by pollutant name
Cadmium<-mutate(Hardness, Objective=(1.101672-(log(Result)*.041838))*exp(.7825*log(Result)-2.715))
Cadmium$AnalyteName<-"Cadmium"
Copper<-mutate(Hardness,Objective=.960*exp(.8545*log(Result)-1.702))
Copper$AnalyteName<-"Copper"
Chromium<-mutate(Hardness,Objective=.860*exp(.8190*log(Result)+1.561))
Chromium$AnalyteName<-"Chromium"
ChromiumIII<-mutate(Hardness,Objective=.860*exp(.8190*log(Result)+1.561))
ChromiumIII$AnalyteName<-"Chromium III"
Lead<-mutate(Hardness,Objective=(1.46203-(log(Result))*.145712)*exp(1.273*log(Result)-4.705))
Lead$AnalyteName<-"Lead"
Nickel<-mutate(Hardness,Objective=.997*exp(.8460*log(Result)+.0584))
Nickel$AnalyteName<-"Nickel"
Silver<-mutate(Hardness,Objective=.5*.85*exp(1.72*log(Result)-6.52))
Zinc=mutate(Hardness,Objective=.986*exp(.8473*log(Result)+.884))
Zinc$AnalyteName<-"Zinc"



#Createplaceholder information in case there is no hardness data so that
#we can still create the HardnessObjectives table for joining and assessment
colnames<-c("AnalyteName","MatrixName","SampleDate","BeneficialUse","StationCode","ProjectName","UnitName","Result","Objective","Type")
fillerinfo<-c("Placeholder","Placeholder","1850-01-01","Fake","Placeholder","Placeholder","Placeholder",0,0,"Brack")
HardnessObjectives<-as.data.frame(rbind(colnames,fillerinfo))
colnames(HardnessObjectives)<-colnames
row.names(HardnessObjectives)<-NULL
HardnessObjectives<-HardnessObjectives[-1,]
HardnessObjectives<-tbl_df(HardnessObjectives)
HardnessObjectives$SampleDate<-Sys.Date()
HardnessObjectives$Objective<-0
HardnessObjectives$Result<-0




# Combine all of these tables into one large table
HardnessObjectives2<-tbl_df(rbind(Cadmium, Copper, Chromium, ChromiumIII, Lead, Nickel, Silver, Zinc))
HardnessObjectives2$Type<-"Fresh"
HardnessObjectives<-rbind.data.frame(HardnessObjectives,HardnessObjectives2)





#Create tables of objectives by date and station separated by pollutant name
SaltCadmium<-mutate(Hardness, Objective=9.3)
SaltCadmium$AnalyteName<-"Cadmium"
SaltCopper<-mutate(Hardness,Objective=3.1)
SaltCopper$AnalyteName<-"Copper"
SaltLead<-mutate(Hardness,Objective=8.1)
SaltLead$AnalyteName<-"Lead"
SaltNickel<-mutate(Hardness,Objective=8.2)
SaltNickel$AnalyteName<-"Nickel"
SaltSilver<-mutate(Hardness,Objective=.95)
SaltZinc=mutate(Hardness,Objective=81)
SaltZinc$AnalyteName<-"Zinc"

SaltHardnessObjectives<-tbl_df(rbind(SaltCadmium, SaltCopper, SaltLead, SaltNickel, SaltSilver, SaltZinc))
SaltHardnessObjectives$Type<-"Salt"


#Combine the salt and freshwater criterion together
HardnessObjectives<-tbl_df(rbind.data.frame(HardnessObjectives,SaltHardnessObjectives))

#Delete unneeded columns
HardnessObjectives$Result<-NULL
HardnessObjectives$UnitName<-NULL

###############################################################################################
#Convert Total fraction results field to dissolved values using conversion factor then 
#change name of fraction to dissolved. Only do this to sample days that ONLY reported total 
#fraction of that metal on that day
FirstHardnessDependentAnalytes<-tbl_df(filter(W_Waterbodies,((AnalyteName=="Cadmium"|AnalyteName=="Copper"
|AnalyteName=="Chromium"|AnalyteName=="Chromium VI"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") &(BeneficialUse=="CO"|BeneficialUse=="WA"
|BeneficialUse=="ES"|BeneficialUse=="MA"))))

# Convert Results, MDL, and RL that are in mg/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]*1000
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]*1000
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]*1000
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="mg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in pg/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]*.000001
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]*.000001
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]*.000001
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="pg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in ng/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]*.001
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]*.001
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]*.001
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="ng/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in g/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]*1000000
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]*1000000
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]*1000000
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="g/L"]<-"ug/L"



HardnessDependentAnalytes<-filter(FirstHardnessDependentAnalytes, (AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") & (BeneficialUse=="CO"|BeneficialUse=="WA"))
	
HardnessDependentAnalytes$Type<-"Fresh"


SaltHardnessDependentAnalytes<-filter(FirstHardnessDependentAnalytes, (AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium VI"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") & (BeneficialUse=="ES"|BeneficialUse=="MA"))
	
SaltHardnessDependentAnalytes$Type<-"Salt"

#Combine fresh and salt hardness analytes together
HardnessDependentAnalytes<-tbl_df(rbind.data.frame(HardnessDependentAnalytes,SaltHardnessDependentAnalytes))

W_Waterbodies_hardness<-tbl_df(HardnessDependentAnalytes)



#Create List of stations that have data for the dissolved fraction of metals
DissolvedOnly<-distinct(select(W_Waterbodies_hardness,AnalyteName,StationCode,FractionName,SampleDate))
DissolvedOnly<-DissolvedOnly[DissolvedOnly$FractionName=="Dissolved",]
#DissolvedOnly$FractionName<-NULL

#Create data frame of data that needs to be converted from total to dissolved fraction
NeedsConverting<-anti_join(W_Waterbodies_hardness,DissolvedOnly,by=c("AnalyteName","StationCode","SampleDate"))

#Create table of this data for the station information calculation
#That happens later, just before the main quantitation check
NeedsConvertingForStations<-NeedsConverting
NeedsConvertingForStations$Type<-NULL
NeedsConvertingForStations<-unique(NeedsConvertingForStations)
NeedsConvertingForStations$FractionName<-"Dissolved"



#Create a data frame of data that does not need converting 
#(has dissolved data only, or both dissolved and total for each day)
W_Waterbodies_hardness_no_conversion<-W_Waterbodies_hardness[W_Waterbodies_hardness$FractionName=="Dissolved",]


#Calculate the converstion factors for the rows of data where only total fraction was reported and
#we need to convert total fraction into dissolved Before we can compare to the objectives

CadmiumCF<-mutate(Hardness, CF=1.101672-(log(Result)*.041838))
CadmiumCF$AnalyteName<-"Cadmium"
LeadCF<-mutate(Hardness,CF=1.46203-(log(Result)*.145712))
LeadCF$AnalyteName<-"Lead"
CopperCF<-mutate(Hardness,CF=.960)
CopperCF$AnalyteName<-"Copper"
ChromiumVICF<-mutate(Hardness,CF=.962)
ChromiumVICF$AnalyteName<-"Chromium VI"
ChromiumIIICF<-mutate(Hardness,CF=.860)
ChromiumIIICF$AnalyteName<-"Chromium III"
NickelCF<-mutate(Hardness,CF=.997)
NickelCF$AnalyteName<-"Nickel"
ZincCF<-mutate(Hardness,CF=.986)
ZincCF$AnalyteName<-"Zinc"
ConversionFactors<-rbind.data.frame(CadmiumCF, CopperCF, ChromiumVICF,ChromiumIIICF, LeadCF, NickelCF, ZincCF)
ConversionFactors$Result<-NULL
ConversionFactors$UnitName<-NULL
ConversionFactors$MatrixName<-NULL
ConversionFactors$Type<-"Fresh"


SaltCadmiumCF<-mutate(Hardness, CF=.994)
SaltCadmiumCF$AnalyteName<-"Cadmium"
SaltLeadCF<-mutate(Hardness,CF=.951)
SaltLeadCF$AnalyteName<-"Lead"
SaltCopperCF<-mutate(Hardness,CF=.83)
SaltCopperCF$AnalyteName<-"Copper"
SaltChromiumVICF<-mutate(Hardness,CF=.993)
SaltChromiumVICF$AnalyteName<-"Chromium VI"
SaltNickelCF<-mutate(Hardness,CF=.990)
SaltNickelCF$AnalyteName<-"Nickel"
SaltZincCF<-mutate(Hardness,CF=.946)
SaltZincCF$AnalyteName<-"Zinc"
SaltConversionFactors<-rbind.data.frame(SaltCadmiumCF, SaltCopperCF, SaltChromiumVICF, SaltLeadCF, SaltNickelCF, SaltZincCF)
SaltConversionFactors$Result<-NULL
SaltConversionFactors$UnitName<-NULL
SaltConversionFactors$MatrixName<-NULL
SaltConversionFactors$Type<-"Salt"


#Createplaceholder information in case there is no hardness data so that
#we can still create the ConversionFactors table for joining and assessment
Tempinfo1<-c("AnalyteName","SampleDate","BeneficialUse","StationCode","ProjectName","CF","Type")
fillerinfo<-c("Placeholder","1850-01-01","Fake","Placeholder","Placeholder",0,"Brack")
FakeCF<-as.data.frame(rbind(Tempinfo1,fillerinfo),stringsAsFactors=FALSE)
colnames(FakeCF)<-Tempinfo1
row.names(FakeCF)<-NULL
FakeCF<-FakeCF[-1,]
FakeCF<-tbl_df(FakeCF)
FakeCF$SampleDate<-Sys.Date()
FakeCF$CF<-0
ConversionFactors1<-FakeCF



#Stack the conversion factors and the fake conversionfactors to create a full size conversion factor table
ConversionFactors1<-tbl_df(rbind.data.frame(ConversionFactors1,ConversionFactors))
ConversionFactors<-ConversionFactors1
ConversionFactors<-tbl_df(rbind.data.frame(ConversionFactors,SaltConversionFactors))



#Add the conversion factors to the data that needs converting by pollutant, matrix, BeneficalUse, SampleDate, ProjectDate
NeedsConverting<-tbl_df(merge(NeedsConverting,ConversionFactors,all.x=TRUE))


#Add a converstion factor to the rows that did not have cresponding hardness data collected
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Cadmium"& is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.909001
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Copper" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.960
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Chromium VI" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.962
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Chromium III" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.860
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Lead" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.791001
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Nickel" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.997
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Silver"&is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.85
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Zinc" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.986

NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Cadmium"& is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.994
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Copper" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.83
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Chromium VI" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.993
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Lead" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.951
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Nickel" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.990
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Silver"&is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.85
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Zinc" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.946


#Convert the results field for all rows in the needs converting table based on their converstion factos
#Then delete the CF field and results field, then change the name of NewResults to Result
NeedsConverting<-mutate(NeedsConverting, NewResults=Result*CF)
NeedsConverting$CF<-NULL
NeedsConverting$Result<-NULL
names(NeedsConverting)[names(NeedsConverting)=="NewResults"]<-"Result"

#Change fraction from total to dissolved, because that is what we did with this calculation
NeedsConverting$FractionName[NeedsConverting$FractionName=="Total"]<-"Dissolved"

#Combine the results that were converted to the results that did not need converting
W_Waterbodies_hardness<-rbind.data.frame(W_Waterbodies_hardness_no_conversion, NeedsConverting)

#Remove metals data from the table if it falls in an Ocean water. CTR does not apply there
W_Waterbodies_hardness<-anti_join(W_Waterbodies_hardness,Ocean_Waters)

#Join Hardness Objectives to W_Waterbodies_hardness (analytes that require hardness objective)
Hardness_W_Objectives<-tbl_df(merge(W_Waterbodies_hardness,HardnessObjectives,by=c("ProjectName"
	,"StationCode","SampleDate","MatrixName","AnalyteName","BeneficialUse","Type"),all.x=TRUE))

#Will introduce NA for any analyte that does not have hardness data collected with it

#Replace NAs created in step above for values from CTR (CCC) when hardness is taken to be 100
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Cadmium"&Hardness_W_Objectives$Type=="Fresh"]<-2.2
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Copper"&Hardness_W_Objectives$Type=="Fresh"]<-9
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium VI"&Hardness_W_Objectives$Type=="Fresh"]<-11
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium"&Hardness_W_Objectives$Type=="Fresh"]<-180
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium III"&Hardness_W_Objectives$Type=="Fresh"]<-180
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Lead"&Hardness_W_Objectives$Type=="Fresh"]<-2.5
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Nickel"&Hardness_W_Objectives$Type=="Fresh"]<-52
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Fresh"]<-1.7
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Zinc"&Hardness_W_Objectives$Type=="Fresh"]<-120
Hardness_W_Objectives<-tbl_df(Hardness_W_Objectives)


#Replace NAs created in step above for values from CTR (CCC) when hardness is taken to be 100
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Cadmium"&Hardness_W_Objectives$Type=="Salt"]<-9.3
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Copper"&Hardness_W_Objectives$Type=="Salt"]<-3.1
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium VI"&Hardness_W_Objectives$Type=="Salt"]<-50
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Lead"&Hardness_W_Objectives$Type=="Salt"]<-8.1
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Nickel"&Hardness_W_Objectives$Type=="Salt"]<-8.2
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Salt"]<-.95
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Zinc"&Hardness_W_Objectives$Type=="Salt"]<-81
Hardness_W_Objectives<-tbl_df(Hardness_W_Objectives)

#Tell R that Hardness_W_Objectives column "Objective" is a numeric value just in case it wants to believe otherwise
Hardness_W_Objectives$Objective<-as.numeric(Hardness_W_Objectives$Objective)

#Add objective and Eval Guideline fields so that we can join this back in to main table later in the process
Hardness_W_Objectives$Evaluation_Guideline<-""
Hardness_W_Objectives$Eval_Ref_Number<-""
Hardness_W_Objectives$Objective_Language<-""
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$Type=="Fresh")]<-paste0("California Toxics Rule (CTR) lists criterion continuous concentrations to protect aquatic life in freshwater. The criterion in freshwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")
Hardness_W_Objectives$Objective_Ref_Number<-"476"

#Add language for salt water.  keep the below command on a single line or it will cause issues with the LOE export
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$Type=="Salt")]<-paste0("California Toxics Rule (CTR) lists criterion continuous concentrations to protect aquatic life in saltwater. The criterion in saltwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")

#Add language to silver because silver is CMC value not CCC value
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Salt")]<-paste0("California Toxics Rule (CTR) lists criterion maximum concentrations to protect aquatic life in saltwater. The criterion in saltwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Fresh")]<-paste0("California Toxics Rule (CTR) lists criterion maximum concentrations to protect aquatic life in freshwater. The criterion in freshwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")


#Richard's code to correct results based on quantitation limtis
Hardness_W_Objectives$Result[which(Hardness_W_Objectives$RL<=Hardness_W_Objectives$Objective & (Hardness_W_Objectives$ResQualCode=="ND"
	|Hardness_W_Objectives$ResQualCode=="DNQ"))]<-Hardness_W_Objectives$MDL[which(Hardness_W_Objectives$RL<=
	Hardness_W_Objectives$Objective & (Hardness_W_Objectives$ResQualCode=="ND"|Hardness_W_Objectives$ResQualCode=="DNQ"))]*.5 #perfect scenario

	Hardness_W_Objectives$Result[which((Hardness_W_Objectives$RL<=Hardness_W_Objectives$Objective)&is.na(Hardness_W_Objectives$MDL)&(Hardness_W_Objectives$ResQualCode=="ND"
	|Hardness_W_Objectives$ResQualCode=="DNQ"))]<-0 #When sample can be quantified, but is missing the MDL, Result becomes zero

	Hardness_W_Objectives$Result[which(Hardness_W_Objectives$RL>Hardness_W_Objectives$Objective
	|is.na(Hardness_W_Objectives$RL)&(Hardness_W_Objectives$ResQualCode=="ND"|Hardness_W_Objectives$ResQualCode=="DNQ"))]<--100000000 #imperfect results in very negative number




#Create table of samples thrown out due to quantitation limits
Hardness_Tossed<-filter(Hardness_W_Objectives, (Result==-100000000))
Hardness_Tossed$Type<-NULL
#Hardness_Tossed$RL<-NULL
#Hardness_Tossed$MDL<-NULL
#Hardness_Tossed$ResQualCode<-NULL
#Hardness_Tossed$QACode<-NULL
Hardness_Tossed$AveragingPeroid<-4
#Change objective to be 1 for later join
Hardness_Tossed$Objective<-1


#Filter out the samples that did not meet quantitiation limitations
Hardness_W_Objectives<-filter(Hardness_W_Objectives, (Result!=-100000000))


###Remove replicate samples (collected on the same day and station) 
##from hardness analytes table where only total fraction metals were reported
Hardness_W_Objecitves<-tbl_df(as.data.table(Hardness_W_Objectives)[,mean(Result),list(AnalyteName,MatrixName
	,SampleDate,BeneficialUse,StationCode,ProjectName,UnitName,TargetLatitude
	,TargetLongitude,Waterbody,WBID,Wbtype,FractionName,Type)])
	names(Hardness_W_Objectives)[names(Hardness_W_Objectives)=="V1"]<-"Result"

#Remove unecessary fields from hardness data
#Hardness_W_Objectives$TargetLatitude<-NULL
#Hardness_W_Objectives$TargetLongitude<-NULL
#Hardness_W_Objectives$Wbtype<-NULL
Hardness_W_Objectives$Type<-NULL
Hardness_W_Objectives$AveragingPeroid<-4
Hardness_W_Objectives$AveragingPeroid[which(Hardness_W_Objectives$AnalyteName=="Silver")]<-1


#Create table of hardness adjusted data that have units other than ug/L
#If this table gets exported with any rows, we will need to add a conversion section to this code
HardnessUnitsTossedEnd<-subset(ExportedData,(AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc"))

#Remvoe data from this table if it falls in the ocean
HardnessTossedEnd<-anti_join(HardnessTossedEnd,Ocean_Waters)

HardnessUnitsTossedEnd<-filter(HardnessUnitsTossedEnd,!(UnitName=="ug/L"|UnitName=="mg/L"|UnitName=="pg/L"|UnitName=="ng/L"|UnitName=="g/L"))
HardnessUnitsTossedEnd$Issue<-"Issues with the units of Hardness dependent analytes.  Talk to Jacob about adding unit conversion to ReLEP"
AllExportedData<-rbind.data.frame(AllExportedData,HardnessUnitsTossedEnd)

#Cut rows with bad units out of Hardness adjusted data so that only ug/L remains
Hardness_W_Objectives<-Hardness_W_Objectives[Hardness_W_Objectives$UnitName=="ug/L",]

# Remove old values of Hardness dependent metals with BU of WA or CO from W_Waterbodies
W_Waterbodies<-tbl_df(merge(W_Waterbodies,SiteSalinity))
W_Waterbodies<-filter(W_Waterbodies, !((AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium"|AnalyteName=="Chromium VI"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") & (BeneficialUse=="CO"|BeneficialUse=="WA"
	|BeneficialUse=="MA"|BeneficialUse=="ES")&FreshMarine!="O"))
W_Waterbodies$FreshMarine<-NULL


###########___________________End_Hardness_______________________##############################


#Combine Unionized Ammonia with the main data frame before converting units
W_Waterbodies<-rbind.data.frame(W_Waterbodies,Converted_Ammonia)

##############################Begin UNIT CONVERSION SECTION###################

# Convert Results, MDL, and RL that are in mg/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="mg/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="mg/L")]*1000
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="mg/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="mg/L")]*1000
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="mg/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="mg/L")]*1000
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="mg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in pg/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="pg/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="pg/L")]*.000001
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="pg/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="pg/L")]*.000001
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="pg/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="pg/L")]*.000001
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="pg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in ng/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ng/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ng/L")]*.001
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ng/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ng/L")]*.001
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ng/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ng/L")]*.001
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="ng/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in g/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="g/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="g/L")]*1000000
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="g/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="g/L")]*1000000
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="g/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="g/L")]*1000000
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="g/L"]<-"ug/L"

#Change units for specific conductivity to be un uS/cm from umhos (even though its a 1:1 conversion)
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="umhos/cm"]<-"uS/cm"

#Change units for turbidity from NTRU to NTU (even though its a 1:1 conversion)
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="NTRU"]<-"NTU"


UnitsTossed<-filter(ExportedData,!(UnitName=="ug/L"|UnitName=="mg/L"|UnitName=="pg/L"|UnitName=="ng/L"|UnitName=="g/L"|UnitName=="umhos/cm"|UnitName=="uS/cm"|UnitName=="NTU"|UnitName=="NTRU"|UnitName=="Deg C"|UnitName=="none"|UnitName=="%"))
UnitsTossed$Issue<-"Unit Issues.  Data reported in non-standard units."
AllExportedData<-rbind.data.frame(AllExportedData,UnitsTossed)


##########################___End Unit Conversion################################################

#############_____________________Total Nitrogen Assessment R9______________###############

#Currently only applies to Region 9
#Main intent to code the logic hierarchy among different nitrogen species

#All nitrogen data

if(Region=="9"){
  
  #Create placeholder row in case there is no nitrogen data
  nitrogenrownames<-c("StationCode","ProjectName","SampleDate"
                      ,"Waterbody","WBID","Wbtype","BeneficialUse","MatrixName","AnalyteName","UnitName"
                      ,"FractionName","Result","TargetLatitude","TargetLongitude")
  nitrogenfakeinformation<-c("placeholder","Placeholder","1850-01-01","Placeholder"
                             ,"Placeholder","Placeholer","Placeholder","placeholder","Placeholder","na/L","placeholder","0","0","0")
  nitrogenplaceholderinfo<-as.data.frame(rbind.data.frame(nitrogenrownames,nitrogenfakeinformation))
  colnames(nitrogenplaceholderinfo)<-nitrogenrownames
  row.names(nitrogenplaceholderinfo)<-NULL
  nitrogenplaceholderinfo<-nitrogenplaceholderinfo[-1,]
  nitrogenplaceholderinfo<-tbl_df(nitrogenplaceholderinfo)
  nitrogenplaceholderinfo$Result<-as.numeric(nitrogenplaceholderinfo$Result)
  nitrogenplaceholderinfo$Result<-0
  nitrogenplaceholderinfo$TargetLatitude<-as.numeric(nitrogenplaceholderinfo$TargetLatitude)
  nitrogenplaceholderinfo$TargetLongitude<-as.numeric(nitrogenplaceholderinfo$TargetLongitude)
  nitrogenplaceholderinfo$StationCode<-as.character(nitrogenplaceholderinfo$StationCode)
  nitrogenplaceholderinfo$ProjectName<-as.character(nitrogenplaceholderinfo$ProjectName)
  nitrogenplaceholderinfo$SampleDate<-ymd(nitrogenplaceholderinfo$SampleDate)
  nitrogenplaceholderinfo$Waterbody<-as.character(nitrogenplaceholderinfo$Waterbody)
  nitrogenplaceholderinfo$WBID<-as.character(nitrogenplaceholderinfo$WBID)
  nitrogenplaceholderinfo$Wbtype<-as.character(nitrogenplaceholderinfo$Wbtype)
  nitrogenplaceholderinfo$BeneficialUse<-as.character(nitrogenplaceholderinfo$BeneficialUse)
  nitrogenplaceholderinfo$MatrixName<-as.character(nitrogenplaceholderinfo$MatrixName)
  nitrogenplaceholderinfo$AnalyteName<-as.character(nitrogenplaceholderinfo$AnalyteName)
  nitrogenplaceholderinfo$UnitName<-as.character(nitrogenplaceholderinfo$UnitName)
  nitrogenplaceholderinfo$FractionName<-as.character(nitrogenplaceholderinfo$FractionName)
  
  
Nitrogen<-filter(W_Waterbodies,(AnalyteName=="Nitrogen, Total"|AnalyteName=="Ammonia as N"
                                |AnalyteName=="Nitrate + Nitrite as N"|AnalyteName=="Nitrate as N"
                                |AnalyteName=="Nitrite as N"|AnalyteName=="Nitrogen, Organic"
                                |AnalyteName=="Nitrogen, Total Kjeldahl"))

#Average results to remove replicates
Nitrogen<-as.data.table(Nitrogen)[,mean(Result),list(AnalyteName,StationCode
                        ,TargetLatitude,TargetLongitude,SampleDate,MatrixName
                        ,FractionName,MDL,RL,ResQualCode,ProjectName,UnitName
                        ,Waterbody,WBID,Wbtype,BeneficialUse)]
names(Nitrogen)[names(Nitrogen)=="V1"]<-"Result"

#Check for quantitation discards
NitrogenDiscards<-Nitrogen
NitrogenDiscards$Objective<-1000
NitrogenDiscards<-NitrogenDiscards[which((NitrogenDiscards$ResQualCode=="ND"|NitrogenDiscards$ResQualCode=="DNQ")&NitrogenDiscards$RL>NitrogenDiscards$Objective),]
NitrogenDiscards$Result<--100000000
NitrogenDiscards<-unique(NitrogenDiscards)

#Filter out quantitaiton discards from nitrogen table
Nitrogen$Objective<-1000
Nitrogen<-subset(Nitrogen,!((ResQualCode=="ND"|ResQualCode=="DNQ")&RL>Objective))
Nitrogen$Objective<-NULL



KjeldahlNitrogenDays<-filter(Nitrogen,AnalyteName=="Nitrogen, Total Kjeldahl")
KjeldahlNitrogenDays<-distinct(select(KjeldahlNitrogenDays
                                      ,StationCode,SampleDate
                                      ,Waterbody,MatrixName,BeneficialUse))

#Highest level of hierarchy
#Total nitrogen data if we have this data, no further processing is required
TotalNitrogen<-filter(W_Waterbodies,AnalyteName=="Nitrogen, Total")
TotalNitrogen$ResQualCode<-NULL
TotalNitrogen$QACode<-NULL
TotalNitrogen$RL<-NULL
TotalNitrogen$MDL<-NULL

#station days with total nitrogen
TotalNitrogenDays<-distinct(select(TotalNitrogen
                                   ,StationCode,SampleDate,Waterbody
                                   ,MatrixName,BeneficialUse))

#Station days needing more processing
NitrogenNoTotal<-anti_join(Nitrogen,TotalNitrogenDays)

###########################################################
#second and third levels of hierarchy, nitrate and nitrite are present
NitrateAndNitrite<-filter(NitrogenNoTotal
                          ,AnalyteName=="Nitrate + Nitrite as N")

NitrateAndNitriteDays<-distinct(select(NitrateAndNitrite
                                       ,StationCode,SampleDate
                                       ,Waterbody,MatrixName,BeneficialUse))

#level two had kjeldahl nitrogen
KjeldahlNitrogen<-tbl_df(merge(Nitrogen,NitrateAndNitriteDays))
KjeldahlNitrogen<-filter(KjeldahlNitrogen
                         ,AnalyteName=="Nitrogen, Total Kjeldahl"
                         |AnalyteName=="Nitrate + Nitrite as N")
KjeldahlNitrogen$AnalyteName<-"Nitrogen, Total"
KjeldahlNitrogen<-as.data.table(KjeldahlNitrogen)[,sum(Result)
                 ,list(AnalyteName,StationCode,TargetLatitude
                 ,TargetLongitude,SampleDate,BeneficialUse,ProjectName
                 ,FractionName,MatrixName,UnitName
                 ,Wbtype,Waterbody,WBID)]
names(KjeldahlNitrogen)[names(KjeldahlNitrogen)=="V1"]<-"Result"

#level three has nitrate and nitrite but does not have kjeldahl
NitrateAndNitriteNoKjeldahl<-tbl_df(merge(Nitrogen,NitrateAndNitriteDays))
NitrateAndNitriteNoKjeldahl<-anti_join(NitrateAndNitriteNoKjeldahl,KjeldahlNitrogenDays)
NitrateAndNitriteNoKjeldahl<-filter(NitrateAndNitriteNoKjeldahl
                        ,(AnalyteName=="Nitrate + Nitrite as N"
                        |AnalyteName=="Nitrogen, Organic"
                        |AnalyteName=="Ammonia as N"))
#had to add another level of ifelse here because this table is sometimes empty and the if
#statement around the nutrients section was breaking because of the empty table.
#doesnt make sense, but this fixed the issue.
if(length(NitrateAndNitriteNoKjeldahl[,1]>0)){
NitrateAndNitriteNoKjeldahl$AnalyteName<-"Nitrogen, Total"
}else{
  NitrateAndNitriteNoKjeldahl<-nitrogenplaceholderinfo
}
#end of ifelse
NitrateAndNitriteNoKjeldahl<-as.data.table(NitrateAndNitriteNoKjeldahl)[,sum(Result)
                        ,list(AnalyteName,StationCode,TargetLatitude
                        ,TargetLongitude,SampleDate,BeneficialUse,ProjectName
                        ,FractionName,MatrixName,UnitName
                        ,Wbtype,Waterbody,WBID)]
names(NitrateAndNitriteNoKjeldahl)[names(NitrateAndNitriteNoKjeldahl)=="V1"]<-"Result"

###########################################################
#fourth and fith levels of hierarchy do not have Nitrate and Nitrite as N
NitrogenNoTotalNoNitrateNitrite<-anti_join(NitrogenNoTotal,NitrateAndNitriteDays)

#fourth level has kjeldahl
KjeldahlNitrogenNoTotalNoNitrateNitrite<-tbl_df(merge(NitrogenNoTotalNoNitrateNitrite,KjeldahlNitrogenDays))
KjeldahlNitrogenNoTotalNoNitrateNitrite<-filter(KjeldahlNitrogenNoTotalNoNitrateNitrite
                                                ,(AnalyteName=="Nitrogen, Total Kjeldahl"
                                                  |AnalyteName=="Nitrate as N"
                                                  |AnalyteName=="Nitrite as N"))
KjeldahlNitrogenNoTotalNoNitrateNitrite$AnalyteName<-"Nitrogen, Total"
KjeldahlNitrogenNoTotalNoNitrateNitrite<-as.data.table(KjeldahlNitrogenNoTotalNoNitrateNitrite)[,sum(Result)
                                                                                                ,list(AnalyteName,StationCode,TargetLatitude
                                                                                                      ,TargetLongitude,SampleDate,BeneficialUse,ProjectName
                                                                                                      ,FractionName,MatrixName,UnitName
                                                                                                      ,Wbtype,Waterbody,WBID)]
names(KjeldahlNitrogenNoTotalNoNitrateNitrite)[names(KjeldahlNitrogenNoTotalNoNitrateNitrite)=="V1"]<-"Result"


#Fifth level does not have kjeldal
NoKjeldahlNitrogenNoTotalNoNitrateNitrite<-anti_join(NitrogenNoTotalNoNitrateNitrite,KjeldahlNitrogenDays)
NoKjeldahlNitrogenNoTotalNoNitrateNitrite<-filter(NoKjeldahlNitrogenNoTotalNoNitrateNitrite
                                                  ,(AnalyteName=="Nitrate as N"
                                                    |AnalyteName=="Nitrite as N"
                                                    |AnalyteName=="Nitrogen, Organic"
                                                    |AnalyteName=="Ammonia as N"))
NoKjeldahlNitrogenNoTotalNoNitrateNitrite$AnalyteName<-"Nitrogen, Total"
NoKjeldahlNitrogenNoTotalNoNitrateNitrite<-as.data.table(NoKjeldahlNitrogenNoTotalNoNitrateNitrite)[,sum(Result)
                                                      ,list(AnalyteName,StationCode,TargetLatitude
                                                      ,TargetLongitude,SampleDate,BeneficialUse,ProjectName
                                                      ,FractionName,MatrixName,UnitName
                                                      ,Wbtype,Waterbody,WBID)]
names(NoKjeldahlNitrogenNoTotalNoNitrateNitrite)[names(NoKjeldahlNitrogenNoTotalNoNitrateNitrite)=="V1"]<-"Result"

#Combine all the Total Nitrogen Data back together again
AllTotalNitrogen<-nitrogenplaceholderinfo
AllTotalNitrogen<-rbind.data.frame(AllTotalNitrogen,TotalNitrogen)
AllTotalNitrogen<-rbind.data.frame(AllTotalNitrogen,KjeldahlNitrogen)
AllTotalNitrogen<-rbind.data.frame(AllTotalNitrogen,NitrateAndNitriteNoKjeldahl)
AllTotalNitrogen<-rbind.data.frame(AllTotalNitrogen,KjeldahlNitrogenNoTotalNoNitrateNitrite)
AllTotalNitrogen<-rbind.data.frame(AllTotalNitrogen,NoKjeldahlNitrogenNoTotalNoNitrateNitrite)

#Add objectives before rejoining with main data frame
AllTotalNitrogen<-tbl_df(merge(AllTotalNitrogen,Analytes))

#Add necessary fields for data to be joined to W_Objectives
AllTotalNitrogen$ResQualCode<-"="
AllTotalNitrogen$QACode<-"None"
AllTotalNitrogen$MDL<-0
AllTotalNitrogen$RL<-0
AllTotalNitrogen$MDL<-as.numeric(AllTotalNitrogen$MDL)
AllTotalNitrogen$RL<-as.numeric(AllTotalNitrogen$RL)


#Remove total nitrogen data from main data frame to avoid double counting
W_Waterbodies<-filter(W_Waterbodies,AnalyteName!="Nitrogen, Total")



#############_______/\________End Total Nitrogen Assessment R9____/\________###############
###########################################################################################

############_______\/_________Begin total phosphorus assessment R9___\/_____###############
###########################################################################################

Phosphorus<-filter(W_Waterbodies,(AnalyteName=="Phosphorus as P"|AnalyteName=="OrthoPhosphate as P"))

#Remove replicates
Phosphorus<-as.data.table(Phosphorus)[,mean(Result),list(AnalyteName,StationCode
                               ,TargetLatitude,TargetLongitude,SampleDate,MatrixName
                               ,FractionName,MDL,RL,ResQualCode,ProjectName,UnitName
                               ,Waterbody,WBID,Wbtype,BeneficialUse)]
names(Phosphorus)[names(Phosphorus)=="V1"]<-"Result"

#check for quantitation discards
Phosphorus$Objective<-100
PhosphorusDiscards<-Phosphorus
PhosphorusDiscards<-PhosphorusDiscards[which((PhosphorusDiscards$ResQualCode=="ND"|PhosphorusDiscards$ResQualCode=="DNQ")&PhosphorusDiscards$RL>PhosphorusDiscards$Objective),]
PhosphorusDiscards$Result<--100000000
PhosphorusDiscards<-unique(PhosphorusDiscards)

#Remove discards from phosphorus table
Phosphorus<-subset(Phosphorus,!((ResQualCode=="ND"|ResQualCode=="DNQ")&RL>Objective))
Phosphorus$Objective<-NULL
Phosphorus$ResQualCode<-NULL

#Level 1, phosphorus, total available
PhosphorusTotal<-filter(Phosphorus,(AnalyteName=="Phosphorus as P"&FractionName=="Total"))

#Phosphorus total days
PhosphorusTotalDays<-distinct(select(PhosphorusTotal,StationCode,SampleDate
                    ,Waterbody,MatrixName,BeneficialUse))

#Level 2, phosphorus, dissolved for days that to not have total
PhosphorusDissolved<-anti_join(Phosphorus,PhosphorusTotalDays)

#Level 3 Phoshorus, orthophosphage as p total when phosphorus as p not available
OrthophosphateTotal<-filter(Phosphorus,(AnalyteName=="OrthoPhosphate as P"&FractionName=="Total"))

#OrthophosphateTotalDays
OrthophosphateTotalDays<-distinct(select(OrthophosphateTotal,StationCode,SampleDate
                                          ,Waterbody,MatrixName,BeneficialUse))

#Level 4 phosphorus, orthophosphate dissoled when no other phosphorus is availalbe
OrthophosphateDissolved<-filter(Phosphorus,(AnalyteName=="OrthoPhosphate as P"&FractionName=="Dissolved"))

#Combine all portions together
AllPhosphorusCombined<-nitrogenplaceholderinfo
AllPhosphorusCombined$RL<-0
AllPhosphorusCombined$MDL<-0
AllPhosphorusCombined$MDL<-as.numeric(AllPhosphorusCombined$MDL)
AllPhosphorusCombined$RL<-as.numeric(AllPhosphorusCombined$RL)
AllPhosphorusCombined<-rbind.data.frame(AllPhosphorusCombined,PhosphorusTotal)
AllPhosphorusCombined<-rbind.data.frame(AllPhosphorusCombined,PhosphorusDissolved)
AllPhosphorusCombined<-rbind.data.frame(AllPhosphorusCombined,OrthophosphateTotal)
AllPhosphorusCombined<-rbind.data.frame(AllPhosphorusCombined,OrthophosphateDissolved)
AllPhosphorusCombined$AnalyteName<-"Phosphorus as P"
AllPhosphorusCombined$FractionName<-"Total"

#Add objectives before rejoining with main data frame
AllPhosphorusCombined<-tbl_df(merge(AllPhosphorusCombined,Analytes))

#Add necessary fields for data to be joined to W_Objectives
AllPhosphorusCombined$ResQualCode<-"="
AllPhosphorusCombined$QACode<-"None"

#Remove total phosphorus from W_Waterbodies to avoid double counting
W_Waterbodies<-filter(W_Waterbodies,AnalyteName!="Phosphorus as P")
}
############_______/\_________End total phosphorus assessment R9___/\_____###############
###########################################################################################

#####################___________Start Pentachlorophenol Objective Calc_____#######################
#Create table of pentachlorophenol data then average for replicates
PentaData<-W_Waterbodies[W_Waterbodies$AnalyteName=="Pentachlorophenol"&(W_Waterbodies$BeneficialUse=="CO"|W_Waterbodies$BeneficialUse=="WA"),]


#Remove replicates from penta data
PentaData<-tbl_df(as.data.table(PentaData)[,mean(Result),list(AnalyteName,MatrixName
	,SampleDate,BeneficialUse,StationCode,ProjectName,UnitName,RL,MDL,QACode
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,ResQualCode,FractionName)])
	names(PentaData)[names(PentaData)=="V1"]<-"Result"

#Remove replicates from the pH data
PentapH<-tbl_df(as.data.table(PentapH)[,mean(pHResult),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,TargetLatitude,TargetLongitude
	,Waterbody,WBID,Wbtype)])
	names(PentapH)[names(PentapH)=="V1"]<-"pHResult"


#Merge with pH data add a default pH value of 7.8 for all PentaChlorophenol results 
#that do not have coresponding pH result to use
PentaData<-tbl_df(merge(PentaData,PentapH,by=c("StationCode","ProjectName","SampleDate"
	,"TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse"
	,"MatrixName"), all.x=TRUE))
PentaData$pHResult[which(is.na(PentaData$pHResult))]<-7.8

#Calculate objective based on pH, then remove uncessary columns
Penta_W_Objectives<-PentaData
Penta_W_Objectives<-tbl_df(mutate(Penta_W_Objectives,Objective=(exp(1.005*(pHResult)-5.134))))
Penta_W_Objectives$pHResult<-NULL
Penta_W_Objectives$AveragingPeroid<-4

#Add objective language, and reference codes
Penta_W_Objectives$Evaluation_Guideline<-""
Penta_W_Objectives$Eval_Ref_Number<-""
Penta_W_Objectives$Objective_Language<-paste0("The Pentachlorophenol criterion continuous concentration (expressed as a 4-day average) to protect aquatic life in freshwater is pH dependent.  The criteria has a value of 15 ug/L when based on a default pH of 7.8. (California Toxics Rule, 2000).  ")
Penta_W_Objectives$Objective_Ref_Number<-"476"

#Remove Penta data from the main data frame
W_Waterbodies<-filter(W_Waterbodies, !(AnalyteName=="Pentachlorophenol" & (BeneficialUse=="CO"|BeneficialUse=="WA")))

######################______________End Penta Objective Calc______#######################################

###############_____________________Begin Total Ammonia Criteria Calculation______###########

#Split out total ammonia so that the objective can be calculated
TotalAmmoniaObjectives<-W_Waterbodies[W_Waterbodies$AnalyteName=="Ammonia as N"&(W_Waterbodies$BeneficialUse=="CO"|W_Waterbodies$BeneficialUse=="WA"),]

#Merge ammonia data with pH and Temperature.
TotalAmmoniaObjectives<-tbl_df(merge(TotalAmmoniaObjectives,PentapH,by=c("StationCode","ProjectName"
	,"SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaObjectives<-tbl_df(merge(TotalAmmoniaObjectives,temp,by=c("StationCode","ProjectName"
	,"SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))

#Calculate objectives based on formula in EPA 2013
TotalAmmoniaObjectives<-mutate(TotalAmmoniaObjectives,Objective=1000*.8876*((.0278/(1+10^(7.688-pHResult)))+(1.1994/(1+10^(pHResult-7.688))))*(2.126*10^(.028*(20-TempResult))))

#Remove unecessary Columns
TotalAmmoniaObjectives$TempResult<-NULL
TotalAmmoniaObjectives$pHResult<-NULL


#Add objective language, and reference codes
TotalAmmoniaObjectives$Evaluation_Guideline<-""
TotalAmmoniaObjectives$Eval_Ref_Number<-""
TotalAmmoniaObjectives$Objective_Language<-paste0("The Total Ammonia criterion continuous concentration (expressed as a 30-day average) to protect aquatic life in freshwater is Temperature and pH dependent, and was calculated according to the formula listed in the Aquatic Life Ambient Water Quality Criteria for Ammonia - Freshwater 2013 document.")
TotalAmmoniaObjectives$Objective_Ref_Number<-"4107"
TotalAmmoniaObjectives$AveragingPeroid<-30
TotalAmmoniaObjectives<-tbl_df(TotalAmmoniaObjectives)


#Remove the rows of data from main data frame where the objectives were just calculated
W_Waterbodies<-filter(W_Waterbodies, !(AnalyteName=="Ammonia as N" & (BeneficialUse=="CO"|BeneficialUse=="WA")))


################_______________End Total Ammonia Objective Calc______##############################################

###################################################################################################################

###################################################################################################################

#############____________________Start Summing Pollutant Calculations________________________######################

#Create table of pollutants that need summing
SummingPollutantsSum<-tbl_df(merge(W_Waterbodies,SummingPollutants,by="AnalyteName"))


########
######
####
##
#Split out pyrethroids to calculate LC50 adjusted results
#Attempt BPA version of assessment from Region 5 BPA when in 
#Regions 3 or 5


  #Geneate data frame of all the pyrethroids covered under the BPA
  Pyrethroids<-filter(W_Waterbodies,(AnalyteName=="Bifenthrin"
                    |AnalyteName=="Cyfluthrin, total"|AnalyteName=="Cyfluthrin, gamma-"
                    |AnalyteName=="Cyfluthrin, beta-"|AnalyteName=="Cyfluthrin-1"
                    |AnalyteName=="Cyfluthrin-2"|AnalyteName=="Cyfluthrin-3"
                    |AnalyteName=="Cyfluthrin-4"|AnalyteName=="Cyfluthrin-2/Cyfluthrin-4"
                    
                    |AnalyteName=="Cypermethrin, Total"|AnalyteName=="Cypermethrin-1"
                    |AnalyteName=="Cypermethrin-2"|AnalyteName=="Cypermethrin-2/Cypermethrin-4"
                    |AnalyteName=="Cypermethrin-3"|AnalyteName=="Cypermethrin-4"
                    
                    |AnalyteName=="Esfenvalerate"|AnalyteName=="Esfenvalerate/Fenvalerate, Total"
                    |AnalyteName=="Esfenvalerate/Fenvalerate-1"|AnalyteName=="Esfenvalerate/Fenvalerate-2"
                    
                    |AnalyteName=="Cyhalothrin, lambda-1"|AnalyteName=="Cyhalothrin, lambda-2"
                    |AnalyteName=="Cyhalothrin, Total lambda-"
                    
                    |AnalyteName=="Permethrin, cis-"
                    |AnalyteName=="Permethrin, Total"|AnalyteName=="Permethrin, trans-"))
#Remove pyrethroids from W_Waterbodies table to avoid double counting
W_Waterbodies<-anti_join(W_Waterbodies,Pyrethroids)

#Remove these rows from the summing table as a whole they will be added back later
SummingPollutantsSum<-anti_join(SummingPollutantsSum,Pyrethroids)

  #Add summing Name to the data frame create a field SummingName filled with NAs first
    Pyrethroids$SummingName<-"NA"
    
  #Fill in the SummingName based on the pollutant
   
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Bifenthrin")]<-"Bifenthrin"
    
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin, total")]<-"Cyfluthrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin, gamma-")]<-"Cyfluthrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin, beta-")]<-"Cyfluthrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin-1")]<-"Cyfluthrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin-2")]<-"Cyfluthrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin-3")]<-"Cyfluthrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin-4")]<-"Cyfluthrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyfluthrin-2/Cyfluthrin-4")]<-"Cyfluthrin"
    
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cypermethrin, Total")]<-"Cypermethrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cypermethrin-1")]<-"Cypermethrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cypermethrin-2")]<-"Cypermethrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cypermethrin-3")]<-"Cypermethrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cypermethrin-4")]<-"Cypermethrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cypermethrin-2/Cypermethrin-4")]<-"Cypermethrin"
    
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Esfenvalerate")]<-"Esfenvalerate"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Esfenvalerate/Fenvalerate, Total")]<-"Esfenvalerate"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Esfenvalerate/Fenvalerate-1")]<-"Esfenvalerate"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Esfenvalerate/Fenvalerate-2")]<-"Esfenvalerate"
    
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyhalothrin, lambda-1")]<-"Lambda-cyhalothrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyhalothrin, lambda-2")]<-"Lambda-cyhalothrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Cyhalothrin, Total lambda-")]<-"Lambda-cyhalothrin"

    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Permethrin, cis-")]<-"Permethrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Permethrin, trans-")]<-"Permethrin"
    Pyrethroids$SummingName[which(Pyrethroids$AnalyteName=="Permethrin, Total")]<-"Permethrin"
  
    
  
  #Convert results to Zero if the ResQualCode is ND or DNQ as specified in the BPA
  TempPyrethroids<-Pyrethroids
  Pyrethroids$CEDENName<-Pyrethroids$AnalyteName
  Pyrethroids$AnalyteName<-Pyrethroids$SummingName
  Pyrethroids<-tbl_df(merge(Pyrethroids,Analytes))
  PyrethroidDiscards<-Pyrethroids[which((Pyrethroids$ResQualCode=="ND"|Pyrethroids$ResQualCode=="DNQ")&Pyrethroids$RL>Pyrethroids$Objective),]
  PyrethroidDiscards$Result<--100000000
  PyrethroidDiscards$CEDENName<-NULL
  PyrethroidDiscards$SummingName<-NULL
  PyrethroidDiscards<-unique(PyrethroidDiscards)
  
  #Filter our quantitation discards from pyrethroid table
  Pyrethroids<-subset(Pyrethroids,!((ResQualCode=="ND"|ResQualCode=="DNQ")&RL>Objective))
  Pyrethroids$AnalyteName<-Pyrethroids$CEDENName
  #trim uncessary fields to match pre quantadjusted table
  Pyrethroids<-subset(Pyrethroids,select=c(AnalyteName,Waterbody,WBID,MatrixName,FractionName,StationCode,ProjectName,SampleDate,UnitName,Result,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude,Wbtype,BeneficialUse,SummingName))

      
  ##Split out pyrethroids by type (total or congener)
  TotalPyrethroids<-filter(Pyrethroids,(AnalyteName=="Bifenthrin"|AnalyteName=="Cyfluthrin, total"
                    |AnalyteName=="Cypermethrin, Total"|AnalyteName=="Esfenvalerate/Fenvalerate, Total")
                    |AnalyteName=="Cyhalothrin, Total lambda-")
  
  #Get list of station days that have total portion of pyrethroid reported
  TotalPyrethroidDays<-distinct(select(TotalPyrethroids,StationCode,FractionName,SampleDate,ProjectName,SummingName))
  
  #Remove replicates by averaging total results submitted at same station 
  #on same day by same parent project for same pollutant and change AnalyteName to be generic
  TotalPyrethroids<-as.data.table(TotalPyrethroids)[,mean(Result),list(SummingName,StationCode,TargetLatitude
                    ,TargetLongitude,SampleDate,BeneficialUse,ProjectName,FractionName,MatrixName,UnitName
                    ,MDL,RL,Waterbody,WBID)]
  names(TotalPyrethroids)[names(TotalPyrethroids)=="V1"]<-"Result"
  names(TotalPyrethroids)[names(TotalPyrethroids)=="SummingName"]<-"AnalyteName"
  
  #Create table of non-total pyrethroids (congeners) these are station days that will need
  #to be summed to "Total" before they can move on in the process
  CongenerPyrethroids<-tbl_df(anti_join(Pyrethroids,TotalPyrethroidDays))
  
  #Remove Replicates by averaging results collected at the same staiton on the same day for the same pollutant
  CongenerPyrethroids<-as.data.table(CongenerPyrethroids)[,mean(Result),list(AnalyteName,SummingName,StationCode,TargetLatitude,TargetLongitude,SampleDate,BeneficialUse,ProjectName,FractionName,MatrixName,UnitName,MDL,RL,Waterbody,WBID)]
  names(CongenerPyrethroids)[names(CongenerPyrethroids)=="V1"]<-"Result"
  
  #Sum the various congeners together and change pollutant name to be generic
  CongenerPyrethroids<-as.data.table(CongenerPyrethroids)[,sum(Result),list(SummingName,StationCode,TargetLatitude,TargetLongitude,SampleDate,BeneficialUse,ProjectName,FractionName,MatrixName,UnitName,MDL,RL,Waterbody,WBID)]
  names(CongenerPyrethroids)[names(CongenerPyrethroids)=="V1"]<-"Result"
  names(CongenerPyrethroids)[names(CongenerPyrethroids)=="SummingName"]<-"AnalyteName"
  
  #Stack TotalPyrethroids and CongenerPyrethroids for analysis
  AllPyrethroids<-rbind.data.frame(TotalPyrethroids,CongenerPyrethroids)
  
  #If region is 3, adjust quantitation discard table now no other processing id done to 
  #pyrethroids as the result of summing section
  if(Region==3){
    PyrethroidDiscards<-anti_join(PyrethroidDiscards,AllPyrethroids
                  ,by=c("StationCode","SampleDate","MatrixName","ProjectName"
                  ,"AnalyteName","Waterbody","WBID","BeneficialUse"))
    PyrethroidDiscards<-unique(PyrethroidDiscards)
  }
  
##############################################################################
##############################################################################
##############################################################################
  #The next section does not need to be done for Region 3 because they do not 
  #include freely dissolved conversion and does not include TU assessmen for
  #"Pyrethroids" as a whole
  
 if(Region!=3){
  
  #Get list of StationDatys with dissolved fraction these do not need to be converted
  DissolvedPyrethroids<-AllPyrethroids[AllPyrethroids$FractionName=="Dissolved",]
  
  DissolvedPyrethroidsDays<-distinct(select(DissolvedPyrethroids,StationCode,SampleDate,ProjectName,AnalyteName))
  
  #Remove data from from AllPyrethroids for station days that have the dissolved fraction reported
  #This data does not need to be converted to freely dissolved data.  Instead the dissolved fraction is used
  #Create table of data that does need to be converted first
  AllTotalPyrethroids<-anti_join(AllPyrethroids,DissolvedPyrethroidsDays)
  AllTotalPyrethroids$FractionName[which(AllTotalPyrethroids$FractionName!="Particulate")]<-"Total"
  
  AllPyrethroids<-DissolvedPyrethroids
  
  #Begin conversion of AllTotalPyrethroids to FreelyDissolvedFraction by processing DOC and POC for
  #Joining with AllTotalPyrethroids
  
  #Remove rows of Carbon data that are ND or DNQ
  OrganicCarbon<-OrganicCarbon[which(OrganicCarbon$ResQualCode!="ND"|OrganicCarbon$ResQualCode!="DNQ"),]
  
  #Convert untis into kg/L
  
  OrganicCarbon$Result[which(OrganicCarbon$UnitName=="g/L")]<-OrganicCarbon$Result[which(OrganicCarbon$UnitName=="g/L")]/1000
  OrganicCarbon$RL[which(OrganicCarbon$UnitName=="g/L")]<-OrganicCarbon$RL[which(OrganicCarbon$UnitName=="g/L")]/1000
  OrganicCarbon$MDL[which(OrganicCarbon$UnitName=="g/L")]<-OrganicCarbon$MDL[which(OrganicCarbon$UnitName=="g/L")]/1000
  OrganicCarbon$UnitName[which(OrganicCarbon$UnitName=="g/L")]<-"kg/L"
  
  OrganicCarbon$Result[which(OrganicCarbon$UnitName=="mg/L")]<-OrganicCarbon$Result[which(OrganicCarbon$UnitName=="mg/L")]/1000000
  OrganicCarbon$RL[which(OrganicCarbon$UnitName=="mg/L")]<-OrganicCarbon$RL[which(OrganicCarbon$UnitName=="mg/L")]/1000000
  OrganicCarbon$MDL[which(OrganicCarbon$UnitName=="mg/L")]<-OrganicCarbon$MDL[which(OrganicCarbon$UnitName=="mg/L")]/1000000
  OrganicCarbon$UnitName[which(OrganicCarbon$UnitName=="mg/L")]<-"kg/L"
  
  OrganicCarbon$Result[which(OrganicCarbon$UnitName=="ug/L")]<-OrganicCarbon$Result[which(OrganicCarbon$UnitName=="ug/L")]/1000000000
  OrganicCarbon$RL[which(OrganicCarbon$UnitName=="ug/L")]<-OrganicCarbon$RL[which(OrganicCarbon$UnitName=="ug/L")]/1000000000
  OrganicCarbon$MDL[which(OrganicCarbon$UnitName=="ug/L")]<-OrganicCarbon$MDL[which(OrganicCarbon$UnitName=="ug/L")]/1000000000
  OrganicCarbon$UnitName[which(OrganicCarbon$UnitName=="ug/L")]<-"kg/L"
  
  #Remove Replicates
  OrganicCarbon<-as.data.table(OrganicCarbon)[,mean(Result),list(ProjectName,AnalyteName,StationCode,SampleDate
                                              ,MatrixName,UnitName,TargetLatitude,TargetLongitude,Waterbody,WBID
                                              ,FractionName,BeneficialUse)]
  names(OrganicCarbon)[names(OrganicCarbon)=="V1"]<-"Result"
  
  #Remove unecessary fields
  OrganicCarbon<-subset(OrganicCarbon, select=c("ProjectName","AnalyteName","StationCode","TargetLatitude","TargetLongitude","SampleDate","Result","MatrixName"))
  
  #Remove duplicates
  OrganicCarbon<-distinct(OrganicCarbon)
  
  #Split TOC and DOC apart for easy working
  TOC<-OrganicCarbon[OrganicCarbon$AnalyteName=="Total Organic Carbon",]
  TOC$TOCResult<-TOC$Result
  TOC$Result<-NULL
  TOC$AnalyteName<-NULL
  
  DOC<-OrganicCarbon[OrganicCarbon$AnalyteName=="Dissolved Organic Carbon",]
  DOC$DOCResult<-DOC$Result
  DOC$Result<-NULL
  DOC$AnalyteName<-NULL
  
  #Create table of DOC, TOC, and in turn POC reuslts for each station day it is avialable for
  POC<-tbl_df(merge(DOC,TOC))
  POC<-mutate(POC,POCResult=(TOCResult-DOCResult))
  

  #Join DOC, TOC, and POC to pyrethroid data that needs conversion
  AllTotalPyrethroids<-tbl_df(merge(AllTotalPyrethroids,POC,by=c("SampleDate","StationCode","TargetLatitude"
                                  ,"TargetLongitude","MatrixName","ProjectName"),all.x=TRUE))
  
  #convert all non values to be zero
  AllTotalPyrethroids$TOCResult[which(is.na(AllTotalPyrethroids$TOCResult))]<-0
  AllTotalPyrethroids$DOCResult[which(is.na(AllTotalPyrethroids$DOCResult))]<-0  
  AllTotalPyrethroids$POCResult[which(is.na(AllTotalPyrethroids$POCResult))]<-0
  
  #Create table of Koc and Kdoc numbers to join with AllTotalPyrethroids
  AnalyteName<-c("Bifenthrin","Cyfluthrin","Cypermethrin","Esfenvalerate","Lambda-cyhalothrin","Permethrin")
  Koc<-c(4228000,3870000,3105000,7220000,2056000,6075000)
  Kdoc<-c(1737127,2432071,762765,1733158,952809,957703)
  PyrethroidConversions<-data.frame(AnalyteName,Koc,Kdoc)
  
  #Add conversion factors to TotalPyrethroids for conversion
  AllTotalPyrethroids<-tbl_df(merge(AllTotalPyrethroids,PyrethroidConversions))
  
  #Convert total fraction into dissolved fraction units of pyrethroids unimportant here because conversion is unitless
  AllTotalPyrethroids<-mutate(AllTotalPyrethroids,DissolvedFraction=(Result/(1+(Koc*POCResult)+(Kdoc*DOCResult))))
  
  #Adjust field names appropriately
  AllTotalPyrethroids$Result<-AllTotalPyrethroids$DissolvedFraction
  AllTotalPyrethroids$FractionName<-"Dissolved"
  AllTotalPyrethroids$DissolvedFraction<-NULL
  AllTotalPyrethroids$Koc<-NULL
  AllTotalPyrethroids$Kdoc<-NULL
  AllTotalPyrethroids$TOCResult<-NULL
  AllTotalPyrethroids$POCResult<-NULL
  AllTotalPyrethroids$DOCResult<-NULL
  
  
  #Stack converted pyrethroids and dissolved pyurethroid data frame together for assessment
  AllPyrethroids<-rbind.data.frame(AllPyrethroids,AllTotalPyrethroids)
  
  #Remove replicates again, just in case
  AllPyrethroids<-as.data.table(AllPyrethroids)[,mean(Result),list(AnalyteName,StationCode
                                ,TargetLatitude,TargetLongitude,SampleDate,MatrixName
                                ,FractionName,MDL,RL,ProjectName,UnitName,Waterbody,WBID
                                ,BeneficialUse)]
  names(AllPyrethroids)[names(AllPyrethroids)=="V1"]<-"Result"
  

  #Save a copy of All Pyrethroids in summed form for individual assessment
  SummedIndividualPyrethroids<-AllPyrethroids
  
  #Adjust fractioname of pyrethroid discards to dissolved since this is the only
  #fraction that ends up with LOEs
  PyrethroidDiscards$FractionName<-"Dissolved"
  
  #Remove stationdays from pyrethroid discards if useable samples existed
  PyrethroidDiscards<-anti_join(PyrethroidDiscards,SummedIndividualPyrethroids
              ,by=c("StationCode","SampleDate","MatrixName","ProjectName"
                    ,"AnalyteName","Waterbody","WBID","BeneficialUse"))
  PyrethroidDiscards<-unique(PyrethroidDiscards)
  
  #Add field to sum toxicity units with
  AllPyrethroids$SummingName<-"Pyrethroids"
  
  #Add objective based on pyrethroid being assessed
  AllPyrethroids$Objective<-NA
  AllPyrethroids$Objective[which(AllPyrethroids$AnalyteName=="Bifenthrin")]<-.0001
  AllPyrethroids$Objective[which(AllPyrethroids$AnalyteName=="Cyfluthrin")]<-.0002
  AllPyrethroids$Objective[which(AllPyrethroids$AnalyteName=="Cypermethrin")]<-.0003
  AllPyrethroids$Objective[which(AllPyrethroids$AnalyteName=="Esfenvalerate")]<-.0003
  AllPyrethroids$Objective[which(AllPyrethroids$AnalyteName=="Lambda-cyhalothrin")]<-.0003
  AllPyrethroids$Objective[which(AllPyrethroids$AnalyteName=="Permethrin")]<-.001
  
  #Calculate TU for each stationday
  AllPyrethroids<-mutate(AllPyrethroids,TU=(Result/Objective))
  
  #Sum TUs to get toal TU for the station day
  AllPyrethroids$AnalyteName<-"Pyrethroids"
  AllPyrethroids$SummingName<-NULL
  
  AllPyrethroids<-as.data.table(AllPyrethroids)[,signif(sum(TU),1),list(AnalyteName,BeneficialUse
                            ,UnitName,StationCode,TargetLatitude,TargetLongitude,ProjectName
                            ,SampleDate,MatrixName,FractionName,Waterbody,WBID)]
  names(AllPyrethroids)[names(AllPyrethroids)=="V1"]<-"Result"
  

  
  #add fake MDL and RL values to this data frame for joining purposes.  All data beyond this point should
  #Be considered to have passed quantitaiton anyway.
  AllPyrethroids$MDL<-0
  AllPyrethroids$RL<-0
  
  #Get list of pyrethroid discards
  AllPyrethroidDiscards<-PyrethroidDiscards
  AllPyrethroidDiscards$AnalyteName<-"Pyrethroids"
  AllPyrethroidDiscards$FractionName<-"Dissolved"
  AllPyrethroidDiscards<-unique(AllPyrethroidDiscards)
  AllPyrethroidDiscards<-anti_join(AllPyrethroidDiscards,AllPyrethroids,by=c("AnalyteName"
                  ,"BeneficialUse","UnitName","StationCode","ProjectName","SampleDate"
                  ,"MatrixName","FractionName"))
  #Remove objective info and add info for pyrethroid objective
  AllPyrethroidDiscards<-subset(AllPyrethroidDiscards,select=c(AnalyteName,Waterbody,WBID,MatrixName,FractionName,StationCode,ProjectName,SampleDate,UnitName,Result,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude,Wbtype,BeneficialUse))
  AllPyrethroidDiscards<-tbl_df(merge(AllPyrethroidDiscards,Analytes))

  #Add individual pyrehtroid data back into table for TU pyrehtroids for individual assessment
  AllPyrethroids<-tbl_df(rbind.data.frame(AllPyrethroids,SummedIndividualPyrethroids))
  }

####################/\_End of section that doesnt apply to R3_/\################################

  
  #Add remaining fields for joing with the rest of data
  AllPyrethroids$ResQualCode<-"="
  AllPyrethroids$QACode<-"None"
  AllPyrethroids$Wbtype<-substr(AllPyrethroids$WBID,3,3)
  

  
    AllPyrethroids<-tbl_df(AllPyrethroids)
  

  # 
#########################Assess Organophosphate pesticides###############################
#########################Region 3 only for now#########################

if(Region==3){

OPs<-SummingPollutantsSum[SummingPollutantsSum$SummingName=="Organophosphorus Pesticides",]

#Join pyrehtroid data with the objective (which should be LC50s of the pollutant in question)
#Join to the CEDEN name not the summing name
OPs<-tbl_df(merge(OPs,Analytes,by=c("AnalyteName","BeneficialUse","UnitName")))

#Adjust for quantitation issues
OPs$Result[which(OPs$RL<=OPs$Objective & (OPs$ResQualCode=="ND"|OPs$ResQualCode=="DNQ"))]<-0 #perfect scenario

OPs$Result[which((OPs$RL>OPs$Objective|is.na(OPs$RL))&(OPs$ResQualCode=="ND"|OPs$ResQualCode=="DNQ"))]<--100000000 #imperfect results in zero
OP_Discards<-OPs[OPs$Result==-100000000,]
OPs<-subset(OPs,Result!=-100000000)
OPs<-tbl_df(OPs)

#Remove uncessary fields from OP_Discards
OP_Discards<-subset(OPs,select=c("BeneficialUse","UnitName"
,"StationCode","ProjectName","SampleDate","MatrixName","FractionName"
,"Waterbody","WBID","Wbtype","SummingName","TargetLatitude","TargetLongitude"))
#Summing name (OP pesticides) becomes AnalyteName
OP_Discards$AnalyteName<-OP_Discards$SummingName
#Remove Summing Name field
OP_Discards$SummingName<-NULL


#Remove replicates from the pyrethroid data
OPs<-as.data.table(OPs)[,mean(Result),list(AnalyteName,StationCode,ProjectName
,SampleDate,MatrixName,FractionName,UnitName,TargetLatitude
,TargetLongitude,Waterbody,WBID,Wbtype,BeneficialUse,Objective,SummingName)]
names(OPs)[names(OPs)=="V1"]<-"Result"

#Divide the result by the threshold to get normalized data
OPs<-mutate(OPs,Result=Result/Objective)

#Remove uncessary fields
OPs<-subset(OPs,select=c("AnalyteName","BeneficialUse","UnitName"
,"StationCode","ProjectName","SampleDate","MatrixName","FractionName","Result"
,"Waterbody","WBID","Wbtype","SummingName","TargetLatitude","TargetLongitude"))

#Swap CEDEN analyte name for summed analyte name
OPs$AnalyteName<-NULL
names(OPs)[names(OPs)=="SummingName"]<-"AnalyteName"

#Add threshold normalized results together so that all pyrethroid
#toxicity is summed for the sample day
OPs<-as.data.table(OPs)[,sum(Result),list(AnalyteName,BeneficialUse
,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName,Waterbody,WBID,Wbtype
,TargetLatitude,TargetLongitude)]
names(OPs)[names(OPs)=="V1"]<-"Result"
OPs<-tbl_df(OPs)

#Remove discards from discard table, if there was a useable sample collected for the same stationday
OP_Discards$SummingName<-NULL
OP_Discards$Objective<-1
OP_Discards<-anti_join(OP_Discards,OPs,by=c("Waterbody","AnalyteName","MatrixName","FractionName","BeneficialUse","ProjectName"))

#Add objectives for organophosphates to the OP table
OPs<-tbl_df(merge(OPs,Analytes,by=c("AnalyteName","BeneficialUse","UnitName")))

#Add fake RL, MDL, QACode, and ResQUalCode information so the tables can be joined together easily
#fake RL and MDL is okay here because we already removed quantitation discards from the data set
OPs$RL<-0
OPs$MDL<-0
OPs$QACode<-"None"
OPs$ResQualCode<-"="

}

#######################################################################################################

#add objectives back to the table
AllPyrethroids<-tbl_df(merge(AllPyrethroids, Analytes,by=c("AnalyteName","BeneficialUse","UnitName")))

##########
########
######
###
##
#Split out pollutants with TEF from summing pollutants table and multiply the result 
    #by appropriate TEFs
TEFs<-tbl_df(merge(SummingPollutantsSum,PAH_TEFs,by=c("AnalyteName","SummingName")))

##Remove rows with TEFs that are not in the ocean since the TEs only apply to ocean waters
    #for the water matrix
TEFs<-tbl_df(merge(TEFs,Ocean_Waters))

#adjust result based on TEf by multiplying result by TEF
TEFs<-mutate(TEFs,Result=Result*TEF)
TEFs<-TEFs[which(TEFs$SummingName=="TCDD Equivalents"),]
#Remove TEF field form table
TEFs$TEF<-NULL
TEFs$FreshMarine<-NULL

#Remove the PAH Data from the Summing pollutants table
SummingPollutantsSum<-anti_join(SummingPollutantsSum,TEFs,by=c("AnalyteName","StationCode"
	,"ProjectName","SampleDate","MatrixName","FractionName","Waterbody"
	,"WBID","Wbtype","BeneficialUse","SummingName"))

#Remove all TCDD Equivalents from SummingPollutants table because TCDD only applies
#to ocean waters at the moment, and we do not want to acidentally sum them and add them
#to assess non ocean waters
SummingPollutantsSum<-filter(SummingPollutantsSum,SummingName!="TCDD Equivalents")

#Then add the adjusted PAH rows back in
SummingPollutantsSum<-rbind.data.frame(SummingPollutantsSum,TEFs)

#Change the names of the analytes columns so that we can look up
#objectives based on summing pollutant name instead of CEDEN name
names(SummingPollutantsSum)[names(SummingPollutantsSum)=="AnalyteName"]<-"CEDENName"
names(SummingPollutantsSum)[names(SummingPollutantsSum)=="SummingName"]<-"AnalyteName"

#Convert Aroclors data to total PCB data for aquatic life BUs, also remove congener data for 
#aquatic life PCBs because there is no objective for them
AquaticLifePCBs<-filter(SummingPollutantsSum,(AnalyteName=="Aroclor"&(BeneficialUse=="WA"|BeneficialUse=="CO"|BeneficialUse=="MA"|BeneficialUse=="ES")))
AquaticLifePCBs$AnalyteName<-"PCBs (Polychlorinated biphenyls)"
SummingPollutantsSum<-filter(SummingPollutantsSum,!((AnalyteName=="PCBs (Polychlorinated biphenyls)"|AnalyteName=="PCB, total Congeners")&(BeneficialUse=="CO"|BeneficialUse=="WA"|BeneficialUse=="ES"|BeneficialUse=="MA")))
SummingPollutantsSum<-rbind.data.frame(SummingPollutantsSum,AquaticLifePCBs)

#Split out summing pollutant data that falls in ocean waters to look up objectives in ocean water analytes
OceanSummingPollutantsSum<-tbl_df(merge(SummingPollutantsSum,Ocean_Waters))
OceanSummingPollutantsSum$FreshMarine<-NULL
Ocean_Summing_No_Objectives<-anti_join(OceanSummingPollutantsSum,Ocean_Analytes)
OceanSummingPollutantsSum<-tbl_df(merge(OceanSummingPollutantsSum,Ocean_Analytes,by=c("AnalyteName","UnitName","BeneficialUse")))

#Remove ocean data fro main summing table
SummingPollutantsSum<-anti_join(SummingPollutantsSum,Ocean_Waters)

#Look Up objectives based on summing pollutant name
Sum_No_reps<-tbl_df(merge(SummingPollutantsSum,Analytes,by=c("AnalyteName","UnitName","BeneficialUse")))

#Create a table of summing pollutants that do not have objectives
nosums<-anti_join(SummingPollutantsSum,Analytes,by=c("AnalyteName","UnitName","BeneficialUse"))
#add ocean no sums to nosums
nosums<-tbl_df(rbind.data.frame(nosums,Ocean_Summing_No_Objectives))

#Add ocean summing pollutants and non ocean summing pollutants to be in same table
Sum_No_reps<-tbl_df(rbind.data.frame(Sum_No_reps,OceanSummingPollutantsSum))

# Correct the results based on quantitation limits - I did this before removing
#replicates to avoid averaging samples that we assesed with a different method

Sum_No_reps$Result[which(Sum_No_reps$RL<=Sum_No_reps$Objective 
	& (Sum_No_reps$ResQualCode=="ND"|Sum_No_reps$ResQualCode=="DNQ"))]<-0 #perfect scenario

Sum_No_reps$Result[which((Sum_No_reps$RL>Sum_No_reps$Objective|is.na(Sum_No_reps$RL))
	&(Sum_No_reps$ResQualCode=="ND"|Sum_No_reps$ResQualCode=="DNQ"))]<--100000000 #imperfect results in very negative number
Sum_No_reps_Discards<-Sum_No_reps[Sum_No_reps$Result==-100000000,]
Sum_No_reps_Discards$CEDENName<-NULL
Sum_No_reps_Discards<-unique(Sum_No_reps_Discards)
Sum_No_reps<-subset(Sum_No_reps,Result!=-100000000)
Sum_No_reps<-tbl_df(Sum_No_reps)

#Remove sample days that have summable samples from the list of discarded samples to avoid double counting them
Sum_No_reps_Discards<-anti_join(Sum_No_reps_Discards,Sum_No_reps,by=c("Waterbody","WBID","AnalyteName","MatrixName","FractionName","SampleDate","BeneficialUse","ProjectName","Objective"))


Sum_No_reps<-as.data.table(Sum_No_reps)[,mean(Result),list(AnalyteName,BeneficialUse
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,CEDENName,Objective,AveragingPeroid
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
	names(Sum_No_reps)[names(Sum_No_reps)=='V1']<-"Result"
	Sum_No_reps<-tbl_df(Sum_No_reps)


## Add Results field together
SummedSummingPollutants<-as.data.table(Sum_No_reps)[,sum(Result),list(AnalyteName,BeneficialUse
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName,TargetLatitude,TargetLongitude
	,Waterbody,WBID,Wbtype,AveragingPeroid,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
	,Eval_Ref_Number)]
	names(SummedSummingPollutants)[names(SummedSummingPollutants)=='V1']<-"Result"
	SummedSummingPollutants<-tbl_df(SummedSummingPollutants)


#Split out PCBs and Aroclors from summed summing pollutants to calculate the max 
#sample day result for assessment
PCBs<-SummedSummingPollutants[which((SummedSummingPollutants$AnalyteName=="Aroclor"
	|SummedSummingPollutants$AnalyteName=="PCB, total Congeners")|SummedSummingPollutants$AnalyteName=="PCBs (Polychlorinated biphenyls)"
	&(SummedSummingPollutants$BeneficialUse=="MU"|SummedSummingPollutants$BeneficialUse=="CM"|SummedSummingPollutants$BeneficialUse=="SH")),]

PCBs$PCBName<-"PCBs (Polychlorinated biphenyls)"

#Calculate the max result for each station sample day Aroclors or PCBs
MaxPCBs<-as.data.table(PCBs)[,max(Result),list(PCBName,BeneficialUse,UnitName,StationCode,ProjectName
	,SampleDate,MatrixName,FractionName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype
	,Objective,AveragingPeroid,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
	MaxPCBs<-tbl_df(MaxPCBs)
	names(MaxPCBs)[names(MaxPCBs)=='V1']<-"Result"
	names(MaxPCBs)[names(MaxPCBs)=='PCBName']<-"AnalyteName"

#Cut out CM and MU PCB data and then put the adjusted results back in
SummedSummingPollutants<-filter(SummedSummingPollutants,!((AnalyteName=="Aroclor"
	|AnalyteName=="PCB, total Congeners")&(BeneficialUse=="MU"|BeneficialUse=="CM"|SummedSummingPollutants$BeneficialUse=="SH")))
	SummedSummingPollutants<-rbind.data.frame(SummedSummingPollutants,MaxPCBs)



####Add back generic information for these fields that are no longer neeeded so that 
#####this table can be joined with the W_Objectives table that is about to be created
#######Adding these fields allows us to join the table back in before the station count
######and allows the data to go thoguht he quantitation check a second time without being 
#####modified.  Since the qunaittaiton check has already been run on this data, and since 
######there is no way of knowing what the RL, or MDL would be for the summed
####pollutants, the quantitation check does not need to be run a second time on this data.

SummedSummingPollutants$MDL<-0
SummedSummingPollutants$RL<-0
SummedSummingPollutants$QACode<-"None"
SummedSummingPollutants$ResQualCode<-"="


#Add pyrethroid data back into the summing table
SummedSummingPollutants<-rbind.data.frame(SummedSummingPollutants,AllPyrethroids)

#If region 3 add OPs back to summing pollutant table
if(Region==3){
  SummedSummingPollutants<-rbind.data.frame(SummedSummingPollutants,OPs)
}

################################End summing pollutant Calculations###########################################################################
#############################################################################################################################################
#############################################################################################################################################

#Add objectives to saline ammonia data
SalineAmmonia_W_Objectives<-tbl_df(merge(W_Waterbodies,SalinityAmmoniaObjectives))
SalineAmmonia_W_Objectives$Result[which(is.na(SalineAmmonia_W_Objectives$Result))]<-0


# Add Objectives table
FreshObjectives<-anti_join(W_Waterbodies,Ocean_Waters)
W_Objectives<-tbl_df(merge(FreshObjectives,Analytes,by=c("AnalyteName","BeneficialUse","UnitName")))
W_Objectives<-tbl_df(W_Objectives)
OceanObjectives<-tbl_df(merge(W_Waterbodies,Ocean_Waters))
OceanObjectives$FreshMarine<-NULL
OceanObjectives<-tbl_df(merge(OceanObjectives,Ocean_Analytes))
W_Objectives<-rbind.data.frame(W_Objectives,OceanObjectives)
Missing_Analytes<-anti_join(ExportedData,Analytes,by="AnalyteName")
Missing_Analytes<-anti_join(Missing_Analytes,Ocean_Waters)
Missing_Analytes<-anti_join(Missing_Analytes,SummingPollutants)
	SaltAmmoniaMissingObjectives<-anti_join(ExportedData,Fresh_Fresh)
	SaltAmmoniaMissingObjectives<-filter(SaltAmmoniaMissingObjectives,(Region=="5"|Region=="6"|Region=="7"))
Missing_Analytes<-rbind.data.frame(Missing_Analytes,SaltAmmoniaMissingObjectives)
#Nitrogen has objectives in R9 so filter out nitrogen data if the region is 9
if(Region=="9"){
Missing_Analytes<-filter(Missing_Analytes,!(AnalyteName=="Nitrogen, Total"|AnalyteName=="Ammonia as N"
                                |AnalyteName=="Nitrate + Nitrite as N"|AnalyteName=="Nitrate as N"
                                |AnalyteName=="Nitrite as N"|AnalyteName=="Nitrogen, Organic"
                                |AnalyteName=="Nitrogen, Total Kjeldahl"))
Missing_Analytes<-filter(Missing_Analytes,!(AnalyteName=="Phosphorus as P"|AnalyteName=="OrthoPhosphate as P"))
}
#
Missing_Analytes$Issue<-"Analyte missing objective"
Missing_Analytes<-anti_join(Missing_Analytes,TotalAmmoniaObjectives,by=c("ProjectName","AnalyteName","SampleDate","TargetLatitude","TargetLongitude","StationCode"))
#Generate table of data from ocean stations missing objective in ocean analytes table
#Generate table of data in ocean waters
MissingOceanObjectives<-tbl_df(merge(ExportedData,Ocean_Waters))
MissingOceanObjectives$FreshMarine<-NULL
#remove all rows of data from above table that have objective in ocean analyte table
MissingOceanObjectives<-anti_join(MissingOceanObjectives,Ocean_Analytes)
##Create list of summing pollutants with objective in ocean analyte table
#Copy summing pollutant table
OceanSummingPollutants<-SummingPollutants
#Rename fields to join with ocean anlyte table
OceanSummingPollutants$CEDENName<-OceanSummingPollutants$AnalyteName
OceanSummingPollutants$AnalyteName<-OceanSummingPollutants$SummingName
#Generate list of analyte names in the ocean analytes table
OceanAnalyteNames<-tbl_df(unique(Ocean_Analytes$AnalyteName))
names(OceanAnalyteNames)[names(OceanAnalyteNames)=="value"]<-"AnalyteName"
#Use above lsit to remove summing pollutants that do not have ocean objective
OceanSummingPollutants<-tbl_df(merge(OceanSummingPollutants,OceanAnalyteNames))
#Change names back to be joined with raw data remove unecessary fields
OceanSummingPollutants$SummingName<-OceanSummingPollutants$AnalyteName
OceanSummingPollutants$AnalyteName<-OceanSummingPollutants$CEDENName
OceanSummingPollutants$CEDENName<-NULL
#Remove rows of ocean data that have a summing pollutant objective
MissingOceanObjectives<-anti_join(MissingOceanObjectives,OceanSummingPollutants)
MissingOceanObjectives$Issue<-"Analyte missing objective in Ocean Water"
AllExportedData<-rbind.data.frame(AllExportedData,MissingOceanObjectives)
AllExportedData<-rbind.data.frame(AllExportedData,Missing_Analytes)

#Join Hardness adjusted objective rows to the main data table
W_Objectives<-rbind.data.frame(W_Objectives,Hardness_W_Objectives)

#Join Pentachlorophenol data to the main table
W_Objectives<-rbind.data.frame(W_Objectives,Penta_W_Objectives)

#Join Total Ammonia data to the main table
W_Objectives<-rbind.data.frame(W_Objectives,TotalAmmoniaObjectives)

#Join Saline Ammonia data to main table
W_Objectives<-rbind.data.frame(W_Objectives,SalineAmmonia_W_Objectives)

#Add Summed pollutants back into table
W_Objectives<-rbind.data.frame(W_Objectives,SummedSummingPollutants)

#Add total nitrogen back in
W_Objectives<-rbind.data.frame(W_Objectives,AllTotalNitrogen)

#Add phosphorus back in
W_Objectives<-rbind.data.frame(W_Objectives,AllPhosphorusCombined)


#Remove unecessary fields from summed pollutants
#then add the rows to the w_waterbodies table
#so that they can be included in the LOEs
SummedSummingPollutants$Objective<-NULL
SummedSummingPollutants$Objective_Language<-NULL
SummedSummingPollutants$Evaluation_Guideline<-NULL
SummedSummingPollutants$Objective_Ref_Number<-NULL
SummedSummingPollutants$Eval_Ref_Number<-NULL
SummedSummingPollutants$AveragingPeroid<-NULL

#Remove unecessary fields from total nitrogen
#then add the rows to the w_waterbodies table
#so that they can be included in the LOEs
AllTotalNitrogen$Objective<-NULL
AllTotalNitrogen$Objective_Language<-NULL
AllTotalNitrogen$Evaluation_Guideline<-NULL
AllTotalNitrogen$Objective_Ref_Number<-NULL
AllTotalNitrogen$Eval_Ref_Number<-NULL
AllTotalNitrogen$AveragingPeroid<-NULL

#Remove unecessary fields from total phosphorus
#then add the rows to the w_waterbodies table
#so that they can be included in the LOEs
AllPhosphorusCombined$Objective<-NULL
AllPhosphorusCombined$Objective_Language<-NULL
AllPhosphorusCombined$Evaluation_Guideline<-NULL
AllPhosphorusCombined$Objective_Ref_Number<-NULL
AllPhosphorusCombined$Eval_Ref_Number<-NULL
AllPhosphorusCombined$AveragingPeroid<-NULL


#Add summedsumming pollutants table to the W_Waterbodis table
#So that we can calculate station information
W_Waterbodies<-rbind.data.frame(W_Waterbodies,SummedSummingPollutants)
W_Waterbodies<-rbind.data.frame(W_Waterbodies,NeedsConvertingForStations)
W_Waterbodies<-rbind.data.frame(W_Waterbodies,AllTotalNitrogen)
W_Waterbodies<-rbind.data.frame(W_Waterbodies,AllPhosphorusCombined)

#Create a new table of station codes, waterbodies, analyte, and count of stations
#Stations<-subset(W_Waterbodies,select=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName","MatrixName","FractionName"))
#Stations<-distinct(select(Stations,AnalyteName,Waterbody,StationCode,WBID,ProjectName,MatrixName,FractionName))
#Stations<-Stations%>%group_by(AnalyteName,Waterbody,WBID,ProjectName,MatrixName,FractionName)%>%summarise(count=n(),
#StationCode=paste(StationCode,collapse=", "))
#names(Stations)<-c("AnalyteName","Waterbody","WBID","ProjectName","MatrixName","FractionName","StationCount","StationCode")


# Correct the results based on quantitation limits - I did this before removing replicates to avoid averaging samples that we assesed with a different method

W_Objectives$Result[which(W_Objectives$RL<=W_Objectives$Objective & (W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<-W_Objectives$MDL[which(W_Objectives$RL<=W_Objectives$Objective & (W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]*.5 #perfect scenario

W_Objectives$Result[which((W_Objectives$RL<=W_Objectives$Objective)&is.na(W_Objectives$MDL)&(W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<-0 #When sample can be quantified, but is missing the MDL

W_Objectives$Result[which((W_Objectives$RL>W_Objectives$Objective|is.na(W_Objectives$RL))&(W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<--100000000 #imperfect results in very negative number


#Create table of tossed samples and then combine them with the other tables of tossed samples due to quantitation limts
General_Tossed<-filter(W_Objectives,(Result==-100000000))

#Normalize the total ammonia objectives for quantitation discards to prevent issues with the join later
General_Tossed$Objective[which(General_Tossed$AnalyteName=="Ammonia as N")]<-1

#Add hardness tossed data in to General Tossed
General_Tossed<-rbind.data.frame(General_Tossed,Hardness_Tossed)

#Add toxic unit pyrethroid tossed data
General_Tossed<-rbind.data.frame(General_Tossed,AllPyrethroidDiscards)

#Add summed individual pyrethroid tossed data
General_Tossed<-rbind.data.frame(General_Tossed,PyrethroidDiscards)

#Add OP discards into General Tossed
General_Tossed<-rbind.data.frame(General_Tossed,OP_Discards)

#Add Summing Discards to General Tossed
General_Tossed<-rbind.data.frame(General_Tossed,Sum_No_reps_Discards)

#Add total nitrogen discards to general tossed
General_Tossed<-rbind.data.frame(General_Tossed,NitrogenDiscards)

#Add phosphorus discards to general tossed
General_Tossed<-rbind.data.frame(General_Tossed,PhosphorusDiscards)


#Calculate maximum and minimum date for each waterbody with data
General_Tossed$SampleDate<-as.Date(General_Tossed$SampleDate, "%m/%d/%Y")
Tossed_Max_Date<-as.data.table(General_Tossed)[,max(SampleDate),list(AnalyteName,StationCode
	,BeneficialUse,UnitName,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number,Objective)]
names(Tossed_Max_Date)[names(Tossed_Max_Date)=='V1']<-"MaxDate"
Tossed_Max_Date<-tbl_df(Tossed_Max_Date)
Tossed_Min_Date<-as.data.table(General_Tossed)[,min(SampleDate),list(AnalyteName,StationCode
	,BeneficialUse,UnitName,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number,Objective)]
names(Tossed_Min_Date)[names(Tossed_Min_Date)=='V1']<-"MinDate"
Tossed_Min_Date<-tbl_df(Tossed_Min_Date)
Tossed_Date_Range<-tbl_df(merge(Tossed_Max_Date,Tossed_Min_Date,by=c("AnalyteName","StationCode"
	,"Waterbody","WBID","ProjectName","MatrixName","FractionName","BeneficialUse","UnitName"
	,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number","Objective")))
General_Tossed<-tbl_df(merge(General_Tossed,Tossed_Date_Range))


#Convert General_Tossed into a list of samples thrown out with language specifying they were thrown out due to quantitation issues
Quantitation_Discards<-distinct(select(General_Tossed,AnalyteName,Waterbody,WBID,MatrixName,StationCode
	,FractionName,ProjectName,BeneficialUse,Objective_Language,Evaluation_Guideline
	,Objective_Ref_Number,Eval_Ref_Number,MaxDate,MinDate,SampleDate))
Quantitation_Discards<-Quantitation_Discards%>%dplyr::group_by(AnalyteName,Waterbody,WBID
	,ProjectName,MatrixName,StationCode,FractionName,BeneficialUse
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number
	,MaxDate,MinDate)%>%dplyr::summarise(count=n())
names(Quantitation_Discards)<-c("AnalyteName","Waterbody","WBID","ProjectName"
	,"MatrixName","StationCode","FractionName","BeneficialUse","Objective_Language"
	,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"
	,"MaxDate","MinDate","TossedSampleCount")


W_Objectives<-filter(W_Objectives,(Result!=-100000000))


W_Objectives<-as.data.table(W_Objectives)[,mean(Result),list(AnalyteName,BeneficialUse
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,Objective,AveragingPeroid
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
	names(W_Objectives)[names(W_Objectives)=='V1']<-"Result"
	W_Objectives<-tbl_df(W_Objectives)




#Create table of rows with 1 hour averaging peroid
CMC_Values<-W_Objectives[W_Objectives$AveragingPeroid==1,]
CMC_Values<-filter(CMC_Values,!(CMC_Values$AnalyteName=="Oxygen, Dissolved"|CMC_Values$AnalyteName=="Oxygen, Saturation"))


##################################################################################################
####################################Start 1 day MAX###############################################
##################################################################################################
CMC_Values$SampleDate2<-ymd(CMC_Values$SampleDate)

#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
CMC_Values <- CMC_Values %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName
	,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 1)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = max(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName
 ,FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
 ,Eval_Ref_Number,Result)
  

##############################End 1 Day Max########################################################
###################################################################################################
###################################################################################################


#Split out DO 1 day minimum from W_Objectives
DO_Data<-W_Objectives[which((W_Objectives$AnalyteName=="Oxygen, Dissolved"|W_Objectives$AnalyteName=="Oxygen, Saturation")&W_Objectives$AveragingPeroid=="1"),]

#Split out DO 7 day max from W_Objectives
DO_Data_Max<-W_Objectives[which((W_Objectives$AnalyteName=="Oxygen, Dissolved"|W_Objectives$AnalyteName=="Oxygen, Saturation")&W_Objectives$AveragingPeroid=="7"),]


#Remove DO Data from W_Objectives
W_Objectives<-filter(W_Objectives,!(W_Objectives$AnalyteName=="Oxygen, Dissolved"|W_Objectives$AnalyteName=="Oxygen, Saturation"))


#Remove the 1 hour averaging peroid data form the main table
W_Objectives<-filter(W_Objectives,(AveragingPeroid!=1))


##################################################################################################
####################################Start 1 day MIN###############################################
##################################################################################################
#DO_Data$SampleDate2<-ymd(DO_Data$SampleDate)

#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
DO_Data <- DO_Data %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName
	,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 1)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = min(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName
 ,FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
 ,Eval_Ref_Number,Result)
  

##############################End 1 Day Min########################################################
###################################################################################################
###################################################################################################


##################################################################################################
####################################Start 7 day MAX###############################################
##################################################################################################
#DO_Data$SampleDate2<-ymd(DO_Data$SampleDate)

#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
DO_Data_Max <- DO_Data_Max %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName
           ,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 7)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = max(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName
         ,FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
         ,Eval_Ref_Number,Result)


##############################End 7 Day MAX########################################################
###################################################################################################
###################################################################################################



No_reps<-as.data.table(W_Objectives)[,mean(Result),list(AnalyteName,BeneficialUse,UnitName
	,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,Waterbody,WBID,Wbtype,Objective,AveragingPeroid,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
	,Eval_Ref_Number)]
names(No_reps)[names(No_reps)=='V1']<-"Result"
W_Objectives<-tbl_df(No_reps)


###############_____________________Begin Second Total Ammonia Criteria Calculation______###########

#Split out total ammonia so that the objective can be calculated
TotalAmmoniaObjectives2<-W_Objectives[W_Objectives$AnalyteName=="Ammonia as N"&(W_Objectives$BeneficialUse=="CO"|W_Objectives$BeneficialUse=="WA"),]


#Remove objective fields from the averaged ammonia data
TotalAmmoniaObjectives2$Evaluation_Guideline<-NULL
TotalAmmoniaObjectives2$Eval_Ref_Number<-NULL
TotalAmmoniaObjectives2$Objective_Language<-NULL
TotalAmmoniaObjectives2$Objective_Ref_Number<-NULL


#Average the pH and Temp data outside of objective application (this is less imporant with pH but temperature may not have any objective for CO)
PentapH2<-as.data.table(PentapH)[,mean(pHResult),list(BeneficialUse,StationCode,ProjectName,SampleDate,MatrixName,Waterbody,WBID,Wbtype)]
names(PentapH2)[names(PentapH2)=='V1']<-"pHResult"
PentapH2<-tbl_df(PentapH2)

#Average the pH and Temp data outside of objective application (this is less imporant with pH but temperature may not have any objective for CO)
Temp2<-as.data.table(temp)[,mean(TempResult),list(BeneficialUse,StationCode,ProjectName,SampleDate,MatrixName,Waterbody,WBID,Wbtype)]
names(Temp2)[names(Temp2)=='V1']<-"TempResult"
Temp2<-tbl_df(Temp2)

#Merge ammonia data with pH and Temperature.
TotalAmmoniaObjectives2<-tbl_df(merge(TotalAmmoniaObjectives2,PentapH2,by=c("StationCode"
	,"ProjectName","SampleDate","Waterbody","WBID"
	,"Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaObjectives2<-tbl_df(merge(TotalAmmoniaObjectives2,Temp2,by=c("StationCode"
	,"ProjectName","SampleDate","Waterbody","WBID"
	,"Wbtype","BeneficialUse","MatrixName")))

#Calculate objectives based on formula in EPA 2013
TotalAmmoniaObjectives2<-mutate(TotalAmmoniaObjectives2,Objective=1000*.8876*((.0278/(1+10^(7.688-pHResult)))+(1.1994/(1+10^(pHResult-7.688))))*(2.126*10^(.028*(20-TempResult))))

#Remove unecessary Columns
TotalAmmoniaObjectives2$TempResult<-NULL
TotalAmmoniaObjectives2$pHResult<-NULL
TotalAmmoniaObjectives$AveragingPeroid<-30

#Create a table that contains all the fields for the ammonia information
#that contains fake placeholder information to avoid getting errors when there
#is not ammonia data in the data set being assessed
rownames<-c("StationCode","ProjectName","SampleDate"
	,"Waterbody","WBID","Wbtype","BeneficialUse","MatrixName","AnalyteName","UnitName"
	,"FractionName","Objective","Result")
fakeinformation<-c("placeholder","Placeholder","1850-01-01","Placeholder"
	,"Placeholder","Placeholer","Placeholder","na/L","Placeholder","NA","NA","1","0")
placeholderinfo<-as.data.frame(rbind(rownames,fakeinformation))
colnames(placeholderinfo)<-rownames
row.names(placeholderinfo)<-NULL
placeholderinfo<-placeholderinfo[-1,]
placeholderinfo<-tbl_df(placeholderinfo)
placeholderinfo$Result<-as.numeric(placeholderinfo$Result)
placeholderinfo$Objective<-as.numeric(placeholderinfo$Objective)
placeholderinfo$Result<-0
placeholderinfo$AveragingPeroid<-0


TotalAmmoniaObjectives2<-tbl_df(rbind.data.frame(TotalAmmoniaObjectives2,placeholderinfo))



#Add objective language, and reference codes
TotalAmmoniaObjectives2$Evaluation_Guideline<-""
TotalAmmoniaObjectives2$Eval_Ref_Number<-""
TotalAmmoniaObjectives2$Objective_Language<-paste0("The Total Ammonia criterion continuous concentration (expressed as a 30-day average) to protect aquatic life in freshwater is Temperature and pH dependent, and was calculated according to the formula listed in the Aquatic Life Ambient Water Quality Criteria for Ammonia - Freshwater 2013 document.")
TotalAmmoniaObjectives2$Objective_Ref_Number<-"4107"


#Remove the rows of data from main data frame where the objectives were just calculated
W_Objectives<-filter(W_Objectives, !(AnalyteName=="Ammonia as N" & (BeneficialUse=="CO"|BeneficialUse=="WA")))


################_______________End Second Total Ammonia Objective Calc______######################################




##############################################################################################################

########SPLIT OUT POLLUTATNS WITH AVERAGING PEROIDS OTHER THAN 7 DAYS HERE####################################

##############################################################################################################

#Create table of 4 day avering pollutants
FourDayAverage<-W_Objectives[which(W_Objectives$AveragingPeroid==4),]

#Make table of 30-day averaging peroid data
ThirtyDayAveraging<-tbl_df(W_Objectives[which(W_Objectives$AveragingPeroid==30),])
ThirtyDayAveraging<-rbind.data.frame(ThirtyDayAveraging,TotalAmmoniaObjectives2)

#Make table of 6-month averaging peroid data
SixMonthMedian<-tbl_df(W_Objectives[which(W_Objectives$AveragingPeroid==183),])

#Make table for 10 day averaging (currently just for Microcycstin and Cylindrospermopsin)
TenDayAveraging<-tbl_df(W_Objectives[which(W_Objectives$AveragingPeroid==10),])

#Filter for most common, 7-day averaging peroid data
W_Objectives<-W_Objectives[which(W_Objectives$AveragingPeroid==7),]


##############################################################################################################

###########################Begin 7 Day Averaging peroid#######################################################

##############################################################################################################

W_Objectives$SampleDate<-ymd(W_Objectives$SampleDate)


#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
W_Objectives2 <- W_Objectives %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName,Waterbody
	,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 7)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = mean(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName, FractionName
	,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number,Result)
  

###################################End Seven Day Average##################################

###########################################################################################
####################################Start 4 day average#################################
######################################################################################
FourDayAverage$SampleDate2<-ymd(FourDayAverage$SampleDate)

#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
FourDayAverage2 <- FourDayAverage %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName
	,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 4)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = mean(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName
 ,FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
 ,Eval_Ref_Number,Result)
  



##############################End Four Day Agerage##################################
###################################################################################
#############################Start 30 day average####################################

#This averaging period code was developed by Owen Liu (oliu@ucsb.edu) and Heili Lowman
#(heilil@sccwrp.org) to resolve the averaging period issue that came up when samples
#were collected 4 weeks apart. The previous code would average all samples collected 
#within 28 days of each other rather than average samples collected within a 28 day 
#window.

ThirtyDayAveraging$SampleDate<-ymd(ThirtyDayAveraging$SampleDate)
ThirtyDayAveraging<-ThirtyDayAveraging%>%
  group_by(AnalyteName,BeneficialUse,UnitName,StationCode
           ,ProjectName, MatrixName,FractionName,Waterbody
           ,WBID,Objective_Language,Evaluation_Guideline
           ,Objective_Ref_Number,Eval_Ref_Number)%>%
  mutate(counter=cur_group_id())%>%
  ungroup()

# owen's function for date aggregation
# function takes a current date, and a vector of dates to match
# and finds any dates within the next four days
calc_30days <- function(d,date_vec){
  diffs <- time_length(d %--% date_vec,"days")
  date_vec[diffs<=30]
}


# create a new function for application in the map() portion, using Owen's working code.
# the only input necessary is a dataframe (LOE_data)
# note this function spits out a full dataframe, hence why we need to use the map_dfr() function below
create_date_groups <- function(LOE_data){
  groups_key <- tibble()
  current_p <- 1
  current_d <- LOE_data$SampleDate[1]
  current_date_vec <- sort(LOE_data$SampleDate)
  for(i in seq_along(LOE_data$SampleDate)){
    #if we've already assigned all the dates, stop
    if(length(current_date_vec)==0) break
    # otherwise build a group of dates
    grp <- calc_30days(current_d,current_date_vec)
    # add the group of dates to our key
    groups_key <- bind_rows(groups_key,tibble(SampleDate=grp,group=current_p))
    # remove dates in the group from the pool of possible dates
    current_date_vec <- setdiff(current_date_vec,grp) %>% as_date()
    # iterate our counters
    current_p <- current_p + 1
    current_d <- current_date_vec[1]
  }
  # add data onto final dataset
  LOE_data %>% 
    left_join(groups_key)
}

# use the function create_date_groups
ThirtyDayAveraging2 <- ThirtyDayAveraging%>% # Take the original dataframe and then...
  group_split(.$counter)%>% # Splits by "unique" datasets within the larger dataset (using the counter column) and then...
  map_dfr(create_date_groups) # Map our function create_date_groups onto the Dummy2 dataset that's been split by counter.


ThirtyDayAveraging2<-ThirtyDayAveraging2%>%group_by(AnalyteName,BeneficialUse,UnitName,StationCode
                                                    ,ProjectName, MatrixName,FractionName,Waterbody
                                                    ,WBID,Objective_Language,Evaluation_Guideline
                                                    ,Objective_Ref_Number,Eval_Ref_Number,counter,group)%>%
  summarize(Result=mean(Result),SampleDate=max(SampleDate),Objective=mean(Objective))

ThirtyDayAveraging2$counter<-NULL
ThirtyDayAveraging2$group<-NULL

###########################End 30 Day Averaging##############################################

###########################################################################################
#############################Start 6 Month median##########################################

SixMonthMedian$SampleDate<-ymd(SixMonthMedian$SampleDate)


#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
SixMonthMedian2 <- SixMonthMedian %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName 
           ,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
           ,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 183)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective = mean(Objective), Result = median(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName 
         , FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline
         ,Objective_Ref_Number,Eval_Ref_Number,Result)

###########################End 6 Month Median##############################################
###########################################################################################

############################################################################################
#############################Start 10 Day Averaging#########################################

TenDayAveraging$SampleDate<-ymd(TenDayAveraging$SampleDate)


#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
TenDayAveraging2 <- TenDayAveraging %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName 
                  ,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
                  ,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 10)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective = mean(Objective), Result = median(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName 
         , FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline
         ,Objective_Ref_Number,Eval_Ref_Number,Result)

###########################End 10 Day Averaging############################################
###########################################################################################


#Combine all averaged data back together into a single data frame, one data frame at a time
#in case one of the data frames does not have any data
No_reps_W_Objectives<-tbl_df(W_Objectives2)
No_reps_W_Objectives<-tbl_df(rbind.data.frame(No_reps_W_Objectives,FourDayAverage2))
No_reps_W_Objectives<-tbl_df(rbind.data.frame(No_reps_W_Objectives,ThirtyDayAveraging2))
No_reps_W_Objectives<-tbl_df(rbind.data.frame(No_reps_W_Objectives,SixMonthMedian2))
No_reps_W_Objectives<-tbl_df(rbind.data.frame(No_reps_W_Objectives,CMC_Values))
No_reps_W_Objectives<-tbl_df(rbind.data.frame(No_reps_W_Objectives,DO_Data))
No_reps_W_Objectives<-tbl_df(rbind.data.frame(No_reps_W_Objectives,TenDayAveraging2))


No_reps_W_Objectives$N<-NULL
No_reps_W_Objectives$SampleDate<-as.Date(No_reps_W_Objectives$SampleDate, "%m/%d/%Y")

No_reps<-No_reps_W_Objectives


#Change matrix value from SampleWater to just Water
levels(No_reps$MatrixName)<-c(levels(No_reps$MatrixName), "Water")
No_reps$MatrixName[No_reps$MatrixName=='samplewater']<-'Water'


#Convert No_reps back into W_Objectives
W_Objectives<-tbl_df(No_reps_W_Objectives)

#Get list of samples to calculate the date range of data
W_Objectives_for_date_Range<-W_Objectives

#Add the quantitation discarded samples into the date range table so that we can correctly
#capture the date range if quantitation discards occur at beginning or end of sample peroid
Tossed_Samples_W_Good_Samples<-Tossed_Date_Range
Tossed_Samples_W_Good_Samples<-subset(Tossed_Samples_W_Good_Samples,select=c(Waterbody,WBID,AnalyteName,BeneficialUse,MatrixName,
                         StationCode,ProjectName,FractionName,Objective_Language,Evaluation_Guideline,
                         Eval_Ref_Number,Objective_Ref_Number))
Tossed_Samples_W_Good_Samples<-tbl_df(merge(Tossed_Samples_W_Good_Samples,W_Objectives_for_date_Range))
Tossed_Samples_W_Good_Samples<-subset(Tossed_Samples_W_Good_Samples,select=c(Waterbody,WBID,AnalyteName,BeneficialUse,MatrixName,
                         StationCode,ProjectName,FractionName,Objective_Language,Evaluation_Guideline,
                         Eval_Ref_Number,Objective_Ref_Number))
Tossed_Samples_W_Good_Samples<-unique(Tossed_Samples_W_Good_Samples)
Tossed_Samples_W_Good_Samples<-tbl_df(merge(Tossed_Samples_W_Good_Samples,Tossed_Date_Range))
Tossed_Samples_W_Good_Samples_Max<-Tossed_Samples_W_Good_Samples
Tossed_Samples_W_Good_Samples_Max$SampleDate<-Tossed_Samples_W_Good_Samples_Max$MaxDate
Tossed_Samples_W_Good_Samples_Max$MaxDate<-NULL
Tossed_Samples_W_Good_Samples_Max$MinDate<-NULL
Tossed_Samples_W_Good_Samples_Min<-Tossed_Samples_W_Good_Samples
Tossed_Samples_W_Good_Samples_Min$SampleDate<-Tossed_Samples_W_Good_Samples_Min$MinDate
Tossed_Samples_W_Good_Samples_Min$MaxDate<-NULL
Tossed_Samples_W_Good_Samples_Min$MinDate<-NULL
Tossed_Samples_W_Good_Samples<-rbind.data.frame(Tossed_Samples_W_Good_Samples_Max,Tossed_Samples_W_Good_Samples_Min)
Tossed_Samples_W_Good_Samples$Result<-0
W_Objectives_for_date_Range<-rbind.data.frame(W_Objectives_for_date_Range,Tossed_Samples_W_Good_Samples)



#Export processed data for visualization
VisualizationDataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"For_Visualization_",FileNameInQuotations)
if(Write_Data_Viz_Table=="Yes"){
write.table(W_Objectives,VisualizationDataFileName,sep="\t",row.names=FALSE)
}

####################################################################################################################
##########################################Dissolved Oxygen Floor exceedance count#########################################
#Split out DO Data and then calculate samples and exceedances
DOData<-W_Objectives[W_Objectives$AnalyteName=="Oxygen, Dissolved"|W_Objectives$AnalyteName=="Oxygen, Saturation"|W_Objectives$AnalyteName=="Alkalinity as CaCO3",]

DOData$Objective<-as.numeric(DOData$Objective)
DOOver<-as.data.table(DOData)[,sum(Result<Objective),list(AnalyteName,StationCode,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(DOOver)[names(DOOver)=='V1']<-"Exceedances"
DOOver<-tbl_df(DOOver)

W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
DOTotal<-as.data.table(DOData)[,sum(Result>=0),list(AnalyteName,StationCode,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(DOTotal)[names(DOTotal)=='V1']<-"Total"
DOTotal<-tbl_df(DOTotal)

# Combine the two toegether
DOresults<-merge(DOOver,DOTotal,by=c("AnalyteName","StationCode","BeneficialUse","ProjectName","MatrixName"
,"FractionName","Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
DOresults<-tbl_df(DOresults)

####################################################################################################################
##########################################Dissolved Oxygen Ceiling exceedance count#########################################

#Use older DO ceiling DO_Data_Max that was not combined with the other data frames
#This is DO data with a 7 day max value
DO_Data_Max$Objective<-as.numeric(DO_Data_Max$Objective)
DO_Over_Max<-as.data.table(DO_Data_Max)[,sum(Result>Objective),list(AnalyteName,StationCode,BeneficialUse,ProjectName,MatrixName,FractionName
                                                          ,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(DO_Over_Max)[names(DO_Over_Max)=='V1']<-"Exceedances"
DO_Over_Max<-tbl_df(DO_Over_Max)

DO_Data_Max$Objective<-as.numeric(DO_Data_Max$Objective)
DO_Total_Max<-as.data.table(DO_Data_Max)[,sum(Result>=0),list(AnalyteName,StationCode,BeneficialUse,ProjectName,MatrixName,FractionName
                                                    ,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(DO_Total_Max)[names(DO_Total_Max)=='V1']<-"Total"
DO_Total_Max<-tbl_df(DO_Total_Max)

# Combine the two toegether
DO_results_Max<-merge(DO_Over_Max,DO_Total_Max,by=c("AnalyteName","StationCode","BeneficialUse","ProjectName","MatrixName"
                                     ,"FractionName","Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
DO_results_Max<-tbl_df(DO_results_Max)


###################################################################################################################################
###################################################################################################################################
#################################_______________Begin pH Sample Count____________________########################################

####Split out pH for pH sample count and exceedances
pHdata<-W_Objectives[W_Objectives$AnalyteName=="pH",]
pHdata$Objective<-NULL

#Add alias back to pH data
# Load Region objecives table and convert to tbl_df
Analytes<-read.delim(ObjectivesTable,header=TRUE,stringsAsFactors=FALSE)
Analytes<-tbl_df(Analytes)
Analytes<-subset(Analytes,select=c(AnalyteName,UnitName,BeneficialUse,Alias,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number))
Analytes$Objective_Ref_Number<-as.character(Analytes$Objective_Ref_Number)
Analytes$Eval_Ref_Number<-as.character(Analytes$Eval_Ref_Number)
Analytes<-tbl_df(Analytes)

#Add objective and alias back to data using Analyte, BU, Unit, and objective text to join back on
pHdata<-tbl_df(merge(pHdata,Analytes,by=c("AnalyteName","BeneficialUse","UnitName"
,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))

#Determine if there was an exceedance for each day by including SampleDate in the counting feature
#so that each day will have a max value of one

pHdata$Objective<-as.numeric(pHdata$Objective)
pHdataOver<-pHdata[pHdata$Alias=="High",]
pHdataOver<-as.data.table(pHdataOver)[,sum(Result>Objective),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName,StationCode
,Waterbody,WBID,SampleDate,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataOver)[names(pHdataOver)=='V1']<-"Exceedances"
pHdataOver<-tbl_df(pHdataOver)

pHdata$Objective<-as.numeric(pHdata$Objective)
pHdataUnder<-pHdata[pHdata$Alias=="Low",]
pHdataUnder<-as.data.table(pHdataUnder)[,sum(Result<Objective),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName,StationCode
,Waterbody,WBID,SampleDate,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataUnder)[names(pHdataUnder)=='V1']<-"Exceedances"
pHdataUnder<-tbl_df(pHdataUnder)


#Combine the two tables together with rbind then coutn weather or not there was an exceedance
pHdataExceedances<-rbind.data.frame(pHdataOver,pHdataUnder)

#Take max value. Each station day will be either a zero or a one
pHdataExceedances<-as.data.table(pHdataExceedances)[,max(Exceedances),list(AnalyteName,BeneficialUse
,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,SampleDate,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataExceedances)[names(pHdataExceedances)=='V1']<-"Exceedances"
pHdataExceedances<-tbl_df(pHdataExceedances)


#Then finally count up exceedances for the waterbody by removing sample date
pHdataAllExceedances<-as.data.table(pHdataExceedances)[,sum(Exceedances),list(AnalyteName,BeneficialUse
,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataAllExceedances)[names(pHdataAllExceedances)=='V1']<-"Exceedances"
pHdataAllExceedances<-tbl_df(pHdataAllExceedances)

#Count up the number of pH samples for each waterbody independent of the objective

pHdata$Objective<-as.numeric(pHdata$Objective)
pHdataTotal<-pHdata[pHdata$Alias=="Low",]
pHdataTotal<-as.data.table(pHdataTotal)[,sum(Result>=0),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName,StationCode
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataTotal)[names(pHdataTotal)=='V1']<-"Total"
pHdataTotal<-tbl_df(pHdataTotal)


#Combine samples and exceedances together for pH data
pHResults<-tbl_df(merge(pHdataTotal,pHdataAllExceedances,by=c("AnalyteName","StationCode","BeneficialUse","ProjectName"
,"MatrixName","FractionName","Waterbody","WBID","Objective_Language","Evaluation_Guideline"
,"Objective_Ref_Number","Eval_Ref_Number")))

#Change Sample number to zero in Region 6 waterbodies
pHResults$Exceedances[which(substr(pHResults$WBID,4,4)=="6")]<-0



##########################################################################################################################################
######################################____End pH sample count_______####################################################################
###########################################################################################################################################


###############################################################################################################################################
################################___________________Begin Chromium assessment______________________________#####################################
###############################################################################################################################################

ChromiumData<-W_Objectives[which(W_Objectives$AnalyteName=="Chromium"&(W_Objectives$BeneficialUse=="CO"|W_Objectives$BeneficialUse=="WA")),]

#Add Chromim VI to this data
ChromiumData$VIObjective<-11
CleanChromium<-as.data.table(ChromiumData)[,sum((Result<Objective)&(Result<VIObjective)),list(AnalyteName
,BeneficialUse,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(CleanChromium)[names(CleanChromium)=="V1"]<-"Total"
CleanChromium$Exceedances<-0
Chromiumresults<-tbl_df(CleanChromium)

TossedChromium<-as.data.table(ChromiumData)[,sum((Result>Objective)&(Result>VIObjective)),list(AnalyteName
,BeneficialUse,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
TossedChromium<-tbl_df(TossedChromium)

TossedChromium2<-as.data.table(ChromiumData)[,sum((Result<Objective)&(Result>VIObjective)),list(AnalyteName
,BeneficialUse,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
TossedChromium2<-tbl_df(TossedChromium2)

TossedChromium3<-rbind(TossedChromium,TossedChromium2)

TossedChromium4<-as.data.table(TossedChromium3)[,sum(V1),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName,StationCode,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
TossedChromium<-tbl_df(TossedChromium4)
names(TossedChromium)[names(TossedChromium)=="V1"]<-"Samples"


#Remove chromium data from main table
W_Objectives<-filter(W_Objectives, !(AnalyteName=="Chromium"&(BeneficialUse=="CO"|BeneficialUse=="WA")))


###############################______________End_Chromium_Assessment____________#############################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################



#Remove DO and pH data from main data frame
W_Objectives<-filter(W_Objectives, !(AnalyteName=="Oxygen, Dissolved" |AnalyteName=="Oxygen, Saturation"|AnalyteName=="pH"|AnalyteName=="Alkalinity as CaCO3"))



#############################################################################################################################################
#############################################################################################################################################
##################_______________________Start Exceedance Count and LOE Writing Processes_______________________#############################


W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
Over<-as.data.table(W_Objectives)[,sum(Result>Objective),list(AnalyteName,BeneficialUse,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Over)[names(Over)=='V1']<-"Exceedances"
Over<-tbl_df(Over)

W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
Total<-as.data.table(W_Objectives)[,sum(Result>=0),list(AnalyteName,BeneficialUse,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Total)[names(Total)=='V1']<-"Total"
Total<-tbl_df(Total)


# Combine the two toegether
results<-merge(Over,Total,by=c("AnalyteName","BeneficialUse","ProjectName","MatrixName","FractionName","StationCode"
,"Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
results<-tbl_df(results)

#add the DO, DO_Max, Chromiu, and pH samples and Exceedances together with rbind.data.frame
#once at a time in case one of the data frames is empty
MoreResults<-results
MoreResults<-rbind.data.frame(MoreResults,DOresults)
MoreResults<-rbind.data.frame(MoreResults,DO_results_Max)
MoreResults<-rbind.data.frame(MoreResults,pHResults)
MoreResults<-rbind.data.frame(MoreResults,Chromiumresults)


W_Objectives_for_date_Range$SampleDate<-as.Date(W_Objectives_for_date_Range$SampleDate, "%m/%d/%Y")
Max_Date<-as.data.table(W_Objectives_for_date_Range)[,max(SampleDate),list(AnalyteName,BeneficialUse,StationCode
,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
names(Max_Date)[names(Max_Date)=='V1']<-"MaxDate"
Max_Date<-tbl_df(Max_Date)
Min_Date<-as.data.table(W_Objectives_for_date_Range)[,min(SampleDate),list(AnalyteName,BeneficialUse,StationCode
,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
names(Min_Date)[names(Min_Date)=='V1']<-"MinDate"
Min_Date<-tbl_df(Min_Date)
Date_Range<-tbl_df(merge(Max_Date,Min_Date,by=c("AnalyteName","Waterbody","WBID","ProjectName"
,"MatrixName","FractionName","BeneficialUse","StationCode"
,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))


# Combine results with max a min date range
R_W_Dates<-merge(MoreResults,Date_Range,by=c("Waterbody","WBID","AnalyteName","FractionName","StationCode"
,"MatrixName","BeneficialUse","ProjectName","Objective_Language"
,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
R_W_Dates<-tbl_df(R_W_Dates)



#Create table of LOEs where all data was thrown out from quantitation issues then add necessary fields to write LOEs
ZeroOfZero<-anti_join(Quantitation_Discards,R_W_Dates,by=c("Waterbody","WBID","AnalyteName","StationCode"
,"FractionName","MatrixName","BeneficialUse","ProjectName","Objective_Language","Evaluation_Guideline"
,"Objective_Ref_Number","Eval_Ref_Number"))
ZeroOfZero$Total<-0
ZeroOfZero$Total<-as.integer(ZeroOfZero$Total)
ZeroOfZero$Exceedances<-0
ZeroOfZero$Exceedances<-as.integer(ZeroOfZero$Exceedances)

#Add temporal rep field to the zero of zero LOEs
ZeroOfZero$TEMPORAL_REP<-paste0("Date for this waterbody was collected over the date range ", ZeroOfZero$MinDate, " to ",ZeroOfZero$MaxDate)

#Add station information to the zero of zeros
#ZeroOfZero<-tbl_df(merge(ZeroOfZero,Stations))
ZeroOfZero$StationCount<-1

#Add the station information to the field SpatialRep with some generic text
ZeroOfZero$SPATIAL_REP<-paste0("The samples were collected at ",ZeroOfZero$StationCount," monitoring site (",ZeroOfZero$StationCode,").")
ZeroOfZero<-tbl_df(ZeroOfZero)

ZeroOfZero<-tbl_df(mutate(ZeroOfZero,TotalSamples=Total+TossedSampleCount))

#Keep this comman on a single line to avoid LOE export issues
ZeroOfZero$DATA_USED<-paste0("Water Board staff assessed ",ZeroOfZero$ProjectName," data for ",ZeroOfZero$Waterbody," to determine beneficial use support, and the results are as follows: ",ZeroOfZero$Exceedances, " of the ",ZeroOfZero$Total," samples exceeded the evaluation guideline for ",ZeroOfZero$AnalyteName,". Although a total of ",ZeroOfZero$TotalSamples," samples were collected, ", ZeroOfZero$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the water quality threshold and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")


#Add the information from the tossed samples but remove max and min dates first
Quantitation_Discards_No_Range<-Quantitation_Discards
Quantitation_Discards_No_Range$MaxDate<-NULL
Quantitation_Discards_No_Range$MinDate<-NULL
Quantitation_Discards$Objective_Ref_Number<-as.character(Quantitation_Discards$Objective_Ref_Number)



Results_W_Date_Range_QD<-tbl_df(merge(R_W_Dates,Quantitation_Discards_No_Range,by=c("AnalyteName","StationCode"
,"Waterbody","WBID","ProjectName","MatrixName","FractionName","BeneficialUse"
,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))

#Results_W_Stations_Date_Range_QD<-tbl_df(merge(Results_W_Date_Range_QD,Stations))
Results_W_Stations_Date_Range_QD<-Results_W_Date_Range_QD
Results_W_Stations_Date_Range_QD$StationCount<-1

#Add TotalSample Field
Results_W_Stations_Date_Range_QD<-tbl_df(mutate(Results_W_Stations_Date_Range_QD,TotalSamples=Total+TossedSampleCount))

	#Add the Data used field using the quantitation discarded samples
	#Keep this command on ONE line or it will cause issues with the LOE export
Results_W_Stations_Date_Range_QD$DATA_USED<-paste0("Water Board staff assessed ",Results_W_Stations_Date_Range_QD$ProjectName," data for ",Results_W_Stations_Date_Range_QD$Waterbody," to determine beneficial use support and the results are as follows: ",Results_W_Stations_Date_Range_QD$Exceedances, " of the ",Results_W_Stations_Date_Range_QD$Total," samples exceeded the water quality threshold for ",Results_W_Stations_Date_Range_QD$AnalyteName,". Although a total of ",Results_W_Stations_Date_Range_QD$TotalSamples," samples were collected, ", Results_W_Stations_Date_Range_QD$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the water quality threshold and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")
	#Add Station information to the table to creat Spatial Rep field
Results_W_Stations_Date_Range_QD$SPATIAL_REP<-paste0("The samples were collected at ",Results_W_Stations_Date_Range_QD$StationCount," monitoring site (",Results_W_Stations_Date_Range_QD$StationCode,").")
	#Add temporal rep to table so you can join with zero of zero table
Results_W_Stations_Date_Range_QD$TEMPORAL_REP<-paste0("Data for this waterbody was collected over the date range ", Results_W_Stations_Date_Range_QD$MinDate, " to ",Results_W_Stations_Date_Range_QD$MaxDate)


#Add Zero of Zero and Results with QD together
ZeroOfZero<-tbl_df(rbind.data.frame(Results_W_Stations_Date_Range_QD,ZeroOfZero))

#Remove tossedsamplecount field
ZeroOfZero$TossedSampleCount<-NULL
ZeroOfZero$TotalSamples<-NULL
ZeroOfZero$MaxDate<-NULL
ZeroOfZero$MinDate<-NULL
ZeroOfZero<-tbl_df(ZeroOfZero)

#Remove the rows of data from main data frame that had data with quantitation issues
R_W_Language<-R_W_Dates
R_W_Language<-anti_join(R_W_Language,Quantitation_Discards,by=c("Waterbody","WBID","AnalyteName","FractionName"
,"MatrixName","BeneficialUse","ProjectName","Objective_Language","Evaluation_Guideline","Objective_Ref_Number"))


#Merge list of stations with the LOEs
R_W_Language$DATA_USED<-paste0("Water Board staff assessed ",R_W_Language$ProjectName," data for ",R_W_Language$Waterbody," to determine beneficial use support and results are as follows: ", R_W_Language$Exceedances," of ",R_W_Language$Total," samples exceeded the water quality threshold for ",R_W_Language$AnalyteName,".")

R_W_Language["TEMPORAL_REP"]<-NA
R_W_Language$TEMPORAL_REP<-paste0("The samples were collected between the dates of ", R_W_Language$MinDate, " and ", R_W_Language$MaxDate)

#Add station infomrnation to R_W_Language
#R_W_Language<-merge(R_W_Language,Stations,by=c("AnalyteName","Waterbody","WBID","ProjectName","MatrixName","FractionName"))
R_W_Language$StationCount<-1

R_W_Language<-tbl_df(R_W_Language)
R_W_Language$SPATIAL_REP<-paste0("The samples were collected at ",R_W_Language$StationCount," monitoring site (",R_W_Language$StationCode,")")

R_W_Language$MinDate<-NULL
R_W_Language$MaxDate<-NULL
R_W_Language$Alias<-NULL


R_W_Language<-rbind.data.frame(R_W_Language,ZeroOfZero)
R_W_Language$StationCount<-NULL
R_W_Language$StationCode<-NULL


#Merge with the DataUsed Ref Number and QA ref number data then remove project code from table
R_W_Language<-tbl_df(merge(R_W_Language,DataReferences,by="ProjectName"))
R_W_Language<-tbl_df(R_W_Language)
R_W_Language$ProjectName<-NULL

#Change Sample number to zero in Region 6 waterbodies
R_W_Language$Total[which(substr(R_W_Language$WBID,4,4)=="6"&R_W_Language$AnalyteName=="pH")]<-0
R_W_Language$DATA_USED[which(substr(R_W_Language$WBID,4,4)=="6"&R_W_Language$AnalyteName=="pH")]<-paste0(R_W_Language$DATA_USED," The objective for this pollutant requires background information that is currently unavailable, and therefore an assessment of water quality thresholds could not be made.")


#Change column names to match formatting required by CalWQA upload tool
colnames(R_W_Language)<-c("WB_SEGMENT","WBID","POLLUTANT","FRACTION","MATRIX","BU_CODE"
,"CRITERION/OBJECTIVE","EVAL_GUIDELINE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE_REFERENCES","EXCEEDANCE_COUNT","SAMPLE_COUNT","DATA_USED","TEMPORAL_REP"
,"SPATIAL_REP","DATA_USED_REFERENCES","QA_INFO_REFERENCES","QA_INFO","DATA_SOURCE")


 
R_W_Language<-tbl_df(R_W_Language)
R_W_Language$ASSESSMENT_STATUS<-"LOE In Progress"
R_W_Language$DATE_CREATED<-Sys.Date()

R_W_Language$MIGRATION_ID<-rownames(R_W_Language)

############################################
 ###Enter Region number and Username here#
      #############################
		  ###############
			#######
			  ###
			   #

	R_W_Language$REGION<-Region
if(Region=="5SJ"|Region=="5T"){
  R_W_Language$REGION<-"5"
}

	R_W_Language$AUTHOR<-Author


#########################################
###########################################
R_W_Language$MATRIX<-"Water"
R_W_Language$ENVIRONMENTAL_CONDITIONS<-""
R_W_Language$ASSESSOR_COMMENT<-""
R_W_Language$DATA_TYPE<-"PHYSICAL/CHEMICAL MONITORING"
R_W_Language$SUB_GROUP<-"Pollutant-Water"
#Change LOE Subroup to Ancillary Line of Evidence if there is no QAPP when there should be
R_W_Language$SUB_GROUP[which(R_W_Language$QA_INFO_REFERENCES=="4971"|R_W_Language$QA_INFO_REFERENCES=="5005"|R_W_Language$QA_INFO_REFERENCES=="4686")]<-"Ancillary Line of Evidence"
R_W_Language$AQ_USE_CODE<-""
R_W_Language$UPDATED_BY<-""
R_W_Language$DATE_UPDATED<-""
#Change fraction from particulate to total for chlorophyll-a
R_W_Language$FRACTION[which(R_W_Language$POLLUTANT=="Chlorophyll a"&R_W_Language$FRACTION=="Particulate")]<-"Total"

#Reorder the rows of the LOEs so that it matches the template


#R_W_Language<-R_W_Language[,c(21,22,2,3,1,28,4,5,6,7,9,8,10,12,11,13,14,15
#,16,17,18,19,20,23,24,25,26,27)]





R_W_Language

################_________________________________________####################



################____^^^^____________^^^^_________________####################


#Create unique list of data that was exported (and not assessed) by ReLEP, count the number of errors within each row of data that c19aused export
#and create a comma delimited list of these errors for review by getting a unique list of all fields in the data EXCEPT for the "Issue" field
AllExportedIssues<-AllExportedData%>%dplyr::group_by(WQID)%>%dplyr::summarise(count=n(),Issue=paste(Issue,collapse=", AND "))
AllExportedData<-tbl_df(merge(ExportedData,AllExportedIssues))
AllExportedData<-AllExportedData%>%select(WQID,ProgramCode,ProgramName,ParentProjectCode,ParentProjectName,ProjectCode,ProjectName,ProjectDescr
                                          ,QAPPCode,QAPPName,PublicRelease,SampleDate,StationCode,StationName,StationDescr,TargetLatitude,TargetLongitude,Datum,RegionalBoardID
                                          ,WaterBodyType,SampleAgency,SampleAgencyCode,SampleComments,CollectionTime,PositionWaterColumn,LabCollectionComments,CollectionMethodName
                                          ,SampleTypeCode,SampleTypeName,Replicate,CollectionDeviceCode,CollectionDeviceName,CollectionDepth,UnitCollectionDepth,LabAgencyCode,LabAgencyName
                                          ,SubmittingAgency,LabSubmissionCode,LabBatchComments,MatrixCode,MatrixName,MethodCode,MethodName,AnalyteCode,AnalyteName,FractionCode,FractionName
                                          ,UnitCode,UnitName,MDL,RL,QACode,LabBatch,AnalysisDate,LabReplicate,Result,ResQualCode,LabResultComments,SampleLatitiude,SampleLongitude,SampleDatum
                                          ,SampleElevation,SampleUnitElevation,DataQuality,DataQualityIndicator,FieldWater,count,Issue)

AssessorsComments<-ungroup(AllExportedData)
AssessorsComments<-select(AssessorsComments,AnalyteName,FractionName,ProjectName,StationCode,MatrixName,Result,Issue,SampleDate)
#AssessorsComments$MatrixName<-"Water"
AssessorsComments<-tbl_df(merge(AssessorsComments,Waterbodies))
AssessorsComments<-tbl_df(merge(AssessorsComments,DataReferences))
AssessorsComments<-AssessorsComments%>%dplyr::group_by(AnalyteName,FractionName,Issue,Waterbody,WBID,BeneficialUse,MatrixName,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO)%>%dplyr::summarise(count=n())
AssessorsComments<-ungroup(AssessorsComments)
colnames(AssessorsComments)<-c("POLLUTANT","FRACTION","Issue","WB_SEGMENT","WBID","BU_CODE","MATRIX","DATA_USED_REFERENCES","QA_INFO_REFERENCES","QA_INFO","count")
AssessorsComments$Reasons<-paste0(AssessorsComments$count," samples were thrown out because of ",AssessorsComments$Issue)
AssessorsComments$Issue<-NULL
AssessorsComments$count<-NULL
AssessorsComments$MATRIX<-"Water"
AssessorsComments<-AssessorsComments%>%dplyr::group_by(POLLUTANT,FRACTION,WB_SEGMENT,WBID,BU_CODE,MATRIX,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO)%>%dplyr::summarise(Reasons=paste(Reasons,collapse=", "))



R_W_Language<-tbl_df(merge(R_W_Language,AssessorsComments,all.x=TRUE))
R_W_Language$ASSESSOR_COMMENT<-paste0(R_W_Language$Reasons)
R_W_Language$Reasons<-NULL

R_W_Language<-tbl_df(merge(R_W_Language,ReLEP_to_CalWQA_Lookup,all.x=TRUE))
New_CalWQA_Pollutants<-R_W_Language
R_W_Language$POLLUTANT<-NULL
names(R_W_Language)[names(R_W_Language)=="PollutantName_in_CalWQA"]<-"POLLUTANT"

#Re-Order Columns to match LOE uploader template
R_W_Language<-R_W_Language[,c("MIGRATION_ID","REGION","WB_SEGMENT","WBID","POLLUTANT"
,"SUB_GROUP","MATRIX","FRACTION","BU_CODE","AQ_USE_CODE","CRITERION/OBJECTIVE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE","EVAL_GUIDELINE_REFERENCES","SAMPLE_COUNT","EXCEEDANCE_COUNT","SPATIAL_REP","TEMPORAL_REP","QA_INFO"
,"QA_INFO_REFERENCES","DATA_TYPE","DATA_USED","DATA_USED_REFERENCES","ASSESSMENT_STATUS","DATA_SOURCE"
,"AUTHOR","DATE_CREATED","UPDATED_BY","DATE_UPDATED","ENVIRONMENTAL_CONDITIONS","ASSESSOR_COMMENT")]


R_W_Language$ASSESSOR_COMMENT[R_W_Language$ASSESSOR_COMMENT=="NA"]<-""
R_W_Language$ASSESSOR_COMMENT<-paste0("(LOE written by ReLEP ", ReLEP_Version," ) ", R_W_Language$ASSESSOR_COMMENT)
R_W_Language$FRACTION[which(R_W_Language$POLLUTANT=="pH")]<-"None"

#Create file name for LOEs based on author, date and matrix
FileName<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"_LOEs_",FileNameInQuotations)
ExcelFileName<-paste0(substr(FileName,1,nchar(FileName)-4),".xlsx")
write.table(R_W_Language, FileName,sep="\t",row.names=FALSE,na="")


ExportedDataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_AllTossedData_",FileNameInQuotations)
ExcelExportedDataFileName<-paste0(substr(ExportedDataFileName,1,nchar(ExportedDataFileName)-4),".xlsx")
write.table(AllExportedData,ExportedDataFileName,sep="\t",row.names=FALSE,na="")

SSODataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_Data_W_SiteSpecific_Objectives_",FileNameInQuotations)
ExcelSSODataFileName<-paste0(substr(SSODataFileName,1,nchar(SSODataFileName)-4),".xlsx")
write.table(W_SSOs_for_Export,SSODataFileName,sep="\t",row.names=FALSE,na="")

#Export list of pollutants that need to be added to CalWQA
New_CalWQA_Pollutants<-tbl_df(unique(subset(New_CalWQA_Pollutants
                      ,select=c(POLLUTANT,PollutantName_in_CalWQA))))
New_CalWQA_Pollutants<-New_CalWQA_Pollutants[which(is.na(New_CalWQA_Pollutants$PollutantName_in_CalWQA)),]
New_CalWQA_Pollutants$PollutantName_in_CalWQA<-NULL
if(length(New_CalWQA_Pollutants$POLLUTANT)>0){
  write.table(New_CalWQA_Pollutants,"Outputs\\Pollutants_to_Add_to_CalWQA.txt",row.names=FALSE,na="")
}


Program_Run_Time<-Sys.time()-Start_Time
Program_Run_Time<-paste0("ReLEP took ",Program_Run_Time," minutes to write ",nrow(R_W_Language)," Line(s) of Evidence (LOE's)")
Program_Run_Time



