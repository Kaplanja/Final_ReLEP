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
#try not to add unecessary packages here because
#sometimes packages conflict with eachother
#or they will mask the ommands and cause unpredicable behavior in the code

library(dplyr,warn.conflicts=TRUE)
library(data.table, warn.conflicts=TRUE)
library(lubridate,warn.conflicts=TRUE)


#Removes all defined variables this is only important when you run the
#code multiple times in one session.  it removes previously defined variables
rm(list=ls(all=TRUE))

#saves system time to display how long the code takes to run at the end
Start_Time<- Sys.time()





#####################################################################################

#########################___Enter your name below_______############################

EnterYourNameHereInQuotations<- "Jkaplan"

Author<-EnterYourNameHereInQuotations


Region<-"5SJ"


ReLEP_Version<-"Version 1.1"

Write_Data_Viz_Table<-"Yes"								#Must be "Yes" to write table       #<<<<<<<<<<<-<<<<<<<<<<#

#####You will want to change the name of the below file name to match the name of the data file you########
####are trying to assess.  It must be a text file, and the field names must match.#########################
###########################################################################################################
		 ##########################
		    #####################
					#################
						############
							#######
							  ###
							   #
			FileNameInQuotations = "R5_Water_Sediment_And_Field_Data.txt"

###########################################################################################################

Author<-EnterYourNameHereInQuotations


WorkingDirectory<-paste0("C:\\Users\\",Author,"\\Desktop\\Final_ReLEP\\Region ",Region,"\\Sediment_Module")


SitesTable<-paste0("R",Region,"_Sites_FINAL.txt")

BeneficialUseTable<-paste0("R",Region,"_Beneficial_Use_FINAL.txt")

ObjectivesTable<-paste0("R",Region,"_Analytes_SED_FINAL.txt")

DataReferencesTable<-paste0("R",Region,"_Data_Used_DRAFT.txt")

SummingPollutantsTable<-"SummingPollutants.txt"

SelfNamedSummingPollutants<-"Self_Named_Summing_Pollutants.txt"

OCNormalizedPollutants<-"OC_Normalized_Pollutants.txt"



#Set the directory so that relative paths work for loading the files
setwd(WorkingDirectory)
							   
# Load Region Data file, remove unecessary fields, remove J Flagged samples, and convert to tbl_df
data<-tbl_df(read.delim(FileNameInQuotations,header=TRUE,stringsAsFactors=FALSE))
data<-data[which(data$MatrixName=="sediment"),]
data$SampleDate<-ymd(data$SampleDate)
NotAssessedData<-data
names(data)[names(data)=="Latitude"]<-"TargetLatitude"
names(data)[names(data)=="Longitude"]<-"TargetLongitude"
data<-subset(data,select=c("ParentProjectName","StationCode","SampleDate","MatrixName","AnalyteName","FractionName"
,"UnitName","Result","SampleTypeName","MDL","RL","ResQualCode","QACode","TargetLatitude","TargetLongitude","DataQuality","DataQualityIndicator"))
data$ProjectName<-data$ParentProjectName


#Create table of data with bad QA Codes to be exported and reviewed
BadQACodes<-filter(NotAssessedData,DataQuality!="Passed QC")
BadQACodes$Issue<-"Check DataQuality Field"
AllBadData<-BadQACodes

#Add data with results < 0 and ResQualCode of "=" to tossed data
LessThanZero<-NotAssessedData[which(NotAssessedData$ResQualCode=="="&NotAssessedData$Result<0),]
LessThanZero$Issue<-"Result less than Zero AND ResQualCode of '=' Check data for errors"
AllExportedData<-rbind.data.frame(AllBadData,LessThanZero)

#Remove data with results < 0 and ResQualCode of "=" to tossed data
data<-data[which(!(data$ResQualCode=="="&data$Result<0&!is.na(data$Result))),]



#Now do the opposite command on the data to get the remaining rows of data with acceptable QACodes
data<-filter(data,DataQuality=="Passed QC")


#Change negative results with ResQualCodes of ND or DNQ to be equal to zero
#the negative results are neative MDLs which means they will eather be clean samples
#or quantitation discards depending on the objective used so zero is appropriate
data$Result[which(data$Result<0 & (data$ResQualCode=="ND"|data$ResQualCode=="DNQ"))]<-0 #perfect scenario



WrongSampleType<-subset(NotAssessedData,!(SampleTypeName=="Grab"|SampleTypeName=="Integrated"))
WrongSampleType$Issue<-"Wrong Sample Type"
AllBadData<-tbl_df(rbind.data.frame(AllBadData,WrongSampleType))


#Remove SampleTypes that are related to QA or otherwise inappropriate for use in assessment
data<-subset(data,SampleTypeName=="Grab"|SampleTypeName=="Integrated")

#Remove data with bad res qual codes
MissingResQualCode<-filter(NotAssessedData, ((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode))&(RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))
MissingResQualCode$Issue<-"Missing Res Qual Code, RL and MDL"
AllBadData<-tbl_df(rbind.data.frame(AllBadData,MissingResQualCode))

#Multiply MDL by 3.18 to get RL if RL is missing as per quantitaiton guidance
data$RL[which((is.na(data$RL)|data$RL==""|data$RL==" ")&(data$MDL!="NA"|data$MDL!=""|data$MDL!=
" "))]<-data$MDL[which((is.na(data$RL)|data$RL==""|data$RL==" ")&(data$MDL!="NA"|data$MDL!=""|data$MDL!=" "))]*3.18


####################################################################################################################
#########################OTHER TABLE LOADING SECTION###############################################################


# Load sites table and convert to tbl_df
Sites<-read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE)
Sites<-Sites[Sites$STATUS=="Completed",]
Sites<-tbl_df(Sites)
Sites<-subset(Sites,select=c(StationCode,WBID,Waterbody))

# Load Beneficial Uses table and convert to tbl_df
#you must remove blank rows (rows with waterbodies but no WBIDs, or BUs)
#Before loading the table here or else it will skip rows sometimes
#Also, the program cannot read the NA BU right now.  This is an easy fix, but I have yet to run into any
#Objective that actually used the NA BU.
Beneficial_Uses<-tbl_df(read.delim(BeneficialUseTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Beneficial_Uses<-subset(Beneficial_Uses,select=c(Waterbody,WBID,BeneficialUse,Wbtype))
Beneficial_Uses$BUCODE<-Beneficial_Uses$BeneficialUse
Beneficial_Uses$BUCODE<-as.character(Beneficial_Uses$BUCODE)
Beneficial_Uses$BeneficialUse<-NULL


# Load Region 1 objecives table and convert to tbl_df
Analytes<-tbl_df(read.delim(ObjectivesTable,header=TRUE,stringsAsFactors=FALSE))
#The NA omit command that follows may be a problem because the code for one of the 
#beneficial uses is NA and so I think the code will remove it.  There should be come 
#workaround for this.
#Analytes<-na.omit(Analytes)
Analytes<-subset(Analytes,select=c(AnalyteName,UnitName,BUCODE,Objective,Objective_Language
                                   ,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number))


#Change column names of analyte table to match the names from the water module
colnames(Analytes)<-c("AnalyteName","UnitName","BUCODE","Objective","Objective_Language"
,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")
Analytes<-tbl_df(Analytes)

#Each AnalyteName may apprear multiple times, but each row should be unique
SummingPollutants<-read.delim(SummingPollutantsTable,header=TRUE,stringsAsFactors=FALSE)
SummingPollutants<-SummingPollutants[SummingPollutants$Matrix=="Sediment",]
SummingPollutants<-subset(SummingPollutants,select=c(AnalyteName,SummingName))
SummingPollutants<-tbl_df(SummingPollutants)

#Load the table of "Self Named" summing pollutants
#This is a table of pollutant names that appear in both their raw data form, and summed form
#For example, chlordane is a part of the sum of "Chlordanes" we must use this table to remove 
#these pollutants from the main data table after the summing pollutants have been split out
#If we dont do this, it would result in duplicated results for these analytes since we split 
#out summing pollutants by creating a copy of the rows in a new table, sum them with their associated analytes, 
#change the name, and then stack them onto the bottom of the main tablen. The reason we create 
#a copy instead of a straight split, is because the rest of the summing pollutants that are not "self named"
#will either have their own evaluation guideline separate from the sum that also needs to be assessed, or,
#they will not, in which case they will be removed from the main data table when we look up objectives


Self_Named_Summing_Pollutants<-tbl_df(read.delim(SelfNamedSummingPollutants,header=TRUE,stringsAsFactors=FALSE))


#Contains CalWQA ref numbers for the data set and QAPPs
DataReferences<-read.delim(DataReferencesTable,header=TRUE,stringsAsFactors=FALSE)
DataReferences<-tbl_df(DataReferences)
DataReferences<-subset(DataReferences,select=c(ParentProjectName,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO,DATA_SOURCE,Assess))
DataReferences<-DataReferences[DataReferences$Assess=="Yes",]
DataReferences$Assess<-NULL
MissingQAPP<-NotAssessedData
MissingQAPP$ProjectName1<-MissingQAPP$ProjectName
MissingQAPP$ProjectName<-MissingQAPP$ParentProjectName
MissingQAPP<-anti_join(MissingQAPP,DataReferences)
MissingQAPP$ProjectName<-MissingQAPP$ProjectName1
MissingQAPP$ProjectName1<-NULL
MissingQAPP$Issue<-"Project missing QAPP or flagged as do not assess."
AllBadData<-rbind.data.frame(AllBadData,MissingQAPP)


#List of pollutants that need to be OC normalized
OC_Normalizing_Pollutants<-read.delim(OCNormalizedPollutants,header=TRUE,stringsAsFactors=FALSE)
OC_Normalizing_Pollutants<-tbl_df(OC_Normalizing_Pollutants)

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
Missing_Stations<-anti_join(NotAssessedData,Waterbodies,by="StationCode")
Missing_Stations$Issue<-"Missing waterbody information"
AllBadData<-tbl_df(rbind.data.frame(AllBadData,Missing_Stations))

#Split out organic carbon
TOC<-W_Waterbodies[W_Waterbodies$AnalyteName=="Total Organic Carbon",]



##############################Begin UNIT CONVERSION SECTION###################


# Convert Results, MDL, and RL that are in mg/Kg to ug/Kg datasetwide and change UnitName to ug/Kg dw
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="mg/Kg dw")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="mg/Kg dw")]*1000
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="mg/Kg dw")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="mg/Kg dw")]*1000
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="mg/Kg dw")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="mg/Kg dw")]*1000
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="mg/Kg dw"]<-"ug/Kg dw"

# Convert Results, MDL, and RL that are in pg/Kg to ug/Kg datasetwide and change UnitName to ug/Kg dw
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="pg/Kg dw")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="pg/Kg dw")]*.000001
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="pg/Kg dw")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="pg/Kg dw")]*.000001
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="pg/Kg dw")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="pg/Kg dw")]*.000001
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="pg/Kg dw"]<-"ug/Kg dw"

# Convert Results, MDL, and RL that are in ng/Kg to ug/Kg datasetwide and change UnitName to ug/Kg dw
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ng/Kg dw")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ng/Kg dw")]*.001
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ng/Kg dw")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ng/Kg dw")]*.001
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ng/Kg dw")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ng/Kg dw")]*.001
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="ng/Kg dw"]<-"ug/Kg dw"

# Convert Results, MDL, and RL that are in ng/g to ug/Kg datasetwide and change UnitName to ug/Kg dw
#This is a one to one conversion, so all we have to do is change the name of the units
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="ng/g dw"]<-"ug/Kg dw"

# Convert Results, MDL, and RL that are in g/Kg to ug/Kg datasetwide and change UnitName to ug/Kg dw
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="g/Kg dw")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="g/Kg dw")]*1000000
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="g/Kg dw")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="g/Kg dw")]*1000000
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="g/Kg dw")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="g/Kg dw")]*1000000
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="g/Kg dw"]<-"ug/Kg dw"

#Hiram Sarabia
#Proposed change to fix ug/g unit issue
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ug/g dw")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ug/g dw")]*1000
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ug/g dw")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ug/g dw")]*1000
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ug/g dw")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ug/g dw")]*1000
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="ug/g dw"]<-"ug/Kg dw"

#Create funky units table
Funky_Units<-subset(NotAssessedData,!(UnitName=="g/Kg dw"|UnitName=="ug/g dw"|UnitName=="ng/Kg dw"|UnitName=="ng/g dw"|UnitName=="pg/Kg dw"|UnitName=="mg/Kg dw"|UnitName=="ug/Kg dw"))
Funky_Units$Issue<-"Units not assessable"
AllBadData<-tbl_df(rbind.data.frame(AllBadData,Funky_Units))


##########################___End Unit Conversion____############################################

##########################__Begin Assessment Prep Section___####################################

#Export TOC data that is not reported in % dw (Just in case)
Bad_TOC<-tbl_df(NotAssessedData[which(NotAssessedData$AnalyteName=="Total Organic Carbon"&!(NotAssessedData$UnitName=="% dw"|NotAssessedData$UnitName=="mg/Kg dw")),])
Bad_TOC$Issue<-"Units not assessable"
AllBadData<-tbl_df(rbind.data.frame(AllBadData,Bad_TOC))

#Adjust units of TOC from mg/Kg dw to % dw for TOC conversion
TOC$Result[which(TOC$UnitName=="mg/Kg dw")]<-TOC$Result[which(TOC$UnitName=="mg/Kg dw")]*.0001
TOC$Result[which(TOC$AnalyteName=="Total Organic Carbon"&TOC$UnitName!="% dw")]<-TOC$Result[which(TOC$AnalyteName=="Total Organic Carbon"&TOC$UnitName!="% dw")]*.0001
TOC$UnitName[which(TOC$AnalyteName=="Total Organic Carbon"&TOC$UnitName=="mg/Kg dw")]<-"% dw"

#Prepare TOC table to be joined with OC normalizing pollutants by deleting unecessary columns and changing the names
#to remove ambiguity once joined (e.g. change reuslt to TOCResult)
TOC$MDL<-NULL
TOC$RL<-NULL
TOC$QACode<-NULL
TOC$ResQualCode<-NULL
TOC$UnitName<-NULL
TOC$FractionName<-NULL
TOC$AnalyteName<-NULL
names(TOC)[names(TOC)=="Result"]<-"TOC_Result"
TOC<-TOC[which(!is.na(TOC$TOC_Result)),]


#Remove replicates of TOC
TOC<-as.data.table(TOC)[,mean(TOC_Result),list(StationCode,ProjectName,SampleDate,MatrixName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,BUCODE)]
TOC<-tbl_df(TOC)
names(TOC)[names(TOC)=="V1"]<-"TOC_Result"

#Create table of pollutants for OC normalization
OC_Normalizing<-tbl_df(merge(W_Waterbodies,OC_Normalizing_Pollutants,by="AnalyteName"))

#Remove rows of data that need OC Normalizing from the main data frame
W_Waterbodies<-anti_join(W_Waterbodies,OC_Normalizing_Pollutants,by="AnalyteName")

#Add TOC to the rows that need to be OC normalized
OC_Normalizing<-tbl_df(merge(OC_Normalizing,TOC,by=c("StationCode","SampleDate","Waterbody","WBID","ProjectName","MatrixName","TargetLatitude","TargetLongitude","BUCODE","Wbtype")))

#Create table of pollutants that need OC normilization but do not have TOC to compare to
No_TOC<-tbl_df(merge(NotAssessedData,OC_Normalizing_Pollutants))
No_TOC<-anti_join(No_TOC,TOC,by=c("SampleDate","StationCode","MatrixName"))
No_TOC$Issue<-"Needs TOC conversion, but missing corresponding TOC"
AllBadData<-tbl_df(rbind.data.frame(AllBadData,No_TOC))


#OC Normalize the result, RL, and MDL of the pollutant that needs to be OC normalized so that we can run the quantitation check
#This step actually replaces the Result, MDL, and RL with the OC Normalized versions
#this saves a few lines of code as opposed to creating new fields for these values, deleting the old ones
#and then finally changing the calculated fields to the orignal names, but it could be confusing
OC_Normalized<-mutate(OC_Normalizing,Result=(Result/(TOC_Result/100)))
OC_Normalized<-mutate(OC_Normalized,RL=(RL/(TOC_Result/100)))
OC_Normalized<-mutate(OC_Normalized,MDL=(MDL/(TOC_Result/100)))

#Remove TOC result from table then join back into main data frame
OC_Normalized$TOC_Result<-NULL

#Add OC Normalized data back into main data frame
W_Waterbodies<-rbind.data.frame(W_Waterbodies,OC_Normalized)

#Split out the rows of data that need to be summed
SummingPollutants_Data<-tbl_df(merge(W_Waterbodies,SummingPollutants,by="AnalyteName"))

Pyrethroids<-SummingPollutants_Data[SummingPollutants_Data$SummingName=="Pyrethroids",]
Pyrethroids<-tbl_df(merge(Pyrethroids,Analytes,by=c("AnalyteName","BUCODE","UnitName")))
#Adjust for quantitation issues
Pyrethroids$Result[which(Pyrethroids$RL<Pyrethroids$Objective & (Pyrethroids$ResQualCode=="ND"|Pyrethroids$ResQualCode=="DNQ"))]<-0 #perfect scenario

#Create table to tossed pyrethroid samplesremove tossed samples from assessment table
PyrethroidDiscards<-subset(Pyrethroids,(RL>Objective&(ResQualCode=="ND"|ResQualCode=="DNQ")))
PyrethroidDiscards$AnalyteName<-PyrethroidDiscards$SummingName
PyrethroidDiscards<-subset(PyrethroidDiscards,select=c("AnalyteName", "BUCODE"
                      , "UnitName", "StationCode", "SampleDate", "MatrixName"
                      , "FractionName", "Result", "TargetLatitude", "TargetLongitude"
                      , "ProjectName", "Waterbody", "WBID", "Wbtype","DataQuality"
                      ,"DataQualityIndicator","SampleTypeName","ParentProjectName"))
PyrethroidDiscards$Objective<-NULL
PyrethroidDiscards$Objective_Language<-NULL
PyrethroidDiscards$Objective_Ref_Number<-NULL
PyrethroidDiscards$Eval_Ref_Number<-NULL
PyrethroidDiscards$Evaluation_Guideline<-NULL
PyrethroidDiscards<-tbl_df(merge(PyrethroidDiscards,Analytes,by=c("AnalyteName","BUCODE","UnitName")))

#remove tossed samples from assessment table
Pyrethroids<-subset(Pyrethroids,!(RL>Objective&(ResQualCode=="ND"|ResQualCode=="DNQ")))
Pyrethroids<-tbl_df(Pyrethroids)

#Remove replicates from the pyrethroid data
Pyrethroids<-as.data.table(Pyrethroids)[,mean(Result),list(AnalyteName,StationCode,SampleDate
,MatrixName,FractionName,UnitName,ProjectName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,BUCODE
,Objective,SummingName)]
names(Pyrethroids)[names(Pyrethroids)=="V1"]<-"Result"
#Divide the result by the objective to get LC50 normalized data
Pyrethroids<-mutate(Pyrethroids,Result=Result/Objective)

#Remove uncessary fields
Pyrethroids<-subset(Pyrethroids,select=c("AnalyteName","BUCODE","UnitName"
,"StationCode","ProjectName","SampleDate","MatrixName","FractionName","Result"
,"Waterbody","WBID","Wbtype","SummingName","TargetLatitude","TargetLongitude"))

#Swap CEDEN analyte name for summed analyte name
Pyrethroids$AnalyteName<-NULL
names(Pyrethroids)[names(Pyrethroids)=="SummingName"]<-"AnalyteName"

#Add LC50 normalized results together so that all pyrethroid
#toxicity is summed for the sample day
Pyrethroids<-as.data.table(Pyrethroids)[,sum(Result),list(AnalyteName,BUCODE
,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName,Waterbody,WBID,Wbtype
,TargetLatitude,TargetLongitude)]
names(Pyrethroids)[names(Pyrethroids)=="V1"]<-"Result"
Pyrethroids<-tbl_df(Pyrethroids)
#add objectives back to the table
Pyrethroids<-tbl_df(merge(Pyrethroids, Analytes,by=c("AnalyteName","BUCODE","UnitName")))

#Remove tossed samples from discards if there were usable samples for that station day
PyrethroidDiscards<-anti_join(PyrethroidDiscards,Pyrethroids,by=c("AnalyteName"
                ,"BUCODE","UnitName","StationCode","SampleDate","MatrixName"
                ,"FractionName"))
PyrethroidDiscards$Result<--100000000

#Look up the objectives of the summing pollutants based on the summing name
names(SummingPollutants_Data)[names(SummingPollutants_Data)=="AnalyteName"]<-"CEDEN_Name"
names(SummingPollutants_Data)[names(SummingPollutants_Data)=="SummingName"]<-"AnalyteName"

#Split out the PCB and Arcoclor data from the main data frame to take the max result of the sum for each day
#The max will be used to determine BU suppoort
#Split out PCBs and Aroclors from summed summing pollutants to calculate the max 
#sample day result for assessment
PCBs<-SummingPollutants_Data[which((SummingPollutants_Data$AnalyteName=="Aroclor"
	|SummingPollutants_Data$AnalyteName=="PCBs (Polychlorinated biphenyls)")&(SummingPollutants_Data$BUCODE=="CO"
	|SummingPollutants_Data$BUCODE=="WA")),]

PCBs$ObjectiveName<-paste0("PCBs (Polychlorinated biphenyls)")
PCBs$TempName<-PCBs$AnalyteName
PCBs$AnalyteName<-PCBs$ObjectiveName
PCBs<-tbl_df(merge(PCBs,Analytes,by=c("AnalyteName","UnitName","BUCODE")))


PCBs$Result[which(PCBs$RL<PCBs$Objective 
	& (PCBs$ResQualCode=="ND"|PCBs$ResQualCode=="DNQ"))]<-0 #perfect scenario

#Create table of PCB discards because of imperfect scenario
PCBDiscards<-subset(PCBs,(RL>Objective&(ResQualCode=="ND"|ResQualCode=="DNQ")))



#Remove imperfect scenaior discards from PCB table
PCBs<-subset(PCBs,!(RL>Objective&(ResQualCode=="ND"|ResQualCode=="DNQ")))
PCBs<-tbl_df(PCBs)



PCBs_No_reps<-tbl_df(as.data.table(PCBs)[,mean(Result),list(AnalyteName,TempName,BUCODE
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,CEDEN_Name,Objective
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(PCBs_No_reps)[names(PCBs_No_reps)=="V1"]<-"Result"

PCBs_Sums<-tbl_df(as.data.table(PCBs_No_reps)[,sum(Result),list(AnalyteName,TempName,BUCODE
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,Objective
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(PCBs_Sums)[names(PCBs_Sums)=="V1"]<-"Result"

PCBs_Max<-tbl_df(as.data.table(PCBs_Sums)[,max(Result),list(AnalyteName,BUCODE
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,Objective
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(PCBs_Max)[names(PCBs_Max)=="V1"]<-"Result"

PCBs_Max$TargetLatitude<-NULL
PCBs_Max$TargetLongitude<-NULL


#Add objective to the table
SummingPollutants_W_Objectives<-tbl_df(merge(SummingPollutants_Data,Analytes,by=c("AnalyteName","UnitName","BUCODE")))
#Remove pyrethroids to avoid double counting
SummingPollutants_W_Objectives<-filter(SummingPollutants_W_Objectives,AnalyteName!="Pyrethroids")

#Create a table of summing pollutants that do not have an objective
No_Sums<-tbl_df(merge(NotAssessedData,SummingPollutants))
No_Sums$TempName<-No_Sums$AnalyteName
No_Sums$AnalyteName<-No_Sums$SummingName
No_Sums$SummingName<-NULL
No_Sums<-anti_join(No_Sums,Analytes,by=c("AnalyteName"))
No_Sums$Issue<-paste0("Summing pollutant whos summed name (",No_Sums$AnalyteName,") does not have an objective")
No_Sums$AnalyteName<-No_Sums$TempName
No_Sums$TempName<-NULL
AllBadData<-tbl_df(rbind.data.frame(AllBadData,No_Sums))


# Correct the results based on quantitation limits - I did this before removing 
#replicates to avoid averaging samples that we assesed with a different method

SummingPollutants_W_Objectives$Result[which(SummingPollutants_W_Objectives$RL<SummingPollutants_W_Objectives$Objective 
	& (SummingPollutants_W_Objectives$ResQualCode=="ND"|SummingPollutants_W_Objectives$ResQualCode=="DNQ"))]<-0 #perfect scenario

#Create table of quantitation discards of summing pollutants
SummingPollutants_Discards<-subset(SummingPollutants_W_Objectives,(RL>Objective&(ResQualCode=="ND"|ResQualCode=="DNQ")))
SummingPollutants_Discards<-subset(SummingPollutants_Discards,select=c("AnalyteName", "BUCODE"
                                                       , "UnitName", "StationCode", "SampleDate", "MatrixName"
                                                       , "FractionName", "Result", "TargetLatitude", "TargetLongitude"
                                                       , "ProjectName", "Waterbody", "WBID", "Wbtype","DataQuality"
                                                       ,"DataQualityIndicator","SampleTypeName","ParentProjectName"))
SummingPollutants_Discards<-tbl_df(merge(SummingPollutants_Discards,Analytes,by=c("AnalyteName","UnitName","BUCODE")))

#Remove discards from main summing table
SummingPollutants_W_Objectives<-subset(SummingPollutants_W_Objectives,!(RL>Objective&(ResQualCode=="ND"|ResQualCode=="DNQ")))
SummingPollutants_W_Objectives<-tbl_df(SummingPollutants_W_Objectives)



#Remove replicates
Sum_No_reps<-as.data.table(SummingPollutants_W_Objectives)[,mean(Result),list(AnalyteName,BUCODE
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName,MDL,RL,ResQualCode
	,QACode,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,CEDEN_Name,Objective
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
	names(Sum_No_reps)[names(Sum_No_reps)=='V1']<-"Result"
	Sum_No_reps<-tbl_df(Sum_No_reps)


#Add the results fields together for the quantitation adjusted summing pollutants
#Add the results together based on the summing pollutant name (not the CEDEN name)
SummedSummingPollutants<-as.data.table(Sum_No_reps)[,sum(Result),list(AnalyteName,BUCODE
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,Objective
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(SummedSummingPollutants)[names(SummedSummingPollutants)=="V1"]<-"Result"
SummedSummingPollutants<-tbl_df(SummedSummingPollutants)

#Remove summing pollutant discards from the discard table if they also had valid samples for that day
SummingPollutants_Discards<-anti_join(SummingPollutants_Discards,SummedSummingPollutants
                    ,by=c("AnalyteName","BUCODE","UnitName","StationCode","SampleDate"
                          ,"MatrixName","FractionName"))
SummingPollutants_Discards$Result<--100000000


#Add the pyrethroid data back into the summing polltuants table
SummedSummingPollutants<-rbind.data.frame(SummedSummingPollutants,Pyrethroids)


SummedSummingPollutants$TargetLatitude<-NULL
SummedSummingPollutants$TargetLongitude<-NULL

#Add the PCB data and the summed pollutants together but first remove the ones the were summed with the max of PCB and Aroclors taken
SummedSummingPollutants<-tbl_df(anti_join(SummedSummingPollutants,PCBs,by=c("AnalyteName"
	,"BUCODE","StationCode","ProjectName","SampleDate","MatrixName","FractionName"
	,"Waterbody","WBID","Wbtype","Objective","Objective_Language","Evaluation_Guideline"
	,"Objective_Ref_Number","Eval_Ref_Number")))

SummedSummingPollutants<-rbind.data.frame(SummedSummingPollutants,PCBs_Max)

#Create a new table of station codes, waterbodies, analyte, and count of stations
#SummedStations<-subset(SummedSummingPollutants,select=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName","MatrixName","FractionName"))
#SummedStations<-distinct(select(SummedStations,AnalyteName,Waterbody,StationCode,WBID,ProjectName,MatrixName,FractionName))
#SummedStations<-SummedStations%>%group_by(AnalyteName,Waterbody,WBID,ProjectName,MatrixName,FractionName)%>%summarise(count=n(),
#StationCode=paste(StationCode,collapse=", "))
#names(SummedStations)<-c("AnalyteName","Waterbody","WBID","ProjectName","MatrixName","FractionName","StationCount","StationCode")


#######################################################################################################

########_________________ End of summing pollutants section __________________#########################

#######################################################################################################

#######___________________ Beginning of general pollutants section ___________________#################

#######################################################################################################


#Remove "self-named" pollutants from the main data frame

W_Waterbodies<-anti_join(W_Waterbodies,Self_Named_Summing_Pollutants,by="AnalyteName")


#Create a new table of station codes, waterbodies, analyte, and count of stations
#Stations<-subset(W_Waterbodies,select=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName","MatrixName","FractionName"))
#Stations<-distinct(select(Stations,AnalyteName,Waterbody,StationCode,WBID,ProjectName,MatrixName,FractionName))
#Stations<-Stations%>%group_by(AnalyteName,Waterbody,WBID,ProjectName,MatrixName,FractionName)%>%summarise(count=n(),
#StationCode=paste(StationCode,collapse=", "))
#names(Stations)<-c("AnalyteName","Waterbody","WBID","ProjectName","MatrixName","FractionName","StationCount","StationCode")

#Combine Stations, and SummedStations together to get one stations table
#Stations<-tbl_df(rbind.data.frame(Stations,SummedStations))

#Look up objective for the data
W_Objectives<-tbl_df(merge(W_Waterbodies,Analytes,by=c("AnalyteName","UnitName","BUCODE")))

#Create table of data that does not have any objective
Missing_Objectives<-anti_join(NotAssessedData,Analytes,by=c("AnalyteName"))
Missing_Objectives<-anti_join(Missing_Objectives,SummingPollutants)
Missing_Objectives$Issue<-"Pollutant does not have objective"
AllBadData<-tbl_df(rbind.data.frame(AllBadData,Missing_Objectives))


# Correct the results based on quantitation limits - I did this before removing replicates to avoid averaging samples that we assesed with a different method

W_Objectives$Result[which(W_Objectives$RL<W_Objectives$Objective & (W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<-W_Objectives$MDL[which(
	W_Objectives$RL<W_Objectives$Objective & (W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]*.5 #perfect scenario

W_Objectives$Result[which((W_Objectives$RL>W_Objectives$Objective|is.na(W_Objectives$RL))&(W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<--100000000 #imperfect results in very negative number

#Remove unecessary QA information from the table now that it has been used
W_Objectives$RL<-NULL
W_Objectives$MDL<-NULL
W_Objectives$ResQualCode<-NULL
W_Objectives$QACode<-NULL


#Create table of tossed samples and then combine them with the other tables of tossed samples due to quantitation limts
General_Tossed<-filter(W_Objectives,(Result==-100000000))

#Add tossed pyrethroid samples in to tossed data
General_Tossed<-rbind.data.frame(General_Tossed,PyrethroidDiscards)

#Add tossed summing pollutant samples in to tossed data
General_Tossed<-rbind.data.frame(General_Tossed,SummingPollutants_Discards)

# Calculate maximum and minimum date for each waterbody with data
General_Tossed$SampleDate<-as.Date(General_Tossed$SampleDate, "%m/%d/%Y")
Tossed_Max_Date<-as.data.table(General_Tossed)[,max(SampleDate),list(AnalyteName,StationCode
	,BUCODE,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Tossed_Max_Date)[names(Tossed_Max_Date)=='V1']<-"MaxDate"
Tossed_Max_Date<-tbl_df(Tossed_Max_Date)
Tossed_Min_Date<-as.data.table(General_Tossed)[,min(SampleDate),list(AnalyteName,StationCode
	,BUCODE,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Tossed_Min_Date)[names(Tossed_Min_Date)=='V1']<-"MinDate"
Tossed_Min_Date<-tbl_df(Tossed_Min_Date)

#Combine max and min date together to get date range as well as other sample information
Tossed_Date_Range<-tbl_df(merge(Tossed_Max_Date,Tossed_Min_Date,by=c("AnalyteName","StationCode"
	,"Waterbody","WBID","ProjectName","MatrixName","FractionName","BUCODE","Objective"
	,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))



#Convert General_Tossed into a list of samples thrown out with language specifying they were thrown out due to quantitation issues
Quantitation_Discards<-distinct(select(General_Tossed,AnalyteName,StationCode,SampleDate,Waterbody,WBID,MatrixName
	,FractionName,ProjectName,BUCODE,Objective_Language,Evaluation_Guideline
	,Objective_Ref_Number,Eval_Ref_Number))
Quantitation_Discards<-Quantitation_Discards%>%dplyr::group_by(AnalyteName,StationCode,Waterbody,WBID
	,ProjectName,MatrixName,FractionName,BUCODE
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
	,Eval_Ref_Number)%>%dplyr::summarise(count=n())
names(Quantitation_Discards)<-c("AnalyteName","StationCode","Waterbody","WBID","ProjectName"
	,"MatrixName","FractionName","BUCODE","Objective_Language"
	,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"
	,"TossedSampleCount")

#Combine date range and tossed sample count together

Quantitation_Discards<-tbl_df(merge(Quantitation_Discards,Tossed_Date_Range, by=c("AnalyteName","StationCode"
	,"Waterbody","WBID","ProjectName","MatrixName","FractionName","BUCODE","Objective_Language"
	,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))
Quantitation_Discards$Objective<-NULL


#Filter out the samples that were discarded due to quantitation issues
W_Objectives<-filter(W_Objectives,(Result!=-100000000))


No_reps<-as.data.table(W_Objectives)[,mean(Result),list(AnalyteName,BUCODE,UnitName
	,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,Waterbody,WBID,Wbtype,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
	,Eval_Ref_Number)]
names(No_reps)[names(No_reps)=='V1']<-"Result"
W_Objectives<-tbl_df(No_reps)

#Add unecessary QA information from the table now that it has been used so that we can join the Summing Pollutants back
#To the main data frame

#Add the summed data back to the main data frame
W_Objectives<-rbind.data.frame(W_Objectives,SummedSummingPollutants)

##############################################################################################################

##########################Begin Averaging Peroid##############################################################

###############################################################################################################


#Since none of the sediment evaluation guidelines has specified averaging peroids,
#we default to the averaging peroid specified in the Listing Policy "Seven Days"

W_Objectives$SampleDate<-ymd(W_Objectives$SampleDate)


#Average all samples collected within 7 days of eachother
W_Objectives2 <- W_Objectives %>% arrange(SampleDate)%>%
  dplyr::group_by(AnalyteName,BUCODE,UnitName,StationCode,ProjectName, MatrixName,FractionName,Waterbody
	,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  dplyr::mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 7)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = mean(Result)) %>%
  dplyr::select(AnalyteName,SampleDate,BUCODE,UnitName,StationCode,ProjectName, MatrixName, FractionName
	,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number,Result)

No_reps<-tbl_df(W_Objectives2)

#Convert No_reps back into W_Objectives
W_Objectives<-tbl_df(No_reps)
#W_Objectives$Objective_Ref_Number<-as.numeric(W_Objectives$Objective_Ref_Number)

#Data Viz file export
VisualizationDataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"For_Visualization_",FileNameInQuotations)
if(Write_Data_Viz_Table=="Yes"){
  SedimentDataViz<-W_Objectives
  SedimentDataViz$Region<-Region
  write.table(SedimentDataViz,VisualizationDataFileName,sep="\t",row.names=FALSE)
}

##############################################################################################################

########################_________________End averaging peroid_________########################################

##############################################################################################################

W_Objectives_for_date_Range<-W_Objectives

#Get list of samples that were tossed for quantitation issues and their sample dates
#for LOEs/Data sets that also had quantifiable samples in the data set
#In other words get a list of samples for non zero of zero LOEs that also had quantitation discards
Tossed_Samples_W_Good_Samples<-Tossed_Date_Range
Tossed_Samples_W_Good_Samples<-subset(Tossed_Samples_W_Good_Samples,select=c(Waterbody,WBID,AnalyteName,BUCODE,MatrixName,
                              StationCode,ProjectName,FractionName,Objective_Language,Evaluation_Guideline,
                              Eval_Ref_Number,Objective_Ref_Number))
Tossed_Samples_W_Good_Samples<-tbl_df(merge(Tossed_Samples_W_Good_Samples,W_Objectives_for_date_Range))
Tossed_Samples_W_Good_Samples<-subset(Tossed_Samples_W_Good_Samples,select=c(Waterbody,WBID,AnalyteName,BUCODE,MatrixName,
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
Tossed_Samples_W_Good_Samples$UnitName<-"ug/Kg dw"
W_Objectives_for_date_Range<-rbind.data.frame(W_Objectives_for_date_Range,Tossed_Samples_W_Good_Samples)


W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
Over<-as.data.table(W_Objectives)[,sum(Result>Objective),list(AnalyteName,StationCode,BUCODE,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Over)[names(Over)=='V1']<-"Exceedances"
Over<-tbl_df(Over)

W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
Total<-as.data.table(W_Objectives)[,sum(Result>=0),list(AnalyteName,StationCode,BUCODE,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Total)[names(Total)=='V1']<-"Total"
Total<-tbl_df(Total)


# Combine the two toegether
results<-merge(Over,Total,by=c("AnalyteName","StationCode","BUCODE","ProjectName","MatrixName","FractionName"
,"Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
results<-tbl_df(results)

#Results_W_Stations<-tbl_df(merge(results,Stations,by=c("AnalyteName","Waterbody","WBID","ProjectName","MatrixName","FractionName")))
Results_W_Stations<-results
Results_W_Stations$StationCount<-1


#Add the station information to the field SpatialRep with some generic text
Results_W_Stations$SPATIAL_REP<-paste0("Data were collected from ",Results_W_Stations$StationCount," station(s). Station Code(s): ",Results_W_Stations$StationCode,".")


W_Objectives_for_date_Range$SampleDate<-as.Date(W_Objectives_for_date_Range$SampleDate, "%m/%d/%Y")
Max_Date<-as.data.table(W_Objectives_for_date_Range)[,max(SampleDate),list(AnalyteName,StationCode,BUCODE
,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
names(Max_Date)[names(Max_Date)=='V1']<-"MaxDate"
Max_Date<-tbl_df(Max_Date)
Min_Date<-as.data.table(W_Objectives_for_date_Range)[,min(SampleDate),list(AnalyteName,StationCode,BUCODE
,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
names(Min_Date)[names(Min_Date)=='V1']<-"MinDate"
Min_Date<-tbl_df(Min_Date)
Date_Range<-tbl_df(merge(Max_Date,Min_Date,by=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName"
,"MatrixName","FractionName","BUCODE"
,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))


#Add Date_Range to the end of the Results table
Results_W_Stations_Date_Range<-tbl_df(merge(Results_W_Stations,Date_Range,by=c("AnalyteName","StationCode"
,"Waterbody","WBID","ProjectName","MatrixName","FractionName","BUCODE","Objective_Language"
,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))

#Add date range to generic text

Results_W_Stations_Date_Range$TEMPORAL_REP<-paste0("Data for this waterbody were collected over the date range ", Results_W_Stations_Date_Range$MinDate, " to ",Results_W_Stations_Date_Range$MaxDate)




#Create table of LOEs for data that only contained data with quantitation issues
#These will be LOEs with zero of zero exceedances
ZeroOfZero<-anti_join(Quantitation_Discards,Results_W_Stations_Date_Range,by=c("Waterbody","StationCode","WBID","AnalyteName"
,"FractionName","MatrixName","BUCODE","ProjectName","Objective_Language","Evaluation_Guideline"
,"Objective_Ref_Number","Eval_Ref_Number"))


ZeroOfZero$Total<-0
ZeroOfZero$Total<-as.integer(ZeroOfZero$Total)
ZeroOfZero$Exceedances<-0
ZeroOfZero$Exceedances<-as.integer(ZeroOfZero$Exceedances)

#Add temporal rep field to the zero of zero LOEs
ZeroOfZero$TEMPORAL_REP<-paste0("Date for this waterbody were collected over the date range ", ZeroOfZero$MinDate, " to ",ZeroOfZero$MaxDate)

#Add station information to the zero of zeros
ZeroOfZero$StationCount<-1

#Add the station information to the field SpatialRep with some generic text
ZeroOfZero$SPATIAL_REP<-paste0("Data were collected from ",ZeroOfZero$StationCount," station(s). Station Code(s): ",ZeroOfZero$StationCode,".")
ZeroOfZero<-tbl_df(ZeroOfZero)

ZeroOfZero<-tbl_df(mutate(ZeroOfZero,TotalSamples=Total+TossedSampleCount))

ZeroOfZero$DATA_USED<-paste0("Water Board staff assessed ",ZeroOfZero$ProjectName," data for ",ZeroOfZero$Waterbody," to determine beneficial use support, and the results are as follows: ",ZeroOfZero$Exceedances, " of the ",ZeroOfZero$Total," samples exceeded the evaluation guideline for ",ZeroOfZero$AnalyteName,". Although a total of ",ZeroOfZero$TotalSamples," samples were collected, ", ZeroOfZero$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")




#######Add the information from the tossed samples but remove max and min dates first
######The tossed samples will be added to the data used field
Quantitation_Discards_No_Range<-Quantitation_Discards
Quantitation_Discards_No_Range$MaxDate<-NULL
Quantitation_Discards_No_Range$MinDate<-NULL

#This is a table of LOEs with samples thrown out due to quantitation issues
#Data used must be written separately because of the quantitation samples
Results_W_Stations_Date_Range_QD<-tbl_df(merge(Results_W_Stations_Date_Range
,Quantitation_Discards_No_Range,by=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName"
,"MatrixName","FractionName","BUCODE","Objective_Language","Evaluation_Guideline"
,"Objective_Ref_Number","Eval_Ref_Number")))

#Add TotalSample Field
Results_W_Stations_Date_Range_QD<-tbl_df(mutate(Results_W_Stations_Date_Range_QD,TotalSamples=Total+TossedSampleCount))

#Add the Data used field using the quantitation discarded samples
#Keep this command on ONE line or it will cause issues with the LOE export
Results_W_Stations_Date_Range_QD$DATA_USED<-paste0("Water Board staff assessed ",Results_W_Stations_Date_Range_QD$ProjectName," data for ",Results_W_Stations_Date_Range_QD$Waterbody," to determine beneficial use support and the results are as follows: ",Results_W_Stations_Date_Range_QD$Exceedances, " of the ",Results_W_Stations_Date_Range_QD$Total," samples exceeded the evaluation guideline for ",Results_W_Stations_Date_Range_QD$AnalyteName,". Although a total of ",Results_W_Stations_Date_Range_QD$TotalSamples," samples were collected, ", Results_W_Stations_Date_Range_QD$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")

#Add Zero of Zero and Results with QD together
ZeroOfZero<-tbl_df(rbind.data.frame(Results_W_Stations_Date_Range_QD,ZeroOfZero))

#Remove tossedsamplecount field
ZeroOfZero$TossedSampleCount<-NULL
ZeroOfZero$TotalSamples<-NULL
ZeroOfZero<-tbl_df(ZeroOfZero)


R_W_Date_Range_QD<-tbl_df(anti_join(Results_W_Stations_Date_Range,Quantitation_Discards_No_Range,by=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName"
,"MatrixName","FractionName","BUCODE","Objective_Language","Evaluation_Guideline"
,"Objective_Ref_Number","Eval_Ref_Number")))

#Add the data used field
#Keep this command on ONE line or it will cause issues with the LOE export
R_W_Date_Range_QD$DATA_USED<-paste0("Water Board staff assessed ",R_W_Date_Range_QD$ProjectName," data for ",R_W_Date_Range_QD$Waterbody," to determine beneficial use support and the results are as follows: ",R_W_Date_Range_QD$Exceedances, " of the ",R_W_Date_Range_QD$Total," samples exceeded the evaluation guideline for ",R_W_Date_Range_QD$AnalyteName," .")

#Combine all of the LOEs together
All_LOEs<-tbl_df(rbind.data.frame(R_W_Date_Range_QD,ZeroOfZero))

#Remove unecessary fields
All_LOEs$StationCount<-NULL
All_LOEs$StationCode<-NULL
All_LOEs$MinDate<-NULL
All_LOEs$MaxDate<-NULL

#Change matrix value from sediment to Sediment
All_LOEs$MatrixName<-"Sediment"

#Merge LOEs with the data used ref numbers and qa information then remove the project name field
names(DataReferences)[names(DataReferences)=="ParentProjectName"]<-"ProjectName"
All_LOEs<-tbl_df(merge(All_LOEs,DataReferences,by="ProjectName"))
All_LOEs$ProjectName<-NULL


#Change column names to match formatting required by CalWQA upload tool
colnames(All_LOEs)<-c("POLLUTANT","WB_SEGMENT","WBID","MATRIX","FRACTION","BU_CODE"
,"CRITERION/OBJECTIVE","EVAL_GUIDELINE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE_REFERENCES","EXCEEDANCE_COUNT","SAMPLE_COUNT","SPATIAL_REP","TEMPORAL_REP","DATA_USED"
,"DATA_USED_REFERENCES","QA_INFO_REFERENCES","QA_INFO","DATA_SOURCE")

All_LOEs$AUTHOR<-paste0(Author)


All_LOEs$ENVIRONMENTAL_CONDITIONS<-""
All_LOEs$ASSESSOR_COMMENT<-""
All_LOEs$DATA_TYPE<-"PHYSICAL/CHEMICAL MONITORING"
All_LOEs$SUB_GROUP<-"Pollutant-Sediment"
#Change sub group to Ancillary Line of Evidence if referenc does not have QAPP when it should
All_LOEs$SUB_GROUP[which(All_LOEs$QA_INFO_REFERENCES=="4971"|All_LOEs$QA_INFO_REFERENCES=="5005"|All_LOEs$QA_INFO_REFERENCES=="4686")]<-"Ancillary Line of Evidence"
All_LOEs$DATE_CREATED<-Sys.Date()
All_LOEs$MIGRATION_ID<-rownames(All_LOEs)
All_LOEs$REGION<-Region
if(Region=="5SJ"|Region=="5T"{
  All_LOEs$REGION<-"5"
})
All_LOEs$ASSESSMENT_STATUS<-"LOE In Progress"
All_LOEs$AQ_USE_CODE<-""
All_LOEs$UPDATED_BY<-""
All_LOEs$DATE_UPDATED<-""



All_LOEs<-tbl_df(merge(All_LOEs,ReLEP_to_CalWQA_Lookup,all.x=TRUE))
All_LOEs$POLLUTANT<-NULL
names(All_LOEs)[names(All_LOEs)=="PollutantName_in_CalWQA"]<-"POLLUTANT"


#Re-Order Columns to match LOE uploader template
All_LOEs<-All_LOEs[,c("MIGRATION_ID","REGION","WB_SEGMENT","WBID","POLLUTANT"
,"SUB_GROUP","MATRIX","FRACTION","BU_CODE","AQ_USE_CODE","CRITERION/OBJECTIVE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE","EVAL_GUIDELINE_REFERENCES","SAMPLE_COUNT","EXCEEDANCE_COUNT","SPATIAL_REP","TEMPORAL_REP","QA_INFO"
,"QA_INFO_REFERENCES","DATA_TYPE","DATA_USED","DATA_USED_REFERENCES","ASSESSMENT_STATUS","DATA_SOURCE"
,"AUTHOR","DATE_CREATED","UPDATED_BY","DATE_UPDATED","ENVIRONMENTAL_CONDITIONS")]


#Flag LOEs as electornic generated LOEs
if((!("ASSESSOR_COMMENT"%in% names(All_LOEs)))){
All_LOEs$ASSESSOR_COMMENT<-""
}
All_LOEs$ASSESSOR_COMMENT[All_LOEs$ASSESSOR_COMMENT=="NA"]<-""
All_LOEs$ASSESSOR_COMMENT<-paste0("(LOE written by ReLEP ", ReLEP_Version,") ", All_LOEs$ASSESSOR_COMMENT)


#Write the files to their appropriate folders

#Write LOEs into LOE folder within the ouputs folder
#LOE filename is the name of the filename, the author, and the date the LOEs were written
FileName<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"_R",Region,"_Sediment_LOEs",FileNameInQuotations)

write.table(All_LOEs,FileName,sep="\t",row.names=FALSE)



#The tossed data rows may be duplicated because there were multiple issues that resulted in tossing
#turn this into a unique list with comma separated issues and counts of the number of issues
#this will help with prioritization and tracking...hopefully
#first create a list of tissue ids and the issues associated with them
Issues<-subset(AllBadData,select=c("WQID","Issue"))
Issues<-distinct(select(Issues,WQID,Issue))
Issues<-Issues%>%dplyr::group_by(WQID)%>%dplyr::summarise(count=n(), Issue=paste(Issue,collapse=", "))
names(Issues)<-c("WQID","IssueCount","Issue")

AllBadData$Issue<-NULL
AllBadData<-unique(AllBadData)
AllBadData<-tbl_df(merge(AllBadData,Issues))

AllBadDataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_R",Region,"_All_Tossed_Sediment_Data_R",FileNameInQuotations)

#Then the rows of data that could not be assessed for one reason or another
write.table(AllBadData,AllBadDataFileName,sep="\t",row.names=FALSE)

#Note PCB Table
if(length(PCBDiscards$AnalyteName)>0){
  print("Need to finish PCB discards")
  print("Need to finish PCB discards")
  print("Need to finish PCB discards")
  print("Need to finish PCB discards")
  print("Need to finish PCB discards")
  print("Need to finish PCB discards")
  print("Need to finish PCB discards")
}


