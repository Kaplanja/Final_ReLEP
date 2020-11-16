#________________________________________
# NOTE: Change the working directory both here and at the end of the code to where the data is stored on you PC,
#Change the Author, and
#Create a "Tox Outputs" folder in the working directory prior to running this code.
#________________________________________


#Loads libraries for session use
library(plyr)
library(dplyr)
library(lubridate)
#library(readxl)
#library(rio)
#library(stringr)
library(data.table)


#Sets path to use Rtools to zip up xls/xlsx file with rio
#Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")


#________________________________________
# NOTE #) in the comment correlates to the step in the Toxicity Steps Document by J. Kaplan
#________________________________________


#Removes all defined varibles
rm(list=ls(all=TRUE)) 


#Enter the region number to the right of the Region, your username in quotations as it appears when you login
#to your computer to the right of the Author, and the file name of the data you are trying to assess to the right
#of "EnterFileNameHere"

Region<-"5SJ"

Author<-"jkaplan"

ReLEP_Version<-"Version 1.1"

EnterDataFileNameHere<-"Toxicity_R5.txt"

#Create working directory based on author and region
WorkingDirectory<-paste0("C:\\Users\\",Author,"\\Desktop\\Final_ReLEP\\Region ",Region,"\\Toxicity_Module\\")
setwd(WorkingDirectory)

#Set the directory so that relative paths work for loading the files
#setwd("C:\\Users\\Jkaplan\\Desktop\\Final_ReLEP\\Region 1\\Toxicity_Module\\")

BeneficialUseTable<-paste0("R",Region,"_Beneficial_Use_FINAL.txt")
SitesTable<-paste0("R",Region,"_Sites_FINAL.txt")
DataUsedTable<-paste0("R",Region,"_Data_Used_DRAFT.txt")




# 1) Import data
data_imported <- tbl_df(read.delim(EnterDataFileNameHere, sep="\t",header=TRUE, stringsAsFactors=FALSE))
data_imported <- tbl_df(data_imported)
names(data_imported)[names(data_imported)=="Latitude"]<-"TargetLatitude"
names(data_imported)[names(data_imported)=="Longitude"]<-"TargetLongitude"
data_exported<-data_imported
Beneficial_Uses <- tbl_df(read.delim(BeneficialUseTable, sep= "\t", header=TRUE))
Beneficial_Uses<-subset(Beneficial_Uses,select=c(Waterbody,WBID,Wbtype,BeneficialUse))
Sites <- tbl_df(read.delim(SitesTable, sep= "\t", header=TRUE,stringsAsFactors=FALSE))
Sites<-Sites[Sites$STATUS=="Completed",]
Sites<-subset(Sites,select=c(Waterbody,StationCode,WBID))
Data_Used<-tbl_df(read.delim(DataUsedTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Data_Used<-subset(Data_Used,select=c(ParentProjectName,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO,DATA_SOURCE,Assess))
Data_Used<-Data_Used[Data_Used$Assess=="Yes",]
Toxicity_Bacteria_Language<-tbl_df(read.delim("Toxicity_Bacteria_Language.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE))


# 2) Split out all samples that do not have a SigEffectCode or otherwise did not pass QA
Missing_QA <- filter(data_exported, (SigEffectCode=='NA'|SigEffectCode==''|SigEffectCode==' '|SigEffectCode=="NULL"))
Missing_QA$Issue<-"Missing Sig Effect Code"
AllExportedData<-Missing_QA
PoorQA<-filter(data_exported,(DataQuality!="Passed QC"))
PoorQA$Issue<-"Check QA Code"
AllExportedData<-rbind.data.frame(AllExportedData,PoorQA)


# 3) Split out all samples with questionable ComplianceCode. WHAT CODES ARE BAD of Com, NA, NR, Qual?
Review_Test_Procedures <- filter(data_exported, (ComplianceCode=='NA'| ComplianceCode==''| ComplianceCode==' '| ComplianceCode=="Qual"))
Review_Test_Procedures$Issue<-"Review the test procedures to determine if data should be used"
AllExportedData<-rbind.data.frame(AllExportedData,Review_Test_Procedures)

#>>> 4) Split out all samples with a StatMethod other than T-test<<<<<<<<<<<<<<<<
####>>>This step was removed as per direction from Brian Ogg<<<<<<<<<<<<<<<<<<<<<
####>>>He stated that the test perofomed is irelevant<<<<<<<<<<<<<<<<<<<<<<<<<<<<
####>>>The more imporant factor is weather or not the data has a SigEffect Code<<
####>>>Cuting out data missing sig effect does is taken care of in step #2<<<<<<<


#Import data now filtered out MIssing_QA, bad ComplianceCodes, and included T-test
data_imported <- filter(data_imported, !(SigEffectCode=='NA'|SigEffectCode==''|SigEffectCode==' '|SigEffectCode=="NULL"|ComplianceCode=='NA'| ComplianceCode==''| ComplianceCode==' '| ComplianceCode=="Qual"))
data_imported<-filter(data_imported,(DataQuality=="Passed QC"))



# 5) Set all SigEffectCode NOT SL to 0:NOTE to get thtis to work data needed to be imported with 
#stringsAsFactors=FALSE and set to all not SL to zero had to happen first


# 5) Set all SigEffectCode of SL to 1
data_imported$SigEffectCode[data_imported$SigEffectCode =="SL"|data_imported$SigEffectCode=="SL_0.01"] <- 1

data_imported$SigEffectCode[data_imported$SigEffectCode !=1] <- 0


# 6) Add the beneficial uses to the waterbodies table
Waterbodies<-merge(Sites,Beneficial_Uses,by=c("Waterbody","WBID"))


# 6) Add waterbody names to the data
W_Waterbodies<-tbl_df(merge(data_imported,Waterbodies,by="StationCode"))


#________________________________________
#BenUse, WBID, and Waterbody are now in W_Waterbodies table
#________________________________________


# 6) Split out Missing_Stations
Missing_Stations<-anti_join(data_exported,Waterbodies,by="StationCode")
Missing_Stations$Issue<-"Station does not have waterbody information"
AllExportedData<-rbind.data.frame(AllExportedData,Missing_Stations)



# 7) Sample Date, Station Code, Matrix combination is a sample we ignore species for this assessment
#If any species exceeds for any reason on any given day, toxicity is considered to be present
W_Waterbodies$SigEffectCode <- as.numeric(W_Waterbodies$SigEffectCode)
Counts<-tbl_df(as.data.table(W_Waterbodies)[,max(SigEffectCode),list(SampleDate,StationCode,Waterbody,WBID,MatrixName,ParentProject)])
names(Counts)[names(Counts)=="V1"]<-"Counts"


# 8) Create table with comma seperated values like stations listed in step 6 for Waterbody, Matrix, ProjectName, and species
#Create a new table of station codes, waterbodies, analyte, and count of stations
####I think what you need here is DW_Analyte included
#So the text in the LOEs data used secton ends up being
#Hayella Azteca was assesed for "Reproduction"
#The "Reproduction" is the ToxPointAnalyteName
#We dont need the count of the ToxPointAnalyteName
#I was trying to figure out how to include this, but was struggling
#Can you figure it out for me?
W_Waterbodies_4_Species<-W_Waterbodies
W_Waterbodies_4_Species$Species<-paste0(W_Waterbodies_4_Species$OrganismName,", for ",W_Waterbodies_4_Species$ToxPoint_AnalyteName)
Species<-subset(W_Waterbodies_4_Species,select=c("ParentProject","StationCode", "Waterbody","WBID", "MatrixName", "Species"))
Species<-distinct(select(Species,ParentProject,StationCode, Waterbody, WBID, MatrixName, Species))
Species<-Species%>%dplyr::group_by(ParentProject,StationCode, Waterbody, WBID, MatrixName)%>%dplyr::summarise(count=n()
                          , (Organism_And_Test_Name=paste(Species,collapse=", ")))
names(Species)<-c("ParentProject", "StationCode","Waterbody", "WBID", "MatrixName","SpeciesCount", "OrganismName")


# 9) Count up the number of excedances and samples based on Waterbody, WBID, ParentProject

#Count Exceedances  
Exceedances<-tbl_df(as.data.table(Counts)[,sum(Counts==1),list(StationCode,Waterbody,WBID,ParentProject,MatrixName)])
names(Exceedances)[names(Exceedances)=="V1"]<-"Exceedances"


#Count Total samples
Total<-tbl_df(as.data.table(Counts)[,sum(Counts<=1),list(StationCode,Waterbody,WBID,ParentProject,MatrixName)])
names(Total)[names(Total)=="V1"]<-"TotalSamples"

#Merge Excedances and Total
Samples<-tbl_df(merge(Exceedances,Total,by=c("StationCode","Waterbody", "WBID", "ParentProject", "MatrixName")))



# 10) Create a new table of station codes, waterbodies, matrix, and count of stations for use 
#in LOE writing for stations to come in comma separated form for in sentence being written.
#Stations<-subset(Counts,select=c("MatrixName","ParentProject","StationCode","Waterbody", "WBID"))
#Stations<-distinct(select(Stations,MatrixName,ParentProject,Waterbody,WBID,StationCode))
#Stations<-Stations%>%group_by(MatrixName,ParentProject,Waterbody, WBID)%>%summarise(count=n(), StationCode=paste(StationCode,collapse=", "))
#names(Stations)<-c("MatrixName","ParentProject","Waterbody", "WBID","StationCount","StationCodes")
                                                                    

# 11) Calculate range of dates over which data was collected

# Calculate maximum and minimum date for each waterbody with data
W_Waterbodies$SampleDate<-as.Date(W_Waterbodies$SampleDate, "%m/%d/%Y")

min_date<-tbl_df(as.data.table(W_Waterbodies)[,min(SampleDate),list(StationCode,Waterbody,WBID,MatrixName,ParentProject)])
names(min_date)[names(min_date)=="V1"]<-"Minimum_Date"
max_date<-tbl_df(as.data.table(W_Waterbodies)[,max(SampleDate),list(StationCode,Waterbody,WBID,MatrixName,ParentProject)])
names(max_date)[names(max_date)=="V1"]<-"Maximum_Date"


# Combine results with max a min date range
Samples_w_Dates<-merge(Samples,min_date,by=c("StationCode","Waterbody", "WBID", "MatrixName", "ParentProject"))
Samples_w_Dates<-merge(Samples_w_Dates,max_date,by=c("StationCode","Waterbody", "WBID", "MatrixName", "ParentProject"))


# 12) Write LOE

#Merge Samples with Stations
#Samples_w_Stations<-merge(Samples_w_Dates,Stations,by=c("Waterbody", "WBID","MatrixName","ParentProject"))
Samples_w_Stations<-Samples_w_Dates
Samples_w_Stations$StationCount<-1

#Merge Samples with Species
Samples_W_Species<-tbl_df(merge(Samples_w_Stations,Species,by=c("StationCode","Waterbody", "WBID", "ParentProject", "MatrixName")))

#Change "samplewater" and "overlyingwater" to "water"
Samples_W_Species$MatrixName[Samples_W_Species$MatrixName=='samplewater']<-'Water'
Samples_W_Species$MatrixName[Samples_W_Species$MatrixName=='overlyingwater']<-'Water'
Samples_W_Species$MatrixName[Samples_W_Species$MatrixName=='sediment']<-'Sediment'


Samples_w_BU<-tbl_df(merge(Samples_W_Species,Beneficial_Uses,by=c("Waterbody", "WBID")))
Samples_w_BU$BeneficialUse<-as.character(Samples_w_BU$BeneficialUse)
Samples_w_BU$Wbtype<-NULL
Samples_w_BU$POLLUTANT<-"Toxicity"
Samples_w_BU$SampleDate<-NULL
Samples_w_BU<-filter(Samples_w_BU,(BeneficialUse=="CO"|BeneficialUse=="WA"|BeneficialUse=="ES"|BeneficialUse=="MA"))


#Lookup objective information
Samples_w_BU$Region<-substr(Samples_w_BU$WBID,4,4)
if(Region=="5SJ"|Region=="5T"){
Samples_w_BU$Region<-Region
}

Samples_w_BU$AnalyteName<-Samples_w_BU$POLLUTANT
Samples_w_BU<-tbl_df(merge(Samples_w_BU,Toxicity_Bacteria_Language))
Samples_w_BU$Region<-NULL
Samples_w_BU$AnalyteName<-NULL

#Final dataframe for language and all
Sample_LOEs<-Samples_w_BU

#Input SPATIAL_REP
Sample_LOEs$SPATIAL_REP<-paste("The samples were collected at", Samples_w_BU$StationCount, "station. Monitoring site: (",Samples_w_BU$StationCode,")")

#Input DATA_USED
Sample_LOEs$DATA_USED<-paste(Sample_LOEs$Exceedances,"of the", Sample_LOEs$TotalSamples, "samples collected by", Sample_LOEs$ParentProject," for ",Sample_LOEs$Waterbody, "exhibited toxicity.  A sample may have multiple toxicity test results, but will only be counted once.  A sample is defined as being collected on the same day, at the same location with the same lab sample ID (if provided). The following organisms and parameters were utilized for the toxicity tests: ", Sample_LOEs$OrganismName)

#Input TEMPORAL_REP
Sample_LOEs$TEMPORAL_REP<-paste0("The samples were collected between the dates of ", Samples_w_BU$Minimum_Date, " and ", Samples_w_BU$Maximum_Date, ".")

#Input ASSESSMENT_STATUS
Sample_LOEs$ASSESSMENT_STATUS<-paste("LOE In Progress")

#Input DATE_CREATED
Sample_LOEs$DATE_CREATED<-Sys.Date()


Sample_LOEs$AUTHOR<-Author

#Input MIGRATION_ID
Sample_LOEs$MIGRATION_ID<-rownames(Sample_LOEs)

#Input REGION
Sample_LOEs$REGION<-Region
if(Region=="5SJ"|Region=="5T"){
  Sample_LOEs$REGION<-"5"
}

#Input MATRIX
Sample_LOEs$MATRIX<-paste0(Samples_w_BU$MatrixName)

#Input ENVIRONMENTAL_CONDITIONS
Sample_LOEs$ENVIRONMENTAL_CONDITIONS<-""

#Input ASSESSOR_COMMENT
Sample_LOEs$ASSESSOR_COMMENT<-""



#Lookup data used references and QA information with the Data used table
names(Sample_LOEs)[names(Sample_LOEs)=="ParentProject"]<-"ParentProjectName"
Sample_LOEs<-tbl_df(merge(Sample_LOEs,Data_Used,by="ParentProjectName"))

#Sub_group
Sample_LOEs$SUB_GROUP<-"Toxicity"
#Change sub group to Ancillary Line of Evidence if referenc does not have QAPP when it should
Sample_LOEs$SUB_GROUP[which(Sample_LOEs$QA_INFO_REFERENCES=="4971")]<-"Ancillary Line of Evidence"


#Flag data missing Data used info
MissingDataUsed<-data_exported
names(Data_Used)[names(Data_Used)=="ParentProjectName"]<-"ParentProject"
MissingDataUsed<-anti_join(MissingDataUsed,Data_Used)
MissingDataUsed$Issue<-"ParentProjectName Missing from DataUsed table or marked as do not assess."
AllExportedData<-rbind.data.frame(AllExportedData,MissingDataUsed)


###########
#Data Type
Sample_LOEs$DATA_TYPE<-"TOXICITY TESTING"

#Fraction
Sample_LOEs$FRACTION<-"None"


#Clear unneeded columns and rename columns to match LOE uploader template for CASSM
Sample_LOEs$Project<-NULL
Sample_LOEs$StationCount<-NULL
Sample_LOEs$StationCode<-NULL
Sample_LOEs$SpeciesCount<-NULL
Sample_LOEs$OrganismName<-NULL
Sample_LOEs$Minimum_Date<-NULL
Sample_LOEs$Maximum_Date<-NULL
Sample_LOEs$MatrixName<-NULL
Sample_LOEs$ParentProjectName<-NULL
names(Sample_LOEs)[names(Sample_LOEs)=="Waterbody"]<-"WB_SEGMENT"
names(Sample_LOEs)[names(Sample_LOEs)=="Exceedances"]<-"EXCEEDANCE_COUNT"
names(Sample_LOEs)[names(Sample_LOEs)=="TotalSamples"]<-"SAMPLE_COUNT"
names(Sample_LOEs)[names(Sample_LOEs)=="Objective_Language"]<-"CRITERION/OBJECTIVE"
names(Sample_LOEs)[names(Sample_LOEs)=="Objective_Ref_Number"]<-"CRITERION/OBJECTIVE_REFERENCES"
names(Sample_LOEs)[names(Sample_LOEs)=="Evaluation_Guideline"]<-"EVAL_GUIDELINE"
names(Sample_LOEs)[names(Sample_LOEs)=="Eval_Ref_Number"]<-"EVAL_GUIDELINE_REFERENCES"
names(Sample_LOEs)[names(Sample_LOEs)=="BeneficialUse"]<-"BU_CODE"
Sample_LOEs$ASSESSOR_COMMENT<-"NA"
Sample_LOEs$AQ_USE_CODE<-""
Sample_LOEs$UPDATED_BY<-""
Sample_LOEs$DATE_UPDATED<-""


#Re-Order Columns to match LOE uploader template
Sample_LOEs<-Sample_LOEs[,c("MIGRATION_ID","REGION","WB_SEGMENT","WBID","POLLUTANT"
,"SUB_GROUP","MATRIX","FRACTION","BU_CODE","AQ_USE_CODE","CRITERION/OBJECTIVE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE","EVAL_GUIDELINE_REFERENCES","SAMPLE_COUNT","EXCEEDANCE_COUNT","SPATIAL_REP","TEMPORAL_REP","QA_INFO"
,"QA_INFO_REFERENCES","DATA_TYPE","DATA_USED","DATA_USED_REFERENCES","ASSESSMENT_STATUS","DATA_SOURCE"
,"AUTHOR","DATE_CREATED","UPDATED_BY","DATE_UPDATED","ENVIRONMENTAL_CONDITIONS","ASSESSOR_COMMENT")]



#Make unique list with count of issues for each row of data
Issues<-AllExportedData%>%dplyr::group_by(ToxID,ProgramCode,Program,ParentProject
,ProjectCode,Project,ProjectDescr,QAPPCode,QAPPName,PublicRelease
,StationName,StationCode,StationDescr,CoordinateSource,TargetLatitude
,TargetLongitude,Datum,UnitElevation,Elevation,SWRCBWatTypeCode
,DWC_WaterBodyType,RegionalBoard,SampleDate,CollectionTime,LocationCode
,CollectionDepth,UnitCollectionDepth,SampleTypeCode,CollectionReplicate
,LabReplicate,ToxBatch,LabSampleID,MatrixName,MethodName,ToxTestDurCode
,OrganismName,Analyte,Unit,WQSource,TimePointName,Result,ResQualCode
,ToxResultQACode,QACode,BatchVerificationCode,ComplianceCode,SampleComments
,CollectionComments,ToxTestComments,TimePointComments,ToxResultComments
,ToxBatchComments,EventCode,ProtocolCode,SampleAgency,CollectionMethodName
,CollectionDeviceDescr,PositionWaterColumn,TIENarrative,Dilution,ToxPointMethod
,Treatment,UnitTreatment,TreatmentConcentration,LabAgency,SubmittingAgency
,ToxBatchStartDate,RefToxBatch,OrganismAgeAtTestStart,LabSubmissionCode
,OccupationMethod,StartingBank,DistanceFromBank,UnitDistanceFromBank,StreamWidth
,UnitStreamWidth,StationWaterDepth,UnitStationWaterDepth,Hydromod,HydromodLoc
,LocationDetailWQComments,PctControl,RepCount,Mean,StdDev,StatMethod,CalcValueType
,AlphaLevel,EvalThreshold,MSD,SigEffectCode,CalculatedValue,PercentEffect,SampleID
,ToxPoint_AnalyteName,bValue,CritcalValue,TestExposureType,DataQuality
,DataQualityIndicator)%>%dplyr::summarise(count=n(),Issue=paste(Issue,collapse=", AND "))


#Flag LOEs as electornic generated LOEs
Sample_LOEs$ASSESSOR_COMMENT[Sample_LOEs$ASSESSOR_COMMENT=="NA"]<-""
Sample_LOEs$ASSESSOR_COMMENT<-paste0("(LOE written by ReLEP ",ReLEP_Version,") ", Sample_LOEs$ASSESSOR_COMMENT)


FileName<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"_R",Region,"_LOEs_",EnterDataFileNameHere)
ExportedDataFileName<-paste0("Outputs\\TossedData_",Author,"_",Sys.Date(),"_R",Region,"_TossedData_",EnterDataFileNameHere)

#Write out all "missing" tables
write.table(Issues, ExportedDataFileName,sep="\t",row.names=FALSE,na="")

#write.table(Review_Test_Procedures, "Review_Test_Procedures.txt",sep="\t",row.names=FALSE)
#write.table(Wrong_Stat_Test, "Wrong_Stat_Test.txt",sep="\t",row.names=FALSE)
#write.table(Missing_Stations, "Missing_Stations.txt",sep="\t",row.names=FALSE)

write.table(Sample_LOEs, FileName,sep="\t",row.names=FALSE)





