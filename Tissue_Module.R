#######################################################################################################
#######################-----------------------------------------#######################################
#######################_______________Ignore____________________#######################################

#This section loads nexeccary pagages for assessment
#try not to add unecessary packages here because
#sometimes packages conflict with eachother
#or they will mask the ommands and cause unpredicable behavior in the code

library(dplyr,warn.conflicts=TRUE)
library(data.table, warn.conflicts=TRUE)
library(lubridate,warn.conflicts=TRUE)


#Removes all defined variables this is only imporant when you run the
#code multiple times in one session.  it removes previously defined variables
rm(list=ls(all=TRUE))

#saves system time to display how long the code takes to run at the end
Start_Time<- Sys.time()

######################_____________End Ignore___________________######################################
######################------------------------------------------######################################
######################################################################################################

#       _______.___________.    ___      .______     .___________.
#      /       |           |   /   \     |   _  \    |           |
#     |   (----`---|  |----`  /  ^  \    |  |_)  |   `---|  |----`
#      \   \       |  |      /  /_\  \   |      /        |  |     
#  .----)   |      |  |     /  _____  \  |  |\  \----.   |  |     
#  |_______/       |__|    /__/     \__\ | _| `._____|   |__|     
  
####################--------Instructions Below------------------######################################

#change the name of the file in the "Load Data Set" command to match the file you want assessed
#Change the working directory file pat command at the bottom to match the location of the output 
#folder on your computer once the program has been run check the output folder for the LOEs and 
#the missing rows files the missing rows contain the data that could not run through the program 
#because of mismatched names (station, analyte, waterbody, etc.)check those files to make sure 
#that all data that should have been assessed WAS assessed



######################################################################################
######################################################################################
######################################################################################
#                                                                                    #
#       The portion below should be the only things you need to change to            #
#      get the program to run.  Enter your Region number and the username you        #
#              log into your computer with                                           #
#                                                                                    #
###################################################################################################################
###################################################################################################################
###################################################################################################################
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
                EnterYourNameHereInQuotations<- "jkaplan"                                                         #
#                                                                                                                 #
                                Region<-"9"                                                                        #
#                                                                                                                 #
               FileNameInQuotations = "Tissue_R9.txt"                                                  #
#                                                                                                                 #
			                ReLEP_Version<-"Version 1.1"
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
###################################################################################################################
###################################################################################################################
###################################################################################################################



###################################################################################################################
#                                                                                                                 #
#                    This section creates the table names based on the Region being assessed                      #
#                                       Dont change anything in this section                                      #
#                                                                                                                 #
###################################################################################################################
#                                                                                                                 #
#                                                                                                                 #
    Author<-EnterYourNameHereInQuotations                                                                         #
#                                                                                                                 #
    SitesTable<-paste0("R",Region,"_Sites_FINAL.txt")                                                             #
#                                                                                                                 #
    BeneficialUseTable<-paste0("R",Region,"_Beneficial_Use_FINAL.txt")                                            #
#                                                                                                                 #
    ObjectivesTable<-paste0("R",Region,"_Analytes_TIS_FINAL.txt")                                                 #
#                                                                                                                 #
    SummingPollutantsTable<-"SummingPollutants.txt"                                                               #
#                                                                                                                 #
    PAHTable<-"PAH_TEFs.txt"                                                                                      #
#                                                                                                                 #
    DataReferencesTable<-paste0("R",Region,"_Data_Used_DRAFT.txt")                                          #
#                                                                                                                 #
    TissueSpecies<-"TissueSpecies.txt"                                                                            #
#                                                                                                                 #
    SelfNamed<-"Self_Named_Summing_Pollutants.txt"                                                                #
#                                                                                                                 #
    AcceptableTissue<-"Acceptable_Tissue_Types.txt"                                                               #
#                                                                                                                 #
    Site_Specific_Objectives<-paste0("R",Region,"_Site_Specifics_FINAL.txt")
#                                                                                                                 #
###################################################################################################################
#                                                                                                                 #
#_____________________________________________End of Previous Section_____________________________________________#
#                                                                                                                 #
###################################################################################################################
###################################################################################################################
#                                                                                                                 #
#                              Set the directory so that relative paths work for loading the files                #
#                                       Dont change anything here either!                                         #
#                                                                                                                 #
###################################################################################################################
#                                                                                                                 #
     WorkingDirectory<-paste0("C:\\Users\\",Author,"\\Desktop\\Final_ReLEP\\Region ",Region,"\\Tissue_Module\\")  #
#                                                                                                                 #
                                          setwd(WorkingDirectory)                                                 #
#                                                                                                                 #
###################################################################################################################


							   
# Load Region 2 Data file, remove unecessary fields, remove J Flagged samples, and convert to tbl_df
Tissue_data<-tbl_df(read.delim(FileNameInQuotations,header=TRUE,stringsAsFactors=FALSE))
Tissue_data$SampleDate<-mdy(Tissue_data$SampleDate)
names(Tissue_data)[names(Tissue_data)=="Latitude"]<-"TargetLatitude"
names(Tissue_data)[names(Tissue_data)=="Longitude"]<-"TargetLongitude"
#names(Tissue_data)[names(Tissue_data)=="Analyte"]<-"AnalyteName"

#Change ParentProjectName to ProjectName because I coded the program with project name
#and it is easier to change it in the field here, than it is to go back through the entire program and add parent infront of 
#every single time project name appears in the code.  Someone should go back and change this though.  It will result in a better program
Tissue_data$ProjectName<-Tissue_data$ParentProjectName

#only tissue data moves on beyond this point
Tissue_data<-Tissue_data[Tissue_data$Matrix=="tissue",]


#Save Tissue_data with all fidelds in case you need it later
Tissue_Data<-Tissue_data


#Create table of data with bad QA Codes to be exported and reviewed
	BadQACodes<-filter(Tissue_Data,DataQuality!="Passed QC")
	BadQACodes$Issue<-"Check DataQuality Field"
	AllTossedData<-BadQACodes


#Add data with results < 0 and ResQualCode of "=" to tossed data
LessThanZero<-Tissue_Data[which(Tissue_Data$ResQualCode=="="&Tissue_Data$Result<0),]
LessThanZero$Issue<-"Result less than Zero AND ResQualCode of '=' Check data for errors"
AllTossedData<-rbind.data.frame(AllTossedData,LessThanZero)

#Remove data with results < 0 and ResQualCode of "=" to tossed data
Tissue_data<-Tissue_data[which(!(Tissue_data$ResQualCode=="="&Tissue_data$Result<0&!is.na(Tissue_data$Result))),]



#Now do the opposite command on the data to get the remaining rows of data with acceptable QACodes
Tissue_data<-filter(Tissue_data,DataQuality=="Passed QC")


#Change all negative results to be equal to zero
#The negative numbers negative MDLs which means the samples are either clean
#or they should be thrown out due to quantitation but this depents on the objective uesed
#Either way,zero is a more appropriate reuslt than negative

Tissue_data$Result[which(Tissue_data$Result<0 & (Tissue_data$ResQualCode=="ND"|Tissue_data$ResQualCode=="DNQ"))]<-0


	WrongSampleType<-subset(Tissue_Data,!(SampleTypeCode=="Grab"|SampleTypeCode=="Integrated"|SampleTypeCode=="Composite"))
	WrongSampleType$Issue<-"WrongSampleType"
	AllTossedData<-rbind.data.frame(AllTossedData,WrongSampleType)



#Remove SampleTypes that are related to QA or otherwise inappropriate for use in assessment
Tissue_data<-subset(Tissue_data,(SampleTypeCode=="Grab"|SampleTypeCode=="Integrated"|SampleTypeCode=="Composite"))



	#Remove data with bad res qual codes
	MissingResQualCode<-filter(Tissue_Data, ((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode)|ResQualCode=="ND"|ResQualCode=="DNQ")&(RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))
	MissingResQualCode$Issue<-"No ResQualCode"
	AllTossedData<-rbind.data.frame(AllTossedData,MissingResQualCode)

#Remove data with no res qual code, RL AND MDL
Tissue_data<-filter(Tissue_data, !((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode)|ResQualCode=="ND"|ResQualCode=="DNQ")&(RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))

#Multiply MDL by 3.18 to get RL if RL is missing as per quantitaiton guidance
Tissue_data$RL[which((is.na(Tissue_data$RL)|Tissue_data$RL==""|Tissue_data$RL==" ")&(Tissue_data$MDL!="NA"|Tissue_data$MDL!=""|Tissue_data$MDL!=
" "))]<-Tissue_data$MDL[which((is.na(Tissue_data$RL)|Tissue_data$RL==""|Tissue_data$RL==" ")&(Tissue_data$MDL!="NA"|Tissue_data$MDL!=""|Tissue_data$MDL!=" "))]*3.18


#Rem Commove fields that are not needed from this point on
#I used FinalID instead of CommonName here because some CommonNames in CEDEN are blank at this point,
#but all FinalIDs are filled in. onNames are looked up later in the program
Tissue_data<-select(Tissue_data,TisID,ProjectName,StationCode,ComplianceCode,SampleDate,Analyte,Matrix,Unit,Result,TissueName,TargetLatitude,TargetLongitude,Method,FinalID,ResQualCode,MDL,RL,QACode,TLAvgLength_mm,NumberFishperComp,CompositeCompositeID)

####################################################################################################################
#########################OTHER TABLE LOADING SECTION################################################################


# Load sites table and convert to tbl_df
Sites<-read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE)
Sites<-Sites[Sites$STATUS=="Completed",]
Sites<-subset(Sites,select=c(Waterbody,WBID,StationCode))
Sites<-tbl_df(Sites)

# Load Beneficial Uses table and convert to tbl_df
#you must remove blank rows (rows with waterbodies but no WBIDs, or BUs)
#Before loading the table here or else it will skip rows sometimes
#Also, the program cannot read the NA BU right now.  This is an easy fix, but I have yet to run into any
#Objective that actually used the NA BU.
Beneficial_Uses<-read.delim(BeneficialUseTable,sep="\t",header=TRUE,stringsAsFactors=FALSE)
#Beneficial_Uses<-na.omit(Beneficial_Uses)
Beneficial_Uses<-tbl_df(Beneficial_Uses)
Beneficial_Uses<-subset(Beneficial_Uses,select=c(Waterbody,WBID,Wbtype,BeneficialUse))
Beneficial_Uses$BUCODE<-Beneficial_Uses$BeneficialUse
Beneficial_Uses$BeneficialUse<-NULL
Beneficial_Uses$BUCODE<-as.character(Beneficial_Uses$BUCODE)

#Get list of waerbodies that have CM assigned to generate a list of waterbodies that DONT have CM assigned
#This list will be used to ensure all tissue data is assessed for the CM bu as a minimum
WBs_W_CM_BU<-Beneficial_Uses[which(Beneficial_Uses$BUCODE=="CM"),]
WBs_W_CM_BU$BUCODE<-NULL
WBs_W_CM_BU<-unique(WBs_W_CM_BU)
WBs_WO_CM_BU<-anti_join(Beneficial_Uses,WBs_W_CM_BU)
WBs_WO_CM_BU$BUCODE<-NULL
WBs_WO_CM_BU<-unique(WBs_WO_CM_BU)
WBs_WO_CM_BU$BUCODE<-"CM"

#Add the newly created list of WB's "with" CM to the list of true beneficial uses
Beneficial_Uses<-rbind.data.frame(Beneficial_Uses,WBs_WO_CM_BU)


#Create waterbody table
Waterbodies<-tbl_df(merge(Sites,Beneficial_Uses,by=c("Waterbody","WBID")))
SitesNoWaterbodies<-anti_join(Sites,Beneficial_Uses)

# Load Region 6 objecives table and convert to tbl_df
Analytes<-tbl_df(read.delim(ObjectivesTable,header=TRUE,stringsAsFactors=FALSE))
#The NA omit command that follows may be a problem because the code for one of the 
#beneficial uses is NA and so I think the code will remove it.  There should be come 
#workaround for this.
#Analytes<-na.omit(Analytes)
Analytes<-subset(Analytes,select=c(AnalyteName,UnitName,BUCODE,Fraction,Objective,AveragingPeroid
                                   ,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
                                   ,Eval_Ref_Number))
Analytes$Analyte<-Analytes$AnalyteName
Analytes$AnalyteName<-NULL
Analytes$Unit<-Analytes$UnitName
Analytes$UnitName<-NULL


#Change column names of analyte table to match the names from the water module
#colnames(Analytes)<-c("Analyte","Unit","BUCODE","Fraction","Objective","Objective_Language"
#,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")
Analytes<-tbl_df(Analytes)

#Each AnalyteName may apprear multiple times, but each row should be unique
SummingPollutants<-read.delim(SummingPollutantsTable,header=TRUE,stringsAsFactors=FALSE)
SummingPollutants<-tbl_df(SummingPollutants)
SummingPollutants<-SummingPollutants[SummingPollutants$Matrix=="Tissue",]
SummingPollutants<-subset(SummingPollutants,select=c(AnalyteName,SummingName))
names(SummingPollutants)[names(SummingPollutants)=="AnalyteName"]<-"Analyte"

#Load the table of "Self Named" summing pollutants
#This is a table of pollutant names that appear in both their raw data form, and summed form
#For example, chlordane is a part of the sum of "Chlordanes"
#we must use this table to remove these pollutants from the main data table after the summing pollutants have been split out
#Failing to do so would result in duplicated results for these analytes because we split out summing pollutants by creating a copy of the rows
#in a new table sum them with their associated analytes, change the name, and then stack them onto the bottom of the main table
#The reason we create a copy instead of a straight split, is because the rest of the summing pollutants that are not "self named"
#will either have their own evaluation guideline separate from the sum that also needs to be assessed, or
#they will not, in which case they will be removed from the main data table when we look up objectives
#Anyway, without further adeu

Self_Named_Summing_Pollutants<-tbl_df(read.delim(SelfNamed,header=TRUE,stringsAsFactors=FALSE))
names(Self_Named_Summing_Pollutants)[names(Self_Named_Summing_Pollutants)=="AnalyteName"]<-"Analyte"

#Contains CalWQA ref numbers for the data set and QAPPs
DataReferences<-read.delim(DataReferencesTable,header=TRUE,stringsAsFactors=FALSE)
DataReferences<-tbl_df(DataReferences)
DataReferences<-subset(DataReferences,select=c(ParentProjectName,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO,DATA_SOURCE,Assess))
names(DataReferences)[names(DataReferences)=="ParentProjectName"]<-"ProjectName"
DataReferences<-DataReferences[DataReferences$Assess=="Yes",]
DataReferences$Assess<-NULL

	#Check to make sure all projects are in data used table
	MissingQAPP<-tbl_df(anti_join(Tissue_Data,DataReferences))
	MissingQAPP$Issue<-"Project not in Data References table of flagged as do not assess."
	AllTossedData<-rbind.data.frame(AllTossedData,MissingQAPP)


#Load PAH_TEF table
PAH_TEFs<-read.delim(PAHTable,header=TRUE,stringsAsFactors=FALSE)
PAH_TEFs<-tbl_df(PAH_TEFs)
names(PAH_TEFs)[names(PAH_TEFs)=="AnalyteName"]<-"Analyte"

#Add acceptable tissue types table
Acceptable_Tissue<-tbl_df(read.delim(AcceptableTissue,header=TRUE,stringsAsFactors=FALSE))

#load table of "edible" fish
Eatable_Fish<-tbl_df(read.delim(TissueSpecies,header=TRUE,stringsAsFactors=FALSE))
#Filter out non eatable fish
Eatable_Fish<-filter(Eatable_Fish,Assess=="Yes")
#Remove unecessary Fields
Eatable_Fish<-subset(Eatable_Fish,select=c(FinalID,Fraction))


#Load trophic level table
TrophicLevel<-tbl_df(read.delim(TissueSpecies,header=TRUE,stringsAsFactors=FALSE))
TrophicLevel<-filter(TrophicLevel,TrophicLevel!="")
TrophicLevel<-subset(TrophicLevel,select=c(FinalID,TrophicLevel))


#Create CommonName Lookup from TL table
CommonNameLookup<-tbl_df(read.delim(TissueSpecies,header=TRUE,stringsAsFactors=FALSE))
CommonNameLookup<-subset(CommonNameLookup,select=c(CommonName,FinalID))



#Contains conversion of ReLEP AnalyteName to CalWQA names
ReLEP_to_CalWQA_Lookup<-tbl_df(read.delim("ReLEP_to_CalWQA_Lookup.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE))
names(ReLEP_to_CalWQA_Lookup)[names(ReLEP_to_CalWQA_Lookup)=="ReLEP_AnalyteName"]<-"POLLUTANT"

#Legal Size limits for fish
LegalSizeLimits<-tbl_df(read.delim("Fishing_Regulations.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE))
LegalSizeLimits<-subset(LegalSizeLimits,select=c("FinalID","Waterbody","WBID","MinLength_mm","MaxLength_mm"))

#Convert -1 to NA
#Done this way to avoid messing with data types once the table is loaded
LegalSizeLimits$MinLength_mm[which(LegalSizeLimits$MinLength_mm==-1)]<-NA
LegalSizeLimits$MaxLength_mm[which(LegalSizeLimits$MaxLength_mm==-1)]<-NA

#Load Site Speficics Table
SSOs<-tbl_df(read.delim(Site_Specific_Objectives,sep="\t",header=TRUE,stringsAsFactors=FALSE))
SSOs$Waterbody<-NULL
names(SSOs)[names(SSOs)=="AnalyteName"]<-"Analyte"
SSOs<-SSOs[which(SSOs$Units=="mg/Kg"|SSOs$Units=="mg/kg"),]

#Create SSO table for Tissue
W_SSOs<-tbl_df(merge(Tissue_Data,Waterbodies))
W_SSOs<-tbl_df(merge(W_SSOs,SSOs))
W_SSOs_For_Export<-W_SSOs
W_SSOs_For_Export$BeneficialUse<-NULL
W_SSOs_For_Export<-unique(W_SSOs_For_Export)

#Add data with SSO reason for being tossed to the tossed data table
Has_SSOs<-subset(W_SSOs,select=c(StationCode,TisID,ProgramCode,ProgramName,ParentProjectCode,ParentProjectName
                                 ,ProjectCode,ProjectName,ProjectDescr,QAPPCode,QAPPName,CompositeID
                                 ,StationName,TargetLatitude,TargetLongitude,Datum,GeometryShape,RegionalBoardID
                                 ,UnitElevation,Elevation,SWRCBWatTypeCode,DWC_WaterBodyType
                                 ,EarliestDateSampled,CompositeType,CommonName,FinalID,TissuePrep
                                 ,TissueName,NumberFishperComp,SampleTypeCode,CompositeReplicate
                                 ,ResultReplicate,Matrix,Method,Analyte,Unit,Result,ResQualCode,MDL
                                 ,RL,QACode,BatchVerification,ComplianceCode,DilutionFactor,LabSampleID
                                 ,ResultComments,PrepPreservationName,PrepPreservationDate
                                 ,DigestExtractMethod,DigestExtractDate,AnalysisDate,CompositeComments
                                 ,LabBatch,LabBatchComments,AnalyzingAgency,SubmittingAgency
                                 ,LabSubmissionCode,OrganismGroup,WeightAvg_g,TLMax_mm,TLAvgLength_mm
                                 ,CompSizeCheck,SexSummary,LatestDateSampled,SampleDateRange_Days
                                 ,SampleDate,CollectionTime,TissueResultRowID,CompositeParentProjectName
                                 ,CompositeProjectCode,CompositeProjectName,CompositeCompositeID
                                 ,CompositeStationName,CompositeStationCode,CompositeLatitude
                                 ,CompositeLongitude,CompositeGeometryShape,CompositeSampleDate
                                 ,CompositeProgramName,CompositeCompositeType,CompositeCommonName
                                 ,CompositeFinalID,CompositeTissuePrep,CompositeTissueName
                                 ,CompositeSampleTypeCode,SamplingAgency,GroupSample,ProtocolCode
                                 ,LocationCode,HydroModLoc,Hydromod,CollectionReplicate,CollectionMethodName
                                 ,CollectionDeviceName,OrganismID,TotalCount,TagNumber,ForkLength,TotalLength
                                 ,LengthSource,OrganismWeight,UnitLengthFish,UnitWeightFish,WeightSource
                                 ,SizeDescr,Age,Sex,Anomaly,ProcessedOrganismsExpandedComments,TissueID
                                 ,TissueWeight,UnitTissueWeight,PartsComments,CompAgency,CompositeWeight
                                 ,UnitCompositeWeight,HomogonizedDate,CompositeRowID,PersonnelCode,SampleComments
                                 ,TissueCollectionComments,LocationDetailTIComments,CompositeJunctionRowID
                                 ,TLMin_mm,DWC_AnalyteWFraction,SampleID,DataQuality,DataQualityIndicator
                                 ,IR_REGIONALBOARD))
Has_SSOs$Issue<-"Data row has SSO for one or more beneficial uses that apply to it"
Has_SSOs<-unique(Has_SSOs)
AllTossedData<-rbind.data.frame(AllTossedData,Has_SSOs)

#Use Has_SSO table to remove rows of data with SSOs from the tissue_data frame
W_SSOs<-Has_SSOs
W_SSOs<-subset(W_SSOs,select=c("TisID"))
W_SSOs<-unique(W_SSOs)
Tissue_data<-anti_join(Tissue_data,W_SSOs)

#Remove TisID from dat since its no longer needed
Tissue_data$TisID<-NULL

#############_______________End Data Loading Section___________###############################
##############################################################################################
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################
##############################################################################################
######################Begin table joining and data splitting section##########################

Moisture<-tbl_df(Tissue_data[which(Tissue_data$Analyte=="Moisture"),])
Moisture<-select(Moisture,ProjectName,StationCode,SampleDate,Matrix,FinalID,Result,CompositeCompositeID)
Moisture$MoistureResult<-Moisture$Result
Moisture$Result<-NULL

#Remove duplicates of Moisture (samples collected same day at the same station)
Moisture<-tbl_df(as.data.table(Moisture)[,mean(MoistureResult),list(ProjectName,StationCode,SampleDate,Matrix,FinalID,CompositeCompositeID)])
names(Moisture)[names(Moisture)=="V1"]<-"MoistureResult"


#Needs conveting from dw to ww
Needs_Converting<-filter(Tissue_data,grepl("g dw",Unit))


#Add moisture to to the end of the data that needs converting
Needs_Converting_W_Moisture<-tbl_df(merge(Needs_Converting,Moisture,by=c("ProjectName","StationCode","FinalID","SampleDate","Matrix","CompositeCompositeID")))

	#Create a table of data reported in dry weight that does not have coresponding moisutre data
	#and thus will not be assessed
	DryWeight_Cant_Convert<-anti_join(Tissue_Data,Moisture,by=c("ProjectName","StationCode","FinalID","SampleDate","Matrix","CompositeCompositeID"))
	DryWeight_Cant_Convert<-filter(DryWeight_Cant_Convert,grepl("g dw", Unit))
	DryWeight_Cant_Convert$Issue<-"Missing coresponding moisture data"
	AllTossedData<-rbind.data.frame(AllTossedData,DryWeight_Cant_Convert)

#Convert the result of dw samples to be wet weight
Converting_Needs_Converting<-mutate(Needs_Converting_W_Moisture,Result=(Result*(1-(MoistureResult/100))))
Converting_Needs_Converting<-mutate(Converting_Needs_Converting,RL=(RL*(1-(MoistureResult/100))))
Converting_Needs_Converting<-mutate(Converting_Needs_Converting,MDL=(MDL*(1-(MoistureResult/100))))
Converted<-Converting_Needs_Converting
Converted$Unit[Converted$Unit=="ug/g dw"]<-"ug/g ww"
Converted$Unit[Converted$Unit=="g/g dw"]<-"g/g ww"
Converted$Unit[Converted$Unit=="mg/g dw"]<-"mg/g ww"
Converted$Unit[Converted$Unit=="pg/g dw"]<-"pg/g ww"
Converted$Unit[Converted$Unit=="ng/g dw"]<-"ng/g ww"
Converted$MoistureResult<-NULL

#Remove the rows of data with dry weight results
Tissue_data<-filter(Tissue_data,!grepl("g dw",Unit))

#Add converted data back into the main data frame
Tissue_data<-rbind.data.frame(Tissue_data,Converted)


##############################Begin UNIT CONVERSION SECTION###################

# Convert Results, MDL, and RL that are in mg/g to ug/kg datasetwide and change UnitName to ug/kg
Tissue_data$Result[which(Tissue_data$Unit=="mg/g ww")]<-Tissue_data$Result[which(Tissue_data$Unit=="mg/g ww")]*1000000
Tissue_data$MDL[which(Tissue_data$Unit=="mg/g ww")]<-Tissue_data$MDL[which(Tissue_data$Unit=="mg/g ww")]*1000000
Tissue_data$RL[which(Tissue_data$Unit=="mg/g ww")]<-Tissue_data$RL[which(Tissue_data$Unit=="mg/g ww")]*1000000
Tissue_data$Unit[Tissue_data$Unit=="mg/g ww"]<-"ug/kg ww"


# Convert Results, MDL, and RL that are in ug/g to ug/kg datasetwide and change UnitName to ug/kg
Tissue_data$Result[which(Tissue_data$Unit=="ug/g ww")]<-Tissue_data$Result[which(Tissue_data$Unit=="ug/g ww")]*1000
Tissue_data$MDL[which(Tissue_data$Unit=="ug/g ww")]<-Tissue_data$MDL[which(Tissue_data$Unit=="ug/g ww")]*1000
Tissue_data$RL[which(Tissue_data$Unit=="ug/g ww")]<-Tissue_data$RL[which(Tissue_data$Unit=="ug/g ww")]*1000
Tissue_data$Unit[Tissue_data$Unit=="ug/g ww"]<-"ug/kg ww"

# Convert Results, MDL, and RL that are in ng/g to ug/kg datasetwide and change UnitName to ug/kg
Tissue_data$Result[which(Tissue_data$Unit=="ng/g ww")]<-Tissue_data$Result[which(Tissue_data$Unit=="ng/g ww")]*1
Tissue_data$MDL[which(Tissue_data$Unit=="ng/g ww")]<-Tissue_data$MDL[which(Tissue_data$Unit=="ng/g ww")]*1
Tissue_data$RL[which(Tissue_data$Unit=="ng/g ww")]<-Tissue_data$RL[which(Tissue_data$Unit=="ng/g ww")]*1
Tissue_data$Unit[Tissue_data$Unit=="ng/g ww"]<-"ug/kg ww"

# Convert Results, MDL, and RL that are in pg/g to ug/kg datasetwide and change UnitName to ug/kg
Tissue_data$Result[which(Tissue_data$Unit=="pg/g ww")]<-Tissue_data$Result[which(Tissue_data$Unit=="pg/g ww")]*.001
Tissue_data$MDL[which(Tissue_data$Unit=="pg/g ww")]<-Tissue_data$MDL[which(Tissue_data$Unit=="pg/g ww")]*.001
Tissue_data$RL[which(Tissue_data$Unit=="pg/g ww")]<-Tissue_data$RL[which(Tissue_data$Unit=="pg/g ww")]*.001
Tissue_data$Unit[Tissue_data$Unit=="pg/g ww"]<-"ug/kg ww"

#Create table of data with units other than ug/g ww to be checked for issues
#and to potenitally add more converstions to the above section

Unit_Issues<-filter(Tissue_Data,!(Unit=="ug/kg ww"|Unit=="pg/g ww"|Unit=="ng/g ww"|Unit=="ug/g ww"|Unit=="mg/g ww"|Unit=="ug/g dw"|Unit=="ng/g dw"))
Unit_Issues$Issue<-"Funky Units"

##########################___End Unit Conversion################################################

######____Look up the assessability of the tissue data types _____######

#Cut out data for unacceptable tissue types and create table of this data for export and review
Unacceptable_Tissue<-anti_join(Tissue_Data,Acceptable_Tissue)
Unacceptable_Tissue$Issue<-"Bad Tissue Name"
AllTossedData<-rbind.data.frame(AllTossedData,Unacceptable_Tissue)

Tissue_data<-tbl_df(merge(Tissue_data,Acceptable_Tissue,by="TissueName"))


	#Create table of data for species that is not on the Eatable list for export and review
	Not_Eatable<-anti_join(Tissue_Data,Eatable_Fish,by="FinalID")
	Not_Eatable$Issue<-"Not on eatable list"
	AllTossedData<-rbind.data.frame(AllTossedData,Not_Eatable)

#Look up fraction of fish and its "eat-ability"
Tissue_data<-tbl_df(merge(Tissue_data,Eatable_Fish,by=c("FinalID")))

	###Toss out mercury data from fish that are too long or too short
	WrongSize<-Tissue_Data[which(Tissue_Data$TLAvgLength_mm>500|Tissue_Data$TLAvgLength_mm<50|is.na(Tissue_Data$TLAvgLength_mm)),]
	WrongSize<-filter(WrongSize,grepl("mercury|Mercury",Analyte))
	WrongSize$Issue<-"Fish too long or too short for assessment"
	AllTossedData<-rbind.data.frame(AllTossedData,WrongSize)


###################################
#Modify sample date to be first day fish were collected for each composite
#This only changes the sample date if the composite was collected over multiple days
CompositeDates<-Tissue_data
CompositeDates$Result<-NULL
CompositeDates$TLAvgLength_mm<-NULL
CompositeDates$QACode<-NULL
CompositeDates$NumberFishperComp<-NULL

MaxCompositeDates<-CompositeDates
MaxCompositeDates<-tbl_df(as.data.table(CompositeDates)[,max(SampleDate),list(FinalID
,TissueName,ProjectName,StationCode,ComplianceCode,Analyte,Matrix,Unit,TargetLatitude
,TargetLongitude,Method,ResQualCode,MDL,RL,CompositeCompositeID,Fraction)])
names(MaxCompositeDates)[names(MaxCompositeDates)=="V1"]<-"MaxSampleDate"


CompositeDates<-tbl_df(as.data.table(CompositeDates)[,min(SampleDate),list(FinalID
,TissueName,ProjectName,StationCode,ComplianceCode,Analyte,Matrix,Unit,TargetLatitude
,TargetLongitude,Method,ResQualCode,MDL,RL,CompositeCompositeID,Fraction)])
names(CompositeDates)[names(CompositeDates)=="V1"]<-"SampleDate2"

#Replace SampleDate in data with composite date which is equal to the first day
#a sample was collected for any given composite
Tissue_data<-tbl_df(merge(Tissue_data,CompositeDates))
Tissue_data$SampleDate<-Tissue_data$SampleDate2
Tissue_data$SampleDate2<-NULL

Tissue_data<-tbl_df(merge(Tissue_data,MaxCompositeDates))

###############################################################################################

######################_____Begin Summing Section__________#####################################


#Create table of rows of data that are for pollutants that need to be summed
SummingPollutants_Sum<-tbl_df(merge(Tissue_data,SummingPollutants,by="Analyte"))
                         

#Split out samples for pollutants with TEFs
TEF_Pollutants<-tbl_df(merge(SummingPollutants_Sum,PAH_TEFs,by="Analyte"))


	TossedTEFs<-merge(Tissue_Data,SummingPollutants)
	TossedTEFs<-tbl_df(TossedTEFs[TossedTEFs$ResQualCode!="=",])
	TossedTEFs<-TossedTEFs[TossedTEFs$SummingName=="PAHs (Polycyclic Aromatic Hydrocarbons)",]
	TossedTEFs$Issue<-"TEFs that were not quantifiable"
	TossedTEFs$TEF<-NULL
	TossedTEFs$SummingName<-NULL
	AllTossedData<-rbind.data.frame(AllTossedData,TossedTEFs)


#Filter out rows for pollutants other than PAHs
TEF_Pollutants<-filter(TEF_Pollutants,SummingName=="PAHs (Polycyclic Aromatic Hydrocarbons)")


#Remove rows of PAHs from the main summing table through antijoin
SummingPollutants_Sum<-anti_join(SummingPollutants_Sum,TEF_Pollutants,by=c("Analyte"
,"TissueName","ProjectName","StationCode","ComplianceCode","SampleDate","MaxSampleDate"
,"Matrix","Unit","TargetLatitude","TargetLongitude","TLAvgLength_mm","Method","FinalID","ResQualCode"
,"MDL","RL","QACode","SummingName","NumberFishperComp","CompositeCompositeID"))


#Remove samples from this table that were ND or DNQ because they will be counted as zero anyway
TEF_Pollutants<-filter(TEF_Pollutants,ResQualCode=="=")


#Adjust TEF pollutants by multiplying result by TEF
TEF_Pollutants_Adj<-tbl_df(mutate(TEF_Pollutants,Result=Result*TEF))

#Remove unecessary columns
TEF_Pollutants_Adj$TEF<-NULL

#Add adjusted rows back to the bottom of the table
SummingPollutants_Sum<-tbl_df(rbind.data.frame(SummingPollutants_Sum,TEF_Pollutants_Adj))

#Change the name of analye fields so that we can look up objectives based on sumed name
#not based on the name in CEDEN
SummingPollutants_Sum$CEDENName<-SummingPollutants_Sum$Analyte
SummingPollutants_Sum$Analyte<-SummingPollutants_Sum$SummingName

#Look up waterbody and beneficial use of each row of data
SummingPollutants_W_Waterbodies<-tbl_df(merge(SummingPollutants_Sum,Waterbodies,by="StationCode"))

#Split out PCBs and Archlors to assess spearately
PCBs<-SummingPollutants_W_Waterbodies[which(SummingPollutants_W_Waterbodies$Analyte=="PCBs (Polychlorinated biphenyls)"|SummingPollutants_W_Waterbodies$Analyte=="Aroclor"),]

#Remove all ND or DNQ samples because they would end up being zero anyway
PCBs<-filter(PCBs,!grepl("ND|DNQ",ResQualCode))

PCBs<-unique(PCBs)

#Sum PCBs based on Analyte Name "Summing Name", while removing CEDEN Name since it is no longer needed
PCBs_Sum_One<-tbl_df(as.data.table(PCBs)[,sum(Result),list(StationCode
,Analyte,FinalID,TissueName,ProjectName,MaxSampleDate
,SampleDate,Matrix,Unit,TargetLatitude,TargetLongitude,TLAvgLength_mm
,Fraction,SummingName,Waterbody,WBID,Wbtype,BUCODE,NumberFishperComp,CompositeCompositeID)])
names(PCBs_Sum_One)[names(PCBs_Sum_One)=="V1"]<-"Result"

#adjust analyte name so that arclors and PCBs have the same analyte name
PCBs_Sum_One$Analyte<-"PCBs (Polychlorinated biphenyls)"

#Take the max for each day of PCBs/Aroclors
PCBs_Max<-tbl_df(as.data.table(PCBs_Sum_One)[,max(Result),list(StationCode
,Analyte,FinalID,TissueName,ProjectName,SampleDate,MaxSampleDate,Matrix,Unit,NumberFishperComp,CompositeCompositeID
,TargetLatitude,TargetLongitude,TLAvgLength_mm,Fraction,Waterbody,WBID,Wbtype,BUCODE)])
names(PCBs_Max)[names(PCBs_Max)=="V1"]<-"Result"

#Add objectives to PCB data
PCBs_W_Objectives<-tbl_df(merge(PCBs_Max,Analytes,by=c("Analyte","Unit","BUCODE","Fraction")))


#Remove PCBs and Aroclors from the main summing table
SummingPollutants_W_Waterbodies<-filter(SummingPollutants_W_Waterbodies,!(Analyte=="Aroclor"|Analyte=="PCBs (Polychlorinated biphenyls)"))


#Look up objectives
SummingPollutants_W_Objectives<-tbl_df(merge(SummingPollutants_W_Waterbodies,Analytes,by=c("Analyte","Unit","BUCODE","Fraction")))

#Run quantitation check on summing pollutants
SummingPollutants_W_Objectives$Result[which(SummingPollutants_W_Objectives$RL<SummingPollutants_W_Objectives$Objective 
	& (SummingPollutants_W_Objectives$ResQualCode=="ND"|SummingPollutants_W_Objectives$ResQualCode=="DNQ"))]<-0 #perfect scenario
SummingPollutants_W_Objectives$Result[which((SummingPollutants_W_Objectives$RL>SummingPollutants_W_Objectives$Objective|is.na(SummingPollutants_W_Objectives$RL))
	&(SummingPollutants_W_Objectives$ResQualCode=="ND"|SummingPollutants_W_Objectives$ResQualCode=="DNQ"))]<-0 #imperfect results in zero for summing pollutants
SummingPollutants_W_Objectives<-tbl_df(SummingPollutants_W_Objectives)

SummingPollutants_W_Objectives<-unique(SummingPollutants_W_Objectives)

#add the reuslts fields together for the summing pollutants
SummedSumming_Pollutants<-tbl_df(as.data.table(SummingPollutants_W_Objectives)[,sum(Result),list(Analyte
,Unit,Fraction,StationCode,FinalID,TissueName,ProjectName,SampleDate,MaxSampleDate,Matrix,BUCODE,TargetLatitude,NumberFishperComp,CompositeCompositeID
,TargetLongitude,TLAvgLength_mm,Waterbody,WBID,Wbtype,AveragingPeroid,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(SummedSumming_Pollutants)[names(SummedSumming_Pollutants)=="V1"]<-"Result"


#Combine summed pollutants and PCbs together
SummedSummingPollutants<-rbind.data.frame(SummedSumming_Pollutants,PCBs_W_Objectives)


######################_____ End Summing Section________________________________########################

#####################_______Begin Mercury spearation section___________________########################

#Cut out mercury data from main data frame
Mercury<-filter(Tissue_data,grepl("mercury|Mercury",Analyte))

#Create list of data that only reported total mercury by first creating a list of methyl mercury station organism days
MethylMercury<-filter(Mercury,Analyte=="Mercury, Methyl")
MethylMercury<-distinct(select(MethylMercury,Analyte,StationCode,NumberFishperComp,FinalID,TissueName,SampleDate,MaxSampleDate))

#Then create a table of station organism days that do NOT have methyl mercury data
TotalOnly<-anti_join(Mercury,MethylMercury,by=c("Analyte","StationCode","FinalID","NumberFishperComp","TissueName","SampleDate","MaxSampleDate"))

	TotalandMethyl<-tbl_df(merge(Tissue_Data,MethylMercury))
	TotalandMethyl$Issue<-"This is not really an issue.  Data was not assessed because both total and methyl mercury were reported"
	AllTossedData<-rbind.data.frame(AllTossedData,TotalandMethyl)

#Remove all total mercury data from the mercury table since total mercury is only
#assessed for station organism days that do not have methyl mercury reported
Mercury<-Mercury[which(Mercury$Analyte!="Mercury"|Mercury$Analyte=="mercury"),]

#change the name of the methyl mercury data to be "mercury"
#since this is th pollutant name as it appears in the anlyte tables as well as in calwqa
Mercury$Analyte<-"Mercury"

#add the total mercury data for station organism days that did not have methyl mercury reproted
#back to the main mercury data table
Mercury<-rbind.data.frame(Mercury,TotalOnly)

#Add mercury back to main data frame before looking up objectives
Tissue_data<-filter(Tissue_data,!grepl("mercury|Mercury",Analyte))
Tissue_data<-rbind.data.frame(Tissue_data,Mercury)


##########################_____End mercury separation section__________##############################

####################_____Begin objective lookup and quantitation check section________###############

#Remove data for "selfnamed" pollutants, i.e. pollutants who's name matches the resulting summing name
#this means it is entirely captured by the pollutant summing section and leaving the rows in here 
#would reuslt in inflated sample counts
Tissue_data<-anti_join(Tissue_data,Self_Named_Summing_Pollutants,by="Analyte")


#Lookup waterbody and beneficial use informaiton
Tissue_data_W_Waterbodies<-tbl_df(merge(Tissue_data,Waterbodies,by="StationCode"))

	TissueDataNoWaterbodies<-anti_join(Tissue_Data,Waterbodies)
	TissueDataNoWaterbodies$Issue<-"Station does not have coresponding waterbody"
	AllTossedData<-rbind.data.frame(AllTossedData,TissueDataNoWaterbodies)

#Lookup objectives
Tissue_data_W_Objectives<-tbl_df(merge(Tissue_data_W_Waterbodies,Analytes,by=c("Analyte","BUCODE","Unit","Fraction")))

	TissueDataNoObjectives<-anti_join(Tissue_Data,Analytes,by="Analyte")
	SummingPollutantsThatHaveObjectives<-tbl_df(merge(Tissue_Data,SummingPollutants))
	SummingPollutantsThatHaveObjectives$Analyte<-NULL
	SummingPollutantsThatHaveObjectives$Analyte<-SummingPollutantsThatHaveObjectives$SummingName
	SummingPollutantsThatHaveObjectives$SummingName<-NULL
	SummingPollutantsThatHaveObjectives<-tbl_df(merge(SummingPollutantsThatHaveObjectives,Analytes,by="Analyte"))
	SummingPollutantsThatHaveObjectives<-tbl_df(unique(SummingPollutantsThatHaveObjectives$TisID))
	colnames(SummingPollutantsThatHaveObjectives)<-c("TisID")
	TissueDataNoObjectives<-anti_join(TissueDataNoObjectives,SummingPollutantsThatHaveObjectives,by="TisID")
	TissueDataNoObjectives$Issue<-"Analyte does not have objective"
	AllTossedData<-rbind.data.frame(AllTossedData,TissueDataNoObjectives)


#Run quantitation check
# Correct the results based on quantitation limits - I did this before removing replicates to avoid averaging samples that we assesed with a different method

Tissue_data_W_Objectives$Result[which(Tissue_data_W_Objectives$RL<Tissue_data_W_Objectives$Objective & (Tissue_data_W_Objectives$ResQualCode=="ND"|Tissue_data_W_Objectives$ResQualCode=="DNQ"))]<-Tissue_data_W_Objectives$MDL[which(
	Tissue_data_W_Objectives$RL<Tissue_data_W_Objectives$Objective & (Tissue_data_W_Objectives$ResQualCode=="ND"|Tissue_data_W_Objectives$ResQualCode=="DNQ"))]*.5 #perfect scenario

Tissue_data_W_Objectives$Result[which((Tissue_data_W_Objectives$RL>Tissue_data_W_Objectives$Objective|is.na(Tissue_data_W_Objectives$RL))&(Tissue_data_W_Objectives$ResQualCode=="ND"|Tissue_data_W_Objectives$ResQualCode=="DNQ"))]<--100000000 #imperfect results in very negative number

#Remove unecessary QA information from the table now that it has been used
Tissue_data_W_Objectives$RL<-NULL
Tissue_data_W_Objectives$MDL<-NULL
Tissue_data_W_Objectives$ResQualCode<-NULL
Tissue_data_W_Objectives$QACode<-NULL
Tissue_data_W_Objectives$Method<-NULL
Tissue_data_W_Objectives$CompositeID<-NULL


#Create table of tossed samples and then combine them with the other tables of tossed samples due to quantitation limts
General_Tossed<-filter(Tissue_data_W_Objectives,(Result==-100000000))
Tossed_Tissue_4_Species<-General_Tossed
Tossed_Tissue_4_Species$ComplianceCode<-NULL

#why did I create this a second time?  I am not sure, but I am hesitant to delete it
#until I figure out why I added it in the first place *face plam*
Tossed_4_Species<-General_Tossed
Tossed_4_Species$ComplianceCode<-NULL

# Calculate maximum and minimum date for each waterbody with data
General_Tossed$SampleDate<-as.Date(General_Tossed$SampleDate, "%m/%d/%Y")
Tossed_Max_Date<-as.data.table(General_Tossed)[,max(SampleDate),list(Analyte,StationCode
	,BUCODE,ProjectName,Matrix,Fraction,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Tossed_Max_Date)[names(Tossed_Max_Date)=='V1']<-"MaxDate"
Tossed_Max_Date<-tbl_df(Tossed_Max_Date)
Tossed_Min_Date<-as.data.table(General_Tossed)[,min(SampleDate),list(Analyte,StationCode
	,BUCODE,ProjectName,Matrix,Fraction,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Tossed_Min_Date)[names(Tossed_Min_Date)=='V1']<-"MinDate"
Tossed_Min_Date<-tbl_df(Tossed_Min_Date)
Tossed_Date_Range<-tbl_df(merge(Tossed_Max_Date,Tossed_Min_Date,by=c("Analyte","StationCode"
	,"Waterbody","WBID","ProjectName","Matrix","Fraction","BUCODE"
	,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))
General_Tossed<-tbl_df(merge(General_Tossed,Tossed_Date_Range))


#Convert General_Tossed into a list of samples thrown out with language specifying they were thrown out due to quantitation issues
Quantitation_Discards<-distinct(select(General_Tossed,Analyte,StationCode,Waterbody,WBID,Matrix
	,Fraction,ProjectName,BUCODE,Objective_Language,Evaluation_Guideline
	,Objective_Ref_Number,Eval_Ref_Number,MaxDate,MinDate,FinalID,CompositeCompositeID))
Quantitation_Discards<-Quantitation_Discards%>%dplyr::group_by(Analyte,StationCode,Waterbody,WBID
	,ProjectName,Matrix,Fraction,BUCODE
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number
	,MaxDate,MinDate)%>%dplyr::summarise(count=n())
names(Quantitation_Discards)<-c("Analyte","StationCode","Waterbody","WBID","ProjectName"
	,"Matrix","Fraction","BUCODE","Objective_Language"
	,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"
	,"MaxDate","MinDate","TossedSampleCount")


#Convert General_Tossed into a list of samples thrown out with language specifying they were thrown out due to quantitation issues specific to mercury
Mercury_Quantitation_Discards<-General_Tossed
Mercury_Quantitation_Discards<-tbl_df(merge(Mercury_Quantitation_Discards,TrophicLevel))
Prey_Merc_QD<-subset(Mercury_Quantitation_Discards,TLAvgLength_mm>50&TLAvgLength_mm<150&(BUCODE=="WI"|BUCODE=="MA"))
Prey_Merc_QD<-subset(Prey_Merc_QD,Analyte=="Mercury")
Prey_Merc_QD$SampleDate2<-month(Prey_Merc_QD$SampleDate)
Prey_Merc_QD<-subset(Prey_Merc_QD,SampleDate2>2&SampleDate2<8)
Prey_Merc_QD$TrophicLevel<-"PF1"
Prey_Merc_QD$SampleDate2<-NULL
Prey_Merc_QDYR<-subset(Mercury_Quantitation_Discards,TLAvgLength_mm>50&TLAvgLength_mm<150&(BUCODE=="WI"|BUCODE=="MA"))
Prey_Merc_QDYR<-subset(Prey_Merc_QDYR,Analyte=="Mercury")
Prey_Merc_QDYR$SampleDate2<-month(Prey_Merc_QDYR$SampleDate)
Prey_Merc_QDYR$TrophicLevel<-"PFY"
Prey_Merc_QDYR$SampleDate2<-NULL
PreyMercQD<-rbind.data.frame(Prey_Merc_QD,Prey_Merc_QDYR)
General_Merc_QD<-subset(Mercury_Quantitation_Discards,TLAvgLength_mm>150&TLAvgLength_mm<500
&(BUCODE=="CM"|BUCODE=="WI"|BUCODE=="MA")&(TrophicLevel==3|TrophicLevel==4))
General_Merc_QD<-subset(General_Merc_QD,Analyte=="Mercury")
Mercury_Quantitation_Discards<-rbind.data.frame(PreyMercQD,General_Merc_QD)


Mercury_Quantitation_Discards<-distinct(select(Mercury_Quantitation_Discards,Analyte,Waterbody,WBID,Matrix
	,Fraction,ProjectName,BUCODE,Objective_Language,Evaluation_Guideline
	,Objective_Ref_Number,Eval_Ref_Number,MaxDate,MinDate,FinalID,CompositeCompositeID,TrophicLevel))
Mercury_Quantitation_Discards<-Mercury_Quantitation_Discards%>%dplyr::group_by(Analyte,Waterbody,WBID
	,ProjectName,Matrix,Fraction,BUCODE
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number
	,MaxDate,MinDate,TrophicLevel)%>%dplyr::summarise(count=n())
names(Mercury_Quantitation_Discards)<-c("Analyte","Waterbody","WBID","ProjectName"
	,"Matrix","Fraction","BUCODE","Objective_Language"
	,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"
	,"MaxDate","MinDate","TrophicLevel","TossedSampleCount")


#Filter out samples thrown out due to quantitation issues from the main data table
Tissue_data_W_Objectives<-filter(Tissue_data_W_Objectives,(Result!=-100000000))


#Remove replicates from data table
Tissue_data_no_reps<-tbl_df(as.data.table(Tissue_data_W_Objectives)[,mean(Result),list(Analyte
,BUCODE,Unit,Fraction,StationCode,TissueName,FinalID,ProjectName,SampleDate,MaxSampleDate,Matrix,TargetLatitude
,TargetLongitude,TLAvgLength_mm,Waterbody,WBID,Wbtype,AveragingPeroid,Objective,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number,NumberFishperComp,CompositeCompositeID)])
names(Tissue_data_no_reps)[names(Tissue_data_no_reps)=="V1"]<-"Result"

#Combine the summed pollutants and main data table back together
Tissue_data_no_reps<-tbl_df(rbind.data.frame(Tissue_data_no_reps,SummedSummingPollutants))

#########_________________End objective look up and quantitation check section ______#########################

#########_________________Begin averaging peroid section ______________________###############################

##############################################################################################################



#Re-Load trophic level table
TrophicLevel<-tbl_df(read.delim(TissueSpecies,header=TRUE,stringsAsFactors=FALSE))
TrophicLevel<-filter(TrophicLevel,TrophicLevel!="")
TrophicLevel<-tbl_df(subset(TrophicLevel,select=c(FinalID,TrophicLevel)))

#Split mercury data from the rest of the data for 365 day averaging peroid

MercuryAveraging<-subset(Tissue_data_no_reps,Analyte=="Mercury")
MercuryAveraging<-tbl_df(merge(MercuryAveraging,TrophicLevel))
MercuryAveraging<-subset(MercuryAveraging,(TLAvgLength_mm>150 & TLAvgLength_mm<500&(BUCODE=="CM"|BUCODE=="WI"|BUCODE=="MA")&TrophicLevel==3&Objective==200)|(TLAvgLength_mm>200 & TLAvgLength_mm<500&(BUCODE=="CM"|BUCODE=="WI"|BUCODE=="MA")&TrophicLevel==4&Objective==200))
MercuryAveraging$Legal<-""

#Add table of Legal Size Limits to data
LegalMercuryAveraging<-tbl_df(merge(MercuryAveraging,LegalSizeLimits,all.x=TRUE))

#Limit legal size limit data to waterbodies with legal size limits
LimitLegalSizeLimit<-subset(LegalSizeLimits,select=c(Waterbody,WBID))
StatewideLimits<-subset(LegalMercuryAveraging,select=c(Waterbody,WBID,FinalID))
StatewideLimits<-subset(StatewideLimits,(FinalID=="Micropterus salmoides"|FinalID=="Micropterus punctulatus"|FinalID=="Micropterus dolomieu"|FinalID=="Morone saxatilis"|FinalID=="Acipenser transmontanus"|FinalID=="Acipenser medirostris"))
StatewideLimits$FinalID<-NULL
LimitLegalSizeLimit<-rbind.data.frame(LimitLegalSizeLimit,StatewideLimits)
LimitLegalSizeLimit<-unique(LimitLegalSizeLimit)
LegalMercuryAveraging<-tbl_df(merge(LegalMercuryAveraging,LimitLegalSizeLimit))

#Apply statewide size limits to fish/waterbodies that are not exceptions to the rule
LegalMercuryAveraging$MinLength_mm[which(is.na(LegalMercuryAveraging$MinLength_mm)&LegalMercuryAveraging$Wbtype=="L"&(LegalMercuryAveraging$FinalID=="Micropterus salmoides"|LegalMercuryAveraging$FinalID=="Micropterus punctulatus"|LegalMercuryAveraging$FinalID=="Micropterus dolomieu"))]<-304.8
LegalMercuryAveraging$MinLength_mm[which(is.na(LegalMercuryAveraging$MaxLength_mm)&LegalMercuryAveraging$FinalID=="Morone saxatilis")]<-457.2
LegalMercuryAveraging$MinLength_mm[which(is.na(LegalMercuryAveraging$MinLength_mm)&(LegalMercuryAveraging$FinalID=="Acipenser transmontanus"|LegalMercuryAveraging$FinalID=="Acipenser medirostris"))]<-1016
LegalMercuryAveraging$MaxLength_mm[which(is.na(LegalMercuryAveraging$MaxLength_mm)&(LegalMercuryAveraging$FinalID=="Acipenser transmontanus"|LegalMercuryAveraging$FinalID=="Acipenser medirostris"))]<-1524


#Convert "exception" codes back to is.na
LegalMercuryAveraging$MinLength_mm[which(LegalMercuryAveraging$MinLength_mm==-1)]<-NA
LegalMercuryAveraging$MaxLength_mm[which(LegalMercuryAveraging$MaxLength_mm==-1)]<-NA

LegalMercuryAveraging$MaxLength_mm[which(is.na(LegalMercuryAveraging$MaxLength_mm))]<-500
LegalMercuryAveraging$MinLength_mm[which(is.na(LegalMercuryAveraging$MinLength_mm)&LegalMercuryAveraging$TrophicLevel==3)]<-150
LegalMercuryAveraging$MinLength_mm[which(is.na(LegalMercuryAveraging$MinLength_mm)&LegalMercuryAveraging$TrophicLevel==4)]<-200
LegalMercuryAveraging<-subset(LegalMercuryAveraging,TLAvgLength_mm>MinLength_mm &TLAvgLength_mm<MaxLength_mm)
LegalMercuryAveraging$Legal<-paste0("This LOE does not contain data from fish who's average length was outside of the legal size limits as described by the California Department of Fish and Wildlife Fishing Regulations.")
LegalMercuryAveraging$MaxLength_mm<-NULL
LegalMercuryAveraging$MinLength_mm<-NULL

MercuryAveraging<-rbind.data.frame(MercuryAveraging,LegalMercuryAveraging)


#Split mercury data for preyfish collected between March and July
PreyFishMercury<-subset(Tissue_data_no_reps,Analyte=="Mercury")
PreyFishMercury<-subset(PreyFishMercury,TLAvgLength_mm>50&TLAvgLength_mm<150&(BUCODE=="WI"|BUCODE=="MA"))
PreyFishMercury<-subset(PreyFishMercury,Objective==50)
PreyFishMercury$SampleDate2<-month(PreyFishMercury$SampleDate)
PreyFishMercury<-subset(PreyFishMercury,SampleDate2>1&SampleDate2<8)
PreyFishMercury$TrophicLevel<-"PF1"
PreyFishMercury$SampleDate2<-NULL
PreyFishMercury$Legal<-""

#Create table of year round preyfish mercury data
PreyFish_Y_Mercury<-subset(Tissue_data_no_reps,Analyte=="Mercury")
PreyFish_Y_Mercury<-subset(PreyFish_Y_Mercury,TLAvgLength_mm>50&TLAvgLength_mm<150&(BUCODE=="WI"|BUCODE=="MA"))
PreyFish_Y_Mercury<-subset(PreyFish_Y_Mercury,Objective==50)
PreyFish_Y_Mercury$SampleDate2<-month(PreyFish_Y_Mercury$SampleDate)
PreyFish_Y_Mercury$TrophicLevel<-"PFY"
PreyFish_Y_Mercury$SampleDate2<-NULL
PreyFish_Y_Mercury$Legal<-""

#Combine the mercury subsets together before averaging
MercuryAveraging<-rbind.data.frame(MercuryAveraging,PreyFishMercury)
MercuryAveraging<-rbind.data.frame(MercuryAveraging,PreyFish_Y_Mercury)

#Add field of sample data year
MercuryAveraging$Year<-year(MercuryAveraging$SampleDate)

#Create table of mercury data that will be used for species list, fish count, and date range
MercuryLOEInfo<-MercuryAveraging

TossedMercury<-General_Tossed
TossedMercury$MaxDate<-NULL
TossedMercury$MinDate<-NULL
TossedMercury<-tbl_df(merge(TossedMercury,TrophicLevel))
TossedMercury$ComplianceCode<-NULL
TossedMercury$Legal<-""
TossedMercuryLegal<-TossedMercury[which(TossedMercury$TrophicLevel==3|TossedMercury$TrophicLevel==4),]
TossedMercuryLegal$Legal<-paste0("This LOE does not contain data from fish who's average length was outside of the legal size limits as described by the California Department of Fish and Wildlife Fishing Regulations.")
TossedMercury<-rbind.data.frame(TossedMercury,TossedMercuryLegal)
TossedMercury_4_Species<-TossedMercury


#Create a table of stations with mercury data associated with each waterbody pollutant tissue type project name trophic level combination of station codes, waterbodies, analyte, and count of stations
#Mercury_Stations<-MercuryLOEInfo
#Mercury_Stations$Year<-NULL
#Mercury_Stations<-rbind.data.frame(Mercury_Stations,TossedMercury)
#Mercury_Stations<-subset(Mercury_Stations,select=c("Analyte","StationCode","Waterbody","WBID","BUCODE","ProjectName","Matrix","Fraction","TrophicLevel","Legal"))
#Mercury_Stations<-distinct(select(Mercury_Stations,Analyte,Waterbody,StationCode,WBID,BUCODE,ProjectName,Matrix,Fraction,TrophicLevel,Legal))
#Mercury_Stations<-Mercury_Stations%>%group_by(Analyte,Waterbody,WBID,BUCODE,ProjectName,Matrix,Fraction,TrophicLevel,Legal)%>%dplyr::summarise(count=n(),
#StationCode=paste(StationCode,collapse=", "))
#names(Mercury_Stations)<-c("Analyte","Waterbody","WBID","BUCODE","ProjectName","Matrix","Fraction","TrophicLevel","Legal","StationCount","StationCodes")


#Remove mercury data from main data frame
Tissue_data_no_reps<-subset(Tissue_data_no_reps,!(Analyte=="Mercury"&Fraction=="Fish Fillet"))

###########################Begin 7 Day Averaging peroid#######################################################

##############################################################################################################



Tissue_data_no_reps$SampleDate<-ymd(Tissue_data_no_reps$SampleDate)
Tissue_data_no_reps$MaxSampleDate<-ymd(Tissue_data_no_reps$MaxSampleDate)


#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
Tissue_data2 <- Tissue_data_no_reps %>% dplyr::arrange(SampleDate)%>%
  dplyr::group_by(Analyte,BUCODE,Unit,StationCode,ProjectName, Matrix,Fraction,TissueName,FinalID,Waterbody
	,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  dplyr::mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 7)) %>% 
  dplyr::group_by(interval, add = TRUE) %>% 
  dplyr::summarise(SampleDate2 = max(MaxSampleDate),SampleDate = min(SampleDate),Objective=mean(Objective), Result = mean(Result)) %>%
  select(Analyte,SampleDate,SampleDate2,BUCODE,Unit,StationCode,ProjectName, Matrix, Fraction,TissueName,FinalID
	,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number,Result)



###########################Begin 365 Day Averaging peroid#######################################################

################################################################################################################




MercuryAveraging$SampleDate<-ymd(MercuryAveraging$SampleDate)
MercuryAveraging$MaxSampleDate<-ymd(MercuryAveraging$MaxSampleDate)
MercuryAveraging$TLAvgLength_mm<-NULL
MercuryAveraging<-unique(MercuryAveraging)
MercuryAveraging<-mutate(MercuryAveraging,Result=(Result*NumberFishperComp))


AveragedMercury<-tbl_df(as.data.table(MercuryAveraging)[,.(SumResult=sum(Result),NumberFish=sum(NumberFishperComp)),list(Analyte,BUCODE,Unit,StationCode,ProjectName, Matrix,Fraction,TissueName,TrophicLevel,Legal,Waterbody
	,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number,Year)])
AveragedMercury<-tbl_df(mutate(AveragedMercury,Result=(SumResult/NumberFish)))
AveragedMercury<-tbl_df(AveragedMercury)
AveragedMercury$SumResult<-NULL




###################################################################################################################

############################_______ End averaging peroid section _______________________###########################

###################################################################################################################

#Lookup CommonName for fish species then remove FinalID from table since it is no longer needed
Tissue_data2<-tbl_df(merge(Tissue_data2,CommonNameLookup))
Tissue_data2$FinalID<-NULL

#Calculate date range of the samples that made it to this point
Max_Date<-tbl_df(as.data.table(Tissue_data2)[,max(SampleDate2),list(Analyte,StationCode,BUCODE,ProjectName,Matrix,Fraction,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(Max_Date)[names(Max_Date)=="V1"]<-"MaxDate"
Min_Date<-tbl_df(as.data.table(Tissue_data2)[,min(SampleDate),list(Analyte,StationCode,BUCODE,ProjectName,Matrix,Fraction,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(Min_Date)[names(Min_Date)=="V1"]<-"MinDate"

AllDates<-tbl_df(merge(Max_Date,Min_Date))

Tissue_data2$SampleDate2<-NULL
Tissue_Species<-Tissue_data_no_reps


#Get count of composites tossed due to quantitation issues
#TossedSpecies<-subset(Tossed_4_Species,select=c("Analyte","StationCode","BUCODE","ProjectName", "Waterbody","WBID", "Matrix","Fraction","Result","CompositeCompositeID", "FinalID","NumberFishperComp"))
#TossedSpecies<-tbl_df(merge(TossedSpecies,CommonNameLookup))
#TossedSpecies$FinalID<-NULL
#TossedSpecies<-unique(TossedSpecies)
#TossedSpecies<-tbl_df(as.data.table(TossedSpecies)[,length(CompositeCompositeID),list(Analyte,StationCode,BUCODE,ProjectName,Waterbody,WBID,Matrix,Fraction,CommonName,NumberFishperComp)])
#names(TossedSpecies)[names(TossedSpecies)=="V1"]<-"NumberComposites"


#Combine tissue data no reps with tissue tossed because of quantitation issues
Tissue_Species<-rbind.data.frame(Tissue_Species,Tossed_Tissue_4_Species)

#Remove unecessary fields
Tissue_Species$TargetLatitude<-NULL
Tissue_Species$TargetLongitude<-NULL
Tissue_Species$Wbtype<-NULL
Tissue_Species$TLAvgLength_mm<-NULL
Tissue_Species$AveragingPeroid<-NULL

#Lookup common name of species for species list then remove latin name (FinalID)
Tissue_Species<-tbl_df(merge(Tissue_Species,CommonNameLookup))
Tissue_Species$FinalID<-NULL


#Create table of composites listing number of species and number of fish per composite
Species<-subset(Tissue_Species,select=c("Analyte","StationCode","BUCODE","ProjectName", "Waterbody","WBID", "Matrix","Fraction", "CompositeCompositeID","CommonName","NumberFishperComp"))
Species<-unique(Species)
Species<-tbl_df(as.data.table(Species)[,length(CompositeCompositeID),list(Analyte,StationCode,BUCODE,ProjectName,Waterbody,WBID,Matrix,Fraction,CommonName,NumberFishperComp)])
names(Species)[names(Species)=="V1"]<-"NumberComposites"
#Species<-rbind.data.frame(Species,TossedSpecies)
#Species<-tbl_df(as.data.table(Species)[,sum(NumberComposites),list(Analyte,StationCode,BUCODE,ProjectName,Waterbody,WBID,Matrix,Fraction,CommonName,NumberFishperComp)])
#names(Species)[names(Species)=="V1"]<-"NumberComposites"
Species$CommonName<-paste0(Species$NumberComposites," composite(s) of ",Species$CommonName, " each composed of ",Species$NumberFishperComp," fish per composite")
Species<-distinct(select(Species,Analyte,StationCode,BUCODE,ProjectName, Waterbody, WBID, Matrix,Fraction, CommonName))
Species<-Species%>%dplyr::group_by(Analyte,StationCode, BUCODE,ProjectName, Waterbody, WBID, Matrix,Fraction)%>%dplyr::summarise(count=n(), CommonName=paste(CommonName,collapse=", "))
names(Species)<-c("Analyte","StationCode","BUCODE","ProjectName", "Waterbody", "WBID", "Matrix","Fraction","SpeciesCount", "CommonName")


#Count up exceedances
SpeciesExceedances<-tbl_df(as.data.table(Tissue_data2)[,sum(Result>Objective),list(Analyte,StationCode
,BUCODE,Unit,ProjectName,Matrix,Fraction,TissueName,CommonName,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
SpeciesExceedances<-tbl_df(SpeciesExceedances[SpeciesExceedances$V1>=1,])
SpeciesExceedances<-subset(SpeciesExceedances,select=c("Analyte","StationCode","ProjectName", "BUCODE","Waterbody","WBID", "Matrix","Fraction", "CommonName"))
SpeciesExceedances<-distinct(select(SpeciesExceedances,Analyte,StationCode,ProjectName,BUCODE, Waterbody, WBID, Matrix,Fraction, CommonName))
SpeciesExceedances<-SpeciesExceedances%>%dplyr::group_by(Analyte,StationCode,ProjectName,BUCODE, Waterbody, WBID, Matrix,Fraction)%>%dplyr::summarise(count=n(), CommonName=paste(CommonName,collapse=", "))
names(SpeciesExceedances)<-c("Analyte","StationCode","ProjectName","BUCODE","Waterbody", "WBID", "Matrix","Fraction","ExceedingSpeciesCount", "ExceedingCommonName")

#create table of WB Pollutant combos with exceedances
ExceedingSpecies<-tbl_df(merge(Species,SpeciesExceedances,by=c("Analyte","StationCode","BUCODE","ProjectName","Waterbody","WBID","Matrix","Fraction")))

#Create table of wb pollutant combos without exceedances
Species_No_Exceedances<-anti_join(Species,SpeciesExceedances,by=c("Analyte","StationCode","BUCODE","ProjectName","Waterbody","WBID","Matrix","Fraction"))

#Get table of exceedance counts by waterbody pollutant combination
Exceedances<-tbl_df(as.data.table(Tissue_data2)[,sum(Result>Objective),list(Analyte,StationCode
,BUCODE,ProjectName,Matrix,Fraction,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)])
names(Exceedances)[names(Exceedances)=="V1"]<-"EXCEEDANCE_COUNT"

#Get table of total samples assessed
TotalSamples<-tbl_df(as.data.table(Tissue_data2)[,sum(Result>=0),list(Analyte,StationCode
,BUCODE,ProjectName,Matrix,Fraction,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)])
names(TotalSamples)[names(TotalSamples)=="V1"]<-"SAMPLE_COUNT"

#merge samples and exceedances together
AllLOEs<-tbl_df(merge(Exceedances,TotalSamples))

#Add max and min dates to the LOEs for temporal rep
AllLOEs<-tbl_df(merge(AllLOEs,AllDates))

#Add Temporal rep field then remove max and min date fields
AllLOEs$TEMPORAL_REP<-paste0("Data for this line of evidence were collected between ", AllLOEs$MinDate," and ",AllLOEs$MaxDate,".  When composited fish were collected over multiple days, the first day of fish collection was used as the sample date in the LOE, both for LOE writing, and for averaging period purposes.")
AllLOEs$MaxDate<-NULL
AllLOEs$MinDate<-NULL

#################Zero of zero LOEs####################

#table of LOEs where all samples were thrown out
ZeroOfZero<-anti_join(Quantitation_Discards,AllLOEs,by=c("Analyte","StationCode"
,"Waterbody","WBID","ProjectName","Matrix","Fraction","BUCODE","Objective_Language"
,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))


#Create date range field then remove max and min date field
ZeroOfZero$TEMPORAL_REP<-paste0("Data for this line of evidence were collected between ", ZeroOfZero$MinDate," and ",ZeroOfZero$MaxDate,".  When composited fish were collected over multiple days, the first day of fish collection was used as the sample date in the LOE, both for LOE writing, and for averaging period purposes.")
ZeroOfZero$MinDate<-NULL
ZeroOfZero$MaxDate<-NULL

#Add species to the end of the zero of zero LOEs
ZeroOfZero<-tbl_df(merge(ZeroOfZero,Species))

#Create the data used field
ZeroOfZero$DATA_USED<-paste0("Waterboard staff assessed ",ZeroOfZero$ProjectName," data for ",ZeroOfZero$Waterbody," to determine beneficial use support and the results are as follows: Zero of Zero samples exceeded the water quality threshold for ",ZeroOfZero$Analyte,". Although data was collected for ",ZeroOfZero$SpeciesCount," fish species (", ZeroOfZero$CommonName,"), all ",ZeroOfZero$TossedSampleCount," sample(s) had to be thrown out due to quantitation issues.")
ZeroOfZero$SAMPLE_COUNT<-0
ZeroOfZero$EXCEEDANCE_COUNT<-0
ZeroOfZero$TossedSampleCount<-NULL

#####################LOEs with exceedances###################

#Create table of LOEs with exceedances
WithExceedances<-tbl_df(merge(AllLOEs,ExceedingSpecies))

#Create table of LOEs with exceedances and quantitation discards
Exceedances_W_Discards<-tbl_df(merge(WithExceedances,Quantitation_Discards))

#Add total sample field then add data used field
Exceedances_W_Discards<-mutate(Exceedances_W_Discards,TotalSamples=SAMPLE_COUNT+TossedSampleCount)
Exceedances_W_Discards$DATA_USED<-paste0("Water Board staff assessed ",Exceedances_W_Discards$ProjectName," data for ",Exceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",Exceedances_W_Discards$EXCEEDANCE_COUNT," of the ",Exceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Exceedances_W_Discards$Analyte,". Data were collected for ",Exceedances_W_Discards$SpeciesCount," fish species (", Exceedances_W_Discards$CommonName,").  Of these species, ",Exceedances_W_Discards$ExceedingSpeciesCount," species (",Exceedances_W_Discards$ExceedingCommonName,") exceeded the evaluation guideline.","  Although a total of ",Exceedances_W_Discards$TotalSamples," samples were collected ",Exceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")

#Remove the total samples field and other fields related to quantitation discards
Exceedances_W_Discards$TotalSamples<-NULL
Exceedances_W_Discards$TossedSampleCount<-NULL
Exceedances_W_Discards$MinDate<-NULL
Exceedances_W_Discards$MaxDate<-NULL

#Remove the LOEs with quantititation discards from Exceedances table then create data used field
WithExceedances<-anti_join(WithExceedances,Quantitation_Discards,by=c("Analyte","StationCode","BUCODE","ProjectName","Matrix","Fraction","Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
WithExceedances$DATA_USED<-paste0("Water Board staff assessed ",WithExceedances$ProjectName," data for ",WithExceedances$Waterbody, " to determine beneficial use support and the results are as follows ",WithExceedances$EXCEEDANCE_COUNT," of the ",WithExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",WithExceedances$Analyte,". Data were collected for ",WithExceedances$SpeciesCount," fish species (", WithExceedances$CommonName,").  Of these, ",WithExceedances$ExceedingSpeciesCount," species (",WithExceedances$ExceedingCommonName,") exceeded the water quality threshold.")

#Join exceedances with and without quantitation discards together then remove unecessary fields
ExceedingLOEs<-rbind.data.frame(Exceedances_W_Discards,WithExceedances)
ExceedingLOEs$ExceedingSpeciesCount<-NULL
ExceedingLOEs$ExceedingCommonName<-NULL



##########################___LOEs No Exceedances_____###############################

#Create table of LOEs with no exceedances
No_Exceedances<-anti_join(AllLOEs,ExceedingSpecies,by=c("Analyte","StationCode","BUCODE","ProjectName","Matrix","Fraction","Waterbody","WBID"))
No_Exceedances<-tbl_df(merge(No_Exceedances,Species))

#Create table of LOEs with without exceedances that have quantitation discards
NoExceedances_W_Discards<-tbl_df(merge(No_Exceedances,Quantitation_Discards))

#Add total sample field then add data used field
NoExceedances_W_Discards<-mutate(NoExceedances_W_Discards,TotalSamples=SAMPLE_COUNT+TossedSampleCount)
NoExceedances_W_Discards$DATA_USED<-paste0("Water Board staff assessed ",NoExceedances_W_Discards$ProjectName," data for ",NoExceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",NoExceedances_W_Discards$EXCEEDANCE_COUNT," of the ",NoExceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",NoExceedances_W_Discards$Analyte,". Data were collected for ",NoExceedances_W_Discards$SpeciesCount," fish species (", NoExceedances_W_Discards$CommonName,").  Although a total of ",NoExceedances_W_Discards$TotalSamples," samples were collected ",NoExceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")

#Remove the total samples field and other fields related to quantitation discards
NoExceedances_W_Discards$TotalSamples<-NULL
NoExceedances_W_Discards$TossedSampleCount<-NULL
NoExceedances_W_Discards$MinDate<-NULL
NoExceedances_W_Discards$MaxDate<-NULL

#Remove the LOEs with quantititation discards from NoExceedances table then create data used field
NoExceedances<-anti_join(No_Exceedances,Quantitation_Discards,by=c("Analyte","StationCode","BUCODE","ProjectName","Matrix","Fraction","Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))


NoExceedances$DATA_USED<-paste0("Water Board staff assessed ",NoExceedances$ProjectName," data for ",NoExceedances$Waterbody, " to determine beneficial use support and the results are as follows ",NoExceedances$EXCEEDANCE_COUNT," of the ",NoExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",NoExceedances$Analyte,". Data were collected for ",NoExceedances$SpeciesCount," fish species (", NoExceedances$CommonName,").")



#Combine all LOEs exceedances togehter
AllLOEs_All<-rbind.data.frame(NoExceedances,NoExceedances_W_Discards)
AllLOEs_All<-rbind.data.frame(AllLOEs_All,ExceedingLOEs)
AllLOEs_All<-rbind.data.frame(AllLOEs_All,ZeroOfZero)



##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
# Mercury_LOE_Generation -----------------------------------------------------------------------------------------
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################


#Calculate date range of the samples that made it to this point
Mercury_Max_Date<-tbl_df(as.data.table(MercuryAveraging)[,max(MaxSampleDate),list(Analyte,BUCODE,ProjectName,TrophicLevel,Matrix,Fraction,StationCode,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(Mercury_Max_Date)[names(Mercury_Max_Date)=="V1"]<-"MaxDate"
Mercury_Min_Date<-tbl_df(as.data.table(MercuryAveraging)[,min(SampleDate),list(Analyte,BUCODE,ProjectName,TrophicLevel,Matrix,Fraction,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(Mercury_Min_Date)[names(Mercury_Min_Date)=="V1"]<-"MinDate"


Mercury_AllDates<-tbl_df(merge(Mercury_Max_Date,Mercury_Min_Date))

#Create table that totals number of fish used in a waterbody based on the trophic level
NumberOfFish<-AveragedMercury
NumberOfFish<-unique(NumberOfFish)
NumberOfFish<-tbl_df(as.data.table(NumberOfFish)[,sum(NumberFish),list(Analyte,BUCODE,ProjectName,Matrix,Fraction,TrophicLevel,Legal,Waterbody,WBID)])
names(NumberOfFish)[names(NumberOfFish)=="V1"]<-"TotalNumberOfFish"


#Make mercury specific tossed tissue table then lookup trophic level of the fish
Mercury_Tossed_Tissue_4_Species<-Tossed_Tissue_4_Species

#Join Tossed_Tissue_4_Species to Tissue_Species to capture the species that were tossed due to quantitation issues
Mercury_Tossed_Tissue_4_Species$TargetLatitude<-NULL
Mercury_Tossed_Tissue_4_Species$TargetLongitude<-NULL
Mercury_Tossed_Tissue_4_Species$Wbtype<-NULL
Mercury_Tossed_Tissue_4_Species<-tbl_df(merge(Mercury_Tossed_Tissue_4_Species,TrophicLevel))
Mercury_Tossed_Tissue_4_Species$Legal<-""

#Create table of tossed mercury data that meets legal size requirments
LegalTossedMercuryAveraging<-tbl_df(merge(Mercury_Tossed_Tissue_4_Species,LegalSizeLimits,all.x=TRUE))
LegalTossedMercuryAveraging$MaxLength_mm[which(is.na(LegalTossedMercuryAveraging$MaxLength_mm))]<-500
LegalTossedMercuryAveraging$MinLength_mm[which(is.na(LegalTossedMercuryAveraging$MinLength_mm)&LegalTossedMercuryAveraging$TrophicLevel==3)]<-150
LegalTossedMercuryAveraging$MinLength_mm[which(is.na(LegalTossedMercuryAveraging$MinLength_mm)&LegalTossedMercuryAveraging$TrophicLevel==4)]<-200
LegalTossedMercuryAveraging<-subset(LegalTossedMercuryAveraging,TLAvgLength_mm>MinLength_mm &TLAvgLength_mm<MaxLength_mm)
LegalTossedMercuryAveraging$Legal<-paste0("This LOE does not contain data from fish who's average length was outside of the legal size limits as described by the California Department of Fish and Wildlife Fishing Regulations.")
LegalTossedMercuryAveraging$MaxLength_mm<-NULL
LegalTossedMercuryAveraging$MinLength_mm<-NULL


LegalTossedMercuryAveraging<-rbind.data.frame(Mercury_Tossed_Tissue_4_Species,LegalTossedMercuryAveraging)

Mercury_Tossed_Tissue_4_Species$TLAvgLength_mm<-NULL
Mercury_Tossed_Tissue_4_Species$AveragingPeroid<-NULL



#Create table of Mercury data for species
MercuryAveraging_4_Species<-MercuryAveraging
MercuryAveraging_4_Species$TargetLatitude<-NULL
MercuryAveraging_4_Species$TargetLongitude<-NULL
MercuryAveraging_4_Species$Wbtype<-NULL
MercuryAveraging_4_Species$TLAvgLength_mm<-NULL
MercuryAveraging_4_Species$AveragingPeroid<-NULL
MercuryAveraging_4_Species$Year<-NULL

#Combine tossed mercury tissue with assessed mercury tissue to generate species list
Mercury_Tissue_Species<-rbind.data.frame(MercuryAveraging_4_Species,Mercury_Tossed_Tissue_4_Species)

#lookup CommonName, then remove FinalID field from Mercury_Tissue_Species table
Mercury_Tissue_Species<-tbl_df(merge(Mercury_Tissue_Species,CommonNameLookup))
Mercury_Tissue_Species$FinalID<-NULL


#Create table of composites listing number of species and number of fish per composite
Mercury_Species<-subset(Mercury_Tissue_Species,select=c("Analyte","StationCode","BUCODE","ProjectName", "Waterbody","WBID", "Matrix","Fraction", "CompositeCompositeID","CommonName","NumberFishperComp","TrophicLevel","Legal"))
Mercury_Species<-unique(Mercury_Species)
Mercury_Species<-tbl_df(as.data.table(Mercury_Species)[,length(CompositeCompositeID),list(Analyte,BUCODE,ProjectName,Waterbody,WBID,Matrix,Fraction,CommonName,NumberFishperComp,TrophicLevel,Legal)])
names(Mercury_Species)[names(Mercury_Species)=="V1"]<-"NumberComposites"
Mercury_Species$CommonName<-paste0(Mercury_Species$NumberComposites," composite(s) of ",Mercury_Species$CommonName, " each composed of ",Mercury_Species$NumberFishperComp," fish per composite")
Mercury_Species<-distinct(select(Mercury_Species,Analyte,BUCODE,ProjectName, Waterbody, WBID, Matrix,Fraction, CommonName,TrophicLevel,Legal))
Mercury_Species<-Mercury_Species%>%dplyr::group_by(Analyte,BUCODE,ProjectName, Waterbody, WBID, Matrix,Fraction,TrophicLevel,Legal)%>%dplyr::summarise(count=n(), CommonName=paste(CommonName,collapse=", "))
names(Mercury_Species)<-c("Analyte","BUCODE","ProjectName", "Waterbody", "WBID", "Matrix","Fraction","TrophicLevel","Legal","SpeciesCount", "CommonName")

#Add total number of Fish assessed on the waterbody to Mercury Species table
Mercury_Species<-tbl_df(merge(Mercury_Species,NumberOfFish))

#Count up exceedances
Mercury_SpeciesExceedances<-tbl_df(as.data.table(AveragedMercury)[,sum(Result>Objective),list(Analyte
	,BUCODE,Unit,ProjectName,Matrix,Fraction,TissueName,TrophicLevel,Legal,Year,Waterbody,WBID
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
Mercury_SpeciesExceedances<-tbl_df(Mercury_SpeciesExceedances[Mercury_SpeciesExceedances$V1>=1,])
Mercury_SpeciesExceedances<-subset(Mercury_SpeciesExceedances,select=c("Analyte","ProjectName", "BUCODE","Waterbody","WBID", "Matrix","TrophicLevel","Legal","Fraction","Objective_Language","Evaluation_Guideline", "Year"))
Mercury_SpeciesExceedances<-distinct(select(Mercury_SpeciesExceedances,Analyte,ProjectName,BUCODE, Waterbody, WBID, Matrix,TrophicLevel,Legal,Fraction,Objective_Language,Evaluation_Guideline, Year))
Mercury_SpeciesExceedances<-Mercury_SpeciesExceedances%>%dplyr::group_by(Analyte,ProjectName,BUCODE, Waterbody, WBID, Matrix,TrophicLevel,Legal,Fraction,Objective_Language,Evaluation_Guideline)%>%dplyr::summarise(count=n(), Year=paste(Year,collapse=", "))
names(Mercury_SpeciesExceedances)<-c("Analyte","ProjectName","BUCODE","Waterbody", "WBID", "Matrix","TrophicLevel","Legal","Fraction","Objective_Language","Evaluation_Guideline","ExceedingSpeciesCount", "ExceedingYear")
Mercury_SpeciesExceedances<-ungroup(Mercury_SpeciesExceedances)


#create table of WB Pollutant combos with exceedances
Mercury_ExceedingSpecies<-tbl_df(merge(Mercury_Species,Mercury_SpeciesExceedances,by=c("Analyte","BUCODE","ProjectName","Waterbody","WBID","Matrix","Fraction","TrophicLevel","Legal")))


#Create table of wb pollutant combos without exceedances
Mercury_Species<-anti_join(Mercury_Species,Mercury_SpeciesExceedances,by=c("Analyte","BUCODE","ProjectName","Waterbody","WBID","Matrix","Fraction","TrophicLevel","Legal"))

#Get table of exceedance counts by waterbody pollutant combination
Mercury_Exceedances<-tbl_df(as.data.table(AveragedMercury)[,sum(Result>Objective),list(Analyte
	,BUCODE,ProjectName,Matrix,Fraction,TrophicLevel,Legal,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(Mercury_Exceedances)[names(Mercury_Exceedances)=="V1"]<-"EXCEEDANCE_COUNT"

#Get table of total samples assessed
Mercury_TotalSamples<-tbl_df(as.data.table(AveragedMercury)[,sum(Result>=0),list(Analyte
	,BUCODE,ProjectName,Matrix,Fraction,TrophicLevel,Legal,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(Mercury_TotalSamples)[names(Mercury_TotalSamples)=="V1"]<-"SAMPLE_COUNT"

#merge samples and exceedances together
Mercury_AllLOEs<-tbl_df(merge(Mercury_Exceedances,Mercury_TotalSamples))

#Add max and min dates to the LOEs for temporal rep
Mercury_AllLOEs<-tbl_df(merge(Mercury_AllLOEs,Mercury_AllDates))

#Add Temporal rep field then remove max and min date fields
Mercury_AllLOEs$TEMPORAL_REP<-paste0("Data for this line of evidence were collected between ", Mercury_AllLOEs$MinDate," and ",Mercury_AllLOEs$MaxDate,".  When composited fish were collected over multiple days, the first day of fish collection was used as the sample date in the LOE, both for LOE writing, and for averaging period purposes.")
Mercury_AllLOEs$MaxDate<-NULL
Mercury_AllLOEs$MinDate<-NULL

#################Zero of zero LOEs####################

#table of LOEs where all samples were thrown out
Mercury_ZeroOfZero<-anti_join(Mercury_Quantitation_Discards,Mercury_AllLOEs,by=c("Analyte"
	,"Waterbody","WBID","ProjectName","Matrix","Fraction","BUCODE","Objective_Language"
	,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))


#Create date range field then remove max and min date field
Mercury_ZeroOfZero$TEMPORAL_REP<-paste0("Data for this line of evidence were collected between ", Mercury_ZeroOfZero$MinDate," and ",Mercury_ZeroOfZero$MaxDate,".  When composited fish were collected over multiple days, the first day of fish collection was used as the sample date in the LOE, both for LOE writing, and for averaging period purposes.")
Mercury_ZeroOfZero$MinDate<-NULL
Mercury_ZeroOfZero$MaxDate<-NULL

#Add species to the end of the zero of zero LOEs
Mercury_ZeroOfZero<-tbl_df(merge(Mercury_ZeroOfZero,Mercury_Species))

#Create the data used field
Mercury_ZeroOfZero$DATA_USED<-paste0("Waterboard staff assessed ",Mercury_ZeroOfZero$ProjectName," data for ",Mercury_ZeroOfZero$Waterbody," to determine beneficial use support and the results are as follows: Zero of Zero samples exceeded the water quality threshold for ",Mercury_ZeroOfZero$Analyte,". Although data was collected for ",Mercury_ZeroOfZero$SpeciesCount," fish species (", Mercury_ZeroOfZero$CommonName,"), all ",Mercury_ZeroOfZero$TossedSampleCount," sample(s) had to be thrown out due to quantitation issues.")
Mercury_ZeroOfZero$SAMPLE_COUNT<-0
Mercury_ZeroOfZero$EXCEEDANCE_COUNT<-0
Mercury_ZeroOfZero$TossedSampleCount<-NULL

#####################LOEs with exceedances###################

#Create table of LOEs with exceedances
Mercury_WithExceedances<-tbl_df(merge(Mercury_AllLOEs,Mercury_ExceedingSpecies))

#Create table of LOEs with exceedances and quantitation discards
Mercury_Exceedances_W_Discards<-tbl_df(merge(Mercury_WithExceedances,Mercury_Quantitation_Discards))


#Add total sample field then add data used field
Mercury_Exceedances_W_Discards<-mutate(Mercury_Exceedances_W_Discards,TotalSamples=SAMPLE_COUNT+TossedSampleCount)
Mercury_Exceedances_W_Discards$DATA_USED<-paste0("Water Board staff assessed ",Mercury_Exceedances_W_Discards$ProjectName," data for ",Mercury_Exceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",Mercury_Exceedances_W_Discards$EXCEEDANCE_COUNT," of the ",Mercury_Exceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_Exceedances_W_Discards$Analyte,". This LOE contains data only for trophic level ",Mercury_Exceedances_W_Discards$TrophicLevel," fish.  Fish collected within the same calendar year and the sample trophic level were aggregated into a single sample for comparison with the objective.", "  A total of ",Mercury_Exceedances_W_Discards$TotalNumberOfFish," fish were aggregated into ",Mercury_Exceedances_W_Discards$SAMPLE_COUNT," annual averages.","  Of these annual averages, ",Mercury_Exceedances_W_Discards$EXCEEDANCE_COUNT," averages exceeded the objective, (",Mercury_Exceedances_W_Discards$ExceedingYear,").  These annual averages consisted of ",Mercury_Exceedances_W_Discards$SpeciesCount," fish species (", Mercury_Exceedances_W_Discards$CommonName,").  Although a total of ",Mercury_Exceedances_W_Discards$TotalSamples," samples were collected ",Mercury_Exceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.",Mercury_Exceedances_W_Discards$Legal)
Mercury_Exceedances_W_Discards$DATA_USED<-ifelse(Mercury_Exceedances_W_Discards$TrophicLevel=="PF1",paste0("Water Board staff assessed ",Mercury_Exceedances_W_Discards$ProjectName," data for ",Mercury_Exceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",Mercury_Exceedances_W_Discards$EXCEEDANCE_COUNT," of the ",Mercury_Exceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_Exceedances_W_Discards$Analyte,". All fish collected between February and July of the same calendar year with a reported length of 50-150 mm were averaged into a single smaple, indepdendent of species or trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_Exceedances_W_Discards$TotalNumberOfFish," fish which were aggregated into ",Mercury_Exceedances_W_Discards$SAMPLE_COUNT," annual averages, consisting of ",Mercury_Exceedances_W_Discards$SpeciesCount," fish species (", Mercury_Exceedances_W_Discards$CommonName,").  Of these annual averages, ",Mercury_Exceedances_W_Discards$EXCEEDANCE_COUNT," average(s), (Year(s): ",Mercury_Exceedances_W_Discards$ExceedingYear,") exceeded the objective.  Although a total of ",Mercury_Exceedances_W_Discards$TotalSamples," samples were collected ",Mercury_Exceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.",Mercury_Exceedances_W_Discards$Legal),Mercury_Exceedances_W_Discards$DATA_USED)
Mercury_Exceedances_W_Discards$DATA_USED<-ifelse(Mercury_Exceedances_W_Discards$TrophicLevel=="PFY",paste0("Water Board staff assessed ",Mercury_Exceedances_W_Discards$ProjectName," data for ",Mercury_Exceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",Mercury_Exceedances_W_Discards$EXCEEDANCE_COUNT," of the ",Mercury_Exceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_Exceedances_W_Discards$Analyte,". All fish collected within the same calendar year with a reported length of 50-150 mm were averaged into a single smaple, indepdendent of species or trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_Exceedances_W_Discards$TotalNumberOfFish," fish which were aggregated into ",Mercury_Exceedances_W_Discards$SAMPLE_COUNT," annual averages, consisting of ",Mercury_Exceedances_W_Discards$SpeciesCount," fish species (", Mercury_Exceedances_W_Discards$CommonName,").  Of these annual averages, ",Mercury_Exceedances_W_Discards$EXCEEDANCE_COUNT," average(s), (Year(s): ",Mercury_Exceedances_W_Discards$ExceedingYear,") exceeded the objective.  Although a total of ",Mercury_Exceedances_W_Discards$TotalSamples," samples were collected ",Mercury_Exceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.",Mercury_Exceedances_W_Discards$Legal),Mercury_Exceedances_W_Discards$DATA_USED)

#Remove the total samples field and other fields related to quantitation discards
Mercury_Exceedances_W_Discards$TotalSamples<-NULL
Mercury_Exceedances_W_Discards$TossedSampleCount<-NULL
Mercury_Exceedances_W_Discards$MinDate<-NULL
Mercury_Exceedances_W_Discards$MaxDate<-NULL

#Remove the LOEs with quantititation discards from Exceedances table then create data used field
Mercury_WithExceedances<-anti_join(Mercury_WithExceedances,Quantitation_Discards,by=c("Analyte","BUCODE","ProjectName","Matrix","Fraction","Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
Mercury_WithExceedances$DATA_USED<-paste0("Water Board staff assessed ",Mercury_WithExceedances$ProjectName," data for ",Mercury_WithExceedances$Waterbody, " to determine beneficial use support and the results are as follows: ",Mercury_WithExceedances$EXCEEDANCE_COUNT," of the ",Mercury_WithExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_WithExceedances$Analyte,".  This LOE contains data only for trophic level ",Mercury_WithExceedances$TrophicLevel," fish.  The concentration of ",Mercury_WithExceedances$Analyte," in fish collected within the same calendar year, for the same trophic level were averaged into a single sample for comparison with the objective.", "  A total of ",Mercury_WithExceedances$TotalNumberOfFish," fish were aggregated into ",Mercury_WithExceedances$SAMPLE_COUNT," annual averages, which consisted of ",Mercury_WithExceedances$SpeciesCount," fish species (", Mercury_WithExceedances$CommonName,").","  Of these annual averages, ",Mercury_WithExceedances$EXCEEDANCE_COUNT," average(s), (Year(s): ",Mercury_WithExceedances$ExceedingYear,") exceeded the objective.",Mercury_WithExceedances$Legal)
Mercury_WithExceedances$DATA_USED<-ifelse(Mercury_WithExceedances$TrophicLevel=="PF1",paste0("Water Board staff assessed ",Mercury_WithExceedances$ProjectName," data for ",Mercury_WithExceedances$Waterbody, " to determine beneficial use support and the results are as follows: ",Mercury_WithExceedances$EXCEEDANCE_COUNT," of the ",Mercury_WithExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_WithExceedances$Analyte,".  All fish collected between February and July of the same calendar year with a reported length of 50-150 mm were averaged into a single sample, independent or species of trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_WithExceedances$TotalNumberOfFish," fish, which were aggregated into ",Mercury_WithExceedances$SAMPLE_COUNT," annual averages, consisting of ",Mercury_WithExceedances$SpeciesCount," fish species (", Mercury_WithExceedances$CommonName,").","  Of these annual averages, ",Mercury_WithExceedances$EXCEEDANCE_COUNT," average(s), (Year(s): ",Mercury_WithExceedances$ExceedingYear,") exceeded the objective.",Mercury_WithExceedances$Legal),Mercury_WithExceedances$DATA_USED)
Mercury_WithExceedances$DATA_USED<-ifelse(Mercury_WithExceedances$TrophicLevel=="PFY",paste0("Water Board staff assessed ",Mercury_WithExceedances$ProjectName," data for ",Mercury_WithExceedances$Waterbody, " to determine beneficial use support and the results are as follows: ",Mercury_WithExceedances$EXCEEDANCE_COUNT," of the ",Mercury_WithExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_WithExceedances$Analyte,".  All fish collected within the same calendar year with a reported length of 50-150 mm were averaged into a single sample, independent of species or trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_WithExceedances$TotalNumberOfFish," fish, which were aggregated into ",Mercury_WithExceedances$SAMPLE_COUNT," annual averages, consisting of ",Mercury_WithExceedances$SpeciesCount," fish species (", Mercury_WithExceedances$CommonName,").","  Of these annual averages, ",Mercury_WithExceedances$EXCEEDANCE_COUNT," average(s), (Year(s): ",Mercury_WithExceedances$ExceedingYear,") exceeded the objective.",Mercury_WithExceedances$Legal),Mercury_WithExceedances$DATA_USED)


#Join exceedances with and without quantitation discards together then remove unecessary fields
Mercury_ExceedingLOEs<-rbind.data.frame(Mercury_Exceedances_W_Discards,Mercury_WithExceedances)
Mercury_ExceedingLOEs$ExceedingSpeciesCount<-NULL
Mercury_ExceedingLOEs$ExceedingCommonName<-NULL
Mercury_ExceedingLOEs$ExceedingYear<-NULL



##########################___LOEs No Exceedances_____###############################

#Create table of LOEs with no exceedances
Mercury_No_Exceedances<-anti_join(Mercury_AllLOEs,Mercury_ExceedingSpecies,by=c("Analyte","BUCODE","ProjectName","Matrix","Fraction","Waterbody","WBID","TrophicLevel"))
Mercury_No_Exceedances<-tbl_df(merge(Mercury_No_Exceedances,Mercury_Species))

#Create table of LOEs with without exceedances that have quantitation discards
Mercury_NoExceedances_W_Discards<-tbl_df(merge(Mercury_No_Exceedances,Mercury_Quantitation_Discards))

#Add total sample field then add data used field
Mercury_NoExceedances_W_Discards<-mutate(Mercury_NoExceedances_W_Discards,TotalSamples=SAMPLE_COUNT+TossedSampleCount)
Mercury_NoExceedances_W_Discards$DATA_USED<-paste0("Water Board staff assessed ",Mercury_NoExceedances_W_Discards$ProjectName," data for ",Mercury_NoExceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",Mercury_NoExceedances_W_Discards$EXCEEDANCE_COUNT," of the ",Mercury_NoExceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_NoExceedances_W_Discards$Analyte,". This LOE contains data only for trophic level ",Mercury_NoExceedances_W_Discards$TrophicLevel," fish.  Fish collected within the same calendar year and the sample trophic level were aggregated into a single sample for comparison with the objective.", "  A total of ",Mercury_NoExceedances_W_Discards$TotalNumberOfFish," fish were aggregated into ",Mercury_NoExceedances_W_Discards$SAMPLE_COUNT," annual averages.  These annual averages consisted of ",Mercury_NoExceedances_W_Discards$SpeciesCount," fish species (", Mercury_NoExceedances_W_Discards$CommonName,").  Although a total of ",Mercury_NoExceedances_W_Discards$TotalSamples," samples were collected ",Mercury_NoExceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.",Mercury_NoExceedances_W_Discards$Legal)
Mercury_NoExceedances_W_Discards$DATA_USED<-ifelse(Mercury_NoExceedances$TrophicLevel=="PF1",paste0("Water Board staff assessed ",Mercury_NoExceedances_W_Discards$ProjectName," data for ",Mercury_NoExceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",Mercury_NoExceedances_W_Discards$EXCEEDANCE_COUNT," of the ",Mercury_NoExceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_NoExceedances_W_Discards$Analyte,". All fish collected between February and July of the same calendar year with a reported length of 50-150 mm were averaged into a single smaple, indepdendent of species or trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_NoExceedances_W_Discards$TotalNumberOfFish," fish which were aggregated into ",Mercury_NoExceedances_W_Discards$SAMPLE_COUNT," annual averages, consisting of ",Mercury_NoExceedances_W_Discards$SpeciesCount," fish species (", Mercury_NoExceedances_W_Discards$CommonName,").  Although a total of ",Mercury_NoExceedances_W_Discards$TotalSamples," samples were collected ",Mercury_NoExceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.",Mercury_NoExceedances_W_Discards$Legal),Mercury_NoExceedances_W_Discards$DATA_USED)
Mercury_NoExceedances_W_Discards$DATA_USED<-ifelse(Mercury_NoExceedances$TrophicLevel=="PFY",paste0("Water Board staff assessed ",Mercury_NoExceedances_W_Discards$ProjectName," data for ",Mercury_NoExceedances_W_Discards$Waterbody, " to determine beneficial use support and the results are as follows ",Mercury_NoExceedances_W_Discards$EXCEEDANCE_COUNT," of the ",Mercury_NoExceedances_W_Discards$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_NoExceedances_W_Discards$Analyte,". All fish collected within the same calendar year with a reported length of 50-150 mm were averaged into a single sample, independent of species or trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_NoExceedances_W_Discards$TotalNumberOfFish," fish which were aggregated into ",Mercury_NoExceedances_W_Discards$SAMPLE_COUNT," annual averages, consisting of ",Mercury_NoExceedances_W_Discards$SpeciesCount," fish species (", Mercury_NoExceedances_W_Discards$CommonName,").  Although a total of ",Mercury_NoExceedances_W_Discards$TotalSamples," samples were collected ",Mercury_NoExceedances_W_Discards$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.",Mercury_NoExceedances_W_Discards$Legal),Mercury_NoExceedances_W_Discards$DATA_USED)


#Remove the total samples field and other fields related to quantitation discards
Mercury_NoExceedances_W_Discards$TotalSamples<-NULL
Mercury_NoExceedances_W_Discards$TossedSampleCount<-NULL
Mercury_NoExceedances_W_Discards$MinDate<-NULL
Mercury_NoExceedances_W_Discards$MaxDate<-NULL

#Remove the LOEs with quantititation discards from NoExceedances table then create data used field
Mercury_NoExceedances<-anti_join(Mercury_No_Exceedances,Quantitation_Discards,by=c("Analyte","BUCODE","ProjectName","Matrix","Fraction","Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
Mercury_NoExceedances$DATA_USED<-paste0("Water Board staff assessed ",Mercury_NoExceedances$ProjectName," data for ",Mercury_NoExceedances$Waterbody, " to determine beneficial use support and the results are as follows: ",Mercury_NoExceedances$EXCEEDANCE_COUNT," of the ",Mercury_NoExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_NoExceedances$Analyte,".  This LOE contains data only for trophic level ",Mercury_NoExceedances$TrophicLevel," fish.  The concentration of ",Mercury_NoExceedances$Analyte," in fish collected within the same calendar year, for the same trophic level were averaged into a single sample for comparison with the objective.", "  A total of ",Mercury_NoExceedances$TotalNumberOfFish," fish were aggregated into ",Mercury_NoExceedances$SAMPLE_COUNT," annual averages, which consisted of ",Mercury_NoExceedances$SpeciesCount," fish species (", Mercury_NoExceedances$CommonName,").  ",Mercury_NoExceedances$Legal)
Mercury_NoExceedances$DATA_USED<-ifelse(Mercury_NoExceedances$TrophicLevel=="PF1",paste0("Water Board staff assessed ",Mercury_NoExceedances$ProjectName," data for ",Mercury_NoExceedances$Waterbody, " to determine beneficial use support and the results are as follows: ",Mercury_NoExceedances$EXCEEDANCE_COUNT," of the ",Mercury_NoExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_NoExceedances$Analyte,".  All fish collected between February and July of the same calendar year with a reported length of 50-150 mm were averaged into a single sample, independent or species of trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_NoExceedances$TotalNumberOfFish," fish, which were aggregated into ",Mercury_NoExceedances$SAMPLE_COUNT," annual averages, consisting of ",Mercury_NoExceedances$SpeciesCount," fish species (", Mercury_NoExceedances$CommonName,").  ",Mercury_NoExceedances$Legal),Mercury_NoExceedances$DATA_USED)
Mercury_NoExceedances$DATA_USED<-ifelse(Mercury_NoExceedances$TrophicLevel=="PFY",paste0("Water Board staff assessed ",Mercury_NoExceedances$ProjectName," data for ",Mercury_NoExceedances$Waterbody, " to determine beneficial use support and the results are as follows: ",Mercury_NoExceedances$EXCEEDANCE_COUNT," of the ",Mercury_NoExceedances$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Mercury_NoExceedances$Analyte,".  All fish collected within the same calendar year with a reported length of 50-150 mm were averaged into a single sample, independent of species or trophic level, for comparison with the objective.  This LOE consists of a total of ",Mercury_NoExceedances$TotalNumberOfFish," fish, which were aggregated into ",Mercury_NoExceedances$SAMPLE_COUNT," annual averages, consisting of ",Mercury_NoExceedances$SpeciesCount," fish species (", Mercury_NoExceedances$CommonName,").  ",Mercury_NoExceedances$Legal),Mercury_NoExceedances$DATA_USED)


#Combine all LOEs exceedances togehter
Mercury_AllLOEs_All<-Mercury_NoExceedances
Mercury_AllLOEs_All<-rbind.data.frame(Mercury_AllLOEs_All,Mercury_NoExceedances_W_Discards)
Mercury_AllLOEs_All<-rbind.data.frame(Mercury_AllLOEs_All,Mercury_ExceedingLOEs)
Mercury_AllLOEs_All<-rbind.data.frame(Mercury_AllLOEs_All,Mercury_ZeroOfZero)


#Add spatial rep field
Mercury_AllLOEs_All$StationCount<-1
Mercury_AllLOEs_All$SPATIAL_REP<-paste0("Data was collected from ",Mercury_AllLOEs_All$StationCount," station(s) with the station code(s): ",Mercury_AllLOEs_All$StationCode,".")
Mercury_AllLOEs_All$StationCount<-NULL
Mercury_AllLOEs_All$StationCode<-NULL
Mercury_AllLOEs_All$StationCodes<-NULL
Mercury_AllLOEs_All$TrophicLevel<-NULL
Mercury_AllLOEs_All$TotalNumberOfFish<-NULL
Mercury_AllLOEs_All$Legal<-NULL

#Re-Order Columns in case there are no non-mercury LOEs in the data set
Mercury_AllLOEs_All<-Mercury_AllLOEs_All[,c("Analyte","ProjectName","Matrix","Fraction","Waterbody","WBID","BUCODE","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number","EXCEEDANCE_COUNT","SAMPLE_COUNT","TEMPORAL_REP","SpeciesCount","CommonName","DATA_USED","SPATIAL_REP")]
Mercury_AllLOEs_All<-tbl_df(Mercury_AllLOEs_All)


# Write Spatial rep field for general LOEs -----------------------------------


AllLOEs_All$StationCount<-1
AllLOEs_All$SPATIAL_REP<-paste0("Data was collected from ",AllLOEs_All$StationCount," station(s) with the station code(s): ",AllLOEs_All$StationCode,".")
AllLOEs_All$StationCount<-NULL
AllLOEs_All$StationCode<-NULL
AllLOEs_All$StationCodes<-NULL

# Combine General LOEs and Mercury LOEs -----------------------------------
AllLOEs_All<-rbind.data.frame(AllLOEs_All,Mercury_AllLOEs_All)



#Lookup data used information then remove project name field
AllLOEs_All<-tbl_df(merge(AllLOEs_All,DataReferences))
AllLOEs_All$ProjectName<-NULL
AllLOEs_All$CommonName<-NULL
AllLOEs_All$SpeciesCount<-NULL


#Change names of fields to match CASSM uploader tool
colnames(AllLOEs_All)<-c("POLLUTANT","BU_CODE","MATRIX","FRACTION","WB_SEGMENT","WBID","CRITERION/OBJECTIVE","EVAL_GUIDELINE","CRITERION/OBJECTIVE_REFERENCES","EVAL_GUIDELINE_REFERENCES","EXCEEDANCE_COUNT","SAMPLE_COUNT","TEMPORAL_REP","DATA_USED","SPATIAL_REP","DATA_USED_REFERENCES","QA_INFO_REFERENCES","QA_INFO","DATA_SOURCE")

#Add other generic fields
AllLOEs_All$REGION<-Region
if(Region=="5SJ"|Region=="5T"){
  AllLOEs_All$REGION<-"5"
}
AllLOEs_All$AUTHOR<-Author
AllLOEs_All$DATE_CREATED<-Sys.Date()
AllLOEs_All$ENVIRONMENTAL_CONDITIONS<-""
AllLOEs_All$ASSESSMENT_STATUS<-"LOE In Progress"
AllLOEs_All$SUB_GROUP<-"Pollutant-Tissue"
#Change sub group to Ancillary Line of Evidence if referenc does not have QAPP when it should
AllLOEs_All$SUB_GROUP[which(AllLOEs_All$QA_INFO_REFERENCES=="4971"|AllLOEs_All$QA_INFO_REFERENCES=="5005"|AllLOEs_All$QA_INFO_REFERENCES=="4686")]<-"Ancillary Line of Evidence"
AllLOEs_All$DATA_TYPE<-"PHYSICAL/CHEMICAL MONITORING"
AllLOEs_All$ASSESSOR_COMMENT<-""
AllLOEs_All$MIGRATION_ID<-rownames(AllLOEs_All)
AllLOEs_All$AQ_USE_CODE<-""
AllLOEs_All$UPDATED_BY<-""
AllLOEs_All$DATE_UPDATED<-""

AllLOEs_All<-tbl_df(merge(AllLOEs_All,ReLEP_to_CalWQA_Lookup,all.x=TRUE))
AllLOEs_All$POLLUTANT<-NULL
names(AllLOEs_All)[names(AllLOEs_All)=="PollutantName_in_CalWQA"]<-"POLLUTANT"

#Re-Order Columns to match LOE uploader template
AllLOEs_All<-AllLOEs_All[,c("MIGRATION_ID","REGION","WB_SEGMENT","WBID","POLLUTANT"
,"SUB_GROUP","MATRIX","FRACTION","BU_CODE","AQ_USE_CODE","CRITERION/OBJECTIVE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE","EVAL_GUIDELINE_REFERENCES","SAMPLE_COUNT","EXCEEDANCE_COUNT","SPATIAL_REP","TEMPORAL_REP","QA_INFO"
,"QA_INFO_REFERENCES","DATA_TYPE","ASSESSOR_COMMENT","DATA_USED","DATA_USED_REFERENCES","ASSESSMENT_STATUS","DATA_SOURCE"
,"AUTHOR","DATE_CREATED","UPDATED_BY","DATE_UPDATED","ENVIRONMENTAL_CONDITIONS")]


#Flag LOEs as electornic generated LOEs
AllLOEs_All$ASSESSOR_COMMENT[AllLOEs_All$ASSESSOR_COMMENT=="NA"]<-""
AllLOEs_All$ASSESSOR_COMMENT<-paste0("(LOE written by ReLEP ", ReLEP_Version,") ", AllLOEs_All$ASSESSOR_COMMENT)


########################___End of LOE writing____##########################################

###########################################################################################

######################___Begin table export section____####################################


#Write LOEs into LOE folder within the ouputs folder
#LOE filename is the name of the filename, the author, and the date the LOEs were written
FileName<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"_R",Region,"_LOEs_",FileNameInQuotations)
write.table(AllLOEs_All,FileName,sep="\t",row.names=FALSE)

#Write other outputs
if(length(SitesNoWaterbodies$WBID)>0){
write.table(SitesNoWaterbodies,"Outputs\\FillInWaterbodyTable.txt",sep="\t",row.names=FALSE)
}

#The tossed data rows may be duplicated because there were multiple issues that resulted in tossing
#turn this into a unique list with comma separated issues and counts of the number of issues
#this will help with prioritization and tracking...hopefully
#first create a list of tissue ids and the issues associated with them
Issues<-subset(AllTossedData,select=c("TisID","Issue"))
Issues<-distinct(select(Issues,TisID,Issue))
Issues<-Issues%>%dplyr::group_by(TisID)%>%dplyr::summarise(count=n(), Issue=paste(Issue,collapse=", "))
names(Issues)<-c("TisID","IssueCount","Issue")

#Then remove the issue field from the tossed data
AllTossedData$Issue<-NULL
AllTossedData<-unique(AllTossedData)

#Then create a unique list of data rows (removing the rows duplicated because of multiple issues)
AllTossedData<-unique(AllTossedData)

#Finally, add back the issues to the tossed data
AllTossedData<-tbl_df(merge(AllTossedData,Issues))
TossedDataName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_R",Region,"_AllTossedData_",FileNameInQuotations)


write.table(AllTossedData,TossedDataName,sep="\t",row.names=FALSE)

#Export SSO data if there is data with SSOs
#use number of rows based on TissueID field
if(length(W_SSOs_For_Export$TisID>0)){
  #Export SSO table
  SSODataName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_R",Region,"_SSO_Data_",FileNameInQuotations)
  write.table(W_SSOs_For_Export,SSODataName,sep="\t",row.names=FALSE)
}

