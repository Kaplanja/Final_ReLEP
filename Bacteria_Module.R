#       _______.___________.    ___      .______     .___________.
#      /       |           |   /   \     |   _  \    |           |
#     |   (----`---|  |----`  /  ^  \    |  |_)  |   `---|  |----`
#      \   \       |  |      /  /_\  \   |      /        |  |     
#  .----)   |      |  |     /  _____  \  |  |\  \----.   |  |     
#  |_______/       |__|    /__/     \__\ | _| `._____|   |__|     
  
#___________________________________________________________________________
#NOTE: Change the working directory both here and at the end of the code to where the data is stored on you PC,
#Change the Author, and
#Create an "Outputs" folder in the working directory prior to running this code.
#___________________________________________________________________________


#Loads libraries for session use
library(plyr)
library(dplyr)
library(data.table)
library(lubridate)
library(rio)

#Removes all defined varibles
rm(list=ls(all=TRUE))

#Create system time so that we can keep track of how long the program takes to run
StartTime<-Sys.time() 

#Sets path to use Rtools to zip up xls/xlsx file with rio
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")

#Sets path to use rJava for package (unsure if explicitly needed)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')



###############################################################################################################################################
#                                             Enter Relevant Information in the below section                                                 #
#                                                                                                                                             #
#                       This should be the only section you need to change anything in to get the program to run                              #
#                                                                                                                                             #
#                                Just so long as you put ReLEP at the correct location on your computer                                       #
#                                                                                                                                             #
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#                                                                                                                                             #
#                                                                                                                                             #
#                                       Input username you use to log on to your computer below                                               #
                                                           Author <- "Jkaplan"                                                                #
#                                                                                                                                             #
#                                                         Input region number below                                                           #
                                                                 Region <- "3"                                                                #
#                                                                                                                                             #
									ReLEP_Version<-"Version 1.1"
#                                                                                                                                             #
#                                                                                                                                             #
#                                                          Enter File Name Below                                                              #
                                        EnterDataFileName<-"BeachWatch_R3.txt"    
#
#                                             Enter minimum samples requred for geomean calculation
										MinimumSamples<-5					      #																							    #
#									   Is data set for AB411? AB411 = Yes ""= No			      #
											AB411<-"AB411"
#                                                                                                                                             #
#                                                                                                                                             #
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#                                                                                                                                             #
#                                                                                                                                             #
#                                                       End of Previous Section                                                               #
#                                                                                                                                             #
###############################################################################################################################################



###############################################################################################################################################
###############################################################################################################################################
#                                                                                                                                             #
#                                   Table naming convention established based on Region and Author                                            #
#                                                                                                                                             #
###############################################################################################################################################
#                                                                                                                                             #
#                                   Set the directory where ReLEP pulls supplemental tables from                                              #
             WorkingDirectory<-paste0("C:\\Users\\",Author,"\\Desktop\\Final_ReLEP\\Region ",Region,"\\Bacteria_Module")                      #
                                                         setwd(WorkingDirectory)                                                              #
#                                                                                                                                             #
#													                                      #
#                                                                                                                                             #
                                         SitesTable<-paste0("R",Region,"_Sites_FINAL.txt")                                                    #
#                                                                                                                                             #
                                   BeneficialUseTable<-paste0("R",Region,"_Beneficial_Use_FINAL.txt")                                         #
#                                                                                                                                             #
                                  DataReferencesTable<-paste0("R",Region,"_Data_Used_DRAFT.txt")                                              #
#                                                                                                                                             #
                                       Language_Table<-paste0("Region_",Region,"_Bacteria_Language.txt")  					      #
#																	      #
						AcceptableSampleTypes<-paste0("R",Region,"_Acceptable_SampleTypeNames.txt")		      #
#                                                                                                                                             #
	     								OceanWaters<-paste0("Ocean_Waters.txt")	                              #
#                                                                                                                                             #
#                                                                                                                                             #
###############################################################################################################################################
#                                                                                                                                             #
#                                                         End Previous Section                                                                #
#                                                                                                                                             #
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################




###############################################################################################################################################
#####################################################Begin Table Loading Section###############################################################
###############################################################################################################################################

#loads test data file
#Bacteria=dataframe
Bacteria<-read.delim(EnterDataFileName,header=TRUE,stringsAsFactors=FALSE)
Bacteria<-tbl_df(Bacteria)

#Load the list of stations we have already mapped
Stations<-read.delim(SitesTable,header=TRUE,stringsAsFactors=FALSE,colClasses=c("FreshMarine"="character"))
Stations<-tbl_df(Stations)
Stations<-Stations[Stations$STATUS=="Completed",]
names(Stations)[names(Stations)=="WaterbodyName"] <- "Waterbody"
Stations$WBType<-substr(Stations$WBID,3,3)
Stations$FreshMarine<-as.character(Stations$FreshMarine)
Stations<-subset(Stations,select=c(Waterbody,StationCode,WBID,WBType,FreshMarine))

#Remove dots in Header created by spaces in read.delim
names(Bacteria) <- gsub("\\.", "", names(Bacteria))

# Change Header in Bacteria to work in the rest of the code
names(Bacteria)[names(Bacteria)=="Analyte"] <- "AnalyteName"
names(Bacteria)[names(Bacteria)=="Fraction"] <- "FractionName"
names(Bacteria)[names(Bacteria)=="Units"] <- "UnitName"
names(Bacteria)[names(Bacteria)=="Matrix"] <- "MatrixName"
names(Bacteria)[names(Bacteria)=="ParentProject"] <- "ProjectName"
names(Bacteria)[names(Bacteria)=="ResultQualCode"] <- "ResQualCode"
names(Bacteria)[names(Bacteria)=="Latitude"] <- "TargetLatitude"
names(Bacteria)[names(Bacteria)=="Longitude"] <- "TargetLongitude"
Bacteria$ProjectName<-Bacteria$ParentProjectName


#Load in BU
Beneficial_Uses <- tbl_df(read.delim(BeneficialUseTable, sep= "\t", header=TRUE,stringsAsFactors=FALSE))
Beneficial_Uses<-subset(Beneficial_Uses,select=c(Waterbody,WBID,BeneficialUse))

#Load in Language
Lang<-tbl_df(read.delim(Language_Table, sep= "\t", header=TRUE,stringsAsFactors=FALSE))
Lang<-subset(Lang,select=c(AnalyteName,BeneficialUse,FreshMarine,GeomeanSTV,Obj_Lang,Obj_Ref))

#Load in data used information
Data_Used_Refs<-tbl_df(read.delim(DataReferencesTable, sep= "\t", header=TRUE))
Data_Used_Refs<-subset(Data_Used_Refs,select=c(ParentProjectName,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO,DATA_SOURCE,Assess))
Data_Used_Refs<-Data_Used_Refs[Data_Used_Refs$Assess=="Yes",]
Data_Used_Refs$Assess<-NULL
#Data_Used_Refs$ParentProjectName<-NULL

##################################################################################################

#Load Acceptable SampleTypeNames Table
Acceptable_SampleTypes<-tbl_df(read.delim(AcceptableSampleTypes,header=TRUE,stringsAsFactors=FALSE))
Acceptable_SampleTypes<-Acceptable_SampleTypes[Acceptable_SampleTypes$Acceptable=="Yes",]
Acceptable_SampleTypes$Acceptable<-NULL

##################################################################################################

####################################End Table loading section#####################################


#_____________________Data cleaninging and Prep section_____________________#

#Remove all data not for bacteria do not export it since this data will be exported as part of the water module
#if it should not be assesed/  In this case we are only concerened with bacteria data

#Cut out all data that isnt related to bacteria
Bacteria<-Bacteria[which(Bacteria$AnalyteName=="E. coli"|Bacteria$AnalyteName=="Enterococcus"|Bacteria$AnalyteName=="Coliform, Total"|Bacteria$AnalyteName=="Coliform, Fecal"),]


#Create a table of data from stations that do not have waterbody
#information assigned in the sties tables
Missing_Stations<-anti_join(Bacteria,Stations,by="StationCode")
Missing_Stations$Issue<-"Missing Stations"
All_Missing_Data<-Missing_Stations

#Create a table of data whos parent project does not appear in the data used table
MissingQAPP<-anti_join(Bacteria,Data_Used_Refs,by=c("ProjectName"="ParentProjectName"))
MissingQAPP$Issue<-"ParentProjectName Missing from DataUsed table or marked as do not assess."
All_Missing_Data<-rbind.data.frame(All_Missing_Data,MissingQAPP)


#If AB411 = TRUE flag data outside of dry season as unincluded in LOEs
if(AB411=="AB411"){
NotAB411<-Bacteria
NotAB411$Month<-mdy(NotAB411$SampleDate)
NotAB411$Month<-month(NotAB411$Month)
NotAB411<-filter(NotAB411,((Month<4|Month>10)&AnalyteName!="Coliform, Total"))
NotAB411$Month<-NULL
NotAB411$Issue<-"Wet weather data not included in LOE b/c of AB411"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,NotAB411)
}

#Remove data for incorrect matrix
BadMatrix<-filter(Bacteria,!(MatrixName=="samplewater"|MatrixName=="Surface Water"))
BadMatrix$Issue<-"Check MatrixName"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,BadMatrix)

#Remove bad matrix data from bacteria data frame
Bacteria<-filter(Bacteria, (MatrixName=="samplewater"|MatrixName=="Surface Water"))


#Remove data with no/wrong units
############MPN and CFU are more or less synonymous for assessment purposes
#############See email from Helen for details
Missing_Units<- filter(Bacteria, ((UnitName=='NA'|UnitName==''|UnitName==' ')|!(UnitName=="cfu/100mL"|UnitName=="MPN/100 mL"|UnitName=="MPN/100ml"|UnitName=="CFU/100ml")))
Missing_Units$Issue<-"Missing Units"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,Missing_Units)

#Create a table of bad ResQualCodes
Missing_ResQualCode<-filter(Bacteria, ((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode))&(RL=="-88"|RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="-88"|MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))
BadResQualCode<-filter(Bacteria,(ResQualCode=="M"|ResQualCode=="NA"))
FunkyResQualCode<-filter(Bacteria,(ResQualCode==">"&Result<=0))
Missing_ResQualCode<-tbl_df(rbind.data.frame(Missing_ResQualCode,BadResQualCode))
Missing_ResQualCode<-tbl_df(rbind.data.frame(Missing_ResQualCode,FunkyResQualCode))
Missing_ResQualCode$Issue<-"Missing or incorrect ResQualCode"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,Missing_ResQualCode)

#Flag weird red qual codes
WeirdResQualCode<-tbl_df(filter(Bacteria, (!(ResQualCode=="="|ResQualCode=="ND"|ResQualCode=="DNQ"|ResQualCode=="NR"|ResQualCode=="<")&Result<=0)))
WeirdResQualCode$Issue<-"Check ResQualCode and Result"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,WeirdResQualCode)

#flag data with insuficient sentitivity for assessment
HighRL<-tbl_df(filter(Bacteria,(ResQualCode=="ND"& RL>2)))
HighRL$Issue<-"Sample likely over diluted"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,HighRL)

############################################################################################################################
#Remove data with wrong sample types
BadSampleType<-anti_join(Bacteria,Acceptable_SampleTypes)
BadSampleType$Issue<-"Bad Sample Type"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,BadSampleType)
############################################################################################################################

#Remove data with QA codes that required review
BadQACodes<-filter(Bacteria,DataQuality!="Passed QC")
BadQACodes$Issue<-"Check DataQuality Field"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,BadQACodes)

#Flag Total Coliform data with bad method Colilert
##Remove IDEXX Colilert test for Total Coliform because they often result in false positives
MalMetodo<-Bacteria[Bacteria$AnalyteName=="Coliform, Total",]
MalMetodo<-filter(MalMetodo,MethodName=="Colilert"|MethodName=="Colilert-18"|
                                MethodName=="SM 9223"|MethodName=="SM 9223 B"|
                                MethodName=="SM 9223 B, SM 9230D b"|MethodName=="SM 9223 B-SOP ASWAU"|
                                MethodName=="SM 9223 B-SOP1103"|MethodName=="Enterolert"|
                                MethodName=="Colilert 18 (Fecal)"|MethodName=="Colilert 18 (Total)"|MethodName=="Colilert 18"|
                                MethodName=="Colilert 18 Fecals"|MethodName=="Colilert 18 E. coli"|MethodName=="SM9223")

MalMetodo$Issue<-"Check MethodName"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,MalMetodo)



#Flag data missing objectives in Region by first creating table of data with objectives
#Then using that to create a table of all rows that are not in the table of rows WITH objectives
HaveObjectives<-tbl_df(merge(Bacteria,Stations))
Beneficial_Uses<-filter(Beneficial_Uses,BeneficialUse=="SH"|BeneficialUse=="R1"|BeneficialUse=="R2")
if(Region!="3"){
Beneficial_Uses<-filter(Beneficial_Uses,BeneficialUse=="SH"|BeneficialUse=="R1")  
}
HaveObjectives<-tbl_df(merge(HaveObjectives,Beneficial_Uses,by=c("Waterbody","WBID")))
HaveObjectives<-tbl_df(merge(HaveObjectives,Lang))
HaveObjectives<-tbl_df(unique(HaveObjectives$WQID))
names(HaveObjectives)[names(HaveObjectives)=="value"]<-"WQID"
NoObjectives<-tbl_df(anti_join(Bacteria,HaveObjectives))
NoObjectives$Issue<-"Data does not have objective"
NoObjectives<-tbl_df(unique(NoObjectives))
All_Missing_Data<-rbind.data.frame(All_Missing_Data,NoObjectives)


#Flag fecal data in Region 1 from waterbodies with SH beneficial use
if(Region==1){
FecalShell<-tbl_df(merge(Bacteria,Stations))
Beneficial_Uses<-filter(Beneficial_Uses,BeneficialUse=="SH"|BeneficialUse=="R1")
FecalShell<-tbl_df(merge(FecalShell,Beneficial_Uses,by=c("Waterbody","WBID")))
FecalShell<-tbl_df(filter(FecalShell,(substr(FecalShell$WBID,4,4)==1)&AnalyteName=="Coliform, Fecal"&BeneficialUse=="SH"&FreshMarine=="F"))
FecalShell$WBID<-NULL
FecalShell$WBType<-NULL
FecalShell$BeneficialUse<-NULL
FecalShell$FreshMarine<-NULL
FecalShell$Waterbody<-NULL
FecalShell$Issue<-"Data MAY have an objective.  Treat as SSO.  Check location of station"
All_Missing_Data<-rbind.data.frame(All_Missing_Data,FecalShell)
}


#Remove data with bad res qual codes from the main data table
Bacteria<-filter(Bacteria, !((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode))&(RL=="-88"|RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="-88"|MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))
Bacteria<-filter(Bacteria,!(ResQualCode=="M"|ResQualCode=="NA"))
Bacteria<-filter(Bacteria,!(ResQualCode==">"&Result<=0))

#Remove Weird res qual code data
Bacteria<-tbl_df(filter(Bacteria, !(!(ResQualCode=="="|ResQualCode=="ND"|ResQualCode=="DNQ"|ResQualCode=="NR"|ResQualCode=="<")&Result<=0)))

#Remove ND's with high RLs.  THey were probably diluted incorrectly
Bacteria<-tbl_df(filter(Bacteria,!(RL>2&ResQualCode=="ND")))

#Convert negative numbers to = 1/2 of MDL
Bacteria$Result[which(Bacteria$Result<=0)]<-Bacteria$MDL[which(Bacteria$Result<=0)]*.5
Bacteria$Result[which(is.na(Bacteria$Result)&Bacteria$ResQualCode!="=")]<-Bacteria$MDL[which(is.na(Bacteria$Result)&Bacteria$ResQualCode!="=")]*.5
Bacteria$Result[which(is.na(Bacteria$Result))]<-.0001
Bacteria<-tbl_df(Bacteria)

#Remove data with unsuitable QA codes
Bacteria<-filter(Bacteria,DataQuality=="Passed QC")

#Remove data missing correct units
Bacteria<- filter(Bacteria, ((UnitName=="cfu/100mL"|UnitName=="MPN/100 mL"|UnitName=="MPN/100ml"|UnitName=="CFU/100ml")))


############################################################################################################################
Bacteria<-tbl_df(merge(Bacteria,Acceptable_SampleTypes))
############################################################################################################################

#Change ParentProjectName to ProjectName
#Only doing this because it is easier to program this way than to change
#Each time ProjectName appears in the code
Bacteria$ProjectName<-Bacteria$ParentProjectName

#Remove unecessary fields, select out from data only the fields needed
Bacteria<-subset(Bacteria,select=c("ProjectName","StationCode","SampleDate","MatrixName","AnalyteName","FractionName","UnitName","Result","MDL","RL","ResQualCode","QACode","MethodName","TargetLatitude","TargetLongitude"))


#Creates new column in Bacteria with name "SampleDate2" based on "SampleDate" in format (mdy) this is important for assessment
#If the date field is in the incorrect format the computer cannot properly manipulate it
Bacteria$SampleDate2<-mdy(Bacteria$SampleDate)

#This code converts the sample date into a useable format
#Converts the date into weeks and years so that we can use the first day of the week
#to set the geomean interval start time
#It does this by multiplying the week number by 7 to get the day number
#for leap years this reults in days with values greater than 365 days
#We convert these numbers back to 365

Bacteria$Week<-week(Bacteria$SampleDate2)
Bacteria$Year<-year(Bacteria$SampleDate2)

#Code that converts sample date in to the first day of the week associated with the orignal sample date
#This date is used to assess the geomean on a weekly basis instead of daily
Bacteria$Day<-Bacteria$Week*7
Bacteria$Day[which(Bacteria$Day>365)]<-365
Bacteria$IntervalStart<-(paste(Bacteria$Day,Bacteria$Year,sep="-"))
Bacteria$IntervalStart<-as.Date(Bacteria$IntervalStart,"%j-%Y")
Bacteria$Week<-NULL
Bacteria$Year<-NULL
Bacteria$Day<-NULL

if(AB411=="AB411"){
Bacteria$Month<-month(Bacteria$SampleDate2)
Bacteria<-filter(Bacteria,((Month>3&Month<11)|AnalyteName=="Coliform, Total"))
Bacteria$Month<-NULL
Bacteria<-tbl_df(Bacteria)
}

#Remove samples collected at the same Station, for the same Matrix, Unit, Analyte, Project, on the same day (Replicates)
Bacteria_No_reps<-as.data.table(Bacteria)[,mean(Result),list(AnalyteName,UnitName,StationCode,ProjectName,SampleDate,SampleDate2,MatrixName,FractionName,TargetLatitude,TargetLongitude,RL,MDL,QACode,MethodName,ResQualCode,IntervalStart)]
names(Bacteria_No_reps)[names(Bacteria_No_reps)=="V1"]<-"Result"

#Create table of station information to be joined back with information at end when writing LOEs
#StationInfo<-tbl_df(merge(Bacteria_No_reps,Stations,by=c("StationCode")))
#StationInfo<-distinct(select(StationInfo,AnalyteName,ProjectName,MatrixName,FractionName,StationCode,Waterbody,WBID,WBType))
#StationInfo<-StationInfo%>%group_by(AnalyteName,Waterbody,WBID,ProjectName,MatrixName,FractionName,WBType)%>%dplyr::summarise(count=n(),paste(StationCode,collapse=", "))
#names(StationInfo)<-c("AnalyteName","Waterbody","WBID","ProjectName","MatrixName","FractionName","WBType","StationCount","StationCode")
#StationInfo<-tbl_df(StationInfo)


#Calculate the Date range for the geomean data for use in the TEMPORAL_REP section of the LOE
#Save bacteria_no_reps as bacteria while we create the new date range table "bacteria date range"
TemporaryBacteria<-Bacteria_No_reps
Bacteria_No_reps<-tbl_df(merge(Bacteria_No_reps,Stations,by=c("StationCode")))
BacteriaMaxDate<-as.data.table(Bacteria_No_reps)[,max(SampleDate2),list(AnalyteName,StationCode,WBType,ProjectName,MatrixName,FractionName,Waterbody,WBID)]
names(BacteriaMaxDate)[names(BacteriaMaxDate)=="V1"]<-"MaxDate"
BacteriaMinDate<-as.data.table(Bacteria_No_reps)[,min(SampleDate2),list(AnalyteName,StationCode,WBType,ProjectName,MatrixName,FractionName,Waterbody,WBID)]
names(BacteriaMinDate)[names(BacteriaMinDate)=="V1"]<-"MinDate"
BacteriaDateRange<-tbl_df(merge(BacteriaMaxDate,BacteriaMinDate,by=c("AnalyteName","StationCode","WBType","ProjectName","MatrixName","FractionName","Waterbody","WBID")))


############################################################################################################################
OceanStations<-tbl_df(Stations[Stations$FreshMarine=="O",])
Non_Ocean_Stations<-tbl_df(Stations[which(Stations$FreshMarine=="F"|Stations$FreshMarine=="M"),])

Bacteria_No_reps<-tbl_df(TemporaryBacteria)

#Split data into Ocean and Non Ocean waters
Ocean_Data<-anti_join(Bacteria_No_reps,Non_Ocean_Stations)



############################################################################################################################





#_______________________________Six Week Rolling Geomean code_______________________________#

#We chose to use calendar week for weekly caclulation.  To do this we convert the date to the number of seven day peroids since January 1 of that year

#Create new data frame from Bacteria_No_Reps for the six week geomean assessment
SixWeek<-Bacteria_No_reps

#Cut out all data that isn't E. coli and Enterococcus
SixWeek<-tbl_df(SixWeek[which(SixWeek$AnalyteName=="E. coli"|SixWeek$AnalyteName=="Enterococcus"),])

#Creates new columns in SixWeek with name "dateinterval" which is an interval of 42 days
# first sample date plus the following 41 days
SixWeek$dateinterval<-interval(SixWeek$IntervalStart-days(42),SixWeek$IntervalStart)

#Remove unecessary fields
SixWeek<-subset(SixWeek,select=c("AnalyteName","UnitName","StationCode","ProjectName","SampleDate","SampleDate2","MatrixName","FractionName","Result","MethodName","IntervalStart","dateinterval"))

#Assigns a new dataframe, SixWeek2, which is created from splitting SixWeek based on unique 
#"StationCode", "Analyte", "Unit", "MatrixName" combinations and applying a function
#to the dataframe

#The function creates two empty columns, SixWeekGeoMean and
#SixWeekN (number of samples avged over) filled with NA to be filled with the loop below.

#Loop repeats for i=1 to i=length of SampleDate.

#Fills ith row with values obtained from loop functions.

#Returns dataframe SixWeek2 with columns "StationCode", "AnalyteName", "UnitName", "MatrixName", "dateinterval"
#"SixWeekGeomean", "Date", and "SixWeekN"
SixWeek2<-ddply(SixWeek, c("AnalyteName", "UnitName","StationCode", "ProjectName","MatrixName","FractionName"),function(df){
  SixWeekGeoMean<-rep(NA, length(df$SampleDate))
  SixWeekN<-rep(NA, length(df$SampleDate))
  
  for(i in 1:length(df$SampleDate)){
	SixWeekN[i]<-length(df$Result[df$SampleDate2%within%df$dateinterval[i]])
    SixWeekGeoMean[i]<-prod(df$Result[df$SampleDate2%within%df$dateinterval[i]]^(1/SixWeekN[i]), na.rm=T)
    
  }
  return(data.frame(Result=SixWeekGeoMean, SampleDate=as.character(df$IntervalStart), SixWeekN=SixWeekN))
}
)

#Filter out duplicate results made from loop and write to file
SixWeek2<-tbl_df(SixWeek2)
SixWeek2<-distinct(SixWeek2,StationCode,AnalyteName,UnitName,MatrixName,FractionName,ProjectName,SampleDate,.keep_all=TRUE)


#######_______________________________Assess with New Objectives (Draft Staff Report Values)_________________###################

#Add waterbody information
SixWeekGeo<-tbl_df(merge(SixWeek2,Stations,by=c("StationCode")))

#Remove sampling peroids with less than the minimum number of samples
SixWeekGeo<-SixWeekGeo[which(SixWeekGeo$SixWeekN>=MinimumSamples),]

#Create new data frame of the objectives to be used in this assessment
AnalyteName<-c("E. coli","Enterococcus","Enterococcus")
Objective<-c(100,30,30)
FreshMarine<-c("F","M","O")
NewObjective<-data.frame(AnalyteName,Objective,FreshMarine)

#Add objective to the data
SixWKGeo<-tbl_df(merge(SixWeekGeo,NewObjective, by=c("AnalyteName","FreshMarine")))


#Change objectives of data that applies to Lake Tahoe
SixWKGeo$Objective[which(SixWKGeo$Waterbody=="Tahoe, Lake"&SixWKGeo$AnalyteName=="E. coli")]<-17

# Calulate the number of exceedances by comapring the result with the objective
SixWKGeo$Objective<-as.numeric(SixWKGeo$Objective)
Over<-as.data.table(SixWKGeo)[,sum(Result>Objective),list(AnalyteName,StationCode,ProjectName,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Over)[names(Over)=='V1']<-"Exceedances"
Over<-tbl_df(Over)

# Calulate the total number of samples collected
SixWKGeo$Objective<-as.numeric(SixWKGeo$Objective)
Total<-as.data.table(SixWeekGeo)[,sum(Result>0),list(AnalyteName,StationCode,ProjectName,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Total)[names(Total)=='V1']<-"Total"
Total<-tbl_df(Total)

#Combined exceedance count and total sample count together based on common pollutant, matrix, waterbody, WBID, freshmarine combinations
SixWKResults<-tbl_df(merge(Over,Total,by=c("AnalyteName","StationCode","MatrixName","Waterbody","WBID","FreshMarine","ProjectName","FractionName","WBType")))

#SixWKResults$WBType <- NULL
SixWKResults$FractionName <- NULL

#add field that specifies this was a geomean assessment
SixWKResults$GeomeanSTV<-"Geomean"




###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################

#####################_____________________________________________Applies to Region 6 ONLY_________________________________________________####################################

###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################


#################################______________________________Monthly Log mean code_______________________________############################################################



#Create new data frame from Bacteria_No_Reps for the six week geomean assessment
LogMean<-tbl_df(Bacteria_No_reps)

#Cut out all data that isn't Fecal Coliform
LogMean<-LogMean[which(LogMean$AnalyteName=="Coliform, Fecal"),]

#Remove Interval Start based on week of the year, and
#replace it with Interval Start that is euqal to the sample date
LogMean$IntervalStart<-NULL
LogMean$IntervalStart<-LogMean$SampleDate2

#Creates new columns in SixWeek with name "dateinterval" which is an interval of 30 days
# first sample date plus the following 29 days
LogMean$dateinterval<-interval(LogMean$IntervalStart-days(30),LogMean$IntervalStart)

#Remove unecessary fields
LogMean<-subset(LogMean,select=c("AnalyteName","UnitName","StationCode","ProjectName","SampleDate","SampleDate2","MatrixName","FractionName","Result","MethodName","IntervalStart","dateinterval"))

#Assigns a new dataframe, SixWeek2, which is created from splitting LogMean based on unique 
#"StationCode", "Analyte", "Unit", "MatrixName" combinations and applying a function
#to the dataframe

#The function creates two empty columns, SixWeekGeoMean and
#LogMeanN (number of samples avged over) filled with NA to be filled with the loop below.

#Loop repeats for i=1 to i=length of SampleDate.

#Fills ith row with values obtained from loop functions.


#Returns dataframe SixWeek2 with columns "StationCode", "AnalyteName", "UnitName", "MatrixName", "dateinterval"
#"SixWeekGeomean", "Date", and "SixWeekN"
LogMean2<-ddply(LogMean, c("AnalyteName", "UnitName","StationCode", "ProjectName","MatrixName","FractionName"),function(df){
  LogMeanMean<-rep(NA, length(df$SampleDate))
  LogMeanN<-rep(NA, length(df$SampleDate))
  
  for(i in 1:length(df$SampleDate)){
	LogMeanN[i]<-length(df$Result[df$SampleDate2%within%df$dateinterval[i]])
    LogMeanMean[i]<-prod(df$Result[df$SampleDate2%within%df$dateinterval[i]]^(1/LogMeanN[i]), na.rm=T)
    
  }
  return(data.frame(Result=LogMeanMean, SampleDate=as.character(df$IntervalStart), LogMeanN=LogMeanN))
}
)

#Filter out duplicate results made from loop and write to file
LogMean2<-tbl_df(LogMean2)
LogMean2<-distinct(LogMean2,StationCode,AnalyteName,UnitName,MatrixName,FractionName,ProjectName,SampleDate,.keep_all=TRUE)


#######_______________________________Assess with New Objectives (Draft Staff Report Values)_________________###################

#Add waterbody information
LogMean<-tbl_df(merge(LogMean2,Stations,by=c("StationCode")))

#Add objective to table
LogMean$Objective<-20

# Calulate the number of exceedances by comapring the result with the objective
LogMean$Objective<-as.numeric(LogMean$Objective)
Over<-as.data.table(LogMean)[,sum(Result>Objective),list(AnalyteName,ProjectName,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Over)[names(Over)=='V1']<-"Exceedances"
Over<-tbl_df(Over)

# Calulate the total number of samples collected
LogMean$Objective<-as.numeric(LogMean$Objective)
Total<-as.data.table(LogMean)[,sum(Result>0),list(AnalyteName,ProjectName,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Total)[names(Total)=='V1']<-"Total"
Total<-tbl_df(Total)

#Combined exceedance count and total sample count together based on common pollutant, matrix, waterbody, WBID, freshmarine combinations
LogMean<-tbl_df(merge(Over,Total,by=c("AnalyteName","MatrixName","Waterbody","WBID","FreshMarine","ProjectName","FractionName","WBType")))

LogMean$FractionName <- NULL

#add field that specifies this was a geomean assessment
LogMean$GeomeanSTV<-"Geomean"



LogMean$Region<-substr(LogMean$WBID,4,4)
LogMean<-LogMean[which(LogMean$Region=="6"),]
LogMean$Region<-NULL



#_____________________________\/10% Fecal Region 6\/_____________________________#

Fecal_Data<-Bacteria_No_reps

#Cut out all data that isn't Total Coliform
Fecal_Data<-Fecal_Data[which(Fecal_Data$AnalyteName=="Coliform, Fecal"),]


#Remove unecessary fields
Fecal_Data<-subset(Fecal_Data,select=c("ProjectName","StationCode","SampleDate","MatrixName","AnalyteName","UnitName","Result","TargetLatitude","TargetLongitude"))  

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
Fecal_Data$SampleDate2<-mdy(Fecal_Data$SampleDate)

#Remove samples collected at the same Station, for the same Matrix, Unit, Analyte, Project, on the same day
Fecal_Data<-as.data.table(Fecal_Data)[,mean(Result),list(ProjectName,StationCode,SampleDate,MatrixName,AnalyteName,UnitName)]
names(Fecal_Data)[names(Fecal_Data)=="V1"]<-"Result"

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
Fecal_Data$SampleDate2<-mdy(Fecal_Data$SampleDate)

#Convert sample date into Month and year values
#this will allow exccedance ratio tests on a monthly basis
Fecal_Data$Month<-month(Fecal_Data$SampleDate2)
Fecal_Data$Year<-year(Fecal_Data$SampleDate2)

Fecal_Data$Objectives<-40


#Add necessary information to add objectives then add objectives
Fecal_Data<-tbl_df(merge(Fecal_Data,Stations,by=c("StationCode")))


#Calculate exceedances on a daily basis first then add the exceedances up for each month
OverFecal<-as.data.table(Fecal_Data)[,sum(Result>Objectives),list(AnalyteName,MatrixName,StationCode,Waterbody,WBID,ProjectName,WBType,FreshMarine,SampleDate2,Month,Year)]
names(OverFecal)<-c("AnalyteName","Matrix","StationCode","Waterbody","WBID","ProjectName","WBType","FreshMarine","SampleDate2","Month","Year","Exceedances")
MonthyEFecal<-as.data.table(OverFecal)[,sum(Exceedances),list(AnalyteName,Matrix,StationCode,Waterbody,WBID,ProjectName,WBType,FreshMarine,Month,Year)]
names(MonthyEFecal)<-c("AnalyteName","Matrix","StationCode","Waterbody","WBID", "ProjectName","WBType","FreshMarine","Month","Year","Exceedances")
MonthyEFecal<-tbl_df(MonthyEFecal)
OverFecal<-tbl_df(OverFecal)

#Count the number of samples for each day and then add each sample together for each month
TotalFecal<-as.data.table(Fecal_Data)[,sum(Result>0),list(AnalyteName,MatrixName,StationCode,Waterbody,WBID,ProjectName,WBType,FreshMarine,SampleDate2,Month,Year)]
names(TotalFecal)<-c("AnalyteName","Matrix","StationCode","Waterbody","WBID","ProjectName","WBType","FreshMarine","SampleDate2","Month","Year","Total")
MonthyTFecal<-as.data.table(TotalFecal)[,sum(Total),list(AnalyteName,Matrix,StationCode,Waterbody,WBID,ProjectName,WBType,FreshMarine,Month,Year)]
names(MonthyTFecal)<-c("AnalyteName","Matrix","StationCode","Waterbody","WBID","ProjectName","WBType","FreshMarine","Month","Year","Total")
MonthyTFecal<-tbl_df(MonthyTFecal)
TotalFecal<-tbl_df(TotalFecal)

#add the exceedance and the sample fields to the table calculate the exceedance ratio on a monthly basis
FecalCounts<-tbl_df(merge(MonthyEFecal,MonthyTFecal,by=c("AnalyteName","Waterbody","StationCode","Matrix","WBID","ProjectName","WBType","FreshMarine","Month","Year")))
FecalCounts<-mutate(FecalCounts, ratio=Exceedances/Total)
FecalCounts$Objective<-0.1
FecalCounts$Exceedances[FecalCounts$ratio >= FecalCounts$Objective]<-1
FecalCounts$Exceedances[FecalCounts$ratio < FecalCounts$Objective]<-0
FecalCounts$Total<-1

#add field that counts each time the exceedance ratio is above .1 at the end of the month
FecalExceedances<-as.data.table(FecalCounts)[,sum(Exceedances),list(AnalyteName, Waterbody, Matrix, WBID, ProjectName, WBType, FreshMarine)]
names(FecalExceedances)<-c("AnalyteName","Waterbody","MatrixName","WBID", "ProjectName","WBType","FreshMarine","Exceedances")

#Count each month as 1 sample
FecalTotal<-as.data.table(FecalCounts)[,sum(Total),list(AnalyteName,Waterbody,Matrix,WBID,ProjectName, WBType, FreshMarine)]
names(FecalTotal)<-c("AnalyteName","Waterbody","MatrixName","WBID", "ProjectName","WBType","FreshMarine","Total")

#add samples and exceedances to be in the same table
FinalTotalFecal<-tbl_df(merge(FecalExceedances,FecalTotal,by=c("AnalyteName","Waterbody","WBID","ProjectName","WBType","MatrixName","FreshMarine")))

#Add field that identifies this as a STV assessment
FinalTotalFecal$GeomeanSTV<-"STV"


Results_Fecal <- FinalTotalFecal
Results_Fecal <- Results_Fecal[,c("AnalyteName", "Waterbody", "WBID", "ProjectName", "MatrixName", "WBType", "FreshMarine", "Exceedances", "Total", "GeomeanSTV")]

Results_Fecal$Region<-substr(Results_Fecal$WBID,4,4)
Results_Fecal<-Results_Fecal[which(Results_Fecal$Region=="6"),]
Results_Fecal$Region<-NULL


All_R6_FecalAssessments<-rbind.data.frame(Results_Fecal,LogMean)

All_R6_FecalAssessments<-tbl_df(merge(All_R6_FecalAssessments,Beneficial_Uses))
All_R6_FecalAssessments<-All_R6_FecalAssessments[All_R6_FecalAssessments$BeneficialUse=="R1"|All_R6_FecalAssessments$BeneficialUse=="SH",]




###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################

#######################################________________________/\________End of Region 6 Specific Section_______/\____________________________################################

###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
########################                      \/  REC STV ISWEBE Section     \/                      ##########################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


#_____________________________Calculate STV for New Objectives (Draft Staff Report Values, R1)_______________#

NewSTV<-Bacteria_No_reps

#Cut out all data that isn't E. coli and Enterococcus
NewSTV<-NewSTV[which(NewSTV$AnalyteName=="E. coli"|NewSTV$AnalyteName=="Enterococcus"),]

#Remove unecessary fields
NewSTV<-subset(NewSTV,select=c("ProjectName","StationCode","SampleDate","MatrixName","AnalyteName","UnitName","Result","MDL","RL","ResQualCode","QACode","MethodName","TargetLatitude","TargetLongitude"))  

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
NewSTV$SampleDate2<-mdy(NewSTV$SampleDate)


#Remove samples collected at the same Station, for the same Matrix, Unit, Analyte, Project, on the same day
NewSTV<-as.data.table(NewSTV)[,mean(Result),list(ProjectName,StationCode,SampleDate,MatrixName,AnalyteName,UnitName)]
names(NewSTV)[names(NewSTV)=="V1"]<-"Result"

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
NewSTV$SampleDate2<-mdy(NewSTV$SampleDate)

#Convert sample date into Month and year values
#this will allow exccedance ratio tests on a monthly basis
NewSTV$Month<-month(NewSTV$SampleDate2)
NewSTV$Year<-year(NewSTV$SampleDate2)


############################_____________________________Assess using New Objectives (Draft Staff Report Values, Rec1)____________________________________________#########################
#Create objectives data frame for objectives comparison
AnalyteName<-c("E. coli","Enterococcus","Enterococcus")
Objectives<-c(320,110,110)
FreshMarine<-c("F","M","O")
STVObjectives<-data.frame(AnalyteName,Objectives,FreshMarine)


#Add necessary information to add objectives then add objectives
NewSTV<-tbl_df(merge(NewSTV,Stations,by=c("StationCode")))
NewSTV<-tbl_df(merge(NewSTV,STVObjectives, by=c("AnalyteName","FreshMarine")))

NewSTV$Objectives[which(NewSTV$Waterbody=="Tahoe, Lake"&NewSTV$AnalyteName=="E. coli")]<-55

#Calculate exceedances on a daily basis first then add the exceedances up for each month
OverSTV<-as.data.table(NewSTV)[, sum(Result>Objectives), list(NewSTV$AnalyteName,NewSTV$StationCode,NewSTV$MatrixName,NewSTV$Waterbody,NewSTV$WBID,NewSTV$ProjectName,NewSTV$WBType,NewSTV$FreshMarine,NewSTV$SampleDate2,NewSTV$Month,NewSTV$Year)]
names(OverSTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID","ProjectName","WBType","FreshMarine","SampleDate2","Month","Year","Exceedances")
MonthyESTV<-as.data.table(OverSTV)[,sum(Exceedances),list(OverSTV$AnalyteName,OverSTV$StationCode,OverSTV$Matrix,OverSTV$Waterbody,OverSTV$WBID,OverSTV$ProjectName,OverSTV$WBType,OverSTV$FreshMarine,OverSTV$Month,OverSTV$Year)]
names(MonthyESTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID", "ProjectName","WBType","FreshMarine","Month","Year","Exceedances")
MonthyESTV<-tbl_df(MonthyESTV)
OverSTV<-tbl_df(OverSTV)

#Count the number of samples for each day and then add each sample together for each month
TotalSTV<-as.data.table(NewSTV)[, sum(Result>0),list(NewSTV$AnalyteName,NewSTV$StationCode,NewSTV$MatrixName,NewSTV$Waterbody,NewSTV$WBID,NewSTV$ProjectName,NewSTV$WBType,NewSTV$FreshMarine,NewSTV$SampleDate2,NewSTV$Month,NewSTV$Year)]
names(TotalSTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID","ProjectName","WBType","FreshMarine","SampleDate2","Month","Year","Total")
MonthyTSTV<-as.data.table(TotalSTV)[,sum(Total),list(TotalSTV$AnalyteName,TotalSTV$StationCode,TotalSTV$Matrix,TotalSTV$Waterbody,TotalSTV$WBID,TotalSTV$ProjectName,TotalSTV$WBType,TotalSTV$FreshMarine,TotalSTV$Month,TotalSTV$Year)]
names(MonthyTSTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID","ProjectName","WBType","FreshMarine","Month","Year","Total")
MonthyTSTV<-tbl_df(MonthyTSTV)
TotalSTV<-tbl_df(TotalSTV)

#add the exceedance and the sample fields to the table calculate the exceedance ratio on a monthly basis
Counts<-tbl_df(merge(MonthyESTV,MonthyTSTV,by=c("AnalyteName","StationCode","Waterbody","Matrix","WBID","ProjectName","WBType","FreshMarine","Month","Year")))
Counts<-mutate(Counts, ratio=Exceedances/Total)
Counts$Objective<-0.1
Counts$Exceedances[Counts$ratio >= Counts$Objective]<-1
Counts$Exceedances[Counts$ratio < Counts$Objective]<-0
Counts$Total<-1

#add field that counts each time the exceedance ratio is above .1 at the end of the month
Exceedances<-as.data.table(Counts)[,sum(Exceedances),list(Counts$AnalyteName, Counts$StationCode,Counts$Waterbody, Counts$Matrix, Counts$WBID, Counts$ProjectName, Counts$WBType, Counts$FreshMarine)]
names(Exceedances)<-c("AnalyteName","StationCode","Waterbody","MatrixName","WBID", "ProjectName","WBType","FreshMarine","Exceedances")

#Count each month as 1 sample
Total<-as.data.table(Counts)[,sum(Total),list(Counts$AnalyteName,Counts$StationCode,Counts$Waterbody,Counts$Matrix,Counts$WBID,Counts$ProjectName, Counts$WBType, Counts$FreshMarine)]
names(Total)<-c("AnalyteName","StationCode","Waterbody","MatrixName","WBID", "ProjectName","WBType","FreshMarine","Total")

#add samples and exceedances to be in the same table
FinalTotalSTV<-tbl_df(merge(Exceedances,Total,by=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName","WBType","MatrixName","FreshMarine")))

#Add field that identifies this as a STV assessment
FinalTotalSTV$GeomeanSTV<-"STV"

STVResults <- FinalTotalSTV


#Combine STVResults and SixWKResults
STVResults <- STVResults[,c("AnalyteName","StationCode", "Waterbody", "WBID", "ProjectName", "MatrixName", "WBType", "FreshMarine", "Exceedances", "Total", "GeomeanSTV")]
SixWKResults <- SixWKResults[,c("AnalyteName", "StationCode","Waterbody", "WBID", "ProjectName", "MatrixName", "WBType", "FreshMarine", "Exceedances", "Total", "GeomeanSTV")]


Results_STVand6WKGEO <- rbind.data.frame(STVResults, SixWKResults)


Results_STVand6WKGEO<-tbl_df(merge(Results_STVand6WKGEO,Beneficial_Uses))
Results_STVand6WKGEO<-Results_STVand6WKGEO[Results_STVand6WKGEO$BeneficialUse=="R1",]

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
########################              /\  REC STV ISWEBE Section     /\                      ##########################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
########################              \/  REC2 "STV" in Region 3 Section     \/                      ##########################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


#_____________________________Calculate STV for Rec2 Objectives (Region 3 Basin Plan)_______________#

if(Region==3){
  
  REC2STV<-Bacteria_No_reps
  
  #Cut out all data that isn't E. coli and Enterococcus
  REC2STV<-REC2STV[which(REC2STV$AnalyteName=="Coliform, Fecal"),]
  
  #Remove unecessary fields
  REC2STV<-subset(REC2STV,select=c("ProjectName","StationCode","SampleDate","MatrixName","AnalyteName","FractionName","UnitName","Result","MDL","RL","ResQualCode","QACode","MethodName","TargetLatitude","TargetLongitude"))  
  
  #Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
  REC2STV$SampleDate2<-mdy(REC2STV$SampleDate)
  
  #Remove samples collected at the same Station, for the same Matrix, Unit, Analyte, Project, on the same day
  REC2STV<-as.data.table(REC2STV)[,mean(Result),list(ProjectName,StationCode,SampleDate,MatrixName,AnalyteName,UnitName)]
  names(REC2STV)[names(REC2STV)=="V1"]<-"Result"
  
  #Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
  REC2STV$SampleDate2<-mdy(REC2STV$SampleDate)
  
  #Convert sample date into Month and year values
  #this will allow exccedance ratio tests on a monthly basis
  REC2STV$Month<-month(REC2STV$SampleDate2)
  REC2STV$Year<-year(REC2STV$SampleDate2)
  
  
  ############################_____________________________Assess using Rec2 Objectives (R3 Basin Plan)____________________________________________#########################
  #Create objectives data frame for objectives comparison
  AnalyteName<-c("Coliform, Fecal")
  Objectives<-c(4000)
  FreshMarine<-c("F")
  STVObjectives<-data.frame(AnalyteName,Objectives,FreshMarine)
  
  
  #Add necessary information to add objectives then add objectives
  REC2STV<-tbl_df(merge(REC2STV,Stations,by=c("StationCode")))
  REC2STV<-tbl_df(merge(REC2STV,STVObjectives, by=c("AnalyteName","FreshMarine")))
  
  
  #Calculate exceedances on a daily basis first then add the exceedances up for each month
  REC2OverSTV<-as.data.table(REC2STV)[, sum(Result>Objectives), list(REC2STV$AnalyteName,REC2STV$StationCode,REC2STV$MatrixName,REC2STV$Waterbody,REC2STV$WBID,REC2STV$ProjectName,REC2STV$WBType,REC2STV$FreshMarine,REC2STV$SampleDate2,REC2STV$Month,REC2STV$Year)]
  names(REC2OverSTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID","ProjectName","WBType","FreshMarine","SampleDate2","Month","Year","Exceedances")
  REC2MonthyESTV<-as.data.table(REC2OverSTV)[,sum(Exceedances),list(REC2OverSTV$AnalyteName,REC2OverSTV$StationCode,REC2OverSTV$Matrix,REC2OverSTV$Waterbody,REC2OverSTV$WBID,REC2OverSTV$ProjectName,REC2OverSTV$WBType,REC2OverSTV$FreshMarine,REC2OverSTV$Month,REC2OverSTV$Year)]
  names(REC2MonthyESTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID", "ProjectName","WBType","FreshMarine","Month","Year","Exceedances")
  REC2MonthyESTV<-tbl_df(REC2MonthyESTV)
  REC2OverSTV<-tbl_df(REC2OverSTV)
  
  #Count the number of samples for each day and then add each sample together for each month
  REC2TotalSTV<-as.data.table(REC2STV)[, sum(Result>0),list(REC2STV$AnalyteName,REC2STV$StationCode,REC2STV$MatrixName,REC2STV$Waterbody,REC2STV$WBID,REC2STV$ProjectName,REC2STV$WBType,REC2STV$FreshMarine,REC2STV$SampleDate2,REC2STV$Month,REC2STV$Year)]
  names(REC2TotalSTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID","ProjectName","WBType","FreshMarine","SampleDate2","Month","Year","Total")
  REC2MonthyTSTV<-as.data.table(REC2TotalSTV)[,sum(Total),list(REC2TotalSTV$AnalyteName,REC2TotalSTV$StationCode,REC2TotalSTV$Matrix,REC2TotalSTV$Waterbody,REC2TotalSTV$WBID,REC2TotalSTV$ProjectName,REC2TotalSTV$WBType,REC2TotalSTV$FreshMarine,REC2TotalSTV$Month,REC2TotalSTV$Year)]
  names(REC2MonthyTSTV)<-c("AnalyteName","StationCode","Matrix","Waterbody","WBID","ProjectName","WBType","FreshMarine","Month","Year","Total")
  REC2MonthyTSTV<-tbl_df(REC2MonthyTSTV)
  REC2TotalSTV<-tbl_df(REC2TotalSTV)
  
  #add the exceedance and the sample fields to the table calculate the exceedance ratio on a monthly basis
  REC2Counts<-tbl_df(merge(REC2MonthyESTV,REC2MonthyTSTV,by=c("AnalyteName","StationCode","Waterbody","Matrix","WBID","ProjectName","WBType","FreshMarine","Month","Year")))
  REC2Counts<-mutate(REC2Counts, ratio=Exceedances/Total)
  REC2Counts$Objective<-0.1
  REC2Counts$Exceedances[REC2Counts$ratio >= REC2Counts$Objective]<-1
  REC2Counts$Exceedances[REC2Counts$ratio < REC2Counts$Objective]<-0
  REC2Counts$Total<-1
  
  #add field that REC2Counts each time the exceedance ratio is above .1 at the end of the month
  REC2Exceedances<-as.data.table(REC2Counts)[,sum(Exceedances),list(REC2Counts$AnalyteName, REC2Counts$StationCode,REC2Counts$Waterbody, REC2Counts$Matrix, REC2Counts$WBID, REC2Counts$ProjectName, REC2Counts$WBType, REC2Counts$FreshMarine)]
  names(REC2Exceedances)<-c("AnalyteName","StationCode","Waterbody","MatrixName","WBID", "ProjectName","WBType","FreshMarine","Exceedances")
  
  #Count each month as 1 sample
  REC2Total<-as.data.table(REC2Counts)[,sum(Total),list(REC2Counts$AnalyteName,REC2Counts$StationCode,REC2Counts$Waterbody,REC2Counts$Matrix,REC2Counts$WBID,REC2Counts$ProjectName, REC2Counts$WBType, REC2Counts$FreshMarine)]
  names(REC2Total)<-c("AnalyteName","StationCode","Waterbody","MatrixName","WBID", "ProjectName","WBType","FreshMarine","Total")
  
  #add samples and exceedances to be in the same table
  FinalREC2TotalSTV<-tbl_df(merge(REC2Exceedances,REC2Total,by=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName","WBType","MatrixName","FreshMarine")))
  
  #Add field that identifies this as a STV assessment
  FinalREC2TotalSTV$GeomeanSTV<-"10%"
  
  REC2STVResults <- FinalREC2TotalSTV
  
  REC2STVResults<-tbl_df(merge(REC2STVResults,Beneficial_Uses))
  
  #Filter out all data not for R2 beneficial use
  REC2STVResults<-REC2STVResults[REC2STVResults$BeneficialUse=="R2",]
  
  Results_STVand6WKGEO <- rbind.data.frame(Results_STVand6WKGEO, REC2STVResults)
  
}

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
########################              /\  REC2 "STV" in Region 3 Section     /\                      ##########################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################



###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
########################              \/         Ocean Plan Section           \/                     ##########################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################



#_____________________________Calculate 30 day median AND less than 10% of samples > 230 MPN for same 30 days (SH)_______________#



#_____________________________\/30- Day Rolling 10%\/_____________________________#

shell_data<-Bacteria_No_reps

#Cut out all data that isn't Total Coliform
shell_data<-shell_data[shell_data$AnalyteName=="Coliform, Total",]


##Remove IDEXX Colilert test for Total Coliform because they often result in false positives
shell_data<-filter(shell_data,!(MethodName=="Colilert"|MethodName=="Colilert-18"|
                                  MethodName=="SM 9223"|MethodName=="SM 9223 B"|
                                  MethodName=="SM 9223 B, SM 9230D b"|MethodName=="SM 9223 B-SOP ASWAU"|
                                  MethodName=="SM 9223 B-SOP1103"|MethodName=="Enterolert"|
                                  MethodName=="Colilert 18 (Fecal)"|MethodName=="Colilert 18 (Total)"|MethodName=="Colilert 18"|
                                  MethodName=="Colilert 18 Fecals"|MethodName=="Colilert 18 E. coli"|MethodName=="SM9223"))

#Remove unecessary fields
shell_data<-subset(shell_data,select=c("ProjectName","StationCode","SampleDate","MatrixName","FractionName","AnalyteName","UnitName","Result","MDL","RL","ResQualCode","QACode","IntervalStart","TargetLatitude","TargetLongitude"))  

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
shell_data$SampleDate2<-mdy(shell_data$SampleDate)


#Remove samples collected at the same Station, for the same Matrix, Unit, Analyte, Project, on the same day
shell_data<-as.data.table(shell_data)[,mean(Result),list(ProjectName,StationCode,SampleDate,MatrixName,FractionName,AnalyteName,UnitName,IntervalStart)]
names(shell_data)[names(shell_data)=="V1"]<-"Result"

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
shell_data$SampleDate2<-mdy(shell_data$SampleDate)


#Create objectives data frame for objectives comparison
AnalyteName<-c("Coliform, Total","Coliform, Total","Coliform, Total")
Objectives<-c(230,230,230)

#Since this came from the ocean plan, I assume waters would be ocean.
FreshMarine<-c("O","F","M")

shell_Objectives<-data.frame(AnalyteName,Objectives,FreshMarine)

#Add necessary information to add objectives then add objectives
shell_data<-tbl_df(merge(shell_data,Stations,by=c("StationCode")))
shell_data<-tbl_df(merge(shell_data,shell_Objectives, by=c("AnalyteName","FreshMarine")))

#Filter out Shell data for Fresh and ISWEBE Salt water if not in Region 3 or Region 9
if(Region!="3"&Region!="9"){
  shell_data<-shell_data[which(shell_data$FreshMarine=="O"),]
}


#Creates new column in "d with name "IntervalStart" based on "SampleDate2" for beginning of assessmet window
shell_data$IntervalStart<-shell_data$SampleDate2

shell_data<-tbl_df(shell_data)
shell_data$dateinterval<-interval(shell_data$IntervalStart-days(30),shell_data$IntervalStart)
shell_data<-subset(shell_data,select=c("AnalyteName","UnitName","StationCode","ProjectName","SampleDate","SampleDate2","MatrixName","FractionName","Result","Objectives","IntervalStart","dateinterval"))
shell_data$Exceedance<-0
shell_data$Exceedance[which(shell_data$Result>shell_data$Objectives)]<-1


#Assigns a new dataframe, Fecal_Ocean2, which is created from splitting Fecal_Ocean based on unique 
#"StationCode", "Analyte", "Unit", "MatrixName" combinations and applying a function
#to the dataframe

#The function creates two empty columns, Fecal_OceanGeoMean and
#Fecal_OceanN (number of samples avged over) filled with NA to be filled with the loop below.

#Loop repeats for i=1 to i=length of SampleDate.

#Fills ith row with values obtained from loop functions.


#Returns dataframe Fecal_Ocean2 with columns "StationCode", "AnalyteName", "UnitName", "MatrixName", "dateinterval"
#"shell_data", "Date", and "shell_dataN"
shell_data2<-ddply(shell_data, c("AnalyteName", "UnitName","StationCode", "ProjectName","MatrixName","FractionName","Objectives"),function(df){
  shell_dataExceedance<-rep(NA, length(df$SampleDate))
  shell_dataN<-rep(NA, length(df$SampleDate))
  
  for(i in 1:length(df$SampleDate)){
    shell_dataN[i]<-length(df$Result[df$SampleDate2%within%df$dateinterval[i]])
    shell_dataExceedance[i]<-sum(df$Exceedance[df$SampleDate2%within%df$dateinterval[i]],na.rm=T)
    
  }
  return(data.frame(Exceedance=shell_dataExceedance, SampleDate=as.character(df$IntervalStart), shell_dataN=shell_dataN))
}
)

#Filter out duplicate results made from loop and write to file
shell_data2<-tbl_df(shell_data2)
shell_data2<-distinct(shell_data2,StationCode,AnalyteName,UnitName,MatrixName,FractionName,ProjectName,SampleDate,.keep_all=TRUE)



shell_data2<-mutate(shell_data2,Result=100*Exceedance/shell_dataN)



#Add objective for the data (10% exceedance rate, or .1 exceedance ratio)
shell_data2$Objective<-.1

shell_data2<-tbl_df(merge(shell_data2,Stations))


#add field that counts each time the exceedance ratio is above .1 at the end of the month
Exceedances<-as.data.table(shell_data2)[,sum(Result>Objective),list(AnalyteName, StationCode,Waterbody, MatrixName,FractionName,WBID, ProjectName, WBType,FreshMarine)]
names(Exceedances)<-c("AnalyteName","StationCode","Waterbody","MatrixName","FractionName","WBID", "ProjectName","WBType","FreshMarine","Exceedances")

#Count each month as 1 sample
Total<-as.data.table(shell_data2)[,sum(Result>=0),list(AnalyteName,StationCode,Waterbody,MatrixName,FractionName,WBID,ProjectName, WBType,FreshMarine)]
names(Total)<-c("AnalyteName","StationCode","Waterbody","MatrixName","FractionName", "WBID", "ProjectName","WBType","FreshMarine","Total")

#add samples and exceedances to be in the same table
FinalTotalTenPercent<-tbl_df(merge(Exceedances,Total,by=c("AnalyteName","StationCode","Waterbody","FractionName","WBID","ProjectName","WBType","MatrixName","FreshMarine")))

#Add field that identifies this as a STV assessment or 10% if in region 3
FinalTotalTenPercent$GeomeanSTV<-"10%"

FinalTotalTenPercent<-tbl_df(merge(FinalTotalTenPercent,Beneficial_Uses))
FinalTotalTenPercent<-FinalTotalTenPercent[FinalTotalTenPercent$BeneficialUse=="SH",]
FinalTotalTenPercent$FractionName<-NULL


Results_ShellSTV <- FinalTotalTenPercent
Results_ShellSTV <- Results_ShellSTV[,c("AnalyteName", "StationCode","Waterbody", "WBID", "ProjectName", "MatrixName","WBType", "FreshMarine", "Exceedances", "Total", "GeomeanSTV","BeneficialUse")]

#_____________________________/\10%/\_____________________________#



#_____________________________\/30 Day Rolling Median\/_____________________________#
shell_data <- Bacteria_No_reps

#Cut out all data that isn't Total Coliform
shell_data<-shell_data[which(shell_data$AnalyteName=="Coliform, Total"),]

##Remove IDEXX Colilert test for Total Coliform because they often result in false positives
shell_data<-filter(shell_data,!(MethodName=="Colilert"|MethodName=="Colilert-18"|
                                  MethodName=="SM 9223"|MethodName=="SM 9223 B"|
                                  MethodName=="SM 9223 B, SM 9230D b"|MethodName=="SM 9223 B-SOP ASWAU"|
                                  MethodName=="SM 9223 B-SOP1103"|MethodName=="Enterolert"|
                                  MethodName=="Colilert 18 (Fecal)"|MethodName=="Colilert 18 (Total)"|MethodName=="Colilert 18"|
                                  MethodName=="Colilert 18 Fecals"|MethodName=="Colilert 18 E. coli"|MethodName=="SM9223"))


#Remove unecessary fields
shell_data<-subset(shell_data,select=c("ProjectName","StationCode","SampleDate","MatrixName","FractionName","AnalyteName","UnitName","Result","MDL","RL","ResQualCode","QACode","IntervalStart","TargetLatitude","TargetLongitude"))  

#Remove samples collected at the same Station, for the same Matrix, Unit, Analyte, Project, on the same day
shell_data<-as.data.table(shell_data)[,mean(Result),list(ProjectName,StationCode,SampleDate,IntervalStart,MatrixName,FractionName,AnalyteName,UnitName)]
names(shell_data)[names(shell_data)=="V1"]<-"Result"

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
shell_data$SampleDate2<-mdy(shell_data$SampleDate)

#Creates new column in d with name "SampleDate2" based on "SampleDate" in format (mdy)
shell_data$IntervalStart<-shell_data$SampleDate2

shell_data<-tbl_df(shell_data)
shell_data$dateinterval<-interval(shell_data$IntervalStart-days(30),shell_data$IntervalStart)
shell_data<-subset(shell_data,select=c("AnalyteName","UnitName","StationCode","ProjectName","SampleDate","SampleDate2","MatrixName","FractionName","Result","IntervalStart","dateinterval"))


#Assigns a new dataframe, Fecal_Ocean2, which is created from splitting Fecal_Ocean based on unique 
#"StationCode", "Analyte", "Unit", "MatrixName" combinations and applying a function
#to the dataframe

#The function creates two empty columns, Fecal_OceanGeoMean and
#Fecal_OceanN (number of samples avged over) filled with NA to be filled with the loop below.

#Loop repeats for i=1 to i=length of SampleDate.

#Fills ith row with values obtained from loop functions.


#Returns dataframe Fecal_Ocean2 with columns "StationCode", "AnalyteName", "UnitName", "MatrixName", "dateinterval"
#"shell_data", "Date", and "shell_dataN"
shell_data2<-ddply(shell_data, c("AnalyteName", "UnitName","StationCode", "ProjectName","MatrixName","FractionName"),function(df){
  shell_dataMedian<-rep(NA, length(df$SampleDate))
  shell_dataN<-rep(NA, length(df$SampleDate))
  
  for(i in 1:length(df$SampleDate)){
	shell_dataN[i]<-length(df$Result[df$SampleDate2%within%df$dateinterval[i]])
    shell_dataMedian[i]<-median(df$Result[df$SampleDate2%within%df$dateinterval[i]], na.rm=T)
    
  }
  return(data.frame(Result=shell_dataMedian, SampleDate=as.character(df$IntervalStart), shell_dataN=shell_dataN))
}
)

#Filter out duplicate results made from loop and write to file
shell_data2<-tbl_df(shell_data2)
shell_data2<-distinct(shell_data2,StationCode,AnalyteName,UnitName,MatrixName,FractionName,ProjectName,SampleDate,.keep_all=TRUE)

#Create objectives data frame for objectives comparison
AnalyteName<-c("Coliform, Total","Coliform, Total","Coliform, Total")
Objectives<-c(70,70,70)

#Since this came from the ocean plan, I assume waters would be marine.
FreshMarine<-c("O","F","M")

shell_Objectives<-data.frame(AnalyteName,Objectives,FreshMarine)

#Add necessary information to add objectives then add objectives
shell_data2<-tbl_df(merge(shell_data2,Stations,by=c("StationCode")))
shell_data2<-tbl_df(merge(shell_data2,shell_Objectives, by=c("AnalyteName","FreshMarine")))

#Filter out Shell data for EST water if not in Region 3
if(Region!="3"&Region!="9"){
  shell_data2<-shell_data2[which(shell_data2$FreshMarine=="O"),]
}


#Count the number of samples for each month, max sample is one sample per month:
#Total samples are number of samples over how many months were sampled.

ShellMed<-shell_data2


#Count the number of samples for each month, max sample is one sample per month:
#Total samples are number of samples over how many months were sampled.

OverShellMed<-as.data.table(ShellMed)[,sum(Result>Objectives),list(AnalyteName,StationCode,MatrixName,Waterbody,WBID,ProjectName,WBType,FreshMarine)]
names(OverShellMed)<-c("AnalyteName","StationCode","MatrixName","Waterbody","WBID","ProjectName","WBType","FreshMarine","Exceedances")

TotalShellMed<-as.data.table(ShellMed)[,sum(Result>0),list(AnalyteName,StationCode,MatrixName,Waterbody,WBID,ProjectName,WBType,FreshMarine)]
names(TotalShellMed)<-c("AnalyteName","StationCode","MatrixName","Waterbody","WBID","ProjectName","WBType","FreshMarine","Total")


#add samples and exceedances to be in the same table
FinalTotalMedian<-tbl_df(merge(OverShellMed,TotalShellMed,by=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName","WBType","MatrixName","FreshMarine")))

#Add field that identifies this as a STV assessment
FinalTotalMedian$GeomeanSTV<-"Median"

#Add Beneficial Uses
FinalTotalMedian<-tbl_df(merge(FinalTotalMedian,Beneficial_Uses))
FinalTotalMedian<-FinalTotalMedian[FinalTotalMedian$BeneficialUse=="SH",]

Results_ShellMedian <- FinalTotalMedian

#_____________________________/\30 Day Median/\_____________________________#


##################################################################################################################################################################################

#____________________________\/30 Day Fecal Fresh R2 and Ocean R1 Geomean\/____________________#
#This section was originally written for fecal assessments in ocean waters but expanded to include Rec2 assessments
#in Region 3 during the 2020 cycle, Assessment was made generic to reduceduplicative coding necessary.


#Create table of Fecal Data from Ocean Data
Fecal_Ocean<-Bacteria_No_reps
Fecal_Ocean<-tbl_df(Fecal_Ocean[Fecal_Ocean$AnalyteName=="Coliform, Fecal",])

#Create date interval for geomean
Fecal_Ocean$dateinterval<-interval(Fecal_Ocean$IntervalStart-days(30),Fecal_Ocean$IntervalStart)
Fecal_Ocean<-subset(Fecal_Ocean,select=c("AnalyteName","UnitName","StationCode","ProjectName","SampleDate","SampleDate2","MatrixName","FractionName","Result","MethodName","IntervalStart","dateinterval"))


#Assigns a new dataframe, Fecal_Ocean2, which is created from splitting Fecal_Ocean based on unique 
#"StationCode", "Analyte", "Unit", "MatrixName" combinations and applying a function
#to the dataframe

#The function creates two empty columns, Fecal_OceanGeoMean and
#Fecal_OceanN (number of samples avged over) filled with NA to be filled with the loop below.

#Loop repeats for i=1 to i=length of SampleDate.

#Fills ith row with values obtained from loop functions.


#Returns dataframe Fecal_Ocean2 with columns "StationCode", "AnalyteName", "UnitName", "MatrixName", "dateinterval"
#"Fecal_OceanGeomean", "Date", and "Fecal_OceanN"
Fecal_Ocean2<-ddply(Fecal_Ocean, c("AnalyteName", "UnitName","StationCode", "ProjectName","MatrixName","FractionName"),function(df){
  Fecal_OceanGeoMean<-rep(NA, length(df$SampleDate))
  Fecal_OceanN<-rep(NA, length(df$SampleDate))
  
  for(i in 1:length(df$SampleDate)){
	Fecal_OceanN[i]<-length(df$Result[df$SampleDate2%within%df$dateinterval[i]])
    Fecal_OceanGeoMean[i]<-prod(df$Result[df$SampleDate2%within%df$dateinterval[i]]^(1/Fecal_OceanN[i]), na.rm=T)
    
  }
  return(data.frame(Result=Fecal_OceanGeoMean, SampleDate=as.character(df$IntervalStart), Fecal_OceanN=Fecal_OceanN))
}
)

#Filter out duplicate results made from loop and write to file
Fecal_Ocean2<-tbl_df(Fecal_Ocean2)
Fecal_Ocean2<-distinct(Fecal_Ocean2,StationCode,AnalyteName,UnitName,MatrixName,FractionName,ProjectName,SampleDate,.keep_all=TRUE)


#######_______________________________Assess with New Objectives (Draft Staff Report Values)_________________###################
#Add waterbody information
Fecal_Ocean<-tbl_df(merge(Fecal_Ocean2,Stations,by=c("StationCode")))

#Remove sampling peroids with less than the minimum number of samples
Fecal_Ocean<-Fecal_Ocean[which(Fecal_Ocean$Fecal_OceanN>=MinimumSamples),]

#Create objectives data frame for objectives comparison
AnalyteName<-c("Coliform, Fecal","Coliform, Fecal")
Objectives<-c(200,2000)

#Since this came from the ocean plan, I assume waters would be ocean.
FreshMarine<-c("O","F")

Fecal_Ocean_Objectives<-data.frame(AnalyteName,Objectives,FreshMarine)

#Add necessary information to add objectives then add objectives
Fecal_Ocean<-tbl_df(merge(Fecal_Ocean,Fecal_Ocean_Objectives, by=c("AnalyteName","FreshMarine")))

#Filter out Shell data for EST water if not in Region 3
if(Region!="3"){
  Fecal_Ocean<-Fecal_Ocean[which(Fecal_Ocean$FreshMarine=="O"),]
}

#Calculate the number of exceedances by comparing the result with the objective
Fecal_Ocean$Objectives<-as.numeric(Fecal_Ocean$Objectives)
Fecal_Ocean_Over<-as.data.table(Fecal_Ocean)[,sum(Result>Objectives),list(AnalyteName,ProjectName,StationCode,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Fecal_Ocean_Over)[names(Fecal_Ocean_Over)=='V1']<-"Exceedances"
Fecal_Ocean_Over<-tbl_df(Fecal_Ocean_Over)

#Calculate the total number of samples collected
Fecal_Ocean$Objectives<-as.numeric(Fecal_Ocean$Objectives)
Fecal_Ocean_Total<-as.data.table(Fecal_Ocean)[,sum(Result>0),list(AnalyteName,ProjectName,StationCode,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Fecal_Ocean_Total)[names(Fecal_Ocean_Total)=='V1']<-"Total"
Total<-tbl_df(Fecal_Ocean_Total)


#Combined exceedance count and total sample count together based on common pollutant, matrix, waterbody, WBID, freshmarine combinations
Fecal_Ocean_Results<-tbl_df(merge(Fecal_Ocean_Over,Fecal_Ocean_Total,by=c("AnalyteName","StationCode","MatrixName","Waterbody","WBID","FreshMarine","ProjectName","FractionName","WBType")))

Fecal_Ocean_Results$FractionName <- NULL

#add field that specifies this was a geomean assessment
Fecal_Ocean_Results$GeomeanSTV<-"Geomean"

#Add Beneficial Uses
Fecal_Ocean_Results<-tbl_df(merge(Fecal_Ocean_Results,Beneficial_Uses))
if(Region=="3"){
Fecal_Ocean_ResultsR1<-Fecal_Ocean_Results[Fecal_Ocean_Results$BeneficialUse=="R1" & Fecal_Ocean_Results$FreshMarine=="O",]
Fecal_Ocean_ResultsR2<-Fecal_Ocean_Results[Fecal_Ocean_Results$BeneficialUse=="R2" & Fecal_Ocean_Results$FreshMarine=="F",]
Fecal_Ocean_Results<-rbind.data.frame(Fecal_Ocean_ResultsR1,Fecal_Ocean_ResultsR2)
}

if(Region!="3"){
  Fecal_Ocean_Results<-Fecal_Ocean_Results[Fecal_Ocean_Results$BeneficialUse=="R1" & Fecal_Ocean_Results$FreshMarine=="O",]
}


#__________________________/\30 Day Fecal, Rec2 and Ocean Plan /\_____________________#

#________________________\/ Ocean Fecal SSM Calculation \/__________________#

#Create table of Ocean Fecal data for SSM calculation
Fecal_SSM_Ocean<-Ocean_Data
Fecal_SSM_Ocean<-tbl_df(Fecal_SSM_Ocean[Fecal_SSM_Ocean$AnalyteName=="Coliform, Fecal",])

#Add waterbody information to SSM data
Fecal_SSM_Ocean<-tbl_df(merge(Fecal_SSM_Ocean,Stations,by=c("StationCode")))

#Add objective to data frame
Fecal_SSM_Ocean$Objective<-as.numeric(400)

#Calculate number of exceedances of SSM
Fecal_SSM_Ocean_Over<-as.data.table(Fecal_SSM_Ocean)[,sum(Result>Objective),list(AnalyteName,ProjectName,StationCode,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Fecal_SSM_Ocean_Over)[names(Fecal_SSM_Ocean_Over)=='V1']<-"Exceedances"
Fecal_SSM_Ocean_Over<-tbl_df(Fecal_SSM_Ocean_Over)

# Calulate the total number of samples collected
Fecal_SSM_Ocean_Total<-as.data.table(Fecal_SSM_Ocean)[,sum(Result>0),list(AnalyteName,ProjectName,StationCode,MatrixName,FractionName,Waterbody,WBID,FreshMarine,WBType)]
names(Fecal_SSM_Ocean_Total)[names(Fecal_SSM_Ocean_Total)=='V1']<-"Total"
Fecal_SSM_Ocean_Total<-tbl_df(Fecal_SSM_Ocean_Total)

#Combine exceedances and results into the same table
Fecal_SSM_Ocean_Results<-tbl_df(merge(Fecal_SSM_Ocean_Total,Fecal_SSM_Ocean_Over))

Fecal_SSM_Ocean_Results$FractionName<-NULL
Fecal_SSM_Ocean_Results$GeomeanSTV<-"SSM"

#Add Beneficial Uses
Fecal_SSM_Ocean_Results<-tbl_df(merge(Fecal_SSM_Ocean_Results,Beneficial_Uses))
Fecal_SSM_Ocean_Results<-Fecal_SSM_Ocean_Results[Fecal_SSM_Ocean_Results$BeneficialUse=="R1",]

#________________________/\ Ocean Fecal SSM Calculation /\__________________#


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
########################            /\           Ocean Plan Section                /\                #########################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################


#Results, combining all results dataframes.
Results_ALL<-Results_STVand6WKGEO
Results_ALL<- rbind.data.frame(Results_ALL, Results_ShellSTV)
Results_ALL<-rbind.data.frame(Results_ALL, Results_ShellMedian)

Results_ALL<-rbind.data.frame(Results_ALL,Fecal_SSM_Ocean_Results)
Results_ALL<-rbind.data.frame(Results_ALL,Fecal_Ocean_Results)

#add Region 6 specific rows
Results_ALL<-rbind.data.frame(Results_ALL,All_R6_FecalAssessments)

# Write LOE

#Add in date range
Samples_w_Dates<-tbl_df(merge(Results_ALL,BacteriaDateRange,by=c("Waterbody","StationCode", "WBID","MatrixName", "ProjectName", "AnalyteName", "WBType")))

#Merge Samples with Stations
#Samples_w_Stations<-tbl_df(merge(Samples_w_Dates,StationInfo,by=c("Waterbody","StationCode", "WBID","MatrixName", "ProjectName", "AnalyteName", "WBType", "FractionName")))
#Samples_w_Stations$StationCode<-NULL

Samples_w_Stations<-Samples_w_Dates
Samples_w_Stations$StationCount<-1

#Change "samplewater" and "overlyingwater" to "water"
Samples_w_Stations$MatrixName[Samples_w_Stations$MatrixName=="samplewater"]<-'Water'
Samples_w_Stations$MatrixName[Samples_w_Stations$MatrixName=='overlyingwater']<-'Water'


#Add in BUs
#Samples_w_BU<-tbl_df(merge(Samples_w_Stations,Beneficial_Uses,by=c("Waterbody", "WBID")))
#Samples_w_BU$BeneficialUse<-as.character(Samples_w_BU$BeneficialUse)
#Samples_w_BU$Wbtype<-NULL
#Samples_w_BU$SampleDate<-NULL
#Samples_w_BU$SampleDate2<-NULL
#Samples_w_BU_not_fecal <- filter(Samples_w_BU, (BeneficialUse=='R1'| BeneficialUse=='R2'))
#Samples_w_BU_not_fecal<-Samples_w_BU_not_fecal[which(Samples_w_BU_not_fecal$AnalyteName=="Enterococcus"|Samples_w_BU_not_fecal$AnalyteName=="E. coli"),]
#Samples<-Samples_w_BU
#Samples_Total<-Samples[which(Samples$AnalyteName=="Coliform, Total"&Samples$BeneficialUse=="SH"),]
#Samples_w_BU_not_fecal<-rbind.data.frame(Samples_Total,Samples_w_BU_not_fecal)
#Samples_W_BU_Fecal<-Samples_w_BU[which(Samples_w_BU$AnalyteName=="Coliform, Fecal"),]
#Samples_W_BU<-rbind.data.frame(Samples_W_BU_Fecal,Samples_w_BU_not_fecal)

Samples_W_BU<-Samples_w_Stations

#Add objective and obj ref to each line of data
Samples_w_Lang <- tbl_df(merge(Samples_W_BU,Lang,by=c("FreshMarine","GeomeanSTV", "BeneficialUse", "AnalyteName")))

#Reorder columns 
Samples_w_Lang<-Samples_w_Lang[,c("AnalyteName", "Waterbody", "WBID", "StationCode", "BeneficialUse", "Exceedances", "Total", "Obj_Lang" , "Obj_Ref", "ProjectName", "StationCount","GeomeanSTV", "MinDate", "MaxDate", "MatrixName", "FractionName", "WBType", "FreshMarine" )]

#Final dataframe for language and all
Sample_LOEs<-Samples_w_Lang

#Add fields
Sample_LOEs$SUB_GROUP <-"Pollutant-Water"
#Change sub group to Ancillary Line of Evidence if reference does not have QAPP when it should
Sample_LOEs$FRACTION <-"None"
names(Sample_LOEs)[names(Sample_LOEs)=="ProjectName"]<-"ParentProjectName"
Sample_LOEs <- merge(Sample_LOEs,Data_Used_Refs, by=c("ParentProjectName"))
Sample_LOEs$SUB_GROUP[which(Sample_LOEs$QA_INFO_REFERENCES=="4971"|Sample_LOEs$QA_INFO_REFERENCES=="5005"|Sample_LOEs$QA_INFO_REFERENCES=="4686")]<-"Ancillary Line of Evidence"
names(Sample_LOEs)[names(Sample_LOEs)=="Waterbody"] <- "WB_SEGMENT"

#Matching field names with LOE upload tool
names(Sample_LOEs)[names(Sample_LOEs) == 'AnalyteName'] <- 'POLLUTANT'
names(Sample_LOEs)[names(Sample_LOEs) == 'BeneficialUse'] <- 'BU_CODE'
names(Sample_LOEs)[names(Sample_LOEs) == 'Exceedances'] <- 'EXCEEDANCE_COUNT'
names(Sample_LOEs)[names(Sample_LOEs) == 'Total'] <- 'SAMPLE_COUNT'
names(Sample_LOEs)[names(Sample_LOEs) == 'Obj_Lang'] <- 'CRITERION/OBJECTIVE'
names(Sample_LOEs)[names(Sample_LOEs) == 'Obj_Ref'] <- 'CRITERION/OBJECTIVE_REFERENCES'

#Input SPATIAL_REP
Sample_LOEs$SPATIAL_REP<-paste("The samples were collected at", Sample_LOEs$StationCount, "monitoring site(s), station(s): ",Sample_LOEs$StationCode)

###Input DATA_USED
Sample_LOEs$DATA_USED<-NA
Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$GeomeanSTV=="STV",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the Statistical Threshold Value (STV) water quality threshold for ",Sample_LOEs$POLLUTANT,". The STV is based on a 10% exceedance rate that is calculated monthly."),Sample_LOEs$DATA_USED)
Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$GeomeanSTV=="Geomean",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the geomean water quality threshold for ",Sample_LOEs$POLLUTANT,". This is a six week rolling geomean that is calculated weekly."),Sample_LOEs$DATA_USED)


#########This only applies to Total Coliform that is the on threshold that applies to Shellfish
Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$BU_CODE=="SH"&Sample_LOEs$GeomeanSTV=="Median",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold for shellfish is based on the median concentration of Total Coliform at each station, and is calculated as a 30-day rolling median."),Sample_LOEs$DATA_USED)
Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$BU_CODE=="SH"&Sample_LOEs$GeomeanSTV=="10%"&Sample_LOEs$POLLUTANT=="Coliform, Total",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold is based on a 10% exceedance rate that is calculated for a 30-Day peroid."),Sample_LOEs$DATA_USED)


#Data used for R1 Fecal geomean assessment
Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$BU_CODE=="R1"&Sample_LOEs$GeomeanSTV=="Geomean"&Sample_LOEs$POLLUTANT=="Coliform, Fecal"&Sample_LOEs$FreshMarine=="O",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold is based on a 30-Day rolling geomean."),Sample_LOEs$DATA_USED)

#Data used for REC1, SSM, Fecal Coliform data which only applies to ocean waters
Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$BU_CODE=="R1"&Sample_LOEs$GeomeanSTV=="SSM"&Sample_LOEs$POLLUTANT=="Coliform, Fecal"&Sample_LOEs$FreshMarine=="O",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold is based on a Single Sample Maximum (SSM) value that is calculated daily."),Sample_LOEs$DATA_USED)


if(Region=="3"){
  #Data used for Rec2 in R3
  Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$BU_CODE=="R2"&Sample_LOEs$GeomeanSTV=="10%"&Sample_LOEs$POLLUTANT=="Coliform, Fecal",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold is based on a 10% exceedance rate that is calculated for a 30-Day peroid."),Sample_LOEs$DATA_USED)
  }

if(Region=="6"){
  ###############These only apply to Region 6 Fecal Coliform data
  Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$GeomeanSTV=="STV"&Sample_LOEs$POLLUTANT=="Coliform, Fecal",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold is based on a 10% exceedance rate that is calculated monthly."),Sample_LOEs$DATA_USED)
  Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$GeomeanSTV=="Geomean"&Sample_LOEs$POLLUTANT=="Coliform, Fecal",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the logmean water quality threshold for ",Sample_LOEs$POLLUTANT,". This is a 30-day rolling logmean that is calculated daily."),Sample_LOEs$DATA_USED)
}

#Input AB411 Data used if applicable
if(AB411=="AB411"){
  Sample_LOEs$DATA_USED<-NA
  Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$GeomeanSTV=="STV",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the Statistical Threshold Value (STV) water quality threshold for ",Sample_LOEs$POLLUTANT,". The STV is based on a 10% exceedance rate that is calculated monthly.  This data is for an AB411 beach.  This LOE specifically corresponds with dry weather samples and only includes samples collected between April 1 and October 31 of each year."),Sample_LOEs$DATA_USED)
  Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$GeomeanSTV=="Geomean",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the geomean water quality threshold for ",Sample_LOEs$POLLUTANT,". This is a six week rolling geomean that is calculated weekly. This data is for an AB411 beach.  This LOE specifically corresponds with dry weather samples and only includes samples collected between April 1 and October 31 of each year."),Sample_LOEs$DATA_USED)
  #Data used for R1 Fecal geomean assessment
  Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$BU_CODE=="R1"&Sample_LOEs$GeomeanSTV=="Geomean"&Sample_LOEs$POLLUTANT=="Coliform, Fecal"&Sample_LOEs$FreshMarine=="O",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold is based on a 30-Day rolling geomean. This data is for an AB411 beach.  This LOE specifically corresponds with dry weather samples and only includes samples collected between April 1 and October 31 of each year."),Sample_LOEs$DATA_USED)
  Sample_LOEs$DATA_USED<-ifelse(Sample_LOEs$BU_CODE=="R1"&Sample_LOEs$GeomeanSTV=="SSM"&Sample_LOEs$POLLUTANT=="Coliform, Fecal"&Sample_LOEs$FreshMarine=="O",paste0("Water Board staff assessed ",Sample_LOEs$ProjectName," data for ",Sample_LOEs$WB_SEGMENT," to determine beneficial use support and the results are as follows: ",Sample_LOEs$EXCEEDANCE_COUNT," of the ",Sample_LOEs$SAMPLE_COUNT," samples exceeded the water quality threshold for ",Sample_LOEs$POLLUTANT,". The water quality threshold is based on a Single Sample Maximum (SSM) value that is calculated daily. This data is for an AB411 beach.  This LOE specifically corresponds with dry weather samples and only includes samples collected between April 1 and October 31 of each year."),Sample_LOEs$DATA_USED)
  }



#Input TEMPORAL_REP
Sample_LOEs$TEMPORAL_REP<-paste("The samples were collected between the days of", Sample_LOEs$MinDate, "and", Sample_LOEs$MaxDate, ".")

#Input ASSESSMENT_STATUS
Sample_LOEs$ASSESSMENT_STATUS<-paste("LOE In Progress")

#Input DATE_CREATED
Sample_LOEs$DATE_CREATED<-Sys.Date()

#Input AUTHOR
Sample_LOEs$AUTHOR<-Author

#Input MIGRATION_ID
Sample_LOEs$MIGRATION_ID<-rownames(Sample_LOEs)

#Input REGION
Sample_LOEs$REGION<-Region

if(Region=="5SJ"|Region=="5T"){
  Sample_LOEs$REGION<-"5"
}

#Input MATRIX
Sample_LOEs$MATRIX<-Sample_LOEs$MatrixName
Sample_LOEs$MATRIX<-"Water"

#Input ENVIRONMENTAL_CONDITIONS
Sample_LOEs$ENVIRONMENTAL_CONDITIONS<-""

#Input ASSESSOR_COMMENT
Sample_LOEs$ASSESSOR_COMMENT<-""

#Input DATA_TYPE
Sample_LOEs$DATA_TYPE<-paste("PATHOGEN MONITORING")


#Clear unneeded columns
Sample_LOEs$ProjectName<-NULL
Sample_LOEs$StationCount<-NULL
Sample_LOEs$StationCode<-NULL
Sample_LOEs$MinDate<-NULL
Sample_LOEs$MaxDate<-NULL
Sample_LOEs$MatrixName<-NULL
Sample_LOEs$WBType <- NULL
Sample_LOEs$FreshMarine <- NULL
Sample_LOEs$FractionName <- NULL
Sample_LOEs$GeomeanSTV <- NULL
Sample_LOEs$AQ_USE_CODE<-""
Sample_LOEs$UPDATED_BY<-""
Sample_LOEs$DATE_UPDATED<-""
Sample_LOEs$EVAL_GUIDELINE<-""
Sample_LOEs$EVAL_GUIDELINE_REFERENCES<-""


#Reorder columns to match LOE uploader tool

#Re-Order Columns to match LOE uploader template
Sample_LOEs<-Sample_LOEs[,c("MIGRATION_ID","REGION","WB_SEGMENT","WBID","POLLUTANT"
,"SUB_GROUP","MATRIX","FRACTION","BU_CODE","AQ_USE_CODE","CRITERION/OBJECTIVE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE","EVAL_GUIDELINE_REFERENCES","SAMPLE_COUNT","EXCEEDANCE_COUNT","SPATIAL_REP","TEMPORAL_REP","QA_INFO"
,"QA_INFO_REFERENCES","DATA_TYPE","DATA_USED","DATA_USED_REFERENCES","ASSESSMENT_STATUS","DATA_SOURCE"
,"AUTHOR","DATE_CREATED","UPDATED_BY","DATE_UPDATED","ENVIRONMENTAL_CONDITIONS","ASSESSOR_COMMENT")]


#Sample_LOEs<-mutate(Sample_LOEs,ExceedanceRate=EXCEEDANCE_COUNT/SAMPLE_COUNT)
#Sample_LOEs$List<-""
#Sample_LOEs$List[which(Sample_LOEs$ExceedanceRate>.167)]<-"Yes"
#Sample_LOEs$List[which(Sample_LOEs$ExceedanceRate<.165)]<-"No"


#Flag LOEs as electornic generated LOEs
Sample_LOEs$ASSESSOR_COMMENT[Sample_LOEs$ASSESSOR_COMMENT=="NA"]<-""
Sample_LOEs$ASSESSOR_COMMENT<-paste0("(LOE written by ReLEP ", ReLEP_Version,") ", Sample_LOEs$ASSESSOR_COMMENT)

#Convert pollutant names to CalWQA Names
Sample_LOEs$POLLUTANT[which(Sample_LOEs$POLLUTANT=="Coliform, Fecal")]<-"Fecal Coliform"
Sample_LOEs$POLLUTANT[which(Sample_LOEs$POLLUTANT=="E. coli")]<-"Escherichia coli (E. coli)"
Sample_LOEs$POLLUTANT[which(Sample_LOEs$POLLUTANT=="Enterococci")]<-"Enterococcus"
Sample_LOEs$POLLUTANT[which(Sample_LOEs$POLLUTANT=="Coliform, Total")]<-"Total Coliform"


#The tossed data rows may be duplicated because there were multiple issues that resulted in tossing
#turn this into a unique list with comma separated issues and counts of the number of issues
#this will help with prioritization and tracking...hopefully

#Create unique list of data that was exported (and not assessed) by ReLEP, count the number of errors within each row of data that caused export
#and create a comma delimited list of these errors for review by getting a unique list of all fields in the data EXCEPT for the "Issue" field
AllExportedData<-All_Missing_Data%>%dplyr::group_by(WQID,ProgramCode,ProgramName,ParentProjectCode,ParentProjectName,ProjectCode,ProjectName,ProjectDescr
,QAPPCode,QAPPName,PublicRelease,SampleDate,StationCode,StationName,StationDescr,TargetLatitude,TargetLongitude,Datum,RegionalBoardID
,WaterBodyType,SampleAgency,SampleAgencyCode,SampleComments,CollectionTime,PositionWaterColumn,LabCollectionComments,CollectionMethodName
,SampleTypeCode,SampleTypeName,Replicate,CollectionDeviceCode,CollectionDeviceName,CollectionDepth,UnitCollectionDepth,LabAgencyCode,LabAgencyName
,SubmittingAgency,LabSubmissionCode,LabBatchComments,MatrixCode,MatrixName,MethodCode,MethodName,AnalyteCode,AnalyteName,FractionCode,FractionName
,UnitCode,UnitName,MDL,RL,QACode,LabBatch,AnalysisDate,LabReplicate,Result,ResQualCode,LabResultComments,SampleLatitiude,SampleLongitude,SampleDatum
,SampleElevation,SampleUnitElevation,DataQuality,DataQualityIndicator,FieldWater)%>%dplyr::summarise(count=n(),Issue=paste(Issue,collapse=", AND "))



#ExportedData File Name
ExportedData<-paste0("Outputs\\",Sys.Date(),"_R",Region,"_ExportedData",EnterDataFileName)
write.table(AllExportedData, ExportedData,sep="\t",row.names=FALSE,na="")


#LOE file name
LOEs<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"_R",Region,"_Bacteria_LOEs_Minimum_",MinimumSamples,"_LOEs",EnterDataFileName)

#Change filename if AB411 LOEs
if(AB411=="AB411"){
LOEs<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"R",Region,"_AB411_Bacteria_LOEs",EnterDataFileName)
}

#Write out LOEs
write.table(Sample_LOEs, LOEs,sep="\t",row.names=FALSE)

RunTime<-Sys.time()-StartTime
RunTime

#   _______ .__   __.  _______  
#  |   ____||  \ |  | |       \ 
#  |  |__   |   \|  | |  .--.  |
#  |   __|  |  . `  | |  |  |  |
#  |  |____ |  |\   | |  '--'  |
#  |_______||__| \__| |_______/ 


