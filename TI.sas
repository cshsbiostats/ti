/*
Macro Name:          ti
Current Version:     V1
Last Update:         12/13/2023
Working Environment: SAS 9.4 English version
Purpose:             To calculate toxicity index (TI) and export TI data.
Reference:           Rogatko A, Babb JS, Wang H, Slifker MJ, Hudes GR. 
                     Patient characteristics compete with dose as predictors of acute treatment toxicity in early phase clinical trials. 
                     Clin Cancer Res. 2004;10(14):4645-51. doi: 10.1158/1078-0432.CCR-03-0535. PMID: 15269136.
Author:              Greg Yothers <YothersG@nrgoncology.org> and Sungjin Kim <sungjin.kim@cshs.org>

Parameters: 
data:   The name of the data set to be analyzed (long-format, individual toxicity grades in separate rows). 
id:     Subject unique id.
grade:  Toxicity grade.
fname:  File name for output data.
 
Usage:  
%ti(data=dt, id=UniqueID, grade=AE_Grade, fname=Output_TI_sas);
where dataset dt has subject id of UniqueID and toxicity grade of AE_Grade as follows:
UniqueID  AE_Grade   
1         	
2	      1
3	      1
3         1
4         2
5         2
5	      1
.......
*/

%macro ti(data=, id=, grade=, fname=);

/* Standardize variable names and order of records */ 
proc sql;
create table AEsummary
   as select &id as PatientID, &grade as AEGrade 
      from &data 
      order by PatientID, AEGrade desc ; * Order by PatientID and descending AEGrade;
quit;
 
/* Calculate TI */ 
data TIStep1;
      set AEsummary;
      by PatientID;
      retain TI W X; * Retain keeps the value from the last record until it is overwritten in the current record;
      if first.PatientID then do;  * Intialize first record per patient;
            If AEGrade ne . then X = AEGrade;
            else if AEGrade=. then X = .; * This will give missing value on TI for patients with only missing value for AEGrade;
            W = 1;
            TI = 0;
      end;
      else do;  * Subsequent records for the same patient;
	  	 if AEGrade = . then X = 0; 
         else do;
           W = W * (1 + X); * Uses values of W and X from prior record to update W for the current record;
           X = AEGrade; * Update X to the current AEGrade;
		 end;
      end;
 
      TI = TI + X / W; * Update TI from prior TI and current X and W;
run;

data TIFinal ; * One record per PatientID with PatientID and TI;
      set TIStep1;
      by PatientID;
      if Last.PatientID; * Last record per patient has the complete summation of TI;
      drop AEGrade X W;
run;

/* Subject with missing grade */
proc sql noprint;
   select unique(PatientID) 
   into :_idlist1 separated by ' '
   from TIFinal where TI is missing;
quit;

/* Subject with both missing and non-missing grades */
proc sql noprint;
    select unique(&id)  
	into :_idlist2 separated by ' '
    from &data t 
    left join TIFinal y
        on y.PatientID = t.&id 
    where &grade is missing and TI is not missing;
quit;

title;
ods csvall file="&FNAME..csv" newfile=none;
     proc print data=TIFinal(rename=(PatientID=&id)) noobs;
		 footnote1 "Note:";
		 footnote2 "Subject with single or more than one record of grade which is/are all missing: &_idlist1.";
		 footnote3 "Subject with both missing and non-missing records of grade (TI was calculated based on available non-missing grade values.): &_idlist2.";
     run;
ods csvall close;

%mend ti;

