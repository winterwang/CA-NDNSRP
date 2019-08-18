/* Project	For NDNS RP correspondence anlaysis about time slot and food group for 
** Different Diabetes status Aug 17 2019   
*/

libname PROJ "\\tsclient\Documents\CA-NDNSRP" ;

PROC IMPORT 
			DATAFILE = "\\tsclient\Documents\CA-NDNSRP\TFood.csv"
			DBMS = csv
			OUT = PROJ.NDNSRP
			REPLACE; 
RUN;




/* CREATE BINARY INDICATORS FOR THE LOGISTIC REGRESSION */

DATA TEST; 
			SET  PROJ.NDNSRP; 
			YESPUDDING = 0; 
			IF MAINFOODGROUPCODE = 9 THEN YESPUDDING = 1;
			AGECAT = 0; 
			IF AGE > 18 & AGE < 41 THEN AGECAT = 1;
			IF AGE >= 41 & AGE < 51 THEN AGECAT = 2; 
			IF AGE >= 51 & AGE < 61 THEN AGECAT = 3; 
			IF AGE >= 61 & AGE < 71 THEN AGECAT = 4; 
			IF AGE >= 71 & AGE < 81 THEN AGECAT = 5; 
			IF AGE >= 81                   THEN AGECAT = 6; 
RUN;	

/* multilevel logistic regression using GEE */ 

PROC GENMOD DATA =  TEST DESCENDING;
	CLASS SERIALI YESPUDDING   TIME3G DM4CAT(REF = "0") SEX(REF = "1") AGECAT NSSEC8 AREA;
	MODEL YESPUDDING = TIME3G DM4CAT SEX AGECAT NSSEC8/ ALPHA = 0.01 DIST = B TYPE3; 
	WHERE NSSEC8 > 0; 
	REPEATED SUBJECT = SERIALI * AREA / TYPE = EXCH; 
	TITLE "PUDDING GEE "; 
	ESTIMATE "PUDDING EVENING VS MORNING" TIME3G 0 1 -1 /ALPHA = 0.01 EXP; 
	ESTIMATE "PUDDING EVENING VS AFTERNOON" TIME3G -1 1 0 /ALPHA = 0.01 EXP;
RUN;


/* multilevel logistic regression using GEE WITH INTERATCION BETWEEN DM4CAT AND TIME3G */ 

PROC GENMOD DATA =  TEST DESCENDING;
	CLASS SERIALI YESPUDDING   TIME3G DM4CAT(REF = "0") SEX(REF = "1") AGECAT NSSEC8 AREA;
	MODEL YESPUDDING = TIME3G DM4CAT TIME3G*DM4CAT SEX AGECAT NSSEC8/ ALPHA = 0.01 DIST = B TYPE3; 
	REPEATED SUBJECT = SERIALI*AREA / TYPE = IND; 
	TITLE "PUDDING GEE "; 
RUN;

*	ESTIMATE "PUDDING EVENING VS MORNING" TIME3G 0 1 -1 /ALPHA = 0.01 EXP; 
*	ESTIMATE "PUDDING EVENING VS AFTERNOON" TIME3G -1 1 0 /ALPHA = 0.01 EXP;