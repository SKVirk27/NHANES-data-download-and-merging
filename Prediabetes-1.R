library(RNHANES)
library(sqldf)
library(plyr)
library(dplyr)
library(haven)
DEMO <- nhanes_load_data(c("DEMO_G","DEMO_H","DEMO_I"),
                         c("2011-2012","2013-2014"))
# Load BMI
BMI <- nhanes_load_data(c("BMX_G", "BMX_H"), c("2011-2012", "2013-2014"))

# Load Blood pressure data for 2011-2014
BPX <- nhanes_load_data(c("BPX_G", "BPX_H"), c("2011-2012", "2013-2014"))

# Load Glucose data for 2011-2014
GLU <- nhanes_load_data(c("GLU_G", "GLU_H"), c("2011-2012", "2013-2014"))

#Load DiabetesPedigreeFunction data for 2011-2014
DPQ <- nhanes_load_data(c("DPQ_G", "DPQ_H"), c("2011-2012", "2013-2014"))
# Load Outcome data for 2011-2014
DIQ <- nhanes_load_data(c("DIQ_G", "DIQ_H"), c("2011-2012", "2013-2014"))

# Load Reproductive health data for 2011-2014
RHQ <- nhanes_load_data(c("RHQ_G", "RHQ_H"), c("2011-2012", "2013-2014"))

# Load Triglycerides data for 2011-2014
TRIGLY <- nhanes_load_data(c("TRIGLY_G", "TRIGLY_H"), c("2011-2012", "2013-2014"))

# Load HDL cholesterol data for 2011-2014
HDL <- nhanes_load_data(c("HDL_G", "HDL_H"), c("2011-2012", "2013-2014"))

# Load Waist Circumference data for 2011-2014
WHQ <- nhanes_load_data(c("WHQ_G", "WHQ_H"), c("2011-2012", "2013-2014"))

# Load Smoking data for 2011-2014
SMQ <- nhanes_load_data(c("SMQ_G", "SMQ_H"), c("2011-2012", "2013-2014"))

# Select variables of interest for Physical activity level
PAQ <- nhanes_load_data(c("PAQ_G", "PAQ_H"), c("2011-2012", "2013-2014"))

# Select variables of interest for Alcohol consumption
ALQ <- nhanes_load_data(c("ALQ_G", "ALQ_H"), c("2011-2012", "2013-2014"))
# Select important demographic variables
DEMO_vars <- select(DEMO, SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, INDHHIN2)
# Select variables of interest for Diet
DBQ <- nhanes_load_data(c("DBQ_G", "DBQ_H"), c("2011-2012", "2013-2014"))

# Select variables of interest for Sleep duration and quality
SLQ <- nhanes_load_data(c("SLQ_G", "SLQ_H"), c("2011-2012", "2013-2014"))

# Load medications data
MED<- nhanes_load_data(c("RXQ_RX_G","RXQ_RX_H"), c("2011-2012","2013-2014"))

# Load cholesterol levels data
CHOL <- nhanes_load_data(c("TCHOL_G","TCHOL_H"), c("2011-2012","2013-2014"))

#Fasting
FAS <- nhanes_load_data(c("FASTQX_G","FASTQX_H"),c("2011-2012","2013-2014"))
#A1c level
A1C_2011 <- read_xpt("GHB_G.XPT")
A1C_2013 <-read_xpt("GHB_H.XPT")

#
# Load Sleep data for 2011-2014
Demo_2011 <- select(DEMO$DEMO_G, SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, INDFMPIR, RIDEXPRG, INDFMIN2)
Demo_2011 <- mutate(Demo_2011, WAVE = "2011-2012", YEAR = "2011")

Demo_2013 <- select(DEMO$DEMO_H, SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, INDFMPIR, RIDEXPRG, INDFMIN2)
Demo_2013 <- mutate(Demo_2013, WAVE = "2013-2014", YEAR = "2013")

# Combining demographics into one dataframe for all waves
Demographics_ALL <- bind_rows(Demo_2011, Demo_2013)

# Selecting important variables and renaming columns
Demographics_ALL <- select(Demographics_ALL, SEQN, WAVE, YEAR, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, INDFMPIR, RIDEXPRG, INDFMIN2)
names(Demographics_ALL) <- c("seqn", "wave", "year", "gender", "age", "race_ethnicity", "education_level", "family_income_ratio", "pregnancy_status", "annual_income")


# Select and mutate variables of interest for BMI
BMI_ALL <- bind_rows(select(BMI$BMX_G, SEQN, BMXBMI, BMXWT, BMXHT), select(BMI$BMX_H, SEQN, BMXBMI, BMXWT, BMXHT))
BMI_ALL <- transmute(BMI_ALL, seqn = SEQN, bmi = BMXBMI, weight = BMXWT, height = BMXHT)
head(BMI_ALL)

# Select and mutate variables of interest for BP
BP_ALL <- bind_rows(select(BPX$BPX_G, SEQN, BPXSY1, BPXDI1), select(BPX$BPX_H, SEQN, BPXSY1, BPXDI1))
BP_ALL <- transmute(BP_ALL, seqn = SEQN, systolic_bp = BPXSY1, diastolic_bp = BPXDI1)
head(BP_ALL)

# Select and mutate variables of interest for glucose
GLU_ALL <- bind_rows(select(GLU$GLU_G, SEQN, LBXGLU), select(GLU$GLU_H, SEQN, LBXGLU))
GLU_ALL <- transmute(GLU_ALL, seqn = SEQN, glucose = LBXGLU)
head(GLU_ALL)

# Select and mutate variables of interest for DiabetesPedigreeFunction
DPQ_ALL <- bind_rows(select(DPQ$DPQ_G, SEQN, DPQ010), select(DPQ$DPQ_H, SEQN, DPQ010))
DPQ_ALL <- transmute(DPQ_ALL, seqn = SEQN, diabetes_pedigree_func = DPQ010)
head(DPQ_ALL)

## Select and mutate variables of interest for Outcome
DIQ_ALL <- bind_rows(select(DIQ$DIQ_G, SEQN, DIQ010), select(DIQ$DIQ_H, SEQN, DIQ010))
DIQ_ALL <- transmute(DIQ_ALL, seqn = SEQN, diabetes_outcome = DIQ010)
head(DIQ_ALL)


# Select and mutate variables of interest for Prediabetes
RHQ_ALL <- bind_rows(select(RHQ$RHQ_G, SEQN, RHQ131, RHQ162), select(RHQ$RHQ_H, SEQN, RHQ131, RHQ162))
RHQ_ALL <- transmute(RHQ_ALL, seqn = SEQN, ever_pregnant = RHQ131, diabetes_during_pregnancy = RHQ162)
head(RHQ_ALL)


# Select and mutate variables of interest for Triglycerides
TRIGLY_ALL <- bind_rows(select(TRIGLY$TRIGLY_G, SEQN, LBDLDL), select(TRIGLY$TRIGLY_H, SEQN, LBDLDL))
TRIGLY_ALL <- transmute(TRIGLY_ALL, seqn = SEQN, triglycerides = LBDLDL)
head(TRIGLY_ALL)

# Select and mutate variables of interest for HDL cholesterol
HDL_ALL <- bind_rows(select(HDL$HDL_G, SEQN, LBDHDD), select(HDL$HDL_H, SEQN, LBDHDD))
HDL_ALL <- transmute(HDL_ALL, seqn = SEQN, hdl_cholesterol = LBDHDD)
head(HDL_ALL)

# Select and mutate variables of interest for Waist Circumference
WHQ_ALL <- bind_rows(select(WHQ$WHQ_G, SEQN, WHD010), select(WHQ$WHQ_H, SEQN, WHD010))
WHQ_ALL <- transmute(WHQ_ALL, seqn = SEQN, waist_circumference = WHD010)
head(WHQ_ALL)

# Select and mutate variables of interest for prediabetes
SMQ_ALL <- bind_rows(select(SMQ$SMQ_G, SEQN, SMD030), select(SMQ$SMQ_H, SEQN, SMD030))
SMQ_ALL <- transmute(SMQ_ALL, seqn = SEQN, smoker = ifelse(SMD030 == 1, "yes", "no"))
head(SMQ_ALL)


PAQ_ALL <- bind_rows(select(PAQ$PAQ_G, SEQN, PAQ605), select(PAQ$PAQ_H, SEQN, PAQ605))
PAQ_ALL <- transmute(PAQ_ALL, seqn = SEQN, physical_activity_level = PAQ605)

# Select variables of interest for Diet

DBQ_ALL <- bind_rows(select(DBQ$DBQ_G, SEQN, DBD900), select(DBQ$DBQ_H, SEQN, DBD900))
DBQ_ALL <- transmute(DBQ_ALL, seqn = SEQN, diet_intake = DBD900)

# Select variables of interest for Alcohol consumption

ALQ_ALL <- bind_rows(select(ALQ$ALQ_G, SEQN, ALQ130), select(ALQ$ALQ_H, SEQN, ALQ130))
ALQ_ALL <- transmute(ALQ_ALL, seqn = SEQN, alcohol_consumption = ALQ130)


# Select and mutate variables of interest for sleep duration and quality
SLQ_ALL <- bind_rows(select(SLQ$SLQ_G, SEQN, SLD010H, SLQ050, SLQ060), select(SLQ$SLQ_H, SEQN, SLD010H, SLQ050, SLQ060))
SLQ_ALL <- transmute(SLQ_ALL, seqn = SEQN, sleep_duration_hours = SLD010H, doctor_trouble_sleeping = SLQ050, doctor_sleep_disorder = SLQ060)
head(SLQ_ALL)

#Select and mutate variables of interest for medications
MED_ALL <- bind_rows(select(MED$RXQ_RX_G, SEQN, RXDDRUG, RXDDRGID, RXDDAYS), select(MED$RXQ_RX_H, SEQN, RXDDRUG, RXDDRGID, RXDDAYS))
MED_ALL <- transmute(MED_ALL, seqn = SEQN, medication = RXDDRUG, medication_id = RXDDRGID, days_used = RXDDAYS)

#Select and mutate variables of interest for cholesterol levels
CHOL_ALL <- bind_rows(select(CHOL$TCHOL_G, SEQN, LBXTC), select(CHOL$TCHOL_H, SEQN, LBXTC))
CHOL_ALL <- transmute(CHOL_ALL, seqn = SEQN, cholesterol = LBXTC)

#Select and mutate variables of interest for fasting
FAS_ALL <- bind_rows(select(FAS$FASTQX_G, SEQN, PHQ020), select(FAS$FASTQX_H, SEQN, PHQ020))
FAS_ALL <- transmute(FAS_ALL, seqn = SEQN, coffee_tea_added_sugar = PHQ020)
head(FAS_ALL)

# Combine A1C data for 2011-2014
A1C_list <- list(A1C_2011, A1C_2013)
A1C_All <- bind_rows(A1C_list)
# Select and mutate variables for A1C level
A1C_level <- transmute(A1C, seqn = SEQN, A1C_level = LBXGH)

# View first few rows of the new dataframe
head(A1C_level)
# Merge all datasets
merged_data <- sqldf("SELECT d.*, b.*, p.*, g.*, dpq.*, diq.*, rhq.*, trigly.*, hdl.*, whq.*, smq.*, dbq.*, alq.*, paq.*, slq.*, m.*, c.*, f.*,A1.*
                      FROM Demographics_ALL AS d
                      LEFT JOIN BMI_ALL AS b ON d.seqn = b.seqn
                      LEFT JOIN BP_ALL AS p ON d.seqn = p.seqn
                      LEFT JOIN GLU_ALL AS g ON d.seqn = g.seqn
                      LEFT JOIN DPQ_ALL AS dpq ON d.seqn = dpq.seqn
                      LEFT JOIN DIQ_ALL AS diq ON d.seqn = diq.seqn
                      LEFT JOIN RHQ_ALL AS rhq ON d.seqn = rhq.seqn
                      LEFT JOIN TRIGLY_ALL AS trigly ON d.seqn = trigly.seqn
                      LEFT JOIN HDL_ALL AS hdl ON d.seqn = hdl.seqn
                      LEFT JOIN WHQ_ALL AS whq ON d.seqn = whq.seqn
                      LEFT JOIN SMQ_ALL AS smq ON d.seqn = smq.seqn
                      LEFT JOIN DBQ_ALL AS dbq ON d.seqn = dbq.seqn
                      LEFT JOIN ALQ_ALL AS alq ON d.seqn = alq.seqn
                      LEFT JOIN PAQ_ALL AS paq ON d.seqn = paq.seqn
                      LEFT JOIN SLQ_ALL AS slq ON d.seqn = slq.seqn
                      LEFT JOIN MED_ALL AS m ON d.seqn = m.seqn
                      LEFT JOIN CHOL_ALL AS c ON d.seqn = c.seqn
                      LEFT JOIN FAS_ALL AS f ON d.seqn = f.seqn
                      LEFT JOIN A1C_level AS A1 ON d.seqn = A1.seqn")
#creating column if patient has diabetes.

#merged_data$outcome <- cut(merged_data$glucose, c(0, 100, 125, Inf), labels = c("not pre-diabetes", "pre-diabetes", "diabetes"))
# Identify and remove duplicate rows
merged_data <- merged_data[, !duplicated(names(merged_data))]


# Identify columns with more than 80% missing values
missing_cols <- sapply(merged_data, function(x) mean(is.na(x))) > 0.8

View(merged_data)

# create a new column 'pre_diabetic' and set it to 0 initially
merged_data$pre_diabetic <- 0

# set the values to 1 if the patient is pre-diabetic
merged_data$pre_diabetic <- ifelse(
  (merged_data$glucose >= 100 & merged_data$glucose < 126) |
    (merged_data$A1C_level >= 5.7 & merged_data$A1C_level < 6.5) |
    (merged_data$cholesterol >= 200),
  1, 
  0
)
merged_data_Prediabetic <- merged_data[!is.na(merged_data$pre_diabetic),]

View(merged_data_Prediates)
write.csv(merged_data_Prediabetic, file = "merged_data_Prediabetic.csv", row.names = FALSE)



