# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

################################################################
# EPIC_score.R : cldata
################################################################

isChkEPIC = 1
if ( isChkEPIC == 1 ) {
  ###############################################
  # cleansing: missing values ( >= 10 )
  # epic_a1 
  cldata$epic_q1 <- as.numeric(cldata$epic_q1)
  cldata <- cldata[which(cldata$epic_q1 >= 1 & cldata$epic_q1 <= 5),]
  cldata <- cldata[which(cldata$epic_q8_a >= 1 & cldata$epic_q8_a <= 5),]
  cldata <- cldata[which(cldata$epic_q8_b >= 1 & cldata$epic_q8_b <= 5),]
  cldata <- cldata[which(cldata$epic_q10 >= 1 & cldata$epic_q10 <= 5),]
  cldata <- cldata[which(cldata$epic_q11 >= 1 & cldata$epic_q11 <= 5),]
  
  cldata <- cldata[which(cldata$epic_q2 >= 1 & cldata$epic_q2 <= 4),]
  cldata <- cldata[which(cldata$epic_q9 >= 1 & cldata$epic_q9 <= 4),]
  
  cldata <- cldata[which(cldata$epic_q3 >= 0 & cldata$epic_q3 <= 3),]
  
  cldata <- cldata[which(cldata$epic_q4_a >= 0 & cldata$epic_q4_a <= 4),]
  cldata <- cldata[which(cldata$epic_q4_b >= 0 & cldata$epic_q4_b <= 4),]
  cldata <- cldata[which(cldata$epic_q4_c >= 0 & cldata$epic_q4_c <= 4),]
  cldata <- cldata[which(cldata$epic_q4_d >= 0 & cldata$epic_q4_d <= 4),]
  cldata <- cldata[which(cldata$epic_q4_e >= 0 & cldata$epic_q4_e <= 4),]
  cldata <- cldata[which(cldata$epic_q6_a >= 0 & cldata$epic_q6_a <= 4),]
  cldata <- cldata[which(cldata$epic_q6_b >= 0 & cldata$epic_q6_b <= 4),]
  cldata <- cldata[which(cldata$epic_q6_c >= 0 & cldata$epic_q6_c <= 4),]
  cldata <- cldata[which(cldata$epic_q6_d >= 0 & cldata$epic_q6_d <= 4),]
  cldata <- cldata[which(cldata$epic_q6_e >= 0 & cldata$epic_q6_e <= 4),]
  cldata <- cldata[which(cldata$epic_q13_a >= 0 & cldata$epic_q13_a <= 4),]
  cldata <- cldata[which(cldata$epic_q13_b >= 0 & cldata$epic_q13_b <= 4),]
  cldata <- cldata[which(cldata$epic_q13_c >= 0 & cldata$epic_q13_c <= 4),]
  cldata <- cldata[which(cldata$epic_q13_d >= 0 & cldata$epic_q13_d <= 4),]
  cldata <- cldata[which(cldata$epic_q13_e >= 0 & cldata$epic_q13_e <= 4),]
  
  cldata <- cldata[which(cldata$epic_q5 >= 1 & cldata$epic_q5 <= 5),]
  cldata <- cldata[which(cldata$epic_q7 >= 1 & cldata$epic_q7 <= 5),]
  cldata <- cldata[which(cldata$epic_q12 >= 1 & cldata$epic_q12 <= 5),]
}

# character to numeric value
n <- ncol(cldata)
cols <- names(cldata)
for( i in 1:n )
{ 
  if ( substr(cols[i],1,4) == "epic" )
  {
    cldata[,cols[i]] <- as.numeric(cldata[,cols[i]])
  }
}
#str(cldata)


###### 
# scoring
#epic_q1
head(cldata$epic_q1)
cldata<- within( cldata, {
  EPIC_Q1_SCORE = character(0) 
  EPIC_Q1_SCORE[ epic_q1 == 1 ] = "0"
  EPIC_Q1_SCORE[ epic_q1 == 2 ] = "25"
  EPIC_Q1_SCORE[ epic_q1 == 3 ] = "50"
  EPIC_Q1_SCORE[ epic_q1 == 4 ] = "75"
  EPIC_Q1_SCORE[ epic_q1 == 5 ] = "100"
})
cldata$EPIC_Q1_SCORE <- as.numeric(cldata$EPIC_Q1_SCORE)
#epic_q2
head(cldata$epic_q2)
cldata<- within( cldata, {
  EPIC_Q2_SCORE = character(0) 
  EPIC_Q2_SCORE[ epic_q2 == 1 ] = "0"
  EPIC_Q2_SCORE[ epic_q2 == 2 ] = "33"
  EPIC_Q2_SCORE[ epic_q2 == 3 ] = "67"
  EPIC_Q2_SCORE[ epic_q2 == 4 ] = "100"
})
cldata$EPIC_Q2_SCORE <- as.numeric(cldata$EPIC_Q2_SCORE)
#epic_q3
head(cldata$epic_q3)
cldata<- within( cldata, {
  EPIC_Q3_SCORE = character(0) 
  EPIC_Q3_SCORE[ epic_q3 == 0 ] = "100"
  EPIC_Q3_SCORE[ epic_q3 == 1 ] = "67"
  EPIC_Q3_SCORE[ epic_q3 == 2 ] = "33"
  EPIC_Q3_SCORE[ epic_q3 == 3 ] = "0"
})
cldata$EPIC_Q3_SCORE <- as.numeric(cldata$EPIC_Q3_SCORE)
#epic_q4_a
head(cldata$epic_q4_a)
cldata<- within( cldata, {
  EPIC_Q4_A_SCORE = character(0) 
  EPIC_Q4_A_SCORE[ epic_q4_a == 0 ] = "100"
  EPIC_Q4_A_SCORE[ epic_q4_a == 1 ] = "75"
  EPIC_Q4_A_SCORE[ epic_q4_a == 2 ] = "50"
  EPIC_Q4_A_SCORE[ epic_q4_a == 3 ] = "25"
  EPIC_Q4_A_SCORE[ epic_q4_a == 4 ] = "0"
})
cldata$EPIC_Q4_A_SCORE <- as.numeric(cldata$EPIC_Q4_A_SCORE)
#epic_q4_b
head(cldata$epic_q4_b)
cldata<- within( cldata, {
  EPIC_Q4_B_SCORE = character(0) 
  EPIC_Q4_B_SCORE[ epic_q4_b == 0 ] = "100"
  EPIC_Q4_B_SCORE[ epic_q4_b == 1 ] = "75"
  EPIC_Q4_B_SCORE[ epic_q4_b == 2 ] = "50"
  EPIC_Q4_B_SCORE[ epic_q4_b == 3 ] = "25"
  EPIC_Q4_B_SCORE[ epic_q4_b == 4 ] = "0"
})
cldata$EPIC_Q4_B_SCORE <- as.numeric(cldata$EPIC_Q4_B_SCORE)
#epic_q4_c
head(cldata$epic_q4_c)
cldata<- within( cldata, {
  EPIC_Q4_C_SCORE = character(0) 
  EPIC_Q4_C_SCORE[ epic_q4_c == 0 ] = "100"
  EPIC_Q4_C_SCORE[ epic_q4_c == 1 ] = "75"
  EPIC_Q4_C_SCORE[ epic_q4_c == 2 ] = "50"
  EPIC_Q4_C_SCORE[ epic_q4_c == 3 ] = "25"
  EPIC_Q4_C_SCORE[ epic_q4_c == 4 ] = "0"
})
cldata$EPIC_Q4_C_SCORE <- as.numeric(cldata$EPIC_Q4_C_SCORE)
#epic_q4_d
head(cldata$epic_q4_d)
cldata<- within( cldata, {
  EPIC_Q4_D_SCORE = character(0) 
  EPIC_Q4_D_SCORE[ epic_q4_d == 0 ] = "100"
  EPIC_Q4_D_SCORE[ epic_q4_d == 1 ] = "75"
  EPIC_Q4_D_SCORE[ epic_q4_d == 2 ] = "50"
  EPIC_Q4_D_SCORE[ epic_q4_d == 3 ] = "25"
  EPIC_Q4_D_SCORE[ epic_q4_d == 4 ] = "0"
})
cldata$EPIC_Q4_D_SCORE <- as.numeric(cldata$EPIC_Q4_D_SCORE)
#epic_q4_e
head(cldata$epic_q4_e)
cldata<- within( cldata, {
  EPIC_Q4_E_SCORE = character(0) 
  EPIC_Q4_E_SCORE[ epic_q4_e == 0 ] = "100"
  EPIC_Q4_E_SCORE[ epic_q4_e == 1 ] = "75"
  EPIC_Q4_E_SCORE[ epic_q4_e == 2 ] = "50"
  EPIC_Q4_E_SCORE[ epic_q4_e == 3 ] = "25"
  EPIC_Q4_E_SCORE[ epic_q4_e == 4 ] = "0"
})
cldata$EPIC_Q4_E_SCORE <- as.numeric(cldata$EPIC_Q4_E_SCORE)
#epic_q5
head(cldata$epic_q5)
cldata<- within( cldata, {
  EPIC_Q5_SCORE = character(0) 
  EPIC_Q5_SCORE[ epic_q5 == 1 ] = "100"
  EPIC_Q5_SCORE[ epic_q5 == 2 ] = "75"
  EPIC_Q5_SCORE[ epic_q5 == 3 ] = "50"
  EPIC_Q5_SCORE[ epic_q5 == 4 ] = "25"
  EPIC_Q5_SCORE[ epic_q5 == 5 ] = "0"
})
cldata$EPIC_Q5_SCORE <- as.numeric(cldata$EPIC_Q5_SCORE)
#epic_q6_a
head(cldata$epic_q6_a)
cldata<- within( cldata, {
  EPIC_Q6_A_SCORE = character(0) 
  EPIC_Q6_A_SCORE[ epic_q6_a == 0 ] = "100"
  EPIC_Q6_A_SCORE[ epic_q6_a == 1 ] = "75"
  EPIC_Q6_A_SCORE[ epic_q6_a == 2 ] = "50"
  EPIC_Q6_A_SCORE[ epic_q6_a == 3 ] = "25"
  EPIC_Q6_A_SCORE[ epic_q6_a == 4 ] = "0"
})
cldata$EPIC_Q6_A_SCORE <- as.numeric(cldata$EPIC_Q6_A_SCORE)
#epic_q6_b
head(cldata$epic_q6_b)
cldata<- within( cldata, {
  EPIC_Q6_B_SCORE = character(0) 
  EPIC_Q6_B_SCORE[ epic_q6_b == 0 ] = "100"
  EPIC_Q6_B_SCORE[ epic_q6_b == 1 ] = "75"
  EPIC_Q6_B_SCORE[ epic_q6_b == 2 ] = "50"
  EPIC_Q6_B_SCORE[ epic_q6_b == 3 ] = "25"
  EPIC_Q6_B_SCORE[ epic_q6_b == 4 ] = "0"
})
cldata$EPIC_Q6_B_SCORE <- as.numeric(cldata$EPIC_Q6_B_SCORE)
#epic_q6_c
head(cldata$epic_q6_c)
cldata<- within( cldata, {
  EPIC_Q6_C_SCORE = character(0) 
  EPIC_Q6_C_SCORE[ epic_q6_c == 0 ] = "100"
  EPIC_Q6_C_SCORE[ epic_q6_c == 1 ] = "75"
  EPIC_Q6_C_SCORE[ epic_q6_c == 2 ] = "50"
  EPIC_Q6_C_SCORE[ epic_q6_c == 3 ] = "25"
  EPIC_Q6_C_SCORE[ epic_q6_c == 4 ] = "0"
})
cldata$EPIC_Q6_C_SCORE <- as.numeric(cldata$EPIC_Q6_C_SCORE)
#epic_q6_d
head(cldata$epic_q6_d)
cldata<- within( cldata, {
  EPIC_Q6_D_SCORE = character(0) 
  EPIC_Q6_D_SCORE[ epic_q6_d == 0 ] = "100"
  EPIC_Q6_D_SCORE[ epic_q6_d == 1 ] = "75"
  EPIC_Q6_D_SCORE[ epic_q6_d == 2 ] = "50"
  EPIC_Q6_D_SCORE[ epic_q6_d == 3 ] = "25"
  EPIC_Q6_D_SCORE[ epic_q6_d == 4 ] = "0"
})
cldata$EPIC_Q6_D_SCORE <- as.numeric(cldata$EPIC_Q6_D_SCORE)
#epic_q6_e
head(cldata$epic_q6_e)
cldata<- within( cldata, {
  EPIC_Q6_E_SCORE = character(0) 
  EPIC_Q6_E_SCORE[ epic_q6_e == 0 ] = "100"
  EPIC_Q6_E_SCORE[ epic_q6_e == 1 ] = "75"
  EPIC_Q6_E_SCORE[ epic_q6_e == 2 ] = "50"
  EPIC_Q6_E_SCORE[ epic_q6_e == 3 ] = "25"
  EPIC_Q6_E_SCORE[ epic_q6_e == 4 ] = "0"
})
cldata$EPIC_Q6_E_SCORE <- as.numeric(cldata$EPIC_Q6_E_SCORE)
#epic_q7
head(cldata$epic_q7)
cldata<- within( cldata, {
  EPIC_Q7_SCORE = character(0) 
  EPIC_Q7_SCORE[ epic_q7 == 1 ] = "100"
  EPIC_Q7_SCORE[ epic_q7 == 2 ] = "75"
  EPIC_Q7_SCORE[ epic_q7 == 3 ] = "50"
  EPIC_Q7_SCORE[ epic_q7 == 4 ] = "25"
  EPIC_Q7_SCORE[ epic_q7 == 5 ] = "0"
})
cldata$EPIC_Q7_SCORE <- as.numeric(cldata$EPIC_Q7_SCORE)
#epic_q8_a
head(cldata$epic_q8_a)
cldata<- within( cldata, {
  EPIC_Q8_A_SCORE = character(0) 
  EPIC_Q8_A_SCORE[ epic_q8_a == 1 ] = "0"
  EPIC_Q8_A_SCORE[ epic_q8_a == 2 ] = "25"
  EPIC_Q8_A_SCORE[ epic_q8_a == 3 ] = "50"
  EPIC_Q8_A_SCORE[ epic_q8_a == 4 ] = "75"
  EPIC_Q8_A_SCORE[ epic_q8_a == 5 ] = "100"
})
cldata$EPIC_Q8_A_SCORE <- as.numeric(cldata$EPIC_Q8_A_SCORE)
#epic_q8_b
head(cldata$epic_q8_b)
cldata<- within( cldata, {
  EPIC_Q8_B_SCORE = character(0) 
  EPIC_Q8_B_SCORE[ epic_q8_b == 1 ] = "0"
  EPIC_Q8_B_SCORE[ epic_q8_b == 2 ] = "25"
  EPIC_Q8_B_SCORE[ epic_q8_b == 3 ] = "50"
  EPIC_Q8_B_SCORE[ epic_q8_b == 4 ] = "75"
  EPIC_Q8_B_SCORE[ epic_q8_b == 5 ] = "100"
})
cldata$EPIC_Q8_B_SCORE <- as.numeric(cldata$EPIC_Q8_B_SCORE)
#epic_q9
head(cldata$epic_q9)
cldata<- within( cldata, {
  EPIC_Q9_SCORE = character(0) 
  EPIC_Q9_SCORE[ epic_q9 == 1 ] = "0"
  EPIC_Q9_SCORE[ epic_q9 == 2 ] = "33"
  EPIC_Q9_SCORE[ epic_q9 == 3 ] = "67"
  EPIC_Q9_SCORE[ epic_q9 == 4 ] = "100"
})
cldata$EPIC_Q9_SCORE <- as.numeric(cldata$EPIC_Q9_SCORE)
#epic_q10
head(cldata$epic_q10)
cldata<- within( cldata, {
  EPIC_Q10_SCORE = character(0) 
  EPIC_Q10_SCORE[ epic_q10 == 1 ] = "0"
  EPIC_Q10_SCORE[ epic_q10 == 2 ] = "25"
  EPIC_Q10_SCORE[ epic_q10 == 3 ] = "50"
  EPIC_Q10_SCORE[ epic_q10 == 4 ] = "75"
  EPIC_Q10_SCORE[ epic_q10 == 5 ] = "100"
})
cldata$EPIC_Q10_SCORE <- as.numeric(cldata$EPIC_Q10_SCORE)
#epic_q11
head(cldata$epic_q11)
cldata<- within( cldata, {
  EPIC_Q11_SCORE = character(0) 
  EPIC_Q11_SCORE[ epic_q11 == 1 ] = "0"
  EPIC_Q11_SCORE[ epic_q11 == 2 ] = "25"
  EPIC_Q11_SCORE[ epic_q11 == 3 ] = "50"
  EPIC_Q11_SCORE[ epic_q11 == 4 ] = "75"
  EPIC_Q11_SCORE[ epic_q11 == 5 ] = "100"
})
cldata$EPIC_Q11_SCORE <- as.numeric(cldata$EPIC_Q11_SCORE)
#epic_q12
head(cldata$epic_q12)
cldata<- within( cldata, {
  EPIC_Q12_SCORE = character(0) 
  EPIC_Q12_SCORE[ epic_q12 == 1 ] = "100"
  EPIC_Q12_SCORE[ epic_q12 == 2 ] = "75"
  EPIC_Q12_SCORE[ epic_q12 == 3 ] = "50"
  EPIC_Q12_SCORE[ epic_q12 == 4 ] = "25"
  EPIC_Q12_SCORE[ epic_q12 == 5 ] = "0"
})
cldata$EPIC_Q12_SCORE <- as.numeric(cldata$EPIC_Q12_SCORE)
#epic_q13_a
head(cldata$epic_q13_a)
cldata<- within( cldata, {
  EPIC_Q13_A_SCORE = character(0) 
  EPIC_Q13_A_SCORE[ epic_q13_a == 0 ] = "100"
  EPIC_Q13_A_SCORE[ epic_q13_a == 1 ] = "75"
  EPIC_Q13_A_SCORE[ epic_q13_a == 2 ] = "50"
  EPIC_Q13_A_SCORE[ epic_q13_a == 3 ] = "25"
  EPIC_Q13_A_SCORE[ epic_q13_a == 4 ] = "0"
})
cldata$EPIC_Q13_A_SCORE <- as.numeric(cldata$EPIC_Q13_A_SCORE)
#epic_q13_b
head(cldata$epic_q13_b)
cldata<- within( cldata, {
  EPIC_Q13_B_SCORE = character(0) 
  EPIC_Q13_B_SCORE[ epic_q13_b == 0 ] = "100"
  EPIC_Q13_B_SCORE[ epic_q13_b == 1 ] = "75"
  EPIC_Q13_B_SCORE[ epic_q13_b == 2 ] = "50"
  EPIC_Q13_B_SCORE[ epic_q13_b == 3 ] = "25"
  EPIC_Q13_B_SCORE[ epic_q13_b == 4 ] = "0"
})
cldata$EPIC_Q13_B_SCORE <- as.numeric(cldata$EPIC_Q13_B_SCORE)
#epic_q13_c
head(cldata$epic_q13_c)
cldata<- within( cldata, {
  EPIC_Q13_C_SCORE = character(0) 
  EPIC_Q13_C_SCORE[ epic_q13_c == 0 ] = "100"
  EPIC_Q13_C_SCORE[ epic_q13_c == 1 ] = "75"
  EPIC_Q13_C_SCORE[ epic_q13_c == 2 ] = "50"
  EPIC_Q13_C_SCORE[ epic_q13_c == 3 ] = "25"
  EPIC_Q13_C_SCORE[ epic_q13_c == 4 ] = "0"
})
cldata$EPIC_Q13_C_SCORE <- as.numeric(cldata$EPIC_Q13_C_SCORE)
#epic_q13_d
head(cldata$epic_q13_d)
cldata<- within( cldata, {
  EPIC_Q13_D_SCORE = character(0) 
  EPIC_Q13_D_SCORE[ epic_q13_d == 0 ] = "100"
  EPIC_Q13_D_SCORE[ epic_q13_d == 1 ] = "75"
  EPIC_Q13_D_SCORE[ epic_q13_d == 2 ] = "50"
  EPIC_Q13_D_SCORE[ epic_q13_d == 3 ] = "25"
  EPIC_Q13_D_SCORE[ epic_q13_d == 4 ] = "0"
})
cldata$EPIC_Q13_D_SCORE <- as.numeric(cldata$EPIC_Q13_D_SCORE)
#epic_q13_e
head(cldata$epic_q13_e)
cldata<- within( cldata, {
  EPIC_Q13_E_SCORE = character(0) 
  EPIC_Q13_E_SCORE[ epic_q13_e == 0 ] = "100"
  EPIC_Q13_E_SCORE[ epic_q13_e == 1 ] = "75"
  EPIC_Q13_E_SCORE[ epic_q13_e == 2 ] = "50"
  EPIC_Q13_E_SCORE[ epic_q13_e == 3 ] = "25"
  EPIC_Q13_E_SCORE[ epic_q13_e == 4 ] = "0"
})
cldata$EPIC_Q13_E_SCORE <- as.numeric(cldata$EPIC_Q13_E_SCORE)

