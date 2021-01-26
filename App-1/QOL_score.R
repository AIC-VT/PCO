# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

################################################################
# QOL_score.R : cldata
# !!!! do not run this source code
################################################################


###########
# Rectal QOL
n <- nrow(cldata)
cldata$RECTAL_MEAN <- NA
for( i in 1:n )
{
  cldata[i,"RECTAL_MEAN"] <- (cldata[i,"EPIC_Q6_A_SCORE"] + cldata[i,"EPIC_Q6_B_SCORE"] + cldata[i,"EPIC_Q6_C_SCORE"] + cldata[i,"EPIC_Q6_D_SCORE"] + cldata[i,"EPIC_Q6_E_SCORE"] + cldata[i,"EPIC_Q7_SCORE"])/6
}




######
# Urinary QOL
n <- nrow(cldata)
cldata$URINARY_QOL_INCONTINENCE_MEAN <- NA
for( i in 1:n )
{
  cldata[i,"URINARY_QOL_INCONTINENCE_MEAN"] <- (cldata[i,"EPIC_Q1_SCORE"] + cldata[i,"EPIC_Q2_SCORE"] + cldata[i,"EPIC_Q3_SCORE"] + cldata[i,"EPIC_Q4_A_SCORE"])/4
}

cldata$URINARY_QOL_IRRITATION_MEAN <- NA
for( i in 1:n )
{
  cldata[i,"URINARY_QOL_IRRITATION_MEAN"] <- (cldata[i,"EPIC_Q4_B_SCORE"] + cldata[i,"EPIC_Q4_C_SCORE"] + cldata[i,"EPIC_Q4_D_SCORE"] + cldata[i,"EPIC_Q4_E_SCORE"])/4
}

######
# Sexual QOL - EPIC_Q12_SCORE: Overall sexuality problem
n <- nrow(cldata)
cldata$SEXUAL_QOL_MEAN <- NA
for( i in 1:n )
{
  cldata[i,"SEXUAL_QOL_MEAN"] <- (cldata[i,"EPIC_Q8_A_SCORE"] + cldata[i,"EPIC_Q8_B_SCORE"] + cldata[i,"EPIC_Q9_SCORE"] + cldata[i,"EPIC_Q10_SCORE"] + cldata[i,"EPIC_Q11_SCORE"])/5
}

