################################################################################
# File: DataManagementTest.sh
# Desc: Test Data Management Function
################################################################################
#!/bin/bash

touch UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" > UNIT_TEST_DATAMANAGEMENT.txt
echo "= PCO UNIT TEST for Data Mamangement;Data loading and preprocessing =" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "= PCO UNIT TEST for Data Mamangement: STEP 1 =" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
Rscript UnitTest.R mysql_init_data.R 1 1 >> UNIT_TEST_DATAMANAGEMENT.txt 2>&1
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "= PCO UNIT TEST for Data Mamangement: STEP 2 =" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
Rscript UnitTest.R cleansing.R 1 2 >> UNIT_TEST_DATAMANAGEMENT.txt 2>&1
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "= PCO UNIT TEST for Data Mamangement: STEP 3 =" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
Rscript UnitTest.R SAVE_SURVIVAL_DATA2.R 1 3 >> UNIT_TEST_DATAMANAGEMENT.txt 2>&1
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "= PCO UNIT TEST for Data Mamangement: STEP 4 =" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
Rscript UnitTest.R EPIC_score.R 1 4 >> UNIT_TEST_DATAMANAGEMENT.txt 2>&1
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "= PCO UNIT TEST for Data Mamangement: STEP 5 =" >> UNIT_TEST_DATAMANAGEMENT.txt
echo "=====================================================================" >> UNIT_TEST_DATAMANAGEMENT.txt
Rscript UnitTest.R QOL_score.R 1 5 >> UNIT_TEST_DATAMANAGEMENT.txt 2>&1
