################################################################################
# File: ReportTest.R
# Desc: Test Report Function
################################################################################

touch UNIT_TEST_REPORT.txt
echo "=====================================================================" > UNIT_TEST_REPORT.txt
echo "= PCO UNIT TEST for Data Report;Report of Likely outcome=" >> UNIT_TEST_REPORT.txt
echo "=====================================================================" >> UNIT_TEST_REPORT.txt
echo "=====================================================================" >> UNIT_TEST_REPORT.txt
echo "= PCO UNIT TEST for Data Report: STEP 1 =" >> UNIT_TEST_REPORT.txt
echo "=====================================================================" >> UNIT_TEST_REPORT.txt
Rscript UnitTest.R report.Rmd 3 1 >> UNIT_TEST_REPORT.txt 2>&1

