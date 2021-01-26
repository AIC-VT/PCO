################################################################################
# File: VisualizationTest.R
# Desc: Test Visualziation Function
################################################################################


touch UNIT_TEST_VISUALIZATION.txt
echo "=====================================================================" > UNIT_TEST_VISUALIZATION.txt
echo "= PCO UNIT TEST for Data Visualization;Visualization of Likely outcome=" >> UNIT_TEST_VISUALIZATION.txt
echo "=====================================================================" >> UNIT_TEST_VISUALIZATION.txt
echo "=====================================================================" >> UNIT_TEST_VISUALIZATION.txt
echo "= PCO UNIT TEST for Data Visualization: STEP 1 =" >> UNIT_TEST_VISUALIZATION.txt
echo "=====================================================================" >> UNIT_TEST_VISUALIZATION.txt
Rscript UnitTest.R GAUGE_CHART.R 2 1 >> UNIT_TEST_VISUALIZATION.txt 2>&1

echo "=====================================================================" >> UNIT_TEST_VISUALIZATION.txt
echo "= PCO UNIT TEST for Data Visualization: STEP 2 =" >> UNIT_TEST_VISUALIZATION.txt
echo "=====================================================================" >> UNIT_TEST_VISUALIZATION.txt
Rscript UnitTest.R OverallVSProfile.R 2 2 >> UNIT_TEST_VISUALIZATION.txt 2>&1

echo "=====================================================================" >> UNIT_TEST_VISUALIZATION.txt
echo "= PCO UNIT TEST for Data Visualization: STEP 3 =" >> UNIT_TEST_VISUALIZATION.txt
echo "=====================================================================" >> UNIT_TEST_VISUALIZATION.txt
Rscript UnitTest.R DiseaseFree_Risk.R 2 3 >> UNIT_TEST_VISUALIZATION.txt 2>&1
