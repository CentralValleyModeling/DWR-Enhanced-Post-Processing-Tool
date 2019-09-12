import os
from os import listdir

folder_path_array = [r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\base-alt-diff-table', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\coa-table', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\control-table', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\exceedance', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\exceedance-page', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\line-plot', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\list', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\percent-diff-table', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\scatter-plot', r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports']
for folder_path in folder_path_array:
    print(folder_path)
    for file_name in listdir(folder_path):
        if file_name.endswith('.jasper'):
            os.remove(folder_path +'/'+ file_name)
