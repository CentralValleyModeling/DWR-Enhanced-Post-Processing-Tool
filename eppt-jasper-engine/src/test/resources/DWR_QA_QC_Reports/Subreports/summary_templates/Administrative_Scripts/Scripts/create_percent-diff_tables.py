from shutil import copyfile
import os

csv_file = r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\summary_templates\Administrative_Scripts\SummaryComponents2.csv' #full path to csv file. Keep the r in front
template = r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\summary_templates\QAQC_percent_difference_table.jrxml' #full path to template file. Keep the r in front
new_directory = r'C:\Users\bryan\JaspersoftWorkspace\MyReports\DWR_QA_QC_Reports\Subreports\percent-diff-table'
column_d_type = 'percent-diff-table'

with open(csv_file, 'r') as g: #opens the file, renames it as 'g'
    for line in g: #loop through each line
        sline = line.strip().split(',') #strip removes and new lines ('\n') or weird characters, split creates a list based on the ',' characters
        if sline[0] == '!':
            continue
        if sline[0] == column_d_type:
            print(sline[1])
            new_name = sline[1] + '.jrxml' #grabs the name from a 0 indexed list, change 1 to whatever the number is..
            if os.path.exists(os.path.join(new_directory, new_name)):
                print(os.path.join(new_directory, new_name), 'already exists..')
                continue
            copyfile(template, os.path.join(new_directory, new_name)) #copies from the file to the new place with the new name