/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.reportengine.filechanges;

import gov.ca.water.reportengine.EpptReportException;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.logging.Logger;

public class CodeChangesDataProcessor
{
    private static final Logger LOGGER = Logger.getLogger(CodeChangesDataProcessor.class.getName());

    private static final int TYPE_COLUMN = 0;
    private static final int SUBTYPE_COLUMN = 1;
    private static final int WRESL_FILES_COLUMN = 2;

    //private Map<CodeChangesType, List<CodeChangesSubType>> _changesMap = new HashMap<>();
    private final List<CodeChangesType> _codeChangesTypes = new ArrayList<>();

    public CodeChangesDataProcessor(Path csvPath) throws EpptReportException
    {
        loadCodeChangesCSV(csvPath);
    }

    public List<CodeChangesType> getCodeChangesTypes()
    {
        return _codeChangesTypes;
    }

    public FileChangesStatistics processCodeChanges(Path baseOutputPath, Path altOutputPath)
    {
        Set<String> addedToAlt = new HashSet<>();
        Set<String> deletedFromBase = new HashSet<>();
        Map<CodeChangesSubType, Set<String>> subtypeToFileChangesMap = new HashMap<>();


        for(CodeChangesType parent : _codeChangesTypes)
        {
            for(CodeChangesSubType subType : parent.getSubTypes())
            {
                Set<String> modifiedFilesForSubtype = new HashSet<>();

                for(String wreslFile : subType.getWreslFiles())
                {
                    boolean baseHasFile = doesFileExistInDirectory(baseOutputPath);
                    boolean altHasFile = doesFileExistInDirectory(altOutputPath);
                    if(baseHasFile && altHasFile)
                    {
                        //compare them
                        if(areFilesDifferent(baseOutputPath,altOutputPath))
                        {
                            modifiedFilesForSubtype.add(baseOutputPath.toString());
                        }
                    }
                    else if(baseHasFile && !altHasFile)
                    {
                        //file was deleted in alt
                        addedToAlt.add(altOutputPath.toString());

                    }
                    else if(!baseHasFile && altHasFile)
                    {
                        //file was added to alt
                        deletedFromBase.add(baseOutputPath.toString());
                    }
                }

                subtypeToFileChangesMap.put(subType,modifiedFilesForSubtype);
            }
        }

        return new FileChangesStatistics.Builder()
                .withRecordsOnlyInAlt(addedToAlt)
                .withRecordsOnlyInBase(deletedFromBase)
                .withSubtypeModifiedFiles(subtypeToFileChangesMap)
                .build();

    }



    private boolean doesFileExistInDirectory(Path outputPath)
    {
        return true;
    }

    private boolean areFilesDifferent(Path baseFile, Path altFile)
    {
        return true;
    }

    private void loadCodeChangesCSV(Path csvPath) throws EpptReportException
    {
        LOGGER.info("");

        String line = "";
        String csvSplitBy = ",";

        try (BufferedReader br = Files.newBufferedReader(csvPath))
        {

            //skip first line
            int z = 0;
            while ((line = br.readLine()) != null)
            {
                if (z == 0)
                {
                    z++;
                    continue;
                }

                String[] row = line.split(csvSplitBy);

                String type = row[TYPE_COLUMN];
                String subtype = row[SUBTYPE_COLUMN];
                String wreslFile = null;
                if(row.length>2)
                {
                    wreslFile = row[WRESL_FILES_COLUMN];
                }

                boolean isNewType = true;
                for(int i = 0;i<_codeChangesTypes.size();i++)
                {
                    CodeChangesType parent = _codeChangesTypes.get(i);
                    if(Objects.equals(parent.getName(), type))
                    {
                        isNewType = false;
                        updateSubtypes(parent, subtype, wreslFile);
                    }
                }

                if(isNewType)
                {
                    //create new type
                    CodeChangesType codeChangeType = new CodeChangesType(type);
                    CodeChangesSubType codeChangesSubType = new CodeChangesSubType(subtype);
                    if(wreslFile != null)
                    {
                        codeChangesSubType.addWreslFile(wreslFile);
                    }
                    codeChangeType.addSubtype(codeChangesSubType);
                    _codeChangesTypes.add(codeChangeType);
                }
            }
        }
        catch (IOException e)
        {
            throw new EpptReportException("Error loading values from the code changes xml file: " + csvPath, e);
        }
    }

    private void updateSubtypes(CodeChangesType type, String subtype, String file)
    {
        List<CodeChangesSubType> subTypes = type.getSubTypes();
        boolean foundSubtype = false;
        for(int i = 0;i<subTypes.size();i++)
        {
            if(Objects.equals(subTypes.get(i).getName(), subtype))
            {
                if(file != null)
                {
                    subTypes.get(i).addWreslFile(file);
                }
                foundSubtype = true;
                break;
            }
        }
        if(foundSubtype == false)
        {
            CodeChangesSubType codeChangesSubType = new CodeChangesSubType(subtype);
            if(file != null)
            {
                codeChangesSubType.addWreslFile(file);
            }
            type.addSubtype(codeChangesSubType);
        }
    }


}
