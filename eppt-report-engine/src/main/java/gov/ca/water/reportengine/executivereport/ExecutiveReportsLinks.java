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

package gov.ca.water.reportengine.executivereport;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class ExecutiveReportsLinks
{

    private static final int MODULE_COLUMN = 0;
    private static final int SUB_MODULE_COLUMN = 1;
    private static final int LINKED_VARS_COLUMN = 2;
    private static final int FLAG_COLUMN = 3;


    private List<Module> _modules = new ArrayList<>();

     ExecutiveReportsLinks()
    {
    }

    //read the csv file and create modules and sub modules
     void readCSVFile(Path csvFilePath) throws ExecutiveReportException
     {
        String line = "";
        String csvSplitBy = ",";

        try (BufferedReader br = Files.newBufferedReader(csvFilePath))
        {

            //skip first line
            int i = 0;
            while ((line = br.readLine()) != null)
            {
                if (i == 0)
                {
                    i++;
                    continue;
                }
                // use comma as separator
                String[] row = line.split(csvSplitBy);
                String modName = row[MODULE_COLUMN];
                if (findModuleByName(modName) == null)
                {
                    //create a new module object
                    Module mod = new Module(modName);
                    _modules.add(mod);
                }
                processSubModules(modName, row);
            }
        }
        catch (IOException e)
        {
           throw new ExecutiveReportException("Error reading the csv file: " + csvFilePath, e);
        }
    }


    private Module findModuleByName(String modName)
    {
        for (Module mod : _modules)
        {
            if (mod.getName().equals(modName))
            {
                return mod;
            }
        }
        return null;
    }

    private void processSubModules(String moduleName, String[] rowValues)
    {
        Module mod = findModuleByName(moduleName);
        if(mod != null)
        {
            List<SubModule> subModules = mod.getSubModules();
            for (SubModule subMod : subModules)
            {
                if (Objects.equals(subMod.getName(), rowValues[SUB_MODULE_COLUMN]))
                {
                    //the submodule already exists, just add to it
                    subMod.addLinkedRecord(rowValues[LINKED_VARS_COLUMN]);
                    return;
                }
            }
            //if we get here, then this is a new module
            SubModule subMod = new SubModule(rowValues[SUB_MODULE_COLUMN], convertStringToFlagType(rowValues[FLAG_COLUMN]));
            subMod.addLinkedRecord(rowValues[LINKED_VARS_COLUMN]);
            subModules.add(subMod);
        }
    }

    private SubModule.FlagType convertStringToFlagType(String flag)
    {
        SubModule.FlagType retVal = SubModule.FlagType.NEGATIVE_INFINITY;
        if ("0".equalsIgnoreCase(flag))
        {
            retVal = SubModule.FlagType.ZERO;
        }
        else if ("1".equalsIgnoreCase(flag))
        {
            retVal = SubModule.FlagType.ONE;
        }
        else if ("2".equalsIgnoreCase(flag))
        {
            retVal = SubModule.FlagType.TWO;
        }
        return retVal;
    }

    List<Module> getModules()
    {
        return new ArrayList<>( _modules);
    }
}
