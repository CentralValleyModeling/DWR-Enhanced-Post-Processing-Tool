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

package gov.ca.water.reportengine.reportreaders;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import gov.ca.water.reportengine.executivereport.ExecutiveReportException;
import gov.ca.water.reportengine.executivereport.Module;
import gov.ca.water.reportengine.executivereport.SubModule;

import static gov.ca.water.reportengine.EPPTReport.checkInterrupt;

public class ModulesReader
{

	private static final int ID_COLUMN = 0;
	private static final int MODULE_COLUMN = 1;
	private static final int SUB_MODULE_COLUMN = 2;
	private static final int TITLE_COLUMN = 3;
	private static final int FLAG_COLUMN = 4;

	private final List<Module> _modules = new ArrayList<>();


	private final Path _moduleCSVPath;

	public ModulesReader(Path moduleCSVPath)
	{
		_moduleCSVPath = moduleCSVPath;
	}

	public List<Module> read() throws ExecutiveReportException
	{
		String line = "";
		String csvSplitBy = ",";

		try(BufferedReader br = Files.newBufferedReader(_moduleCSVPath))
		{

			//skip first line
			int i = 0;
			while((line = br.readLine()) != null)
			{
				if(i == 0)
				{
					i++;
					continue;
				}
				// use comma as separator
				String[] row = line.split(csvSplitBy);
				//skip comments
				if(row.length > 0)
				{
					String firstString = row[0];
					String trimmedString = firstString.trim();
					char firstChar = trimmedString.charAt(0);
					if(firstChar == '#' || firstChar == '!')
					{
						continue;
					}
				}

				String modName = row[MODULE_COLUMN];
				Module mod = findModuleByName(modName);
				if(mod == null)
				{
					//create a new module object
					mod = new Module(modName);
					_modules.add(mod);
				}
				// int modID = Integer.parseInt(row[ID_COLUMN]);
				//List<String> linkedFiles = getLinkedFilesFromID(modID, moduleLinkingCSVPath);
				updateSubModules(mod, row);
			}

			//we have finished creating all the mods and sub mods. Now add the linked variables to the submods
		}
		catch(IOException e)
		{
			throw new ExecutiveReportException("Error reading the csv file: " + _moduleCSVPath, e);
		}
		return _modules;
	}


	//    private void addLinkedVariablesToSubModules(Path moduleLinkingCSVPath) throws ExecutiveReportException
	//    {
	//        for(Module mod: _modules)
	//        {
	//            for(SubModule sub: mod.getSubModules())
	//            {
	//                int id = sub.getId();
	//                List<String> linkedFiles = getLinkedFilesFromID(id, moduleLinkingCSVPath);
	//                sub.addLinkedRecords(linkedFiles);
	//            }
	//        }
	//    }

	//    private List<String> getLinkedFilesFromID(int id, Path moduleLinkingCSVPath) throws ExecutiveReportException
	//    {
	//        String line = "";
	//        String csvSplitBy = ",";
	//        List<String> retval = new ArrayList<>();
	//        try (BufferedReader br = Files.newBufferedReader(moduleLinkingCSVPath))
	//        {
	//            //skip first line
	//            int i = 0;
	//            while ((line = br.readLine()) != null)
	//            {
	//                if (i == 0)
	//                {
	//                    i++;
	//                    continue;
	//                }
	//                String[] row = line.split(csvSplitBy);
	//                int rowID = Integer.parseInt(row[0]);
	//                if(rowID == id)
	//                {
	//                    retval.add(row[1]);
	//                }
	//            }
	//        }
	//        catch (IOException e)
	//        {
	//            throw new ExecutiveReportException("Error reading the csv file: " + moduleLinkingCSVPath, e);
	//        }
	//        return retval;
	//    }

	private Module findModuleByName(String modName)
	{
		for(Module mod : _modules)
		{
			if(mod.getName().equals(modName))
			{
				return mod;
			}
		}
		return null;
	}

	private void updateSubModules(Module mod, String[] rowValues)
	{
		if(mod != null)
		{
			List<SubModule> subModules = mod.getSubModules();
			for(SubModule subMod : subModules)
			{
				if(Objects.equals(subMod.getName(), rowValues[SUB_MODULE_COLUMN]))
				{
					//the submodule already exists
					return;
				}
			}
			//if we get here, then this is a new module
			int modID = Integer.parseInt(rowValues[ID_COLUMN]);
			String name = rowValues[SUB_MODULE_COLUMN];

			SubModule subMod = new SubModule(modID, name, convertStringToFlagType(rowValues[FLAG_COLUMN]), rowValues[TITLE_COLUMN]);
			subModules.add(subMod);
		}
	}


	private SubModule.FlagType convertStringToFlagType(String flag)
	{
		SubModule.FlagType retVal = SubModule.FlagType.NEGATIVE_INFINITY;
		if("0".equalsIgnoreCase(flag))
		{
			retVal = SubModule.FlagType.ZERO;
		}
		else if("1".equalsIgnoreCase(flag))
		{
			retVal = SubModule.FlagType.ONE;
		}
		else if("2".equalsIgnoreCase(flag))
		{
			retVal = SubModule.FlagType.TWO;
		}
		return retVal;
	}


}
