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

package gov.ca.water.reportengine;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.busservice.impl.DetailedIssuesReader;
import gov.ca.water.reportengine.executivereport.Module;
import gov.ca.water.reportengine.executivereport.SubModule;
import gov.ca.water.reportengine.reportreaders.ModulesReader;

import static gov.ca.water.reportengine.EPPTReport.checkInterrupt;

public class ModuleCreator
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private final List<DetailedIssue> _detailedIssues = new ArrayList<>();
	private final List<Module> _modules = new ArrayList<>();

	public List<Module> createModules(Path moduleCSVPath) throws EpptReportException
	{
		LOGGER.at(Level.INFO).log("Reading Detailed Issues Configuration");
		_detailedIssues.addAll(DetailedIssuesReader.getInstance().getDetailedIssues());
		LOGGER.at(Level.INFO).log("Reading Modules Configuration");
		ModulesReader modulesReader = new ModulesReader(moduleCSVPath);
		checkInterrupt();
		_modules.addAll(modulesReader.read());

		LOGGER.at(Level.INFO).log("Reading Linked Variables Configuration");
		addLinkedVariablesToSubModules();

		return _modules;
	}


	public List<DetailedIssue> getAllDetailedIssues()
	{
		return _detailedIssues;
	}

	private void addLinkedVariablesToSubModules() throws EpptReportException
	{
		for(Module mod : _modules)
		{
			for(SubModule sub : mod.getSubModules())
			{
				checkInterrupt();
				int id = sub.getId();
				List<String> linkedFiles = getLinkedFilesFromID(id);
				sub.addLinkedRecords(linkedFiles);
			}
		}
	}

	private List<String> getLinkedFilesFromID(int id)
	{
		List<String> retval = new ArrayList<>();

		for(DetailedIssue di : _detailedIssues)
		{
			if(di.getSubModuleID() == id)
			{
				retval.add(di.getLinkedVar());
			}
		}
		return retval;
	}

}
