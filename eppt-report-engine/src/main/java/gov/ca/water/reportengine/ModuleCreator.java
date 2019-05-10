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

import gov.ca.water.reportengine.detailedissues.DetailedIssue;
import gov.ca.water.reportengine.executivereport.ExecutiveReportException;
import gov.ca.water.reportengine.executivereport.Module;
import gov.ca.water.reportengine.executivereport.SubModule;
import gov.ca.water.reportengine.reportreaders.DetailedIssuesReader;
import gov.ca.water.reportengine.reportreaders.ModulesReader;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class ModuleCreator
{
    private final List<DetailedIssue> _detailedIssues = new ArrayList<>();
    private final List<Module> _modules = new ArrayList<>();

    public ModuleCreator()
    {

    }

    public List<Module> createModules(Path moduleCSVPath, Path detailedIssuesCSVPath) throws EpptReportException
    {
        DetailedIssuesReader diReader = new DetailedIssuesReader(detailedIssuesCSVPath);
        _detailedIssues.addAll(diReader.read());

        ModulesReader modulesReader = new ModulesReader(moduleCSVPath);
        _modules.addAll(modulesReader.read());

        addLinkedVariablesToSubModules();

        return _modules;
    }


    public List<DetailedIssue> getAllDetailedIssues()
    {
        return _detailedIssues;
    }

    private void addLinkedVariablesToSubModules() throws ExecutiveReportException
    {
        for(Module mod: _modules)
        {
            for(SubModule sub: mod.getSubModules())
            {
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
