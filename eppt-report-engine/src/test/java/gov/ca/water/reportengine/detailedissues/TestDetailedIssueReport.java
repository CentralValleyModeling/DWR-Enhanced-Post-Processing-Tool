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

package gov.ca.water.reportengine.detailedissues;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.ModuleCreator;
import gov.ca.water.reportengine.executivereport.DTSProcessor;
import gov.ca.water.reportengine.executivereport.FlagViolation;
import gov.ca.water.reportengine.executivereport.Module;
import gov.ca.water.reportengine.executivereport.SubModule;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TestDetailedIssueReport extends TestDetailedIssuesReportBase
{


//    @Test
    void testDetailedIssueSameModel() throws Exception
    {
        
        EpptScenarioRun baseScenarioRun = getBaseScenarioRun();
        List<EpptScenarioRun> altScenarioRuns = getAltScenarioRuns();
        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(baseScenarioRun);
        allRuns.addAll(altScenarioRuns);

        ModuleCreator mc = new ModuleCreator();
        List<Module> modules = mc.createModules(getCSVPath(), getModuleLinkingCSVPath());
        List<DetailedIssue> allDetailedIssues = mc.getAllDetailedIssues();

        DTSProcessor dtsProcessor = new DTSProcessor(modules);
        Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToViolations = dtsProcessor.processDSSFiles(allRuns, getDssFilePathsForSameModel());



        DetailedIssueProcessor processor = new DetailedIssueProcessor(runsToViolations, modules, allDetailedIssues, allRuns,true);
        Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> process = processor.process();
        //Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToViolations = processor.process();

        int test = 0;

    }

}
