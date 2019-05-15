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
import org.w3c.dom.Document;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TestDetailedIssueReport extends TestDetailedIssuesReportBase
{


    @Test
    void testDetailedIssueSameModel() throws Exception
    {
//        WaterYearTableReader wyTypeReader = new WaterYearTableReader(getWyTypeTablePath(), getWyTypeNameLookupTablePath());
//        List<WaterYearType> waterYearTypes = wyTypeReader.read();

        EpptScenarioRun baseScenarioRun = getBaseScenarioRun();
        List<EpptScenarioRun> altScenarioRuns = getAltScenarioRuns();
        List<EpptScenarioRun> allRuns = new ArrayList<>();
        allRuns.add(baseScenarioRun);
        allRuns.addAll(altScenarioRuns);

        ModuleCreator mc = new ModuleCreator();
        List<Module> modules = mc.createModules(getCSVPath(), getModuleLinkingCSVPath());
        List<DetailedIssue> allDetailedIssues = mc.getAllDetailedIssues();

        DTSProcessor dtsProcessor = new DTSProcessor(modules);
        Map<EpptScenarioRun, Map<SubModule, List<FlagViolation>>> runsToFlagViolations = dtsProcessor.processDSSFiles(allRuns, getDssFilePathsForSameModel());

        Document doc = getDoc();

        DetailedIssueProcessor processor = new DetailedIssueProcessor(getWyTypeTablePath(), getWyTypeNameLookupTablePath(), runsToFlagViolations, modules, allDetailedIssues, allRuns,true);
        Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToDetailedViolations = processor.process();

        DetailedIssuesXMLCreator xmlCreator = new DetailedIssuesXMLCreator();
        xmlCreator.createDetailedIssuesElement(runsToDetailedViolations, baseScenarioRun, altScenarioRuns, modules,doc);

        Path path = writeXmlFile(doc);

        int test = 0;

    }

}
