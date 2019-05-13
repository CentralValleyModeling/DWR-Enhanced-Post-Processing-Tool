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
import gov.ca.water.reportengine.executivereport.FlagViolation;
import gov.ca.water.reportengine.executivereport.Module;
import gov.ca.water.reportengine.executivereport.SubModule;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class DetailedIssuesXMLCreator
{
    private static final String ISSUES_REPORT = "issues-report";
    private static final String ISSUES_TYPE = "issue-type";
    private static final String ISSUE = "issue";
    private static final String NAME = "name";
    private static final String LOCATION = "location";
    private static final String BASE = "base";
    private static final String ALTERNATIVES = "alternatives";
    private static final String ALTERNATIVE = "alternative";


    public void appendDetailedIssuesElement(Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToModViolations, Document document)
    {

        Element rootElem = document.createElement(ISSUES_REPORT);

        Element baseElem = document.createElement(BASE);

        for(Module mod : modules)
        {
            baseElem.appendChild(createIssueTypeElement(mod, document));
        }
        rootElem.appendChild(baseElem);

        document.appendChild(rootElem);
    }


    private Element createIssueTypeElement(Module mod, Document document)
    {
        Element issuesTypeElem = document.createElement(ISSUES_TYPE);
        issuesTypeElem.setAttribute(NAME, mod.getName());



        //issuesTypeElem.appendChild(createLocationElement(getLocationName(), , document));




        return issuesTypeElem;
    }

    private String getTitle(Module mod)
    {
        //List<String> dtsFileNames = new ArrayList<>();
        List<DetailedIssue> detailedIssues = new ArrayList<>();
        List<SubModule> subModules = mod.getSubModules();
        for(SubModule subMod : subModules)
        {
            //List<FlagViolation> baseViolations = subMod.getBaseViolations();
            //for(FlagViolation violation : baseViolations)
            {
                //DetailedIssue di = new DetailedIssue(subMod.getId(), violation.getTimes(), violation.getDtsFileName());
                //detailedIssues.add(di);
                //dtsFileNames.add(violation.getDtsFileName());
            }
        }
        //now i have all the dtsfile names
        return "";
    }




    private Element createLocationElement(String locationName, List<FlagViolation> violations, Document document)
    {
        Element locationElem = document.createElement(locationName);
        for(FlagViolation violation : violations)
        {

            locationElem.appendChild(createIssueElement(document,getIssueString()));
        }
        return locationElem;
    }

    private String getLocationName()
    {
        return "";
    }

    private String getIssueString()
    {
        return "";
    }

    private Element createIssueElement(Document document, String value)
    {
        Element issueElem = document.createElement(ISSUE);
        issueElem.setTextContent(value);
        return issueElem;
    }

}
