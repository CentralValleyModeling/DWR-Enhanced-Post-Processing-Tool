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

import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.reportengine.executivereport.Module;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class DetailedIssuesXMLCreator
{
	private static final Logger LOGGER = Logger.getLogger(DetailedIssuesXMLCreator.class.getName());

	private static final String ISSUES_REPORT = "issues-report";
	private static final String ISSUES_TYPE = "issue-type";
	private static final String ISSUE = "issue";
	private static final String NAME = "name";
	private static final String LOCATION = "location";
	private static final String BASE = "base";
	private static final String ALTERNATIVES = "alternatives";
	private static final String ALTERNATIVE = "alternative";
	private static final String ALTERNATIVE_NAME = "alternative-name";
	private static final String NUM_ISSUES = "number-of-issues";


	public Element createDetailedIssuesElement(Map<EpptScenarioRun, Map<Module, List<DetailedIssueViolation>>> runsToModViolations,
											   EpptScenarioRun baseRun, List<EpptScenarioRun> altRuns, List<Module> modules, Document document)
	{
		Element rootElem = document.createElement(ISSUES_REPORT);

		if(runsToModViolations.containsKey(baseRun))
		{
			Element baseDetailedIssues = createBaseDetailedIssues(runsToModViolations.get(baseRun), modules, document);
			rootElem.appendChild(baseDetailedIssues);
		}
		else
		{
			LOGGER.error("Unable to create base detailed issue report because the scenario run " + baseRun.toString() +
					" was not found in the map of runs to violations.");
		}

		Element alternativesElem = document.createElement(ALTERNATIVES);
		for(EpptScenarioRun run : altRuns)
		{
			if(runsToModViolations.containsKey(run))
			{
				Element alternativeDetailedIssues = createAlternativeDetailedIssues(run.getName(), runsToModViolations.get(run),
						modules, document);
				alternativesElem.appendChild(alternativeDetailedIssues);
			}
		}

		rootElem.appendChild(alternativesElem);
		return rootElem;
	}

	private Element createAlternativeDetailedIssues(String altName, Map<Module, List<DetailedIssueViolation>> modsToViolations, List<Module> modules,
													Document document)
	{
		Element altElem = document.createElement(ALTERNATIVE);
		altElem.setAttribute(ALTERNATIVE_NAME, altName);
		for(Module mod : modules)
		{
			List<DetailedIssueViolation> divs = modsToViolations.get(mod);
			altElem.appendChild(createIssueTypeElement(mod, divs, document));
		}
		return altElem;
	}

	private Element createBaseDetailedIssues(Map<Module, List<DetailedIssueViolation>> modsToViolations, List<Module> modules, Document document)
	{
		Element baseElem = document.createElement(BASE);
		for(Module mod : modules)
		{
			List<DetailedIssueViolation> divs = modsToViolations.get(mod);
			if(divs != null)
			{
				baseElem.appendChild(createIssueTypeElement(mod, divs, document));
			}
		}
		return baseElem;
	}

	private Element createIssueTypeElement(Module mod, List<DetailedIssueViolation> violations, Document document)
	{
		Element issuesTypeElem = document.createElement(ISSUES_TYPE);
		issuesTypeElem.setAttribute(NAME, mod.getName());
		int totalNumIssues = 0;
		for(DetailedIssueViolation div : violations)
		{
			totalNumIssues+= div.getTotalNumberOfViolations();
		}
		issuesTypeElem.setAttribute(NUM_ISSUES, Integer.toString(totalNumIssues));

		if(violations.isEmpty())
		{
			//we still want an empty location element
			Element emptyLocationElement = createEmptyLocationElement(document);
			issuesTypeElem.appendChild(emptyLocationElement);
		}
		else
		{
			for(DetailedIssueViolation div : violations)
			{
				Element locationElement = createLocationElement(div, document);
				issuesTypeElem.appendChild(locationElement);
			}
		}

		return issuesTypeElem;
	}

	private Element createEmptyLocationElement(Document document)
	{
		Element locationElem = document.createElement(LOCATION);
		Element issueElement = createEmptyIssueElement(document);
		locationElem.appendChild(issueElement);

		return locationElem;
	}

	private Element createEmptyIssueElement(Document document)
	{
		return document.createElement(ISSUE);

	}

	private Element createLocationElement(DetailedIssueViolation div, Document document)
	{
		Element locationElem = document.createElement(LOCATION);
		locationElem.setAttribute(LOCATION, div.getTitle());

		if(div.getIssues().isEmpty())
		{
			Element issueElement = createEmptyIssueElement(document);
			locationElem.appendChild(issueElement);
		}
		else
		{
			for(DetailedIssueViolation.Issue issue : div.getIssues())
			{
				Element issueElement = createIssueElement(issue.toString(), document);
				locationElem.appendChild(issueElement);
			}
		}
		return locationElem;
	}

	private Element createIssueElement(String value, Document document)
	{
		Element issueElem = document.createElement(ISSUE);
		issueElem.setTextContent(value);
		return issueElem;
	}

}
