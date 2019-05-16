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

import java.util.Set;
import java.util.logging.Level;

import com.google.common.flogger.FluentLogger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class AssumptionChangesXMLCreator
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private static final String ASSUMPTION_CHANGES = "assumption-changes";
	private static final String INPUT = "input";
	private static final String INPUT_TYPE = "input-type";

	private static final String INITIAL_CONDITIONS = "Initial Conditions";
	private static final String STATE_VARIABLES = "State Variables";

	private static final String COM_REC_DIF_DATA = "common-records-different-data";
	private static final String ONLY_IN_BASE = "records-only-in-base";
	private static final String ONLY_IN_ALT = "records-only-in-alternative";

	private static final String RECORD = "record";

	private static final String COMMON_RECORD = "Common Records, Different Data";
	private static final String BASE_ONLY = "Records only included in the Base Study";
	private static final String ALT_ONLY = "Records only included in the Alt Study";


	public Element createAssumptionChangesElement(Document document, FileChangesStatistics stats)
	{
		LOGGER.at(Level.INFO).log("Writing Assumption Changes to XML");
		Element assumptionChangesRoot = document.createElement(ASSUMPTION_CHANGES);
		AssumptionChangesStatistics initAssumptionStats = stats.getInitAssumptionStats();
		AssumptionChangesStatistics svAssumptionStats = stats.getSVAssumptionStats();

		assumptionChangesRoot.appendChild(createConditionElement(INITIAL_CONDITIONS, document, initAssumptionStats.getChangedFiles(),
				initAssumptionStats.getRecordsOnlyInBase(), initAssumptionStats.getRecordsOnlyInAlt()));

		assumptionChangesRoot.appendChild(createConditionElement(STATE_VARIABLES, document, svAssumptionStats.getChangedFiles(),
				svAssumptionStats.getRecordsOnlyInBase(), svAssumptionStats.getRecordsOnlyInAlt()));

		return assumptionChangesRoot;
	}


	private Element createConditionElement(String sectionTitle, Document document, Set<String> commonRecordsWithDifData,
										   Set<String> recordsInBaseOnly, Set<String> recordsInAltOnly)
	{
		Element conditionsElement = document.createElement(INPUT);
		conditionsElement.setAttribute(INPUT, sectionTitle);
		conditionsElement.setAttribute(COM_REC_DIF_DATA, Integer.toString(commonRecordsWithDifData.size()));
		conditionsElement.setAttribute(ONLY_IN_BASE, Integer.toString(recordsInBaseOnly.size()));
		conditionsElement.setAttribute(ONLY_IN_ALT, Integer.toString(recordsInAltOnly.size()));


		Element comRecDifDataElem = createInputTypeElement(COMMON_RECORD, commonRecordsWithDifData, document);
		Element recsOnlyInBaseElem = createInputTypeElement(BASE_ONLY, recordsInBaseOnly, document);
		Element onlyInAltElem = createInputTypeElement(ALT_ONLY, recordsInAltOnly, document);

		boolean atLeastOneInputTypeElem = false;

		if(comRecDifDataElem != null)
		{
			atLeastOneInputTypeElem = true;
			conditionsElement.appendChild(comRecDifDataElem);
		}
		if(recsOnlyInBaseElem != null)
		{
			atLeastOneInputTypeElem = true;
			conditionsElement.appendChild(recsOnlyInBaseElem);
		}
		if(onlyInAltElem != null)
		{
			atLeastOneInputTypeElem = true;
			conditionsElement.appendChild(onlyInAltElem);
		}

		if(	!atLeastOneInputTypeElem)
		{
			//create empty input type elem
			Element emptyInputTypeElement = createEmptyInputTypeElement(document);
			conditionsElement.appendChild(emptyInputTypeElement);
		}

		return conditionsElement;
	}

	private Element createEmptyInputTypeElement(Document document)
	{
		Element inputTypeElem = document.createElement(INPUT_TYPE);
		Element recordElem = document.createElement(RECORD);

		inputTypeElem.appendChild(recordElem);

		return inputTypeElem;
	}

	private Element createInputTypeElement(String inputType, Set<String> records, Document document)
	{
		Element inputTypeElem = document.createElement(INPUT_TYPE);
		inputTypeElem.setAttribute(INPUT_TYPE, inputType);

		if(records.isEmpty())
		{
			return null;
		}
		else
		{
			for(String rec : records)
			{
				inputTypeElem.appendChild(createRecordElem(rec, document));
			}
		}
		return inputTypeElem;
	}

	private Element createRecordElem(String record, Document document)
	{
		Element recordElem = document.createElement(RECORD);
		recordElem.setTextContent(record);
		return recordElem;
	}

}
