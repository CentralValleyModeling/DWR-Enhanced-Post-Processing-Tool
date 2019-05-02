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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.util.HashSet;
import java.util.Set;

public class AssumptionChangesXMLCreator
{

    private static final String ASSUMPTION_CHANGES = "assumption-changes";

    private static final String INITIAL_CONDITIONS = "initial-conditions";
    private static final String COM_REC_DIF_DATA = "common-records-different-data";
    private static final String ONLY_IN_BASE = "records-only-in-base";
    private static final String ONLY_IN_ALT = "records-only-in-alternative";

    private static final String STATE_VARS = "state-variables";

    private static final String RECORDS_LIST = "records-list";

    private static final String RECORD = "record";



    void appendAssumptionChangesElement(Document document, FileChangesStatistics initCondStats, FileChangesStatistics stateVarStats)
    {
        Element assumptionChangesRoot = document.createElement(ASSUMPTION_CHANGES);

        assumptionChangesRoot.appendChild(createInitialConditionsElement(document, initCondStats.getChangedFiles().size(),
                initCondStats.getRecordsOnlyInBase().size(), initCondStats.getRecordsOnlyInAlt().size()));

        assumptionChangesRoot.appendChild(createStateVariablesElement(document, stateVarStats.getChangedFiles().size(),
                stateVarStats.getRecordsOnlyInBase().size(), stateVarStats.getRecordsOnlyInAlt().size()));

        Set<String> totalChanges = new HashSet<>();
        totalChanges.addAll(initCondStats.getChangedFiles());
        totalChanges.addAll(stateVarStats.getChangedFiles());

        assumptionChangesRoot.appendChild(createRecordsList(totalChanges,document));
        document.appendChild(assumptionChangesRoot);
    }


    private Element createInitialConditionsElement(Document document, int commonRecordsWithDifData, int recordsInBaseOnly, int recordsInAltOnly)
{
    Element initConditionsElement = document.createElement(INITIAL_CONDITIONS);
    initConditionsElement.appendChild(createCommonRecDifData(commonRecordsWithDifData, document));
    initConditionsElement.appendChild(createOnlyInBase(recordsInBaseOnly, document));
    initConditionsElement.appendChild(createOnlyInAlt(recordsInAltOnly, document));
    return initConditionsElement;
}

    private Element createStateVariablesElement(Document document, int commonRecordsWithDifData, int recordsInBaseOnly, int recordsInAltOnly)
    {
        Element stateVarElement = document.createElement(STATE_VARS);
        stateVarElement.appendChild(createCommonRecDifData(commonRecordsWithDifData, document));
        stateVarElement.appendChild(createOnlyInBase(recordsInBaseOnly, document));
        stateVarElement.appendChild(createOnlyInAlt(recordsInAltOnly, document));
        return stateVarElement;
    }

    private Element createRecordsList(Set<String> records, Document document)
    {
        Element recordsRootElem = document.createElement(RECORDS_LIST);

        for(String rec: records)
        {
            recordsRootElem.appendChild(createRecordElem(rec,document));
        }
        return recordsRootElem;
    }

    private Element createRecordElem(String record,  Document document)
    {
        Element recordElem = document.createElement(RECORD);
        recordElem.setTextContent(record);
        return recordElem;
    }

    private Element createCommonRecDifData(int numRecordsWithDifData,  Document document)
    {
        Element comRecordDifDataElem = document.createElement(COM_REC_DIF_DATA);
        comRecordDifDataElem.setTextContent(Integer.toString(numRecordsWithDifData));
        return comRecordDifDataElem;
    }

    private Element createOnlyInBase(int numRecordsWithDifData,  Document document)
    {
        Element onlyInBaseElem = document.createElement(ONLY_IN_BASE);
        onlyInBaseElem.setTextContent(Integer.toString(numRecordsWithDifData));
        return onlyInBaseElem;
    }

    private Element createOnlyInAlt(int numRecordsWithDifData,  Document document)
    {
        Element onlyInAltElem = document.createElement(ONLY_IN_ALT);
        onlyInAltElem.setTextContent(Integer.toString(numRecordsWithDifData));
        return onlyInAltElem;
    }

}
