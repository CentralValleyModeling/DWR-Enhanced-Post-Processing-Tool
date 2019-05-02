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

import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.nio.file.Path;
import java.util.List;
import java.util.Set;

public class CodeChangesXMLCreator
{

    private static final String CODE_CHANGES = "code-changes";

    private static final String HEADER = "header";
    private static final String ALTERNATIVES = "alternatives";
    private static final String ALTERNATIVE = "alternative";
    private static final String FILES_UPDATED = "files-updated";

    private static final String FILES_ADDED_TO = "files-added-to";

    private static final String FILES_DELETED_FROM_BASE = "files-deleted-from-base";

    private static final String TYPE = "type";
    private static final String CHANGE_TYPE = "change-type";
    private static final String SUBTYPE = "subtype";

    private static final String CHANGE = "change";
    private static final String CHANGE_SUBTYPE = "change-subtype";
    private static final String UPDATED = "updated";






    public void appendCodeChangesElement(Path csvPath, Path baseOutputPath, Path altOutputPath, String altName, Document document) throws EpptReportException
    {

        CodeChangesDataProcessor processor = new CodeChangesDataProcessor(csvPath);
        FileChangesStatistics fileChangesStatistics = processor.processCodeChanges(baseOutputPath, altOutputPath);
        List<CodeChangesType> codeChangesTypes = processor.getCodeChangesTypes();

        Element codeChangesElemRoot = document.createElement(CODE_CHANGES);

        //create header
        codeChangesElemRoot.appendChild(createHeader(altName, fileChangesStatistics, document));


        for(CodeChangesType cct : codeChangesTypes)
        {
            codeChangesElemRoot.appendChild(createTypeElement(cct,fileChangesStatistics, document));
        }

        document.appendChild(codeChangesElemRoot);
    }


    private Element createHeader(String altName, FileChangesStatistics stats, Document document)
    {
        Element headerElem = document.createElement(HEADER);
        Element alternativesElem = document.createElement(ALTERNATIVES);
        headerElem.appendChild(alternativesElem);

        alternativesElem.appendChild(createAlternativeElem(altName, stats.getChangedFiles().size(),stats.getRecordsOnlyInAlt().size(),document));

        Element filesDeletedElem = document.createElement(FILES_DELETED_FROM_BASE);
        filesDeletedElem.setTextContent(Integer.toString(stats.getFilesDeletedFromBase().size()));

        headerElem.appendChild(filesDeletedElem);
        return headerElem;
    }

    private Element createAlternativeElem(String altName, int filesUpdated, int filesAdded, Document document)
    {
        Element altElem = document.createElement(ALTERNATIVE);
        altElem.setAttribute("name",altName);

        Element filesUpdatedElem = document.createElement(FILES_UPDATED);
        filesUpdatedElem.setTextContent(Integer.toString(filesUpdated));
        altElem.appendChild(filesUpdatedElem);

        Element filesAddedElem = document.createElement(FILES_ADDED_TO);
        filesAddedElem.setTextContent(Integer.toString(filesAdded));
        altElem.appendChild(filesAddedElem);

        return altElem;
    }

    private Element createTypeElement(CodeChangesType type,FileChangesStatistics stats, Document document)
    {

        Element typeElem = document.createElement(TYPE);
        typeElem.setAttribute(CHANGE_TYPE, type.getName());

        List<CodeChangesSubType> subTypes = type.getSubTypes();

        for(CodeChangesSubType subType : subTypes)
        {
            typeElem.appendChild(createSubtype(subType, stats, document));
        }

        return typeElem;
    }


    private Element createSubtype(CodeChangesSubType subType,FileChangesStatistics stats, Document document)
    {
        Set<String> modifiedFilesForSubtype = stats.getModifiedFilesForSubtype(subType);
        String updatedVal = modifiedFilesForSubtype.size()>0 ? "true" : "false";

        Element subTypeElem = document.createElement(SUBTYPE);
        subTypeElem.setAttribute(CHANGE_SUBTYPE, subType.getName());
        subTypeElem.setAttribute(UPDATED, updatedVal);

        //create the list of changes
        for(String change : modifiedFilesForSubtype)
        {
            Element changeElem = document.createElement(CHANGE);
            changeElem.setTextContent(change);
            subTypeElem.appendChild(changeElem);
        }
        return subTypeElem;
    }




}
