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

package gov.ca.water.reportengine.assumptionchanges;

import hec.heclib.dss.HecDss;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

    //type and subtype
    private static final String HYDROLOGY = "Hydrology";
    private static final String CLIMATE = "Climate";
    private static final String SYSTEM_CONNECTIVITY = "System-Connectivity";
    private static final String DEMANDS = "Demands";
    private static final String NORTH_OF_DELTA = "North of Delta";
    private static final String SOUTH_OF_DELTA = "South of Delta";
    private static final String SAN_JOAQUIN_RIVER = "San Joaquin River";

    private static final String FACILITIES = "Facilities";
    private static final String SYSTEM_WIDE = "System-wide";
    private static final String SACRAMENT0_VALLEY = "Sacramento Valley";
    private static final String DELTA_EXPORT_CONVEYANCE = "Delta Export Conveyance";
    private static final String RIVER_SPEC_REGS = "River-special Regulatory Standards &amp; Operations";

    private static final String TRINITY_RIVER = "Trinity River";
    private static final String CLEAR_CREEK = "Clear Creek";
    private static final String FEATHER_RIVER = "Feather River";
    private static final String AMERICAN_RIVER = "American River";
    private static final String SAC_RIVER = "Sacramento River";
    private static final String MOKELUMNE_RIVER = "Mokelumne River";
    private static final String STANISLAUS_RIVER = "Stanislaus River";
    private static final String MERCED_RIVER = "Merced River";
    private static final String TUOLUMNE_RIVER = "Tuolumne River";


    private static final String SAC_SANJOAQUIN_DELTA = "Sacramento - San Joaquin Delta";
    private static final String PROJECT_OPERATIONS  = "Project Operations";
    private static final String CVP_WATER_ALLOC = "CVP Water Allocation";
    private static final String SWP_WATER_ALLOC = "SWP Water Allocation";
    private static final String COORDINATED_OPERATIONS_AGREEMENT = "Coordinated Operations Agreement";
    private static final String CVPIA = "CVPIA (b)(2)";
    private static final String WHEELING = "Wheeling";

    private static final String SAN_LUIS = "San Luis";
    private static final String SHORTAGE = "Shortage";
    private static final String WSI_DI = "WSI - DI";
    private static final String WATER_MAN_ACTIONS = "Water Management Actions";
    private static final String SHORT_TERM_WAT_TRANS = "Short-term Water Transfers";
    private static final String LONG_TERM_WAT_TRANS = "Long-term Water Transfers";

    private static final String OTHERS = "Others";
    private static final String MODEL_SETUP = "Model Setup";
    private static final String LOOKUP_TABLES = "Lookup Tables";
    private static final String NEW = "*New";
    private static final String DELETED = "*Deleted";



    public Element appendCodeChangesElement(Document document)
    {

        Element codeChangesElemRoot = document.createElement(CODE_CHANGES);
        //create header
        List<String> altNames = new ArrayList<>();
        codeChangesElemRoot.appendChild(createHeader(altNames, document));

        //create subtypes
        List<String> changesList = new ArrayList<>();
        //hydrology
        Map<String, List<String>> subTypeMap = new HashMap<>();
        subTypeMap.put(CLIMATE, changesList);
        subTypeMap.put(SYSTEM_CONNECTIVITY, changesList);

        codeChangesElemRoot.appendChild(createTypeElement(HYDROLOGY, subTypeMap, document));

        //demands
        subTypeMap.put(NORTH_OF_DELTA, changesList);
        subTypeMap.put(SOUTH_OF_DELTA, changesList);
        subTypeMap.put(SAN_JOAQUIN_RIVER, changesList);

        codeChangesElemRoot.appendChild(createTypeElement(DEMANDS, subTypeMap, document));

        //facilities
        subTypeMap.put(SYSTEM_WIDE, changesList);
        subTypeMap.put(SACRAMENT0_VALLEY, changesList);
        subTypeMap.put(DELTA_EXPORT_CONVEYANCE, changesList);
        subTypeMap.put(SAN_JOAQUIN_RIVER, changesList);
        subTypeMap.put(SOUTH_OF_DELTA, changesList);

        codeChangesElemRoot.appendChild(createTypeElement(FACILITIES, subTypeMap, document));

        //river spec regs
        subTypeMap.put(TRINITY_RIVER, changesList);
        subTypeMap.put(CLEAR_CREEK, changesList);
        subTypeMap.put(FEATHER_RIVER, changesList);
        subTypeMap.put(AMERICAN_RIVER, changesList);
        subTypeMap.put(SAC_RIVER, changesList);
        subTypeMap.put(MOKELUMNE_RIVER, changesList);
        subTypeMap.put(STANISLAUS_RIVER, changesList);
        subTypeMap.put(MERCED_RIVER, changesList);
        subTypeMap.put(TUOLUMNE_RIVER, changesList);
        subTypeMap.put(SAN_JOAQUIN_RIVER, changesList);
        subTypeMap.put(SAC_SANJOAQUIN_DELTA, changesList);

        codeChangesElemRoot.appendChild(createTypeElement(RIVER_SPEC_REGS, subTypeMap, document));

        //river spec regs
        subTypeMap.put(CVP_WATER_ALLOC, changesList);
        subTypeMap.put(SWP_WATER_ALLOC, changesList);
        subTypeMap.put(COORDINATED_OPERATIONS_AGREEMENT, changesList);
        subTypeMap.put(CVPIA, changesList);
        subTypeMap.put(WHEELING, changesList);
        subTypeMap.put(MOKELUMNE_RIVER, changesList);
        subTypeMap.put(STANISLAUS_RIVER, changesList);
        subTypeMap.put(MERCED_RIVER, changesList);
        subTypeMap.put(TUOLUMNE_RIVER, changesList);
        subTypeMap.put(SAN_JOAQUIN_RIVER, changesList);
        subTypeMap.put(SAC_SANJOAQUIN_DELTA, changesList);

        codeChangesElemRoot.appendChild(createTypeElement(PROJECT_OPERATIONS, subTypeMap, document));


        return codeChangesElemRoot;
    }


    private Element createHeader(List<String> alternatives, Document document)
    {
        Element headerElem = document.createElement(HEADER);
        for(String alt : alternatives)
        {
            headerElem.appendChild(createAlternativeElem(alt, 99,99,document));
        }
        Element filesDeletedElem = document.createElement(FILES_DELETED_FROM_BASE);
        filesDeletedElem.setTextContent("99");

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

    private Element createTypeElement(String type, Map<String, List<String>> subTypeToChanges, Document document)
    {

        Element typeElem = document.createElement(TYPE);
        typeElem.setAttribute(CHANGE_TYPE, type);

        for(Map.Entry<String, List<String>> entry : subTypeToChanges.entrySet())
        {
            typeElem.appendChild(createSubtype(entry.getKey(), entry.getValue(), document));
        }

        return typeElem;
    }


    private Element createSubtype(String subtypeName, List<String> changes, Document document)
    {
        String updatedVal = changes.size()>0 ? "true" : "false";

        Element subTypeElem = document.createElement(SUBTYPE);
        subTypeElem.setAttribute(CHANGE_SUBTYPE, subtypeName);
        subTypeElem.setAttribute(UPDATED, updatedVal);

        //create the list of changes
        for(String change : changes)
        {
            Element changeElem = document.createElement(CHANGE);
            changeElem.setTextContent(change);
            subTypeElem.appendChild(changeElem);
        }
        return subTypeElem;
    }




}
