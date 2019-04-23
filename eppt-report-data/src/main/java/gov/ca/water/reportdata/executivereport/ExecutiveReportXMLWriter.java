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

package gov.ca.water.reportdata.executivereport;

import gov.ca.water.reportdata.ReportHeaderXMLWriter;
import org.apache.log4j.Logger;
import org.w3c.dom.*;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public class ExecutiveReportXMLWriter
{
    private static Logger LOGGER = Logger.getLogger(ReportHeaderXMLWriter.class.getName());
    private static final String REPORT_HEADER = "executive-report";
    private static final String ALTERNATIVES = "alternative";
    static final String NEW_LINE = "&#xD;";

    static final String MODULE = "module";
    static final String MODEL_ENTRIES = "model-entries";
    static final String NAME = "name";
    static final String STUDY = "study";
    static final String STUDY_ORDER = "study-order";
    static final String MODEL_ORDER = "model-order";

    //header entries
    static final String REPORT_HEADER_ENTRIES = "executive-report-header-entries";
    static final String BASE_VIOLATIONS = "base-violations";
    static final String ALTERNATIVE_VIOLATIONS = "alternative-violations";
    static final String PCT_CHANGE_VIOLATIONS = "pct-change-violations";

    static final String AVG_ANNUAL_EXPORT_PREV = "avg-annual-export-previous";
    static final String AVG_ANNUAL_EXPORT_CUR = "avg-annual-export-current";

    static final String ANNUAL_EXPORT_INC_DEC = "annual-export-increase-decrease";

    static final String AVG_CVP_NOD_STORAGE_PREV = "avg-cvp-nod-storage-previous";
    static final String AVG_CVP_NOD_STORAGE_CUR = "avg-cvp-nod-storage-current";

    static final String CVP_NOD_STORAGE_INC_DEC = "cvp-nod-storage-increase-decrease";

    static final String AVG_SWP_NOD_STOR_PREV = "avg-swp-nod-storage-previous";
    static final String AVG_SWP_NOD_STOR_CUR = "avg-swp-nod-storage-current";

    static final String SWP_NOD_STORAGE_INC_DEC = "swp-nod-storage-increase-decrease";

    //Model inputs row
    static final String MODEL_INPUTS_TAG = "model-inputs";
    static final String MI = "Model Inputs";
    static final String BASE = "Base";

    //reservoir operations row
    static final String RES_OPS_TAG = "reservoir-operation";
    static final String RES_OPS = "Reservoir Operation";

    //Upstream mif reqs
    static final String UPSTREAM_MIF_REQ_TAG = "upstream-mif-req";
    static final String UPSTREAM_MIF_REQ = "Upstream MIF Requirements";

    //nod weirs ops
    static final String NOD_WEIR_TAG = "nod-weirs-op";
    static final String NOD_WEIR = "NOD Weirs Operations";
    static final String INCIDENTS_SPILLING = "incidents-spilling";

    //nod groundwater pumping
    static final String NOD_GROUND_PUMP_TAG = "nod-groundwater-pumping";
    static final String NOD_GROUND_PUMP = "NOD Groundwater Pumping";

    //delta operations row
    static final String DELTA_OPS_TAG = "delta-operation";
    static final String DELTA_OPS = "Delta Operation";

    //COA row
    static final String COA_TAG = "coa";
    static final String COA = "COA";
    static final String PERCENT_DIFF = "percent-diff";

    //Allocations and deliveries row
    static final String ALLOCATIONS_DEL_TAG = "allocations-deliveries";
    static final String ALLOCATIONS_DEL = "Allocations and Deliveries";
    static final String INCIDENTS_SHORTAGE = "incidents-shortage";

    //mass balance row
    static final String MASS_BALANCE_TAG = "mass-balance";
    static final String MASS_BALANCE = "Mass Balance";
    static final String INCIDENTS_BALANCE = "incidents-balance";

    //values from the helper xml
    private Document _helperDoc;
    static final String INCIDENTS_VIOLATION = "incidents-violation";


    public ExecutiveReportXMLWriter()
    {

    }

    public void write(String filePath, ExecutiveReport er)
    {

        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        try
        {
            loadHelperXMLFile();

            DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
            Document doc = documentBuilder.newDocument();

            //root
            Element root = doc.createElement(REPORT_HEADER);
            Attr numAlts = doc.createAttribute(ALTERNATIVES);
            numAlts.setValue(Integer.toString(er.getNumberOfAlternatives()));
            root.setAttributeNode(numAlts);
            doc.appendChild(root);

            //report header
            //Element reportHeaderElem = createExecutiveReportHeaderElement(er.getExecutiveReportHeader(), doc);
            //root.appendChild(reportHeaderElem);

            //executive report table
            Element execReportTableElem = doc.createElement("executive-report-table");

            //model inputs row
            List<Element> modelInputRowElems = createModelInputsRow(er.getModelInputs(), doc);
            for (Element elem : modelInputRowElems)
            {
                execReportTableElem.appendChild(elem);
            }

            //reservoir operation row
            List<Element> reservoirOperationsRowElems = createReservoirOperationsRow(er.getReservoirOperations(), doc);
            for(Element elem : reservoirOperationsRowElems)
            {
                execReportTableElem.appendChild(elem);
            }

            //upstream MIF requirements row
            List<Element> upstreamMifs = createUpstreamMIFReqRow(er.getUpstreamMIFReqs(), doc);
            for(Element elem : upstreamMifs)
            {
                execReportTableElem.appendChild(elem);
            }

            //nod weirs operations row
            List<Element> nodWeirs = createNodWeirOpRow(er.getNodWeirsOperations(), doc);
            for(Element elem : nodWeirs)
            {
                execReportTableElem.appendChild(elem);
            }

            //nod groundwater pumping row
            List<Element> nodGroundPumpElems = createNodGroundwaterPumpingRow(er.getNodGroundwaterPumpings(), doc);
            for(Element elem : nodGroundPumpElems)
            {
                execReportTableElem.appendChild(elem);
            }

            //delta ops row
            List<Element> deltaOpsElems = createDeltaOpRow(er.getDeltaOperations(), doc);
            for(Element elem : deltaOpsElems)
            {
                execReportTableElem.appendChild(elem);
            }

            //COA row
            List<Element> coaRow = createCOARow(er.getCOAs(), doc);
            for(Element elem : coaRow)
            {
                execReportTableElem.appendChild(elem);
            }

            //allocations and deliveries row
            List<Element> allocationDels = createAllocationsAndDeliveriesRow(er.getAllocationDeliveries(), doc);
            for(Element elem : allocationDels)
            {
                execReportTableElem.appendChild(elem);
            }

            //mass balance row
            List<Element> massBalances = createMassBalanceRow(er.getMassBalances(), doc);
            for(Element elem : massBalances)
            {
                execReportTableElem.appendChild(elem);
            }

            root.appendChild(execReportTableElem);
            //write the file out
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            DOMSource domSource = new DOMSource(doc);
            StreamResult streamResult = new StreamResult(new File(filePath));

            transformer.transform(domSource, streamResult);
        }
        catch (ParserConfigurationException e)
        {
            LOGGER.fatal(e.getMessage(), e);
        }
        catch (TransformerConfigurationException e)
        {
            e.printStackTrace();
        }
        catch (TransformerException e)
        {
            e.printStackTrace();
        }
    }

//    private Element createExecutiveReportHeaderElement(ExecutiveReportHeader erHeader, Document doc)
//    {
//        Element headerEntriesElem = doc.createElement(REPORT_HEADER_ENTRIES);
//
//        Element baseViolationsElem = doc.createElement(BASE_VIOLATIONS);
//        baseViolationsElem.appendChild(doc.createTextNode(Integer.toString(erHeader.getBaseViolations())));
//        headerEntriesElem.appendChild(baseViolationsElem);
//
//        Element altViolationsElem = doc.createElement(ALTERNATIVE_VIOLATIONS);
//        altViolationsElem.appendChild(doc.createTextNode(Integer.toString(erHeader.getAltViolations())));
//        headerEntriesElem.appendChild(altViolationsElem);
//
//        Element pctChangeViolationsElem = doc.createElement(PCT_CHANGE_VIOLATIONS);
//        pctChangeViolationsElem.appendChild(doc.createTextNode(erHeader.getPctChangeViolations()));
//        headerEntriesElem.appendChild(pctChangeViolationsElem);
//
//        Element avgAnnualExportPrev = doc.createElement(AVG_ANNUAL_EXPORT_PREV);
//        avgAnnualExportPrev.appendChild(doc.createTextNode(Double.toString(erHeader.getAvgAnnualExportPrev())));
//        headerEntriesElem.appendChild(avgAnnualExportPrev);
//
//        Element avgAnnualExportCur = doc.createElement(AVG_ANNUAL_EXPORT_CUR);
//        avgAnnualExportCur.appendChild(doc.createTextNode(Double.toString(erHeader.getAvgAnnualExportCur())));
//        headerEntriesElem.appendChild(avgAnnualExportCur);
//
//        Element annualExportIncDec = doc.createElement(ANNUAL_EXPORT_INC_DEC);
//        annualExportIncDec.appendChild(doc.createTextNode(erHeader.getAnnualExportIncDec()));
//        headerEntriesElem.appendChild(annualExportIncDec);
//
//        Element avgCvpNodPrev = doc.createElement(AVG_CVP_NOD_STORAGE_PREV);
//        avgCvpNodPrev.appendChild(doc.createTextNode(Double.toString(erHeader.getAvgCVPNodStoragePrev())));
//        headerEntriesElem.appendChild(avgCvpNodPrev);
//
//        Element avgCvpNodCur = doc.createElement(AVG_CVP_NOD_STORAGE_CUR);
//        avgCvpNodCur.appendChild(doc.createTextNode(Double.toString(erHeader.getAvgCVPNodStorageCur())));
//        headerEntriesElem.appendChild(avgCvpNodCur);
//
//        Element cvpNodStorIncDec = doc.createElement(CVP_NOD_STORAGE_INC_DEC);
//        cvpNodStorIncDec.appendChild(doc.createTextNode(erHeader.getCvpNodStorIncDec()));
//        headerEntriesElem.appendChild(cvpNodStorIncDec);
//
//        Element avgSwpNodStorPrev = doc.createElement(AVG_SWP_NOD_STOR_PREV);
//        avgSwpNodStorPrev.appendChild(doc.createTextNode(Double.toString(erHeader.getAvgSWPNodStorPrev())));
//        headerEntriesElem.appendChild(avgSwpNodStorPrev);
//
//        Element avgSwpNodStorCur = doc.createElement(AVG_SWP_NOD_STOR_CUR);
//        avgSwpNodStorCur.appendChild(doc.createTextNode(Double.toString(erHeader.getAvgSWPNodStorCur())));
//        headerEntriesElem.appendChild(avgSwpNodStorCur);
//
//        Element swpNodStorIncDec = doc.createElement(SWP_NOD_STORAGE_INC_DEC);
//        swpNodStorIncDec.appendChild(doc.createTextNode(erHeader.getSwpNodStorIncDec()));
//        headerEntriesElem.appendChild(swpNodStorIncDec);
//
//        return headerEntriesElem;
//    }

    private List<Element> createMassBalanceRow(List<MassBalance> massBals, Document doc)
    {
        String incidents = "";

        NodeList nodNodes = _helperDoc.getElementsByTagName(MASS_BALANCE_TAG);
        if (nodNodes.getLength() > 0)
        {
            Node nodeNode = nodNodes.item(0);
            if (nodeNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element upstreamElem = (Element) nodeNode;

                incidents = upstreamElem.getElementsByTagName(INCIDENTS_BALANCE).item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!massBals.isEmpty())
        {
            //create the base column element
            MassBalance baseMassBal = massBals.get(0);
            String percDiffFormatted = String.format(incidents, baseMassBal.getIncedents());

            retVal.add(createBaseModuleElement(doc, 9, MASS_BALANCE, percDiffFormatted));

            //we already did the first one for "base" get rid of it and go over the rest
            massBals.remove(0);
            //create elements for each alternative
            int i = 2;
            for (MassBalance massBal : massBals)
            {
                percDiffFormatted = String.format(incidents, massBal.getIncedents());

                retVal.add(createAlternativeModuleElement(doc, i, 9, MASS_BALANCE, percDiffFormatted));
                i++;
            }

        }
        return retVal;
    }
    private List<Element> createAllocationsAndDeliveriesRow(List<AllocationsDeliveries> allDels, Document doc)
    {
        String incidents = "";

        NodeList nodNodes = _helperDoc.getElementsByTagName(ALLOCATIONS_DEL_TAG);
        if (nodNodes.getLength() > 0)
        {
            Node nodeNode = nodNodes.item(0);
            if (nodeNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element upstreamElem = (Element) nodeNode;

                incidents = upstreamElem.getElementsByTagName(INCIDENTS_SHORTAGE).item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!allDels.isEmpty())
        {
            //create the base column element
            AllocationsDeliveries baseAllDel = allDels.get(0);
            String percDiffFormatted = String.format(incidents, baseAllDel.getIncedents());

            retVal.add(createBaseModuleElement(doc, 8, ALLOCATIONS_DEL, percDiffFormatted));

            //we already did the first one for "base" get rid of it and go over the rest
            allDels.remove(0);
            //create elements for each alternative
            int i = 2;
            for (AllocationsDeliveries allDel : allDels)
            {
                percDiffFormatted = String.format(incidents, allDel.getIncedents());

                retVal.add(createAlternativeModuleElement(doc, i, 8, ALLOCATIONS_DEL, percDiffFormatted));
                i++;
            }

        }
        return retVal;
    }

    private List<Element> createCOARow(List<COA> coas, Document doc)
    {
        String percDiff = "";

        NodeList nodNodes = _helperDoc.getElementsByTagName(COA_TAG);
        if (nodNodes.getLength() > 0)
        {
            Node nodeNode = nodNodes.item(0);
            if (nodeNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element upstreamElem = (Element) nodeNode;

                percDiff = upstreamElem.getElementsByTagName(PERCENT_DIFF).item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!coas.isEmpty())
        {
            //create the base column element
            COA baseCoa = coas.get(0);
            String percDiffFormatted = String.format(percDiff, baseCoa.getPercentDifference());

            retVal.add(createBaseModuleElement(doc, 7, COA, percDiffFormatted));

            //we already did the first one for "base" get rid of it and go over the rest
            coas.remove(0);
            //create elements for each alternative
            int i = 2;
            for (COA coa : coas)
            {
                percDiffFormatted = String.format(percDiff, coa.getPercentDifference());

                retVal.add(createAlternativeModuleElement(doc, i, 7, COA, percDiffFormatted));
                i++;
            }

        }
        return retVal;
    }

    private List<Element> createDeltaOpRow(List<DeltaOperation> deltaOps, Document doc)
    {
        String incidents = "";

        NodeList nodNodes = _helperDoc.getElementsByTagName(DELTA_OPS_TAG);
        if (nodNodes.getLength() > 0)
        {
            Node nodeNode = nodNodes.item(0);
            if (nodeNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element upstreamElem = (Element) nodeNode;

                incidents = upstreamElem.getElementsByTagName(INCIDENTS_VIOLATION).item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!deltaOps.isEmpty())
        {
            //create the base column element
            DeltaOperation baseDeltaOp = deltaOps.get(0);
            String incidentsViolation = String.format(incidents, baseDeltaOp.getIncedents());

            retVal.add(createBaseModuleElement(doc, 6, DELTA_OPS, incidentsViolation));

            //we already did the first one for "base" get rid of it and go over the rest
            deltaOps.remove(0);
            //create elements for each alternative
            int i = 2;
            for (DeltaOperation nod : deltaOps)
            {
                incidentsViolation = String.format(incidents, nod.getIncedents());

                retVal.add(createAlternativeModuleElement(doc, i, 6, DELTA_OPS, incidentsViolation));
                i++;
            }

        }
        return retVal;
    }

    private List<Element> createNodGroundwaterPumpingRow(List<NodGroundwaterPumping> nodPumps, Document doc)
    {
        String incidents = "";

        NodeList nodNodes = _helperDoc.getElementsByTagName(NOD_GROUND_PUMP_TAG);
        if (nodNodes.getLength() > 0)
        {
            Node nodeNode = nodNodes.item(0);
            if (nodeNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element upstreamElem = (Element) nodeNode;

                incidents = upstreamElem.getElementsByTagName(INCIDENTS_VIOLATION).item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!nodPumps.isEmpty())
        {
            //create the base column element
            NodGroundwaterPumping baseNod = nodPumps.get(0);
            String incidentsViolation = String.format(incidents, baseNod.getIncedents());

            retVal.add(createBaseModuleElement(doc, 5, NOD_GROUND_PUMP, incidentsViolation));

            //we already did the first one for "base" get rid of it and go over the rest
            nodPumps.remove(0);
            //create elements for each alternative
            int i = 2;
            for (NodGroundwaterPumping nod : nodPumps)
            {
                incidentsViolation = String.format(incidents, nod.getIncedents());

                retVal.add(createAlternativeModuleElement(doc, i, 5, NOD_GROUND_PUMP, incidentsViolation));
                i++;
            }

        }
        return retVal;
    }

    private List<Element> createNodWeirOpRow(List<NodWeirsOperations> nodWeirs, Document doc)
    {
        String incidents = "";

        NodeList nodNodes = _helperDoc.getElementsByTagName(NOD_WEIR_TAG);
        if (nodNodes.getLength() > 0)
        {
            Node nodeNode = nodNodes.item(0);
            if (nodeNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element upstreamElem = (Element) nodeNode;

                incidents = upstreamElem.getElementsByTagName(INCIDENTS_SPILLING).item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!nodWeirs.isEmpty())
        {
            //create the base column element
            NodWeirsOperations baseNod = nodWeirs.get(0);
            String incidentsViolation = String.format(incidents, baseNod.getIncedents());

            retVal.add(createBaseModuleElement(doc, 4, NOD_WEIR, incidentsViolation));

            //we already did the first one for "base" get rid of it and go over the rest
            nodWeirs.remove(0);
            //create elements for each alternative
            int i = 2;
            for (NodWeirsOperations nod : nodWeirs)
            {
                incidentsViolation = String.format(incidents, nod.getIncedents());

                retVal.add(createAlternativeModuleElement(doc, i, 4, NOD_WEIR, incidentsViolation));
                i++;
            }

        }
        return retVal;
    }

    private List<Element> createUpstreamMIFReqRow(List<UpstreamMIFReq> mifs, Document doc)
    {
        String incidents = "";

        NodeList upstreamNodes = _helperDoc.getElementsByTagName(UPSTREAM_MIF_REQ_TAG);
        if (upstreamNodes.getLength() > 0)
        {
            Node upstreamNode = upstreamNodes.item(0);
            if (upstreamNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element upstreamElem = (Element) upstreamNode;

                incidents = upstreamElem.getElementsByTagName(INCIDENTS_VIOLATION).item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!mifs.isEmpty())
        {
            //create the base column element
            UpstreamMIFReq baseMIF = mifs.get(0);
            String incidentsViolation = String.format(incidents, baseMIF.getIncedents());

            retVal.add(createBaseModuleElement(doc, 3, UPSTREAM_MIF_REQ, incidentsViolation));

            //we already did the first one for "base" get rid of it and go over the rest
            mifs.remove(0);
            //create elements for each alternative
            int i = 2;
            for (UpstreamMIFReq mif : mifs)
            {
                incidentsViolation = String.format(incidents, mif.getIncedents());

                retVal.add(createAlternativeModuleElement(doc, i, 3, UPSTREAM_MIF_REQ, incidentsViolation));
                i++;
            }

        }
        return retVal;
    }

    private List<Element> createReservoirOperationsRow(List<ReservoirOperation> resOps, Document doc)
    {
        String incidentsBelowDP = "";
        String incidentsAboveFlood = "";
        String incidentsSpill = "";

        NodeList resNodes = _helperDoc.getElementsByTagName(RES_OPS_TAG);
        if (resNodes.getLength() > 0)
        {
            Node resNode = resNodes.item(0);
            if (resNode.getNodeType() == Node.ELEMENT_NODE)
            {
                Element mIElem = (Element) resNode;

                incidentsBelowDP = mIElem.getElementsByTagName("incidents-below-dp").item(0).getTextContent();
                incidentsAboveFlood = mIElem.getElementsByTagName("incidents-above-flood").item(0).getTextContent();
                incidentsSpill = mIElem.getElementsByTagName("incidents-spill").item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        if(!resOps.isEmpty())
        {
            //create the base column element
            ReservoirOperation baseResOp = resOps.get(0);
            String aboveFlood = String.format(incidentsAboveFlood, baseResOp.getIncidentsAboveFlood());
            String belowDP = String.format(incidentsBelowDP, baseResOp.getIncidentsBelowDP());
            String spill = String.format(incidentsSpill, baseResOp.getIncidentsSpill());

            String value =  belowDP + NEW_LINE + aboveFlood + NEW_LINE + spill;
            retVal.add(createBaseModuleElement(doc, 2, RES_OPS, value));

            //we already did the first one for "base" get rid of it and go over the rest
            resOps.remove(0);
            //create elements for each alternative
            int i = 2;
            for (ReservoirOperation resOp : resOps)
            {
                aboveFlood = String.format(incidentsAboveFlood, resOp.getIncidentsAboveFlood());
                belowDP = String.format(incidentsBelowDP, resOp.getIncidentsBelowDP());
                spill = String.format(incidentsSpill, resOp.getIncidentsSpill());

                value =  belowDP + NEW_LINE+ aboveFlood +  NEW_LINE + spill;
                retVal.add(createAlternativeModuleElement(doc, i, 2, RES_OPS, value));
                i++;
            }

        }
        return retVal;
    }



    private List<Element> createModelInputsRow(List<ModelInputs> modelInputs, Document doc)
    {
        //load the strings in from the helper xml file
        String tsUpdated = "";
        String filesChanged = "";
        String filesAdded = "";
        String filesDeleted = "";

        NodeList modelInputsNodes = _helperDoc.getElementsByTagName(MODEL_INPUTS_TAG);
        if (modelInputsNodes.getLength() > 0)
        {
            Node modelInput = modelInputsNodes.item(0);
            if (modelInput.getNodeType() == Node.ELEMENT_NODE)
            {
                Element mIElem = (Element) modelInput;

                tsUpdated = mIElem.getElementsByTagName("ts-updated").item(0).getTextContent();
                filesChanged = mIElem.getElementsByTagName("files-changed").item(0).getTextContent();
                filesAdded = mIElem.getElementsByTagName("files-added").item(0).getTextContent();
                filesDeleted = mIElem.getElementsByTagName("files-deleted").item(0).getTextContent();
            }
        }

        List<Element> retVal = new ArrayList<>();

        //create the base column element
        retVal.add(createBaseModuleElement(doc, 1, MI, BASE));


        //create elements for alternative columns
        int i = 2;
        for (ModelInputs modelInput : modelInputs)
        {
            String formattedTSUpdated = String.format(tsUpdated, modelInput.getTimeSeriesUpdatedInSV());
            String formattedFilesChanged = String.format(filesChanged, modelInput.getFileChanges());
            String formattedFilesAdded = String.format(filesAdded, modelInput.getNewFilesAdded());
            String formattedFilesDeleted = String.format(filesDeleted, modelInput.getFilesDeleted());

            String value = formattedTSUpdated + NEW_LINE + formattedFilesChanged + NEW_LINE + formattedFilesAdded +
                    NEW_LINE + formattedFilesDeleted;

            retVal.add(createAlternativeModuleElement(doc, i, 1, MI, value));
            i++;
        }
        return retVal;
    }

    /**
     *
     * @param doc
     * @param altColumnNumber this needs to start with 2 since base is in column 1
     * @param row
     * @param rowName
     * @param value
     * @return
     */
    private Element createAlternativeModuleElement(Document doc,int altColumnNumber, int row, String rowName, String value)
    {
        Element moduleElement = createModuleElement(doc);
        moduleElement.appendChild(createAlternativeStudyElement(doc, altColumnNumber));
        moduleElement.appendChild(createModelEntriesElement(doc,row,rowName,value));
        return moduleElement;
    }
    private Element createBaseModuleElement(Document doc, int row, String rowName, String value)
    {
        Element moduleElement = createModuleElement(doc);
        moduleElement.appendChild(createBaseStudyElement(doc));
        moduleElement.appendChild(createModelEntriesElement(doc,row,rowName,value));
        return moduleElement;
    }

    private Element createModuleElement(Document doc)
    {
        Element moduleElement = doc.createElement(MODULE);
        Attr headerAttr = doc.createAttribute("header");
        headerAttr.setValue("Issues");
        moduleElement.setAttributeNode(headerAttr);
        return moduleElement;
    }

    private Element createModelEntriesElement(Document doc, int row, String rowName, String value)
    {
        Element modelEntry = doc.createElement(MODEL_ENTRIES);

        Attr modelOrderAttr = doc.createAttribute("model-order");
        modelOrderAttr.setValue(Integer.toString(row));
        modelEntry.setAttributeNode(modelOrderAttr);

        Attr nameAttr = doc.createAttribute(NAME);
        nameAttr.setValue(rowName);
        modelEntry.setAttributeNode(nameAttr);

        modelEntry.appendChild(doc.createTextNode(value));

        return modelEntry;
    }

    private Element createBaseStudyElement(Document doc)
    {
        Element baseStudyElem = doc.createElement("study");
        Attr baseStudyAttr = doc.createAttribute("study-order");
        baseStudyAttr.setValue(Integer.toString(1));
        baseStudyElem.setAttributeNode(baseStudyAttr);
        baseStudyElem.appendChild(doc.createTextNode("Base Study"));

        return baseStudyElem;
    }

    private Element createAlternativeStudyElement(Document doc, int altNumber)
    {
        Element altStudyElem = doc.createElement("study");
        Attr altStudyAttr = doc.createAttribute("study-order");
        altStudyAttr.setValue(Integer.toString(altNumber));
        altStudyElem.setAttributeNode(altStudyAttr);
        altStudyElem.appendChild(doc.createTextNode("Alternative Study"));

        return altStudyElem;
    }
    public void loadHelperXMLFile()
    {

        URL helperXMLURL = this.getClass().getClassLoader().getResource("ExecutiveReportHelperXML.xml");
        String path = helperXMLURL.getPath();
        try
        {
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            _helperDoc = dBuilder.parse(path);
            _helperDoc.getDocumentElement().normalize();
        }
        catch (ParserConfigurationException e)
        {
            e.printStackTrace();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        catch (SAXException e)
        {
            e.printStackTrace();
        }
    }
}
