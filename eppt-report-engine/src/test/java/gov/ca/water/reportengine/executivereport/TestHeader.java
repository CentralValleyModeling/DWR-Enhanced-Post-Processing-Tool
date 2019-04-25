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

package gov.ca.water.reportengine.executivereport;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestHeader
{
//    @Test
//    void testWritingAndReadingExecutiveReportHeaderElement() throws Exception
//    {
//
//        String path = System.getProperty("user.dir");
//        String filePath = path + "\\executivereport.xml";
//
//        String helperPath = path + "\\ExecutiveReportHelperXML.xml";
//
//        int baseViolation = 5;
//        int altViolation = 6;
//        String pctChangeViolations = "7% increase";
//        double avgAnnualExportPrev = 8.0;
//        double avgAnnualExportCur = 9.0;
//        String annualExportIncDec = "increase";
//        double avgCvpNodStoragePrev = 11;
//        double avgCvpNodStorageCur = 12;
//        String cvpNodStorIncDec = "increase";
//        double avgSWPNodStorPrev = 14;
//        double avgSWPNodStorCur = 15;
//        String swpNodStorIncDec = "increase";
//
//        ExecutiveReportHeader erHeader = new ExecutiveReportHeader(baseViolation, altViolation, pctChangeViolations, avgAnnualExportPrev, avgAnnualExportCur, annualExportIncDec,
//                avgCvpNodStoragePrev, avgCvpNodStorageCur, cvpNodStorIncDec, avgSWPNodStorPrev, avgSWPNodStorCur, swpNodStorIncDec);
//
//        ModelInputs mi = new ModelInputs(1, 2, 3, 4);
//        List<ModelInputs> mis = new ArrayList<>();
//        mis.add(mi);
//        executivereport er = new executivereport(erHeader, mis, new ArrayList<>(),new ArrayList<>(),new ArrayList<>(),
//                new ArrayList<>(),new ArrayList<>(),new ArrayList<>(),new ArrayList<>(),new ArrayList<>());
//        ExecutiveReportXMLWriter erWriter = new ExecutiveReportXMLWriter();
//        erWriter.createXMLElement(filePath, er);
//
//
//        //check the results
//
//        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
//        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
//        Document doc = dBuilder.parse(filePath);
//
//        doc.getDocumentElement().normalize();
//
//        NodeList elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.BASE_VIOLATIONS);
//        Element baseViolationsElem = (Element) elementsByTagName.item(0);
//        String textContent = baseViolationsElem.getTextContent();
//        assertEquals(baseViolation, Integer.valueOf(textContent));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.ALTERNATIVE_VIOLATIONS);
//        assertEquals(altViolation, Integer.valueOf(((Element) elementsByTagName.item(0)).getTextContent()));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.PCT_CHANGE_VIOLATIONS);
//        assertEquals(pctChangeViolations, ((Element) elementsByTagName.item(0)).getTextContent());
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.AVG_ANNUAL_EXPORT_PREV);
//        assertEquals(avgAnnualExportPrev, Double.valueOf(((Element) elementsByTagName.item(0)).getTextContent()));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.AVG_ANNUAL_EXPORT_CUR);
//        assertEquals(avgAnnualExportCur, Double.valueOf(((Element) elementsByTagName.item(0)).getTextContent()));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.ANNUAL_EXPORT_INC_DEC);
//        assertEquals(annualExportIncDec, ((Element) elementsByTagName.item(0)).getTextContent());
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.AVG_CVP_NOD_STORAGE_PREV);
//        assertEquals(avgCvpNodStoragePrev, Double.valueOf(((Element) elementsByTagName.item(0)).getTextContent()));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.AVG_CVP_NOD_STORAGE_CUR);
//        assertEquals(avgCvpNodStorageCur, Double.valueOf(((Element) elementsByTagName.item(0)).getTextContent()));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.CVP_NOD_STORAGE_INC_DEC);
//        assertEquals(cvpNodStorIncDec, ((Element) elementsByTagName.item(0)).getTextContent());
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.AVG_SWP_NOD_STOR_PREV);
//        assertEquals(avgSWPNodStorPrev, Double.valueOf(((Element) elementsByTagName.item(0)).getTextContent()));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.AVG_SWP_NOD_STOR_CUR);
//        assertEquals(avgSWPNodStorCur, Double.valueOf(((Element) elementsByTagName.item(0)).getTextContent()));
//
//        elementsByTagName = doc.getElementsByTagName(ExecutiveReportXMLWriter.SWP_NOD_STORAGE_INC_DEC);
//        assertEquals(swpNodStorIncDec, ((Element) elementsByTagName.item(0)).getTextContent());
//    }
}
