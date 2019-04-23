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

import gov.ca.water.reportdata.TestReportHeader;
import org.apache.log4j.Logger;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TestUpstreamMIFReqs
{

    private static Logger LOGGER = Logger.getLogger(TestReportHeader.class.getName());
    private Document _qAQCMaster;
    private Document _qAQCReportToTest;


    private ExecutiveReportHeader createERHeader()
    {
        int baseViolation = 5;
        int altViolation = 6;
        String pctChangeViolations = "7% increase";
        double avgAnnualExportPrev = 8.0;
        double avgAnnualExportCur = 9.0;
        String annualExportIncDec = "increase";
        double avgCvpNodStoragePrev = 11;
        double avgCvpNodStorageCur = 12;
        String cvpNodStorIncDec = "increase";
        double avgSWPNodStorPrev = 14;
        double avgSWPNodStorCur = 15;
        String swpNodStorIncDec = "increase";

        return new ExecutiveReportHeader(baseViolation, altViolation, pctChangeViolations, avgAnnualExportPrev, avgAnnualExportCur, annualExportIncDec,
                avgCvpNodStoragePrev, avgCvpNodStorageCur, cvpNodStorIncDec, avgSWPNodStorPrev, avgSWPNodStorCur, swpNodStorIncDec);

    }

    private void loadQAQCReportMaster() throws Exception
    {
        URL resource = this.getClass().getClassLoader().getResource("QAQCReport.xml");
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        _qAQCMaster = dBuilder.parse(resource.getPath());
        _qAQCMaster.getDocumentElement().normalize();
    }

    private void loadQAQCReportToTest(String path) throws Exception
    {
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        _qAQCReportToTest = dBuilder.parse(path);
        _qAQCReportToTest.getDocumentElement().normalize();
    }





    @Test
    void testWritingAndReadingReservoirOperationsRowElementWithNoAlternative() throws Exception
    {
        String path = System.getProperty("user.dir");
        String filePath = path + "\\executivereport.xml";

        //input values
        int incidents = 3;

        //create ER
        List<ModelInputs> mis = new ArrayList<>();
        List<ReservoirOperation> resOps = new ArrayList<>();

        List<UpstreamMIFReq> mifs = new ArrayList<>();
        UpstreamMIFReq baseMif = new UpstreamMIFReq(incidents);
        mifs.add(baseMif);

        ExecutiveReport er = new ExecutiveReport(createERHeader(), mis, resOps, mifs, new ArrayList<>(),
                new ArrayList<>(),new ArrayList<>(),new ArrayList<>(),new ArrayList<>(),new ArrayList<>());

        //write out the file
        ExecutiveReportXMLWriter erWriter = new ExecutiveReportXMLWriter();
        erWriter.write(filePath, er);


        //read the xml file to test
        loadQAQCReportToTest(filePath);
        loadQAQCReportMaster();

        List<Element> elemsToTest = ExecutiveReportTestUtilities.getModuleElementsWithName(_qAQCReportToTest, ExecutiveReportXMLWriter.UPSTREAM_MIF_REQ);
        List<Element> elemsFromMaster = ExecutiveReportTestUtilities.getModuleElementsWithName(_qAQCMaster, ExecutiveReportXMLWriter.UPSTREAM_MIF_REQ);

        //check that the number of modules (# of columns) is equal to the base + number of alternatives
        assertTrue(elemsToTest.size() == 1);

        for(int i = 0;i<elemsToTest.size();i++)
        {
            assertEquals(ExecutiveReportTestUtilities.getStudyOrderForModule(elemsFromMaster.get(i)), ExecutiveReportTestUtilities.getStudyOrderForModule(elemsToTest.get(i)));
            assertEquals(ExecutiveReportTestUtilities.getModelEntriesOrderForModule(elemsFromMaster.get(i)), ExecutiveReportTestUtilities.getModelEntriesOrderForModule(elemsToTest.get(i)));

            assertEquals(ExecutiveReportTestUtilities.getStudyValue(elemsFromMaster.get(i)), ExecutiveReportTestUtilities.getStudyValue(elemsToTest.get(i)));
            assertEquals(ExecutiveReportTestUtilities.getModelEntriesValue(elemsFromMaster.get(i)),ExecutiveReportTestUtilities.getModelEntriesValue(elemsToTest.get(i)));
        }

    }

    @Test
    void testWritingAndReadingReservoirOperationsRowElementWith1Alternative() throws Exception
    {
        String path = System.getProperty("user.dir");
        String filePath = path + "\\executivereport.xml";

        //input values
        int incidents = 3;
        int incidents2 = 4;

        //create ER
        List<ModelInputs> mis = new ArrayList<>();

        List<ReservoirOperation> resOps = new ArrayList<>();

        List<UpstreamMIFReq> mifs = new ArrayList<>();
        UpstreamMIFReq baseMif = new UpstreamMIFReq(incidents);
        mifs.add(baseMif);
        mifs.add(new UpstreamMIFReq(incidents2));

        ExecutiveReport er = new ExecutiveReport(createERHeader(), mis, resOps, mifs, new ArrayList<>(),
                new ArrayList<>(),new ArrayList<>(),new ArrayList<>(),new ArrayList<>(),new ArrayList<>());

        //write out the file
        ExecutiveReportXMLWriter erWriter = new ExecutiveReportXMLWriter();
        erWriter.write(filePath, er);


        //read the xml file to test
        loadQAQCReportToTest(filePath);
        loadQAQCReportMaster();

        List<Element> modelInputElems = ExecutiveReportTestUtilities.getModuleElementsWithName(_qAQCReportToTest, ExecutiveReportXMLWriter.UPSTREAM_MIF_REQ);
        List<Element> masterModelInputElems = ExecutiveReportTestUtilities.getModuleElementsWithName(_qAQCMaster, ExecutiveReportXMLWriter.UPSTREAM_MIF_REQ);

        //check that the number of modules (# of columns) is equal to the base + number of alternatives
        assertTrue(modelInputElems.size() == 2);
        assertTrue(modelInputElems.size() == masterModelInputElems.size());


        for(int i = 0;i<modelInputElems.size();i++)
        {
            assertEquals(ExecutiveReportTestUtilities.getStudyOrderForModule(masterModelInputElems.get(i)), ExecutiveReportTestUtilities.getStudyOrderForModule(modelInputElems.get(i)));
            assertEquals(ExecutiveReportTestUtilities.getModelEntriesOrderForModule(masterModelInputElems.get(i)), ExecutiveReportTestUtilities.getModelEntriesOrderForModule(modelInputElems.get(i)));

            assertEquals(ExecutiveReportTestUtilities.getStudyValue(masterModelInputElems.get(i)), ExecutiveReportTestUtilities.getStudyValue(modelInputElems.get(i)));
            assertEquals(ExecutiveReportTestUtilities.getModelEntriesValue(masterModelInputElems.get(i)),ExecutiveReportTestUtilities.getModelEntriesValue(modelInputElems.get(i)));
        }
    }

}
