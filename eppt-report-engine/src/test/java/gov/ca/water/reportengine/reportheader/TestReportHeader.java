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

package gov.ca.water.reportengine.reportheader;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestReportHeader extends TestReportHeaderBase
{
//    private static final String XML_PATH = System.getProperty("user.dir") + "\\headerXML2Alts.xml";
    private static final String REPORT_HEADER_TAG = "report-header";

    private Document _qAQCMaster;
    private Document _qAQCReportToTest;


    @Test
    void testHeaderWithBaseOnly() throws Exception
    {
        Document doc = getDoc();

        //create report header
        List<String> altNames = new ArrayList<>();
        ReportHeader rh = new ReportHeader("Bryan Gray", "EPPT Prototype", "CSII_Base", altNames);
        ReportHeaderXMLCreator rhWriter = new ReportHeaderXMLCreator();
        rhWriter.appendReportHeaderElement(rh, doc);
        Path path = writeXmlFile(doc);

        _qAQCReportToTest = loadReportToTest(path);
        _qAQCMaster = loadBaseOnlyReport();

        //get the elements to compare
        Element elemsToTest = getElementsWithName(_qAQCReportToTest, REPORT_HEADER_TAG);
        Element elemsFromMaster = getElementsWithName(_qAQCMaster, REPORT_HEADER_TAG);

        Assertions.assertEquals(getAuthor(elemsFromMaster), getAuthor(elemsToTest), "Author");
        Assertions.assertEquals(getSubtitle(elemsFromMaster), getSubtitle(elemsToTest), "Subtitle");
        Assertions.assertEquals(getBase(elemsFromMaster), getBase(elemsToTest), "base");

        List<String> altsToTest = getAlternatives(elemsToTest);
        List<String> altsFromMaster = getAlternatives(elemsFromMaster);

        assertEquals(altsFromMaster.size(), altsToTest.size());

        for(int i = 0;i<altsToTest.size();i++)
        {
            Assertions.assertEquals(altsToTest.get(i),altsFromMaster.get(i), "alternative");
        }
    }

    @Test
    void testHeaderWithOneAlternatives() throws Exception
    {
        Document doc = getDoc();

        //create report header
        List<String> altNames = new ArrayList<>(Arrays.asList("CSII Alternative"));
        ReportHeader rh = new ReportHeader("Bryan Gray", "EPPT Prototype", "CSII_Base", altNames);
        ReportHeaderXMLCreator rhWriter = new ReportHeaderXMLCreator();
        rhWriter.appendReportHeaderElement(rh, doc);
        Path path = writeXmlFile(doc);

        _qAQCReportToTest = loadReportToTest(path);
        _qAQCMaster = loadComparisonSameModelReport();

        //get the elements to compare
        Element elemsToTest = getElementsWithName(_qAQCReportToTest, REPORT_HEADER_TAG);
        Element elemsFromMaster = getElementsWithName(_qAQCMaster, REPORT_HEADER_TAG);

        Assertions.assertEquals(getAuthor(elemsFromMaster), getAuthor(elemsToTest), "Author");
        Assertions.assertEquals(getSubtitle(elemsFromMaster), getSubtitle(elemsToTest), "Subtitle");
        Assertions.assertEquals(getBase(elemsFromMaster), getBase(elemsToTest), "base");

        List<String> altsToTest = getAlternatives(elemsToTest);
        List<String> altsFromMaster = getAlternatives(elemsFromMaster);

        assertEquals(altsFromMaster.size(), altsToTest.size());

        for(int i = 0;i<altsToTest.size();i++)
        {
            Assertions.assertEquals(altsToTest.get(i),altsFromMaster.get(i), "alternative");
        }
    }
}
