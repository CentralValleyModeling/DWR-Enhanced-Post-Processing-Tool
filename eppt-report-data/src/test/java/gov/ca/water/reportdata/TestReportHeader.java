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

package gov.ca.water.reportdata;

import org.junit.jupiter.api.Test;
import org.apache.log4j.Logger;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;


public class TestReportHeader
{
    private static Logger LOGGER = Logger.getLogger(TestReportHeader.class.getName());


    @Test
    void testWritingAndReadingHeaderXMLFileWith2Alternatives() throws Exception
    {
        String path = System.getProperty("user.dir");
        String filePath = path + "\\headerXML2Alts.xml";


        //create report header
        List<String> altNames = new ArrayList<>(Arrays.asList("CSII Alternative", "CSII Alternative 2"));
        ReportHeader rh = new ReportHeader("Bryan Gray", "CSII_Base", altNames);
        ReportHeaderXMLWriter rhWriter = new ReportHeaderXMLWriter();
        rhWriter.Write(filePath, rh);

        //check the results
        try (BufferedReader br = new BufferedReader(new FileReader(new File(filePath))))
        {
            String line;
            StringBuilder sb = new StringBuilder();

            while ((line = br.readLine()) != null)
            {
                sb.append(line.trim());
            }
            assertEquals(sb.toString(), "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><report-header>" +
                    "<author>Bryan Gray</author><subtitle>EPPT Prototype</subtitle><base-file>CSII_Base</base-file>" +
                    "<alternatives><alternative>CSII Alternative</alternative><alternative>CSII Alternative 2</alternative></alternatives>" +
                    "</report-header>");
        }

    }

    @Test
    void testWritingAndReadingHeaderXMLFileWith1Alternatives() throws Exception
    {
        String path = System.getProperty("user.dir");
        String filePath = path + "\\headerXML1Alt.xml";


        //create report header
        List<String> altNames = new ArrayList<>(Arrays.asList("CSII Alternative"));
        ReportHeader rh = new ReportHeader("Bryan Gray", "CSII_Base", altNames);
        ReportHeaderXMLWriter rhWriter = new ReportHeaderXMLWriter();
        rhWriter.Write(filePath, rh);

        //check the results
        try (BufferedReader br = new BufferedReader(new FileReader(new File(filePath))))
        {
            String line;
            StringBuilder sb = new StringBuilder();

            while ((line = br.readLine()) != null)
            {
                sb.append(line.trim());
            }
            assertEquals(sb.toString(), "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><report-header>" +
                    "<author>Bryan Gray</author><subtitle>EPPT Prototype</subtitle><base-file>CSII_Base</base-file>" +
                    "<alternatives><alternative>CSII Alternative</alternative></alternatives>" +
                    "</report-header>");
        }

    }

    @Test
    void testWritingAndReadingHeaderXMLFileWithNoAlternatives() throws Exception
    {
        String path = System.getProperty("user.dir");
        String filePath = path + "\\headerXMLNoAlts.xml";


        //create report header
        List<String> altNames = new ArrayList<>();
        ReportHeader rh = new ReportHeader("Bryan Gray", "CSII_Base", altNames);
        ReportHeaderXMLWriter rhWriter = new ReportHeaderXMLWriter();
        rhWriter.Write(filePath, rh);

        //check the results
        try (BufferedReader br = new BufferedReader(new FileReader(new File(filePath))))
        {
            String line;
            StringBuilder sb = new StringBuilder();

            while ((line = br.readLine()) != null)
            {
                sb.append(line.trim());
            }
            assertEquals(sb.toString(), "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><report-header>" +
                    "<author>Bryan Gray</author><subtitle>EPPT Prototype</subtitle><base-file>CSII_Base</base-file>" +
                    "</report-header>");
        }


    }

}
