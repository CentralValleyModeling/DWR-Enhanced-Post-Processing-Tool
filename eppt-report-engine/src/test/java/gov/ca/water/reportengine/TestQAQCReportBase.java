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

package gov.ca.water.reportengine;

import gov.ca.water.reportengine.executivereport.ExecutiveReportXMLCreator;
import hec.data.meta.RatingCatalogQuery;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import hec.heclib.dss.HecDSSFileAccess;

public class TestQAQCReportBase
{
    static
    {
        HecDSSFileAccess.setMessageLevel(HecDSSFileAccess.MESS_LEVEL_CRITICAL);
    }

    protected Path getInitialConditionsCSV()
    {
        URL initCondResource = this.getClass().getClassLoader().getResource("AssumptionChangesInitialConditionsDSSPaths.csv");
        return new File(initCondResource.getPath()).toPath();
    }
    protected Path getInitialConditionsBaseDSSPath()
    {
        URL resource = this.getClass().getClassLoader().getResource("SampleInit_Base.dss");
        return new File(resource.getPath()).toPath();
    }

    protected Path getInitialConditionsAltDSSPath()
    {
        URL alt = this.getClass().getClassLoader().getResource("SampleInit_Alt.dss");
        return new File(alt.getPath()).toPath();
    }

    protected Path getAssumpChangesStateVariablesCSV()
    {
        URL stateVarResource = this.getClass().getClassLoader().getResource("AssumptionChangesStateVariablesDSSPaths.csv");
        return new File(stateVarResource.getPath()).toPath();
    }

    protected Path getStateVariableBaseDSSPath()
    {
        URL svBaseDss = this.getClass().getClassLoader().getResource("SampleSV_Base.dss");
        return new File(svBaseDss.getPath()).toPath();
    }

    protected Path getStateVariableAltDSSPath()
    {
        URL altSV = this.getClass().getClassLoader().getResource("SampleSV_Alt.dss");
        return new File(altSV.getPath()).toPath();
    }

    public void writeXmlFile(String xmlPath, Document doc) throws ParserConfigurationException, TransformerException
    {
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        DOMSource domSource = new DOMSource(doc);
        StreamResult streamResult = new StreamResult(new File(xmlPath));
        transformer.transform(domSource, streamResult);
    }

    public Document getDoc() throws ParserConfigurationException
    {
        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
        return documentBuilder.newDocument();
    }

    public Path getCSVPath()
    {
        URL resource = this.getClass().getClassLoader().getResource("Category_V1.01.csv");
        return new File(resource.getPath()).toPath();
    }

    public Document loadReportToTest(String path) throws Exception
    {
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(path);
        doc.getDocumentElement().normalize();
        return doc;
    }

    public Document loadBaseOnlyReport() throws ParserConfigurationException, IOException, SAXException
    {
        URL resource = this.getClass().getClassLoader().getResource("QAQC_Report_BaseOnly.xml");
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(resource.getPath());
        doc.getDocumentElement().normalize();
        return doc;
    }

    public Document loadComparisonSameModelReport() throws ParserConfigurationException, IOException, SAXException
    {
        URL resource = this.getClass().getClassLoader().getResource("QAQC_Report_Comparison-SameModel.xml");
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(resource.getPath());
        doc.getDocumentElement().normalize();
        return doc;
    }

    public Element getElementsWithName(Document doc, String elemName)
    {
        List<Element> moduleElements = new ArrayList<>();
        NodeList elementsByTagName = doc.getElementsByTagName(elemName);
        if(elementsByTagName.getLength()>0)
        {
            Node nNode = elementsByTagName.item(0);
            return (Element)nNode;
        }
        else
        {
            return null;
        }

    }

    public Element getChildElementWithName(Element elem, String name)
    {
        NodeList elementsByTagName = elem.getElementsByTagName(name);
        return (Element) elementsByTagName.item(0);
    }

}
