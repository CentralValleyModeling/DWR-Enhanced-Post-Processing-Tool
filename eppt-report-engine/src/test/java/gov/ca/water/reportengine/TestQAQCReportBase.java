/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.reportengine;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;

import javafx.scene.paint.Color;
import org.junit.jupiter.api.BeforeAll;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import hec.heclib.dss.HecDSSFileAccess;

public class TestQAQCReportBase
{
    static
    {
        HecDSSFileAccess.setMessageLevel(HecDSSFileAccess.MESS_LEVEL_CRITICAL);
    }

    @BeforeAll
    public static void setup() throws EpptInitializationException
    {
        Path target = Paths.get(System.getProperty("user.dir")).resolve("target").resolve("test-classes");
        System.setProperty("user.dir", target.toString());
        GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
        ThresholdLinksSeedDataSvc.createSeedDataSvcImplInstance();
    }


    public EpptScenarioRun getBaseScenarioRun()
    {

        Path ivPath = getInitialConditionsBaseDSSPath();
        NamedDssPath ivDssFile = new NamedDssPath(ivPath, "INIT", "CALSIM", "1MON", "");

        Path svPath = getStateVariableBaseDSSPath();
        NamedDssPath svDssFile = new NamedDssPath(svPath, "SV", "CALSIM", "1MON", "");

        Path dvPath = getDVPath();
        NamedDssPath dvDssFile = new NamedDssPath(dvPath, "DV", "CALSIM", "1MON", "");

        List<NamedDssPath> extraDssFiles = Collections.emptyList();
        EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, ivDssFile, extraDssFiles);


        GUILinksAllModelsBO.Model calSim2 = GUILinksAllModelsBO.Model.findModel("CalSim2");// model = new GUILinksAllModelsBO.Model("CalSim2");
        EpptScenarioRun baseRun = new EpptScenarioRun("baseScenario", "desc", calSim2,null,null, null, dssContainer, Color.PINK);
        return baseRun;
    }

    public List<EpptScenarioRun> getAltScenarioRuns()
    {
        Path ivPath = getInitialConditionsAltDSSPath();
        NamedDssPath ivDssFile = new NamedDssPath(ivPath, "INIT", "CALSIM", "1MON", "");

        Path svPath = getStateVariableAltDSSPath();
        NamedDssPath svDssFile = new NamedDssPath(svPath, "SV", "CALSIM", "1MON", "");

        Path dvPath = getDVPath();
        NamedDssPath dvDssFile = new NamedDssPath(dvPath, "DV", "CALSIM", "1MON", "");

        List<NamedDssPath> extraDssFiles = Collections.emptyList();
        EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, ivDssFile, extraDssFiles);

        GUILinksAllModelsBO.Model calSim2 = GUILinksAllModelsBO.Model.findModel("CalSim2");// model = new GUILinksAllModelsBO.Model("CalSim2");
        EpptScenarioRun alt1Run = new EpptScenarioRun("alt1Scenario", "desc", calSim2,null,null,null, dssContainer, Color.PINK);

        List<EpptScenarioRun> altRuns = new ArrayList<>();
        altRuns.add(alt1Run);
        return altRuns;
    }

    protected Path getCodeChangesCsvPath()
    {
        URL codeChangesPath = this.getClass().getClassLoader().getResource("CodeChangesDSSPaths.csv");
        return new File(codeChangesPath.getPath()).toPath();
    }

    protected Path getBaseOutputPath()
    {
        URL baseOutputPath = this.getClass().getClassLoader().getResource("BaseOutputDirectory");
        return new File(baseOutputPath.getPath()).toPath();

    }

    protected Path getAltOutputPath()
    {
        URL altOutputPath = this.getClass().getClassLoader().getResource("AltOutputDirectory");
        return new File(altOutputPath.getPath()).toPath();
    }

    public List<Path> getDssFilePathsForBaseOnly()
    {
        URL resource = this.getClass().getClassLoader().getResource("SamplePstPrcss_Base_v1.01.dss");
        Path dssFilePath = new File(resource.getPath()).toPath();

        List<Path> dssFiles = new ArrayList<>();
        dssFiles.add(dssFilePath);

        return dssFiles;
    }

    public List<Path> getDssFilePathsForSameModel()
    {
        URL resource = this.getClass().getClassLoader().getResource("SamplePstPrcss_Base_v1.01.dss");
        Path dssFilePath = new File(resource.getPath()).toPath();

        URL resource2 = this.getClass().getClassLoader().getResource("SamplePstPrcss_Alt1_v1.01.dss");
        Path dssFilePath2 = new File(resource2.getPath()).toPath();

        List<Path> dssFiles = new ArrayList<>();
        dssFiles.add(dssFilePath);
        dssFiles.add(dssFilePath2);

        return dssFiles;
    }

    protected Path getDVPath()
    {
        URL sampleDVBase = this.getClass().getClassLoader().getResource("SampleDV_Base.dss");
        return new File(sampleDVBase.getPath()).toPath();
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

    public Path writeXmlFile(Document doc) throws ParserConfigurationException, TransformerException, IOException
    {
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        DOMSource domSource = new DOMSource(doc);
        Path report = Files.createTempFile("reportXML", ".xml" );

        report.toFile().deleteOnExit();
        StreamResult streamResult = new StreamResult(report.toString());
        transformer.transform(domSource, streamResult);
        return report;
    }

    public Document getDoc() throws ParserConfigurationException
    {
        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
        return documentBuilder.newDocument();
    }

    protected Path getCSVPath()
    {
        URL resource = this.getClass().getClassLoader().getResource("ExecutiveReportModulesCSV.csv");
        return new File(resource.getPath()).toPath();
    }

    protected Path getModuleLinkingCSVPath()
    {
        URL resource = this.getClass().getClassLoader().getResource("Details.csv");
        return new File(resource.getPath()).toPath();
    }

    public Document loadReportToTest(Path path) throws Exception
    {
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(path.toString());
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
