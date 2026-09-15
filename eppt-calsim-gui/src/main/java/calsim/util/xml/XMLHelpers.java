package calsim.util.xml;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class XMLHelpers {

    private static final Logger LOGGER = Logger.getLogger(XMLHelpers.class.getName());

    private XMLHelpers() {
        // static utility class
    }

    public static void writeDocumentToXMLFile(Document document, String file) throws IOException, TransformerException {
        try(PrintWriter printWriter = new PrintWriter(new FileOutputStream(file))) {
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            DOMSource domSource = new DOMSource(document);

            StreamResult result = new StreamResult(printWriter);

            transformer.transform(domSource, result);
        }
    }

    public static Document readXmlDocumentFromFile(String filename) throws IOException, SAXException, ParserConfigurationException {
        return DocumentBuilderFactory.newDefaultInstance().newDocumentBuilder().parse(filename);
    }

    public static Document createXmlDocument() {
        try {
            return DocumentBuilderFactory.newDefaultInstance().newDocumentBuilder().newDocument();
        } catch (ParserConfigurationException e) {
            LOGGER.log(Level.SEVERE, "Error creating XML document", e);
            throw new IllegalStateException("Error creating XML document", e);
        }
    }

}
