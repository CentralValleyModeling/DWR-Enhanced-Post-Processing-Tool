/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2026.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package calsim.app;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Paths;

import calsim.util.xml.XMLHelpers;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import vista.set.TimeSeriesMath;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * Tests for {@link DerivedTimeSeries}, focused on the data-reference bookkeeping
 * (operator/DTS-name/var-type/b-part/c-part rows) and the two independent
 * serialization round trips the class supports: the CSV-like {@code save}/{@code load}
 * text format, and the {@code toXml}/{@code fromXml} XML format.
 */
public class DerivedTimeSeriesTest
{
	@Test
	public void newDtsHasNoDataReferences()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("EMPTY.DTS");
		assertEquals(0, dts.getNumberOfDataReferences());
	}

	@Test
	public void copyConstructorCopiesNameAndRows()
	{
		DerivedTimeSeries original = buildSampleDts("ORIGINAL.DTS");

		DerivedTimeSeries copy = new DerivedTimeSeries(original);

		assertEquals(original.getName(), copy.getName());
		assertEquals(original.getNumberOfDataReferences(), copy.getNumberOfDataReferences());
		for(int i = 0; i < original.getNumberOfDataReferences(); i++)
		{
			assertEquals("row " + i + " operator", original.getOperationIdAt(i), copy.getOperationIdAt(i));
			assertEquals("row " + i + " dts name", original.getDTSNameAt(i), copy.getDTSNameAt(i));
			assertEquals("row " + i + " var type", original.getVarTypeAt(i), copy.getVarTypeAt(i));
			assertEquals("row " + i + " b part", original.getBPartAt(i), copy.getBPartAt(i));
			assertEquals("row " + i + " c part", original.getCPartAt(i), copy.getCPartAt(i));
		}
	}

	@Test
	public void copyConstructorRowVectorsAreIndependentOfTheOriginal()
	{
		DerivedTimeSeries original = new DerivedTimeSeries("ORIGINAL.DTS");
		original.setBPartAt(0, "B1");

		DerivedTimeSeries copy = new DerivedTimeSeries(original);
		copy.setBPartAt(0, "CHANGED");

		assertEquals("B1", original.getBPartAt(0));
		assertEquals("CHANGED", copy.getBPartAt(0));
	}

	@Test
	public void renamingCopyConstructorCopiesRowsAndAppliesTheNewName()
	{
		DerivedTimeSeries original = buildSampleDts("ORIGINAL.DTS");

		DerivedTimeSeries copy = new DerivedTimeSeries(original, "RENAMED.DTS");

		assertEquals("RENAMED.DTS", copy.getName());
		assertEquals(original.getNumberOfDataReferences(), copy.getNumberOfDataReferences());
		for(int i = 0; i < original.getNumberOfDataReferences(); i++)
		{
			assertEquals("row " + i + " operator", original.getOperationIdAt(i), copy.getOperationIdAt(i));
			assertEquals("row " + i + " dts name", original.getDTSNameAt(i), copy.getDTSNameAt(i));
			assertEquals("row " + i + " var type", original.getVarTypeAt(i), copy.getVarTypeAt(i));
			assertEquals("row " + i + " b part", original.getBPartAt(i), copy.getBPartAt(i));
			assertEquals("row " + i + " c part", original.getCPartAt(i), copy.getCPartAt(i));
		}
	}

	@Test
	public void renamingCopyConstructorRowVectorsAreIndependentOfTheOriginal()
	{
		DerivedTimeSeries original = new DerivedTimeSeries("ORIGINAL.DTS");
		original.setBPartAt(0, "B1");

		DerivedTimeSeries copy = new DerivedTimeSeries(original, "RENAMED.DTS");
		copy.setBPartAt(0, "CHANGED");

		assertEquals("B1", original.getBPartAt(0));
		assertEquals("CHANGED", copy.getBPartAt(0));
		assertEquals("ORIGINAL.DTS", original.getName());
	}

	@Test
	public void settersExpandRowsAndNormalizeValues()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("NAME.DTS");
		dts.setBPartAt(2, " flow ");
		assertEquals(3, dts.getNumberOfDataReferences());
		// intervening rows get default values
		assertEquals(DerivedTimeSeries.DEFAULT_B_PART, dts.getBPartAt(0));
		assertEquals(AppUtils.DVAR, dts.getVarTypeAt(0));
		// value is upper-cased and trimmed
		assertEquals("FLOW", dts.getBPartAt(2));
	}

	@Test
	public void setCPartAtTreatsNullAsEmptyString()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("NAME.DTS");
		dts.setCPartAt(0, null);
		assertEquals("", dts.getCPartAt(0));
	}

	@Test
	public void setDTSNameAtAppendsDtsExtensionAndNormalizesCase()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("NAME.DTS");
		dts.setDTSNameAt(0, " other ");
		assertEquals("OTHER.DTS", dts.getDTSNameAt(0));

		dts.setDTSNameAt(1, "already.dts");
		assertEquals("ALREADY.DTS", dts.getDTSNameAt(1));
	}

	@Test
	public void setDTSNameAtIgnoresSelfReference()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("SELF.DTS");
		dts.setDTSNameAt(0, "self");
		// row was expanded, but the self-referencing name was never stored
		assertEquals("", dts.getDTSNameAt(0));
	}

	@Test
	public void operationIdRoundTripsThroughAppUtilsNames()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("OPS.DTS");
		dts.setOperationIdAt(0, TimeSeriesMath.ADD);
		dts.setOperationIdAt(1, TimeSeriesMath.SUB);
		dts.setOperationIdAt(2, TimeSeriesMath.MUL);
		dts.setOperationIdAt(3, TimeSeriesMath.DIV);

		assertEquals("+", AppUtils.getOperationName(dts.getOperationIdAt(0)));
		assertEquals("-", AppUtils.getOperationName(dts.getOperationIdAt(1)));
		assertEquals("*", AppUtils.getOperationName(dts.getOperationIdAt(2)));
		assertEquals("/", AppUtils.getOperationName(dts.getOperationIdAt(3)));
	}

	@Test
	public void removeShiftsSubsequentRows()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("REMOVE.DTS");
		dts.setBPartAt(0, "B0");
		dts.setBPartAt(1, "B1");
		dts.remove(0);
		assertEquals(1, dts.getNumberOfDataReferences());
		assertEquals("B1", dts.getBPartAt(0));
	}

	@Test
	public void insertAtAddsEmptyRowAndShiftsExistingRows()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("INSERT.DTS");
		dts.setBPartAt(0, "B0");
		dts.setBPartAt(1, "B1");
		dts.insertAt(1);
		assertEquals(3, dts.getNumberOfDataReferences());
		assertEquals("B0", dts.getBPartAt(0));
		assertEquals("", dts.getBPartAt(1));
		assertEquals("B1", dts.getBPartAt(2));
	}

	@Test
	public void insertAtBeyondSizeIsNoOp()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("INSERT2.DTS");
		dts.setBPartAt(0, "B0");
		dts.insertAt(5);
		assertEquals(1, dts.getNumberOfDataReferences());
	}

	@Test
	public void equalsIsBasedOnNameOnly()
	{
		DerivedTimeSeries a = new DerivedTimeSeries("SAME.DTS");
		DerivedTimeSeries b = new DerivedTimeSeries("SAME.DTS");
		DerivedTimeSeries c = new DerivedTimeSeries("DIFFERENT.DTS");
		assertEquals(a, b);
		assertNotEquals(a, c);
        assertNotEquals("SAME.DTS", a);
	}

	@Test
	public void toStringIncludesName()
	{
		DerivedTimeSeries dts = new DerivedTimeSeries("TOSTRING.DTS");
		assertTrue(dts.toString().contains("TOSTRING.DTS"));
	}

	// --- toXml / fromXml -------------------------------------------------

	@Test
	public void toXmlThenFromXmlRoundTripsBPartCPartRowsAndDtsReferenceRows()
	{
		DerivedTimeSeries original = buildSampleDts("XML_ROUNDTRIP.DTS");

		Document doc = XMLHelpers.createXmlDocument();
		Element master = doc.createElement("dts_master");
		doc.appendChild(master);
		original.toXml(doc, master);

		Element dtsElement = (Element) master.getElementsByTagName("DTS").item(0);
		assertEquals("XML_ROUNDTRIP.DTS", dtsElement.getAttribute("name"));

		NodeList rows = dtsElement.getElementsByTagName("row");
		assertEquals(3, rows.getLength());

		DerivedTimeSeries reloaded = new DerivedTimeSeries();
		reloaded.fromXml(dtsElement);

		assertEquals(original.getName(), reloaded.getName());
		assertEquals(original.getNumberOfDataReferences(), reloaded.getNumberOfDataReferences());
		for(int i = 0; i < original.getNumberOfDataReferences(); i++)
		{
			assertEquals("row " + i + " operator", original.getOperationIdAt(i), reloaded.getOperationIdAt(i));
			assertEquals("row " + i + " dts name", original.getDTSNameAt(i), reloaded.getDTSNameAt(i));
			assertEquals("row " + i + " var type", original.getVarTypeAt(i), reloaded.getVarTypeAt(i));
			assertEquals("row " + i + " b part", original.getBPartAt(i), reloaded.getBPartAt(i));
			assertEquals("row " + i + " c part", original.getCPartAt(i), reloaded.getCPartAt(i));
		}
	}

	@Test
	public void fromXmlAppendsDtsExtensionOnlyWhenMissing()
	{
		Document doc = XMLHelpers.createXmlDocument();
		Element dtsElement = doc.createElement("DTS");
		dtsElement.setAttribute("name", "plainname");
		doc.appendChild(dtsElement);

		DerivedTimeSeries dts = new DerivedTimeSeries();
		dts.fromXml(dtsElement);

		assertEquals("PLAINNAME.DTS", dts.getName());
	}

	@Test
	public void toXmlThenFromXmlStoresUnresolvedOperatorAsNegativeOneInsteadOfDroppingRow()
	{
		// A row whose operator was never explicitly set keeps its default int id (0),
		// which AppUtils.getOperationName() cannot map back to a symbol ("?"). Unlike the
		// text save/load format (see below), fromXml does not drop such a row - it stores
		// the unresolved operator id as -1 and keeps the row.
		DerivedTimeSeries dts = new DerivedTimeSeries("UNRESOLVED_OP.DTS");
		dts.setBPartAt(0, "B1");
		dts.setCPartAt(0, "C1");

		Document doc = XMLHelpers.createXmlDocument();
		Element master = doc.createElement("dts_master");
		doc.appendChild(master);
		dts.toXml(doc, master);
		Element dtsElement = (Element) master.getElementsByTagName("DTS").item(0);
		assertEquals("?", dtsElement.getElementsByTagName("row").item(0).getAttributes()
								   .getNamedItem("operator").getNodeValue());

		DerivedTimeSeries reloaded = new DerivedTimeSeries();
		reloaded.fromXml(dtsElement);

		assertEquals(1, reloaded.getNumberOfDataReferences());
		assertEquals(-1, reloaded.getOperationIdAt(0));
	}

	// --- save / load (text format) ----------------------------------------

	@Test
	public void saveThenLoadRoundTripsRowsWithValidOperators() throws IOException
	{
		DerivedTimeSeries original = buildSampleDts("SAVE_LOAD.DTS");

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		original.save(out);

		DerivedTimeSeries reloaded = DerivedTimeSeries.load(new ByteArrayInputStream(out.toByteArray()));

		assertEquals(original.getNumberOfDataReferences(), reloaded.getNumberOfDataReferences());
		for(int i = 0; i < original.getNumberOfDataReferences(); i++)
		{
			assertEquals("row " + i + " operator", original.getOperationIdAt(i), reloaded.getOperationIdAt(i));
			assertEquals("row " + i + " dts name", original.getDTSNameAt(i), reloaded.getDTSNameAt(i));
			if(original.getDTSNameAt(i).isEmpty())
			{
				assertEquals("row " + i + " var type", original.getVarTypeAt(i), reloaded.getVarTypeAt(i));
				assertEquals("row " + i + " b part", original.getBPartAt(i), reloaded.getBPartAt(i));
				assertEquals("row " + i + " c part", original.getCPartAt(i), reloaded.getCPartAt(i));
			}
			else
			{
				// Unlike toXml/fromXml, the text format serializes a DTS-reference row through
				// DTSTableModel.getValueAt, which blanks the var-type/b-part/c-part columns for
				// display. load() reads those blanks back literally, so a row that references
				// another DTS loses its (otherwise-unused) var-type/b-part/c-part values.
				assertEquals("", reloaded.getVarTypeAt(i));
				assertEquals("", reloaded.getBPartAt(i));
				assertEquals("", reloaded.getCPartAt(i));
			}
		}
	}

	@Test
	public void saveThenLoadDropsRowWithUnresolvedOperator() throws IOException
	{
		// Unlike toXml/fromXml, the text format cannot represent an unresolved operator:
		// AppUtils.getOperationId("?") returns -1, and load() silently skips any row whose
		// operator token does not parse to +,-,*,/. A never-set operator (default int id 0)
		// is therefore dropped entirely on a save/load round trip.
		DerivedTimeSeries dts = new DerivedTimeSeries("DROPPED_ROW.DTS");
		dts.setBPartAt(0, "B1");
		dts.setCPartAt(0, "C1");
		dts.setOperationIdAt(1, TimeSeriesMath.ADD);
		dts.setBPartAt(1, "B2");
		dts.setCPartAt(1, "C2");

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		dts.save(out);

		DerivedTimeSeries reloaded = DerivedTimeSeries.load(new ByteArrayInputStream(out.toByteArray()));

		assertEquals(1, reloaded.getNumberOfDataReferences());
		assertEquals(TimeSeriesMath.ADD, reloaded.getOperationIdAt(0));
		assertEquals("B2", reloaded.getBPartAt(0));
		assertEquals("C2", reloaded.getCPartAt(0));
	}

	@Test
	public void saveThenLoadDoubleAppendsDtsExtensionWhenNameAlreadyHasOne()
	{
		// load() unconditionally appends ".DTS" to the name it parses from the "name, ..."
		// line, even when that name already ends with ".DTS" (DerivedTimeSeries.java ~184).
		// This pins down that existing (surprising) behavior rather than asserting it is
		// desirable: a DTS saved with an already-qualified name does not round trip its name.
		DerivedTimeSeries dts = new DerivedTimeSeries("ALREADY_QUALIFIED.DTS");
		dts.setOperationIdAt(0, TimeSeriesMath.ADD);
		dts.setBPartAt(0, "B1");
		dts.setCPartAt(0, "C1");

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		try
		{
			dts.save(out);
		}
		catch(IOException e)
		{
			fail("Unexpected IOException: " + e.getMessage());
		}

		DerivedTimeSeries reloaded;
		try
		{
			reloaded = DerivedTimeSeries.load(new ByteArrayInputStream(out.toByteArray()));
		}
		catch(IOException e)
		{
			fail("Unexpected IOException: " + e.getMessage());
			return;
		}

		assertEquals("ALREADY_QUALIFIED.DTS.DTS", reloaded.getName());
	}

	@Test
	public void loadThrowsIOExceptionWhenNameLineIsMissing()
	{
		String noNameLine = "Operator,Derived Time Series,Dvar/Svar,B part,C part,\n"
				+ "+,,DVAR,B1,C1,\n";
		try
		{
			DerivedTimeSeries.load(new ByteArrayInputStream(noNameLine.getBytes()));
			fail("Expected IOException when no 'name' line is present");
		}
		catch(IOException expected)
		{
			assertTrue(expected.getMessage().contains("No name found"));
		}
	}

	@Test
	public void loadThrowsIOExceptionWhenNoRowDefinitionsFollowName()
	{
		String nameOnly = "name, NO_ROWS.DTS\n";
		try
		{
			DerivedTimeSeries.load(new ByteArrayInputStream(nameOnly.getBytes()));
			fail("Expected IOException when no row-definition header is present");
		}
		catch(IOException expected)
		{
			assertTrue(expected.getMessage().contains("No definition's found"));
		}
	}


	@Test
	public void loadReadsNameAndRowsFromRealSavedTextDtsFile() throws IOException, URISyntaxException
	{
		DerivedTimeSeries dts = DerivedTimeSeries.load(resourcePath("legacy_sample.dts"));

		assertEquals("LEGACY_SAMPLE.DTS", dts.getName());
		assertEquals(3, dts.getNumberOfDataReferences());

		assertEquals(TimeSeriesMath.ADD, dts.getOperationIdAt(0));
		assertEquals("", dts.getDTSNameAt(0));
		assertEquals(AppUtils.DVAR, dts.getVarTypeAt(0));
		assertEquals("FLOW-CHANNEL", dts.getBPartAt(0));
		assertEquals("FLOW", dts.getCPartAt(0));

		assertEquals(TimeSeriesMath.SUB, dts.getOperationIdAt(1));
		assertEquals("", dts.getDTSNameAt(1));
		assertEquals(AppUtils.SVAR, dts.getVarTypeAt(1));
		assertEquals("STORAGE", dts.getBPartAt(1));
		assertEquals("STORAGE", dts.getCPartAt(1));

		assertEquals(TimeSeriesMath.MUL, dts.getOperationIdAt(2));
		assertEquals("OTHER_DTS.DTS", dts.getDTSNameAt(2));
	}

	@Test
	public void loadDoubleAppendsDtsExtensionWhenRealSavedFileNameIsAlreadyQualified()
			throws IOException, URISyntaxException
	{
		// Every DTS actually produced by save() already has a ".DTS"-suffixed name (see
		// saveThenLoadDoubleAppendsDtsExtensionWhenNameAlreadyHasOne for the in-memory version
		// of this). This resource was captured straight from such a save() call, so it shows the
		// double-suffix quirk is not a test artifact - it is what happens to any real saved file
		// once it is reloaded.
		DerivedTimeSeries dts = DerivedTimeSeries.load(resourcePath("already_qualified_sample.dts"));

		assertEquals("ALREADY_QUALIFIED.DTS.DTS", dts.getName());
		assertEquals(3, dts.getNumberOfDataReferences());
	}

	@Test
	public void loadDropsRowWithUnresolvedOperatorFromRealSavedFile() throws IOException, URISyntaxException
	{
		DerivedTimeSeries dts = DerivedTimeSeries.load(resourcePath("row_with_unresolved_operator.dts"));

		// the "?,,DVAR,B1,C1," row is silently skipped; only the valid "+" row survives
		assertEquals(1, dts.getNumberOfDataReferences());
		assertEquals(TimeSeriesMath.ADD, dts.getOperationIdAt(0));
		assertEquals("FLOW-CHANNEL", dts.getBPartAt(0));
		assertEquals("FLOW", dts.getCPartAt(0));
	}

	@Test
	public void loadThrowsIOExceptionForRealFileMissingNameHeader() throws URISyntaxException
	{
		try
		{
			DerivedTimeSeries.load(resourcePath("missing_name_header.dts"));
			fail("Expected IOException when no 'name' line is present");
		}
		catch(IOException expected)
		{
			assertTrue(expected.getMessage().contains("No name found"));
		}
	}

	@Test
	public void fromXmlReadsNameAndRowsFromRealSavedXmlDtsFile() throws Exception
	{
		Document doc = XMLHelpers.readXmlDocumentFromFile(resourcePath("xml_format_sample.dts"));
		Element dtsElement = (Element) doc.getDocumentElement().getElementsByTagName("DTS").item(0);

		DerivedTimeSeries dts = new DerivedTimeSeries();
		dts.fromXml(dtsElement);

		assertEquals("XML_RESOURCE_SAMPLE.DTS", dts.getName());
		assertEquals(3, dts.getNumberOfDataReferences());

		assertEquals(TimeSeriesMath.ADD, dts.getOperationIdAt(0));
		assertEquals(AppUtils.DVAR, dts.getVarTypeAt(0));
		assertEquals("FLOW-CHANNEL", dts.getBPartAt(0));
		assertEquals("FLOW", dts.getCPartAt(0));

		assertEquals(TimeSeriesMath.SUB, dts.getOperationIdAt(1));
		assertEquals(AppUtils.SVAR, dts.getVarTypeAt(1));
		assertEquals("STORAGE", dts.getBPartAt(1));
		assertEquals("STORAGE", dts.getCPartAt(1));

		assertEquals(TimeSeriesMath.MUL, dts.getOperationIdAt(2));
		assertEquals("OTHER_DTS.DTS", dts.getDTSNameAt(2));
		// unlike the text format, fromXml preserves the raw (blank) attributes verbatim
		assertEquals("", dts.getVarTypeAt(2));
		assertEquals("", dts.getBPartAt(2));
		assertEquals("", dts.getCPartAt(2));
	}

	private static String resourcePath(String name) throws URISyntaxException
	{
		URL url = DerivedTimeSeriesTest.class.getResource(name);
		if(url == null)
		{
			throw new IllegalStateException("Test resource not found on classpath: " + name);
		}
		return Paths.get(url.toURI()).toString();
	}

	private DerivedTimeSeries buildSampleDts(String name)
	{
		DerivedTimeSeries dts = new DerivedTimeSeries(name);

		dts.setOperationIdAt(0, TimeSeriesMath.ADD);
		dts.setVarTypeAt(0, AppUtils.DVAR);
		dts.setBPartAt(0, "FLOW-CHANNEL");
		dts.setCPartAt(0, "FLOW");

		dts.setOperationIdAt(1, TimeSeriesMath.SUB);
		dts.setVarTypeAt(1, AppUtils.SVAR);
		dts.setBPartAt(1, "STORAGE");
		dts.setCPartAt(1, "STORAGE");

		dts.setOperationIdAt(2, TimeSeriesMath.MUL);
		dts.setDTSNameAt(2, "OTHER_DTS");

		return dts;
	}
}
