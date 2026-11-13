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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class MultipleTimeSeriesTest
{
	@Test
	public void newMtsHasNoDataReferences()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("EMPTY.MTS");
		assertEquals(0, mts.getNumberOfDataReferences());
	}

	@Test
	public void copyConstructorDoesNotActuallyPopulateTheNewInstance()
	{
		// MultipleTimeSeries(MultipleTimeSeries) builds a *local* MultipleTimeSeries, copies the
		// source's fields into that local, and then throws it away - "this" is never touched, so
		// its _name and all four row vectors are left at their Java default of null. Any call
		// that touches a row vector (even getNumberOfDataReferences()) then throws an NPE. This
		// pins down the existing (broken) behavior rather than asserting it is desirable.
		MultipleTimeSeries original = new MultipleTimeSeries("ORIGINAL.MTS");
		original.setBPartAt(0, "B1");

		MultipleTimeSeries copy = new MultipleTimeSeries(original);

		assertNull(copy.getName());
		try
		{
			copy.getNumberOfDataReferences();
			fail("Expected NullPointerException: the copy constructor never initializes 'this'");
		}
		catch(NullPointerException expected)
		{
			// documents the bug - see comment above
		}
	}

	@Test
	public void renamingCopyConstructorAlsoDoesNotPopulateTheNewInstance()
	{
		MultipleTimeSeries original = new MultipleTimeSeries("ORIGINAL.MTS");
		original.setBPartAt(0, "B1");

		MultipleTimeSeries copy = new MultipleTimeSeries(original, "RENAMED.MTS");

		assertNull(copy.getName());
	}

	@Test
	public void settersExpandRowsAndNormalizeValues()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("NAME.MTS");
		mts.setBPartAt(2, " flow ");
		assertEquals(3, mts.getNumberOfDataReferences());
		// intervening rows get default values
		assertEquals("", mts.getBPartAt(0));
		assertEquals(AppUtils.DVAR, mts.getVarTypeAt(0));
		// value is upper-cased and trimmed
		assertEquals("FLOW", mts.getBPartAt(2));
	}

	@Test
	public void setDTSNameAtDoesNotAppendDtsExtensionUnlikeDerivedTimeSeries()
	{
		// Contrast with DerivedTimeSeries.setDTSNameAt, which appends ".DTS" automatically: the
		// equivalent logic here is present but commented out in the source, so
		// MultipleTimeSeries.setDTSNameAt only upper-cases/trims - it never qualifies the name.
		MultipleTimeSeries mts = new MultipleTimeSeries("NAME.MTS");
		mts.setDTSNameAt(0, " other ");
		assertEquals("OTHER", mts.getDTSNameAt(0));
	}

	@Test
	public void setCPartAtSilentlyIgnoresNullInsteadOfTreatingItAsEmptyString()
	{
		// Contrast with DerivedTimeSeries.setCPartAt(i, null), which treats null as "" and still
		// expands the row. Here a null cpart is a pure no-op: it doesn't even call expandTo(i),
		// so a never-touched row index stays entirely unallocated.
		MultipleTimeSeries mts = new MultipleTimeSeries("NAME.MTS");
		mts.setCPartAt(0, null);
		assertEquals(0, mts.getNumberOfDataReferences());
		try
		{
			mts.getCPartAt(0);
			fail("Expected RuntimeException: row 0 was never allocated");
		}
		catch(RuntimeException expected)
		{
			// documents the bug - see comment above
		}
	}

	@Test
	public void setCPartAtWithNullLeavesAnExistingRowsValueUnchanged()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("NAME.MTS");
		mts.setCPartAt(0, "ORIGINAL");
		mts.setCPartAt(0, null);
		assertEquals("ORIGINAL", mts.getCPartAt(0));
	}

	@Test
	public void removeShiftsSubsequentRows()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("REMOVE.MTS");
		mts.setBPartAt(0, "B0");
		mts.setBPartAt(1, "B1");
		mts.remove(0);
		assertEquals(1, mts.getNumberOfDataReferences());
		assertEquals("B1", mts.getBPartAt(0));
	}

	@Test
	public void insertAtAddsEmptyRowAndShiftsExistingRows()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("INSERT.MTS");
		mts.setBPartAt(0, "B0");
		mts.setBPartAt(1, "B1");
		mts.insertAt(1);
		assertEquals(3, mts.getNumberOfDataReferences());
		assertEquals("B0", mts.getBPartAt(0));
		assertEquals("", mts.getBPartAt(1));
		assertEquals("B1", mts.getBPartAt(2));
	}

	@Test
	public void insertAtBeyondSizeIsNoOp()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("INSERT2.MTS");
		mts.setBPartAt(0, "B0");
		mts.insertAt(5);
		assertEquals(1, mts.getNumberOfDataReferences());
	}

	@Test
	public void equalsIsNotOverriddenUnlikeDerivedTimeSeries()
	{
		// DerivedTimeSeries.equals() compares by name. MultipleTimeSeries has no equals()
		// override at all, so two MTS objects with the same name are still distinct per
		// Object's reference-identity default.
		MultipleTimeSeries a = new MultipleTimeSeries("SAME.MTS");
		MultipleTimeSeries b = new MultipleTimeSeries("SAME.MTS");
		assertNotEquals(a, b);
		assertEquals(a, a);
	}

	@Test
	public void toStringIncludesName()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("TOSTRING.MTS");
		assertTrue(mts.toString().contains("TOSTRING.MTS"));
	}

	@Test
	public void getDataReferencesReturnsNullWhenThereAreNoRows()
	{
		MultipleTimeSeries mts = new MultipleTimeSeries("NODATA.MTS");
        assertNull(mts.getDataReferences(0));
	}

	// --- toXml / fromXml -------------------------------------------------

	@Test
	public void toXmlThenFromXmlRoundTripsBPartCPartRowsAndDtsReferenceRows()
	{
		MultipleTimeSeries original = buildSampleMts("XML_ROUNDTRIP.MTS");

		Document doc = XMLHelpers.createXmlDocument();
		Element master = doc.createElement("mts_master");
		doc.appendChild(master);
		original.toXml(doc, master);

		Element mtsElement = (Element) master.getElementsByTagName("MTS").item(0);
		assertEquals("XML_ROUNDTRIP.MTS", mtsElement.getAttribute("name"));

		NodeList rows = mtsElement.getElementsByTagName("row");
		assertEquals(3, rows.getLength());

		MultipleTimeSeries reloaded = new MultipleTimeSeries();
		reloaded.fromXml(mtsElement);

		assertEquals(original.getName(), reloaded.getName());
		assertEquals(original.getNumberOfDataReferences(), reloaded.getNumberOfDataReferences());
		for(int i = 0; i < original.getNumberOfDataReferences(); i++)
		{
			assertEquals("row " + i + " dts name", original.getDTSNameAt(i), reloaded.getDTSNameAt(i));
			assertEquals("row " + i + " var type", original.getVarTypeAt(i), reloaded.getVarTypeAt(i));
			assertEquals("row " + i + " b part", original.getBPartAt(i), reloaded.getBPartAt(i));
			assertEquals("row " + i + " c part", original.getCPartAt(i), reloaded.getCPartAt(i));
		}
	}

	@Test
	public void fromXmlAppendsDtsExtensionToDtsAttributeUnlikeSetDTSNameAt()
	{
		// Contrast with setDTSNameAtDoesNotAppendDtsExtensionUnlikeDerivedTimeSeries above:
		// fromXml normalizes the "dts" attribute itself (appends ".DTS" when missing) before
		// delegating to setDTSNameAt, so the net effect of loading via XML differs from calling
		// setDTSNameAt directly with the same raw string.
		Document doc = XMLHelpers.createXmlDocument();
		Element mtsElement = doc.createElement("MTS");
		mtsElement.setAttribute("name", "WITHREF.MTS");
		Element row = doc.createElement("row");
		row.setAttribute("dts", "other");
		row.setAttribute("vartype", "");
		row.setAttribute("bpart", "");
		row.setAttribute("cpart", "");
		mtsElement.appendChild(row);
		doc.appendChild(mtsElement);

		MultipleTimeSeries mts = new MultipleTimeSeries();
		mts.fromXml(mtsElement);

		assertEquals("OTHER.DTS", mts.getDTSNameAt(0));
	}

	@Test
	public void fromXmlAppendsMtsExtensionOnlyWhenMissing()
	{
		Document doc = XMLHelpers.createXmlDocument();
		Element mtsElement = doc.createElement("MTS");
		mtsElement.setAttribute("name", "plainname");
		doc.appendChild(mtsElement);

		MultipleTimeSeries mts = new MultipleTimeSeries();
		mts.fromXml(mtsElement);

		assertEquals("PLAINNAME.MTS", mts.getName());
	}

	// --- save / load (text format) ----------------------------------------

	@Test
	public void saveThenLoadRoundTripsRowsAndDoesNotQualifyTheDtsReferenceName() throws IOException
	{
		MultipleTimeSeries original = buildSampleMts("SAVE_LOAD.MTS");

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		original.save(out);

		MultipleTimeSeries reloaded = MultipleTimeSeries.load(new ByteArrayInputStream(out.toByteArray()));

		assertEquals(original.getNumberOfDataReferences(), reloaded.getNumberOfDataReferences());
		for(int i = 0; i < original.getNumberOfDataReferences(); i++)
		{
			assertEquals("row " + i + " dts name", original.getDTSNameAt(i), reloaded.getDTSNameAt(i));
			if(original.getDTSNameAt(i).isEmpty())
			{
				assertEquals("row " + i + " var type", original.getVarTypeAt(i), reloaded.getVarTypeAt(i));
				assertEquals("row " + i + " b part", original.getBPartAt(i), reloaded.getBPartAt(i));
				assertEquals("row " + i + " c part", original.getCPartAt(i), reloaded.getCPartAt(i));
			}
			else
			{
				// as with DerivedTimeSeries, MTSTableModel.getValueAt blanks var-type/b-part/
				// c-part for a DTS-reference row, and load() reads those blanks back literally.
				assertEquals("", reloaded.getVarTypeAt(i));
				assertEquals("", reloaded.getBPartAt(i));
				assertEquals("", reloaded.getCPartAt(i));
			}
		}
	}

	@Test
	public void saveThenLoadDropsRowWithInvalidVarType() throws IOException
	{
		// Unlike DerivedTimeSeries (where an unresolved *operator* gets dropped), here it's an
		// invalid *var type* that gets silently skipped: AppUtils.SVAR/DVAR are the only
		// accepted values, and load() simply discards any row that doesn't match either.
		MultipleTimeSeries mts = new MultipleTimeSeries("DROPPED_ROW.MTS");
		mts.setVarTypeAt(0, "XVAR");
		mts.setBPartAt(0, "B1");
		mts.setCPartAt(0, "C1");
		mts.setVarTypeAt(1, AppUtils.DVAR);
		mts.setBPartAt(1, "B2");
		mts.setCPartAt(1, "C2");

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		mts.save(out);

		MultipleTimeSeries reloaded = MultipleTimeSeries.load(new ByteArrayInputStream(out.toByteArray()));

		assertEquals(1, reloaded.getNumberOfDataReferences());
		assertEquals("B2", reloaded.getBPartAt(0));
		assertEquals("C2", reloaded.getCPartAt(0));
	}

	@Test
	public void saveThenLoadDoubleAppendsMtsExtensionWhenNameAlreadyHasOne() throws IOException
	{
		// Same quirk as DerivedTimeSeries.load(): the name parsed from the "name, ..." line
		// unconditionally gets ".MTS" appended, even when it already ends with ".MTS" - which it
		// always will for any MTS actually produced by save(), since getName() already carries
		// the suffix by the time save() runs.
		MultipleTimeSeries mts = new MultipleTimeSeries("ALREADY_QUALIFIED.MTS");
		mts.setBPartAt(0, "B1");
		mts.setCPartAt(0, "C1");

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		mts.save(out);

		MultipleTimeSeries reloaded = MultipleTimeSeries.load(new ByteArrayInputStream(out.toByteArray()));

		assertEquals("ALREADY_QUALIFIED.MTS.MTS", reloaded.getName());
	}

	@Test
	public void loadThrowsIOExceptionWhenNameLineIsMissing()
	{
		String noNameLine = "Derived Time Series,Dvar/Svar,B part,C part,\n"
				+ ",DVAR,B1,C1,\n";
		try
		{
			MultipleTimeSeries.load(new ByteArrayInputStream(noNameLine.getBytes()));
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
		String nameOnly = "name, NO_ROWS.MTS\n";
		try
		{
			MultipleTimeSeries.load(new ByteArrayInputStream(nameOnly.getBytes()));
			fail("Expected IOException when no row-definition header is present");
		}
		catch(IOException expected)
		{
			assertTrue(expected.getMessage().contains("No definition's found"));
		}
	}

	// --- reading from saved MTS files (test resources) ----------------------
	//
	// These resources live under src/test/resources/calsim/app/ and were produced by actually
	// calling MultipleTimeSeries.save() (see the DerivedTimeSeriesTest equivalent for the
	// throwaway program used to capture the exact byte layout), so they reproduce what a real
	// saved .mts file looks like rather than a hand-guessed approximation of the format.

	@Test
	public void loadReadsNameAndRowsFromRealSavedTextMtsFile() throws IOException, URISyntaxException
	{
		MultipleTimeSeries mts = MultipleTimeSeries.load(resourcePath("legacy_sample.mts"));

		assertEquals("LEGACY_SAMPLE.MTS", mts.getName());
		assertEquals(3, mts.getNumberOfDataReferences());

		assertEquals("", mts.getDTSNameAt(0));
		assertEquals(AppUtils.DVAR, mts.getVarTypeAt(0));
		assertEquals("FLOW-CHANNEL", mts.getBPartAt(0));
		assertEquals("FLOW", mts.getCPartAt(0));

		assertEquals("", mts.getDTSNameAt(1));
		assertEquals(AppUtils.SVAR, mts.getVarTypeAt(1));
		assertEquals("STORAGE", mts.getBPartAt(1));
		assertEquals("STORAGE", mts.getCPartAt(1));

		// the dts-reference row round trips verbatim - load() never qualifies it with ".DTS"
		assertEquals("OTHER_DTS.DTS", mts.getDTSNameAt(2));
	}

	@Test
	public void loadDoubleAppendsMtsExtensionWhenRealSavedFileNameIsAlreadyQualified()
			throws IOException, URISyntaxException
	{
		MultipleTimeSeries mts = MultipleTimeSeries.load(resourcePath("already_qualified_sample.mts"));

		assertEquals("ALREADY_QUALIFIED.MTS.MTS", mts.getName());
		assertEquals(3, mts.getNumberOfDataReferences());
	}

	@Test
	public void loadDropsRowWithInvalidVarTypeFromRealSavedFile() throws IOException, URISyntaxException
	{
		MultipleTimeSeries mts = MultipleTimeSeries.load(resourcePath("row_with_invalid_vartype.mts"));

		// the ",XVAR,B1,C1," row is silently skipped; only the valid DVAR row survives
		assertEquals(1, mts.getNumberOfDataReferences());
		assertEquals("FLOW-CHANNEL", mts.getBPartAt(0));
		assertEquals("FLOW", mts.getCPartAt(0));
	}

	@Test
	public void loadThrowsIOExceptionForRealFileMissingNameHeader() throws URISyntaxException
	{
		try
		{
			MultipleTimeSeries.load(resourcePath("missing_name_header.mts"));
			fail("Expected IOException when no 'name' line is present");
		}
		catch(IOException expected)
		{
			assertTrue(expected.getMessage().contains("No name found"));
		}
	}

	@Test
	public void fromXmlReadsNameAndRowsFromRealSavedXmlMtsFile() throws Exception
	{
		Document doc = XMLHelpers.readXmlDocumentFromFile(resourcePath("xml_format_sample.mts"));
		Element mtsElement = (Element) doc.getDocumentElement().getElementsByTagName("MTS").item(0);

		MultipleTimeSeries mts = new MultipleTimeSeries();
		mts.fromXml(mtsElement);

		assertEquals("XML_RESOURCE_SAMPLE.MTS", mts.getName());
		assertEquals(3, mts.getNumberOfDataReferences());

		assertEquals("", mts.getDTSNameAt(0));
		assertEquals(AppUtils.DVAR, mts.getVarTypeAt(0));
		assertEquals("FLOW-CHANNEL", mts.getBPartAt(0));
		assertEquals("FLOW", mts.getCPartAt(0));

		assertEquals("", mts.getDTSNameAt(1));
		assertEquals(AppUtils.SVAR, mts.getVarTypeAt(1));
		assertEquals("STORAGE", mts.getBPartAt(1));
		assertEquals("STORAGE", mts.getCPartAt(1));

		// the resource's row has dts="OTHER_DTS" (no .DTS); fromXml qualifies it
		assertEquals("OTHER_DTS.DTS", mts.getDTSNameAt(2));
	}

	private static String resourcePath(String name) throws URISyntaxException
	{
		URL url = MultipleTimeSeriesTest.class.getResource(name);
		if(url == null)
		{
			throw new IllegalStateException("Test resource not found on classpath: " + name);
		}
		return Paths.get(url.toURI()).toString();
	}

	private MultipleTimeSeries buildSampleMts(String name)
	{
		MultipleTimeSeries mts = new MultipleTimeSeries(name);

		mts.setVarTypeAt(0, AppUtils.DVAR);
		mts.setBPartAt(0, "FLOW-CHANNEL");
		mts.setCPartAt(0, "FLOW");

		mts.setVarTypeAt(1, AppUtils.SVAR);
		mts.setBPartAt(1, "STORAGE");
		mts.setCPartAt(1, "STORAGE");

		mts.setDTSNameAt(2, "OTHER_DTS.DTS");

		return mts;
	}
}
