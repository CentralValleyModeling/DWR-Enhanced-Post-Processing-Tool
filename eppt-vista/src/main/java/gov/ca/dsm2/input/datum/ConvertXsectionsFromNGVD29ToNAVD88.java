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

package gov.ca.dsm2.input.datum;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import gov.ca.dsm2.input.gis.GisUtil;
import gov.ca.dsm2.input.model.Channel;
import gov.ca.dsm2.input.model.DSM2Model;
import gov.ca.dsm2.input.model.Gate;
import gov.ca.dsm2.input.model.GateDevice;
import gov.ca.dsm2.input.model.Gates;
import gov.ca.dsm2.input.model.Node;
import gov.ca.dsm2.input.model.XSection;
import gov.ca.dsm2.input.model.XSectionLayer;
import gov.ca.dsm2.input.model.XSectionProfile;
import gov.ca.dsm2.input.parser.InputTable;
import gov.ca.dsm2.input.parser.Parser;
import gov.ca.dsm2.input.parser.Tables;

public class ConvertXsectionsFromNGVD29ToNAVD88 {

	public static void writeOutGateDataForVerconConversion(String echoFile,
			String gisFile, String vertconInputFile) throws Exception {

		Parser p = new Parser();
		Tables tables = p.parseModel(echoFile);
		p.parseAndAddToModel(tables, new FileInputStream(gisFile));

		DSM2Model model = tables.toDSM2Model();
		Gates gates = model.getGates();
		PrintWriter wr = new PrintWriter(vertconInputFile);

		for (Gate g : gates.getGates()) {
			String toNode = g.getToNode();
			Node node = model.getNodes().getNode(toNode);
			wr.println(String.format("%16.8f%16.8f%40s", node.getLatitude(),
					-node.getLongitude(), "Node: " + toNode));
		}
		wr.close();
	}

	public static void writeOutGatesAfterVertconConversion(String echoFile,
			String gisFile, String vertconOutputFile) throws Exception {
		Parser p = new Parser();
		Tables tables = p.parseModel(echoFile);
		p.parseAndAddToModel(tables, new FileInputStream(gisFile));

		DSM2Model model = tables.toDSM2Model();
		Gates gates = model.getGates();

		LineNumberReader lnr = new LineNumberReader(new FileReader(
				vertconOutputFile));
		HashMap<String, String> nodeCorrectionMap = new HashMap<String, String>();
		String line = lnr.readLine();
		while ((line = lnr.readLine()) != null) {
			line = line.trim();
			if (line.equals(""))
				continue;
			String[] fields = line.split("\\s+");
			nodeCorrectionMap.put(fields[3]+" "+fields[4], fields[2]);
		}
		lnr.close();

		for (Gate g : gates.getGates()) {
			String toNode = g.getToNode();
			String key = "Node: " + toNode;
			String corrVal = nodeCorrectionMap.get(key);
			if (corrVal == null) {
				System.err.println("No corrections found for : " + key
						+ "! Correct manually.");
				continue;
			}
			double corr_navd88_minus_ngvd29 = Double.parseDouble(corrVal) / 0.3048;
			for (GateDevice gd : g.getGateDevices()) {
				gd.elevation = gd.elevation + corr_navd88_minus_ngvd29;
			}
		}

		String preFixFilename = echoFile.split(".inp")[0];
		PrintWriter wr = new PrintWriter(preFixFilename
				+ "gates_after_conv.inp");
		tables.fromDSM2Model(model);
		for (InputTable table : tables.getTables()) {
			wr.println(table.toStringRepresentation());
		}
		wr.close();

	}

	/**
	 * Takes a hydro echo file and gis input file corresponding to it and writes
	 * out the location of each cross section to a file in the format acceptable
	 * to <a href="http://www.ngs.noaa.gov/PC_PROD/VERTCON/">vertcon program</a>
	 * 
	 * The default
	 * 
	 * @param echoFile
	 * @param gisFile
	 * @param vertconInputFile
	 */

	public static void writeOutXsectionsDataForVertconConversion(
			String echoFile, String gisFile, String vertconInputFile)
			throws Exception {

		Parser p = new Parser();
		Tables tables = p.parseModel(echoFile);
		p.parseAndAddToModel(tables, new FileInputStream(gisFile));

		DSM2Model model = tables.toDSM2Model();
		GisUtil gisUtil = new GisUtil();
		PrintWriter wr = new PrintWriter(vertconInputFile);
		for (Channel channel : model.getChannels().getChannels()) {
			for (XSection xs : channel.getXsections()) {
				double[] midPoint = null;
				XSectionProfile profile = xs.getProfile();
				if (profile == null) {
					System.err
							.println("No profile for "
									+ xs.getChannelId()
									+ ":"
									+ xs.getDistance()
									+ ". Using distance from upnode along channel profile");
					midPoint = gisUtil.getPointAtDistanceFromUpNode(
							channel.getId(), xs.getDistance(), model);
				} else {
					List<double[]> endPoints = profile.getEndPoints();
					double[] pt1 = endPoints.get(0), pt2 = endPoints.get(1);
					midPoint = new double[] { (pt1[0] + pt2[0]) / 2,
							(pt1[1] + pt2[1]) / 2 };
				}
				wr.println(String.format("%16.8f%16.8f%40s", midPoint[0],
						-midPoint[1],
						xs.getChannelId() + "," + xs.getDistance()));
			}
		}

		wr.close();

	}

	public static void writeVertconControlFile(String vertconInputFile,
			String vertconOutputFile) throws Exception {
		new File(vertconOutputFile).delete();

		File dir = new File(vertconOutputFile).getParentFile();
		PrintWriter wr = new PrintWriter(new FileWriter(new File(dir,
				"vertcon.ctrl")));
		wr.println("");
		wr.println("");
		wr.println("Y");
		wr.println("2");
		wr.println(vertconInputFile);
		wr.println(vertconOutputFile);
		wr.println();
		wr.println();
		wr.close();
		//
	}

	/**
	 * Takes a hydro echo file and gis file and a vertcon output file and
	 * applies the corrections to the cross sections in the hydro echo file and
	 * writes them out in the hydro echo file (without .inp prefix) and
	 * xsections_after_conv.inp (postfix). For example hydro_echo.inp will be
	 * create a hydro_echo_xsections_after_conv.inp file with the conversions
	 * 
	 * @param echoFile
	 * @param gisFile
	 * @param vertconOutputFile
	 * @throws Exception
	 */
	public static void writeOutXsectionsAfterVertconConversion(String echoFile,
			String gisFile, String vertconOutputFile) throws Exception {
		Parser p = new Parser();
		Tables tables = p.parseModel(echoFile);
		p.parseAndAddToModel(tables, new FileInputStream(gisFile));

		LineNumberReader lnr = new LineNumberReader(new FileReader(
				vertconOutputFile));
		HashMap<String, String> xsToCorrectionMap = new HashMap<String, String>();
		String line = lnr.readLine();
		while ((line = lnr.readLine()) != null) {
			line = line.trim();
			if (line.equals(""))
				continue;
			String[] fields = line.split("\\s+");
			xsToCorrectionMap.put(fields[3], fields[2]);
		}
		lnr.close();

		DSM2Model model = tables.toDSM2Model();
		for (Channel channel : model.getChannels().getChannels()) {
			for (XSection xs : channel.getXsections()) {
				String key = xs.getChannelId() + "," + xs.getDistance();
				String corrVal = xsToCorrectionMap.get(key);
				if (corrVal == null) {
					System.err.println("No corrections found for : " + key
							+ "! Correct manually.");
					continue;
				}
				double corr_navd88_minus_ngvd29 = Double.parseDouble(corrVal) / 0.3048;
				ArrayList<XSectionLayer> layers = xs.getLayers();
				for (XSectionLayer l : layers) {
					l.setElevation(l.getElevation() + corr_navd88_minus_ngvd29);
				}
			}
		}

		String preFixFilename = echoFile.split(".inp")[0];
		PrintWriter wr = new PrintWriter(preFixFilename
				+ "xsections_after_conv.inp");
		tables.fromDSM2Model(model);
		for (InputTable table : tables.getTables()) {
			wr.println(table.toStringRepresentation());
		}
		wr.close();

	}

	public static void main(String[] args) throws Exception {
		String echoFile = "D:/models/convert_ngvd29_navd88/hydro_echo_ALT2A_LLT_SLR45_ROA65.inp";
		String gisFile = "D:/models/convert_ngvd29_navd88/gis_2009_calibration.inp";
		File dir = new File(echoFile).getParentFile();
		String vertconInputFile = new File(dir, "vertcon.in").getAbsolutePath();
		String vertconOutputFile = new File(dir, "vertcon.out")
				.getAbsolutePath();
		//writeOutXsectionsDataForVertconConversion(echoFile, gisFile,vertconInputFile);
		// writeVertconControlFile(vertconInputFile, vertconOutputFile);
		// writeOutXsectionsAfterVertconConversion(echoFile, gisFile,
		// vertconOutputFile);
		writeOutGatesAfterVertconConversion(echoFile, gisFile, vertconOutputFile);
	}
}
