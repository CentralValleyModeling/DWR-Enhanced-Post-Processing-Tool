/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.dsm2.input.parser.InputTable;
import gov.ca.dsm2.input.parser.Parser;
import gov.ca.dsm2.input.parser.Tables;
import gov.ca.water.calgui.presentation.display.ReportPDFWriter;
import vista.db.dss.DSSUtil;
import vista.report.TSMath;
import vista.set.Constants;
import vista.set.DataReference;
import vista.set.DataSetElement;
import vista.set.ElementFilter;
import vista.set.ElementFilterIterator;
import vista.set.Group;
import vista.set.MultiIterator;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.Stats;
import vista.set.TimeSeries;
import vista.time.SubTimeFormat;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeWindow;

/**
 * Generates a report based on the template file instructions
 *
 * @author psandhu
 */
public class Report extends SwingWorker<Void, String>
{

	private static final Logger LOG = Logger.getLogger(Report.class.getName());
	private static final String TIME_SERIES = "timeseries";
	private static final String EXCEEDANCE = "exceedance";
	private final JFrame _mainFrame;
	private final StringBuilder _messages = new StringBuilder();
	private ProgressFrameForPDF _progressFrameForPDF;
	private InputStream _inputStream;

	/*
	 * ********* START SwingWorker additions
	 */
	private String _outputFilename;
	private boolean _isInitialized = false;
	private List<ArrayList<String>> _twValues;
	private List<PathnameMap> _pathnameMaps;
	private HashMap<String, String> _scalars;
	private Writer _writer;

	public Report(InputStream inputStream, String outputFilename, JFrame mainFrame)
	{
		_mainFrame = mainFrame;
		this._inputStream = inputStream;
		this._outputFilename = outputFilename;
		this._isInitialized = true;
	}

	/*
	 * ********* END SwingWorker additions
	 */

	public Report(String templateFile, JFrame mainFrame) throws IOException
	{
		this(new FileInputStream(templateFile), mainFrame);
	}

	public Report(InputStream inputStream, JFrame mainFrame) throws IOException
	{
		_mainFrame = mainFrame;
		generateReport(inputStream);
	}

	@Override
	protected Void doInBackground() throws Exception
	{
		_progressFrameForPDF = ProgressFrameForPDF.getProgressFrameInstance(_mainFrame);
		_progressFrameForPDF.toFront();
		_progressFrameForPDF.setVisible(true);
		publish("Generating report in background thread.");

		LOG.fine("Parsing input template");
		publish("Parsing input template.");
		parseTemplateFile(_inputStream);

		publish("Processing DSS files.");
		doProcessing();
		publish("Done");

		LOG.fine("Done generating report");

		return null;
	}

	@Override
	protected void process(List<String> status)
	{
		_progressFrameForPDF.setList(status.get(status.size() - 1));
		return;
	}

	@Override
	protected void done()
	{

		String command = "cmd /c start " + _outputFilename;
		try
		{
			Runtime.getRuntime().exec(command);
		}
		catch(IOException e)
		{
			LOG.log(Level.FINE, "Error thrown processing command: " + command, e);
		}
	}

	private void generateReport(InputStream templateContentStream) throws IOException
	{
		LOG.fine("Parsing input template");
		clearMessages();
		parseTemplateFile(templateContentStream);
		doProcessing();
		LOG.fine("Done generating report");
	}

	private void parseTemplateFile(InputStream templateFileStream) throws IOException
	{

		if(_isInitialized)
		{
			publish("Parsing template file.");
		}

		Parser p = new Parser();
		Tables tables = p.parseModel(templateFileStream);
		// load scalars into a map
		InputTable scalarTable = tables.getTableNamed("SCALAR");
		ArrayList<ArrayList<String>> scalarValues = scalarTable.getValues();
		int nscalars = scalarValues.size();
		_scalars = new HashMap<String, String>();
		for(int i = 0; i < nscalars; i++)
		{
			String name = scalarTable.getValue(i, "NAME");
			String value = scalarTable.getValue(i, "VALUE");
			_scalars.put(name, value);
		}
		// load pathname mapping into a map
		InputTable pathnameMappingTable = tables.getTableNamed("PATHNAME_MAPPING");
		ArrayList<ArrayList<String>> pmap_values = pathnameMappingTable.getValues();
		int nvalues = pmap_values.size();
		_pathnameMaps = new ArrayList<PathnameMap>();
		for(int i = 0; i < nvalues; i++)
		{
			String var_name = pathnameMappingTable.getValue(i, "VARIABLE");
			var_name = var_name.replace("\"", "");
			PathnameMap path_map = new PathnameMap(var_name);
			path_map._reportType = pathnameMappingTable.getValue(i, "REPORT_TYPE").toLowerCase();
			path_map._pathBase = pathnameMappingTable.getValue(i, "PATH_BASE");
			path_map._pathAlt = pathnameMappingTable.getValue(i, "PATH_ALT");
			path_map._varCategory = pathnameMappingTable.getValue(i, "VAR_CATEGORY");
			path_map._rowType = pathnameMappingTable.getValue(i, "ROW_TYPE");
			if((path_map._pathAlt == null) || (path_map._pathAlt.length() == 0))
			{
				path_map._pathAlt = path_map._pathBase;
			}
			path_map._plot = pathnameMappingTable.getValue(i, "PLOT").equalsIgnoreCase("Y");
			path_map._units = pathnameMappingTable.getValue(i, "UNIT");
			_pathnameMaps.add(path_map);
		}
		InputTable timeWindowTable = tables.getTableNamed("TIME_PERIODS");
		_twValues = timeWindowTable.getValues();
	}

	public void doProcessing()
	{
		// open files 1 and file 2 and loop over to plot
		if(_isInitialized)
		{
			publish("Processing template file.");
		}
		Group dssGroupBase = opendss(_scalars.get("FILE_BASE"));
		Group dssGroupAlt = opendss(_scalars.get("FILE_ALT"));
		ArrayList<TimeWindow> timewindows = new ArrayList<TimeWindow>();
		for(ArrayList<String> values : _twValues)
		{
			String v = values.get(1).replace("\"", "");
			timewindows.add(TimeFactory.getInstance().createTimeWindow(v));
		}
		TimeWindow tw = null;
		if(timewindows.size() > 0)
		{
			tw = timewindows.get(0);
		}
		String output_file = _scalars.get("OUTFILE");
		_writer = new ReportPDFWriter();
		_writer.startDocument(output_file);
		String author = _scalars.get("MODELER").replace("\"", "");
		_writer.addTitlePage(String.format("System Water Balance Report: %s vs %s", _scalars.get("NAME_ALT"),
				_scalars.get("NAME_BASE")), author, _scalars.get("FILE_BASE"), _scalars.get("FILE_ALT"));
		_writer.setAuthor(author);
		if((dssGroupBase == null) || (dssGroupAlt == null))
		{
			String msg = "No data available in either : " + _scalars.get("FILE_BASE") + " or " + _scalars.get(
					"FILE_ALT");
			LOG.severe(msg);
			addMessage(msg);
			return;
		}

		generateSummaryTable();
		int dataIndex = 0;
		for(PathnameMap pathMap : _pathnameMaps)
		{
			dataIndex = dataIndex + 1;
			if(_isInitialized)
			{
				publish("Generating plot " + dataIndex + " of " + _pathnameMaps.size() + ".");
			}

			LOG.fine("Working on index: " + dataIndex);
			if((pathMap._pathAlt == null) || (pathMap._pathAlt == ""))
			{
				pathMap._pathAlt = pathMap._pathBase;
			}
			boolean calculate_dts = false;
			if(pathMap._varCategory.equals("HEADER"))
			{
				LOG.fine("Inserting header");
				continue;
			}
			if(pathMap._reportType.endsWith("_post"))
			{
				calculate_dts = true;
			}
			DataReference refBase = getReference(dssGroupBase, pathMap._pathBase, calculate_dts, _pathnameMaps, 1);
			DataReference refAlt = getReference(dssGroupAlt, pathMap._pathAlt, calculate_dts, _pathnameMaps, 2);
			if((refBase == null) || (refAlt == null))
			{
				continue;
			}
			// Switch order from original code to reverse legends ... LimnoTech
			// 20110816
			String[] series_name = new String[]{_scalars.get("NAME_ALT"), _scalars.get("NAME_BASE")};
			// String[] series_name = new String[] { scalars.get("NAME_BASE"),
			// scalars.get("NAME_ALT") };
			if(pathMap._units.equals("CFS2TAF"))
			{
				TSMath.cfs2taf((RegularTimeSeries) refBase.getData());
				TSMath.cfs2taf((RegularTimeSeries) refAlt.getData());
			}
			else if(pathMap._units.equals("TAF2CFS"))
			{
				TSMath.taf2cfs((RegularTimeSeries) refBase.getData());
				TSMath.taf2cfs((RegularTimeSeries) refAlt.getData());
			}
			String data_units = getUnits(refBase, refAlt);
			String data_type = getType(refBase, refAlt);
			if(pathMap._plot)
			{
				if(pathMap._reportType.startsWith("average"))
				{
					generatePlot(buildDataArray(refAlt, refBase, tw), dataIndex,
							"Average " + pathMap._varName.replace("\"", ""), series_name,
							data_type + "(" + data_units + ")", "Time", TIME_SERIES);
				}
				else if(pathMap._reportType.startsWith("exceedance"))
				{
					generatePlot(buildExceedanceArray(refAlt, refBase, pathMap._varCategory == "S_SEPT", tw), dataIndex,
							getExceedancePlotTitle(pathMap), series_name, data_type + "(" + data_units + ")",
							"Percent at or above", EXCEEDANCE);
				}
				else if(pathMap._reportType.startsWith("avg_excd"))
				{
					generatePlot(buildDataArray(refAlt, refBase, tw), dataIndex,
							"Average " + pathMap._varName.replace("\"", ""), series_name,
							data_type + "(" + data_units + ")", "Time", TIME_SERIES);
					generatePlot(buildExceedanceArray(refAlt, refBase, pathMap._varCategory == "S_SEPT", tw), dataIndex,
							getExceedancePlotTitle(pathMap), series_name, data_type + "(" + data_units + ")",
							"Percent at or above", EXCEEDANCE);
				}
				else if(pathMap._reportType.startsWith("timeseries"))
				{
					generatePlot(buildDataArray(refAlt, refBase, tw), dataIndex,
							"Average " + pathMap._varName.replace("\"", ""), series_name,
							data_type + "(" + data_units + ")", "Time", TIME_SERIES);
				}
				else if(pathMap._reportType.equals("alloc"))
				{
					generatePlot(buildExceedanceArray(refAlt, refBase, true, tw), dataIndex,
							"Exceedance " + pathMap._varName.replace("\"", ""), series_name, "Allocation (%)",
							"Probability", EXCEEDANCE);
				}
			}
		}
		_writer.endDocument();
	}

	private void generateSummaryTable()
	{

		if(_isInitialized)
		{
			publish("Generating summary table.");
		}

		_writer.setTableFontSize(_scalars.get("TABLE_FONT_SIZE"));

		_writer.addTableTitle(
				String.format("System Flow Comparision: %s vs %s", _scalars.get("NAME_ALT"),
						_scalars.get("NAME_BASE")));
		_writer.addTableSubTitle(_scalars.get("NOTE").replace("\"", ""));
		_writer.addTableSubTitle(_scalars.get("ASSUMPTIONS").replace("\"", ""));
		_writer.addTableSubTitle(" "); // add empty line to increase space
		// between title and table
		Group dssGroupBase = opendss(_scalars.get("FILE_BASE"));
		Group dssGroupAlt = opendss(_scalars.get("FILE_ALT"));
		ArrayList<TimeWindow> timewindows = new ArrayList<TimeWindow>();
		for(ArrayList<String> values : _twValues)
		{
			String v = values.get(1).replace("\"", "");
			timewindows.add(TimeFactory.getInstance().createTimeWindow(v));
		}
		ArrayList<String> headerRow = new ArrayList<String>();
		headerRow.add("");
		ArrayList<String> headerRow2 = new ArrayList<String>();
		headerRow2.add("");

		for(TimeWindow tw : timewindows)
		{
			headerRow.add(formatTimeWindowAsWaterYear(tw));
			headerRow2.addAll(Arrays.asList(_scalars.get("NAME_ALT"), _scalars.get("NAME_BASE"), "Diff", "% Diff"));
		}
		int[] columnSpans = new int[timewindows.size() + 1];
		columnSpans[0] = 1;
		for(int i = 1; i < columnSpans.length; i++)
		{
			columnSpans[i] = 4;
		}
		_writer.addTableHeader(headerRow, columnSpans);
		_writer.addTableHeader(headerRow2, null);
		List<String> categoryList = Arrays.asList("RF", "DI", "DO", "DE", "SWPSOD", "CVPSOD");
		boolean firstDataRow = true;
		int dataIndex = 0;
		for(PathnameMap pathMap : _pathnameMaps)
		{
			dataIndex++;
			if(_isInitialized)
			{
				publish("Processing dataset " + dataIndex + " of " + _pathnameMaps.size());
			}

			if(!categoryList.contains(pathMap._varCategory))
			{
				continue;
			}
			ArrayList<String> rowData = new ArrayList<String>();
			rowData.add(pathMap._varName);
			boolean calculate_dts = false;
			if(pathMap._reportType.toLowerCase().endsWith("_post"))
			{
				calculate_dts = true;
			}
			DataReference refBase = null, refAlt = null;
			if(!pathMap._pathBase.equalsIgnoreCase("ignore"))
			{
				refBase = getReference(dssGroupBase, pathMap._pathBase, calculate_dts, _pathnameMaps, 1);
			}
			if(!pathMap._pathAlt.equalsIgnoreCase("ignore"))
			{
				refAlt = getReference(dssGroupAlt, pathMap._pathAlt, calculate_dts, _pathnameMaps, 2);
			}
			for(TimeWindow tw : timewindows)
			{
				double avgBase = 0, avgAlt = 0;
				if(refAlt != null)
				{
					avgAlt = avg(cfs2taf((RegularTimeSeries) refAlt.getData()), tw);
					rowData.add(formatDoubleValue(avgAlt));
				}
				else
				{
					rowData.add("");
				}
				if(refBase != null)
				{
					avgBase = avg(cfs2taf((RegularTimeSeries) refBase.getData()), tw);
					rowData.add(formatDoubleValue(avgBase));
				}
				else
				{
					rowData.add("");
				}
				if((refBase == null) || (refAlt == null))
				{
					rowData.add("");
					rowData.add("");
				}
				else
				{
					double diff = avgAlt - avgBase;
					double pctDiff = Double.NaN;
					if(avgBase != 0)
					{
						pctDiff = diff / avgBase * 100;
					}
					rowData.add(formatDoubleValue(diff));
					rowData.add(formatDoubleValue(pctDiff));
				}
			}
			if("B".equals(pathMap._rowType))
			{
				if(!firstDataRow)
				{
					ArrayList<String> blankRow = new ArrayList<String>();
					for(int i = 0; i < rowData.size(); i++)
					{
						blankRow.add(" ");
					}
					_writer.addTableRow(blankRow, null, Writer.NORMAL, false);
				}
				_writer.addTableRow(rowData, null, Writer.BOLD, false);
			}
			else
			{
				_writer.addTableRow(rowData, null, Writer.NORMAL, false);
			}
			firstDataRow = false;
		}
		_writer.endTable();
	}

	private String formatDoubleValue(double val)
	{
		return Double.isNaN(val) ? "" : String.format("%3d", Math.round(val));
	}

	private void generatePlot(List<double[]> buildDataArray, int dataIndex, String title, String[] seriesName,
							  String yAxisLabel, String xAxisLabel, String plotType)
	{
		if(plotType.equals(TIME_SERIES))
		{
			_writer.addTimeSeriesPlot(buildDataArray, title, seriesName, xAxisLabel, yAxisLabel);
		}
		else if(plotType.equals(EXCEEDANCE))
		{
			_writer.addExceedancePlot(buildDataArray, title, seriesName, xAxisLabel, yAxisLabel);
		}
		else
		{
			String msg = "Requested unknown plot type: " + plotType + " for title: " + title + " seriesName: "
					+ seriesName[0] + ",..";
			LOG.warning(msg);
			addMessage(msg);
		}
	}

	public String getOutputFile()
	{
		return _scalars.get("OUTFILE");
	}

	private ArrayList<double[]> buildDataArray(DataReference ref1, DataReference ref2, TimeWindow tw)
	{
		ArrayList<double[]> dlist = new ArrayList<double[]>();
		if((ref1 == null) || (ref2 == null))
		{
			return dlist;
		}
		TimeSeries data1 = (TimeSeries) ref1.getData();
		TimeSeries data2 = (TimeSeries) ref2.getData();
		if(tw != null)
		{
			data1 = data1.createSlice(tw);
			data2 = data2.createSlice(tw);
		}
		MultiIterator iterator = buildMultiIterator(new TimeSeries[]{data1, data2},
				vista.set.Constants.DEFAULT_FLAG_FILTER);
		while(!iterator.atEnd())
		{
			DataSetElement e = iterator.getElement();
			Date date = convertToDate(TimeFactory.getInstance().createTime(e.getXString()));
			dlist.add(new double[]{date.getTime(), e.getX(1), e.getX(2)});
			iterator.advance();
		}
		return dlist;
	}

	private Date convertToDate(Time time_val)
	{
		return new Date(time_val.getDate().getTime() - TimeZone.getDefault().getRawOffset());
	}

	private MultiIterator buildMultiIterator(TimeSeries[] dsarray, ElementFilter filter)
	{
		if(filter == null)
		{
			return new MultiIterator(dsarray);
		}
		else
		{
			return new MultiIterator(dsarray, filter);
		}
	}

	public ArrayList<double[]> buildExceedanceArray(DataReference ref1, DataReference ref2, boolean end_of_sept,
													TimeWindow tw)
	{
		ArrayList<Double> x1 = sort(ref1, end_of_sept, tw);
		ArrayList<Double> x2 = sort(ref2, end_of_sept, tw);
		ArrayList<double[]> darray = new ArrayList<double[]>();
		int i = 0;
		int n = Math.round(Math.min(x1.size(), x2.size()));
		while(i < n)
		{
			darray.add(new double[]{100.0 - 100.0 * i / (n + 1), x1.get(i), x2.get(i)});
			i = i + 1;
		}
		return darray;
	}

	private ArrayList<Double> sort(DataReference ref, boolean end_of_sept, TimeWindow tw)
	{
		TimeSeries data = (TimeSeries) ref.getData();
		if(tw != null)
		{
			data = data.createSlice(tw);
		}
		ArrayList<Double> dx = new ArrayList<Double>();
		ElementFilterIterator iter = new ElementFilterIterator(data.getIterator(), Constants.DEFAULT_FLAG_FILTER);
		while(!iter.atEnd())
		{
			if(end_of_sept)
			{
				if(iter.getElement().getXString().indexOf("30SEP") >= 0)
				{
					dx.add(iter.getElement().getY());
				}
			}
			else
			{
				dx.add(iter.getElement().getY());
			}
			iter.advance();
		}
		Collections.sort(dx);
		return dx;
	}

	public String getTypeOfReference(DataReference ref)
	{
		if(ref != null)
		{
			Pathname p = ref.getPathname();
			return p.getPart(Pathname.C_PART);
		}
		return "";
	}

	public String getType(DataReference ref1, DataReference ref2)
	{
		if(ref1 == null)
		{
			if(ref2 == null)
			{
				return "";
			}
			else
			{
				return getTypeOfReference(ref2);
			}
		}
		else
		{
			return getTypeOfReference(ref1);
		}
	}

	/**
	 * Retrieves the contents list for a dss file
	 *
	 * @param filename
	 * @return a handle to the content listing for a dss file
	 */
	public Group opendss(String filename)
	{
		return DSSUtil.createGroup("local", filename);
	}

	public RegularTimeSeries cfs2taf(RegularTimeSeries data)
	{
		RegularTimeSeries data_taf = (RegularTimeSeries) TSMath.createCopy(data);
		TSMath.cfs2taf(data_taf);
		return data_taf;
	}

	public double avg(RegularTimeSeries data, TimeWindow tw)
	{
		try
		{
			return Stats.avg(data.createSlice(tw)) * 12;
		}
		catch(Exception ex)
		{
			LOG.fine(ex.getMessage());
			return Double.NaN;
		}
	}

	private DataReference getReference(Group group, String path, boolean calculate_dts,
									   List<PathnameMap> pathname_maps, int group_no)
	{
		if(calculate_dts)
		{
			try
			{
				// FIXME: add expression parser to enable any expression
				String bpart = path.split("/")[2];
				String[] vars = bpart.split("\\+");
				DataReference ref = null;
				for(String varname : vars)
				{
					DataReference xref = null;
					String varPath = createPathFromVarname(path, varname);
					xref = getReference(group, varPath, false, pathname_maps, group_no);
					if(xref == null)
					{
						throw new RuntimeException("Aborting calculation of " + path + " due to previous path missing");
					}
					if(ref == null)
					{
						ref = xref;
					}
					else
					{
						ref = ref.__add__(xref);
					}
				}
				return ref;
			}
			catch(Exception ex)
			{
				addMessage(ex.getMessage());
				LOG.fine(ex.getMessage());
				return null;
			}
		}
		else
		{
			try
			{
				DataReference[] refs = findpath(group, path, true);
				if(refs == null)
				{
					String msg = "No data found for " + group + " and " + path;
					addMessage(msg);
					LOG.severe(msg);
					return null;
				}
				else
				{
					return refs[0];
				}
			}
			catch(Exception ex)
			{
				String msg = "Exception while trying to retrieve " + path + " from " + group;
				LOG.severe(msg);
				addMessage(msg);
				LOG.fine(msg);
				return null;
			}
		}
	}

	/**
	 * findpath(g,path,exact=1): this returns an array of matching data
	 * references g is the group returned from opendss function path is the
	 * dsspathname e.g. '//C6/FLOW-CHANNEL////' exact means that the exact
	 * string is matched as opposed to the reg. exp.
	 *
	 * @param g
	 * @param path
	 * @param exact
	 * @return
	 */
	private DataReference[] findpath(Group g, String path, boolean exact)
	{
		String[] pa = new String[6];
		for(int i = 0; i < 6; i++)
		{
			pa[i] = "";
		}
		int i = 0;
		String[] split = path.trim().split("/");
		for(String p : split)
		{
			if(i == 0)
			{
				i++;
				continue;
			}
			if(i >= pa.length)
			{
				break;
			}
			pa[i - 1] = p;
			if(exact && p.length() > 0)
			{
				pa[i - 1] = "^" + pa[i - 1] + "$";
			}
			i++;
		}
		return g.find(pa);
	}

	private String createPathFromVarname(String path, String varname)
	{
		String[] parts = path.split("/");
		if(parts.length > 2)
		{
			parts[2] = varname;
		}
		StringBuilder builder = new StringBuilder();
		for(String part : parts)
		{
			if(part.length() > 0)
			{
				part = "^" + part + "$";
			}
			builder.append(part).append("/");
		}
		return builder.toString();
	}

	private String formatTimeWindowAsWaterYear(TimeWindow tw)
	{
		SubTimeFormat yearFormat = new SubTimeFormat("yyyy");
		return tw.getStartTime().__add__("3MON").format(yearFormat) + "-"
				+ tw.getEndTime().__add__("3MON").format(yearFormat);
	}

	private String getExceedancePlotTitle(PathnameMap pathMap)
	{
		String title = "Exceedance " + pathMap._varName.replace("\"", "");
		if("S_SEPT".equals(pathMap._varCategory))
		{
			title = title + " (Sept)";
		}
		return title;
	}

	private String getUnitsForReference(DataReference ref)
	{
		if(ref != null)
		{
			return ref.getData().getAttributes().getYUnits();
		}
		return "";
	}

	private String getUnits(DataReference ref1, DataReference ref2)
	{
		if(ref1 == null)
		{
			if(ref2 == null)
			{
				return "";
			}
			else
			{
				return getUnitsForReference(ref2);
			}
		}
		else
		{
			return getUnitsForReference(ref1);
		}
	}

	/**
	 * Clear {@link ProgressFrameForPDF} status window message buffer
	 */
	private void clearMessages()
	{
		_messages.setLength(0);
	}

	/**
	 * Add a message to the {@link ProgressFrameForPDF} status window
	 *
	 * @param msg
	 */
	private void addMessage(String msg)
	{
		_messages.append(msg).append("\n");
	}

	/**
	 * Externalizes the format for output. This allows the flexibility of
	 * defining a writer to output the report to a PDF file vs an HTML file.
	 *
	 * @author psandhu
	 */

	public interface Writer
	{

		int BOLD = 100;
		int NORMAL = 1;

		void startDocument(String outputFile);

		void endDocument();

		void setTableFontSize(String tableFontSize);

		void addTableTitle(String string);

		void addTableHeader(List<String> headerRow, int[] columnSpans);

		void addTableRow(List<String> rowData, int[] columnSpans, int style, boolean centered);

		void endTable();

		void addTimeSeriesPlot(List<double[]> buildDataArray, String title, String[] seriesName, String xAxisLabel,
							   String yAxisLabel);

		void addExceedancePlot(List<double[]> buildDataArray, String title, String[] seriesName, String xAxisLabel,
							   String yAxisLabel);

		void setAuthor(String author);

		void addTableSubTitle(String string);

		void addTitlePage(String compareInfo, String author, String fileBase, String fileAlt);
	}

	private static final class PathnameMap
	{
		private String _reportType;
		private String _pathBase;
		private String _pathAlt;
		private String _rowType;
		private String _units;
		private String _varCategory;
		private String _varName;
		private boolean _plot;

		private PathnameMap(String varName)
		{
			_varName = varName;
		}
	}

}
