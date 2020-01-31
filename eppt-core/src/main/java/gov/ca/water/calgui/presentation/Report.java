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

package gov.ca.water.calgui.presentation;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.*;

import gov.ca.dsm2.input.parser.InputTable;
import gov.ca.dsm2.input.parser.Parser;
import gov.ca.dsm2.input.parser.Tables;
import gov.ca.water.calgui.presentation.display.ReportPDFWriter;
import vista.db.dss.DSSDataReference;
import vista.db.dss.DSSGroup;
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
	private final List<String> _messages = new ArrayList<>();
	private ProgressFrameForPDF _progressFrameForPDF;
	private InputStream _inputStream;

	/*
	 * ********* START SwingWorker additions
	 */
	private String _outputFilename;
	private List<ArrayList<String>> _twValues;
	private List<PathnameMap> _pathnameMaps;
	private HashMap<String, String> _scalars;
	private Writer _writer;

	public Report(InputStream inputStream, String outputFilename, JFrame mainFrame)
	{
		_mainFrame = mainFrame;
		this._inputStream = inputStream;
		this._outputFilename = outputFilename;
		_progressFrameForPDF = ProgressFrameForPDF.getInstance(_mainFrame, this);
		_progressFrameForPDF.toFront();
		_progressFrameForPDF.setVisible(true);
	}

	@Override
	protected Void doInBackground() throws Exception
	{
		publish("Generating report in background thread.");

		LOG.fine("Parsing input template");
		publish("Parsing input template.");
		parseTemplateFile(_inputStream);

		publish("Processing DSS files.");
		try
		{
			doProcessing();
			publish("Done");
		}
		catch(RuntimeException ex)
		{
			LOG.log(Level.SEVERE, "Error processing report", ex);
			publish("Error running report: " + ex.getMessage());
		}
		catch(ReportGenerationCanceledException ex)
		{
			LOG.log(Level.FINEST, ex.getMessage(), ex);
			publish(ex.getMessage());
		}

		LOG.fine("Done generating report");

		return null;
	}

	@Override
	protected void process(List<String> status)
	{
		_progressFrameForPDF.setList(status.get(status.size() - 1));
	}

	@Override
	protected void done()
	{

		try
		{
			if(!_messages.isEmpty())
			{
				_progressFrameForPDF.setList(_messages);
			}
			_writer.endDocument();
			checkInterrupt();
			File file = Paths.get(_outputFilename).toFile();
			if(file.exists())
			{
				Desktop.getDesktop().open(file);
			}
		}
		catch(IOException | RuntimeException e)
		{
			LOG.log(Level.SEVERE, "Unable to open file: " + _outputFilename, e);
		}
		catch(ReportGenerationCanceledException e)
		{
			publish(e.getMessage());
			LOG.log(Level.FINEST, "Report canceled: " + _outputFilename, e);
		}
	}

	private void parseTemplateFile(InputStream templateFileStream) throws IOException, ReportGenerationCanceledException
	{

		publish("Parsing template file.");

		Parser p = new Parser();
		Tables tables = p.parseModel(templateFileStream);
		// load scalars into a map
		InputTable scalarTable = tables.getTableNamed("SCALAR");
		ArrayList<ArrayList<String>> scalarValues = scalarTable.getValues();
		int nscalars = scalarValues.size();
		_scalars = new HashMap<>();
		for(int i = 0; i < nscalars; i++)
		{
			ArrayList<String> row = scalarTable.getValues().get(i);
			int index = scalarTable.getHeaders().indexOf("NAME");
			String name = row.get(index);
			ArrayList<String> copy = new ArrayList<>(row);
			copy.remove(index);
			String value = copy.stream().collect(Collectors.joining(" "));
			_scalars.put(name, value.replace("\"", ""));
		}
		checkInterrupt();
		// load pathname mapping into a map
		InputTable pathnameMappingTable = tables.getTableNamed("PATHNAME_MAPPING");
		ArrayList<ArrayList<String>> pmapValues = pathnameMappingTable.getValues();
		int nvalues = pmapValues.size();
		_pathnameMaps = new ArrayList<>();
		for(int i = 0; i < nvalues; i++)
		{
			checkInterrupt();
			String varName = pathnameMappingTable.getValue(i, "VARIABLE");
			varName = varName.replace("\"", "");
			PathnameMap pathMap = new PathnameMap(varName);
			pathMap._reportType = pathnameMappingTable.getValue(i, "REPORT_TYPE").toLowerCase();
			pathMap._pathBase = pathnameMappingTable.getValue(i, "PATH_BASE");
			pathMap._pathAlt = pathnameMappingTable.getValue(i, "PATH_ALT");
			pathMap._varCategory = pathnameMappingTable.getValue(i, "VAR_CATEGORY");
			pathMap._rowType = pathnameMappingTable.getValue(i, "ROW_TYPE");
			if((pathMap._pathAlt == null) || (pathMap._pathAlt.length() == 0))
			{
				pathMap._pathAlt = pathMap._pathBase;
			}
			pathMap._plot = "Y".equalsIgnoreCase(pathnameMappingTable.getValue(i, "PLOT"));
			pathMap._units = pathnameMappingTable.getValue(i, "UNIT");
			_pathnameMaps.add(pathMap);
		}
		InputTable timeWindowTable = tables.getTableNamed("TIME_PERIODS");
		_twValues = timeWindowTable.getValues();
	}

	private void checkInterrupt() throws ReportGenerationCanceledException
	{
		if(isCancelled())
		{
			throw new ReportGenerationCanceledException();
		}
	}

	public void doProcessing() throws ReportGenerationCanceledException
	{
		// open files 1 and file 2 and loop over to plot
		publish("Processing template file.");
		Group dssGroupBase = opendss(_scalars.get("FILE_BASE"));
		Group dssGroupAlt = opendss(_scalars.get("FILE_ALT"));
		ArrayList<TimeWindow> timewindows = new ArrayList<>();
		for(ArrayList<String> values : _twValues)
		{
			String v = values.get(1).replace("\"", "");
			timewindows.add(TimeFactory.getInstance().createTimeWindow(v));
		}
		TimeWindow tw = null;
		if(!timewindows.isEmpty())
		{
			tw = timewindows.get(0);
		}
		String outputFile = _scalars.get("OUTFILE");
		_writer = new ReportPDFWriter();
		boolean wasSuccessful = _writer.startDocument(outputFile);
		if(!wasSuccessful)
		{
			return;
		}
		String author = _scalars.get("MODELER").replace("\"", "");
		_writer.addTitlePage(String.format("System Water Balance Report: %s vs %s", _scalars.get("NAME_ALT"),
				_scalars.get("NAME_BASE")), author, _scalars.get("FILE_BASE"), _scalars.get("FILE_ALT"));
		_writer.setAuthor(author);
		if((dssGroupBase == null) || (dssGroupAlt == null))
		{
			String msg = "No data available in either : " + _scalars.get("FILE_BASE") + " or " + _scalars.get(
					"FILE_ALT");
			LOG.log(Level.FINE, msg);
			addMessage(msg);
			return;
		}

		generateSummaryTable();
		int dataIndex = 0;
		generatePlots(dssGroupBase, dssGroupAlt, tw, dataIndex);
		checkInterrupt();
		_writer.endDocument();
		checkInterrupt();
	}

	private void generatePlots(Group dssGroupBase, Group dssGroupAlt, TimeWindow tw, int dataIndex) throws ReportGenerationCanceledException
	{
		for(PathnameMap pathMap : _pathnameMaps)
		{
			checkInterrupt();
			dataIndex = dataIndex + 1;
			publish("Generating plot " + dataIndex + " of " + _pathnameMaps.size() + ".");

			LOG.log(Level.FINE, "Working on index: {0}", dataIndex);
			if((pathMap._pathAlt == null) || (pathMap._pathAlt == ""))
			{
				pathMap._pathAlt = pathMap._pathBase;
			}
			boolean calculateDts = false;
			if("HEADER".equals(pathMap._varCategory))
			{
				LOG.fine("Inserting header");
				continue;
			}
			if(pathMap._reportType.endsWith("_post"))
			{
				calculateDts = true;
			}
			DataReference refBase = getReference(dssGroupBase, pathMap._pathBase, calculateDts);
			DataReference refAlt = getReference(dssGroupAlt, pathMap._pathAlt, calculateDts);
			if((refBase != null) && (refAlt != null))
			{
				checkInterrupt();
				// Switch order from original code to reverse legends ... LimnoTech
				// 20110816
				String[] seriesName = new String[]{_scalars.get("NAME_ALT"), _scalars.get("NAME_BASE")};
				if("CFS2TAF".equals(pathMap._units))
				{
					TSMath.cfs2taf((RegularTimeSeries) refBase.getData());
					TSMath.cfs2taf((RegularTimeSeries) refAlt.getData());
				}
				else if("TAF2CFS".equals(pathMap._units))
				{
					TSMath.taf2cfs((RegularTimeSeries) refBase.getData());
					TSMath.taf2cfs((RegularTimeSeries) refAlt.getData());
				}
				String dataUnits = getUnits(refBase, refAlt);
				String dataType = getType(refBase, refAlt);
				if(pathMap._plot)
				{
					generatePlotForReportType(tw, pathMap, refBase, refAlt, seriesName, dataUnits, dataType);
				}
			}
		}
	}

	private void generatePlotForReportType(TimeWindow tw, PathnameMap pathMap, DataReference refBase, DataReference refAlt,
										   String[] seriesName, String dataUnits, String dataType) throws ReportGenerationCanceledException
	{
		if(pathMap._reportType.startsWith("average"))
		{
			generatePlot(buildDataArray(refAlt, refBase, tw),
					"Average " + pathMap._varName.replace("\"", ""), seriesName,
					dataType + "(" + dataUnits + ")", "Time", TIME_SERIES);
		}
		else if(pathMap._reportType.startsWith("exceedance"))
		{
			generatePlot(buildExceedanceArray(refAlt, refBase, pathMap._varCategory == "S_SEPT", tw),
					getExceedancePlotTitle(pathMap), seriesName, dataType + "(" + dataUnits + ")",
					"Percent at or above", EXCEEDANCE);
		}
		else if(pathMap._reportType.startsWith("avg_excd"))
		{
			generatePlot(buildDataArray(refAlt, refBase, tw),
					"Average " + pathMap._varName.replace("\"", ""), seriesName,
					dataType + "(" + dataUnits + ")", "Time", TIME_SERIES);
			generatePlot(buildExceedanceArray(refAlt, refBase, pathMap._varCategory == "S_SEPT", tw),
					getExceedancePlotTitle(pathMap), seriesName, dataType + "(" + dataUnits + ")",
					"Percent at or above", EXCEEDANCE);
		}
		else if(pathMap._reportType.startsWith("timeseries"))
		{
			generatePlot(buildDataArray(refAlt, refBase, tw),
					"Average " + pathMap._varName.replace("\"", ""), seriesName,
					dataType + "(" + dataUnits + ")", "Time", TIME_SERIES);
		}
		else if("alloc".equals(pathMap._reportType))
		{
			generatePlot(buildExceedanceArray(refAlt, refBase, true, tw),
					"Exceedance " + pathMap._varName.replace("\"", ""), seriesName, "Allocation (%)",
					"Probability", EXCEEDANCE);
		}
	}

	private void generateSummaryTable() throws ReportGenerationCanceledException
	{

		publish("Generating summary table.");

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
		ArrayList<TimeWindow> timewindows = new ArrayList<>();
		for(ArrayList<String> values : _twValues)
		{
			String v = values.get(1).replace("\"", "");
			timewindows.add(TimeFactory.getInstance().createTimeWindow(v));
		}
		ArrayList<String> headerRow = new ArrayList<>();
		headerRow.add("");
		ArrayList<String> headerRow2 = new ArrayList<>();
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
			checkInterrupt();
			dataIndex++;
			publish("Processing dataset " + dataIndex + " of " + _pathnameMaps.size());

			if(!categoryList.contains(pathMap._varCategory))
			{
				continue;
			}
			firstDataRow = processSummaryForPath(dssGroupBase, dssGroupAlt, timewindows, firstDataRow, pathMap);
		}
		_writer.endTable();
	}

	private boolean processSummaryForPath(Group dssGroupBase, Group dssGroupAlt, ArrayList<TimeWindow> timewindows, boolean firstDataRow,
										  PathnameMap pathMap)
	{
		try
		{
			ArrayList<String> rowData = new ArrayList<>();
			rowData.add(pathMap._varName);
			boolean calculateDts = false;
			if(pathMap._reportType.toLowerCase().endsWith("_post"))
			{
				calculateDts = true;
			}
			DataReference refBase = null, refAlt = null;
			if(!"ignore".equalsIgnoreCase(pathMap._pathBase))
			{
				refBase = getReference(dssGroupBase, pathMap._pathBase, calculateDts);
			}
			if(!"ignore".equalsIgnoreCase(pathMap._pathAlt))
			{
				refAlt = getReference(dssGroupAlt, pathMap._pathAlt, calculateDts);
			}
			for(TimeWindow tw : timewindows)
			{
				processSummaryTimeWindow(rowData, refBase, refAlt, tw);
			}
			if("B".equals(pathMap._rowType))
			{
				if(!firstDataRow)
				{
					ArrayList<String> blankRow = new ArrayList<>();
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
		catch(RuntimeException e)
		{
			addMessage(e.getMessage());
			LOG.log(Level.FINE, "Error obtaining dataset.", e);
		}
		return firstDataRow;
	}

	private void processSummaryTimeWindow(ArrayList<String> rowData, DataReference refBase, DataReference refAlt, TimeWindow tw)
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

	private String formatDoubleValue(double val)
	{
		return Double.isNaN(val) ? "" : String.format("%3d", Math.round(val));
	}

	private void generatePlot(List<double[]> buildDataArray, String title, String[] seriesName,
							  String yAxisLabel, String xAxisLabel, String plotType) throws ReportGenerationCanceledException
	{
		checkInterrupt();
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
			LOG.log(Level.FINE, msg);
			addMessage(msg);
		}
	}

	public String getOutputFile()
	{
		return _scalars.get("OUTFILE");
	}

	private ArrayList<double[]> buildDataArray(DataReference ref1, DataReference ref2, TimeWindow tw)
	{
		ArrayList<double[]> dlist = new ArrayList<>();
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

	private Date convertToDate(Time timeVal)
	{
		return new Date(timeVal.getDate().getTime() - TimeZone.getDefault().getRawOffset());
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

	public List<double[]> buildExceedanceArray(DataReference ref1, DataReference ref2, boolean endOfSept,
											   TimeWindow tw)
	{
		ArrayList<Double> x1 = sort(ref1, endOfSept, tw);
		ArrayList<Double> x2 = sort(ref2, endOfSept, tw);
		ArrayList<double[]> darray = new ArrayList<>();
		int i = 0;
		int n = Math.min(x1.size(), x2.size());
		while(i < n)
		{
			darray.add(new double[]{100.0 - 100.0 * i / (n + 1), x1.get(i), x2.get(i)});
			i = i + 1;
		}
		return darray;
	}

	private ArrayList<Double> sort(DataReference ref, boolean endOfSept, TimeWindow tw)
	{
		TimeSeries data = (TimeSeries) ref.getData();
		if(tw != null)
		{
			data = data.createSlice(tw);
		}
		ArrayList<Double> dx = new ArrayList<>();
		ElementFilterIterator iter = new ElementFilterIterator(data.getIterator(), Constants.DEFAULT_FLAG_FILTER);
		while(!iter.atEnd())
		{
			if(endOfSept)
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
		RegularTimeSeries dataTaf = (RegularTimeSeries) TSMath.createCopy(data);
		TSMath.cfs2taf(dataTaf);
		return dataTaf;
	}

	public double avg(RegularTimeSeries data, TimeWindow tw)
	{
		try
		{
			return Stats.avg(data.createSlice(tw)) * 12;
		}
		catch(RuntimeException ex)
		{
			LOG.log(Level.FINE, ex.getMessage(), ex);
			return Double.NaN;
		}
	}

	private DataReference getReference(Group group, String path, boolean calculateDts)
	{
		if(calculateDts)
		{
			return getDtsReference(group, path);
		}
		else
		{
			return getTsReference(group, path);
		}
	}

	private DataReference getTsReference(Group group, String path)
	{
		try
		{
			DataReference[] refs = findpath(group, path);
			if(refs == null || refs.length == 0)
			{
				String filename = ((DSSGroup) group).getFile().toString();
				String msg = "No data found for " + filename + " and " + path;
				addMessage(msg);
				LOG.log(Level.FINE, msg);
				return null;
			}
			else
			{
				DataReference firstRef = refs[0];
				DataReference retval = firstRef;
				if(firstRef != null && refs.length > 1)
				{

					DataReference lastRef = refs[refs.length - 1];
					String serverName = firstRef.getServername();
					String fileName = firstRef.getFilename();
					String firstDPart = firstRef.getPathname().getPart(Pathname.D_PART);
					if(firstDPart.contains("-"))
					{
						firstDPart = firstDPart.split("-")[0];
					}
					String lastDPart = lastRef.getPathname().getPart(Pathname.D_PART);
					if(lastDPart.contains("-"))
					{
						String[] split = lastDPart.split("-");
						lastDPart = split[split.length - 1];
					}
					String newDPart = firstDPart;
					if(!Objects.equals(firstDPart, lastDPart))
					{
						newDPart = firstDPart + " - " + lastDPart;
					}
					Pathname pathname = Pathname.createPathname(new String[]
							{
									firstRef.getPathname().getPart(Pathname.A_PART),
									firstRef.getPathname().getPart(Pathname.B_PART),
									firstRef.getPathname().getPart(Pathname.C_PART),
									newDPart,
									firstRef.getPathname().getPart(Pathname.E_PART),
									firstRef.getPathname().getPart(Pathname.F_PART),
							});
					retval = new DSSDataReference(serverName, fileName, pathname);
				}
				return retval;
			}
		}
		catch(RuntimeException ex)
		{
			String msg = "Exception while trying to retrieve " + path + " from " + group;
			LOG.log(Level.FINE, msg, ex);
			addMessage(msg);
			return null;
		}
	}

	private DataReference getDtsReference(Group group, String path)
	{
		try
		{
			String bpart = path.split("/")[2];
			String[] vars = bpart.split("\\+");
			DataReference ref = null;
			for(String varname : vars)
			{
				checkInterrupt();
				String varPath = createPathFromVarname(path, varname);
				DataReference xref = getReference(group, varPath, false);
				if(xref == null)
				{
					throw new IllegalArgumentException("Aborting calculation of " + path + " due to previous path missing");
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
		catch(RuntimeException | ReportGenerationCanceledException ex)
		{
			addMessage(ex.getMessage());
			LOG.log(Level.FINE, "Error obtaining dataset.", ex);
			return null;
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
	 * @return
	 */
	private DataReference[] findpath(Group g, String path)
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
			if(i != 0)
			{
				if(i >= pa.length)
				{
					break;
				}
				pa[i - 1] = p;
				if(p.length() > 0)
				{
					pa[i - 1] = "^" + pa[i - 1] + "$";
				}
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
	 * Add a message to the {@link ProgressFrameForPDF} status window
	 *
	 * @param msg
	 */
	private void addMessage(String msg)
	{
		_messages.add(msg);
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

		boolean startDocument(String outputFile);

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
