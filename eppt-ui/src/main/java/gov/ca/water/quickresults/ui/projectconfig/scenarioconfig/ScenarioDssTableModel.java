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

package gov.ca.water.quickresults.ui.projectconfig.scenarioconfig;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.NamedDssPath;

import hec.heclib.dss.CondensedReference;
import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDss;
import hec.heclib.dss.HecDssCatalog;
import rma.swing.table.RmaTableModel;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-29-2019
 */
class ScenarioDssTableModel extends RmaTableModel
{
	static final int ROW_TYPE_COLUMN = 0;
	static final int DSS_PATH_COLUMN = 1;
	static final int ALIAS_COLUMN = 2;
	static final int A_PART_COLUMN = 3;
	static final int F_PART_COLUMN = 4;
	private static final Logger LOGGER = Logger.getLogger(ScenarioDssTableModel.class.getName());
	private static final int[] COLUMNS = new int[]{ROW_TYPE_COLUMN, DSS_PATH_COLUMN, ALIAS_COLUMN, A_PART_COLUMN, F_PART_COLUMN};
	private final List<Row> _rows = new ArrayList<>();
	private final Map<Path, Set<String>> _fPaths = new ConcurrentHashMap<>();
	private final Map<Path, Set<String>> _aPaths = new ConcurrentHashMap<>();
	private final LoadingDss _loadingDss;
	private final ExecutorService _executor;

	ScenarioDssTableModel(LoadingDss loading)
	{
		_executor = Executors.newFixedThreadPool(10);
		_loadingDss = loading;
		Row dvRowModel = createRowModel(new NamedDssPath(Paths.get("EPPT_DEFAULT.dss"), "", "EPPT_DEFAULT", "1MON", "EPPT_DEFAULT"),
				RowType.DV);
		_rows.add(dvRowModel);
		Row svRowModel = createRowModel(new NamedDssPath(Paths.get("EPPT_DEFAULT.dss"), "", "EPPT_DEFAULT", "1MON", "EPPT_DEFAULT"),
				RowType.SV);
		_rows.add(svRowModel);
		Row ivRowModel = createRowModel(new NamedDssPath(Paths.get("EPPT_DEFAULT.dss"), "", "EPPT_DEFAULT", "1MON", "EPPT_DEFAULT"),
				RowType.INIT);
		_rows.add(ivRowModel);
		Row dtwRowModel = createRowModel(new NamedDssPath(Paths.get("EPPT_DEFAULT.dss"), "", "EPPT_DEFAULT", "1MON", "EPPT_DEFAULT"),
				RowType.QA_QC);
		_rows.add(dtwRowModel);
	}

	void shutdown()
	{
		_executor.shutdownNow();
	}

	void fillModel(EpptDssContainer dssContainer)
	{
		_rows.clear();
		Row dvRowModel = createRowModel(dssContainer.getDvDssFile(), RowType.DV);
		_rows.add(dvRowModel);
		Row svRowModel = createRowModel(dssContainer.getSvDssFile(), RowType.SV);
		_rows.add(svRowModel);
		Row ivRowModel = createRowModel(dssContainer.getIvDssFile(), RowType.INIT);
		_rows.add(ivRowModel);
		Row dtwRowModel = createRowModel(dssContainer.getDtsDssFile(), RowType.QA_QC);
		_rows.add(dtwRowModel);
		_rows.addAll(dssContainer.getExtraDssFiles()
								 .stream()
								 .map(dss -> createRowModel(dss, RowType.EXTRA))
								 .collect(toList()));
		List<Path> collect = _rows.stream()
								  .map(r -> r._dssPath)
								  .collect(toList());
		loadDssAandFParts(collect);
	}

	EpptDssContainer createDssContainer(String scenarioName, Path outputPath)
	{
		NamedDssPath dvDssFile = createNamedDssPath(getRowForType(RowType.DV), scenarioName, outputPath);
		NamedDssPath svDssFile = createNamedDssPath(getRowForType(RowType.SV), scenarioName, outputPath);
		NamedDssPath ivDssFile = createNamedDssPath(getRowForType(RowType.INIT), scenarioName, outputPath);
		NamedDssPath dtsDssFile = createNamedDssPath(getRowForType(RowType.QA_QC), scenarioName, outputPath);
		List<NamedDssPath> extraDssFiles = getExtraRows().stream().map((Row row) -> createNamedDssPath(row, scenarioName, outputPath)).collect(
				toList());
		return new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, dtsDssFile, extraDssFiles);
	}

	private List<Row> getExtraRows()
	{
		List<Row> retval = new ArrayList<>();
		for(Row row : _rows)
		{
			if(row._rowType == RowType.EXTRA)
			{
				retval.add(row);
			}
		}
		return retval;
	}

	private Row getRowForType(RowType rowType)
	{
		Row retval = null;
		for(Row row : _rows)
		{
			if(row._rowType == rowType)
			{
				retval = row;
				break;
			}
		}
		return retval;
	}

	private NamedDssPath createNamedDssPath(Row row, String scenarioName, Path outputPath)
	{
		NamedDssPath retval = null;
		if(row != null)
		{
			String alias = scenarioName;
			if(row._alias != null && !row._alias.isEmpty())
			{
				alias = row._alias;
			}
			Path dssPath = row._dssPath;
			if(dssPath != null && !dssPath.toString().isEmpty() && !dssPath.toFile().isDirectory())
			{
				if(!dssPath.isAbsolute())
				{
					if(outputPath.toString().isEmpty())
					{
						outputPath = EpptPreferences.getLastProjectConfiguration().getParent();
					}
					dssPath = outputPath.resolve(dssPath);
				}
				if(!dssPath.toString().toLowerCase().endsWith(".dss"))
				{
					dssPath = Paths.get(dssPath.toString() + ".dss");
				}
				retval = new NamedDssPath(dssPath, alias, row._aPart, Row.E_PART, row._fPart);
			}
		}
		return retval;
	}

	private Row createRowModel(NamedDssPath dvDssFile, RowType rowType)
	{
		if(dvDssFile != null)
		{
			return new Row(rowType, dvDssFile.getDssPath(), dvDssFile.getAliasName(), dvDssFile.getAPart(), dvDssFile.getFPart());
		}
		else
		{
			return new Row(rowType, Paths.get(""), "", "", "");
		}
	}

	@Override
	public int getRowCount()
	{
		return _rows.size();
	}

	@Override
	public Class getColumnClass(int column)
	{
		return String.class;
	}

	@Override
	public int getColumnCount()
	{
		return COLUMNS.length;
	}

	@Override
	public void setValueAt(Object value, int row, int column)
	{
		Row rowModel = _rows.get(row);
		switch(column)
		{
			case DSS_PATH_COLUMN:
				rowModel._dssPath = (value == null) ? Paths.get("") : Paths.get(value.toString());
				if(value != null)
				{
					loadDssAandFParts(Collections.singletonList(Paths.get(value.toString())));
				}
				break;
			case ALIAS_COLUMN:
				rowModel._alias = (value == null) ? "" : value.toString();
				break;
			case A_PART_COLUMN:
				rowModel._aPart = (value == null) ? "" : value.toString();
				break;
			case F_PART_COLUMN:
				rowModel._fPart = (value == null) ? "" : value.toString();
				break;
		}
		fireTableDataChanged();
	}

	private void loadDssAandFParts(List<Path> paths)
	{
		if(!_executor.isShutdown())
		{
			_executor.submit(() ->
			{
				try
				{
					SwingUtilities.invokeLater(() -> _loadingDss.loadingStart("Loading DSS A and F parts"));
					paths.stream().filter(path -> path != null && path.toFile().exists() && !_aPaths.containsKey(path))
						 .parallel().forEach(this::loadDss);
				}
				finally
				{
					SwingUtilities.invokeLater(_loadingDss::loadingFinished);
				}
			});
		}
	}

	private void loadDss(Path path)
	{
		SwingUtilities.invokeLater(() -> _loadingDss.loadingStart("Loading DSS A and F parts for: " + path));
		HecDssCatalog hecDssCatalog = null;
		try
		{
			hecDssCatalog = new HecDssCatalog();
			hecDssCatalog.setDSSFileName(path.toString(), false);
			Set<String> aParts = _aPaths.computeIfAbsent(path, e -> new HashSet<>());
			Set<String> fParts = _fPaths.computeIfAbsent(path, e -> new HashSet<>());
			IGuiLinksSeedDataSvc seedDataSvcImplInstance = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance();
			List<GUILinksAllModelsBO> guiLinks = getDefaultGuiLinks(seedDataSvcImplInstance);
			iterateDefaultGuiLinks(hecDssCatalog, guiLinks, aParts, fParts);
			if(aParts.isEmpty() || fParts.isEmpty())
			{
				iterateAllDssPaths(path);
			}
		}
		catch(RuntimeException e)
		{
			LOGGER.log(Level.WARNING, "Unable to open DSS file: " + path, e);
		}
		finally
		{
			if(hecDssCatalog != null)
			{
				hecDssCatalog.closeDSSFile();
			}
		}
	}

	private void iterateDefaultGuiLinks(HecDssCatalog hecDssCatalog, List<GUILinksAllModelsBO> guiLinks, Set<String> aParts, Set<String> fParts)
	{
		for(GUILinksAllModelsBO link : guiLinks)
		{
			Map<GUILinksAllModelsBO.Model, String> primary = link.getPrimary();
			for(String bAndCParts : primary.values())
			{
				String pathWithWildcards = "/*/" + bAndCParts + "/*/*/*";
				String[] catalog = hecDssCatalog.getCatalog(false, pathWithWildcards);
				for(Object catPath : catalog)
				{
					if(catPath != null)
					{
						DSSPathname dssPathname = new DSSPathname(catPath.toString());
						aParts.add(dssPathname.getAPart());
						fParts.add(dssPathname.getFPart());
					}
				}
				if(catalog.length > 0)
				{
					break;
				}
			}
		}
	}

	private void iterateAllDssPaths(Path path)
	{

		HecDss hecDss = null;
		try
		{

			hecDss = HecDss.open(path.toString(), true, false);
			Set<String> aParts = _aPaths.computeIfAbsent(path, e -> new HashSet<>());
			Set<String> fParts = _fPaths.computeIfAbsent(path, e -> new HashSet<>());
			List<CondensedReference> condensedReferences = hecDss.getCondensedCatalog();
			for(CondensedReference condensedReference : condensedReferences)
			{
				DSSPathname firstPathname = new DSSPathname(condensedReference.getFirstPathname());
				if(Thread.currentThread().isInterrupted())
				{
					break;
				}
				aParts.add(firstPathname.getAPart());
				fParts.add(firstPathname.getFPart());
			}
		}
		catch(Exception e)
		{
			LOGGER.log(Level.WARNING, "Unable to open DSS file: " + path, e);
		}
		finally
		{
			if(hecDss != null)
			{
				hecDss.close();
			}
		}
	}

	private List<GUILinksAllModelsBO> getDefaultGuiLinks(IGuiLinksSeedDataSvc seedDataSvcImplInstance)
	{
		List<GUILinksAllModelsBO> guiLinks = new ArrayList<>();
		GUILinksAllModelsBO guiLink310 = seedDataSvcImplInstance.getGuiLink("310");
		guiLinks.add(guiLink310);
		GUILinksAllModelsBO guiLink311 = seedDataSvcImplInstance.getGuiLink("311");
		guiLinks.add(guiLink311);
		GUILinksAllModelsBO guiLink901 = seedDataSvcImplInstance.getGuiLink("901");
		guiLinks.add(guiLink901);
		return guiLinks;
	}

	@Override
	public Object getValueAt(int row, int column)
	{
		Object retval = null;
		Row rowModel = _rows.get(row);
		switch(column)
		{
			case ROW_TYPE_COLUMN:
				retval = rowModel._rowType;
				break;
			case DSS_PATH_COLUMN:
				retval = rowModel._dssPath;
				break;
			case ALIAS_COLUMN:
				retval = rowModel._alias;
				break;
			case A_PART_COLUMN:
				retval = rowModel._aPart;
				break;
			case F_PART_COLUMN:
				retval = rowModel._fPart;
				break;
		}
		return retval;
	}

	void addExtraDss()
	{
		_rows.add(new Row(RowType.EXTRA, Paths.get(""), "", "", ""));
		fireTableDataChanged();
	}

	@Override
	public String getColumnName(int column)
	{
		String retval = "";
		switch(column)
		{
			case ROW_TYPE_COLUMN:
				retval = "";
				break;
			case DSS_PATH_COLUMN:
				retval = "DSS Path";
				break;
			case ALIAS_COLUMN:
				retval = "Alias Name";
				break;
			case A_PART_COLUMN:
				retval = "DSS A Part";
				break;
			case F_PART_COLUMN:
				retval = "DSS F Part";
				break;
		}
		return retval;
	}

	void removeExtraDss(int row)
	{
		Row rowModel = _rows.get(row);
		if(rowModel._rowType == RowType.EXTRA)
		{
			_rows.remove(rowModel);
			fireTableDataChanged();
		}
	}

	RowType getRowType(int row)
	{
		return _rows.get(row)._rowType;
	}

	@Override
	public boolean isCellEditable(int row, int column)
	{
		return column != ROW_TYPE_COLUMN;
	}

	Set<String> getAPartsForRow(int row)
	{
		Row rowModel = _rows.get(row);
		return _aPaths.getOrDefault(rowModel._dssPath, new HashSet<>());
	}

	Set<String> getFPartsForRow(int row)
	{
		Row rowModel = _rows.get(row);
		return _fPaths.getOrDefault(rowModel._dssPath, new HashSet<>());
	}

	enum RowType
	{
		DV("DV"), SV("SV"), INIT("INIT"), QA_QC("QA_QC"), EXTRA("Extra");
		private final String _render;

		RowType(String render)
		{
			_render = render;
		}

		@Override
		public String toString()
		{
			return _render;
		}
	}

	private static class Row
	{
		private static final String E_PART = "1MON";
		private final RowType _rowType;
		private Path _dssPath;
		private String _alias;
		private String _aPart;
		private String _fPart;

		Row(RowType rowType, Path dssPath, String alias, String aPart, String fPart)
		{
			_rowType = rowType;
			_dssPath = dssPath;
			_alias = alias;
			_aPart = aPart;
			_fPart = fPart;
		}
	}
}
