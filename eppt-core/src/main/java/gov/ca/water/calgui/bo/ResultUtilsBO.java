/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;

//! Utilities for display of results

import java.awt.Component;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import calsim.app.Project;
import calsim.gui.DtsTreeModel;
import calsim.gui.DtsTreePanel;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * Supporting utilities for display of results
 *
 * @author tslawecki
 */
public class ResultUtilsBO implements ChangeListener
{
	private static final Logger LOG = Logger.getLogger(ResultUtilsBO.class.getName());
	private static ResultUtilsBO resultUtilsBO;
	private IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();
	private HashMap<String, Integer> _monthMap;
	private SwingEngine _swingEngine;
	private Project _project;
	private FileDialogBO fdDSSFiles;

	/**
	 * Constructor stores SwiXml instance, builds month-to-integer map, and sets
	 * up a WRIMS GUI Project object for use in Custom Results
	 *
	 * @param swingEngine
	 */
	private ResultUtilsBO(SwingEngine swingEngine)
	{
		this._swingEngine = swingEngine;

		// Build map for mmm -> m mapping

		_monthMap = new HashMap<>();
		_monthMap.put("jan", 1);
		_monthMap.put("feb", 2);
		_monthMap.put("mar", 3);
		_monthMap.put("apr", 4);
		_monthMap.put("may", 5);
		_monthMap.put("jun", 6);
		_monthMap.put("jul", 7);
		_monthMap.put("aug", 8);
		_monthMap.put("sep", 9);
		_monthMap.put("oct", 10);
		_monthMap.put("nov", 11);
		_monthMap.put("dec", 12);

		// Create a WRIMS GUI project for WRIMS GUI to work off of

		_project = new Project();
		AppUtils.setCurrentProject(_project);
		AppUtils.baseOn = false;
	}

	/**
	 * This method is for implementing the singleton.
	 *
	 * @return
	 */
	public static ResultUtilsBO getResultUtilsInstance(SwingEngine swingEngine)
	{
		if(resultUtilsBO == null)
		{
			resultUtilsBO = new ResultUtilsBO(swingEngine);
		}
		return resultUtilsBO;
	}

	/**
	 * Sets up a spinner for a numeric range
	 *
	 * @param jspn           - Swing spinner component
	 * @param val            - Initial value
	 * @param min            - Minimum value
	 * @param max            - Maximum value
	 * @param step           - Increment between values
	 * @param format         - Format for display
	 * @param obj            - ChangeListener
	 * @param changelistener - True is a ChangeListener is to be assigned
	 */
	public static void SetNumberModelAndIndex(JSpinner jspn, int val, int min, int max, int step, String format,
											  Object obj, boolean changelistener)
	{

		SpinnerModel spnmod = new SpinnerNumberModel(val, min, max, step);
		jspn.setModel(spnmod);
		jspn.setEditor(new JSpinner.NumberEditor(jspn, format));
		if(changelistener)
		{
			jspn.addChangeListener((ChangeListener) obj);
		}
	}

	/**
	 * Sets up a spinner for months Jan - Dec
	 *
	 * @param jspn           - Swing spinner component
	 * @param idx
	 * @param obj            - ChangeListener
	 * @param changelistener - True is a ChangeListener is to be assigned
	 */
	public static void SetMonthModelAndIndex(JSpinner jspn, int idx, Object obj, boolean changelistener)
	{
		String[] monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

		try
		{
			SpinnerListModel monthModel = new SpinnerListModel(monthNames);
			jspn.setModel(monthModel);
			jspn.setValue(monthNames[idx]);
			if(changelistener == true)
			{
				jspn.addChangeListener((ChangeListener) obj);
			}
		}

		catch(Exception e)
		{
			LOG.debug("Problem reading table files. " + e);
		}
	}

	/**
	 * Reads QuickResults output list, Custom Results Dts Tree
	 */
	public Optional<List<String>> readCGR()
	{
		String aLine;
		List<String> data = null;
		JFileChooser fc = new JFileChooser();
		fc.setFileFilter(new SimpleFileFilter("cgr", "CalLite GUI Report File (*.cgr)"));
		fc.setCurrentDirectory(new File(".//Config"));
		File file;
		String filename;
		int retval = fc.showOpenDialog(_swingEngine.find(Constant.MAIN_FRAME_NAME));
		if(retval == JFileChooser.APPROVE_OPTION)
		{
			data = new ArrayList<>();
			// ... The user selected a file, get it, use it.
			file = fc.getSelectedFile();
			filename = file.toString();
			try(FileInputStream fin = new FileInputStream(filename);
				BufferedReader br = new BufferedReader(new InputStreamReader(fin)))
			{
				aLine = br.readLine();
				while((aLine != null) && !aLine.startsWith("===== Dts Tree ====="))
				{
					data.add(aLine);
					aLine = br.readLine();
				}
				if(aLine != null)
				{
					DtsTreePanel.getCurrentModel().readData(filename + ".tree.xml", "");
					Vector<MultipleTimeSeries> mts = DtsTreeModel.getPrjMts();
					Vector<DerivedTimeSeries> dts = DtsTreeModel.getPrjDts();
					Project p = getProject();
					p.clearMTSList();
					for(MultipleTimeSeries mt : mts)
					{
						p.add(mt);
					}
					p.clearDTSList();
					for(DerivedTimeSeries dt : dts)
					{
						p.add(dt);
					}
				}
			}
			catch(Exception e1)
			{
				LOG.debug(e1.getMessage());
			}
		}
		return Optional.ofNullable(data);
	}

	/**
	 * Writes Quick Results display list, Custom Result DTS tree
	 */
	public void writeCGR()
	{
		JFileChooser fc = new JFileChooser();
		fc.setFileFilter(new SimpleFileFilter("cgr", "CalLite Report File (*.cgr)"));
		fc.setCurrentDirectory(new File(".//Config"));
		File file = null;
		String filename = null;
		int retval = fc.showSaveDialog(_swingEngine.find(Constant.MAIN_FRAME_NAME));
		if(retval == JFileChooser.APPROVE_OPTION)
		{
			// ... The user selected a file, get it, use it.
			file = fc.getSelectedFile();
			filename = file.toString();
			if(!filename.toUpperCase().endsWith(".CGR") && !filename.endsWith("."))
			{
				filename = filename + ".cgr";
			}
			boolean saveFlag = true;
			if(new File(filename).exists())
			{
				saveFlag = "OK".equals(_dialogSvc
						.getOKCancel("The display list file '" + filename + "' already exists. Press OK to overwrite.",
								JOptionPane.QUESTION_MESSAGE));
			}
			if(saveFlag)
			{
				try(OutputStream outputStream = new FileOutputStream(filename))
				{
					// Store previous list items
					JList lstReports = (JList) (_swingEngine).find("lstReports");
					int size = lstReports.getModel().getSize(); // 4
					int n;
					n = 0;
					String[] lstArray = new String[size];
					for(int i = 0; i < size; i++)
					{
						Object item = lstReports.getModel().getElementAt(i);
						if(!item.toString().equals(" "))
						{
							lstArray[n] = item.toString();
							n = n + 1;
						}
					}
					// Store contents of Project
					List<String> pList = new ArrayList<>();
					pList.add("===== Dts Tree =====");
					Project p = getProject();
					pList.add(String.valueOf(p.getNumberOfMTS()));
					for(int i = 0; i < p.getNumberOfMTS(); i++)
					{
						MultipleTimeSeries mts = p.getMTSList()[i];
						pList.add(mts.getName());
						pList.add(String.valueOf(mts.getNumberOfDataReferences()));
						for(int j = 0; j < mts.getNumberOfDataReferences(); j++)
						{
							pList.add(mts.getBPartAt(j) + ";" + mts.getCPartAt(j) + ";" + mts.getVarTypeAt(j) + ";"
									+ mts.getDTSNameAt(i));
						}
					}
					pList.add(String.valueOf(p.getNumberOfDTS()));
					for(int i = 0; i < p.getNumberOfDTS(); i++)
					{
						DerivedTimeSeries dts = p.getDTSList()[i];
						pList.add(dts.getName());
						pList.add(String.valueOf(dts.getNumberOfDataReferences()));
						for(int j = 0; j < dts.getNumberOfDataReferences(); j++)
						{
							pList.add(dts.getBPartAt(j) + ";" + dts.getCPartAt(j) + ";" + dts.getVarTypeAt(j) + ";"
									+ dts.getOperationIdAt(j) + ";" + dts.getDTSNameAt(j));
						}
					}
					try(PrintStream output = new PrintStream(outputStream))
					{
						for(int i = 0; i < n; i++)
						{
							output.println(lstArray[i]);
						}
						for(String s : pList)
						{
							output.println(s);
						}
						DtsTreePanel.getCurrentModel().saveFile(filename + ".tree.xml");
					}
					catch(IOException ex)
					{
						LOG.debug(ex.getMessage());
					}
				}
				catch(IOException e2)
				{
					LOG.debug(e2.getMessage());
					return;
				}

			}
		}
	}

	/**
	 * Getter access to WRIMS GUI project for Custom Results
	 *
	 * @return
	 */
	public Project getProject()
	{
		return _project;
	}

	/**
	 * Convert three-letter month abbreviation to integer 1-12
	 */
	public int monthToInt(String month)
	{
		month = month.toLowerCase();
		Integer monthCode = null;
		try
		{
			monthCode = _monthMap.get(month);
		}
		catch(Exception e)
		{
			LOG.debug(e.getMessage());
		}
		if(monthCode == null)
		{
			LOG.debug("Invalid Key at UnitsUtils.monthToInt");
			return -1;
		}
		return monthCode.intValue();
	}

	@Override
	/**
	 * Custom ChangeListener constrains time spinners to WY 1922 - WY 2003
	 */
	public void stateChanged(ChangeEvent changeEvent)
	{
		Component c = (Component) changeEvent.getSource();
		String lcName = c.getName().toLowerCase();
		if("spn".equals(lcName.substring(0, 3)))
		{
			// Constrain run times to [10/1921,9/2003]
			int syr = (Integer) ((JSpinner) _swingEngine.find("spnRunStartYear")).getValue();
			int eyr = (Integer) ((JSpinner) _swingEngine.find("spnRunEndYear")).getValue();
			int smo = monthToInt(((String) ((JSpinner) _swingEngine.find("spnRunStartMonth")).getValue()).trim());
			int emo = monthToInt(((String) ((JSpinner) _swingEngine.find("spnRunEndMonth")).getValue()).trim());
			if((syr == 1921) && (smo < 10))
			{
				((JSpinner) _swingEngine.find("spnRunStartMonth")).setValue("Oct");
			}
			if((eyr == 2003) && (emo > 9))
			{
				((JSpinner) _swingEngine.find("spnRunEndMonth")).setValue("Sep");
			}
			// Constrain display times the same way [inefficient?]
			syr = (Integer) ((JSpinner) _swingEngine.find("spnStartYear")).getValue();
			eyr = (Integer) ((JSpinner) _swingEngine.find("spnEndYear")).getValue();
			smo = monthToInt(((String) ((JSpinner) _swingEngine.find("spnStartMonth")).getValue()).trim());
			emo = monthToInt(((String) ((JSpinner) _swingEngine.find("spnEndMonth")).getValue()).trim());
			if((syr == 1921) && (smo < 10))
			{
				((JSpinner) _swingEngine.find("spnStartMonth")).setValue("Oct");
			}
			if((eyr == 2003) && (emo > 9))
			{
				((JSpinner) _swingEngine.find("spnEndMonth")).setValue("Sep");
			}
		}
		else if("tabbedpane1".equals(lcName))
		{
			JMenuBar menuBar = (JMenuBar) this._swingEngine.find("menu");
			menuBar.setSize(150, 20);
		}
	}

	/**
	 * Store the custom file dialog containing the Quick Results scenario list
	 *
	 * @param fdDSSFiles
	 */
	public void setFdDSSFiles(FileDialogBO fdDSSFiles)
	{
		this.fdDSSFiles = fdDSSFiles;
	}
}
