/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.swing.*;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.bus_delegate.IAllButtonsDele;
import gov.ca.water.calgui.bus_delegate.impl.AllButtonsDeleImp;
import gov.ca.water.calgui.bus_service.IDynamicControlSvc;
import gov.ca.water.calgui.bus_service.IMonitorSvc;
import gov.ca.water.calgui.bus_service.IScenarioSvc;
import gov.ca.water.calgui.bus_service.ITableSvc;
import gov.ca.water.calgui.bus_service.impl.DynamicControlSvcImpl;
import gov.ca.water.calgui.bus_service.impl.MonitorSvcImpl;
import gov.ca.water.calgui.bus_service.impl.ScenarioSvcImpl;
import gov.ca.water.calgui.bus_service.impl.TableSvcImpl;
import gov.ca.water.calgui.bus_service.impl.XMLParsingSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * This frame is used for displaying updated status during long-duration
 * activities like saving scenarios and running CalLite.
 *
 * @author Mohan
 */
public final class ProgressFrame extends JFrame implements ActionListener
{

	private static final Logger LOG = Logger.getLogger(ProgressFrame.class.getName());
	private static final long serialVersionUID = -606008444073979623L;
	private static ProgressFrame progressFrame;
	Properties properties = new Properties();
	private JList<String> list;
	private JScrollPane listScroller;
	private Map<String, String> scenarioNamesAndAction;
	private IMonitorSvc monitorSvc = new MonitorSvcImpl();
	private ITableSvc tableSvc = TableSvcImpl.getTableSvcImplInstance();
	private IScenarioSvc scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();
	private IAllButtonsDele allButtonsDele = new AllButtonsDeleImp();
	private SwingEngine swingEngine = XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine();
	private IDynamicControlSvc dynamicControlSvc = DynamicControlSvcImpl.getDynamicControlSvcImplInstance();
	private JButton btnClose;
	private SwingWorker<Void, String> workerScenarioMonitor = new SwingWorker<Void, String>()
	{
		private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

		private String[] oldValue;

		@Override
		protected Void doInBackground() throws InterruptedException
		{
			try
			{
				while(true)
				{
					if(isCancelled())
					{
						return null;
					}
					Thread.sleep(2000);
					boolean sleepAfterDisplay = false;
					String[] listData = null;
					List<String> data = new ArrayList<String>();
					List<String> scenariosToDrop = new ArrayList<String>();
					String text = "";
					if(scenarioNamesAndAction.isEmpty())
					{
						listData = new String[1];
						listData[0] = "No active scenarios";
						setBtnText(Constant.STATUS_BTN_TEXT_CLOSE);
					}
					else
					{

						for(String scenarioName : scenarioNamesAndAction.keySet())
						{
							switch(scenarioNamesAndAction.get(scenarioName))
							{
								case Constant.SAVE:
									text = monitorSvc.save(scenarioName);
									data.add(text);
									if(text.endsWith("Save is completed."))
									{
										sleepAfterDisplay = true;
										scenariosToDrop.add(scenarioName);
									}
									break;

								case Constant.BATCH_RUN:
									text = monitorSvc.runModel(scenarioName);
									data.add(text);
									if(text.toLowerCase().contains("done - run c".toLowerCase()))
									{
										LOG.info(text);
										// ResultUtilsBO.getResultUtilsInstance(null).getFdDSSFiles()
										// .addFileToList(new
										// File(Constant.SCENARIOS_DIR +
										// scenarioName + "_DV.DSS"));
										sleepAfterDisplay = true;
										scenariosToDrop.add(scenarioName);
									}
									break;

								case Constant.BATCH_RUN_WSIDI:
									text = monitorSvc.runWSIDI(scenarioName);
									data.add(text);
									if(text.toLowerCase()
										   .endsWith("(wsidi iteration " + properties.getProperty(
												   "wsidi.iterations") + "/"
												   + properties.getProperty("wsidi.iterations")
												   + ")  - DONE - run completed".toLowerCase()))
									{
										sleepAfterDisplay = true;
										loadGeneratedWSIDI(scenarioName);
										scenariosToDrop.add(scenarioName);
									}
									break;
							}
						}
						for(String s : scenariosToDrop)
						{
							scenarioNamesAndAction.remove(s);
						}

						listData = new String[data.size()];
						for(int i = 0; i < data.size(); i++)
						{
							listData[i] = data.get(i);
						}
					}
					if(!Arrays.equals(oldValue, listData))
					{
						setList(listData);
						oldValue = listData;
					}
					if(sleepAfterDisplay)
					{
						Thread.sleep(2000);
						sleepAfterDisplay = false;
					}
				}
			}
			catch(Exception e)
			{
				if(!(e instanceof InterruptedException))
				{
					LOG.error(e.getMessage());
					String messageText = "Unable to display progress frame.";
					errorHandlingSvc.businessErrorHandler(messageText,
							(JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
				}
			}
			return null;
		}

		@Override
		protected void done()
		{
			return;

		}
	};

	/**
	 * This will prepare the Dialog box to show.
	 *
	 * @param title The title of the frame.
	 */
	private ProgressFrame(String title)
	{
		IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
		try
		{
			this.scenarioNamesAndAction = new HashMap<String, String>();
			setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			setPreferredSize(new Dimension(400, 210));
			setMinimumSize(new Dimension(400, 210));
			setLayout(new BorderLayout(5, 5));
			setTitle(title);
			String[] start = {"No scenarios active"};
			list = new JList<String>(start);

			list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			list.setLayoutOrientation(JList.VERTICAL);
			list.setVisibleRowCount(-1);
			list.setDragEnabled(true);
			list.setVisible(true);
			listScroller = new JScrollPane(list);
			listScroller.setPreferredSize(new Dimension(350, 150));
			listScroller.setMinimumSize(new Dimension(350, 150));
			listScroller.setVisible(true);
			add(BorderLayout.PAGE_START, listScroller);
			btnClose = new JButton(Constant.STATUS_BTN_TEXT_CLOSE);
			btnClose.setPreferredSize(new Dimension(50, 20));
			btnClose.setMinimumSize(new Dimension(50, 20));
			btnClose.addActionListener(this);
			btnClose.setActionCommand("Stop");
			btnClose.setVisible(true);
			add(BorderLayout.PAGE_END, btnClose);
			pack();
			Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
			setLocation((dim.width - 400) / 2, (dim.height - 200) / 2);
			java.net.URL imgURL = getClass().getResource("/images/CalLiteIcon.png");
			setIconImage(Toolkit.getDefaultToolkit().getImage(imgURL));
			addWindowListener(new java.awt.event.WindowAdapter()
			{
				@Override
				public void windowClosed(java.awt.event.WindowEvent windowEvent)
				{
					workerScenarioMonitor.cancel(true);
					progressFrame = null;
				}
			});
			try
			{
				properties.load(
						GlobalActionListener.class.getClassLoader().getResourceAsStream("callite-gui.properties"));
			}
			catch(Exception e)
			{
				LOG.debug("Problem loading properties. " + e.getMessage());
			}

			setVisible(true);
			list.invalidate();
			repaint();
			workerScenarioMonitor.execute();
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display progress frame.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static ProgressFrame getProgressFrameInstance()
	{
		if(progressFrame == null)
		{
			progressFrame = new ProgressFrame("");
		}
		return progressFrame;
	}

	/**
	 * Sets the text for the button at the botom of the dialog
	 *
	 * @param text
	 */
	public void setBtnText(String text)
	{
		btnClose.setText(text);
	}

	/**
	 * This method will set the monitor window to visible.
	 */
	public void makeDialogVisible()
	{

		// this.getRootPane().setWindowDecorationStyle(JRootPane.FRAME);
		setVisible(true);
		list.invalidate();
		repaint();
		// paintComponents(this.getGraphics());
		// print(this.getGraphics());
		// paintAll(getGraphics());
	}

	/**
	 * This will set the listData to the monitor window.
	 *
	 * @param listData The list of values to be displayed.
	 */
	public void setList(String[] listData)
	{
		if(!listScroller.isVisible())
		{
			listScroller.setVisible(true);
		}
		list.setListData(listData);
		repaint();
		// paintComponents(this.getGraphics());
		// print(this.getGraphics());
		// paintAll(getGraphics());
	}

	/**
	 * This method will add the key and type to the monitor list.
	 *
	 * @param key  The name of which we should monitor.
	 * @param type which type.
	 */
	public void addScenarioNamesAndAction(String key, String type)
	{
		scenarioNamesAndAction.put(key, type);
		if(listScroller.isVisible())
		{
			invalidate();
		}
	}

	/**
	 * This method will add the list of key and type to the monitor list.
	 *
	 * @param keys The names of which we should monitor.
	 * @param type which type like save or batch run.
	 */
	public void addScenarioNamesAndAction(List<String> keys, String type)
	{
		keys.forEach(key -> scenarioNamesAndAction.put(key, type));

		if(listScroller.isVisible())
		{
			invalidate();
		}
	}

	@Override
	public void actionPerformed(ActionEvent ae)
	{
		if("Go".equals(ae.getActionCommand()))
		{
			this.setVisible(true);
		}
		else if("Stop".equals(ae.getActionCommand()))
		{
			if(btnClose.getText().equals(Constant.STATUS_BTN_TEXT_CLOSE))
			{
				this.setVisible(false);
			}
			else
			{
				Runtime rt = Runtime.getRuntime();
				Process proc;
				try
				{
					proc = rt.exec("taskkill /f /t /fi \"WINDOWTITLE eq CalLiteRun*\" ");
					scenarioNamesAndAction.clear();
					setBtnText(Constant.STATUS_BTN_TEXT_CLOSE);
				}
				catch(IOException ex)
				{
					LOG.error(ex);
				}
			}
		}
	}

	/**
	 * After the WSIDI batch runs this method is called to load the tables in
	 * the "Operations" tab.
	 *
	 * @param scenarioName The scenario file name which the batch program is run. the
	 *                     name should not have any extension.
	 */
	public void loadGeneratedWSIDI(String scenarioName)
	{
		String wsiDiSwpPath = Paths.get(Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR + Constant.LOOKUP_DIR
				+ Constant.SWP_START_FILENAME + Constant.TABLE_EXT).toString();
		String wsiDiCvpSwpPath = Paths.get(Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR
				+ Constant.LOOKUP_DIR + Constant.CVP_START_FILENAME + Constant.TABLE_EXT).toString();
		try
		{
			DataTableModel swpDtm = tableSvc.getWsiDiTable(wsiDiSwpPath);
			swpDtm.setCellEditable(true);
			swpDtm.setTableName(Constant.SWP_START_FILENAME);
			swpDtm.setSwingEngine(swingEngine);
			DataTableModel cvpDtm = tableSvc.getWsiDiTable(wsiDiCvpSwpPath);
			cvpDtm.setCellEditable(true);
			cvpDtm.setTableName(Constant.CVP_START_FILENAME);
			cvpDtm.setSwingEngine(swingEngine);
			scenarioSvc.addUserDefinedTable(Constant.SWP_START_FILENAME, swpDtm);
			scenarioSvc.addUserDefinedTable(Constant.CVP_START_FILENAME, cvpDtm);
			/*
			 * The following code we are sedtting the SWP and CVP file names as
			 * user defined because the table values we are getting after the
			 * batch run we consider them as user defined.
			 */
			tableSvc.setWsidiForSWPFullFileName(Constant.USER_DEFINED);
			tableSvc.setWsidiForCVPFullFileName(Constant.USER_DEFINED);
			JComponent component = (JComponent) swingEngine.find("op_btn1");
			allButtonsDele.editButtonOnOperations(component);
			List<String> value = dynamicControlSvc.getLabelAndGuiLinks4BOBasedOnTheRadioButtons(swingEngine);
			JLabel jLabel = (JLabel) swingEngine.find("op_WSIDI_Status");
			jLabel.setText(value.get(1) + " (Generated via "
					+ Integer.parseInt(properties.getProperty("wsidi.iterations")) + " iterations)");
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex);
		}
	}
}