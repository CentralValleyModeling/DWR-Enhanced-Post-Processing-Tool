/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.scenario.presentation;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import calsim.gui.GuiUtils;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.*;
import gov.ca.water.calgui.bus_delegate.IAllButtonsDele;
import gov.ca.water.calgui.bus_delegate.IApplyDynamicConDele;
import gov.ca.water.calgui.bus_delegate.IVerifyControlsDele;
import gov.ca.water.calgui.bus_delegate.impl.AllButtonsDeleImp;
import gov.ca.water.calgui.bus_delegate.impl.ApplyDynamicConDeleImp;
import gov.ca.water.calgui.bus_delegate.impl.VerifyControlsDeleImp;
import gov.ca.water.calgui.bus_service.IScenarioSvc;
import gov.ca.water.calgui.bus_service.ISeedDataSvc;
import gov.ca.water.calgui.bus_service.ITableSvc;
import gov.ca.water.calgui.bus_service.IXMLParsingSvc;
import gov.ca.water.calgui.bus_service.impl.*;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.*;
import gov.ca.water.calgui.tech_service.IAuditSvc;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.AuditSvcImpl;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.jfree.util.Log;
import org.swixml.SwingEngine;
import vista.set.DataReference;
import vista.set.Group;

import javax.swing.*;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.List;

/**
 * This class is for initializing the Application and adding the Action, Item,
 * and Mouse Listeners to the main frame.
 *
 * @author Mohan
 */
public class CalLiteInitClass
{
	private static final Logger LOGGER = Logger.getLogger(CalLiteInitClass.class.getName());
	private  IXMLParsingSvc _xmlParsingSvc;
	private  SwingEngine _swingEngine;
	private final IAuditSvc _auditSvc = AuditSvcImpl.getAuditSvcImplInstance();

	/**
	 * Helper function that scans GUI for a button with the indicated label
	 * starting with a given component. Used in CalLite GUI to find the "Open"
	 * button on the CLG panel and replace the associated action.
	 *
	 * @param comp Starting component. Function recurses through childre.n
	 * @param text Text to match on JButton
	 * @return JButton component with specified label
	 */
	public static Component findFirstButtonMatchingText(Component comp, String text)
	{

		if((comp instanceof JButton) && ((JButton) comp).getText().equals(text))
		{
			return comp;
		}

		if(comp instanceof Container)
		{
			Container container = (Container) comp;
			for(int i = 0; i < container.getComponentCount(); i++)
			{
				Component comp2 = findFirstButtonMatchingText(container.getComponent(i), text);
				if(comp2 != null)
				{
					return comp2;
				}
			}
		}
		return null;
	}

	/**
	 * This method is called to initialize the ui.
	 */
	public void init() throws EpptInitializationException
	{

		IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
		IDialogSvc dialogSvc = DialogSvcImpl.getDialogSvcInstance();
		 _swingEngine = null;
		try
		{
			// ----- Build all the Services.
			 _xmlParsingSvc = XMLParsingSvcImpl.createXMLParsingSvcImplInstance();
			 _swingEngine = _xmlParsingSvc.getSwingEngine();

			init2(errorHandlingSvc, dialogSvc, _swingEngine, _xmlParsingSvc);
		}
		catch(RuntimeException e )
		{
			LOGGER.error("Error initializing CalLite", e);


		}
	}

	private void init2(IErrorHandlingSvc errorHandlingSvc, IDialogSvc dialogSvc, SwingEngine swingEngine, IXMLParsingSvc xmlParsingSvc) throws EpptInitializationException
	{
		try
		{


			ISeedDataSvc seedDataSvc = SeedDataSvcImpl.createSeedDataSvcImplInstance();
			IVerifyControlsDele verifyControlsDele = new VerifyControlsDeleImp();
			verifyControlsDele.verifyTheDataBeforeUI(Constant.SCENARIOS_DIR + Constant.DEFAULT + Constant.CLS_EXT);
			DynamicControlSvcImpl.createDynamicControlSvcImplInstance();
			ITableSvc tableSvc = TableSvcImpl.createTableSvcImplInstance(seedDataSvc.getUserTables());
			IScenarioSvc scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();
			IAllButtonsDele allButtonsDele = new AllButtonsDeleImp();

			// ----- Set up the GUI
			initSpinners();
			initGlobalListeners(allButtonsDele);
			// Load the default cls file.
			scenarioSvc.applyClsFile(Constant.SCENARIOS_DIR + Constant.DEFAULT + Constant.CLS_EXT, swingEngine,
					seedDataSvc.getTableIdMap());
			// check
			checkForNewUserDefinedTables(xmlParsingSvc.getNewUserDefinedTables(), scenarioSvc, tableSvc, swingEngine);
			// we clear because when we 1st load the cls
			_auditSvc.clearAudit();
			// file
			// we should not have any records.
			addJTextFieldListener(xmlParsingSvc.getjTextFieldIds());
			addJCheckBoxListener(xmlParsingSvc.getjCheckBoxIDs());
			// Count threads and update batch run selector appropriately
			initBatchRun();
			// For Result part.
			ResultUtilsBO resultUtilsBO = ResultUtilsBO.getResultUtilsInstance(swingEngine);
			// Setup for Reporting page
			// Set up additional UI elements
			JList<?> lstScenarios = (JList<?>) swingEngine.find("SelectedList");
			JRadioButton rdb1 = (JRadioButton) swingEngine.find("rdbp001");
			JRadioButton rdb2 = (JRadioButton) swingEngine.find("rdbp002");
			FileDialogBO fdDSSFiles = new FileDialogBO(lstScenarios, (JLabel) swingEngine.find("lblBase"), rdb1, rdb2,
					(JButton) swingEngine.find("btnPower"), true);
			resultUtilsBO.setFdDSSFiles(fdDSSFiles);
			lstScenarios.setModel(fdDSSFiles.getLmScenNames());
			lstScenarios.setBorder(new LineBorder(Color.gray, 1));

			JButton btnScenarioDel = (JButton) swingEngine.find("btnDelScenario");
			btnScenarioDel.addActionListener(fdDSSFiles);

			JButton btnClearAll = (JButton) swingEngine.find("btnClearScenario");
			btnClearAll.addActionListener(fdDSSFiles);

			// Set up month spinners on result page
			JSpinner spnSM = (JSpinner) swingEngine.find("spnStartMonth");
			ResultUtilsBO.SetMonthModelAndIndex(spnSM, 9, resultUtilsBO, true);
			JSpinner spnEM = (JSpinner) swingEngine.find("spnEndMonth");
			ResultUtilsBO.SetMonthModelAndIndex(spnEM, 8, resultUtilsBO, true);
			// Set up year spinners
			JSpinner spnSY = (JSpinner) swingEngine.find("spnStartYear");
			ResultUtilsBO.SetNumberModelAndIndex(spnSY, 1921, 1921, 2003, 1, "####", resultUtilsBO, true);
			JSpinner spnEY = (JSpinner) swingEngine.find("spnEndYear");
			ResultUtilsBO.SetNumberModelAndIndex(spnEY, 2003, 1921, 2003, 1, "####", resultUtilsBO, true);
			// Set up report list
			JList<?> lstReports = (JList<?>) swingEngine.find("lstReports");
			lstReports.setBorder(new LineBorder(Color.gray, 1));
			lstReports.setVisible(true);

			// For Custom Results ....

			WRIMSGUILinks.buildWRIMSGUI((JFrame) _swingEngine.find(Constant.MAIN_FRAME_NAME),
					(JPanel) _swingEngine.find("WRIMS"));
			// Replace WRIMS GUI display action with CalLite GUI action
			JButton retrieveBtn = GuiUtils.getCLGPanel().getRetrievePanel().getRetrieveBtn();
			for (ActionListener al : retrieveBtn.getActionListeners())
			{
				retrieveBtn.removeActionListener(al);
			}
			retrieveBtn.addActionListener(arg0 -> retrieve(dialogSvc));
			Component openButtonComponent = findFirstButtonMatchingText(GuiUtils.getCLGPanel(), "Open");
			if (openButtonComponent != null)
			{
				JButton openButton = (JButton) openButtonComponent;
				for (ActionListener al : openButton.getActionListeners())
				{
					openButton.removeActionListener(al);
				}
				openButton.addActionListener(arg0 -> retrieve2(errorHandlingSvc, dialogSvc));
			}
			// For PDF Report ...
			((JButton) swingEngine.find("btnGetTemplateFile"))
					.addActionListener(new FileDialogBO(null, (JTextField) swingEngine.find("tfTemplateFILE"), "inp"));
			((JButton) swingEngine.find("btnGetReportFile1"))
					.addActionListener(new FileDialogBO(null, (JTextField) swingEngine.find("tfReportFILE1")));
			((JButton) swingEngine.find("btnGetReportFile2"))
					.addActionListener(new FileDialogBO(null, (JTextField) swingEngine.find("tfReportFILE2")));
			((JButton) swingEngine.find("btnGetReportFile3"))
					.addActionListener(new FileDialogBO(null, (JTextField) swingEngine.find("tfReportFILE3"), "PDF"));

			// Schematic views

			new SchematicMain((JPanel) swingEngine.find("schematic_holder"),
					"file:///" + System.getProperty("user.dir") + "/Config/callite_merged.svg", swingEngine, 1.19, 0.0,
					0.0, 1.19, -8.0, 5.0);
			new SchematicMain((JPanel) swingEngine.find("schematic_holder2"),
					"file:///" + System.getProperty("user.dir") + "/Config/callite-massbalance_working.svg",
					swingEngine, 1.2, 0, 0.0, 1.2, 21.0, 15.0);

			// Recolor results tabs
			JTabbedPane jTabbedPane = (JTabbedPane) swingEngine.find("tabbedPane1");
			jTabbedPane.setForegroundAt(6, Color.blue);
			jTabbedPane.setForegroundAt(7, Color.blue);
			jTabbedPane.setForegroundAt(8, Color.blue);
			jTabbedPane.setForegroundAt(9, Color.blue);
			jTabbedPane.setBackgroundAt(6, Color.WHITE);
			jTabbedPane.setBackgroundAt(7, Color.WHITE);
			jTabbedPane.setBackgroundAt(8, Color.WHITE);
			jTabbedPane.setBackgroundAt(9, Color.WHITE);
			jTabbedPane.addChangeListener(resultUtilsBO);

			JMenuBar menuBar = (JMenuBar) swingEngine.find("menu");
			((JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME)).setJMenuBar(menuBar);
			menuBar.setVisible(true);
			swingEngine.find(Constant.MAIN_FRAME_NAME).addComponentListener(new ComponentAdapter()
			{
				@Override
				// Kludge to encourage consistent sizing
				public void componentResized(ComponentEvent event)
				{
					int height = event.getComponent().getHeight();
					int width = event.getComponent().getWidth();
					swingEngine.find("schematic_holder")
							.setPreferredSize(new Dimension(width - 50, height - 200));
					SwingUtilities.updateComponentTreeUI(event.getComponent());
				}
			});
			new ApplyDynamicConDeleImp().applyDynamicControlForListFromFile();

			// Display the GUI
			swingEngine.find(Constant.MAIN_FRAME_NAME).setVisible(true);
		}
		catch(RuntimeException e)
		{
			LOGGER.error(e.getMessage());
			String messageText = "Unable to initialize GUI.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME),
					e);
		}
	}

	private void initBatchRun()
	{
		int maxThreads = Math.max(1, Runtime.getRuntime().availableProcessors());
		ModelRunSvcImpl.setSimultaneousRuns(maxThreads);
		((JSlider) _swingEngine.find("run_sldThreads")).addChangeListener(new GlobalChangeListener());
		_swingEngine.find("run_sldThreads").setEnabled(maxThreads > 1);
		((JSlider) _swingEngine.find("run_sldThreads")).setMaximum(maxThreads);
		((JLabel) _swingEngine.find("run_lblThreads"))
				.setText(" " + maxThreads + ((maxThreads > 1) ? " runs" : " run"));
		((JLabel) _swingEngine.find("run_lblThreadsInfo"))
				.setText("Simultaneous runs " + ((maxThreads > 1) ? "(1-" + maxThreads + ")" : "(1)"));
	}

	private void initGlobalListeners(IAllButtonsDele allButtonsDele)
	{
		// Set up the global Listeners.
		_swingEngine.setActionListener(_swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalActionListener());
		//setCheckBoxorMouseListener(_swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalMouseListener());
		setCheckBoxorRadioButtonItemListener(_swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalItemListener());
		setLinkedSliderChangeListener(_swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalChangeListener());
		ImageIcon icon = new ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
		((JFrame) _swingEngine.find(Constant.MAIN_FRAME_NAME)).setIconImage(icon.getImage());
		((JTabbedPane) _swingEngine.find("reg_tabbedPane")).addChangeListener(new GlobalChangeListener());

		((JFrame) _swingEngine.find(Constant.MAIN_FRAME_NAME))
				.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);// EXIT_ON_CLOSE

	}

	private void initSpinners()
	{
		// Set up month spinners
		JSpinner spnSM1 = (JSpinner) _swingEngine.find("spnRunStartMonth");
		setMonthModelAndIndex(spnSM1, 9);
		JSpinner spnEM1 = (JSpinner) _swingEngine.find("spnRunEndMonth");
		setMonthModelAndIndex(spnEM1, 8);
		// Set up year spinners
		JSpinner spnSY1 = (JSpinner) _swingEngine.find("spnRunStartYear");
		setNumberModelAndIndex(spnSY1, 1921, Constant.MIN_YEAR, Constant.MAX_YEAR, 1, "####");
		JSpinner spnEY1 = (JSpinner) _swingEngine.find("spnRunEndYear");
		setNumberModelAndIndex(spnEY1, 2003, Constant.MIN_YEAR, Constant.MAX_YEAR, 1, "####");
		addJSpinnerListener();
	}

	/**
	 * This method will check for the new tables which are defined in the
	 * gui.xml file and load them.
	 *
	 * @param newUserDefinedIds This is the id's of the new tables defined in gui.xml file.
	 * @param scenarioSvc       The Object of {@link ScenarioSvcImpl}.
	 * @param tableSvc          The Object of {@link TableSvcImpl}.
	 * @param swingEngine       The Object of {@link SwingEngine}.
	 */
	private void checkForNewUserDefinedTables(List<String> newUserDefinedIds, IScenarioSvc scenarioSvc,
											  ITableSvc tableSvc, SwingEngine swingEngine) throws EpptInitializationException
	{
		DataTableModel dtm = null;
		for(String newUserDefinedId : newUserDefinedIds)
		{
			if(scenarioSvc.hasUserDefinedTable(newUserDefinedId))
			{
				dtm = scenarioSvc.getUserDefinedTable(newUserDefinedId);
				((JTable) swingEngine.find(newUserDefinedId)).setModel(dtm);
			}
			else
			{
				try
				{
					dtm = tableSvc.getTable(newUserDefinedId, TableSvcImpl::handleTableFileWithColumnNumber);
					dtm.setCellEditable(true);
					scenarioSvc.addUserDefinedTable(newUserDefinedId, dtm);
					((JTable) swingEngine.find(newUserDefinedId)).setModel(dtm);
				}
				catch(CalLiteGUIException ex)
				{
					Log.error(ex);
					String errorMsg = "There is a table id " + newUserDefinedId + " in the gui.xml file but there is no table file with that name. Please provide the file.";
					throw new EpptInitializationException("Error getting table.", new CalLiteGUIException(errorMsg, ex));
				}
			}
		}
	}

	/**
	 * This method is for loading the strings in the {@link JSpinner}.
	 *
	 * @param jspn The {@link JSpinner} to load.
	 * @param idx  The month value which we should be displaying.
	 */
	private void setMonthModelAndIndex(JSpinner jspn, int idx)
	{
		String[] monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
		try
		{
			SpinnerListModel monthModel = new SpinnerListModel(monthNames);
			jspn.setModel(monthModel);
			jspn.setValue(monthNames[idx]);
		}
		catch(RuntimeException e)
		{
			LOGGER.warn("Unable to initialize month spinner", e);
		}
	}

	/**
	 * This method will set the max and min values for the {@link JSpinner}
	 * object.
	 *
	 * @param jspn   The {@link JSpinner} object.
	 * @param value  the current value of the {@link JSpinner}.
	 * @param min    the first number in the sequence
	 * @param max    the last number in the sequence
	 * @param step   the difference between elements of the sequence
	 * @param format The format that the {@link JSpinner} should look like.
	 */
	private void setNumberModelAndIndex(JSpinner jspn, int value, int min, int max, int step, String format)
	{
		SpinnerModel spnmod = new SpinnerNumberModel(value, min, max, step);
		jspn.setModel(spnmod);
		jspn.setEditor(new JSpinner.NumberEditor(jspn, format));
	}

	/**
	 * This method will set the change listener for the component and for all
	 * its children which are LinkedSliders.
	 *
	 * @param component
	 * @param changeListener Object of the Item Listener.
	 */
	private void setLinkedSliderChangeListener(Component component, Object changeListener)
	{
		if(component instanceof JLinkedSlider)
		{
			((JLinkedSlider) component).addChangeListener((ChangeListener) changeListener);
		}
		for(Component child : ((Container) component).getComponents())
		{
			setLinkedSliderChangeListener(child, changeListener);
		}
	}

	/**
	 * This method will set the item listener for the component and for all it's
	 * children which are Check Box and radio button.
	 *
	 * @param component    The component to which you want to set the itemListener.
	 * @param itemListener Object of the Item Listener.
	 */
	private void setCheckBoxorRadioButtonItemListener(Component component, Object itemListener)
	{
		if(component instanceof JCheckBox || component instanceof JRadioButton)
		{
			((AbstractButton) component).addItemListener((ItemListener) itemListener);
		}
		for(Component child : ((Container) component).getComponents())
		{
			setCheckBoxorRadioButtonItemListener(child, itemListener);
		}
	}

	/**
	 * This method will set the mouse listener for the component and for all
	 * it's children which are Check Box.
	 *
	 * @param component     The component to which you want to set the MouseListener.
	 * @param mouseListener Object of the Mouse Listener.
	 */
	private void setCheckBoxorMouseListener(Component component, Object mouseListener)
	{
		if(component instanceof JCheckBox)
		{
			component.addMouseListener((MouseListener) mouseListener);
		}
		for(Component child : ((Container) component).getComponents())
		{
			setCheckBoxorMouseListener(child, mouseListener);
		}
	}

	/**
	 * This method is to add focus listeners to regulations checkboxes tracking
	 * the changes.
	 *
	 * @param listOfNames The list of names to which we want to add the
	 *                    JCheckBoxListeners.
	 */
	private void addJCheckBoxListener(List<String> listOfNames)
	{
		FocusListener focusListenerForCheckBox = new FocusListener()
		{

			@Override
			public void focusLost(FocusEvent e)
			{
				if(e.getComponent().getName().equals(Constant.CKB_REG_VAMP))
				{
					((JLabel) _swingEngine.find("labReg")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
					((JLabel) _swingEngine.find("labReg2")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
				}
			}

			@Override
			public void focusGained(FocusEvent e)
			{
				boolean showTablePanel = ((JRadioButton) _swingEngine.find("rdbRegQS_UD")).isSelected();
				if(showTablePanel)
				{
					String cName = e.getComponent().getName();
					GUILinks2BO gUILinks2BO = SeedDataSvcImpl.getSeedDataSvcImplInstance().getObjByGuiId(cName);

					if(gUILinks2BO != null)
					{
						showTablePanel = showTablePanel || (!gUILinks2BO.getDataTables().equals(Constant.N_A));
					}

					showTablePanel = showTablePanel
							&& (((JTabbedPane) _swingEngine.find("reg_tabbedPane")).getSelectedIndex() != 2);


					_swingEngine.find("reg_panTab").setVisible(showTablePanel);
					_swingEngine.find("reg_panTabPlaceholder").setVisible(!showTablePanel);

					IApplyDynamicConDele applyDynamicConDele = new ApplyDynamicConDeleImp();
					JCheckBox c = (JCheckBox) e.getComponent();

					applyDynamicConDele.applyDynamicControl(c.getName(), c.isSelected(), c.isEnabled(), false);

					if(c.getName().equals(Constant.CKB_REG_VAMP))
					{

						if(c.isSelected())
						{
							((JLabel) _swingEngine.find("labReg")).setText(Constant.VAMP_SELECTED_TEXT);
							((JLabel) _swingEngine.find("labReg2")).setText(Constant.VAMP_SELECTED_TEXT);
						}
						else
						{
							((JLabel) _swingEngine.find("labReg")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
							((JLabel) _swingEngine.find("labReg2")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
						}
					}
					// Make label red and enabled

					_swingEngine.find("labReg")
								.setForeground(_swingEngine.find("labReg2").getForeground());

					_swingEngine.find("labReg").setEnabled(true);

				}
			}
		};
		for(String name : listOfNames)
		{
			_swingEngine.find(name).addFocusListener(focusListenerForCheckBox);
		}

	}

	/**
	 * This method is to add the listener for the {@link JTextField} for
	 * tracking the changes.
	 *
	 * @param listOfNames The list of names to which we want to add the
	 *                    JTextFieldListener.
	 */
	private void addJTextFieldListener(List<String> listOfNames)
	{
		FocusListener focusListenerForTextField = new FocusListener()
		{
			String _oldValue = "";

			@Override
			public void focusLost(FocusEvent e)
			{
				JTextField field = ((JTextField) e.getComponent());
				String newValue = field.getText();
				if(!_oldValue.equals(newValue) && !_xmlParsingSvc.checkIsItFromResultPart(field.getName()))
				{
					_auditSvc.addAudit(field.getName(), _oldValue, newValue);
				}
			}

			@Override
			public void focusGained(FocusEvent e)
			{
				_oldValue = ((JTextField) e.getComponent()).getText();
			}
		};
		FocusListener focusListenerForTextArea = new FocusListener()
		{
			String _oldValue = "";

			@Override
			public void focusLost(FocusEvent e)
			{
				JTextArea field = ((JTextArea) e.getComponent());
				String newValue = field.getText();
				if(!_oldValue.equals(newValue) && !_xmlParsingSvc.checkIsItFromResultPart(field.getName()))
				{
					_auditSvc.addAudit(field.getName(), _oldValue, newValue);
				}
			}

			@Override
			public void focusGained(FocusEvent e)
			{
				_oldValue = ((JTextArea) e.getComponent()).getText();
			}
		};
		for(String name : listOfNames)
		{
			Component c = _swingEngine.find(name);
			if(c instanceof JTextField)
			{
				_swingEngine.find(name).addFocusListener(focusListenerForTextField);
			}
			else
			{
				_swingEngine.find(name).addFocusListener(focusListenerForTextArea);
			}
		}
	}

	/**
	 * This method is to add the listener for selected {@link JSpinner} for
	 * tracking the changes.
	 */
	private void addJSpinnerListener()
	{
		ChangeListener listener = e -> _auditSvc.addAudit(((JSpinner) e.getSource()).getName(), " ",
				String.valueOf(((JSpinner) e.getSource()).getValue()));
		JSpinner spnRunEndMonth = (JSpinner) _swingEngine.find("spnRunEndMonth");
		spnRunEndMonth.addChangeListener(listener);
		JSpinner spnRunEndYear = (JSpinner) _swingEngine.find("spnRunEndYear");
		spnRunEndYear.addChangeListener(listener);
	}

	/**
	 * Data retrieval for single DSS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */
	private void retrieve(IDialogSvc dialogSvc)
	{
		JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) _swingEngine.find("SelectedList");
		if(!AppUtils.baseOn)
		{
			dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
			return;
		}
		try
		{
			List<RBListItemBO> scenarios = new ArrayList<>();
			ListModel<RBListItemBO> model = lstScenarios.getModel();
			for(int i = 0; i < model.getSize(); i++)
			{
				scenarios.add(model.getElementAt(i));
			}
			String noRowsString = "";
			JTable table = GuiUtils.getCLGPanel().getRetrievePanel().getTable();
			if(table.getRowCount() == 0)
			{
				noRowsString = " after using \"Filter\" to load variables";
			}
			Group group = GuiUtils.getCLGPanel().getRetrievePanel().getGroup();
			if(group == null || table.getSelectedRowCount() == 0)
			{
				dialogSvc.getOK("Variables not selected! Select one or more variables" + noRowsString,
						JOptionPane.INFORMATION_MESSAGE);
				return;
			}
			// checked if count > 0 above
			int[] rows = table.getSelectedRows();
			DataReference[] array = new DataReference[rows.length];
			for(int i = 0; i < rows.length; i++)
			{
				array[i] = group.getDataReference(rows[i]);
			}
			for(int i = 0; i < rows.length; i++)
			{
				String[] parts = array[i].getName().split("::");
				String[] parts2 = parts[2].split("/");
				parts[2] = "/" + parts2[1] + "/" + parts2[2] + "/" + parts2[3] + "/" + parts[3] + "/" + parts2[5] + "/"
						+ parts2[6] + "/";

				if(parts[1].toUpperCase().contains(("_SV.DSS")))
				{
					DisplayFrame.showDisplayFrames(_swingEngine,
							DisplayFrame.quickState(_swingEngine) + ";Locs-" + parts[2] + ";Index-"
							+ parts[2] + ";File-" + parts[1], scenarios);
				}
				else
				{
					DisplayFrame.showDisplayFrames(_swingEngine,
							DisplayFrame.quickState(_swingEngine) + ";Locs-" + parts[2] + ";Index-" + parts[2],
							scenarios);
				}
			}
		}
		catch(RuntimeException e)
		{
			LOGGER.debug("Error in retrieve() -", e);
		}
	}

	/**
	 * Data retrieval for DTS/MTS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */

	private void retrieve2(IErrorHandlingSvc errorHandlingSvc, IDialogSvc dialogSvc)
	{
		JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) _swingEngine.find("SelectedList");
		if(!AppUtils.baseOn)
		{
			dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
			return;
		}

		DerivedTimeSeries dts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getDTS();
		MultipleTimeSeries mts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getMTS();

		if(((mts == null) && (dts == null)) || ((dts != null) && (dts.getBParts().isEmpty()))
				|| ((mts != null) && (mts.getNumberOfDataReferences() < 1)))
		{
			dialogSvc.getOK("Nothing to display! Specify DTS or MTS data reference", JOptionPane.WARNING_MESSAGE);

			return;
		}

		try
		{
			List<RBListItemBO> scenarios = new ArrayList<>();
			ListModel<RBListItemBO> model = lstScenarios.getModel();
			for(int i = 0; i < model.getSize(); i++)
			{
				scenarios.add(model.getElementAt(i));
			}
			DisplayFrame.showDisplayFramesWRIMS(_swingEngine,
					DisplayFrame.quickState(_swingEngine) + ";Locs-;Index-;File-", scenarios, dts,
					mts);

		}
		catch(Exception e)
		{
			LOGGER.debug("Error in retrieve2() -", e);
			errorHandlingSvc.businessErrorHandler(null, e);

		}
	}

}
