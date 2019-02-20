/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import calsim.gui.GuiUtils;
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.*;
import java.util.List;

/**
 * This class is for initializing the Application and adding the Action, Item,
 * and Mouse Listeners to the main frame.
 *
 * @author Mohan
 */
public class CalLiteInitClass
{
	private static final Logger LOG = Logger.getLogger(CalLiteInitClass.class.getName());
	private SwingEngine swingEngine;
	private IAuditSvc auditSvc;
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
	private IDialogSvc dialogSvc = DialogSvcImpl.getDialogSvcInstance();
	private IXMLParsingSvc xmlParsingSvc = XMLParsingSvcImpl.getXMLParsingSvcImplInstance();

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

		if((comp instanceof JButton))
		{
			if(((JButton) comp).getText().equals(text))
			{
				return comp;
			}
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
	 * Iterates through all components inside of a component and sets the
	 * GridBagConstraints.weightx and weighty to 0.5
	 *
	 * @param parent
	 *            Starting point for iteration
	 *
	 */
	// private void reweightComponents(Container parent, GridBagLayout layout1)
	// {
	// for (Component c : parent.getComponents()) {
	//
	// if (c instanceof JPanel) {
	// if (((JPanel) c).getLayout() instanceof GridBagLayout) {
	// GridBagLayout layout2 = (GridBagLayout) ((JPanel) c).getLayout();
	// reweightComponents((Container) c, layout2);
	// }
	// } else if (layout1 != null) {
	// GridBagConstraints gbc = layout1.getConstraints(c);
	// // gbc.weightx = 0.0;
	// // gbc.weighty = 0.0;//
	// gbc.ipadx = 150;
	// gbc.ipady = 15;
	// layout1.setConstraints(c, gbc);
	// System.out.println(c.getName());
	//
	// }
	// }
	// }

	/**
	 * This method is called to initialize the ui.
	 */
	public void init()
	{

		try
		{
			// ----- Build all the Services.
			ISeedDataSvc seedDataSvc = SeedDataSvcImpl.getSeedDataSvcImplInstance();
			IVerifyControlsDele verifyControlsDele = new VerifyControlsDeleImp();
			verifyControlsDele.verifyTheDataBeforeUI(Constant.SCENARIOS_DIR + Constant.DEFAULT + Constant.CLS_EXT);
			DynamicControlSvcImpl.getDynamicControlSvcImplInstance();
			ITableSvc tableSvc = TableSvcImpl.getTableSvcImplInstance(seedDataSvc.getUserTables());
			this.swingEngine = xmlParsingSvc.getSwingEngine();
			IScenarioSvc scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();
			IAllButtonsDele allButtonsDele = new AllButtonsDeleImp();

			// ----- Set up the GUI

			// Set up month spinners
			JSpinner spnSM1 = (JSpinner) swingEngine.find("spnRunStartMonth");
			setMonthModelAndIndex(spnSM1, 9);
			JSpinner spnEM1 = (JSpinner) swingEngine.find("spnRunEndMonth");
			setMonthModelAndIndex(spnEM1, 8);
			// Set up year spinners
			JSpinner spnSY1 = (JSpinner) swingEngine.find("spnRunStartYear");
			setNumberModelAndIndex(spnSY1, 1921, Constant.MIN_YEAR, Constant.MAX_YEAR, 1, "####");
			JSpinner spnEY1 = (JSpinner) swingEngine.find("spnRunEndYear");
			setNumberModelAndIndex(spnEY1, 2003, Constant.MIN_YEAR, Constant.MAX_YEAR, 1, "####");
			addJSpinnerListener();
			// Set up the global Listeners.
			swingEngine.setActionListener(swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalActionListener());
			setCheckBoxorMouseListener(swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalMouseListener());
			setCheckBoxorRadioButtonItemListener(swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalItemListener());
			setLinkedSliderChangeListener(swingEngine.find(Constant.MAIN_FRAME_NAME), new GlobalChangeListener());
			ImageIcon icon = new ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
			((JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME)).setIconImage(icon.getImage());
			((JTabbedPane) swingEngine.find("reg_tabbedPane")).addChangeListener(new GlobalChangeListener());

			((JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME))
					.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);// EXIT_ON_CLOSE
			((JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME)).addWindowListener(new WindowAdapter()
			{
				@Override
				public void windowClosing(WindowEvent we)
				{
					allButtonsDele.windowClosing();
				}
			});
			// Load the default cls file.
			scenarioSvc.applyClsFile(Constant.SCENARIOS_DIR + Constant.DEFAULT + Constant.CLS_EXT, swingEngine,
					seedDataSvc.getTableIdMap());
			// check
			checkForNewUserDefinedTables(xmlParsingSvc.getNewUserDefinedTables(), scenarioSvc, tableSvc, swingEngine);
			auditSvc = AuditSvcImpl.getAuditSvcImplInstance();
			auditSvc.clearAudit(); // we clear because when we 1st load the cls
			// file
			// we should not have any records.
			addJTextFieldListener(xmlParsingSvc.getjTextFieldIds());
			addJCheckBoxListener(xmlParsingSvc.getjCheckBoxIDs());
			// Count threads and update batch run selector appropriately
			int maxThreads = Math.max(1, Runtime.getRuntime().availableProcessors());
			ModelRunSvcImpl.setSimultaneousRuns(maxThreads);
			((JSlider) swingEngine.find("run_sldThreads")).addChangeListener(new GlobalChangeListener());
			swingEngine.find("run_sldThreads").setEnabled(maxThreads > 1);
			((JSlider) swingEngine.find("run_sldThreads")).setMaximum(maxThreads);
			((JLabel) swingEngine.find("run_lblThreads"))
					.setText(" " + maxThreads + ((maxThreads > 1) ? " runs" : " run"));
			((JLabel) swingEngine.find("run_lblThreadsInfo"))
					.setText("Simultaneous runs " + ((maxThreads > 1) ? "(1-" + maxThreads + ")" : "(1)"));
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
			JButton btnScenario = (JButton) swingEngine.find("btnAddScenario");
			btnScenario.addActionListener(fdDSSFiles);
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

			WRIMSGUILinks.buildWRIMSGUI((JPanel) swingEngine.find("WRIMS"));
			WRIMSGUILinks.setStatus("Initialized.");
			// Replace WRIMS GUI display action with CalLite GUI action
			JButton retrieveBtn = GuiUtils.getCLGPanel().getRetrievePanel().getRetrieveBtn();
			for(ActionListener al : retrieveBtn.getActionListeners())
			{
				retrieveBtn.removeActionListener(al);
			}
			retrieveBtn.addActionListener(new ActionListener()
			{

				@Override
				public void actionPerformed(ActionEvent arg0)
				{
					retrieve();
				}
			});
			Component openButtonComponent = findFirstButtonMatchingText(GuiUtils.getCLGPanel(), "Open");
			if(openButtonComponent != null)
			{
				JButton openButton = (JButton) openButtonComponent;
				for(ActionListener al : openButton.getActionListeners())
				{
					openButton.removeActionListener(al);
				}
				openButton.addActionListener(new ActionListener()
				{
					@Override
					public void actionPerformed(ActionEvent arg0)
					{
						retrieve2();
					}
				});
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

			JMenuBar menuBar = (JMenuBar) this.swingEngine.find("menu");
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

			// reweightComponents((Container) swingEngine.find("runsettings"),
			// null);
			// reweightComponents((Container) swingEngine.find("Reporting"),
			// null);

			// Display the GUI
			swingEngine.find(Constant.MAIN_FRAME_NAME).setVisible(true);
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to initialize GUI.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
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
	public void checkForNewUserDefinedTables(List<String> newUserDefinedIds, IScenarioSvc scenarioSvc,
											 ITableSvc tableSvc, SwingEngine swingEngine)
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
					errorHandlingSvc.displayErrorMessageBeforeTheUI(new CalLiteGUIException(
							"There is a table id " + newUserDefinedId
									+ " in the gui.xml file but there is no table file with that name. Please provide the file.",
							ex));
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
	public void setMonthModelAndIndex(JSpinner jspn, int idx)
	{
		String[] monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
		try
		{
			SpinnerListModel monthModel = new SpinnerListModel(monthNames);
			jspn.setModel(monthModel);
			jspn.setValue(monthNames[idx]);
		}
		catch(Exception e)
		{
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
	public void setNumberModelAndIndex(JSpinner jspn, int value, int min, int max, int step, String format)
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
	public void setLinkedSliderChangeListener(Component component, Object changeListener)
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
	public void setCheckBoxorRadioButtonItemListener(Component component, Object itemListener)
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
	public void setCheckBoxorMouseListener(Component component, Object mouseListener)
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
					((JLabel) swingEngine.find("labReg")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
					((JLabel) swingEngine.find("labReg2")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
				}
			}

			@Override
			public void focusGained(FocusEvent e)
			{
				boolean showTablePanel = ((JRadioButton) swingEngine.find("rdbRegQS_UD")).isSelected();
				if(showTablePanel)
				{
					String cName = e.getComponent().getName();
					GUILinks2BO gUILinks2BO = SeedDataSvcImpl.getSeedDataSvcImplInstance().getObjByGuiId(cName);

					if(gUILinks2BO != null)
					{
						showTablePanel = showTablePanel || (!gUILinks2BO.getDataTables().equals(Constant.N_A));
					}

					showTablePanel = showTablePanel
							&& (((JTabbedPane) swingEngine.find("reg_tabbedPane")).getSelectedIndex() != 2);

					// Force display of panel for Trinity, Pumping
					// showTablePanel = showTablePanel ||
					// (cName.equals("ckbReg_TRNTY") ||
					// cName.equals("ckbReg_PUMP"));

					swingEngine.find("reg_panTab").setVisible(showTablePanel);
					swingEngine.find("reg_panTabPlaceholder").setVisible(!showTablePanel);

					IApplyDynamicConDele applyDynamicConDele = new ApplyDynamicConDeleImp();
					JCheckBox c = (JCheckBox) e.getComponent();
					// boolean isSelected = c.isSelected();
					// if (c.getName().equals(Constant.CKB_REG_VAMP) &&
					// e.getComponent() != null)
					// c.setSelected(!isSelected);

					applyDynamicConDele.applyDynamicControl(c.getName(), c.isSelected(), c.isEnabled(), false);

					if(c.getName().equals(Constant.CKB_REG_VAMP))
					{

						if(c.isSelected())
						{
							((JLabel) swingEngine.find("labReg")).setText(Constant.VAMP_SELECTED_TEXT);
							((JLabel) swingEngine.find("labReg2")).setText(Constant.VAMP_SELECTED_TEXT);
						}
						else
						{
							((JLabel) swingEngine.find("labReg")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
							((JLabel) swingEngine.find("labReg2")).setText(Constant.VAMP_NOT_SELECTED_TEXT);
						}
					}
					// Make label red and enabled

					swingEngine.find("labReg")
							.setForeground(swingEngine.find("labReg2").getForeground());

					swingEngine.find("labReg").setEnabled(true);

				}
			}
		};
		for(String name : listOfNames)
		{
			swingEngine.find(name).addFocusListener(focusListenerForCheckBox);
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
			String oldValue = "";

			@Override
			public void focusLost(FocusEvent e)
			{
				JTextField field = ((JTextField) e.getComponent());
				String newValue = field.getText();
				if(!oldValue.equals(newValue))
				{
					if(!xmlParsingSvc.checkIsItFromResultPart(field.getName()))
					{
						auditSvc.addAudit(field.getName(), oldValue, newValue);
					}

				}
			}

			@Override
			public void focusGained(FocusEvent e)
			{
				oldValue = ((JTextField) e.getComponent()).getText();
			}
		};
		FocusListener focusListenerForTextArea = new FocusListener()
		{
			String oldValue = "";

			@Override
			public void focusLost(FocusEvent e)
			{
				JTextArea field = ((JTextArea) e.getComponent());
				String newValue = field.getText();
				if(!oldValue.equals(newValue))
				{
					if(!xmlParsingSvc.checkIsItFromResultPart(field.getName()))
					{
						auditSvc.addAudit(field.getName(), oldValue, newValue);
					}
				}
			}

			@Override
			public void focusGained(FocusEvent e)
			{
				oldValue = ((JTextArea) e.getComponent()).getText();
			}
		};
		for(String name : listOfNames)
		{
			Component c = swingEngine.find(name);
			if(c instanceof JTextField)
			{
				swingEngine.find(name).addFocusListener(focusListenerForTextField);
			}
			else
			{
				swingEngine.find(name).addFocusListener(focusListenerForTextArea);
			}
		}
	}

	/**
	 * This method is to add the listener for selected {@link JSpinner} for
	 * tracking the changes.
	 */
	private void addJSpinnerListener()
	{
		ChangeListener listener = new ChangeListener()
		{
			@Override
			public void stateChanged(ChangeEvent e)
			{
				auditSvc.addAudit(((JSpinner) e.getSource()).getName(), " ",
						String.valueOf(((JSpinner) e.getSource()).getValue()));
			}
		};
		JSpinner spnRunEndMonth = (JSpinner) swingEngine.find("spnRunEndMonth");
		spnRunEndMonth.addChangeListener(listener);
		JSpinner spnRunEndYear = (JSpinner) swingEngine.find("spnRunEndYear");
		spnRunEndYear.addChangeListener(listener);
	}

	/**
	 * Data retrieval for single DSS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */
	private void retrieve()
	{
		JList<?> lstScenarios = (JList<?>) swingEngine.find("SelectedList");
		if(!AppUtils.baseOn)
		{
			// JOptionPane.showMessageDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),
			// "The Base DSS files need to be selected", "DSS Not Selected",
			// JOptionPane.WARNING_MESSAGE);

			// ImageIcon icon = new
			// ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
			// Object[] options = { "OK" };
			// JOptionPane optionPane = new JOptionPane("The Base DSS files need
			// to be selected",
			// JOptionPane.ERROR_MESSAGE, JOptionPane.OK_OPTION, null, options,
			// options[0]);
			// JDialog dialog =
			// optionPane.createDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),"CalLite");
			// dialog.setIconImage(icon.getImage());
			// dialog.setResizable(false);
			// dialog.setVisible(true);
			dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
			return;
		}
		try
		{
			String noRowsString = "";
			JTable _table = GuiUtils.getCLGPanel().getRetrievePanel().getTable();
			if(_table.getRowCount() == 0)
			{
				noRowsString = " after using \"Filter\" to load variables";
			}
			Group _group = GuiUtils.getCLGPanel().getRetrievePanel().getGroup();
			if(_group == null || _table.getSelectedRowCount() == 0)
			{
				// JOptionPane.showMessageDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),
				// "Select one or more variables" + noRowsString,
				// "Variable(s) Not Selected", JOptionPane.INFORMATION_MESSAGE);
				// ImageIcon icon = new
				// ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
				// Object[] options = { "OK" };
				// JOptionPane optionPane = new JOptionPane("Select one or more
				// variables" + noRowsString,
				// JOptionPane.ERROR_MESSAGE, JOptionPane.OK_OPTION, null,
				// options, options[0]);
				// JDialog dialog =
				// optionPane.createDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),"CalLite");
				// dialog.setIconImage(icon.getImage());
				// dialog.setResizable(false);
				// dialog.setVisible(true);
				dialogSvc.getOK("Variables not selected! Select one or more variables" + noRowsString,
						JOptionPane.INFORMATION_MESSAGE);
				return;
			}
			int[] rows = _table.getSelectedRows(); // checked if count > 0 above
			DataReference[] array = new DataReference[rows.length];
			for(int i = 0; i < rows.length; i++)
			{
				array[i] = _group.getDataReference(rows[i]);
			}
			// GuiUtils.displayData(array);
			for(int i = 0; i < rows.length; i++)
			{
				String[] parts = array[i].getName().split("::");
				String[] parts2 = parts[2].split("/");
				parts[2] = "/" + parts2[1] + "/" + parts2[2] + "/" + parts2[3] + "/" + parts[3] + "/" + parts2[5] + "/"
						+ parts2[6] + "/";

				if(parts[1].toUpperCase().contains(("_SV.DSS")))
				{
					DisplayFrame.showDisplayFrames(DisplayFrame.quickState() + ";Locs-" + parts[2] + ";Index-"
							+ parts[2] + ";File-" + parts[1], lstScenarios);
				}
				else
				{
					DisplayFrame.showDisplayFrames(
							DisplayFrame.quickState() + ";Locs-" + parts[2] + ";Index-" + parts[2], lstScenarios);
				}
			}
		}
		catch(Exception e)
		{

			// VistaUtils.displayException(GuiUtils.getCLGPanel(), e);
			LOG.debug("Error in retrieve() -", e);
		}
		WRIMSGUILinks.setStatus("Well??");
	}

	/**
	 * Data retrieval for DTS/MTS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */

	private void retrieve2()
	{
		JList<?> lstScenarios = (JList<?>) swingEngine.find("SelectedList");
		WRIMSGUILinks.setStatus("Retrieve2");

		if(!AppUtils.baseOn)
		{
			// JOptionPane.showMessageDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),
			// "The Base DSS files need to be selected", "DSS Not Selected",
			// JOptionPane.WARNING_MESSAGE);
			// ImageIcon icon = new
			// ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
			// Object[] options = { "OK" };
			// JOptionPane optionPane = new JOptionPane("The Base DSS files need
			// to be selected",
			// JOptionPane.ERROR_MESSAGE, JOptionPane.OK_OPTION, null, options,
			// options[0]);
			// JDialog dialog =
			// optionPane.createDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),
			// "CalLite");
			// dialog.setIconImage(icon.getImage());
			// dialog.setResizable(false);
			// dialog.setVisible(true);
			dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);

			return;
		}

		DerivedTimeSeries dts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getDTS();
		MultipleTimeSeries mts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getMTS();

		if(((mts == null) && (dts == null)) || ((dts != null) && (dts.getBParts().size() < 1))
				|| ((mts != null) && (mts.getNumberOfDataReferences() < 1)))
		{
			// JOptionPane.showMessageDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),
			// "Specify DTS or MTS data references", "Nothing to Display",
			// JOptionPane.WARNING_MESSAGE);
			// ImageIcon icon = new
			// ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
			// Object[] options = { "OK" };
			// JOptionPane optionPane = new JOptionPane("Specify DTS or MTS data
			// references", JOptionPane.ERROR_MESSAGE,
			// JOptionPane.OK_OPTION, null, options, options[0]);
			// JDialog dialog =
			// optionPane.createDialog(swingEngine.find(Constant.MAIN_FRAME_NAME),
			// "CalLite");
			// dialog.setIconImage(icon.getImage());
			// dialog.setResizable(false);
			// dialog.setVisible(true);
			dialogSvc.getOK("Nothing to display! Specify DTS or MTS data reference", JOptionPane.WARNING_MESSAGE);

			return;
		}

		try
		{
			DisplayFrame.showDisplayFrames_WRIMS(DisplayFrame.quickState() + ";Locs-;Index-;File-", lstScenarios, dts,
					mts);

		}
		catch(Exception e)
		{
			// VistaUtils.displayException(GuiUtils.getCLGPanel(), e);
			LOG.debug("Error in retrieve2() -", e);
			errorHandlingSvc.businessErrorHandler(null, e);

		}
		WRIMSGUILinks.setStatus("Done??");
	}

}