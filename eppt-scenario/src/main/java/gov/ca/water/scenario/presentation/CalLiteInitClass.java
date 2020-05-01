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

package gov.ca.water.scenario.presentation;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemListener;
import java.nio.file.Paths;
import java.util.List;
import javax.swing.*;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeListener;

import gov.ca.water.bo.GUILinks2BO;
import gov.ca.water.businessservice.IAllButtonsDele;
import gov.ca.water.businessservice.IApplyDynamicConDele;
import gov.ca.water.businessservice.IScenarioSvc;
import gov.ca.water.businessservice.ISeedDataSvc;
import gov.ca.water.businessservice.ITableSvc;
import gov.ca.water.businessservice.IVerifyControlsDele;
import gov.ca.water.businessservice.IXMLParsingSvc;
import gov.ca.water.businessservice.impl.AllButtonsDeleImp;
import gov.ca.water.businessservice.impl.ApplyDynamicConDeleImp;
import gov.ca.water.businessservice.impl.DynamicControlSvcImpl;
import gov.ca.water.businessservice.impl.ScenarioSvcImpl;
import gov.ca.water.businessservice.impl.SeedDataSvcImpl;
import gov.ca.water.businessservice.impl.TableSvcImpl;
import gov.ca.water.businessservice.impl.VerifyControlsDeleImp;
import gov.ca.water.businessservice.impl.XMLParsingSvcImpl;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.busservice.impl.ModelRunSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.calgui.techservice.IAuditSvc;
import gov.ca.water.calgui.techservice.IDialogSvc;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.AuditSvcImpl;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import gov.ca.water.scenario.ui.JLinkedSlider;
import org.apache.log4j.Logger;
import org.jfree.util.Log;
import org.swixml.SwingEngine;

/**
 * This class is for initializing the Application and adding the Action, Item,
 * and Mouse Listeners to the main frame.
 *
 * @author Mohan
 */
public class CalLiteInitClass
{
	private static final Logger LOGGER = Logger.getLogger(CalLiteInitClass.class.getName());
	private final IAuditSvc _auditSvc = AuditSvcImpl.getAuditSvcImplInstance();
	private IXMLParsingSvc _xmlParsingSvc;
	private SwingEngine _swingEngine;

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
		catch(RuntimeException e)
		{
			LOGGER.error("Error initializing CalLite", e);


		}
	}

	private void init2(IErrorHandlingSvc errorHandlingSvc, IDialogSvc dialogSvc, SwingEngine swingEngine, IXMLParsingSvc xmlParsingSvc)
			throws EpptInitializationException
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
			scenarioSvc.applyClsFile(Paths.get(Constant.SCENARIOS_DIR + Constant.DEFAULT + Constant.CLS_EXT),
					swingEngine,
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
			ResultUtilsBO resultUtilsBO = ResultUtilsBO.getResultUtilsInstance();
			// Setup for Reporting page
			// Set up additional UI elements
			JList<?> lstScenarios = (JList<?>) swingEngine.find("SelectedList");
			lstScenarios.setBorder(new LineBorder(Color.gray, 1));

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
			jTabbedPane.addChangeListener(e ->
			{
				Component c = (Component) e.getSource();
				String lcName = c.getName().toLowerCase();
				if("spn".equals(lcName.substring(0, 3)))
				{
					// Constrain run times to [10/1921,9/2003]
					int syr = (Integer) ((JSpinner) _swingEngine.find("spnRunStartYear")).getValue();
					int eyr = (Integer) ((JSpinner) _swingEngine.find("spnRunEndYear")).getValue();
					int smo = ResultUtilsBO.getResultUtilsInstance().monthToInt(
							((String) ((JSpinner) _swingEngine.find("spnRunStartMonth")).getValue()).trim());
					int emo = ResultUtilsBO.getResultUtilsInstance().monthToInt(
							((String) ((JSpinner) _swingEngine.find("spnRunEndMonth")).getValue()).trim());
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
					smo = ResultUtilsBO.getResultUtilsInstance().monthToInt(
							((String) ((JSpinner) _swingEngine.find("spnStartMonth")).getValue()).trim());
					emo = ResultUtilsBO.getResultUtilsInstance().monthToInt(
							((String) ((JSpinner) _swingEngine.find("spnEndMonth")).getValue()).trim());
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
			});

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
			errorHandlingSvc.businessErrorHandler(messageText, e);
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
		Component mainFrame = _swingEngine.find(Constant.MAIN_FRAME_NAME);
		setCheckBoxorRadioButtonItemListener(mainFrame, new GlobalItemListener());
		setLinkedSliderChangeListener(mainFrame, new GlobalChangeListener());
		ImageIcon icon = new ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
		((JFrame) mainFrame).setIconImage(icon.getImage());
		((JTabbedPane) _swingEngine.find("reg_tabbedPane")).addChangeListener(new GlobalChangeListener());

		((JFrame) mainFrame)
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
		setNumberModelAndIndex(spnSY1, 1921, 1921, 2003, 1, "####");
		JSpinner spnEY1 = (JSpinner) _swingEngine.find("spnRunEndYear");
		setNumberModelAndIndex(spnEY1, 2003, 1921, 2003, 1, "####");
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


}
