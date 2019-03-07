/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice.impl;

import java.awt.Component;
import java.awt.Container;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableCellRenderer;

import gov.ca.water.businessservice.IApplyDynamicConDele;
import gov.ca.water.businessservice.IDynamicControlSvc;
import gov.ca.water.businessservice.ISeedDataSvc;
import gov.ca.water.businessservice.IXMLParsingSvc;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.bo.GUILinks2BO;
import gov.ca.water.calgui.bo.GUILinks4BO;
import gov.ca.water.calgui.bus_service.IScenarioSvc;
import gov.ca.water.calgui.bus_service.ITableSvc;
import gov.ca.water.calgui.bus_service.impl.ScenarioSvcImpl;
import gov.ca.water.calgui.bus_service.impl.TableSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * This class will apply the dynamic behaver which is controlled by the files
 * listed bellow.
 *
 * <pre>
 * 	1. TriggerForDynamicSelection.csv
 * 	2. TriggerForDynamicSelection.csv
 * </pre>
 *
 * @author Mohan
 */
public class ApplyDynamicConDeleImp implements IApplyDynamicConDele
{

	private static final Logger LOG = Logger.getLogger(ApplyDynamicConDeleImp.class.getName());
	private ISeedDataSvc seedDataSvc = SeedDataSvcImpl.getSeedDataSvcImplInstance();
	private IDynamicControlSvc dynamicControlSvc = DynamicControlSvcImpl.getDynamicControlSvcImplInstance();
	private ITableSvc tableSvc = TableSvcImpl.getTableSvcImplInstance();
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
	private IXMLParsingSvc xmlParsingSvc = XMLParsingSvcImpl.getXMLParsingSvcImplInstance();
	private SwingEngine swingEngine = xmlParsingSvc.getSwingEngine();
	private IScenarioSvc scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();
	private IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();

	@Override
	public void applyDynamicControlForListFromFile()
	{
		try
		{
			List<String> controlIds = fileSystemSvc.getFileData(Constant.DYNAMIC_CONTROL_FOR_STARTUP_FILENAME, false,
					line -> !line.startsWith(Constant.EXCLAMATION));

			List<JCheckBox> checkBoxList = controlIds.stream().filter(id -> swingEngine.find(id) instanceof JCheckBox)
													 .map(id -> (JCheckBox) swingEngine.find(id)).filter(
							JCheckBox::isSelected)
													 .collect(Collectors.toList());

			List<JRadioButton> radioButtonList = controlIds.stream()
														   .filter(id -> swingEngine.find(id) instanceof JRadioButton)
														   .map(id -> (JRadioButton) swingEngine.find(id)).filter(
							JRadioButton::isSelected)
														   .collect(Collectors.toList());
			for(JRadioButton jRadioButton : radioButtonList)
			{
				dynamicControlSvc.doDynamicControl(jRadioButton.getName(), jRadioButton.isSelected(),
						jRadioButton.isEnabled(), swingEngine);
			}
			for(JCheckBox jCheckBox : checkBoxList)
			{
				dynamicControlSvc.doDynamicControl(jCheckBox.getName(), jCheckBox.isSelected(), jCheckBox.isEnabled(),
						swingEngine);
			}
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex);
			errorHandlingSvc.businessErrorHandler(ex);
		}
	}

	@Override
	public void applyDynamicControl(String itemName, boolean isSelected, boolean isEnabled, boolean optionFromTheBox)
	{
		try
		{
			regulations(itemName, isSelected);
		}
		catch(CloneNotSupportedException ex)
		{
			LOG.error(ex);
			errorHandlingSvc.businessErrorHandler(new CalLiteGUIException("Unable to clone the table class.", ex));
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex);
			errorHandlingSvc.businessErrorHandler(ex);
		}
		if(itemName.equals("fac_ckb2") && !isSelected)
		{
			((JRadioButton) swingEngine.find("fac_rdb0")).setSelected(true);
		}
		dynamicControlSvc.doDynamicControl(itemName, isSelected, isEnabled, swingEngine);
		// This is for changing the "SV" and "Init" Files in the "Hydroclimate"
		// tab.
		List<String> controlIdForChangeOfSVInitFiles = Arrays.asList("run_rdbD1485", "run_rdbD1641", "run_rdbBO",
				"hyd_rdb2005", "hyd_rdb2030", "hyd_rdbCCEL", "hyd_rdbCCLL", "hyd_ckb1", "hyd_ckb2", "hyd_ckb3",
				"hyd_ckb4", "hyd_ckb5");
		if(isSelected && controlIdForChangeOfSVInitFiles.contains(itemName))
		{
			changeSVInitFilesAndTableInOperations(optionFromTheBox);
		}
		// This is for changing the "San Joaquin River Restoration flows" in the
		// "Regulations" tab.
		List<String> controlIdForChangeOfPanelInRegulations = Arrays.asList("hyd_rdb2005", "hyd_rdb2030", "hyd_rdbCCEL",
				"hyd_rdbCCLL");
		if(isSelected && controlIdForChangeOfPanelInRegulations.contains(itemName))
		{
			changePanelInRegulationsBasedOnId(itemName);
		}
		if(isSelected && itemName.equals("btnDSS_Auto"))
		{
			changeSVInitFilesAndTableInOperations(false);
		}
		if(isSelected && itemName.equals("btnDSS_Manual"))
		{

			if(((JTextField) swingEngine.find("txf_Manual_SV")).getText().toUpperCase().equals("NOT SET"))
			{
				// If the manual values have not been set, use the current auto
				// values
				((JTextField) swingEngine.find("txf_Manual_SV"))
						.setText(((JTextField) swingEngine.find("hyd_DSS_SV")).getText());
				((JTextField) swingEngine.find("txf_Manual_SV_F"))
						.setText(((JTextField) swingEngine.find("hyd_DSS_SV_F")).getText());
				((JTextField) swingEngine.find("txf_Manual_Init"))
						.setText(((JTextField) swingEngine.find("hyd_DSS_Init")).getText());
				((JTextField) swingEngine.find("txf_Manual_Init_F"))
						.setText(((JTextField) swingEngine.find("hyd_DSS_Init_F")).getText());

			}
			else
			{
				((JTextField) swingEngine.find("hyd_DSS_SV"))
						.setText(((JTextField) swingEngine.find("txf_Manual_SV")).getText());
				((JTextField) swingEngine.find("hyd_DSS_SV_F"))
						.setText(((JTextField) swingEngine.find("txf_Manual_SV_F")).getText());
				((JTextField) swingEngine.find("hyd_DSS_Init"))
						.setText(((JTextField) swingEngine.find("txf_Manual_Init")).getText());
				((JTextField) swingEngine.find("hyd_DSS_Init_F"))
						.setText(((JTextField) swingEngine.find("txf_Manual_Init_F")).getText());
			}
		}
		// (inefficiently) disable slider-linked text fields
		for(String s : xmlParsingSvc.getjTextFieldIdsForLinkedSliders())
		{
			swingEngine.find(s).setEnabled(false);
		}
	}

	/**
	 * This method is used for controlling the panel in the "Regulations" tab
	 * based on the "Hydroclimate" tab.
	 *
	 * @param itemName The item name of the control.
	 */
	private void changePanelInRegulationsBasedOnId(String itemName)
	{
		JRadioButton regrdb = (JRadioButton) swingEngine.find("rdbRegQS_UD");
		if(!regrdb.isSelected())
		{
			if(itemName.equals("hyd_rdb2005"))
			{
				regrdb = (JRadioButton) swingEngine.find("SJR_interim");
				regrdb.setSelected(true);
			}
			else
			{
				regrdb = (JRadioButton) swingEngine.find("SJR_full");
				regrdb.setSelected(true);
			}
			dynamicControlSvc.toggleEnComponentAndChildren(swingEngine.find("regpan2c"), false);
		}
	}

	@Override
	public void changeSVInitFilesAndTableInOperations(boolean optionFromTheBox)
	{
		try
		{
			List<String> labelValues = dynamicControlSvc.getLabelAndGuiLinks4BOBasedOnTheRadioButtons(swingEngine);
			GUILinks4BO gUILinks4BO = seedDataSvc.getObjByRunBasisLodCcprojCcmodelIds(labelValues.get(0));
			if(!((JRadioButton) swingEngine.find("btnDSS_Manual")).isSelected())
			{
				((JTextField) swingEngine.find("hyd_DSS_SV")).setText(gUILinks4BO.getSvFile());
				((JTextField) swingEngine.find("hyd_DSS_SV_F")).setText(gUILinks4BO.getfPartSV1());
				((JTextField) swingEngine.find("hyd_DSS_Init")).setText(gUILinks4BO.getInitFile());
				((JTextField) swingEngine.find("hyd_DSS_Init_F")).setText(gUILinks4BO.getfPartSV2());
			}
			if(optionFromTheBox)
			{
				String lookup = gUILinks4BO.getLookup();
				tableSvc.setWsidiForSWPFullFileName(Constant.MODEL_W2_WRESL_LOOKUP_DIR + "\\WSIDI\\"
						+ Constant.SWP_START_FILENAME + Constant.UNDER_SCORE + lookup + Constant.TABLE_EXT);
				tableSvc.setWsidiForCVPFullFileName(Constant.MODEL_W2_WRESL_LOOKUP_DIR + "\\WSIDI\\"
						+ Constant.CVP_START_FILENAME + Constant.UNDER_SCORE + lookup + Constant.TABLE_EXT);
				// Change WSI/DI Status Label
				JLabel jLabel = (JLabel) swingEngine.find("op_WSIDI_Status");
				jLabel.setText(labelValues.get(1) + Constant.UNEDITED_FORLABEL);
			}
		}
		catch(NullPointerException ex)
		{
			errorHandlingSvc.businessErrorHandler(
					new CalLiteGUIException("The data for geting the table name is wrong", ex));
		}
	}

	/**
	 * This method will handle all the functionality of "Regulations" tab.
	 *
	 * @param itemName   The item name which is selected.
	 * @param isSelected weather the item is selected or not.
	 * @throws CalLiteGUIException        see {@link CalLiteGUIException}
	 * @throws CloneNotSupportedException see {@link CloneNotSupportedException}
	 */
	private void regulations(String itemName, boolean isSelected)
			throws CalLiteGUIException, CloneNotSupportedException
	{
		List<GUILinks2BO> seedDataList = seedDataSvc.getRegulationsTabData();
		int[] regFlags = scenarioSvc.getRegulationoptions();
		String tableName = "";
		String optionName = "";
		Component scrRegValues = (this.swingEngine.find("scrRegValues"));
		JLabel labReg = (JLabel) (this.swingEngine.find("labReg"));

		boolean toDisplayMessage = true;
		try
		{
			// if (isSelected) {
			if(isSelected && itemName.equals(Constant.QUICK_SELECT_RB_D1485))
			{
				for(GUILinks2BO gUILinks2BO : seedDataList)
				{
					// if (seedDataBO.getD1485().equals(Constant.N_A)) {
					regFlags[Integer.parseInt(gUILinks2BO.getRegID())] = 3;
					// } else if (seedDataBO.getD1641().equals(Constant.N_A)) {
					// regFlags[Integer.parseInt(seedDataBO.getRegID())] = 1;
					// } else ifx`
					// (seedDataBO.getUserDefined().equals(Constant.N_A)) {
					// regFlags[Integer.parseInt(seedDataBO.getRegID())] = 4;
					// }
					if(!gUILinks2BO.getDataTables().equals(Constant.N_A))
					{
						scenarioSvc.removeUserDefinedTable(gUILinks2BO.getDataTables());
					}
				}
			}
			else if(isSelected && itemName.equals(Constant.QUICK_SELECT_RB_D1641)
					|| itemName.equals(Constant.QUICK_SELECT_RB_D1641_BO))
			{
				for(GUILinks2BO gUILinks2BO : seedDataList)
				{
					// if (seedDataBO.getD1641().equals(Constant.N_A)) {
					regFlags[Integer.parseInt(gUILinks2BO.getRegID())] = 1;
					// } else if (seedDataBO.getD1485().equals(Constant.N_A)) {
					// regFlags[Integer.parseInt(seedDataBO.getRegID())] = 3;
					// } else if
					// (seedDataBO.getUserDefined().equals(Constant.N_A)) {
					// regFlags[Integer.parseInt(seedDataBO.getRegID())] = 4;
					// }
					if(!gUILinks2BO.getDataTables().equals(Constant.N_A))
					{
						scenarioSvc.removeUserDefinedTable(gUILinks2BO.getDataTables());
					}
				}
			}
			else if(itemName.startsWith("ckbReg"))
			{
				GUILinks2BO gUILinks2BO = seedDataSvc.getObjByGuiId(itemName);
				makeRBVisible(gUILinks2BO);
				String panelId = dynamicControlSvc.getTriggerBOById(itemName).getAffectdeGuiId();
				String guiTableName = getTableNameFromTheComponent(swingEngine.find(panelId));

				((TitledBorder) ((JPanel) this.swingEngine.find(panelId)).getBorder())
						.setTitle(((JCheckBox) this.swingEngine.find(itemName)).getText());
				if(!isSelected)
				{
					((TitledBorder) ((JPanel) this.swingEngine.find(panelId)).getBorder())
							.setTitle(((JCheckBox) this.swingEngine.find(itemName)).getText() + " (not selected)");
					if(!gUILinks2BO.getDataTables().equals(Constant.N_A))
					{
						this.swingEngine.find(panelId).setVisible(true);
						labReg.setEnabled(true);
						this.swingEngine.find(panelId + "Placeholder").setVisible(false);
					}
					else
					{
						this.swingEngine.find(panelId).setVisible(false);
						this.swingEngine.find(panelId + "Placeholder").setVisible(true);
					}

				}
				Component c = this.swingEngine.find(itemName).getParent();
				((JRadioButton) swingEngine.find(Constant.PANEL_RB_D1641))
						.setText(c.getName().equals("Others") ? "Default" : "D-1641");

				this.swingEngine.find(panelId).repaint();
				if(isSelected)
				{
					int regId = Integer.parseInt(gUILinks2BO.getRegID());
					if(regFlags[regId] == 1)
					{
						if(gUILinks2BO.getD1641().equals(Constant.N_A))
						{
							((JRadioButton) swingEngine.find(Constant.PANEL_RB_D1641)).setSelected(true);
							optionName = Constant.D1641;
						}
						else if(gUILinks2BO.getD1485().equals(Constant.N_A))
						{
							((JRadioButton) swingEngine.find(Constant.PANEL_RB_D1485)).setSelected(true);
							optionName = Constant.D1485;
						}
						else if(gUILinks2BO.getUserDefined().equals(Constant.N_A))
						{
							regFlags[regId] = 2;
							optionName = Constant.USER_DEFINED;
						}
					}
					else if(regFlags[regId] == 3)
					{
						if(gUILinks2BO.getD1485().equals(Constant.N_A))
						{
							((JRadioButton) swingEngine.find(Constant.PANEL_RB_D1485)).setSelected(true);
							optionName = Constant.D1485;
						}
						else if(gUILinks2BO.getD1641().equals(Constant.N_A))
						{
							((JRadioButton) swingEngine.find(Constant.PANEL_RB_D1641)).setSelected(true);
							optionName = Constant.D1641;
						}
						else if(gUILinks2BO.getUserDefined().equals(Constant.N_A))
						{
							regFlags[regId] = 2;
							optionName = Constant.USER_DEFINED;
						}
					}
					else if(regFlags[regId] == 2)
					{
						((JRadioButton) swingEngine.find(Constant.PANEL_RB_USER_DEFIND)).setSelected(true);
						optionName = Constant.USER_DEFINED;
					}
					if(!gUILinks2BO.getDataTables().equals(Constant.N_A))
					{
						this.swingEngine.find(panelId).setVisible(true);
						this.swingEngine.find(panelId + "Placeholder").setVisible(false);
						labReg.setEnabled(true);
						tableName = gUILinks2BO.getDataTables();

						// Hide D1485 table for Min NDO and show explanation
						boolean customNDOState = (tableName.equals("gui_NDO_Flow")
								&& optionName.equals(Constant.D1485));
						scrRegValues.setVisible(!customNDOState);
						swingEngine.find("labReg_NDO").setVisible(customNDOState);
						swingEngine.find("labReg").setVisible(!customNDOState);

						labReg.setForeground(scrRegValues.getBackground());
						toDisplayMessage = loadTableToUI((JTable) this.swingEngine.find(guiTableName), tableName,
								regFlags[regId], gUILinks2BO, optionName);
					}
					else
					{

						this.swingEngine.find(panelId).setVisible(false);
						this.swingEngine.find(panelId + "Placeholder").setVisible(true);

						String valueToDisplay = Constant.VAMP_NOT_SELECTED_TEXT;
						if(itemName.equals(Constant.CKB_REG_VAMP) && isSelected)
						{
							valueToDisplay = Constant.VAMP_SELECTED_TEXT;
						}
						changeTheLabel(valueToDisplay);
					}
				}
			}
			else if(isSelected && itemName.startsWith("btnReg"))
			{
				JRadioButton radioButton = ((JRadioButton) this.swingEngine.find(itemName));
				TitledBorder titledBorder = (TitledBorder) ((JPanel) radioButton.getParent()).getBorder();
				GUILinks2BO guiLinks2BO = seedDataSvc
						.getObjByGuiId(xmlParsingSvc.getCompIdfromName(titledBorder.getTitle()));
				String guiTableName = getTableNameFromTheComponent(radioButton.getParent());
				tableName = guiLinks2BO.getDataTables();
				if(itemName.endsWith(Constant.D1641))
				{
					optionName = Constant.D1641;
				}
				else if(itemName.endsWith(Constant.D1485))
				{
					optionName = Constant.D1485;
				}
				else if(itemName.endsWith(Constant.USER_DEFINED))
				{
					optionName = Constant.USER_DEFINED;
				}
				int regId = Integer.parseInt(guiLinks2BO.getRegID());
				if(!tableName.equals(Constant.N_A))
				{
					// Hide D1485 table for Min NDO and show explanation
					boolean customNDOState = (tableName.equals("gui_NDO_Flow") && optionName.equals(Constant.D1485));
					scrRegValues.setVisible(!customNDOState);
					swingEngine.find("labReg_NDO").setVisible(customNDOState);
					swingEngine.find("labReg").setVisible(!customNDOState);

					labReg.setForeground(scrRegValues.getBackground());
					toDisplayMessage = loadTableToUI((JTable) this.swingEngine.find(guiTableName), tableName,
							regFlags[regId], guiLinks2BO, optionName);
				}
				else
				{
					String valueToDisplay = Constant.VAMP_NOT_SELECTED_TEXT;
					if(guiLinks2BO.getGuiId().equals(Constant.CKB_REG_VAMP))
					{
						valueToDisplay = Constant.VAMP_SELECTED_TEXT;
					}
					changeTheLabel(valueToDisplay);
				}
				/*
				 * setting the regFlag cann't be done in the above if else
				 * statement. Please see the getTable method in this class.
				 */
				if(itemName.endsWith(Constant.D1641))
				{
					regFlags[regId] = 1;
				}
				else if(itemName.endsWith(Constant.D1485))
				{
					regFlags[regId] = 3;
				}
				else if(itemName.endsWith(Constant.USER_DEFINED))
				{
					regFlags[regId] = 2;
				}
			}
			// }
		}
		catch(NullPointerException ex)
		{
			String message = "The control id " + itemName
					+ " doesn't have the proper data in TriggerForDynamicDisplay.csv.";
			if(itemName.startsWith("ckbReg_"))
			{
				message = message + "\nYou may be missing the entries '" + itemName + ",on,reg_panTab,on' and '"
						+ itemName + ",off,reg_panTab,off'";
			}
			errorHandlingSvc.businessErrorHandler(
					new CalLiteGUIException(message, ex));
		}
		if(toDisplayMessage)
		{
			scrRegValues.setVisible(false);
			labReg.setForeground((this.swingEngine.find("labReg2")).getForeground());
			labReg.setEnabled(true);
		}
	}

	/**
	 * This method will display the table to the ui.
	 *
	 * @param table       This is the object of the {@link JTable} to display the table
	 *                    data.
	 * @param tableName   The name of the table which should be displayed.
	 * @param regValue    The regId from gui_link2.csv file which tells us what is the
	 *                    option for user defined table.
	 * @param gUILinks2BO The seed data object for loading the table data.
	 * @param optionName  The shared option that is from the "Regulations" tab.
	 * @return Will return true if the loading of the table is successful
	 * @throws CalLiteGUIException        see {@link CalLiteGUIException}
	 * @throws CloneNotSupportedException see {@link CloneNotSupportedException}
	 */
	private boolean loadTableToUI(JTable table, String tableName, int regValue, GUILinks2BO gUILinks2BO,
								  String optionName) throws CalLiteGUIException, CloneNotSupportedException
	{
		DataTableModel dtm = getTable(tableName, regValue, gUILinks2BO, optionName);
		if(dtm == null)
		{
			changeTheLabel("The table is not available. The table name is " + tableName);
			return true;
		}
		table.setModel(dtm);
		table.setCellSelectionEnabled(true);
		DefaultTableCellRenderer renderer = (DefaultTableCellRenderer) table.getDefaultRenderer(Object.class);
		renderer.setHorizontalAlignment(JLabel.RIGHT);
		this.swingEngine.find(Constant.MAIN_FRAME_NAME).repaint();
		return false;
	}

	/**
	 * This method will set the label value.
	 *
	 * @param label The value to be set.
	 */
	private void changeTheLabel(String label)
	{
		JLabel lab = (JLabel) swingEngine.find("labReg");
		lab.setText(label);
		lab = (JLabel) swingEngine.find("labReg2");
		lab.setText(label);

	}

	/**
	 * This method is only for the "Regulations" tab tables.
	 * <p>
	 * This will return the {@link DataTableModel} object based on the values
	 * passed in.
	 *
	 * @param tableName   just the table name
	 * @param regValue    The regId from gui_link2.csv file which tells us what is the
	 *                    option for user defined table.
	 * @param gUILinks2BO seedData of the selected object.
	 * @param optionName  The shared option that is from the "Regulations" tab.
	 * @return Will return the {@link DataTableModel} object based on the values
	 * passed in.
	 * @throws CalLiteGUIException        see {@link CalLiteGUIException}
	 * @throws CloneNotSupportedException see {@link CloneNotSupportedException}
	 */
	private DataTableModel getTable(String tableName, int regValue, GUILinks2BO gUILinks2BO, String optionName)
			throws CalLiteGUIException, CloneNotSupportedException
	{
		DataTableModel dataTableModel = null;
		switch(optionName)
		{
			case Constant.D1641:
				dataTableModel = decideTableNameAndGetTable(tableName, gUILinks2BO, Constant.D1641);
				if(scenarioSvc.hasUserDefinedTable(tableName))
				{
					scenarioSvc.removeUserDefinedTable(tableName);
				}
				break;
			case Constant.D1485:
				dataTableModel = decideTableNameAndGetTable(tableName, gUILinks2BO, Constant.D1485);
				if(scenarioSvc.hasUserDefinedTable(tableName))
				{
					scenarioSvc.removeUserDefinedTable(tableName);
				}
				break;
			case Constant.USER_DEFINED:
				if(scenarioSvc.hasUserDefinedTable(tableName))
				{
					dataTableModel = scenarioSvc.getUserDefinedTable(tableName);
				}
				else
				{
					if(regValue == 1)
					{
						dataTableModel = decideTableNameAndGetTable(tableName, gUILinks2BO, Constant.D1641);
					}
					else if(regValue == 3)
					{
						dataTableModel = decideTableNameAndGetTable(tableName, gUILinks2BO, Constant.D1485);
					}
					else if(regValue == 2)
					{
						dataTableModel = decideTableNameAndGetTable(tableName, gUILinks2BO, Constant.USER_DEFINED);
					}
					if(dataTableModel != null)
					{
						dataTableModel = (DataTableModel) dataTableModel.clone();
						dataTableModel.setCellEditable(true);
						scenarioSvc.addUserDefinedTable(tableName, dataTableModel);
					}
				}
				break;
		}
		return dataTableModel;
	}

	/**
	 * This method is only for the "Regulations" tab tables.
	 * <p>
	 * This method will decide the table name based on the seedDataBo and type
	 * passed in and will return the object of {@link DataTableModel}.
	 *
	 * @param tableName   just the table name
	 * @param gUILinks2BO seedData of the selected object.
	 * @param type        The type from the "Regulations" tab.
	 * @return Will return the table data in {@link DataTableModel} object.
	 * @throws CalLiteGUIException see {@link CalLiteGUIException}
	 */
	private DataTableModel decideTableNameAndGetTable(String tableName, GUILinks2BO gUILinks2BO, String type)
			throws CalLiteGUIException
	{
		DataTableModel dtm = null;
		switch(type)
		{
			case Constant.D1485:
				if(gUILinks2BO.getD1485().equalsIgnoreCase(Constant.N_A))
				{
					dtm = tableSvc.getTable(tableName + Constant.DASH + Constant.D1485);
				}
				else if(gUILinks2BO.getD1641().equalsIgnoreCase(Constant.N_A))
				{
					dtm = tableSvc.getTable(tableName + Constant.DASH + Constant.D1641);
				}
				else
				{
					dtm = tableSvc.getTable(tableName);
				}
				break;
			case Constant.D1641:
				if(gUILinks2BO.getD1641().equalsIgnoreCase(Constant.N_A))
				{
					dtm = tableSvc.getTable(tableName + Constant.DASH + Constant.D1641);
				}
				else if(gUILinks2BO.getD1485().equalsIgnoreCase(Constant.N_A))
				{
					dtm = tableSvc.getTable(tableName + Constant.DASH + Constant.D1485);
				}
				else
				{
					dtm = tableSvc.getTable(tableName);
				}
				break;
			case Constant.USER_DEFINED:
				dtm = tableSvc.getTable(tableName);
				break;
		}
		return dtm;
	}

	/**
	 * This method will return the table name from the component which is passed
	 * in.
	 *
	 * @param component The component from which we need to get the table name.
	 * @return Will return the table name from the component which is passed in.
	 */
	private String getTableNameFromTheComponent(Component component)
	{
		if(component instanceof JTable)
		{
			return component.getName();
		}
		for(Component child : ((Container) component).getComponents())
		{
			String value = getTableNameFromTheComponent(child);
			if(!value.equals(""))
			{
				return value;
			}
		}
		return "";
	}

	/**
	 * This method will make the shared radio buttons in the "Regulations" tab
	 * visible based on the seedData passed in.
	 *
	 * @param gUILinks2BO It is used to show which radio button is to display.
	 */
	private void makeRBVisible(GUILinks2BO gUILinks2BO)
	{
		swingEngine.find(Constant.PANEL_RB_D1485).setVisible(false);
		swingEngine.find(Constant.PANEL_RB_D1641).setVisible(false);
		swingEngine.find(Constant.PANEL_RB_USER_DEFIND).setVisible(false);
		if(gUILinks2BO.getD1485().equals(Constant.N_A))
		{
			swingEngine.find(Constant.PANEL_RB_D1485).setVisible(true);
		}
		if(gUILinks2BO.getD1641().equals(Constant.N_A))
		{
			swingEngine.find(Constant.PANEL_RB_D1641).setVisible(true);
		}
		if(gUILinks2BO.getUserDefined().equals(Constant.N_A))
		{
			swingEngine.find(Constant.PANEL_RB_USER_DEFIND).setVisible(true);
		}
	}
}
