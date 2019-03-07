/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice.impl;

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.*;

import gov.ca.water.businessservice.ISeedDataSvc;
import gov.ca.water.businessservice.IVerifyControlsDele;
import gov.ca.water.businessservice.IXMLParsingSvc;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bus_service.IScenarioSvc;
import gov.ca.water.calgui.bus_service.impl.ScenarioSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import org.swixml.SwingEngine;

/**
 * This class we verify the data before the application to start.
 *
 * @author Mohan
 */
public class VerifyControlsDeleImp implements IVerifyControlsDele
{
	IXMLParsingSvc xmlParsingSvc = XMLParsingSvcImpl.getXMLParsingSvcImplInstance();
	private ISeedDataSvc seedDataSvc = SeedDataSvcImpl.getSeedDataSvcImplInstance();
	private IScenarioSvc scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();

	@Override
	public void verifyTheDataBeforeUI(String fileName) throws EpptInitializationException
	{
		this.verifyGUIToSeedData();
		this.verifyCLSToGUIIds(fileName);
	}

	/**
	 * This method will verify the Gui id with the SeedData(gui_link2.csv).
	 */
	private void verifyGUIToSeedData() throws EpptInitializationException
	{
		Set<String> compIds = xmlParsingSvc.getIdFromXML();
		SwingEngine swingEngine = xmlParsingSvc.getSwingEngine();
		List<String> controlIds = compIds.stream()
										 .filter((id) -> swingEngine.find(id) instanceof JRadioButton
												 || swingEngine.find(id) instanceof JCheckBox || swingEngine.find(
												 id) instanceof JTextField)
										 .collect(Collectors.toList());
		List<String> errorControlsIds = new ArrayList<>();
		// The list is for radio buttons.
		List<String> removeList = new ArrayList<>(Arrays.asList("run_rdbProb", "rdbRegQS_UD", "btnDSS_Auto",
				"rdbRegQS_1641BO", "btnDSS_Manual", "rdbRegQS_D1485", "op_rdb3", "rdbRegQS_D1641", "run_rdbDet"));
		// The list is for Text Field.
		removeList.addAll(Arrays.asList("hyd_DSS_Init_F", "run_txfoDSS", "run_txfScen", "fac_txf18", "fac_txf19",
				"fac_txf16", "fac_txf17", "fac_txf15", "fac_txf13", "fac_txf23", "hyd_DSS_SV", "fac_txf21", "fac_txf22",
				"hyd_DSS_SV_F", "fac_txf2", "fac_txf1", "fac_txf7", "fac_txf4", "fac_txf3", "fac_txf6", "fac_txf5",
				"hyd_DSS_Init"));
		// The list is for Check Box.
		removeList.addAll(Arrays.asList("fac_ckb1", "fac_ckb4", "fac_ckb5", "fac_ckb8", "fac_ckb7", "fac_ckb9",
				"fac_ckb6", "fac_ckb30", "fac_ckb31", "fac_ckb32", "fac_ckb33", "fac_ckb38", "fac_ckb20", "fac_ckb21",
				"fac_ckb22", "fac_ckb23", "fac_ckb24", "fac_ckb25", "fac_ckb26", "fac_ckb27", "fac_ckb28", "fac_ckb29",
				"fac_ckb10", "fac_ckb13", "fac_ckb14", "fac_ckb15", "fac_ckb16", "fac_ckb17", "fac_ckb18", "fac_ckb19",
				"fac_ckb42", "fac_ckb43", "fac_ckb45", "Vern_RPA"));
		getTheIds(swingEngine.find("Reporting"), removeList);
		getTheIds(swingEngine.find("Custom"), removeList);
		getTheIds(swingEngine.find("schematics"), removeList);
		getTheIds(swingEngine.find("dataAnalysis"), removeList);
		getTheIds(swingEngine.find("sandbox"), removeList);

		for(String id : controlIds)
		{
			if(!seedDataSvc.hasSeedDataObject(id))
			{
				errorControlsIds.add(id);
			}
		}
		errorControlsIds.removeAll(removeList);
		if(!errorControlsIds.isEmpty())
		{
			StringBuilder buffer = new StringBuilder();
			buffer.append(
					"There is no GUI_Links2.csv record for the following control IDs defined in the GUI XML files: "
							+ Constant.NEW_LINE);
			errorControlsIds.forEach((id) -> buffer.append(" - " + id + Constant.NEW_LINE));
			buffer.append(Constant.NEW_LINE + Constant.NEW_LINE
					+ "Add records to GUI_Links2.csv if the control is valid, or modify the exclusion list in VerifyControlsDeleImp.verifyGUIToSeedData and recompile.");
			throw new EpptInitializationException(buffer.toString());
		}
	}

	/**
	 * This method will verify the Gui id with the cls file id's. In this we
	 * only check the controls from the cls file.
	 *
	 * @param fileName This is the file name with path of the cls file.
	 */
	private void verifyCLSToGUIIds(String fileName) throws EpptInitializationException
	{
		List<String> controlStrList = new ArrayList<>();
		List<String> dataTableModelStrList = new ArrayList<>();
		List<String> regulationoptionsStr = new ArrayList<>();
		List<String> wsidiStatusStr = new ArrayList<>();
		// Read in the cls file data.
		scenarioSvc.getCLSData(fileName, controlStrList, dataTableModelStrList, regulationoptionsStr, wsidiStatusStr);
		Set<String> guiIds = xmlParsingSvc.getIdFromXML();
		List<String> controlStrings = controlStrList.stream().map((key) -> key.split(Constant.PIPELINE_DELIMITER)[0])
													.collect(Collectors.toList());
		controlStrings.removeAll(guiIds);
		List<String> tempList = Arrays.asList("run_txfDir", "hyd_ANNKM", "hyd_DSS_Index", "fac_ckb1", "fac_ckb4",
				"fac_ckb5", "ckbReg5", "ckbReg12", "ckbReg16", "ckbReg17", "btnRegD1641", "btnRegD1485", "btnRegUD",
				"op_rdb4", "op_rdb5", "op_ckbSWP", "op_ntfSWP", "op_ckbCWP", "op_ntfCWP1", "op_ntfCWP2",
				"txf_Manual_SV", "txf_Manual_SV_F", "txf_Manual_Init", "txf_Manual_Init_F");
		controlStrings.removeAll(tempList);
		if(!controlStrings.isEmpty())
		{
			StringBuilder buffer = new StringBuilder();
			buffer.append(
					"The CLS File has these Control Ids but they are missing from the xml file. Do you want to continue?"
							+ Constant.NEW_LINE);
			controlStrings.forEach(id -> buffer.append(id + Constant.NEW_LINE));
			throw new EpptInitializationException(buffer.toString());
		}
		List<String> regData = new ArrayList<>(
				Arrays.asList(regulationoptionsStr.get(0).split(Constant.PIPELINE_DELIMITER)));
		List<String> numb = Arrays.asList("0", "1", "2", "3", "4");
		regData.removeAll(numb);
		if(!regData.isEmpty())
		{
			StringBuilder buffer = new StringBuilder();
			buffer.append("The CLS File has these reg Ids which are wrong please correct them." + Constant.NEW_LINE);
			controlStrings.forEach((id) -> buffer.append(id).append(Constant.NEW_LINE));
			throw new EpptInitializationException(buffer.toString());
		}
	}

	/**
	 * This method will get name of the {@code component} and itschildren names.
	 *
	 * @param component  The starting component.
	 * @param removeList The list of names of the {@code component} and it's children.
	 */
	private void getTheIds(Component component, List<String> removeList)
	{
		removeList.add(component.getName());
		for(Component child : ((Container) component).getComponents())
		{
			getTheIds(child, removeList);
		}
	}
}
