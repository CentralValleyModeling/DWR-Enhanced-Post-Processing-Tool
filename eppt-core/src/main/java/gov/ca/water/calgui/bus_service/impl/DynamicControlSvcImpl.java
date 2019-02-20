/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service.impl;

import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.*;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.TriggerBO;
import gov.ca.water.calgui.bus_service.IDynamicControlSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * This is the class for applying the dynamic controls.
 *
 * @author Mohan
 */
public final class DynamicControlSvcImpl implements IDynamicControlSvc
{
	private static final Logger LOG = Logger.getLogger(DynamicControlSvcImpl.class.getName());
	private static IDynamicControlSvc dynamicControlSvc;
	private IFileSystemSvc fileSystemSvc;
	private Map<String, List<TriggerBO>> triggerMapForEnableDisable;
	private Map<String, List<TriggerBO>> triggerMapForCheckUncheck;
	private IErrorHandlingSvc errorHandlingSvc;
	private boolean preventRoeTrigger = false;

	/*
	 * This will load TriggerForDymanicSelection.table and
	 * TriggerForDynamicDisplay.table files into memory.
	 */
	private DynamicControlSvcImpl()
	{
		LOG.info("Building DynamicControlSvcImpl Object.");
		this.fileSystemSvc = new FileSystemSvcImpl();
		this.triggerMapForEnableDisable = new HashMap<String, List<TriggerBO>>();
		this.triggerMapForCheckUncheck = new HashMap<String, List<TriggerBO>>();
		this.errorHandlingSvc = new ErrorHandlingSvcImpl();
		/*
		 * This method will load the TriggerForDynamicDisplay.table file into
		 * the memory. The data really is if one component is selected then this
		 * will do two process.
		 *
		 * 1. on and off which will enable and disable based on the data. 2.
		 * show and hide which will make the component visible and not visible.
		 *
		 */
		loadTriggerTable(Constant.TRIGGER_ENABLE_DISABLE_FILENAME, this.triggerMapForEnableDisable);
		/*
		 * This method will load the TriggerForDymanicSelection.table file into
		 * the memory. The data really is if one JCheckBox and JRadioButton is
		 * selected then all the other AFFECTED once will be selected or
		 * de-selected based on the data.
		 */
		loadTriggerTable(Constant.TRIGGER_CHECK_UNCHECK_FILENAME, this.triggerMapForCheckUncheck);
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static IDynamicControlSvc getDynamicControlSvcImplInstance()
	{
		if(dynamicControlSvc == null)
		{
			dynamicControlSvc = new DynamicControlSvcImpl();
		}
		return dynamicControlSvc;
	}

	/**
	 * This will tell whether the line is comment or not.
	 *
	 * @param line The line to be checked.
	 * @return Will return true if the line is not commented.
	 */
	private static boolean isNotComments(String line)
	{
		return !line.startsWith(Constant.EXCLAMATION);
	}

	/**
	 * This method will load the file and convert the data into the map.
	 *
	 * @param fileName   The file Name
	 * @param triggerMap The list of trigger map.
	 */
	private void loadTriggerTable(String fileName, Map<String, List<TriggerBO>> triggerMap)
	{
		List<String> triggerStrList;
		String errorStr = "";
		String mapKey = "";
		try
		{
			triggerStrList = fileSystemSvc.getFileData(fileName, true, DynamicControlSvcImpl::isNotComments);
			for(String triggerStr : triggerStrList)
			{
				errorStr = triggerStr;
				String[] list = triggerStr.split(Constant.DELIMITER);
				TriggerBO triggerBO = new TriggerBO(list[0], list[1], list[2], list[3]);
				mapKey = list[0] + Constant.DASH + list[1];
				if(triggerMap.containsKey(mapKey))
				{
					triggerMap.get(mapKey).add(triggerBO);
				}
				else
				{
					List<TriggerBO> temp = new ArrayList<TriggerBO>();
					temp.add(triggerBO);
					triggerMap.put(mapKey, temp);
				}
			}
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + fileName + "\" has a corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOG.error(errorMessage, ex);
			errorHandlingSvc.displayErrorMessageBeforeTheUI(new CalLiteGUIException(errorMessage, ex, true));
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex.getMessage(), ex);
			errorHandlingSvc.displayErrorMessageBeforeTheUI(ex);
		}
	}

	@Override
	public void doDynamicControl(String itemName, boolean isSelected, boolean isEnabled, SwingEngine swingEngine)
	{
		List<TriggerBO> listForEnableDisable = this.triggerMapForEnableDisable
				.get(itemName + Constant.DASH + booleanToStringOnOff(isSelected));
		if(listForEnableDisable != null)
		{
			for(TriggerBO triggerBO : listForEnableDisable)
			{

				if(triggerBO.getAffectdeAction().equalsIgnoreCase("show")
						|| triggerBO.getAffectdeAction().equalsIgnoreCase("hide"))
				{
					toggleVisComponentAndChildren(swingEngine.find(triggerBO.getAffectdeGuiId()),
							stringShowHideToBoolean(triggerBO.getAffectdeAction()));
				}
				else
				{
					if(isEnabled)
					{
						toggleEnComponentAndChildren(swingEngine.find(triggerBO.getAffectdeGuiId()),
								stringOnOffToBoolean(triggerBO.getAffectdeAction()));
					}
				}
			}
		}
		List<TriggerBO> listForCheckUncheck = this.triggerMapForCheckUncheck
				.get(itemName + Constant.DASH + Boolean.toString(isSelected).toUpperCase());
		if(listForCheckUncheck != null)
		{
			for(TriggerBO triggerBoForCheck : listForCheckUncheck)
			{
				if(triggerBoForCheck.getAffectdeGuiId().equals("ckbReg_X2ROE"))
				{
					this.preventRoeTrigger = true;
					setComponentSelected(swingEngine.find(triggerBoForCheck.getAffectdeGuiId()),
							Boolean.valueOf(triggerBoForCheck.getAffectdeAction()));
					this.preventRoeTrigger = false;
				}
				else
				{
					setComponentSelected(swingEngine.find(triggerBoForCheck.getAffectdeGuiId()),
							Boolean.valueOf(triggerBoForCheck.getAffectdeAction()));
				}
			}
		}
	}

	/**
	 * This method will only select and deselect the JCheckBox and JRadioButton.
	 *
	 * @param component The component to be set.
	 * @param isSelect  whether it is selected or not.
	 */
	private void setComponentSelected(Component component, boolean isSelect)
	{
		if(component instanceof JCheckBox)
		{
			((JCheckBox) component).setSelected(isSelect);
		}
		else if(component instanceof JRadioButton)
		{
			((JRadioButton) component).setSelected(isSelect);
		}
	}

	@Override
	public void toggleVisComponentAndChildren(Component component, Boolean isVisible)
	{
		component.setVisible(isVisible);
		for(Component child : ((Container) component).getComponents())
		{
			toggleVisComponentAndChildren(child, isVisible);
		}
	}

	@Override
	public void toggleEnComponentAndChildren(Component component, boolean isEnable)
	{
		component.setEnabled(isEnable);
		for(Component child : ((Container) component).getComponents())
		{
			toggleEnComponentAndChildren(child, isEnable);
		}
	}

	/**
	 * This will convert boolean to on and off string and return it. true = on
	 *
	 * @param value The value of the boolean to convert.
	 * @return return on if the value is true.
	 */
	private String booleanToStringOnOff(boolean value)
	{
		return value ? "on" : "off";
	}

	/**
	 * This will convert the string show and hide to boolean value and return
	 * it. show = true
	 *
	 * @param value The value of the string to convert.
	 * @return return true if the value is show.
	 */
	private boolean stringShowHideToBoolean(String value)
	{
		return value.equals("show");
	}

	/**
	 * This will convert the on and off string to boolean value and return it.
	 * on = true
	 *
	 * @param value The value of the string to convert.
	 * @return return true if the value is on.
	 */
	private boolean stringOnOffToBoolean(String value)
	{
		return value.equals("on");
	}

	@Override
	public TriggerBO getTriggerBOById(String id)
	{
		return this.triggerMapForEnableDisable.get(id + Constant.DASH + "on") != null
				? this.triggerMapForEnableDisable.get(id + Constant.DASH + "on").get(0) : null;
	}

	@Override
	public List<String> getLabelAndGuiLinks4BOBasedOnTheRadioButtons(SwingEngine swingEngine)
	{
		String lookup = "";
		String label = "";
		if(((JRadioButton) swingEngine.find("run_rdbD1641")).isSelected())
		{
			lookup = "1";
			label = "D1641";
		}
		else if(((JRadioButton) swingEngine.find("run_rdbBO")).isSelected())
		{
			lookup = "2";
			label = "BO";
		}
		else if(((JRadioButton) swingEngine.find("run_rdbD1485")).isSelected())
		{
			lookup = "3";
			label = "D1485";
		}
		if(((JRadioButton) swingEngine.find("hyd_rdb2005")).isSelected())
		{
			lookup = lookup + "110";
			label = label + "-Current LOD";
		}
		else if(((JRadioButton) swingEngine.find("hyd_rdb2030")).isSelected())
		{
			lookup = lookup + "210";
			label = label + "-Future LOD";
		}
		else
		{
			if(((JRadioButton) swingEngine.find("hyd_rdbCCEL")).isSelected())
			{
				lookup = lookup + "31";
				label = label + "-Early CC";
			}
			else if(((JRadioButton) swingEngine.find("hyd_rdbCCLL")).isSelected())
			{
				lookup = lookup + "41";
				label = label + "-Late CC";
			}
			if(((JRadioButton) swingEngine.find("hyd_ckb1")).isSelected())
			{
				lookup = lookup + "1";
				label = label + "1";
			}
			else if(((JRadioButton) swingEngine.find("hyd_ckb2")).isSelected())
			{
				lookup = lookup + "2";
				label = label + "2";
			}
			else if(((JRadioButton) swingEngine.find("hyd_ckb3")).isSelected())
			{
				lookup = lookup + "3";
				label = label + "3";
			}
			else if(((JRadioButton) swingEngine.find("hyd_ckb4")).isSelected())
			{
				lookup = lookup + "4";
				label = label + "4";
			}
			else if(((JRadioButton) swingEngine.find("hyd_ckb5")).isSelected())
			{
				lookup = lookup + "5";
				label = label + "5";
			}
			else
			{
				lookup = lookup + "0";
				label = label + "0";
			}
		}
		return Arrays.asList(lookup, label);
	}

	@Override
	public boolean isPreventRoeTrigger()
	{
		return preventRoeTrigger;
	}
}