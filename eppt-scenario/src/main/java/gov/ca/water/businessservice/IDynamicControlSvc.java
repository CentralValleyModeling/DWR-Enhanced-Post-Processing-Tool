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

package gov.ca.water.businessservice;

import java.awt.Component;
import java.util.List;

import gov.ca.water.bo.TriggerBO;
import org.swixml.SwingEngine;

/**
 * This is the interface for applying the dynamic controls like enabling,
 * disabling etc.
 *
 * @author Mohan
 */
public interface IDynamicControlSvc
{

	/**
	 * This will apply all the dynamic controls which are from these two files.
	 *
	 * <pre>
	 * 1. TriggerForDymanicSelection.table
	 * 2. TriggerForDynamicDisplay.table
	 * </pre>
	 *
	 * @param itemName    The item name
	 * @param isSelected  Whether it is selected or not.
	 * @param isEnabled   Whether it is enabled or not.
	 * @param swingEngine The object of the GUI.
	 */
	void doDynamicControl(String itemName, boolean isSelected, boolean isEnabled, SwingEngine swingEngine);

	/**
	 * This will return the first {@link TriggerBO} object for the given
	 * {@code id}.
	 *
	 * @param id The id of the trigger.
	 * @return Will return the first {@link TriggerBO} object for the given
	 * {@code id}.
	 */
	TriggerBO getTriggerBOById(String id);

	/**
	 * This method is used for making components visible or non-visible and it's
	 * children.
	 *
	 * @param component The component.
	 * @param isVisible value to be set.
	 */
	void toggleVisComponentAndChildren(Component component, Boolean isVisible);

	/**
	 * This method is used for making components enabling and disabling the
	 * component and it's children.
	 *
	 * @param component The component.
	 * @param isEnable  to enable or not.
	 */
	void toggleEnComponentAndChildren(Component component, boolean isEnable);

	/**
	 * This method is used to decide what is the lookup value for Gui_Link4 and
	 * the label for the Operations tab. This will return the list of strings in
	 * which the 1st is the lookup value and the 2nd is the label.
	 *
	 * @param swingEngine The object of the GUI.
	 * @return Will return the list of strings in which the 1st is the lookup
	 * value and the 2nd is the label.
	 */
	List<String> getLabelAndGuiLinks4BOBasedOnTheRadioButtons(SwingEngine swingEngine);

	/**
	 * This is used to prevent the ItemListener.
	 *
	 * @return Will return the roe trigger value.
	 */
	boolean isPreventRoeTrigger();
}
