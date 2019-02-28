/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice;

import java.util.List;
import java.util.Set;

import org.swixml.SwingEngine;

/**
 * This is the interface for parsing the GUI.xml into SwingEngine.
 *
 * @author Mohan
 */
public interface IXMLParsingSvc
{

	/**
	 * This method will return the object of current {@link SwingEngine}.
	 *
	 * @return Will return the {@link SwingEngine}.
	 */
	SwingEngine getSwingEngine();

	/**
	 * This will return the control Id which are Visible in the
	 * {@link SwingEngine} Object.
	 *
	 * @return Will return the control Id which are Visible in the
	 * {@link SwingEngine} Object.
	 */
	Set<String> getIdFromXML();

	/**
	 * If you pass in the name of the component it will return the id of it.
	 *
	 * @param name Name of the component.
	 * @return Will return the Id of the component passed in.
	 */
	String getCompIdfromName(String name);

	/**
	 * This will return the table id which are defined by the user. It will
	 * exclude some default table ids.
	 *
	 * @return Will return the table id which are defined by the user.
	 */
	List<String> getNewUserDefinedTables();

	/**
	 * This is used to get all the Text field ids.
	 *
	 * @return Will return all the text fields ids.
	 */

	List<String> getjTextFieldIds();

	/**
	 * This is used to get all the Text field ids.
	 *
	 * @return Will return all the text fields ids which are linked to from a
	 * JLinkedSlider.
	 */

	List<String> getjTextFieldIdsForLinkedSliders();

	/**
	 * This is used to get all the regulation checkbox IDs.
	 *
	 * @return Will return all the text fields ids.
	 */
	List<String> getjCheckBoxIDs();

	/**
	 * This method will tell whether the id is from the result part or not.
	 *
	 * @param compId The ID of the component.
	 * @return Will check whether the id is from the result part of the ui.
	 */
	boolean checkIsItFromResultPart(String compId);
}
