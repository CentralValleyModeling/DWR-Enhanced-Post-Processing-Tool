/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.tech_service;

import java.util.List;
import java.util.function.Predicate;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import org.w3c.dom.Document;

/**
 * This is the interface for File Handling like reading and saving.
 *
 * @author Mohan
 */
public interface IFileSystemSvc
{

	/**
	 * This will take the file name and return the lines in the file as list of
	 * strings.
	 *
	 * @param fileName   Full path and the file Name.
	 * @param isRequired If file is required for the application to start and if the
	 *                   file is missing then it will throw the exception.
	 * @return return the lines in the file as list.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	List<String> getFileData(String fileName, boolean isRequired) throws CalLiteGUIException;

	/**
	 * This will take the file name and read the lines and then filter them
	 * based on the selector and return the lines.
	 *
	 * @param fileName   Full path and the file Name.
	 * @param isRequired If file is required for the application to start and if the
	 *                   file is missing then it will exit the program. If not throw
	 *                   the exception.
	 * @param selector   The function which decide which lines to be in the result.
	 * @return Will return the list by removing all the line which don't satisfy
	 * the selector passed in.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	List<String> getFileData(String fileName, boolean isRequired, Predicate<String> selector)
			throws CalLiteGUIException;

	/**
	 * This will remove all the comment lines from the table file except the
	 * header line.
	 *
	 * @param fileName Full path and the file Name.
	 * @return return the lines in the file as list after removing the comments.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	List<String> getFileDataForTables(String fileName) throws CalLiteGUIException;

	/**
	 * This will save the given data into the given file.
	 *
	 * @param fileName Full path and the file Name.
	 * @param data     The data which is writen to the file.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	void saveDataToFile(String fileName, String data) throws CalLiteGUIException;

	/**
	 * This will generate the Document from the XML file.
	 *
	 * @return Will return the generated document from the XML file.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	Document getXMLDocument() throws CalLiteGUIException;

	/**
	 * This method will return the lookup string from the full file path. This
	 * is only used for SWP and CVP. This will return user defined if the file
	 * name does't have any lookup value
	 *
	 * @param fullName Full path and the file Name.
	 * @return Will return the lookup string.
	 */
	String getTheLookupFromTheFullFileName(String fullName);

	/**
	 * This method will return the lookup string from the file name. This is
	 * only used for SWP and CVP. This will return user defined if the file name
	 * does't have any lookup value
	 *
	 * @param fileName The file Name
	 * @return Will return the lookup string.
	 */
	String getLookupFromTheFileName(String fileName);
}