/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.app;

import javax.swing.table.TableModel;

/**
 * The input table data. Each table has a name, a header ( column description ), and rows. Optionally
 * a comment may also be present.
 *
 * @author Nicky Sandhu ,Armin Munevar
 * @version $Id: InputTableData.java,v 1.1.2.5 2001/05/31 19:58:36 jfenolio Exp $
 */
public interface InputTableData extends TableModel
{
	/**
	 * the name of this table
	 */
	String getTableName();

	/**
	 * the header array for column names
	 */
	String[] getHeaders();

	/**
	 * the number of rows of data
	 */
	int getNumberOfRows();

	/**
	 * the row data @ the ith row, the length of which must match the # of cols
	 */
	String[] getRowData(int i);

	/**
	 * adds a row to this table
	 */
	void addRow(String[] rowData);

	/**
	 * inserts a row in this table
	 */
	void insertRow(int i, String[] rowData);

	/**
	 * removes a row from this table
	 */
	void removeRow(int i);

	/**
	 * gets comment string
	 */
	String getComment();

	/**
	 * sets comment string
	 */
	void setComment(String comment);

	/**
	 * loads from another file
	 */
	void load(String file);

	/*
	 * saves to given file
	 */
	void save(String file);

	/**
	 * the current input file or last file to which saved to or loaded from
	 */
	String getInputFile();

	/**
	 * sort on given column
	 */
	void sort(int column);

	/**
	 * reverse the elements
	 */
	void reverse();

	/**
	 * true if data has been modified since last save
	 */
	boolean needsSaving();

}


