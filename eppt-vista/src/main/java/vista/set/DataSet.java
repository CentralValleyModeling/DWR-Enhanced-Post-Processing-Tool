/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * An abstract class for encapsulating DataSet for use in plotting. This would
 * enable different data sets to implement this interface and use the plotting
 * capabilities as long as these functions were properly implemented along with
 * DataSetElement and DataSetIterator
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: DataSet.java,v 1.1 2003/10/02 20:49:21 redwood Exp $
 * @see DataSetElement
 * @see DataSetIterator
 */
public interface DataSet extends Named, java.io.Serializable
{
	/**
	 * returns the number of elements in the dataset
	 */
	int size();

	/**
	 * gets element at index i
	 */
	DataSetElement getElementAt(int i);

	/**
	 * sets element at index i
	 */
	void putElementAt(int i, DataSetElement dse);

	/**
	 * Return an iterator positioned at my first item.
	 */
	DataSetIterator getIterator();

	/**
	 * returns a name for this DataSet to be used to identify it.
	 */
	String getName();

	/**
	 * sets the name to identify the data set.
	 */
	void setName(String name);

	/**
	 * An object attached to this data set which contains descriptive
	 * information of the underlying data.
	 */
	DataSetAttr getAttributes();

	/**
	 * An object attached to this data set which contains descriptive
	 * information of the underlying data.
	 */
	void setAttributes(DataSetAttr attr);

	/**
	 * true if data set is flagged
	 */
	boolean isFlagged();

	/**
	 * adds flags to the data set only if the data set is not flagged.
	 */
	void addFlags();
}
