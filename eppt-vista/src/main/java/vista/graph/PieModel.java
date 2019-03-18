/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

/**
 * @author Nicky Sandhu
 * @version $Id: PieModel.java,v 1.1 2003/10/02 20:49:06 redwood Exp $
 */
public interface PieModel
{
	/**
	 * get the label for this sector
	 */
	String getLabel();

	/**
	 * get the value for this sector
	 */
	double getValue();

	/**
	 * get the reference object if any for this sector
	 */
	Object getReferenceObject();
}
