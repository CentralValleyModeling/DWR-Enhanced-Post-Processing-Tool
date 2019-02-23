/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: IncompatibleTypeException.java,v 1.2 1998/11/05 17:48:17
 *          nsandhu Exp $
 */
public class IncompatibleTypeException extends RuntimeException {
	/**
   *
   */
	public IncompatibleTypeException(String type1, String type2) {
		super("Incompatible types  " + type1 + " & " + type2
				+ " for time series");
	}
}
