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
