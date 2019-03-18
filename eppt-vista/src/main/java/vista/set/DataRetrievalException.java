/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;


/**
 * Thrown when data set in a data reference cannot be retrieved. This can occur
 * from non-existence of server, file, pathname or timewindow.
 * 
 * @author Nicky Sandhu
 * @version $Id: DataRetrievalException.java,v 1.1 2003/10/02 20:49:20 redwood
 *          Exp $
 */
public class DataRetrievalException extends RuntimeException {
	/**
	 * Exception with message.
	 */
	public DataRetrievalException(String msg) {
		super(msg);
	}
}
