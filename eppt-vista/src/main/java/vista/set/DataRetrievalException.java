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
 * Thrown when data set in a data reference cannot be retrieved. This can occur
 * from non-existence of server, file, pathname or timewindow.
 *
 * @author Nicky Sandhu
 * @version $Id: DataRetrievalException.java,v 1.1 2003/10/02 20:49:20 redwood
 * Exp $
 */
public class DataRetrievalException extends RuntimeException
{
	/**
	 * Exception with message.
	 */
	public DataRetrievalException(String msg)
	{
		super(msg);
	}
}
