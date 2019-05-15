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
package vista.db.dss;

import java.rmi.Remote;
import java.rmi.RemoteException;

import vista.set.DataReference;
import vista.set.DataSet;

/**
 * Implements the server side of the RMI call
 *
 * @author Nicky Sandhu
 * @version $Id: DSSRemoteClient.java,v 1.1 2003/10/02 20:48:45 redwood Exp $
 */
public interface DSSRemoteClient extends Remote
{
	/**
	 * sends request for listing of dss files on server and returns with an
	 * array of string of that list
	 */
	String[] getListing(String directory) throws RemoteException;

	/**
	 * returns an array of string where each element is a line in the catalog
	 * file
	 */
	String[] getCatalog(String dssFile, boolean doFreshCatalog)
			throws RemoteException;

	/**
	 * returns a data set for the given data reference
	 */
	DataSet getData(DataReference ref, boolean retrieveFlags)
			throws RemoteException;

	/**
	 * stores a data set and its attributes
	 */
	void storeData(DataSet ds, String filename, String path,
				   long startJulmin, long endJulmin, boolean storeFlags, String id,
				   String passwd) throws RemoteException;
}
