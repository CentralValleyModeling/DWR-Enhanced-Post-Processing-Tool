/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.db.dss;

import java.util.ArrayList;
import javax.swing.*;

import vista.set.DataReference;
import vista.set.Group;
import vista.set.GroupProxy;
import vista.set.Pathname;

/**
 * This is the default group created from a .dss file. It is a proxy for the
 * group operations initalizing itself before any operations on it are done. For
 * the proxy only the servername and the filename are needed. When the listing
 * of the data references in this group is requested the references are created
 * dynamically from the server and file names.
 * 
 * @author Nicky Sandhu
 * @version $Id: DSSGroup.java,v 1.1 2003/10/02 20:48:45 redwood Exp $
 */
class DSSGroup extends GroupProxy {
	/**
	 * creates a group which can be then initialized from the given server,
	 * port, filename information...
	 */
	public DSSGroup(String server, String dssFile) {
		_serverName = server.trim();
		_filename = dssFile.trim();
		super.setName(_serverName + DSSDataReference.SEPARATOR + _filename);
		_redoCatalog = false;
	}

	/**
	 * do a catalog listing after doing a fresh catalog
	 */
	public void reload() {
		super.reload();
		_redoCatalog = true;
	}

	/**
	 * returns the initialized group object.
	 */
	protected Group getInitializedGroup() {
		// check dssFilename? should throw an exception?
		int portno = -1;
		if (_filename == null)
			return null;
		if (!DSSUtil.isValidDSSFile(_filename))
			return null;
		// get catalog reader
		DSSCatalogReader reader = DSSUtil.createCatalogReader(_serverName,
				_filename, _redoCatalog);
		if (reader == null)
			throw new RuntimeException(
					"Error opening catalog file: Corrupt catalog file? -> "
							+ _filename);
		_redoCatalog = false;
		// create an array of data references with pathname and default window
		ArrayList<DataReference> array = new ArrayList<DataReference>();
		JProgressBar pbar = vista.app.SessionFrame.getProgressBar();
		int npaths = reader.getNumberOfPaths();
		if (pbar != null) {
			pbar.setMinimum(0);
			pbar.setMaximum(npaths);
			try {
				pbar.update(pbar.getGraphics());
			} catch (Exception e) {
			}
		}
		int index = 0;
		int updateCount = 20;// Math.max(npaths/20, 20);
		// System.out.println("Number of paths = " + npaths);
		while (reader.hasMoreElements()) {
			Pathname p = (Pathname) reader.nextElement();
			array.add(DSSUtil.createDataReference(_serverName, _filename, p));
			if (pbar != null) {
				pbar.setValue(index++);
				// System.out.println("Index = " + index);
				try {
					if (index % updateCount == 0)
						pbar.update(pbar.getGraphics());
				} catch (Exception e) {
				}
			}
		}
		try {
			if (pbar != null) {
				pbar.setValue(0);
				pbar.update(pbar.getGraphics());
			}
		} catch (Exception e) {
		}

		// create group with name of dss file and array of data references...
		return Group.createGroup(_filename, array);
	}

	/**
	 * server & filename to use for initializing this group...
	 */
	private String _serverName, _filename;
	private boolean _redoCatalog;
}
