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

import java.util.ArrayList;

import vista.set.Group;
import vista.set.Session;
import vista.set.SessionProxy;

/**
 * A proxy for a session existing as a directory on a remote server which has a
 * list of .dss files.
 */
class DSSSession extends SessionProxy
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	/**
	 * The name of the data server
	 */
	private String _serverName;
	/**
	 * The default session directory containing the default groups (.dss files)
	 */
	private String _directory;
	/**
	 * A session created from a server and directory spec.
	 */
	public DSSSession(String serverName, String directory)
	{
		_serverName = serverName;
		_directory = directory;
		super.setName(_serverName + DSSDataReference.SEPARATOR + _directory);
	}

	/**
	 * returns the initialized session
	 */
	protected Session getInitializedSession()
	{
		String[] dssFiles = null;
		int port = -1;
		_directory.trim();
		String separator = "/";
		if(!_directory.endsWith(separator))
		{
			_directory = _directory + separator;
		}
		// get listing of all dss filenames in the directory.
		try
		{
			DSSRemoteClient client = DSSUtil.createRemoteClient(_serverName,
					port);
			if(DEBUG)
			{
				System.out.println(_directory);
			}
			dssFiles = client.getListing(_directory);
			if(DEBUG)
			{
				System.out.println(dssFiles);
			}
		}
		catch(Exception e)
		{
			e.printStackTrace();
			throw new RuntimeException(e.getMessage());
		}
		// create full pathnames for .dss files
		for(int i = 0; i < dssFiles.length; i++)
		{
			dssFiles[i] = _directory + dssFiles[i];
		}
		// create group proxies
		ArrayList<Group> groupList = new ArrayList<Group>();
		for(int i = 0; i < dssFiles.length; i++)
		{
			String dssFile = dssFiles[i];
			if(!DSSUtil.isValidDSSFile(dssFile))
			{
				continue;
			}
			String catalogFile = DSSUtil.getCatalogFilename(dssFile);
			Group g = DSSUtil.createGroup(_serverName, dssFile);
			groupList.add(g);
		}
		// create session
		return new Session(super.getName(), groupList);
	}
}
