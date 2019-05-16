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

import java.net.InetAddress;
import java.rmi.Naming;

/**
 * Binds a DSSRemoteClient object to the rmi registry at the current port number
 *
 * @author Nicky Sandhu
 * @version $Id: DSSRemoteServer.java,v 1.4 2000/02/12 02:00:37 nsandhu Exp $
 */
public class DSSRemoteServer
{
	/**
	 *
	 */
	public static void main(String[] args)
	{
		// start the rmi server
		System.out.print("Starting vista server...");
		Thread rmiThread = new Thread()
		{
			public void run()
			{
				sun.rmi.registry.RegistryImpl.main(new String[]{"1099"});
			}
		};
		rmiThread.start();
		System.out.println("....RMI Registry started");
		//
		// System.setSecurityManager( new RMISecurityManager());
		String serverName = null;
		if(args.length == 0)
		{
			try
			{
				serverName = InetAddress.getLocalHost().getHostName();
			}
			catch(java.net.UnknownHostException uhe)
			{
				System.out.println("Could not figure out hostname");
				System.out
						.println("You could specify hostname as first argument");
				System.exit(-1);
			}
		}
		else
		{
			serverName = args[0];
		}
		try
		{
			DSSRemoteClientImpl obj = new DSSRemoteClientImpl();
			Naming.rebind("//" + serverName + ":"
							+ DSSUtil.getClientPortNumber() + "/DSSRemoteClientServer",
					obj);
			System.out.println("Vista server now running...");
		}
		catch(Exception e)
		{
			System.out.println("DSSRemoteClientImpl err: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
