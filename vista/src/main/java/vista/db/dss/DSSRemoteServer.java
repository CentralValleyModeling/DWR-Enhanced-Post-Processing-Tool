/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

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
public class DSSRemoteServer {
	/**
   *
   */
	public static void main(String[] args) {
		// start the rmi server
		System.out.print("Starting vista server...");
		Thread rmiThread = new Thread() {
			public void run() {
				sun.rmi.registry.RegistryImpl.main(new String[] { "1099" });
			}
		};
		rmiThread.start();
		System.out.println("....RMI Registry started");
		//
		// System.setSecurityManager( new RMISecurityManager());
		String serverName = null;
		if (args.length == 0) {
			try {
				serverName = InetAddress.getLocalHost().getHostName();
			} catch (java.net.UnknownHostException uhe) {
				System.out.println("Could not figure out hostname");
				System.out
						.println("You could specify hostname as first argument");
				System.exit(-1);
			}
		} else {
			serverName = args[0];
		}
		try {
			DSSRemoteClientImpl obj = new DSSRemoteClientImpl();
			Naming.rebind("//" + serverName + ":"
					+ DSSUtil.getClientPortNumber() + "/DSSRemoteClientServer",
					obj);
			System.out.println("Vista server now running...");
		} catch (Exception e) {
			System.out.println("DSSRemoteClientImpl err: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
