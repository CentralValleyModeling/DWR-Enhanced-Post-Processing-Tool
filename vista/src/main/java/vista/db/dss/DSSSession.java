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

import java.util.ArrayList;

import vista.set.Group;
import vista.set.Session;
import vista.set.SessionProxy;

/**
 * A proxy for a session existing as a directory on a remote server which has a
 * list of .dss files.
 */
class DSSSession extends SessionProxy {
	/**
	 * A session created from a server and directory spec.
	 */
	public DSSSession(String serverName, String directory) {
		_serverName = serverName;
		_directory = directory;
		super.setName(_serverName + DSSDataReference.SEPARATOR + _directory);
	}

	/**
	 * returns the initialized session
	 */
	protected Session getInitializedSession() {
		String[] dssFiles = null;
		int port = -1;
		_directory.trim();
		String separator = "/";
		if (!_directory.endsWith(separator))
			_directory = _directory + separator;
		// get listing of all dss filenames in the directory.
		try {
			DSSRemoteClient client = DSSUtil.createRemoteClient(_serverName,
					port);
			if (DEBUG)
				System.out.println(_directory);
			dssFiles = client.getListing(_directory);
			if (DEBUG)
				System.out.println(dssFiles);
		} catch (Exception e) {
			e.printStackTrace();
			throw new RuntimeException(e.getMessage());
		}
		// create full pathnames for .dss files
		for (int i = 0; i < dssFiles.length; i++) {
			dssFiles[i] = _directory + dssFiles[i];
		}
		// create group proxies
		ArrayList<Group> groupList = new ArrayList<Group>();
		for (int i = 0; i < dssFiles.length; i++) {
			String dssFile = dssFiles[i];
			if (!DSSUtil.isValidDSSFile(dssFile))
				continue;
			String catalogFile = DSSUtil.getCatalogFilename(dssFile);
			Group g = DSSUtil.createGroup(_serverName, dssFile);
			groupList.add(g);
		}
		// create session
		return new Session(super.getName(), groupList);
	}

	/**
	 * The name of the data server
	 */
	private String _serverName;
	/**
	 * The default session directory containing the default groups (.dss files)
	 */
	private String _directory;
	/**
   *
   */
	private static final boolean DEBUG = false;
}
