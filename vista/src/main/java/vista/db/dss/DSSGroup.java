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

import javax.swing.JProgressBar;

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
