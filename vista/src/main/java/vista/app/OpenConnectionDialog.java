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
package vista.app;

import java.awt.BorderLayout;
import java.util.ArrayList;

import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import vista.db.dss.DSSUtil;
import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;

/**
 * A modal dialog to establish connection with server
 * 
 * @author Nicky Sandhu
 * @version $Id: OpenConnectionDialog.java,v 1.1 2003/10/02 20:48:36 redwood Exp
 *          $
 */
public class OpenConnectionDialog extends JDialog implements Changeable {
	private View _view;

	/**
	 * sets up a blocking dialog displaying server and directory names
	 */
	public OpenConnectionDialog(JFrame parent, View view) {
		super(parent, true);
		_view = view;
		// set up connection panel
		JPanel connectionPanel = new JPanel();
		connectionPanel.setLayout(new BorderLayout());
		// add server panel
		if (_serverHistory == null) {
			_serverHistory = new ArrayList<String>();
			_serverHistory.add("");
		}
		_serverField = new JComboBox(_serverHistory.toArray());
		_serverField.setEditable(true);
		_serverField.setSelectedIndex(_serverField.getItemCount() - 1);
		JPanel serverPanel = new JPanel();
		serverPanel.setLayout(new BorderLayout());
		serverPanel.add(new JLabel("SERVER IP/DOMAIN NAME: "),
				BorderLayout.WEST);
		serverPanel.add(_serverField, BorderLayout.CENTER);
		// add directory panel
		if (_directoryHistory == null) {
			_directoryHistory = new ArrayList<String>();
			_directoryHistory.add(DSSUtil.getDefaultDirectory());
		}
		_databaseField = new JComboBox(_directoryHistory.toArray());
		_databaseField.setEditable(true);
		_databaseField.setSelectedIndex(_databaseField.getItemCount() - 1);
		JPanel directoryPanel = new JPanel();
		directoryPanel.setLayout(new BorderLayout());
		directoryPanel.add(new JLabel("Database: "), BorderLayout.WEST);
		directoryPanel.add(_databaseField, BorderLayout.CENTER);
		// add server and directory panels
		connectionPanel.add(serverPanel, BorderLayout.NORTH);
		connectionPanel.add(directoryPanel, BorderLayout.SOUTH);
		// add connection panel
		super.getContentPane().setLayout(new BorderLayout());
		super.getContentPane().add(connectionPanel, BorderLayout.CENTER);
		super.getContentPane().add(new DialogButtonPanel(this),
				BorderLayout.SOUTH);
		pack();
	}

	/**
	 * gets server name.
	 */
	public String getServer() {
		String server = (String) _serverField.getSelectedItem();
		return server;
	}

	/**
	 * returns directory name.
	 */
	public String getDirectory() {
		String str = (String) _databaseField.getSelectedItem();
		return str;
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges() {
		if (DEBUG)
			System.out.println("Making connection to " + getServer());
		if (DEBUG)
			System.out.println("Directory " + getDirectory());
		final String server = getServer();
		final String dir = getDirectory();
		if (!_serverHistory.contains(server))
			_serverHistory.add(server);
		if (!_directoryHistory.contains(server))
			_directoryHistory.add(dir);
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges() {
		this.dispose();
	}

	/**
	 * server and directory
	 */
	private JComboBox _serverField, _databaseField;
	/**
   *
   */
	private static final boolean DEBUG = false;
	/**
 *
 */
	private static ArrayList<String> _directoryHistory, _serverHistory;
}
