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
import java.awt.Container;

import javax.swing.JMenuBar;

import vista.set.Group;

/**
 * A frame containing the view of a group and its associated menu items.
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupFrameApplet.java,v 1.1 2000/05/10 20:38:08 nsandhu Exp $
 */
public class GroupFrameApplet extends DefaultFrame {
	/**
   *
   */
	public GroupFrameApplet(Group g) {
		// setIconImage(Toolkit.getDefaultToolkit().createImage(VistaUtils.getImageAsBytes("/vista/OWE.gif")));
		_groupTablePanel = new GroupTable(g);
		JMenuBar mbar = new JMenuBar();
		_groupTablePanel.addMenus(mbar);
		// mbar.setHelpMenu( new JMenu("Help") );
		getRootPane().setJMenuBar(mbar);
		// add components...
		Container contentPane = getContentPane();
		contentPane.setLayout(new BorderLayout());
		contentPane.add(_groupTablePanel, BorderLayout.CENTER);
		pack();
		this.setTitle(g.getName());
		// set size to default
		// int width = new
		// Integer(_mainProps.getProperty("MainGUI.width")).intValue();
		// int height = new
		// Integer(_mainProps.getProperty("MainGUI.height")).intValue();
		// setSize( width, height );
		setSize(750, 900);
		setVisible(true);

	}

	/**
   *
   */
	public GroupTable getGroupTable() {
		return _groupTablePanel;
	}

	/**
   *
   */
	private GroupTable _groupTablePanel;
}
