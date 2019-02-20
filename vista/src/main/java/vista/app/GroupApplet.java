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
import java.awt.Button;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JApplet;

import vista.db.dss.DSSUtil;
import vista.set.Group;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupApplet.java,v 1.4 2000/05/10 20:37:49 nsandhu Exp $
 */
public class GroupApplet extends JApplet {
	/**
   *
   */
	public GroupApplet() {
		Button hydroBtn = new Button("HYDRO DATA");
		hydroBtn.addActionListener(new DisplayGUI("iep.water.ca.gov",
				"/home/www/htdocs/dss/db/hydro.dss"));
		Button qualBtn = new Button("QUAL DATA");
		qualBtn.addActionListener(new DisplayGUI("iep.water.ca.gov",
				"/home/www/htdocs/dss/db/quality.dss"));
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(hydroBtn, BorderLayout.NORTH);
		getContentPane().add(qualBtn, BorderLayout.SOUTH);
		setVisible(true);
	}

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: GroupApplet.java,v 1.4 2000/05/10 20:37:49 nsandhu Exp $
	 */
	class DisplayGUI implements ActionListener {
		String _server, _file;

		/**
    *
    */
		public DisplayGUI(String server, String file) {
			_server = server;
			_file = file;
		}

		/**
   *
   */
		public void actionPerformed(ActionEvent evt) {
			Group g = DSSUtil.createGroup(_server, _file);
			new GroupFrameApplet(g);
		}
	}// end of DisplayGUI class
}
