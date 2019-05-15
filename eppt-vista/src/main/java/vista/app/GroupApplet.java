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
package vista.app;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

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
