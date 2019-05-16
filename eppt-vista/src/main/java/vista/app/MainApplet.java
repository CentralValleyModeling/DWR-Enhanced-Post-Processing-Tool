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

import java.awt.Button;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

/**
 * @author Nicky Sandhu
 * @version $Id: MainApplet.java,v 1.4 1999/01/07 21:05:32 nsandhu Exp $
 */
public class MainApplet extends JApplet
{
	/**
	 *
	 */
	public MainApplet()
	{
		Button mainButton = new Button("Press here to start");
		mainButton.addActionListener(new DisplayGUI());
		getContentPane().setLayout(new FlowLayout());
		getContentPane().add(mainButton);
		setVisible(true);
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: MainApplet.java,v 1.4 1999/01/07 21:05:32 nsandhu Exp $
	 */
	class DisplayGUI implements ActionListener
	{
		/**
		 *
		 */
		public void actionPerformed(ActionEvent evt)
		{
			try
			{
				// new MainGUI(null);
				System.out.println("Loading vista utiles");
				vista.gui.VistaUtils.showStartUpIcon();
			}
			catch(Exception e)
			{
				System.out.println(e.getMessage());
			}
		}
	}// end of DisplayGUI class
}
