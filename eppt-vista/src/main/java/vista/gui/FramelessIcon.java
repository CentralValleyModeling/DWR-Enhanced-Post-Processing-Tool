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
package vista.gui;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.Window;
import javax.swing.*;

/**
 * A display of the start up icon window
 * 
 * @author Nicky Sandhu
 * @version $Id: FramelessIcon.java,v 1.1 2003/10/02 20:49:15 redwood Exp $
 */
public class FramelessIcon extends Window {
	/**
   *
   */
	public FramelessIcon(byte[] imageData) {
		super(new Frame(""));
		Icon im = new ImageIcon(imageData);
		JLabel lbl = null;
		this.add(lbl = new JLabel("VISTA " + VistaUtils.getVersionId(), im,
				SwingConstants.CENTER));
		lbl.setHorizontalAlignment(SwingConstants.CENTER);
		lbl.setVerticalAlignment(SwingConstants.CENTER);
		lbl.setHorizontalTextPosition(SwingConstants.CENTER);
		lbl.setVerticalTextPosition(SwingConstants.BOTTOM);
		lbl.setFont(new Font("Times Roman", Font.BOLD, 15));
		this.pack();
		Dimension scrSize = Toolkit.getDefaultToolkit().getScreenSize();
		int w = im.getIconWidth();
		int h = im.getIconHeight();
		this.setLocation((scrSize.width - w) / 2, (scrSize.height - h) / 2);
		this.show();
	}
} // end of FramelessIcon class
