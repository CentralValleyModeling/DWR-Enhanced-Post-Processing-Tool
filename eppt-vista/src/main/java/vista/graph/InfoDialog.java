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
package vista.graph;

import java.awt.Dialog;

/**
 * A dialog for displaying information. This dialog positions itself relative to
 * its parent frame. It also beeps before displaying the dialog.
 *
 * @author Nicky Sandhu
 * @version $Id: InfoDialog.java,v 1.1 2003/10/02 20:49:02 redwood Exp $
 */
public class InfoDialog extends Dialog
{
	/**
	 * Label for OK Button
	 */
	public static final String OK_LABEL = "OK";

	/**
	 * Constructor
	 *
	 * @param parent  The parent frame to which this dialog belongs
	 * @param title   The title of this dialog window
	 * @param isModal True if this dialog is modal
	 * @param msg     The message to be displayed in dialog
	 */
	public InfoDialog(java.awt.Frame parent, String title, boolean isModal,
					  String msg)
	{
		super(parent, title, isModal);

		if(msg == null)
		{
			msg = " ";
		}

		java.awt.GridBagConstraints gbc = new java.awt.GridBagConstraints();
		java.awt.GridBagLayout gbl = new java.awt.GridBagLayout();
		setLayout(gbl);

		java.awt.Button OKButton;
		java.awt.Label message;

		message = new java.awt.Label(msg);
		gbc.gridx = 2;
		gbc.gridy = 2;
		gbc.gridwidth = 6;
		gbc.gridheight = 2;
		gbl.setConstraints(message, gbc);
		add(message);

		OKButton = new java.awt.Button(OK_LABEL);
		gbc.gridx = 4;
		gbc.gridy = 6;
		gbc.gridwidth = 2;
		gbc.gridheight = 2;
		gbc.weightx = 1.0;
		gbc.anchor = java.awt.GridBagConstraints.CENTER;
		gbl.setConstraints(OKButton, gbc);
		add(OKButton);

		OKButton.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent e)
			{
				((java.awt.Dialog) ((java.awt.Button) e.getSource())
						.getParent()).dispose();
			}
		});

		pack();

		java.awt.Toolkit tk = getToolkit();
		for(int i = 0; i < 3; i++)
		{
			tk.beep();
		}
		java.awt.Rectangle frameBounds = parent.getBounds();
		java.awt.Dimension dialogSize = getSize();
		setLocation(frameBounds.x + frameBounds.width / 2 - dialogSize.width
				/ 2, frameBounds.y + frameBounds.height / 2 - dialogSize.height
				/ 2);
		show();

	}
}
