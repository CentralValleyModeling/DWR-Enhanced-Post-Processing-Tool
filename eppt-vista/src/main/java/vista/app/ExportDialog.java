/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import javax.swing.*;

import vista.set.DataReference;

/**
 * Creates a dialog to allow user customization of output parameters such as
 * pathname, time window and filename.
 *
 * @author Nicky Sandhu ,Armin Munevar
 * @version $Id: ExportDialog.java,v 1.2 2001/03/05 21:48:07 eli2 Exp $
 */
public class ExportDialog
{
	public ExportDialog(DataReference ref)
	{
		DataReference _ref = ref;
		JTextField _pathField = new JTextField(_ref.getPathname().toString(),
				80);
		if(_ref.getTimeWindow() != null)
		{
			JTextField _twField = new JTextField(_ref.getTimeWindow()
													 .toString(), 80);
			JTextField _fileField = new JTextField(_ref.getFilename(), 80);
		}
	}
}
