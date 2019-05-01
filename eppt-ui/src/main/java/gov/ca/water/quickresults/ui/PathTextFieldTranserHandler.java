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

package gov.ca.water.quickresults.ui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-01-2019
 */
class PathTextFieldTranserHandler extends TransferHandler
{
	private static final Logger LOGGER = Logger.getLogger(PathTextFieldTranserHandler.class.getName());
	private final JTextField _textField;
	private final String _pathExtension;

	PathTextFieldTranserHandler(JTextField textField, String pathExtension)
	{
		_textField = textField;
		_pathExtension = pathExtension;
	}

	PathTextFieldTranserHandler(JTextField textField)
	{
		_textField = textField;
		_pathExtension = null;
	}


	@Override
	public boolean canImport(TransferSupport support)
	{
		boolean retval = false;
		try
		{
			List<File> droppedFiles = (List<File>) support.getTransferable().getTransferData(
					DataFlavor.javaFileListFlavor);
			if(droppedFiles.size() == 1)
			{
				if(_pathExtension == null)
				{
					retval = droppedFiles.get(0).isDirectory();
				}
				else
				{
					retval = droppedFiles.get(0).toString().toLowerCase().endsWith(_pathExtension.toLowerCase());
				}
			}
		}
		catch(UnsupportedFlavorException | IOException | RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Cannot support drag and drop", ex);
		}
		return retval;
	}


	@Override
	public boolean importData(TransferSupport support)
	{
		super.importData(support);
		boolean retval = false;
		try
		{
			List<File> droppedFiles = (List<File>) support.getTransferable().getTransferData(
					DataFlavor.javaFileListFlavor);
			_textField.setText(droppedFiles.get(0).toString());
			retval = true;
		}
		catch(UnsupportedFlavorException | IOException | RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Cannot support drag and drop", ex);
		}
		return retval;
	}


}
