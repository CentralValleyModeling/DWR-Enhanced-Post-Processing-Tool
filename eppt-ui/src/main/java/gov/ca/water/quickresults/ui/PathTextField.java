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

import java.awt.dnd.DropTarget;
import javax.swing.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-01-2019
 */
public class PathTextField extends JTextField
{

	public static PathTextField createFileTextField(String pathExtension)
	{
		PathTextField pathTextField = new PathTextField();
		pathTextField.setTransferHandler(new PathTextFieldTranserHandler(pathTextField, pathExtension));
		return pathTextField;
	}

	public static PathTextField createDirectoryTextField()
	{
		PathTextField pathTextField = new PathTextField();
		pathTextField.setTransferHandler(new PathTextFieldTranserHandler(pathTextField));
		return pathTextField;
	}
}
