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

package gov.ca.water.calgui.techservice;

/**
 * Interface for common JOptionPane for simple dialogs with CalLite logo,
 * centered on main frame
 *
 * @author tslawecki
 */
public interface IDialogSvc
{

	String getOK(String message, int messageType);

	String getYesNo(String message, int messageType);

	String getOKCancel(String message, int messageType);

	String getSaveDontSaveCancel(String message, int messageType);

	String getYesNoCancel(String message, int messageType);

}