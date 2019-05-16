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

package gov.ca.water.businessservice;

import java.nio.file.Path;
import javax.swing.*;

/**
 * This interface is to handle all the button actions in the ui.
 *
 * @author Mohan
 */
public interface IAllButtonsDele
{

	/**
	 * This is to handle the "Save" button on the "Run Settings" tab.
	 *
	 * @return Will return true if the save is successful.
	 */
	boolean saveCurrentStateToFile();

	/**
	 * This method is used to save.
	 *
	 * @param clsFileName Just the name of the cls file whithout the extension.
	 * @return It will return true if the save is successful.
	 */
	boolean saveCurrentStateToFile(Path clsFileName);

	/**
	 * This is to handle the "Save As" button on the "Run Settings" tab.
	 */
	void saveAsButton();

	/**
	 * This will save the current state of the ui for the
	 * "View Scenario Settings" button on the "Run Settings" tab.
	 *
	 * @return It will return true if the save is successful.
	 */
	boolean saveForViewScen();

	/**
	 * This method is used to run multiple batch program at once.
	 */
	void runMultipleBatch();

	/**
	 * This method is used to handle the help button in the whole ui.
	 */
	void helpButton();

	/**
	 * This method is used to handle the about button in the whole ui.
	 */
	void aboutButton();

	/**
	 * This method is used for the "Select.." buttons on the "Hydroclimate" tab.
	 * This method will read the file and set the values of the text fields.
	 *
	 * @param fileNameForDss       The {@link JTextField} from the gui.xml for seting the dss
	 *                             file name.
	 * @param fPartForDss          The {@link JTextField} from the gui.xml for seting the dss f
	 *                             part.
	 * @param manualFileNameForDss The {@link JTextField} from the gui.xml for seting the dss
	 *                             manual file name.
	 * @param manualFPartForDss    The {@link JTextField} from the gui.xml for seting the dss
	 *                             manual f part.
	 */
	void selectingSVAndInitFile(String fileNameForDss, String fPartForDss, String manualFileNameForDss,
								String manualFPartForDss);

	/**
	 * This method will copy the cells which are selected in the table which is
	 * passed as a parameter.
	 *
	 * @param table The table from which we should copy.
	 */
	void copyTableValues(JTable table);

	/**
	 * This method will paste the cells which are selected in the table which is
	 * passed as a parameter.
	 *
	 * @param table The table from which we should paste.
	 */
	void pasteTableValues(JTable table);

	/**
	 * This method will handle the read button on the "operations" tab.
	 */
	void readButtonInOperations();

	/**
	 * This method will handle the default button on the "operations" tab.
	 */
	void defaultButtonOnOperations();

	/**
	 * This method will handle the edit SWP and CVP buttons on the "operations"
	 * tab.
	 *
	 * @param component The selected component (SWP or CVP)
	 */
	void editButtonOnOperations(JComponent component);

	/**
	 * This method is used to verify whether the selected files are of the given
	 * extension or not.
	 *
	 * @param fileChooser The {@link JFileChooser} for the files which user selectes.
	 * @param extension   The correct extension of the file.
	 * @return Will return true if the files selected by the user and the passed
	 * in extension match.
	 */
	boolean verifyTheSelectedFiles(JFileChooser fileChooser, String extension);

	/**
	 * This method will decide which Sv,init files should be selected and also
	 * the tables in the Operations tab.
	 */
	void decisionSVInitFilesAndTableInOperations();

	/**
	 * This method provides access to the Default CLS Protection value in the
	 * properties file. The value can be set to "false" to allow overwriting of
	 * the DEFAULT.CLS file.
	 *
	 * @return
	 */
	boolean defaultCLSIsProtected();
}
