/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice.impl;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.Vector;
import java.util.stream.Collectors;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;

import gov.ca.water.businessservice.IAllButtonsDele;
import gov.ca.water.businessservice.IApplyDynamicConDele;
import gov.ca.water.businessservice.IXMLParsingSvc;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.bo.GUILinks4BO;
import gov.ca.water.calgui.bus_service.IDynamicControlSvc;
import gov.ca.water.calgui.bus_service.IModelRunSvc;
import gov.ca.water.calgui.bus_service.IScenarioSvc;
import gov.ca.water.calgui.bus_service.ISeedDataSvc;
import gov.ca.water.calgui.bus_service.ITableSvc;
import gov.ca.water.calgui.bus_service.impl.DynamicControlSvcImpl;
import gov.ca.water.calgui.bus_service.impl.ModelRunSvcImpl;
import gov.ca.water.calgui.bus_service.impl.ScenarioSvcImpl;
import gov.ca.water.calgui.bus_service.impl.SeedDataSvcImpl;
import gov.ca.water.calgui.bus_service.impl.TableSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.ProgressFrame;
import gov.ca.water.calgui.tech_service.IAuditSvc;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.AuditSvcImpl;
import gov.ca.water.calgui.tech_service.impl.CalLiteHelp;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.commons.io.FilenameUtils;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

import hec.heclib.dss.HecDss;

/**
 * This class is to handle all the button actions in the ui like Load Scenario,
 * Save Scenario etc.
 *
 * @author Mohan
 */
public class AllButtonsDeleImp implements IAllButtonsDele
{
	private static final Logger LOG = Logger.getLogger(AllButtonsDeleImp.class.getName());
	private final IXMLParsingSvc _xmlParsingSvc = XMLParsingSvcImpl.getXMLParsingSvcImplInstance();
	private final SwingEngine _swingEngine = _xmlParsingSvc.getSwingEngine();
	private boolean _okToSkipConfirmation = false;
	private IModelRunSvc _modelRunSvc = new ModelRunSvcImpl();
	private IScenarioSvc _scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();
	private ISeedDataSvc _seedDataSvc = SeedDataSvcImpl.getSeedDataSvcImplInstance();
	private ITableSvc _tableSvc = TableSvcImpl.getTableSvcImplInstance();
	private IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();
	private Properties _properties = new Properties();
	private boolean _defaultCLSProtected = true;
	private IAuditSvc _auditSvc = AuditSvcImpl.getAuditSvcImplInstance();
	private IFileSystemSvc _fileSystemSvc = new FileSystemSvcImpl();
	private IDynamicControlSvc _dynamicControlSvc = DynamicControlSvcImpl.getDynamicControlSvcImplInstance();
	private IApplyDynamicConDele _applyDynamicConDele = new ApplyDynamicConDeleImp();
	private IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();

	public AllButtonsDeleImp()
	{
		try
		{
			_properties.load(AllButtonsDeleImp.class.getClassLoader().getResourceAsStream("callite-gui.properties"));
			_defaultCLSProtected = !_properties.getProperty("default.cls.protected").equals("false");
		}
		catch(Exception ex)
		{
			LOG.error("Problem loading properties. " + ex.getMessage());
		}
	}

	public boolean defaultCLSIsProtected()
	{
		return _defaultCLSProtected;
	}

	@Override
	public void saveAsButton()
	{
		setOKToSkipConfirmation(false);
		JFileChooser fileChooser = new JFileChooser(Constant.SCENARIOS_DIR);
		fileChooser.setMultiSelectionEnabled(false);
		FileNameExtensionFilter filter = new FileNameExtensionFilter("CLS FILES (.cls)", "cls");
		fileChooser.setFileFilter(filter);

		String newScrName = "DEFAULT"; // Check to make sure GUI is not
		// overwriting DEFAULT.CLS - tad
		// 20170202
		int val = JFileChooser.APPROVE_OPTION;
		while(newScrName.equals("DEFAULT") && val == JFileChooser.APPROVE_OPTION)
		{

			val = fileChooser.showSaveDialog(_swingEngine.find(Constant.MAIN_FRAME_NAME));

			if(val == JFileChooser.APPROVE_OPTION)
			{
				newScrName = fileChooser.getSelectedFile().getName();
				if(newScrName.toLowerCase().endsWith(".cls"))
				{
					newScrName = FilenameUtils.removeExtension(newScrName);
				}
				if(newScrName.toUpperCase().equals("DEFAULT") && _defaultCLSProtected)
				{
					_dialogSvc.getOK(
							"The CalLite GUI is not allowed to overwrite DEFAULT.CLS. Please choose a different scenario file name or cancel the save operation.",
							JOptionPane.ERROR_MESSAGE);

				}
				else if(!save(newScrName))
				{
					String tempName = Constant.SCENARIOS_DIR + newScrName + Constant.CLS_EXT;
					if(!(new File(tempName)).exists())
					{
						_errorHandlingSvc.businessErrorHandler("Unable to save the file.", "Unable to save the file.");
					}
				}
			}
		}
	}

	@Override
	public boolean saveCurrentStateToFile()
	{
		String clsFileName = ((JTextField) _swingEngine.find("run_txfScen")).getText();
		return saveCurrentStateToFile(FilenameUtils.removeExtension(clsFileName));
	}

	/**
	 * This method will tell whether to save the existing gui state to the file.
	 *
	 * @return Will return true if we need to save to the file.
	 */

	@Override
	public boolean saveCurrentStateToFile(String clsFileName)
	{
		this.setOKToSkipConfirmation(false);
		if(decisionToSaveOrNot(clsFileName))
		{
			this.setOKToSkipConfirmation(true);
			return save(clsFileName);
		}
		return true;
	}

	/**
	 * This method will tell whether to save the existing gui state to the file.
	 *
	 * @param clsFileName The cls file name.
	 * @return Will return true if we need to save to the file.
	 */
	private boolean decisionToSaveOrNot(String clsFileName)
	{
		if(_auditSvc.hasValues())
		{
			return true;
		}
		if(!Files.isExecutable(Paths.get(Constant.RUN_DETAILS_DIR + clsFileName)))
		{
			return true;
		}
		return (_dialogSvc.getYesNo("The file is up-to-date. Do you want to save again?", JOptionPane.QUESTION_MESSAGE)
						  .equals("Yes"));
	}

	@Override
	public boolean saveForViewScen()
	{
		try
		{
			_scenarioSvc.saveToCLSFile(Constant.SCENARIOS_DIR + Constant.CURRENT_SCENARIO + Constant.CLS_EXT,
					_swingEngine, _seedDataSvc.getGUILinks2BOList());
			return true;
		}
		catch(CalLiteGUIException ex)
		{
			LOG.debug(ex);
			_errorHandlingSvc.businessErrorHandler(ex);
			return false;
		}
	}

	/**
	 * This method will save the current state of the ui to the cls file name
	 * given.
	 *
	 * @param clsFileName The cls file name to save the currnt state of the ui.
	 * @return Will return true if the save is successful.
	 */
	private boolean save(String clsFileName)
	{

		String tempName = Constant.SCENARIOS_DIR + clsFileName + Constant.CLS_EXT;
		boolean proceed = true;
		if((new File(tempName)).exists())
		{
			proceed = isOKToSkipConfirmation() || (_dialogSvc
					.getOKCancel("The scenario file '" + tempName + "' already exists. Press OK to overwrite.",
							JOptionPane.QUESTION_MESSAGE)
					.equals("OK"));

		}

		if(proceed)
		{
			((JTextField) _swingEngine.find("run_txfScen")).setText(clsFileName + Constant.CLS_EXT);
			((JTextField) _swingEngine.find("run_txfoDSS")).setText(clsFileName + Constant.DV_NAME + Constant.DSS_EXT);
			/*
			 * The following code is for checking whether the tables in the
			 * "Operations" tab is up to data or not and if not updating the
			 * tables(SWP and CVP) in the menory.
			 */
			String swpFileName = _scenarioSvc.getUserDefinedTable(Constant.SWP_START_FILENAME).getTableName();
			String cvpFileName = _scenarioSvc.getUserDefinedTable(Constant.CVP_START_FILENAME).getTableName();

			if(!_fileSystemSvc.getTheLookupFromTheFullFileName(_tableSvc.getWsidiForSWPFullFileName())
							  .equalsIgnoreCase(swpFileName))
			{
				try
				{
					_scenarioSvc.addUserDefinedTable(Constant.SWP_START_FILENAME,
							_tableSvc.getWsiDiTable(_tableSvc.getWsidiForSWPFullFileName()));
				}
				catch(CalLiteGUIException ex)
				{
					LOG.error(ex);
					_errorHandlingSvc.businessErrorHandler(ex);
					return false;
				}
			}
			if(!_fileSystemSvc.getTheLookupFromTheFullFileName(_tableSvc.getWsidiForCVPFullFileName())
							  .equalsIgnoreCase(cvpFileName))
			{
				try
				{
					_scenarioSvc.addUserDefinedTable(Constant.CVP_START_FILENAME,
							_tableSvc.getWsiDiTable(_tableSvc.getWsidiForCVPFullFileName()));
				}
				catch(CalLiteGUIException ex)
				{
					LOG.error(ex);
					_errorHandlingSvc.businessErrorHandler(ex);
					return false;
				}
			}
			ProgressFrame progressFrame = ProgressFrame.getProgressFrameInstance();
			progressFrame.addScenarioNamesAndAction(clsFileName, Constant.SAVE);
			progressFrame.makeDialogVisible();
			proceed = ScenarioSvcImpl.getScenarioSvcImplInstance().save(clsFileName,
					XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine(),
					SeedDataSvcImpl.getSeedDataSvcImplInstance().getGUILinks2BOList());
			LOG.debug("Save Complete. " + clsFileName);
			_auditSvc.clearAudit();
			return proceed;
		}
		return false;
	}

	/**
	 * @return Current setting for whether or not the confirmation question can
	 * be skipped
	 */
	private boolean isOKToSkipConfirmation()
	{
		return _okToSkipConfirmation;
	}

	/**
	 * Controls whether or not the user is asked to confirm overwriting a
	 * scenario. The use should *not* be asked if they've already confirmed
	 * saving an up-to-date scenario; this is done by setting to true
	 *
	 * @param value Set to true if the overwrite confirmation question can be
	 *              skipped
	 */
	private void setOKToSkipConfirmation(boolean value)
	{
		_okToSkipConfirmation = value;
	}

	@Override
	public void runMultipleBatch()
	{
		JFileChooser fileChooser = new JFileChooser(Constant.SCENARIOS_DIR);
		fileChooser.setMultiSelectionEnabled(true);
		FileNameExtensionFilter filter = new FileNameExtensionFilter("CLS FILES (.cls)", "cls");
		fileChooser.setFileFilter(filter);
		int val = fileChooser.showOpenDialog(_swingEngine.find(Constant.MAIN_FRAME_NAME));
		if(val == JFileChooser.APPROVE_OPTION && verifyTheSelectedFiles(fileChooser, Constant.CLS_EXT))
		{
			List<String> fileNames = new ArrayList<>();
			for(File file : fileChooser.getSelectedFiles())
			{
				fileNames.add(FilenameUtils.removeExtension(file.getName()));
			}
			List<String> filesWhichAreNotSaved = fileNames.stream()
														  .filter(fileName -> !Files.isExecutable(
																  Paths.get(Constant.RUN_DETAILS_DIR + fileName)))
														  .collect(Collectors.toList());
			if(filesWhichAreNotSaved != null && !filesWhichAreNotSaved.isEmpty())
			{
				String option = _dialogSvc.getYesNo(
						"We can't run the batch for following files because they are not saved.\n"
								+ filesWhichAreNotSaved.stream().map(name -> name + ".cls")
													   .collect(Collectors.joining("\n"))
								+ "\n Do you still want to run the rest?",
						JOptionPane.QUESTION_MESSAGE);
				switch(option)
				{
					case "Yes":
						fileNames.removeAll(filesWhichAreNotSaved);
						break;
					case "No":
						return;
				}
			}
			ProgressFrame progressFrame = ProgressFrame.getProgressFrameInstance();
			progressFrame.addScenarioNamesAndAction(fileNames, Constant.BATCH_RUN);
			progressFrame.setBtnText(Constant.STATUS_BTN_TEXT_STOP);
			progressFrame.makeDialogVisible();
			_modelRunSvc.doBatch(fileNames, _swingEngine, false);
		}
	}

	@Override
	public boolean verifyTheSelectedFiles(JFileChooser fileChooser, String extension)
	{
		File[] files = fileChooser.getSelectedFiles();
		if(files.length <= 0)
		{
			File fi = fileChooser.getSelectedFile();
			files = new File[1];
			files[0] = fi;
		}
		boolean isExpectedFiles = true;
		String errorMessage = "";
		for(File file : files)
		{
			if(!file.getName().toLowerCase().endsWith(extension.toLowerCase()))
			{
				isExpectedFiles = false;
				errorMessage = "Please select " + extension + " files only. The file you selected is " + file.getName();
				break;
			}
		}
		if(!isExpectedFiles)
		{
			_errorHandlingSvc.validationeErrorHandler(errorMessage, errorMessage);
		}
		return isExpectedFiles;
	}

	@Override
	public void copyTableValues(JTable table)
	{
		try
		{
			StringBuilder buffer = new StringBuilder();
			int numcols = table.getSelectedColumnCount();
			int numrows = table.getSelectedRowCount();
			int[] rowsselected = table.getSelectedRows();
			int[] colsselected = table.getSelectedColumns();
			if(!((numrows - 1 == rowsselected[rowsselected.length - 1] - rowsselected[0]
					&& numrows == rowsselected.length)
					&& (numcols - 1 == colsselected[colsselected.length - 1] - colsselected[0]
					&& numcols == colsselected.length)))
			{
				_dialogSvc.getOK("Invalid Copy Selection", JOptionPane.ERROR_MESSAGE);

				return;
			}
			for(int i = 0; i < numrows; i++)
			{
				for(int j = 0; j < numcols; j++)
				{
					buffer.append(table.getValueAt(rowsselected[i], colsselected[j]));
					if(j < numcols - 1)
					{
						buffer.append(Constant.TAB_SPACE);
					}
				}
				buffer.append(Constant.NEW_LINE);
			}
			StringSelection stringSelection = new StringSelection(buffer.toString());
			Clipboard systemClipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
			systemClipboard.setContents(stringSelection, stringSelection);
			_auditSvc.addAudit("copy", "", table.getName());
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			_errorHandlingSvc.validationeErrorHandler("Please select the field from where you want to copy the data",
					"Please select the field from where you want to copy the data");
		}
	}

	@Override
	public void pasteTableValues(JTable table)
	{
		try
		{
			int startRow = (table.getSelectedRows())[0];
			int startCol = (table.getSelectedColumns())[0];

			// get data from the clipboard.
			String totalData = "";
			Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
			// odd: the Object param of getContents is not currently used
			Transferable contents = clipboard.getContents(null);
			boolean hasTransferableText = (contents != null) && contents.isDataFlavorSupported(DataFlavor.stringFlavor);
			if(hasTransferableText)
			{
				try
				{
					totalData = (String) contents.getTransferData(DataFlavor.stringFlavor);
				}
				catch(UnsupportedFlavorException ex)
				{
					// highly unlikely since we are using a standard DataFlavor
					LOG.debug(ex.getMessage());
					_errorHandlingSvc.businessErrorHandler(new CalLiteGUIException(ex));
				}
				catch(IOException ex)
				{
					LOG.debug(ex.getMessage());
					_errorHandlingSvc.businessErrorHandler(new CalLiteGUIException(ex));
				}
			}
			totalData = totalData.replaceAll("(?sm)\t\t", "\t \t");
			totalData = totalData.replaceAll("(?sm)\t\n", "\t \n");
			int colCount = new StringTokenizer(new StringTokenizer(totalData, Constant.NEW_LINE).nextToken(),
					Constant.TAB_SPACE).countTokens();
			if(colCount > table.getColumnCount())
			{
				_errorHandlingSvc.validationeErrorHandler(
						"The column's you selected is more then the column's of the table.",
						"The column's you selected is more then the column's of the table.");
				return;
			}
			// poplute data.
			StringTokenizer st1 = new StringTokenizer(totalData, Constant.NEW_LINE);
			for(int i = 0; st1.hasMoreTokens(); i++)
			{
				String rowstring = st1.nextToken();
				StringTokenizer st2 = new StringTokenizer(rowstring, Constant.TAB_SPACE);
				for(int j = 0; st2.hasMoreTokens(); j++)
				{
					String value = st2.nextToken();
					if(startRow + i < table.getRowCount() && startCol + j < table.getColumnCount())
					{
						table.setValueAt(value, startRow + i, startCol + j);
					}
					table.repaint();
				}
			}
			_auditSvc.addAudit("past", "", table.getName());
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			_errorHandlingSvc.validationeErrorHandler("Please select the field from where you want to paste the data",
					"Please select the field from where you want to paste the data");
		}
		catch(Exception ex)
		{
			LOG.debug(ex.getMessage());
			_errorHandlingSvc.businessErrorHandler(new CalLiteGUIException(ex));
		}

	}

	@Override
	public void readButtonInOperations()
	{
		if(!isOkToContinueOnOperations())
		{
			return;
		}
		String swpFullFileName = "";
		String cvpFullFileName = "";
		JFileChooser fChooser = new JFileChooser(Constant.MODEL_W2_WRESL_LOOKUP_DIR);
		fChooser.setMultiSelectionEnabled(false);
		FileNameExtensionFilter filter = new FileNameExtensionFilter("Data Table FILES", "table");
		fChooser.setFileFilter(filter);
		fChooser.setDialogTitle("Select WSI/DI SWP data table file");
		int val = fChooser.showOpenDialog(_swingEngine.find(Constant.MAIN_FRAME_NAME));
		if(val == JFileChooser.APPROVE_OPTION && verifyTheSelectedFiles(fChooser, Constant.TABLE_EXT))
		{
			swpFullFileName = fChooser.getSelectedFile().getAbsolutePath();
		}
		else
		{
			return;
		}
		fChooser.setDialogTitle("Select WSI/DI CVP data table file");
		val = fChooser.showOpenDialog(_swingEngine.find(Constant.MAIN_FRAME_NAME));
		if(val == JFileChooser.APPROVE_OPTION && verifyTheSelectedFiles(fChooser, Constant.TABLE_EXT))
		{
			cvpFullFileName = fChooser.getSelectedFile().getAbsolutePath();
		}
		LOG.debug("SWP file name " + swpFullFileName + " CVP file name " + cvpFullFileName);

		try
		{
			// for CVP table
			DataTableModel cvpDtm = _tableSvc.getWsiDiTable(cvpFullFileName);
			cvpDtm.setTableName(Constant.USER_DEFINED);
			cvpDtm.setCellEditable(true);
			_scenarioSvc.addUserDefinedTable(Constant.CVP_START_FILENAME, cvpDtm);
			_tableSvc.setWsidiForCVPFullFileName(Constant.USER_DEFINED);
			// for SWP table
			DataTableModel swpDtm = _tableSvc.getWsiDiTable(swpFullFileName);
			swpDtm.setTableName(Constant.USER_DEFINED);
			swpDtm.setCellEditable(true);
			_scenarioSvc.addUserDefinedTable(Constant.SWP_START_FILENAME, swpDtm);
			_tableSvc.setWsidiForSWPFullFileName(Constant.USER_DEFINED);
			JLabel jLabel = (JLabel) _swingEngine.find("op_WSIDI_Status");
			jLabel.setText("WSI/DI read from [" + Paths.get(swpFullFileName).getFileName().toString() + " , "
					+ Paths.get(cvpFullFileName).getFileName().toString() + "]" + Constant.UNEDITED_FORLABEL);
			JComponent component = (JComponent) _swingEngine.find("op_btn1");
			editButtonOnOperations(component);
			_auditSvc.addAudit("wsi_di tables", "", swpFullFileName);
		}
		catch(CalLiteGUIException ex)
		{
			_errorHandlingSvc.businessErrorHandler(ex);
		}
	}

	@Override
	public void defaultButtonOnOperations()
	{
		if(!isOkToContinueOnOperations())
		{
			return;
		}
		try
		{
			List<String> labelNames = _dynamicControlSvc.getLabelAndGuiLinks4BOBasedOnTheRadioButtons(_swingEngine);
			GUILinks4BO gUILinks4BO = _seedDataSvc.getObjByRunBasisLodCcprojCcmodelIds(labelNames.get(0));
			String swpFullFileName = Constant.MODEL_W2_WRESL_LOOKUP_DIR + "\\WSIDI\\" + Constant.SWP_START_FILENAME
					+ Constant.UNDER_SCORE + gUILinks4BO.getLookup() + Constant.TABLE_EXT;
			String cvpFullFileName = Constant.MODEL_W2_WRESL_LOOKUP_DIR + "\\WSIDI\\" + Constant.CVP_START_FILENAME
					+ Constant.UNDER_SCORE + gUILinks4BO.getLookup() + Constant.TABLE_EXT;
			// To Load CVP table
			DataTableModel cvpDtm = _tableSvc.getWsiDiTable(cvpFullFileName);
			cvpDtm.setTableName(FilenameUtils.removeExtension(Paths.get(cvpFullFileName).getFileName().toString()));
			cvpDtm.setCellEditable(true);
			_scenarioSvc.addUserDefinedTable(Constant.CVP_START_FILENAME, cvpDtm);
			_tableSvc.setWsidiForCVPFullFileName(cvpFullFileName);
			// To Load SWP table
			DataTableModel swpDtm = _tableSvc.getWsiDiTable(swpFullFileName);
			swpDtm.setTableName(FilenameUtils.removeExtension(Paths.get(swpFullFileName).getFileName().toString()));
			swpDtm.setCellEditable(true);
			_scenarioSvc.addUserDefinedTable(Constant.SWP_START_FILENAME, swpDtm);
			_tableSvc.setWsidiForSWPFullFileName(swpFullFileName);
			JLabel jLabel = (JLabel) _swingEngine.find("op_WSIDI_Status");
			jLabel.setText(labelNames.get(1) + Constant.UNEDITED_FORLABEL);
			JComponent component = (JComponent) _swingEngine.find("op_btn1");
			editButtonOnOperations(component);
			_auditSvc.addAudit("wsi_di tables", "", swpFullFileName);
		}
		catch(RuntimeException ex)
		{
			_errorHandlingSvc.businessErrorHandler(
					new CalLiteGUIException("The data for geting the table name is wrong", ex));
		}
		catch(CalLiteGUIException ex)
		{
			_errorHandlingSvc.businessErrorHandler(ex);
		}
	}

	@Override
	public void editButtonOnOperations(JComponent component)
	{
		String tableName = "";
		String fileName = "";
		DataTableModel dataTableModel = null;
		try
		{
			if(component instanceof JButton)
			{
				JButton btn = (JButton) component;
				String titleStr = btn.getText();
				titleStr = titleStr.substring(5);
				TitledBorder title = BorderFactory.createTitledBorder(titleStr);
				JPanel pan = (JPanel) _swingEngine.find("op_panTab");
				pan.setBorder(title);
				String comId = component.getName();
				tableName = _seedDataSvc.getObjByGuiId(comId).getDataTables();

				if(titleStr.equalsIgnoreCase(Constant.SWP))
				{
					fileName = _tableSvc.getWsidiForSWPFullFileName();
				}
				else
				{
					fileName = _tableSvc.getWsidiForCVPFullFileName();
				}
				if(fileName.equalsIgnoreCase(Constant.USER_DEFINED))
				{
					dataTableModel = _scenarioSvc.getUserDefinedTable(tableName);
				}
				else
				{
					dataTableModel = _tableSvc.getWsiDiTable(fileName);
					dataTableModel
							.setTableName(FilenameUtils.removeExtension(Paths.get(fileName).getFileName().toString()));
					dataTableModel.setCellEditable(true);
					_scenarioSvc.addUserDefinedTable(tableName, dataTableModel);
				}
				showTableOnOperations(dataTableModel);
			}
			_auditSvc.addAudit("wsi_di tables", "", fileName);
		}
		catch(CalLiteGUIException ex)
		{
			_errorHandlingSvc.businessErrorHandler(ex);
		}
	}

	@Override
	public void decisionSVInitFilesAndTableInOperations()
	{
		_applyDynamicConDele.changeSVInitFilesAndTableInOperations(true);
		/*
		 * The following code we are setting the SWP and CVP file names as user
		 * defined because the table values we are getting it from the cls file
		 * so we consider them as user defined.
		 */
		_tableSvc.setWsidiForSWPFullFileName(Constant.USER_DEFINED);
		_tableSvc.setWsidiForCVPFullFileName(Constant.USER_DEFINED);
	}

	/**
	 * This method is to ask the use whether he wants to continue or not with
	 * WSI/DI modification. This is used in the "Operations" tab.
	 *
	 * @return will return true if user wants to continue.
	 */
	private boolean isOkToContinueOnOperations()
	{
		String label = ((JLabel) _swingEngine.find("op_WSIDI_Status")).getText();
		if(label.contains(Constant.UNEDITED_FORLABEL))
		{
			return true;
		}
		return (_dialogSvc
				.getYesNo("WSI/DI data tables have been modified.  Are you sure you wish to overwrite these changes?",
						JOptionPane.QUESTION_MESSAGE)
				.equals("Yes"));
	}

	/**
	 * This method is used to show the table in the "Operations" tab.
	 *
	 * @param dataTableModel Table data to show.
	 */
	private void showTableOnOperations(DataTableModel dataTableModel)
	{
		JComponent component = (JComponent) _swingEngine.find("scrOpValues");
		JTable table = (JTable) _swingEngine.find("tblOpValues");
		table.setModel(dataTableModel);
		component.setVisible(true);
		component.setEnabled(true);
		table.setVisible(true);
		table.setCellSelectionEnabled(true);
	}

	@Override
	public void selectingSVAndInitFile(String fileNameForDss, String fPartForDss, String manualFileNameForDss,
									   String manualFPartForDss)
	{
		JFileChooser fChooser = new JFileChooser(Constant.MODEL_W2_DSS_DIR);
		FileNameExtensionFilter filter = new FileNameExtensionFilter("DSS FILES (.dss)", "dss", "dss");
		fChooser.setFileFilter(filter);
		fChooser.setMultiSelectionEnabled(false);
		int val = fChooser.showOpenDialog(_swingEngine.find(Constant.MAIN_FRAME_NAME));
		if(val == JFileChooser.APPROVE_OPTION && verifyTheSelectedFiles(fChooser, ".dss"))
		{
			String fPartResult = "NOT FOUND";
			String fileFullName = fChooser.getSelectedFile().getAbsolutePath();
			try
			{
				// Read all pathnames from the DSS file and set the F-PART
				// textfield as
				// "NOT FOUND","MULTIPLE F-PARTS", or the first F-PART found.
				HecDss hecDss = HecDss.open(fileFullName);
				Vector<String> pathNames = hecDss.getCatalogedPathnames();
				String lastFPart = "";
				for(int i = 0; i < pathNames.size(); i++)
				{
					String[] parts = pathNames.elementAt(0).split("/");
					String newFPart = ((parts.length < 7) || (parts[6] == null)) ? "NOT FOUND" : parts[6];
					if(i == 0)
					{
						lastFPart = newFPart;
						fPartResult = newFPart;
					}
					else if(!lastFPart.equals(newFPart) && !newFPart.equals("NOT FOUND"))
					{
						fPartResult = "MULTIPLE F-PARTS";
					}
				}
			}
			catch(Exception ex)
			{
				LOG.debug(ex.getMessage());
				_errorHandlingSvc.businessErrorHandler(new CalLiteGUIException(ex));
			}
			((JTextField) _swingEngine.find(fileNameForDss)).setText(fChooser.getSelectedFile().getName());
			((JTextField) _swingEngine.find(fPartForDss)).setText(fPartResult);
			((JTextField) _swingEngine.find(manualFileNameForDss)).setText(fChooser.getSelectedFile().getName());
			((JTextField) _swingEngine.find(manualFPartForDss)).setText(fPartResult);
		}
	}

	@Override
	public void helpButton()
	{
		try
		{
			JTabbedPane jtp = (JTabbedPane) _swingEngine.find("tabbedPane1");
			String label = jtp.getTitleAt(jtp.getSelectedIndex());
			CalLiteHelp calLiteHelp = new CalLiteHelp();
			calLiteHelp.showHelp(label);
		}
		catch(Exception ex)
		{
			LOG.error("Problem with CalLite Help " + ex.getMessage());
		}
	}

	@Override
	public void aboutButton()
	{
		long longTime = new File(Constant.GUI_XML_FILENAME).lastModified();
		Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("PST"));
		calendar.setTimeInMillis(longTime);
		SimpleDateFormat sdf = new SimpleDateFormat("dd MMMM yyyy");
		String guiXmlDate = sdf.format(calendar.getTime());
		_dialogSvc.getOK(
				"CalLite GUI v. " + _properties.getProperty("version.id") + " ("
						+ System.getProperty("sun.arch.data.model") + "-bit)\nBuild date: "
						+ _properties.getProperty("build.date") + "\nYour last GUI xml revision date: " + guiXmlDate,
				JOptionPane.INFORMATION_MESSAGE);
	}


}
