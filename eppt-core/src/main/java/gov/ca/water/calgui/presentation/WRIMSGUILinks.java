/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

/**
 *
 */
package gov.ca.water.calgui.presentation;

import java.awt.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Scanner;
import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;

import calsim.app.AppUtils;
import calsim.app.Project;
import calsim.gui.CalLiteGUIPanelWrapper;
import calsim.gui.GuiUtils;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.bus_service.impl.XMLParsingSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 *
 * Methods for updating Project values in the WRIMS GUI object space from
 * CalLite GUI
 *
 * @author tslawecki
 *
 */
public class WRIMSGUILinks
{

	private static final Logger LOG = Logger.getLogger(WRIMSGUILinks.class.getName());
	private static SwingEngine swingEngine = XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine();
	private static IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	/**
	 * This method is intended to update the status label inside the WRIMS GUI
	 * panel to reflect progress reading data files. It is currently a stub.
	 *
	 * @param text
	 */
	public static void setStatus(String text)
	{
		LOG.debug("Status update to '" + text + "'");
		// statusLabel.setText(text);
		// statusLabel.invalidate();
	}

	/**
	 * Builds the WRIMS GUI panel for use in CalLite GUI while extracting a
	 * reference to the status label.
	 *
	 * @param p
	 *            Container panel for the WRIMS GUI panel
	 */
	public static void buildWRIMSGUI(JPanel p)
	{

		try
		{
			p.setSize(900, 650);

			CalLiteGUIPanelWrapper pw = new CalLiteGUIPanelWrapper(
					(JFrame) ResultUtilsBO.getResultUtilsInstance(null).getSwix().find(Constant.MAIN_FRAME_NAME));
			pw.getPanel().setSize(900, 650);
			p.add(pw.getPanel(), BorderLayout.NORTH);
			JPanel statusPanel = GuiUtils.getStatusPanel();
			p.add(statusPanel, BorderLayout.CENTER);
			GuiUtils.setStatus("Initialized.");
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to update WRIMS GUI files.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}

		// statusLabel = (JLabel) statusPanel.getComponent(2);
	}

	/**
	 * Update WRIMS GUI project file names from file list
	 *
	 * @param theList
	 *            List of file names
	 */
	public static void updateProjectFiles(JList theList)
	{

		// Get project and clear

		try
		{
			Project project = ResultUtilsBO.getResultUtilsInstance(null).getProject();
			project.setDVFile("");
			project.setDV2File("");
			project.setDV3File("");
			project.setDV4File("");
			AppUtils.baseOn = false;

			// Find and set files

			if(theList.getModel().getSize() == 1)
			{

				RBListItemBO item = (RBListItemBO) theList.getModel().getElementAt(0);
				String dvFileName = item.toString();
				project.setDVFile(dvFileName);
				project.setDVHashtable();

				String svFileName = item.getSVFilename();
				if(svFileName.equals(""))
				{
					svFileName = findSVFileName(dvFileName);
					item.setSVFilename(svFileName);
				}

				project.setSVFile(svFileName);
				if(!(svFileName == null))
				{
					File svFile = new File(svFileName);
					if(svFile.exists() && !svFile.isDirectory())
					{
						project.setSVHashtable();
					}
				}

				AppUtils.baseOn = true;

			}
			else
			{

				int dssCount = 1;
				for(int i = 0; i < theList.getModel().getSize(); i++)
				{

					RBListItemBO item = (RBListItemBO) theList.getModel().getElementAt(i);
					String dvFileName = item.toString();
					String svFileName = item.getSVFilename();
					if(svFileName.equals(""))
					{
						svFileName = findSVFileName(dvFileName);
						item.setSVFilename(svFileName);
					}
					if(item.isSelected())
					{
						project.setDVFile(dvFileName);
						project.setSVFile(svFileName);
						AppUtils.baseOn = true;
					}
					else
					{
						dssCount++;
						switch(dssCount)
						{
							case 2:
								project.setDV2File(dvFileName);
								project.setSV2File(svFileName);
								break;
							case 3:
								project.setDV3File(dvFileName);
								project.setSV3File(svFileName);
								break;
							case 4:
								project.setDV4File(dvFileName);
								project.setSV4File(svFileName);
								break;
						}
					}
				}
			}
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to update WRIMS GUI files.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
	}

	/**
	 * Finds scenario's SV file by looking up SV file name in scenario (.cls)
	 * file
	 *
	 * @param dvFileName
	 * @return Full name of SV file including path
	 */

	private static String findSVFileName(String dvFileName)
	{

		// CalLite scenario file is in the same directory as the DV file

		String clsFileName = dvFileName.substring(0, dvFileName.length() - 7) + ".cls";
		File clsF = new File(clsFileName);
		String svFileName = "";
		try
		{
			Scanner scanner;
			scanner = new Scanner(new FileInputStream(clsF.getAbsolutePath()));
			while(scanner.hasNextLine() && svFileName.equals(""))
			{
				String text = scanner.nextLine();
				if(text.startsWith("hyd_DSS_SV|"))
				{

					String[] texts = text.split("[|]");
					svFileName = texts[1];
				}
			}
			scanner.close();

		}
		catch(IOException e)
		{
			LOG.info(clsF.getName() + " not openable - checking for like-named SV file");
		}

		if(!svFileName.equals(""))
		{

			// Found in CLS - Build string pointing to
			// "Scenarios/Run_Details/scenarioname/Run/DSS/svfilename"

			String svPathString = dvFileName.substring(0, dvFileName.length() - 7); // Strip
			// out
			// "_DV.DSS"
			int i = svPathString.lastIndexOf("\\"); // find rightmost "/"
			svFileName = svPathString.substring(0, i) + "\\Run_Details" + svPathString.substring(i) + "\\Run\\DSS\\"
					+ svFileName;

		}
		else
		{

			// Not found in CLS: first, check if there's a corresponding SV.DSS

			if(dvFileName.substring(dvFileName.length() - 6).toUpperCase().equals("DV.DSS"))
			{
				svFileName = dvFileName.substring(0, dvFileName.length() - 6) + "SV.dss";
				File svF = new File(svFileName);
				if(svF.exists() && !svF.isDirectory())
				{
					LOG.info("Found like-named SV file - " + svFileName);
				}
				else
				{
					svFileName = "";
				}

				if(svFileName.equals(""))
				{

					// No corresponding SV.DSS - get file from file dialog!

					JFileChooser fc = new JFileChooser();
					fc.setCurrentDirectory(new File(dvFileName));
					fc.setDialogTitle("Set SV file for " + dvFileName);
					fc.setFileFilter(new FileNameExtensionFilter("DSS File *.dss", "dss"));
					if(fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION)
					{
						svFileName = fc.getSelectedFile().getAbsolutePath();
						LOG.info("SV file set - " + svFileName);
					}
					else
					{
						LOG.info("No SV file set for " + dvFileName + "!");
					}
				}
			}
		}

		return svFileName;
	}
}
