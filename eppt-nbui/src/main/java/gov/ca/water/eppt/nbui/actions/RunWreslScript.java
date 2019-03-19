/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui.actions;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.nio.file.Path;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.bus_service.IModelRunSvc;
import gov.ca.water.calgui.bus_service.impl.ModelRunSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.presentation.ProgressFrame;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle;
import org.openide.windows.WindowManager;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-18-2019
 */
@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.RunWreslScript"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/run.png",
		displayName = "Run WRESL Script"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/Tools", position = 0000)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 666)
		})
@NbBundle.Messages("CTL_RunWreslScript=Run WRESL Script")
public class RunWreslScript implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(RunWreslScript.class.getName());
	private final IModelRunSvc _modelRunSvc = new ModelRunSvcImpl();

	@Override
	public void actionPerformed(ActionEvent e)
	{
		File selectedFile = chooseScript();
		if(selectedFile != null)
		{
			runScript(selectedFile.toPath());
		}
	}

	private void runScript(Path selectedFile)
	{
		try
		{
			ProgressFrame progressFrame = ProgressFrame.getProgressFrameInstance();
			progressFrame.addScenarioNamesAndAction(selectedFile, Constant.BATCH_RUN);
			progressFrame.setBtnText(Constant.STATUS_BTN_TEXT_STOP);
			progressFrame.setIconImage(WindowManager.getDefault().getMainWindow().getIconImage());
			progressFrame.makeDialogVisible();
			_modelRunSvc.doBatch(Collections.singletonList(selectedFile), false);
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Error running WRESL Script: " + selectedFile, ex);
		}
	}

	private File chooseScript()
	{
		JFileChooser fileChooser = new JFileChooser();
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setCurrentDirectory(EpptPreferences.getScenariosPaths().toFile());
		fileChooser.showOpenDialog(WindowManager.getDefault().getMainWindow());
		fileChooser.setMultiSelectionEnabled(false);
		return fileChooser.getSelectedFile();
	}
}
