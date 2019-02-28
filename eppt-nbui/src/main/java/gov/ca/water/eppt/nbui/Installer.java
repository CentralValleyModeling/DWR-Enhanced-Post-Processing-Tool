/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.openide.modules.ModuleInstall;
import org.openide.windows.WindowManager;

import rma.swing.logging.DialogLogHandler;

public class Installer extends ModuleInstall
{

	@Override
	public void restored()
	{
		initLogger();
	}

	private void initLogger()
	{
		//Root logger
		Logger logger = Logger.getLogger("");

		//nix the netbeans logger
		Logger netbeansLogger = Logger.getLogger("org.netbeans.core");
		netbeansLogger.setParent(logger);
		netbeansLogger.setUseParentHandlers(true);

		WindowManager.getDefault().invokeWhenUIReady(() ->
		{
			WindowManager.getDefault().getMainWindow();
			String publishedLevel = Level.SEVERE.getName();
			String showDialogLevel = Level.SEVERE.getName();

			DialogLogHandler handler = DialogLogHandler.getInstance();
			handler.addIgnoredLoggerName("netbeans");
			handler.addIgnoredSourceClass("org.netbeans");
			handler.addIgnoredSourceClass("org.openide");

			handler.setLevel(Level.parse(publishedLevel));
			handler.setShowDialogLevel(Level.parse(showDialogLevel));

			handler.setFrame(true);
			handler.setTitle("REGI Error Log");
			handler.setParentWindow(WindowManager.getDefault().getMainWindow());

			logger.addHandler(handler);
		});
	}
}
