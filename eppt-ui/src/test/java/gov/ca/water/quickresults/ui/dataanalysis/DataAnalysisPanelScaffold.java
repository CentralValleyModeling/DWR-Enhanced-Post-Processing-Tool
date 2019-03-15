/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.dataanalysis;

import java.awt.BorderLayout;
import javax.swing.*;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2019
 */
public class DataAnalysisPanelScaffold
{
	public static void main(String[] args)
	{
		DataAnalysisPanel dataAnalysisPanel = new DataAnalysisPanel();
		DataAnalysisListener dataAnalysisListener = new DataAnalysisListener(dataAnalysisPanel);
		dataAnalysisPanel.setActionListener(dataAnalysisListener);
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			try
			{
				UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			}
			catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
			{
				fail(e);
			}
			jFrame.setLayout(new BorderLayout());
			jFrame.add(dataAnalysisPanel, BorderLayout.CENTER);
			jFrame.pack();
			jFrame.setVisible(true);
		});
	}
}