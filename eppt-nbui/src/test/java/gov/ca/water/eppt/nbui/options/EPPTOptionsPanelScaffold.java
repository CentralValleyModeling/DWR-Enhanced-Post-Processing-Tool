/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui.options;

import java.awt.BorderLayout;
import javax.swing.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-13-2019
 */
public class EPPTOptionsPanelScaffold
{
	public static void main(String[] args)
	{
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			jFrame.setLayout(new BorderLayout());
			jFrame.add(new EPPTOptionsPanel(new EPPTOptionsOptionsPanelController()), BorderLayout.CENTER);
			jFrame.pack();
			jFrame.setVisible(true);
		});
	}
}
