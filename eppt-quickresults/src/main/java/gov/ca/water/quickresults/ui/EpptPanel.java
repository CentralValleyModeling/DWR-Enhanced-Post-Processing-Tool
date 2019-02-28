/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import org.swixml.SwingEngine;

import rma.swing.RmaJPanel;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-28-2019
 */
public class EpptPanel extends RmaJPanel
{
	private final SwingEngine _swingEngine;

	public EpptPanel()
	{
		_swingEngine = new SwingEngine();
	}

	SwingEngine getSwingEngine()
	{
		return _swingEngine;
	}
}
