/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.BorderLayout;

import org.swixml.SwingEngine;

import rma.swing.RmaJPanel;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-21-2019
 */
public class QuickResultsPanel extends RmaJPanel
{
	private static final String QUICK_RESULTS_XML_PATH = "ui/Quick_Results.xml";
	private final SwingEngine _engine;

	public QuickResultsPanel()
	{
		super.setLayout(new BorderLayout());
		_engine = new SwingEngine();
		try
		{
			_engine.render(QUICK_RESULTS_XML_PATH);
		}
		catch(Exception e)
		{
			throw new IllegalStateException("Unable to find " + QUICK_RESULTS_XML_PATH + " file", e);
		}
		super.add(_engine.find("Reporting"));
	}
}
