/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import java.awt.*;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.*;

import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.bus_service.impl.XMLParsingSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * Floating frame to display Quick Result scenario and display control panels
 *
 * @author tslawecki
 */
public class ControlFrame extends JFrame implements WindowListener
{

	private static final long serialVersionUID = 6984886958106730868L;
	private static final Logger LOG = Logger.getLogger(ControlFrame.class.getName());
	//private final SwingEngine swingEngine;
	private SwingEngine swingEngine = XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine();
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	/**
	 * Constructor moves controls from Quick Results to new frame
	 */
	public ControlFrame()
	{

		//swingEngine = ResultUtilsBO.getResultUtilsInstance(null).getSwix();
		try
		{
			removeClose(this);

			JPanel p = new JPanel(new GridBagLayout());
			add(p);

			GridBagConstraints c = new GridBagConstraints();

			c.gridx = 0;
			c.gridy = 0;
			c.gridheight = 1;
			c.anchor = GridBagConstraints.NORTHWEST;
			p.add(swingEngine.find("ss"), c);

			c.gridy = 1;
			p.add(swingEngine.find("Display"), c);

			p.setSize(new Dimension(450, 748));
			setSize(new Dimension(460, 768));
			setTitle("Result Display Controls");

			addWindowListener(this);
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display control frame.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}

	}

	/**
	 * Removes all "close" buttons from component and children
	 *
	 * @param comp
	 */
	private void removeClose(Component comp)
	{

		if(comp instanceof JButton)
		{
			String accName = comp.getAccessibleContext().getAccessibleName();
			if(accName.equals("Close"))
			{
				comp.getParent().remove(comp);
			}
		}
		if(comp instanceof Container)
		{
			Component[] comps = ((java.awt.Container) comp).getComponents();
			for(int x = 0, y = comps.length; x < y; x++)
			{
				removeClose(comps[x]);
			}
		}
	}

	/**
	 * Places frame to right of "desktop" frame if screen width permits, then
	 * makes visible
	 */
	public void display()
	{

		setSize(new Dimension(460, 768));
		JFrame f = (JFrame) swingEngine.find("desktop");
		int right = f.getWidth() + f.getLocation().x;
		if(right + getWidth() < java.awt.Toolkit.getDefaultToolkit().getScreenSize().width)
		{
			setLocation(right, 0);
		}
		else
		{
			setLocation(0, 0);
		}
		setVisible(true);
		setExtendedState(JFrame.NORMAL);
	}

	@Override
	public void windowActivated(WindowEvent arg0)
	{

	}

	@Override
	public void windowClosed(WindowEvent arg0)
	{
		ResultUtilsBO.getResultUtilsInstance(null).closeControlFrame();

	}

	@Override
	public void windowClosing(WindowEvent arg0)
	{

	}

	@Override
	public void windowDeactivated(WindowEvent arg0)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void windowDeiconified(WindowEvent arg0)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void windowIconified(WindowEvent arg0)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void windowOpened(WindowEvent arg0)
	{
		// TODO Auto-generated method stub

	}

}
