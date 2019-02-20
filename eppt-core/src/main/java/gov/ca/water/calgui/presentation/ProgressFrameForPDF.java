/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import javax.swing.*;

import gov.ca.water.calgui.bus_service.impl.XMLParsingSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * This frame is used for displaying the monitored status of run and save
 * operations.
 *
 * @author mohan
 */
public final class ProgressFrameForPDF extends JFrame implements ActionListener
{

	private static final Logger LOG = Logger.getLogger(ProgressFrameForPDF.class.getName());
	private static final long serialVersionUID = -606008444073979623L;
	private static ProgressFrameForPDF progressFrame;
	private JList list;
	private JScrollPane listScroller;
	private SwingEngine swingEngine = XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine();
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	/**
	 * This will prepare the Dialog box to show.
	 */
	private ProgressFrameForPDF()
	{
		try
		{
			setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			setPreferredSize(new Dimension(400, 210));
			setMinimumSize(new Dimension(400, 210));
			setLayout(new BorderLayout(5, 5));
			setTitle("Monitor for PDF");
			String[] data = {"No reports active"};
			list = new JList(data);
			list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			list.setLayoutOrientation(JList.VERTICAL);
			list.setVisibleRowCount(-1);
			list.setDragEnabled(true);
			list.setVisible(true);
			listScroller = new JScrollPane(list);
			listScroller.setPreferredSize(new Dimension(350, 150));
			listScroller.setMinimumSize(new Dimension(350, 150));
			listScroller.setVisible(true);
			add(BorderLayout.PAGE_START, listScroller);
			JButton btnClose = new JButton("Stop all runs");
			btnClose.setPreferredSize(new Dimension(50, 20));
			btnClose.setMinimumSize(new Dimension(50, 20));
			btnClose.addActionListener(this);
			btnClose.setActionCommand("Stop");
			btnClose.setVisible(true);
			add(BorderLayout.PAGE_END, btnClose);
			pack();
			Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
			setLocation((dim.width - 400) / 2, (dim.height - 200) / 2);
			java.net.URL imgURL = getClass().getResource("/images/CalLiteIcon.png");
			setIconImage(Toolkit.getDefaultToolkit().getImage(imgURL));
			setVisible(true);
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display PDF progress frame.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
	}

	/**
	 * This method is for implementing the singleton.
	 *
	 * @return
	 */
	public static ProgressFrameForPDF getProgressFrameInstance()
	{
		if(progressFrame == null)
		{
			progressFrame = new ProgressFrameForPDF();
		}
		return progressFrame;
	}

	/**
	 * This will set the listData to the monitor window.
	 *
	 * @param data
	 */
	public void setList(String data)
	{
		String[] listData = new String[1];
		if(!listScroller.isVisible())
		{
			listScroller.setVisible(true);
		}
		listData[0] = data;
		list.setListData(listData);
		repaint();
		// paintComponents(this.getGraphics());
	}

	@Override
	public void actionPerformed(ActionEvent ae)
	{
		if("Go".equals(ae.getActionCommand()))
		{
			this.setVisible(true);
		}
		else if("Stop".equals(ae.getActionCommand()))
		{
			Runtime rt = Runtime.getRuntime();
			Process proc;
			try
			{
				proc = rt.exec("taskkill /f /t /fi \"WINDOWTITLE eq CalLiteRun*\" ");
			}
			catch(IOException ex)
			{
				LOG.error(ex);
			}
		}
	}
}