/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.presentation;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.io.IOException;
import javax.swing.*;

import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

/**
 * This frame is used for displaying the monitored status of run and save
 * operations.
 *
 * @author mohan
 */
public final class ProgressFrameForPDF extends JFrame
{

	private static final Logger LOG = Logger.getLogger(ProgressFrameForPDF.class.getName());
	private static final long serialVersionUID = -606008444073979623L;
	private static ProgressFrameForPDF progressFrame;
	private final JList<String> _list;
	private final JScrollPane _listScroller;

	/**
	 * This will prepare the Dialog box to show.
	 */
	private ProgressFrameForPDF(JFrame mainFrame)
	{
		if(mainFrame != null)
		{
			setIconImage(mainFrame.getIconImage());
			setLocationRelativeTo(mainFrame);
		}
		String[] data = {"No reports active"};
		_list = new JList<>(data);
		_listScroller = new JScrollPane(_list);
		try
		{
			initComponents();
			setVisible(true);
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display PDF progress frame.";
			final IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
			errorHandlingSvc.businessErrorHandler(messageText, e);
		}
	}

	/**
	 * This method is for implementing the singleton.
	 *
	 * @return
	 */
	public static ProgressFrameForPDF getProgressFrameInstance(JFrame mainFrame)
	{
		if(progressFrame == null)
		{
			progressFrame = new ProgressFrameForPDF(mainFrame);
		}
		return progressFrame;
	}

	private void initComponents()
	{
		setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		setPreferredSize(new Dimension(400, 210));
		setMinimumSize(new Dimension(400, 210));
		setLayout(new BorderLayout(5, 5));
		setTitle("Monitor for PDF");
		_list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		_list.setLayoutOrientation(JList.VERTICAL);
		_list.setVisibleRowCount(-1);
		_list.setDragEnabled(true);
		_list.setVisible(true);
		_listScroller.setPreferredSize(new Dimension(350, 150));
		_listScroller.setMinimumSize(new Dimension(350, 150));
		_listScroller.setVisible(true);
		add(BorderLayout.PAGE_START, _listScroller);
		JButton btnClose = new JButton("Stop all runs");
		btnClose.setPreferredSize(new Dimension(50, 20));
		btnClose.setMinimumSize(new Dimension(50, 20));
		btnClose.addActionListener(this::closePerformed);
		btnClose.setActionCommand("Stop");
		btnClose.setVisible(true);
		add(BorderLayout.PAGE_END, btnClose);
		pack();
		Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((dim.width - 400) / 2, (dim.height - 200) / 2);
	}

	/**
	 * This will set the listData to the monitor window.
	 *
	 * @param data
	 */
	public void setList(String data)
	{
		String[] listData = new String[1];
		if(!_listScroller.isVisible())
		{
			_listScroller.setVisible(true);
		}
		listData[0] = data;
		_list.setListData(listData);
		repaint();
	}

	public void closePerformed(ActionEvent ae)
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
