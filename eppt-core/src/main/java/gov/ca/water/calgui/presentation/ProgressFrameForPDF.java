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
import java.awt.event.ActionEvent;
import java.util.List;
import javax.swing.*;

/**
 * This frame is used for displaying the monitored status of run and save
 * operations.
 *
 * @author mohan
 */
final class ProgressFrameForPDF extends JFrame
{

	private static final long serialVersionUID = -606008444073979623L;
	private final JList<String> _list;
	private final JScrollPane _listScroller;
	private final Report _report;
	private static ProgressFrameForPDF instance;

	/**
	 * This will prepare the Dialog box to show.
	 */
	private ProgressFrameForPDF(JFrame mainFrame, Report report)
	{
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		_report = report;
		String[] data = {"No reports active"};
		_list = new JList<>(data);
		_listScroller = new JScrollPane(_list);
		initComponents();
		setIconImage(mainFrame.getIconImage());
	}

	static ProgressFrameForPDF getInstance(JFrame mainFrame, Report report)
	{
		if(instance == null)
		{
			instance = new ProgressFrameForPDF(mainFrame, report);
		}
		instance.setLocationRelativeTo(mainFrame);
		instance.setVisible(true);
		return instance;
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
	}

	void setList(List<String> data)
	{
		if(!_listScroller.isVisible())
		{
			_listScroller.setVisible(true);
		}
		_list.setListData(data.toArray(new String[0]));
		repaint();
	}

	void setList(String data)
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

	private void closePerformed(ActionEvent ae)
	{
		if("Go".equals(ae.getActionCommand()))
		{
			this.setVisible(true);
		}
		else if("Stop".equals(ae.getActionCommand()))
		{
			_report.cancel(true);
		}
	}
}
