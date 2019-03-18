/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

/**
 * This frame is used for displaying updated status during long-duration
 * activities like saving scenarios and running CalLite.
 *
 * @author Mohan
 */
public final class ProgressFrame extends JFrame implements ActionListener
{

	private static final Logger LOG = Logger.getLogger(ProgressFrame.class.getName());
	private static final long serialVersionUID = -606008444073979623L;
	private static ProgressFrame progressFrame;
	private final ProgressFrameSwingWorker _workerScenarioMonitor;
	private JList<String> _list;
	private JScrollPane _listScroller;
	private JButton _btnClose;

	/**
	 * This will prepare the Dialog box to show.
	 *
	 * @param title The title of the frame.
	 */
	private ProgressFrame(String title, ProgressFrameSwingWorker workerScenarioMonitor)
	{
		IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
		_workerScenarioMonitor = workerScenarioMonitor;
		try
		{
			setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			setPreferredSize(new Dimension(400, 210));
			setMinimumSize(new Dimension(400, 210));
			setLayout(new BorderLayout(5, 5));
			setTitle(title);
			String[] start = {"No scenarios active"};
			_list = new JList<>(start);

			_list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			_list.setLayoutOrientation(JList.VERTICAL);
			_list.setVisibleRowCount(-1);
			_list.setDragEnabled(true);
			_list.setVisible(true);
			_listScroller = new JScrollPane(_list);
			_listScroller.setPreferredSize(new Dimension(350, 150));
			_listScroller.setMinimumSize(new Dimension(350, 150));
			_listScroller.setVisible(true);
			add(BorderLayout.PAGE_START, _listScroller);
			_btnClose = new JButton(Constant.STATUS_BTN_TEXT_CLOSE);
			_btnClose.setPreferredSize(new Dimension(50, 20));
			_btnClose.setMinimumSize(new Dimension(50, 20));
			_btnClose.addActionListener(this);
			_btnClose.setActionCommand("Stop");
			_btnClose.setVisible(true);
			add(BorderLayout.PAGE_END, _btnClose);
			pack();
			Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
			setLocation((dim.width - 400) / 2, (dim.height - 200) / 2);
			addWindowListener(new java.awt.event.WindowAdapter()
			{
				@Override
				public void windowClosed(java.awt.event.WindowEvent windowEvent)
				{
					_workerScenarioMonitor.cancel(true);
					progressFrame = null;
				}
			});

			setVisible(true);
			_list.invalidate();
			repaint();
			_workerScenarioMonitor.execute();
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display progress frame.";
			errorHandlingSvc.businessErrorHandler(messageText, e);
		}
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static ProgressFrame getProgressFrameInstance()
	{
		if(progressFrame == null)
		{
			ProgressFrameSwingWorker workerScenarioMonitor = new ProgressFrameSwingWorker(ProgressFrame::updateBtnText,
					ProgressFrame::updateList);
			progressFrame = new ProgressFrame("", workerScenarioMonitor);
		}
		return progressFrame;
	}

	private static void updateBtnText(String text)
	{
		SwingUtilities.invokeLater(() ->
		{
			if(progressFrame != null)
			{
				progressFrame.setBtnText(text);
			}
		});
	}

	private static void updateList(String[] listData)
	{
		SwingUtilities.invokeLater(() ->
		{
			if(progressFrame != null)
			{
				progressFrame.setList(listData);
			}
		});
	}

	/**
	 * Sets the text for the button at the botom of the dialog
	 *
	 * @param text
	 */
	public void setBtnText(String text)
	{
		_btnClose.setText(text);
	}

	/**
	 * This method will set the monitor window to visible.
	 */
	public void makeDialogVisible()
	{
		setVisible(true);
		_list.invalidate();
		repaint();
	}

	/**
	 * This will set the listData to the monitor window.
	 *
	 * @param listData The list of values to be displayed.
	 */
	public void setList(String[] listData)
	{
		if(!_listScroller.isVisible())
		{
			_listScroller.setVisible(true);
		}
		_list.setListData(listData);
		repaint();
	}

	/**
	 * This method will add the key and type to the monitor list.
	 *
	 * @param key  The name of which we should monitor.
	 * @param type which type.
	 */
	public void addScenarioNamesAndAction(Path key, String type)
	{
		_workerScenarioMonitor.addScenario(key, type);
		if(_listScroller.isVisible())
		{
			invalidate();
		}
	}

	/**
	 * This method will add the list of key and type to the monitor list.
	 *
	 * @param keys The names of which we should monitor.
	 * @param type which type like save or batch run.
	 */
	public void addScenarioNamesAndAction(List<Path> keys, String type)
	{
		keys.forEach(key -> _workerScenarioMonitor.addScenario(key, type));

		if(_listScroller.isVisible())
		{
			invalidate();
		}
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
			if(_btnClose.getText().equals(Constant.STATUS_BTN_TEXT_CLOSE))
			{
				this.setVisible(false);
			}
			else
			{
				Runtime rt = Runtime.getRuntime();
				try
				{
					rt.exec("taskkill /f /t /fi \"WINDOWTITLE eq CalLiteRun*\" ");
					_workerScenarioMonitor.clearScenarios();
					setBtnText(Constant.STATUS_BTN_TEXT_CLOSE);
				}
				catch(IOException ex)
				{
					LOG.error(ex);
				}
			}
		}
	}

}
