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
package vista.app;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.*;

import vista.set.Group;
import vista.set.Pathname;

/**
 * A default quitable frame.
 *
 * @author Nicky Sandhu
 * @version $Id: GroupTreeFrame.java,v 1.1 2003/10/02 20:48:32 redwood Exp $
 */
public class GroupTreeFrame extends JFrame
{
	/**
	 *
	 */
	final static String A_PART = "A PART";
	/**
	 *
	 */
	final static String B_PART = "B PART";
	/**
	 *
	 */
	final static String C_PART = "C PART";
	/**
	 *
	 */
	final static String D_PART = "D PART";
	/**
	 *
	 */
	final static String E_PART = "E PART";
	/**
	 *
	 */
	final static String F_PART = "F PART";
	/**
	 *
	 */
	private final static String FFWD = ">>";
	/**
	 *
	 */
	private final static String RRWD = "<<";
	/**
	 *
	 */
	private static final String ORDER_BY_ = "Order by ";
	/**
	 *
	 */
	private GroupTree _groupTree;
	/**
	 *
	 */
	private JPanel _treePane;
	/**
	 *
	 */
	private int[] _partOrder = {Pathname.A_PART, Pathname.C_PART,
			Pathname.B_PART, Pathname.D_PART, Pathname.E_PART, Pathname.F_PART};
	/**
	 *
	 */
	private JLabel _label;
	/**
	 * adds the given component to the center of the frame.
	 */
	public GroupTreeFrame(Group g)
	{
		_groupTree = new GroupTree(g);
		JTree tree = new JTree(_groupTree.getRoot());
		getContentPane().setLayout(new BorderLayout());

		_treePane = new JPanel(true);
		_treePane.setLayout(new BorderLayout());
		_treePane.add("Center", tree);
		JViewport port = new JViewport();
		port.add(_treePane);

		JScrollPane scrollpane = new JScrollPane();
		scrollpane.setViewport(port);

		WindowListener l = new DefaultWindowListener();
		addWindowListener(l);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		JButton forwardButton = new JButton(FFWD);
		buttonPanel.add(forwardButton);
		JButton backwardButton = new JButton(RRWD);
		buttonPanel.add(backwardButton);

		ActionListener orderListener = new OrderByListener();
		forwardButton.addActionListener(orderListener);
		backwardButton.addActionListener(orderListener);

		getContentPane().add(scrollpane);
		_label = new JLabel(getPartOrderString());
		getContentPane().add("North", _label);
		getContentPane().add("South", buttonPanel);

		pack();
		setVisible(true);
	}

	/**
	 *
	 */
	private String getPartOrderString()
	{
		StringBuffer buf = new StringBuffer(_partOrder.length * 8);
		buf.append("Pathname Part Order: ");
		for(int i = 0; i < _partOrder.length; i++)
		{
			buf.append(" ").append(Pathname.getPartName(_partOrder[i]));
		}
		return buf.toString();
	}

	/**
	 * sets tree
	 */
	private void circleForward()
	{
		int tmp = _partOrder[0];
		for(int i = 1; i < _partOrder.length; i++)
		{
			_partOrder[i - 1] = _partOrder[i];
		}
		_partOrder[_partOrder.length - 1] = tmp;
		_groupTree.setPartAdditionOrder(_partOrder);

		JTree tree = new JTree(_groupTree.getRoot());
		_treePane.removeAll();
		_treePane.add(tree);
		_label.setText(getPartOrderString());
		paintAll(getGraphics());
	}

	/**
	 * sets tree
	 */
	private void circleBackward()
	{
		int tmp = _partOrder[_partOrder.length - 1];
		for(int i = _partOrder.length - 2; i >= 0; i--)
		{
			_partOrder[i + 1] = _partOrder[i];
		}
		_partOrder[0] = tmp;

		_groupTree.setPartAdditionOrder(_partOrder);

		JTree tree = new JTree(_groupTree.getRoot());
		_treePane.removeAll();
		_treePane.add(tree);
		_label.setText(getPartOrderString());
		paintAll(getGraphics());
	}

	/**
	 *
	 */
	private class DefaultWindowListener extends WindowAdapter
	{
		/**
		 *
		 */
		public final void windowClosing(WindowEvent e)
		{
			setVisible(false);
			dispose();
		}
	} // end of DefaultWindowListener

	/**
	 *
	 */
	private class OrderByListener implements ActionListener
	{
		/**
		 *
		 */
		public void actionPerformed(ActionEvent evt)
		{
			Object obj = evt.getSource();
			if(obj instanceof JButton)
			{
				String label = ((JButton) obj).getText();
				if(label.equals(FFWD))
				{
					circleForward();
				}
				else if(label.equals(RRWD))
				{
					circleBackward();
				}
			}
		}
	}
}
