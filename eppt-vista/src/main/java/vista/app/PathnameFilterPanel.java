/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.*;

import vista.app.commands.GroupPathnameFilterCommand;
import vista.set.Group;
import vista.set.Pathname;

/**
 * Creates a panel with controls for filtering on data references of a group.
 * Most filtering is done on pathnames or parts thereof using regular
 * expressions. One exception to this is the D part of the pathname which
 * represents the time window and has a TimeWindowFilter filter.
 * 
 * @see vista.set.TimeWindowFilter
 * @see vista.set.PathPartPredicate
 * @author Nicky Sandhu
 * @version $Id: PathnameFilterPanel.java,v 1.1 2003/10/02 20:48:37 redwood Exp
 *          $
 */
public class PathnameFilterPanel extends JPanel {
	/**
	 * constructor
	 */
	public PathnameFilterPanel(GroupTable parent) {
		_groupTable = parent;

		JPanel filterPanel = new JPanel();
		filterPanel.setLayout(new GridLayout(1, Pathname.MAX_PARTS + 1));
		// make choice panel
		JPanel choicePanel = new JPanel();
		choicePanel.setLayout(new BorderLayout());
		choicePanel.add(new JLabel("Filter"), BorderLayout.NORTH);
		_filterChoice = new JComboBox();
		_filterChoice.addItem("Select");
		_filterChoice.addItem("Reject");
		_filterChoice.setSelectedItem("Select");
		choicePanel.add(_filterChoice, BorderLayout.SOUTH);

		filterPanel.add(choicePanel);
		// 
		_textFields = new JTextField[Pathname.MAX_PARTS];

		for (int i = 0; i < Pathname.MAX_PARTS; i++) {
			JPanel panel = new JPanel();
			panel.setLayout(new BorderLayout());
			panel.add(new JLabel(Pathname.getPartName(i)), BorderLayout.NORTH);
			_textFields[i] = new JTextField("");
			_textFields[i].addKeyListener(new FilterListener());
			panel.add(_textFields[i], BorderLayout.SOUTH);
			filterPanel.add(panel);
		}

		JPanel pathNameFilterPanel = new JPanel();
		pathNameFilterPanel.setLayout(new GridLayout(1, 2));
		pathNameFilterPanel.add(new JLabel("Pathname Filter : "));
		_pathTextField = new JTextField("");
		_pathTextField.addKeyListener(new FilterListener());
		pathNameFilterPanel.add(_pathTextField);
		setLayout(new BorderLayout());
		add(filterPanel, BorderLayout.SOUTH);
		add(pathNameFilterPanel, BorderLayout.NORTH);
	}

	/**
	 * The table containing the group
	 */
	private GroupTable _groupTable;
	/**
	 * filter choice, selecting/rejecting
	 */
	private JComboBox _filterChoice;
	/**
	 * pathname field
	 */
	private JTextField _pathTextField;
	/**
	 * path part fields
	 */
	private JTextField[] _textFields;

	/**
   *
   */
	private class FilterListener implements KeyListener {
		/**
		 * filters on pressing enter key on any field...
		 */
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() != KeyEvent.VK_ENTER)
				return;
			boolean selecting = true;
			if (_filterChoice.getSelectedItem().equals("Reject"))
				selecting = false;
			String regExp = _pathTextField.getText().trim();
			Group group = _groupTable.getGroup();
			String[] regExps = new String[Pathname.MAX_PARTS];
			for (int i = 0; i < Pathname.MAX_PARTS; i++) {
				regExps[i] = _textFields[i].getText().trim();
				_textFields[i].setText("");
			}
			_pathTextField.setText("");
			_pathTextField.repaint();
			// filtering by pathname
			Executor.execute(new GroupPathnameFilterCommand(group, regExp,
					selecting), _groupTable);
			// filtering by pathname parts
			Executor.execute(new GroupFilterCommand(group, regExps, selecting),
					_groupTable);
		}

		public void keyTyped(KeyEvent evt) {
		}

		public void keyReleased(KeyEvent evt) {
		}
	}// endof FilterListener class
}
