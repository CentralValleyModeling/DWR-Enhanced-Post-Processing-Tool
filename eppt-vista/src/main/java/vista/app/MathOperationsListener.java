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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import vista.set.DataReference;
import vista.set.DataReferenceMath;

/**
 * Listens for math operations on table and performs them.
 * 
 * @author Nicky Sandhu
 * @version $Id: MathOperationsListener.java,v 1.1 2003/10/02 20:48:34 redwood
 *          Exp $
 */
class MathOperationsListener implements ListSelectionListener {
	/**
	 * adds a key listener for +,-,/,*,= for frame containing group table
	 */
	public MathOperationsListener(GroupTable table) {
		_table = table;
		JComponent comp = table.getTable();
		// add equals listener
		ActionListener l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				equalsKeyPressed(evt);
			}
		};
		KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_EQUALS,
				InputEvent.CTRL_MASK, true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add + listener
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				addKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_ADD, InputEvent.CTRL_MASK, true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add - listener
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				subtractKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_SUBTRACT, InputEvent.CTRL_MASK,
				true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add / listener
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				divideKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_DIVIDE, InputEvent.CTRL_MASK,
				true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add multiply key
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				multiplyKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_MULTIPLY, InputEvent.CTRL_MASK,
				true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
	}

	/**
   *
   */
	public void actionPerformed(ActionEvent evt) {
	}

	/**
   *
   */
	public void valueChanged(ListSelectionEvent evt) {
	}

	/**
   *
   */
	public void equalsKeyPressed(ActionEvent evt) {
		System.out.println("Equals key pressed");
		_ref2 = (DataReference) _table.getSelectedValue();
		System.out.println("Selected " + _ref2 + " for math operation");
		try {
			if (_addKeyMode) {
				DataReference ref = DataReferenceMath.vectorOperation(_ref1,
						_ref2, DataReferenceMath.ADD);
				System.out.println("done creating new reference");
				_table.getGroup().addDataReference(ref);
				_addKeyMode = false;
			}
			if (_table.isVisible()) {
				_table.paintAll(_table.getGraphics());
			}
		} catch (IllegalArgumentException iae) {
			iae.printStackTrace();
		}
		_selectionMode = false;
	}

	/**
   *
   */
	public void addKeyPressed(ActionEvent evt) {
		System.out.println("Add key pressed");
		if (!_selectionMode) {
			_addKeyMode = true;
			_ref1 = (DataReference) _table.getSelectedValue();
			_selectionMode = true;
		}
		System.out.println("Selected " + _ref1 + " for math operation");
	}

	/**
   *
   */
	private GroupTable _table;
	/**
   *
   */
	private boolean _addKeyMode = false;
	/**
   *
   */
	private DataReference _ref1, _ref2;
	/**
   *
   */
	private boolean _selectionMode = false;

	/**
   *
   */
	public void subtractKeyPressed(ActionEvent evt) {
	}

	/**
   *
   */
	public void multiplyKeyPressed(ActionEvent evt) {
	}

	/**
   *
   */
	public void divideKeyPressed(ActionEvent evt) {
	}
}
