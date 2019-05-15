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
package vista.gui;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

/**
 * Takes care of creating a panel with OK, Cancel, and Apply buttons. These
 * buttons then call back the applyChanges and doneChanges functions in the
 * Changeable interface.
 * 
 * @see Changeable
 * 
 */
public class DialogButtonPanel extends JPanel {
	/**
	 * Label for OK button
	 */
	public static final String OK = "OK";
	/**
	 * Label for CANCEL button
	 */
	public static final String CANCEL = "CANCEL";
	/**
	 * Label for APPLY button
	 */
	public static final String APPLY = "APPLY";

	/**
	 * constructor
	 */
	public DialogButtonPanel(Changeable chg) {
		this(chg, true);
	}

	/**
	 * constructor
	 */
	public DialogButtonPanel(Changeable chg, boolean addApplyButton) {

		setLayout(new FlowLayout());

		add(okButton = new JButton(OK));
		add(cancelButton = new JButton(CANCEL));
		if (addApplyButton) {
			addApplyButton();
		}
		addChangeable(chg);

	}

	/**
   *
   */
	public void addApplyButton() {
		if (applyButton == null)
			add(applyButton = new JButton(APPLY));
	}

	/**
   *
   */
	public void removeApplyButton() {
		if (applyButton == null)
			return;
		remove(applyButton);
		applyButton = null;
	}

	/**
	 * Adds the reference to the object that is informed of the button events
	 */
	public void addChangeable(Changeable chg) {
		okButton.addActionListener(new OKListener(chg));
		cancelButton.addActionListener(new CancelListener(chg));
		if (applyButton != null) {
			applyButton.addActionListener(new ApplyListener(chg));
		}
	}

	/**
	 * Listens onto the ok button
	 */
	private class OKListener implements ActionListener {
		/**
		 * constructor
		 */
		public OKListener(Changeable comp) {
			_comp = comp;
		}

		/**
		 * Invoked when an action occurs.
		 */
		public void actionPerformed(ActionEvent e) {
			// System.out.println("OK");
			_comp.applyChanges();
			_comp.doneChanges();
		}

		/**
		 * the object to be informed of the changes
		 */
		Changeable _comp;
	}

	/**
	 * Listens to the cancel button and informs the changeable object
	 */
	private class CancelListener implements ActionListener {
		/**
		 * constructor
		 */
		public CancelListener(Changeable comp) {
			_comp = comp;
		}

		/**
		 * Invoked when an action occurs.
		 */
		public void actionPerformed(ActionEvent e) {
			// System.out.println("Cancel");
			_comp.doneChanges();
		}

		/**
		 * the object to be informed of the changes
		 */
		Changeable _comp;
	}

	/**
	 * Listens to the apply button and informs changeable object
	 */
	private class ApplyListener implements ActionListener {
		/**
		 * constructor
		 */
		public ApplyListener(Changeable comp) {
			_comp = comp;
		}

		/**
		 * Invoked when an action occurs.
		 */
		public void actionPerformed(ActionEvent e) {
			// System.out.println("Apply");
			_comp.applyChanges();
		}

		/**
		 * the object to be informed of the changes
		 */
		Changeable _comp;
	}

	/**
	 * The ok button
	 */
	private JButton okButton;
	/**
	 * The cancel button
	 */
	private JButton cancelButton;
	/**
	 * The apply button
	 */
	private JButton applyButton;
}
