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
import java.awt.Frame;
import java.awt.GridLayout;
import javax.swing.*;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;
import vista.gui.VistaUtils;
import vista.set.FlagUtils;

/**
 * A dialog for setting a range of selected data to a flag choice
 * 
 * @author Nicky Sandhu
 * @version $Id: FlagChoiceFrame.java,v 1.1 2003/10/02 20:48:29 redwood Exp $
 */
public class FlagChoiceFrame extends JDialog implements Changeable {
	private JRadioButton _rb, _qb, _gb, _eb;
	private FlagEditor _fe;

	/**
	 * flag choice dialog attached to table or graph frame
	 */
	public FlagChoiceFrame(Frame parent, FlagEditor fe) {
		super(parent);
		_fe = fe;
		setTitle("Flag Data As...");
		getContentPane().setLayout(new BorderLayout());
		// add panel containing choices
		ButtonGroup bg = new ButtonGroup();
		bg.add(_rb = new JRadioButton("Reject"));
		bg.add(_qb = new JRadioButton("Questionable"));
		bg.add(_gb = new JRadioButton("Good"));
		_gb.setSelected(true);
		bg.add(_eb = new JRadioButton("Email maintainers"));
		JPanel bp = new JPanel();
		bp.setLayout(new GridLayout(4, 1));
		bp.add(_rb);
		bp.add(_qb);
		bp.add(_gb);
		bp.add(_eb);
		getContentPane().add(bp, BorderLayout.CENTER);
		// add button panel
		getContentPane().add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		pack();
		// position dialog to the upper left non-overlapping position
		// Rectangle r = parent.getBounds();
		// setLocation( r.x + r.width/2, r.y + r.height/2 );
		VistaUtils.positionComponent(this, parent, 0);
		// VistaUtils.positionComponent(this, parent,
		// SwingConstants.NORTH_EAST);
		setVisible(true);
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges() {
		int flagId = FlagUtils.UNSCREENED_FLAG;
		if (_rb.isSelected()) {
			flagId = FlagUtils.REJECT_FLAG;
		} else if (_qb.isSelected()) {
			flagId = FlagUtils.QUESTIONABLE_FLAG;
		} else if (_gb.isSelected()) {
			flagId = FlagUtils.OK_FLAG;
		} else {
			// send email to maintainers with data reference and selection
			// context.
			_fe.emailRangeTo();
			return;
		}
		_fe.flagRangeTo(flagId);
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges() {
		_fe.doneChanges();
		setVisible(false);
		dispose();
	}
}
