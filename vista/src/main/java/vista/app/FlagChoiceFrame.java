/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

 */
package vista.app;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.GridLayout;

import javax.swing.ButtonGroup;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

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
