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
package vista.gui;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

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
