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
package vista.graph;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Insets;

import javax.swing.BorderFactory;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

/**
 * A panel to edit insets. If invalid value is entered in the text field a
 * message is displayed with the error and previous values are used.
 * 
 * @author Nicky Sandhu
 * @version $Id: InsetsEditPanel.java,v 1.1 2003/10/02 20:49:02 redwood Exp $
 */
public class InsetsEditPanel extends JPanel {
	/**
	 * Create a edit panel with given insets
	 */
	public InsetsEditPanel(Insets i) {
		//
		top = i.top;
		left = i.left;
		bottom = i.bottom;
		right = i.right;
		//
		topField = new JTextField(new Integer(top).toString(), 6);
		topField.setBorder(BorderFactory.createTitledBorder("Top"));
		leftField = new JTextField(new Integer(left).toString(), 6);
		leftField.setBorder(BorderFactory.createTitledBorder("Left"));
		bottomField = new JTextField(new Integer(bottom).toString(), 6);
		bottomField.setBorder(BorderFactory.createTitledBorder("Bottom"));
		rightField = new JTextField(new Integer(right).toString(), 6);
		rightField.setBorder(BorderFactory.createTitledBorder("Right"));
		//
		JPanel iPanel = new JPanel();
		iPanel.setLayout(new FlowLayout());
		iPanel.add(topField);
		iPanel.add(leftField);
		iPanel.add(bottomField);
		iPanel.add(rightField);
		//
		JPanel mainPanel = this;
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(iPanel, BorderLayout.CENTER);
		//
		Border eBorder = BorderFactory.createEtchedBorder();
		Border mainBorder = BorderFactory.createTitledBorder(eBorder, "Insets");
		iPanel.setBorder(mainBorder);
	}

	/**
	 * return an insets object with the set values if possible
	 */
	public Insets getInsets() {
		try {
			top = new Integer(topField.getText()).intValue();
			left = new Integer(leftField.getText()).intValue();
			bottom = new Integer(bottomField.getText()).intValue();
			right = new Integer(rightField.getText()).intValue();
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(this, e);
		}
		return new Insets(top, left, bottom, right);
	}

	/**
	 * inset values
	 */
	private int top, left, bottom, right;
	/**
	 * inset fields
	 */
	private JTextField topField, leftField, bottomField, rightField;
}
