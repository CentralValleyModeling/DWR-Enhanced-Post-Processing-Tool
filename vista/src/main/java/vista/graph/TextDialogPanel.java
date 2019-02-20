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
import java.awt.GridLayout;

import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

/**
 * An editor for the attributes and state of the TextLine object
 * 
 * @see TextLine
 * @author Nicky Sandhu
 * @version $Id: TextDialogPanel.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 */
public class TextDialogPanel extends GEDialogPanel {
	/**
	 * constructor
	 */
	public TextDialogPanel(TextLine textLine) {
		super(textLine);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel() {
		JPanel basicPanel = super.createPanel();
		// text editing
		TextLine tL = (TextLine) getGraphicElement();
		textField = new JTextField(tL.getText(), 15);
		textField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), Labels.TEXT_STRING));
		// FontChooser required... wait for swing set!
		fontPanel = new FontChoice(tL.getFont());
		fontPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), Labels.FONT));
		TextLineAttr attr = (TextLineAttr) tL.getAttributes();
		//  
		justificationChoice = new JComboBox();
		justificationChoice.addItem(CENTER);
		justificationChoice.addItem(LEFT);
		justificationChoice.addItem(RIGHT);
		JPanel justPanel = new JPanel();
		justPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		justPanel.add(new JLabel(Labels.JUSTIFICATION));
		justPanel.add(justificationChoice);
		switch (attr._justification) {
		case TextLineAttr.CENTER:
			justificationChoice.setSelectedItem(CENTER);
			break;
		case TextLineAttr.LEFT:
			justificationChoice.setSelectedItem(LEFT);
			break;
		case TextLineAttr.RIGHT:
			justificationChoice.setSelectedItem(RIGHT);
			break;
		default:
			justificationChoice.setSelectedItem(LEFT);
		}
		//
		arrangementChoice = new JComboBox();
		arrangementChoice.addItem(SIDE_BY_SIDE);
		arrangementChoice.addItem(TOP_ON_TOP);
		JPanel arrangementPanel = new JPanel();
		arrangementPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		arrangementPanel.add(new JLabel(Labels.ARRANGEMENT));
		arrangementPanel.add(arrangementChoice);
		switch (attr._textArrangement) {
		case TextLineAttr.SIDE_BY_SIDE:
			arrangementChoice.setSelectedItem(SIDE_BY_SIDE);
			break;
		case TextLineAttr.TOP_ON_TOP:
			arrangementChoice.setSelectedItem(TOP_ON_TOP);
			break;
		default:
			arrangementChoice.setSelectedItem(SIDE_BY_SIDE);
		}
		//
		JPanel taPanel = new JPanel();
		taPanel.setLayout(new GridLayout(2, 1));
		taPanel.add(justPanel);
		taPanel.add(arrangementPanel);
		taPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Text Attributes"));
		//
		JPanel textPanel = new JPanel();
		textPanel.setLayout(new GridLayout(5, 1));
		textPanel.add(textField);
		textPanel.add(fontPanel);
		textPanel.add(taPanel);
		textPanel.setLayout(new GridLayout(textPanel.getComponentCount(), 1));
		//  
		JTabbedPane interiorPane = new JTabbedPane();
		interiorPane.addTab(BASIC, null, basicPanel);
		interiorPane.addTab(TEXT, null, textPanel);
		interiorPane.setSelectedIndex(0);

		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add("Center", interiorPane);

		return mainPanel;
	}

	/**
	 * apply changes for both the basic graphic element and its specialization
	 */
	public void applyChanges() {
		super.applyChanges();
		TextLine tL = (TextLine) getGraphicElement();
		TextLineAttr attr = (TextLineAttr) tL.getAttributes();
		tL.setText(textField.getText());
		attr._font = fontPanel.getSelectedFont();
		attr._originalFontSize = attr._font.getSize();
		if (justificationChoice.getSelectedItem().equals(CENTER)) {
			attr._justification = TextLineAttr.CENTER;
		} else if (justificationChoice.getSelectedItem().equals(LEFT)) {
			attr._justification = TextLineAttr.LEFT;
		} else {
			attr._justification = TextLineAttr.RIGHT;
		}
		if (arrangementChoice.getSelectedItem().equals(SIDE_BY_SIDE)) {
			attr._textArrangement = TextLineAttr.SIDE_BY_SIDE;
		} else {
			attr._textArrangement = TextLineAttr.TOP_ON_TOP;
		}
	}

	/**
   *
   */
	private JComboBox justificationChoice, arrangementChoice;
	private FontChoice fontPanel;
	private JTextField textField;
	/**
 *
 */
	protected final String BASIC = "Basic";
	/**
 *
 */
	protected final String TEXT = "Text";
	/**
 *
 */
	protected final String CENTER = "Center";
	/**
 *
 */
	protected final String LEFT = "Left";
	/**
 *
 */
	protected final String RIGHT = "Right";
	/**
 *
 */
	protected final String SIDE_BY_SIDE = "Side by Side";
	/**
 *
 */
	protected final String TOP_ON_TOP = "Top on Top";

}
