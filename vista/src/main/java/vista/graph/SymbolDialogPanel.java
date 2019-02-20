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

import java.awt.GridLayout;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * An editor for the attributes and state of the Curve object
 * 
 * @see Curve
 * @author Nicky Sandhu
 * @version $Id: SymbolDialogPanel.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 */
public class SymbolDialogPanel extends GEDialogPanel {
	/**
	 * constructor
	 */
	public SymbolDialogPanel(Symbol symbol) {
		super(symbol);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel() {
		JPanel basicPanel = super.createPanel();
		// text editing
		Symbol symbol = (Symbol) getGraphicElement();
		SymbolAttr attr = (SymbolAttr) symbol.getAttributes();
		//
		symbolType = new JComboBox();
		symbolType.addItem(TRIANGLE);
		symbolType.addItem(SQUARE);
		symbolType.addItem(CROSS);
		symbolType.addItem(SLASH);
		symbolType.addItem(X);
		symbolType.addItem(BUTTERFLY);
		symbolType.addItem(HOURGLASS);
		//
		symbolFill = new JCheckBox("Is Filled?", attr._isFilled);
		symbolSizeField = new JTextField("2");
		//
		JPanel taPanel = new JPanel();
		taPanel.setLayout(new GridLayout(3, 1));
		taPanel.add(symbolType);
		taPanel.add(symbolFill);
		taPanel.add(symbolSizeField);
		taPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Symbol Attributes"));
		//
		basicPanel.add(taPanel);
		basicPanel.setLayout(new GridLayout(basicPanel.getComponentCount(), 1));
		//
		return basicPanel;
	}

	/**
	 * apply changes for both the basic graphic element and its specialization
	 */
	public void applyChanges() {
		super.applyChanges();
		Symbol symbol = (Symbol) getGraphicElement();
		SymbolAttr attr = (SymbolAttr) symbol.getAttributes();
		attr._isFilled = symbolFill.isSelected();
		int size = 2;
		try {
			size = new Integer(symbolSizeField.getText()).intValue();
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(this, e);
		}

		if (symbolType.getSelectedItem().equals(TRIANGLE)) {
			attr.setSymbol(SymbolFactory.createTriangleShape(size));
		} else if (symbolType.getSelectedItem().equals(SQUARE)) {
			attr.setSymbol(SymbolFactory.createSquareShape(size));
		} else if (symbolType.getSelectedItem().equals(CROSS)) {
			attr.setSymbol(SymbolFactory.createCrossShape(size));
		} else if (symbolType.getSelectedItem().equals(SLASH)) {
			attr.setSymbol(SymbolFactory.createSlashShape(size));
		} else if (symbolType.getSelectedItem().equals(X)) {
			attr.setSymbol(SymbolFactory.createXShape(size));
		} else if (symbolType.getSelectedItem().equals(BUTTERFLY)) {
			attr.setSymbol(SymbolFactory.createButterflyShape(size));
		} else if (symbolType.getSelectedItem().equals(HOURGLASS)) {
			attr.setSymbol(SymbolFactory.createHourGlassShape(size));
		} else {
			attr.setSymbol(SymbolFactory.createSquareShape(size));
		}
	}

	/**
    *
    */
	private JTextField textField, symbolSizeField;
	private JCheckBox symbolFill;
	private JComboBox symbolType;
	/**
 *
 */
	private final String BASIC = "Basic";
	/**
 *
 */
	private final String SYMBOL = "Symbol";
	private final String TRIANGLE = "Triangle", SQUARE = "Square",
			CROSS = "Cross", SLASH = "Slash", X = "X", BUTTERFLY = "ButterFly",
			HOURGLASS = "Hour Glass";
}
