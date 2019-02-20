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
import java.awt.GridLayout;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

/**
 * An editor for the attributes and state of the Curve object
 * 
 * @see Curve
 * @author Nicky Sandhu
 * @version $Id: CurveDialogPanel.java,v 1.1 2003/10/02 20:48:52 redwood Exp $
 */
public class CurveDialogPanel extends GEDialogPanel {
	/**
	 * constructor
	 */
	public CurveDialogPanel(Curve curve) {
		super(curve);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel() {
		JPanel basicPanel = super.createPanel();
		// text editing
		Curve curve = (Curve) getGraphicElement();
		CurveAttr attr = (CurveAttr) curve.getAttributes();

		textField = new JTextField(attr._curveName, 15);
		textField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Curve Name"));
		// 
		thicknessField = new JTextField(new Float(attr._thickness).toString(),
				5);
		thicknessField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "thickness"));
		StringBuffer dashsb = new StringBuffer(10);
		for (int i = 0; i < attr._dashArray.length; i++) {
			dashsb.append(new Float(attr._dashArray[i]).toString());
			dashsb.append(" ");
		}
		dashArrayField = new JTextField(dashsb.toString(), 15);
		dashArrayField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Dash Array"));
		symbolCheck = new JCheckBox("Draw Symbol?", attr._drawSymbol);
		lineCheck = new JCheckBox("Draw Line?", attr._drawLines);
		dataPerSymbolField = new JTextField(new Integer(attr._dataPerSymbol)
				.toString());
		dataPerSymbolField.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createEtchedBorder(), "Data/Symbol"));
		//
		JPanel taPanel = new JPanel();
		taPanel.setLayout(new GridLayout(3, 1));
		taPanel.add(thicknessField);
		taPanel.add(dashArrayField);
		taPanel.add(symbolCheck);
		taPanel.add(lineCheck);
		taPanel.add(dataPerSymbolField);
		taPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Curve Attributes"));
		//
		JPanel curvePanel = new JPanel();
		curvePanel.setLayout(new GridLayout(5, 1));
		curvePanel.add(textField);
		curvePanel.add(taPanel);
		curvePanel.setLayout(new GridLayout(curvePanel.getComponentCount(), 1));
		//
		symbolPanel = new SymbolDialogPanel(curve.getSymbol());
		//  
		JTabbedPane interiorPane = new JTabbedPane();
		interiorPane.addTab(BASIC, null, basicPanel);
		interiorPane.addTab(CURVE, null, curvePanel);
		interiorPane.addTab(SYMBOL, null, symbolPanel);
		interiorPane.setSelectedIndex(0);
		//
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
		symbolPanel.applyChanges();
		Curve curve = (Curve) getGraphicElement();
		CurveAttr attr = (CurveAttr) curve.getAttributes();
		attr._curveName = textField.getText();
		try {
			attr._thickness = new Float(thicknessField.getText()).floatValue();
		} catch (NumberFormatException nfe) {
			System.err.println("Incorrect thickness spec");
		}

		try {
			StringTokenizer st = new StringTokenizer(dashArrayField.getText()
					.trim());
			int count = st.countTokens();
			if (count > 0) {
				float[] array = new float[count];
				int i = 0;
				while (st.hasMoreTokens()) {
					array[i] = new Float(st.nextToken()).floatValue();
					i++;
				}
				attr._dashArray = array;
			}
		} catch (NumberFormatException nfe) {
			System.err.println("Error in dash array");
		}

		int dps = attr._dataPerSymbol;
		try {
			dps = new Integer(dataPerSymbolField.getText()).intValue();
		} catch (NumberFormatException nfe) {
			System.err.println("Incorrect value for Data Per Symbol Field");
		}
		attr._dataPerSymbol = dps;
		attr._drawSymbol = symbolCheck.isSelected();
		attr._drawLines = lineCheck.isSelected();
		if (attr._drawSymbol)
			curve.setSymbol((Symbol) symbolPanel.getGraphicElement());
	}

	/**
   *
   */
	private JTextField textField, dataPerSymbolField, thicknessField,
			dashArrayField;
	private JCheckBox symbolCheck, lineCheck;
	private SymbolDialogPanel symbolPanel;
	/**
 *
 */
	protected final String BASIC = "Basic";
	/**
 *
 */
	protected final String CURVE = "Curve";
	/**
 *
 */
	protected final String SYMBOL = "Symbol";

}
