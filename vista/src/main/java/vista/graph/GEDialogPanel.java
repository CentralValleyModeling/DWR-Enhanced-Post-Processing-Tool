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
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Label;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.border.Border;

import vista.gui.Changeable;

/**
 * A dialog class for handling attribute change requests.
 */
public class GEDialogPanel extends JPanel implements Changeable {
	/**
	 * The constructor for initializing class variables and creating panel.
	 */
	public GEDialogPanel(GraphicElement ge) {
		_ge = ge;
		_attr = _ge.getAttributes();
		setLayout(new BorderLayout());
		add(createPanel(), BorderLayout.CENTER);
	}

	/**
	 * get the graphic element
	 */
	public GraphicElement getGraphicElement() {
		return _ge;
	}

	/**
	 * creates Panel with controls to control each attribute
	 */
	protected JPanel createPanel() {
		JPanel mainPanel;
		mainPanel = new JPanel();
		mainPanel.setLayout(new GridLayout(5, 1));
		Border eBorder = BorderFactory.createRaisedBevelBorder();
		Border border = BorderFactory.createTitledBorder(eBorder, _ge
				.toString());
		mainPanel.setBorder(border);
		bgChooser = new ColorChoice(Labels.BACKGROUND_COLOR,
				_attr._backgroundColor);
		fgChooser = new ColorChoice(Labels.FOREGROUND_COLOR,
				_attr._foregroundColor);
		// orientation
		JPanel oPanel = new JPanel();
		oPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		oPanel.add(new Label(Labels.ORIENTATION));
		orientationChoice = new JComboBox();
		orientationChoice.addItem(Labels.HORIZONTAL);
		orientationChoice.addItem(Labels.VERTICAL);
		if (_attr._orientation == GEAttr.HORIZONTAL)
			orientationChoice.setSelectedItem(Labels.HORIZONTAL);
		else
			orientationChoice.setSelectedItem(Labels.VERTICAL);
		oPanel.add(orientationChoice);

		visibilityCheckBox = new JCheckBox(Labels.IS_VISIBLE, _attr._isVisible);
		visibilityCheckBox.setHorizontalAlignment(JCheckBox.LEFT);
		visibilityCheckBox.setHorizontalTextPosition(JCheckBox.LEFT);

		clipCheckBox = new JCheckBox(Labels.CLIP_WITHIN_BOUNDS,
				_attr._clipWithinBounds);
		clipCheckBox.setHorizontalAlignment(JCheckBox.LEFT);
		clipCheckBox.setHorizontalTextPosition(JCheckBox.LEFT);

		insetsPanel = new InsetsEditPanel(_ge.getInsets());
		//
		JPanel attrPanel = new JPanel();
		attrPanel.setLayout(new GridLayout(3, 1));
		attrPanel.add(oPanel);
		attrPanel.add(visibilityCheckBox);
		attrPanel.add(clipCheckBox);
		attrPanel.setBorder(BorderFactory.createLineBorder(Color.black));
		//
		JPanel colorPanel = new JPanel();
		colorPanel.setLayout(new GridLayout(2, 1));
		colorPanel.add(bgChooser);
		colorPanel.add(fgChooser);
		colorPanel.setBorder(BorderFactory.createLineBorder(Color.black));
		//
		mainPanel.add(colorPanel);
		mainPanel.add(attrPanel);
		mainPanel.add(insetsPanel);
		mainPanel.setLayout(new GridLayout(mainPanel.getComponentCount(), 1));

		return mainPanel;
	}

	private ColorChoice bgChooser, fgChooser;
	private JCheckBox visibilityCheckBox, clipCheckBox;
	private JComboBox orientationChoice;
	private InsetsEditPanel insetsPanel;

	/**
	 * applies the changes to the attribute object and/or the graphic element
	 */
	public void applyChanges() {
		_attr._backgroundColor = bgChooser.getColor();
		_attr._foregroundColor = fgChooser.getColor();
		//
		if (orientationChoice.getSelectedItem().equals(Labels.HORIZONTAL))
			_attr._orientation = GEAttr.HORIZONTAL;
		else
			_attr._orientation = GEAttr.VERTICAL;
		//
		_attr._isVisible = visibilityCheckBox.isSelected();
		_attr._clipWithinBounds = clipCheckBox.isSelected();
		_ge.setInsets(insetsPanel.getInsets());
	}

	/**
   *
   */
	public void doneChanges() {
	}

	/**
	 * The graphic element whose attributes are to be displayed
	 */
	private GraphicElement _ge;
	/**
	 * The attributes of the graphic element.
	 */
	private GEAttr _attr;
}
