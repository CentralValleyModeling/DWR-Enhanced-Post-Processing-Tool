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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * Displays a label and a small rectangle of given color. Upon double clicking
 * the rectangle of color a dialog box is displayed and color can be chosen from
 * that.
 * 
 * @author Nicky Sandhu
 * @version $Id: ColorChoice.java,v 1.1 2003/10/02 20:48:50 redwood Exp $
 */
public class ColorChoice extends JPanel {
	/**
	 * constructor displaying the label along with a square of the prescribed
	 * color
	 */
	public ColorChoice(String labelName, Color iColor) {
		_labelName = labelName;
		//
		_colorDisplay = new ColorDisplayPanel(iColor);
		_colorDisplay.addMouseListener(new DisplayMouseListener());
		_colorDisplay.setBorder(BorderFactory.createRaisedBevelBorder());
		_transColor = new JCheckBox("Transparent?", (iColor == null));
		ChangeListener cl = new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (_transColor.isSelected())
					_colorDisplay.setColor(null);
				else
					_colorDisplay.setColor(Color.white);
				_colorDisplay.invalidate();
				_colorDisplay.paint(_colorDisplay.getGraphics());
			}
		};
		_transColor.addChangeListener(cl);
		//
		JPanel cpane = this;
		cpane.setLayout(new java.awt.BorderLayout());
		cpane.add(new JLabel(_labelName), BorderLayout.WEST);
		cpane.add(_colorDisplay, BorderLayout.CENTER);
		cpane.add(_transColor, BorderLayout.EAST);
		//
	}

	/**
	 * returns the current color
	 */
	public Color getColor() {
		return _colorDisplay.getColor();
	}

	/**
	 * show the dialog
	 */
	void showChooserDialog() {
		Color newColor = JColorChooser.showDialog(this, _labelName,
				_colorDisplay.getColor());
		if (newColor != null) {
			_colorDisplay.setColor(newColor);
			_colorDisplay.update(_colorDisplay.getGraphics());
		}
	}

	/**
	 * The attribute to which this color is assigned
	 */
	private String _labelName;
	/**
	 * a panel for displaying chosen color
	 */
	private ColorDisplayPanel _colorDisplay;
	/**
   *
   */
	private JCheckBox _transColor;

	/**
	 * A display panel for the color
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: ColorChoice.java,v 1.1 2003/10/02 20:48:50 redwood Exp $
	 */
	private class ColorDisplayPanel extends JPanel {
		/**
		 * sets color while initializing.
		 */
		public ColorDisplayPanel(Color c) {
			setColor(c);
		}

		/**
		 * gets the preferred size of this component
		 */
		public Dimension getPreferredSize() {
			return new Dimension(30, 30);
		}

		/**
		 * returns the minimum dimension of this compnoent
		 */
		public Dimension getMinimumSize() {
			return getPreferredSize();
		}

		/**
		 * paints this component
		 */
		public void paint(Graphics g) {
			if (_color != null) {
				java.awt.Rectangle r = this.getBounds();
				g.setColor(_color);
				g.fillRect(2, 2, r.width - 2, r.height - 2);
			} else {
				java.awt.Rectangle r = this.getBounds();
				g.setColor(getBackground());
				g.fillRect(0, 0, r.width, r.height);
				g.setColor(Color.black);
				g.drawRect(1, 1, r.width - 1, r.height - 1);
			}
		}

		/**
		 * sets the color
		 */
		public void setColor(Color c) {
			_color = c;
		}

		/**
		 * gets the color
		 */
		public Color getColor() {
			return _color;
		}

		/**
		 * initial color
		 */
		protected Color _color;
	}

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: ColorChoice.java,v 1.1 2003/10/02 20:48:50 redwood Exp $
	 */
	public class DisplayMouseListener implements MouseListener {
		public void mouseClicked(MouseEvent e) {
			int count = e.getClickCount();
			if (count >= 2) {
				if (ColorChoice.this._colorDisplay.getColor() != null)
					ColorChoice.this.showChooserDialog();
			}
		}

		public void mousePressed(MouseEvent e) {
		}

		public void mouseReleased(MouseEvent e) {
		}

		public void mouseEntered(MouseEvent e) {
		}

		public void mouseExited(MouseEvent e) {
		}
	}
}
