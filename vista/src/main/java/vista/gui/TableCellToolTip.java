/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
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

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.TableColumnModel;

/**
 * This class displays a window when the mouse is positioned on the cell of a
 * JTable
 */
public class TableCellToolTip implements MouseMotionListener, MouseListener {
	private JTable _table;
	private Window _window;
	private JLabel _label;

	public TableCellToolTip(JTable table) {
		_table = table;
		_table.addMouseMotionListener(this);
		_table.addMouseListener(this);
		_window = new Window(VistaUtils.getFrameForComponent(_table));
		// create a jlabel and add it to jwindow
		_label = new JLabel();
		_window.add(_label);
	}

	public void mouseDragged(MouseEvent evt) {
		// do nothing
		_window.setVisible(false);
	}

	// show window placed exactly on cell
	public void mouseMoved(MouseEvent evt) {
		if (_window.isVisible())
			return;
		// get cell under this position
		TableColumnModel columnModel = _table.getColumnModel();
		int viewColumn = columnModel.getColumnIndexAtX(evt.getX());
		int column = _table.convertColumnIndexToModel(viewColumn);
		int row = _table.rowAtPoint(evt.getPoint());
		// set the text to be displayed
		_label.setText(_table.getValueAt(row, column).toString());
		// make the window of the cell's dimensions
		Rectangle rect = _table.getCellRect(row, column, true);
		_window.setSize(Math.max(rect.width, _label.getPreferredSize().width),
				rect.height);
		// place it right over the cell
		Point loc = _table.getLocationOnScreen();
		_window.setLocation(loc.x + rect.x, loc.y + rect.y);
		// show the window
		_window.setVisible(true);
	}

	//
	public void mouseClicked(MouseEvent evt) {
		_window.setVisible(false);
	}

	public void mouseEntered(MouseEvent evt) {
		_window.setVisible(true);
	}

	public void mouseExited(MouseEvent evt) {
		_window.setVisible(false);
	}

	public void mousePressed(MouseEvent evt) {
		_window.setVisible(false);
	}

	public void mouseReleased(MouseEvent evt) {
		_window.setVisible(false);
	}
}
