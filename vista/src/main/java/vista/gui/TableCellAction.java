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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JTable;
import javax.swing.table.TableColumnModel;

/**
 * A class that listens to mouse clicks on a table's header and forwards them to
 * an action
 * 
 * @author Nicky Sandhu
 * @version $Id: TableCellAction.java,v 1.2 2000/03/21 18:16:30 nsandhu Exp $
 */
public abstract class TableCellAction extends MouseAdapter implements
		ActionListener {
	/**
    *
    */
	private JTable _table;
	private int _column, _row;
	private MouseEvent _me;
	private int _met;

	/**
	 * an action to be taken when a cell in the table is clicked
	 */
	public TableCellAction(JTable table) {
		this(table, MouseEvent.MOUSE_CLICKED);
	}

	/**
	 * an action to be taken when a cell in the table gets the desired type of
	 * mouse event
	 */
	public TableCellAction(JTable table, int mouseEventType) {
		_table = table;
		_table.addMouseListener(this);
		_column = -1;
		_row = -1;
		_met = mouseEventType;
	}

	/**
    *
    */
	private void setColumnAndRow(MouseEvent e) {
		TableColumnModel columnModel = _table.getColumnModel();
		int viewColumn = columnModel.getColumnIndexAtX(e.getX());
		_column = _table.convertColumnIndexToModel(viewColumn);
		_row = _table.rowAtPoint(e.getPoint());
		_me = e;
	}

	/**
    *
    */
	public void mouseClicked(MouseEvent e) {
		if (_met != MouseEvent.MOUSE_CLICKED)
			return;
		setColumnAndRow(e);
		if (_row == -1)
			return;
		if (e.getClickCount() > 1 && _column != -1) {
			// take action here...
			actionPerformed(new ActionEvent(e.getSource(),
					ActionEvent.ACTION_PERFORMED, "table cell clicked"));
		}
	}

	/**
    *
    */
	public void mouseMoved(MouseEvent e) {
		if (_met != MouseEvent.MOUSE_MOVED)
			return;
		setColumnAndRow(e);
		if (_row == -1)
			return;
		if (e.getClickCount() == 1 && _column != -1) {
			// take action here...
			actionPerformed(new ActionEvent(e.getSource(),
					ActionEvent.ACTION_PERFORMED, "table cell movement"));
		}
	}

	/**
    *
    */
	public int getColumn() {
		return _column;
	}

	/**
    *
    */
	public int getRow() {
		return _row;
	}

	/**
    *
    */
	public MouseEvent getMouseEvent() {
		return _me;
	}

	/**
    *
    */
	public abstract void actionPerformed(ActionEvent evt);
}
