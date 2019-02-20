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
import java.awt.FileDialog;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;
import vista.gui.VistaUtils;
import vista.set.ListDirectedPredicate;

/**
 * A modal dialog to establish connection with server
 * 
 * @author Nicky Sandhu
 * @version $Id: SortDialog.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
 */
public class SortDialog extends JDialog implements Changeable, RowMovable {
	/**
	 * sets up a blocking dialog displaying server and directory names
	 */
	public SortDialog(JFrame parent, GroupTable table) {
		super(parent, true);
		_table = table;
		//
		JMenuBar mbar = new JMenuBar();
		JMenu menu1 = new JMenu("List");
		JMenuItem loadFileMenu = new JMenuItem("Load from file...");
		loadFileMenu.addActionListener(new LoadFileListener());
		JMenuItem removeMenu = new JMenuItem("Remove Selected");
		removeMenu.addActionListener(new RemoveFileListener());
		menu1.add(loadFileMenu);
		menu1.add(removeMenu);
		mbar.add(menu1);
		setJMenuBar(mbar);
		//
		_listV = new Vector();
		_sortList = new JList(_listV);
		// set up connection panel
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(new JScrollPane(_sortList), BorderLayout.CENTER);
		// add main panel
		super.getContentPane().setLayout(new BorderLayout());
		super.getContentPane().add(mainPanel, BorderLayout.CENTER);
		super.getContentPane().add(new DialogButtonPanel(this),
				BorderLayout.SOUTH);
		setLocationRelativeTo(_table);
		addMouseListener(new RowMoveListener(this));
		pack();
	}

	/**
	 * adds str to end of list
	 */
	void addToSortList(String str) {
		_listV.addElement(str);
		_sortList.setListData(_listV);
		_sortList.repaint();
	}

	/**
   *
   */
	void removeSelected() {
		Object[] obj = _sortList.getSelectedValues();
		if (obj == null)
			return;
		for (int i = 0; i < obj.length; i++) {
			_listV.removeElement(obj[i]);
		}
		_sortList.setListData(_listV);
		_sortList.repaint();
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges() {
		String[] list = new String[_listV.size()];
		_listV.copyInto(list);
		if (list != null && list.length > 1){
			throw new RuntimeException("If this functionality is needed. it needs to be fixed first!");
		}
		//FIXME: 
		ListDirectedPredicate ldp = new ListDirectedPredicate(list[0]);
		_table.getGroup().sortBy(ldp);
		_table.repaint();
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges() {
		this.dispose();
	}

	/**
	 * returns the row number at point p
	 */
	public int rowAtPoint(Point p) {
		return _sortList.locationToIndex(p);
	}

	/**
	 * moves row at oldPosition to newPosition
	 */
	public void moveRow(int oldPosition, int newPosition) {
		Object obj = _listV.elementAt(oldPosition);
		_listV.removeElement(obj);
		_listV.insertElementAt(obj, newPosition);
		_sortList.setListData(_listV);
		_sortList.repaint();
	}

	/**
   *
   */
	private static final boolean DEBUG = false;
	private JList _sortList;
	private GroupTable _table;
	private Vector _listV;

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: SortDialog.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
	 */
	public class LoadFileListener implements ActionListener {
		public void actionPerformed(ActionEvent evt) {
			String filename = VistaUtils.getFilenameFromDialog(SortDialog.this,
					FileDialog.LOAD, ".sort", "Sorting Order List");
			if (filename == null)
				return;
			SortDialog.this.addToSortList(filename);
		}
	} // end of load file listener

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: SortDialog.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
	 */
	public class RemoveFileListener implements ActionListener {
		public void actionPerformed(ActionEvent evt) {
			removeSelected();
		}
	} // end of load file listener
}
