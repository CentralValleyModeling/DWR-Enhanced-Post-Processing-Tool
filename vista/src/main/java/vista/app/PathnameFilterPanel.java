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
import java.awt.GridLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import vista.app.commands.GroupPathnameFilterCommand;
import vista.set.Group;
import vista.set.Pathname;

/**
 * Creates a panel with controls for filtering on data references of a group.
 * Most filtering is done on pathnames or parts thereof using regular
 * expressions. One exception to this is the D part of the pathname which
 * represents the time window and has a TimeWindowFilter filter.
 * 
 * @see vista.set.TimeWindowFilter
 * @see vista.set.PathPartPredicate
 * @author Nicky Sandhu
 * @version $Id: PathnameFilterPanel.java,v 1.1 2003/10/02 20:48:37 redwood Exp
 *          $
 */
public class PathnameFilterPanel extends JPanel {
	/**
	 * constructor
	 */
	public PathnameFilterPanel(GroupTable parent) {
		_groupTable = parent;

		JPanel filterPanel = new JPanel();
		filterPanel.setLayout(new GridLayout(1, Pathname.MAX_PARTS + 1));
		// make choice panel
		JPanel choicePanel = new JPanel();
		choicePanel.setLayout(new BorderLayout());
		choicePanel.add(new JLabel("Filter"), BorderLayout.NORTH);
		_filterChoice = new JComboBox();
		_filterChoice.addItem("Select");
		_filterChoice.addItem("Reject");
		_filterChoice.setSelectedItem("Select");
		choicePanel.add(_filterChoice, BorderLayout.SOUTH);

		filterPanel.add(choicePanel);
		// 
		_textFields = new JTextField[Pathname.MAX_PARTS];

		for (int i = 0; i < Pathname.MAX_PARTS; i++) {
			JPanel panel = new JPanel();
			panel.setLayout(new BorderLayout());
			panel.add(new JLabel(Pathname.getPartName(i)), BorderLayout.NORTH);
			_textFields[i] = new JTextField("");
			_textFields[i].addKeyListener(new FilterListener());
			panel.add(_textFields[i], BorderLayout.SOUTH);
			filterPanel.add(panel);
		}

		JPanel pathNameFilterPanel = new JPanel();
		pathNameFilterPanel.setLayout(new GridLayout(1, 2));
		pathNameFilterPanel.add(new JLabel("Pathname Filter : "));
		_pathTextField = new JTextField("");
		_pathTextField.addKeyListener(new FilterListener());
		pathNameFilterPanel.add(_pathTextField);
		setLayout(new BorderLayout());
		add(filterPanel, BorderLayout.SOUTH);
		add(pathNameFilterPanel, BorderLayout.NORTH);
	}

	/**
	 * The table containing the group
	 */
	private GroupTable _groupTable;
	/**
	 * filter choice, selecting/rejecting
	 */
	private JComboBox _filterChoice;
	/**
	 * pathname field
	 */
	private JTextField _pathTextField;
	/**
	 * path part fields
	 */
	private JTextField[] _textFields;

	/**
   *
   */
	private class FilterListener implements KeyListener {
		/**
		 * filters on pressing enter key on any field...
		 */
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() != KeyEvent.VK_ENTER)
				return;
			boolean selecting = true;
			if (_filterChoice.getSelectedItem().equals("Reject"))
				selecting = false;
			String regExp = _pathTextField.getText().trim();
			Group group = _groupTable.getGroup();
			String[] regExps = new String[Pathname.MAX_PARTS];
			for (int i = 0; i < Pathname.MAX_PARTS; i++) {
				regExps[i] = _textFields[i].getText().trim();
				_textFields[i].setText("");
			}
			_pathTextField.setText("");
			_pathTextField.repaint();
			// filtering by pathname
			Executor.execute(new GroupPathnameFilterCommand(group, regExp,
					selecting), _groupTable);
			// filtering by pathname parts
			Executor.execute(new GroupFilterCommand(group, regExps, selecting),
					_groupTable);
		}

		public void keyTyped(KeyEvent evt) {
		}

		public void keyReleased(KeyEvent evt) {
		}
	}// endof FilterListener class
}
