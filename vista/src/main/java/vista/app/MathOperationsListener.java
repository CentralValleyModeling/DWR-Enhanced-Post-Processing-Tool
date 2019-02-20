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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import vista.set.DataReference;
import vista.set.DataReferenceMath;

/**
 * Listens for math operations on table and performs them.
 * 
 * @author Nicky Sandhu
 * @version $Id: MathOperationsListener.java,v 1.1 2003/10/02 20:48:34 redwood
 *          Exp $
 */
class MathOperationsListener implements ListSelectionListener {
	/**
	 * adds a key listener for +,-,/,*,= for frame containing group table
	 */
	public MathOperationsListener(GroupTable table) {
		_table = table;
		JComponent comp = table.getTable();
		// add equals listener
		ActionListener l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				equalsKeyPressed(evt);
			}
		};
		KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_EQUALS,
				InputEvent.CTRL_MASK, true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add + listener
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				addKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_ADD, InputEvent.CTRL_MASK, true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add - listener
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				subtractKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_SUBTRACT, InputEvent.CTRL_MASK,
				true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add / listener
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				divideKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_DIVIDE, InputEvent.CTRL_MASK,
				true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
		// add multiply key
		l = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				multiplyKeyPressed(evt);
			}
		};
		k = KeyStroke.getKeyStroke(KeyEvent.VK_MULTIPLY, InputEvent.CTRL_MASK,
				true);
		comp.registerKeyboardAction(l, k, JComponent.WHEN_IN_FOCUSED_WINDOW);
	}

	/**
   *
   */
	public void actionPerformed(ActionEvent evt) {
	}

	/**
   *
   */
	public void valueChanged(ListSelectionEvent evt) {
	}

	/**
   *
   */
	public void equalsKeyPressed(ActionEvent evt) {
		System.out.println("Equals key pressed");
		_ref2 = (DataReference) _table.getSelectedValue();
		System.out.println("Selected " + _ref2 + " for math operation");
		try {
			if (_addKeyMode) {
				DataReference ref = DataReferenceMath.vectorOperation(_ref1,
						_ref2, DataReferenceMath.ADD);
				System.out.println("done creating new reference");
				_table.getGroup().addDataReference(ref);
				_addKeyMode = false;
			}
			if (_table.isVisible()) {
				_table.paintAll(_table.getGraphics());
			}
		} catch (IllegalArgumentException iae) {
			iae.printStackTrace();
		}
		_selectionMode = false;
	}

	/**
   *
   */
	public void addKeyPressed(ActionEvent evt) {
		System.out.println("Add key pressed");
		if (!_selectionMode) {
			_addKeyMode = true;
			_ref1 = (DataReference) _table.getSelectedValue();
			_selectionMode = true;
		}
		System.out.println("Selected " + _ref1 + " for math operation");
	}

	/**
   *
   */
	private GroupTable _table;
	/**
   *
   */
	private boolean _addKeyMode = false;
	/**
   *
   */
	private DataReference _ref1, _ref2;
	/**
   *
   */
	private boolean _selectionMode = false;

	/**
   *
   */
	public void subtractKeyPressed(ActionEvent evt) {
	}

	/**
   *
   */
	public void multiplyKeyPressed(ActionEvent evt) {
	}

	/**
   *
   */
	public void divideKeyPressed(ActionEvent evt) {
	}
}
