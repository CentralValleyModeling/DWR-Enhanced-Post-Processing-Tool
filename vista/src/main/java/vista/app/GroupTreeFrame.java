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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.JViewport;

import vista.set.Group;
import vista.set.Pathname;

/**
 * A default quitable frame.
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupTreeFrame.java,v 1.1 2003/10/02 20:48:32 redwood Exp $
 */
public class GroupTreeFrame extends JFrame {
	/**
	 * adds the given component to the center of the frame.
	 */
	public GroupTreeFrame(Group g) {
		_groupTree = new GroupTree(g);
		JTree tree = new JTree(_groupTree.getRoot());
		getContentPane().setLayout(new BorderLayout());

		_treePane = new JPanel(true);
		_treePane.setLayout(new BorderLayout());
		_treePane.add("Center", tree);
		JViewport port = new JViewport();
		port.add(_treePane);

		JScrollPane scrollpane = new JScrollPane();
		scrollpane.setViewport(port);

		WindowListener l = new DefaultWindowListener();
		addWindowListener(l);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		JButton forwardButton = new JButton(FFWD);
		buttonPanel.add(forwardButton);
		JButton backwardButton = new JButton(RRWD);
		buttonPanel.add(backwardButton);

		ActionListener orderListener = new OrderByListener();
		forwardButton.addActionListener(orderListener);
		backwardButton.addActionListener(orderListener);

		getContentPane().add(scrollpane);
		_label = new JLabel(getPartOrderString());
		getContentPane().add("North", _label);
		getContentPane().add("South", buttonPanel);

		pack();
		setVisible(true);
	}

	/**
   *
   */
	private String getPartOrderString() {
		StringBuffer buf = new StringBuffer(_partOrder.length * 8);
		buf.append("Pathname Part Order: ");
		for (int i = 0; i < _partOrder.length; i++) {
			buf.append(" ").append(Pathname.getPartName(_partOrder[i]));
		}
		return buf.toString();
	}

	/**
	 * sets tree
	 */
	private void circleForward() {
		int tmp = _partOrder[0];
		for (int i = 1; i < _partOrder.length; i++)
			_partOrder[i - 1] = _partOrder[i];
		_partOrder[_partOrder.length - 1] = tmp;
		_groupTree.setPartAdditionOrder(_partOrder);

		JTree tree = new JTree(_groupTree.getRoot());
		_treePane.removeAll();
		_treePane.add(tree);
		_label.setText(getPartOrderString());
		paintAll(getGraphics());
	}

	/**
	 * sets tree
	 */
	private void circleBackward() {
		int tmp = _partOrder[_partOrder.length - 1];
		for (int i = _partOrder.length - 2; i >= 0; i--)
			_partOrder[i + 1] = _partOrder[i];
		_partOrder[0] = tmp;

		_groupTree.setPartAdditionOrder(_partOrder);

		JTree tree = new JTree(_groupTree.getRoot());
		_treePane.removeAll();
		_treePane.add(tree);
		_label.setText(getPartOrderString());
		paintAll(getGraphics());
	}

	/**
   *
   */
	private GroupTree _groupTree;
	/**
   *
   */
	private JPanel _treePane;
	/**
   *
   */
	private int[] _partOrder = { Pathname.A_PART, Pathname.C_PART,
			Pathname.B_PART, Pathname.D_PART, Pathname.E_PART, Pathname.F_PART };

	/**
   *
   */
	private class DefaultWindowListener extends WindowAdapter {
		/**
   *
   */
		public final void windowClosing(WindowEvent e) {
			setVisible(false);
			dispose();
		}
	} // end of DefaultWindowListener

	/**
   *
   */
	private class OrderByListener implements ActionListener {
		/**
   *
   */
		public void actionPerformed(ActionEvent evt) {
			Object obj = evt.getSource();
			if (obj instanceof JButton) {
				String label = ((JButton) obj).getText();
				if (label.equals(FFWD)) {
					circleForward();
				} else if (label.equals(RRWD)) {
					circleBackward();
				}
			}
		}
	}

	/**
   *
   */
	private final static String FFWD = ">>";
	/**
   *
   */
	private final static String RRWD = "<<";
	/**
   *
   */
	private JLabel _label;
	/**
   *
   */
	final static String A_PART = "A PART";
	/**
   *
   */
	final static String B_PART = "B PART";
	/**
   *
   */
	final static String C_PART = "C PART";
	/**
   *
   */
	final static String D_PART = "D PART";
	/**
   *
   */
	final static String E_PART = "E PART";
	/**
   *
   */
	final static String F_PART = "F PART";
	/**
   *
   */
	private static final String ORDER_BY_ = "Order by ";
}
