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
import java.awt.Container;
import java.awt.Frame;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JDialog;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;

/**
 * Initializes a tree dialog for a GEContainer and redraws the containing canvas
 * on apply/done button press.
 * 
 * @author Nicky Sandhu
 * @version $Id: GETreeDialog.java,v 1.1 2003/10/02 20:48:59 redwood Exp $
 */
public class GETreeDialog extends JDialog implements Changeable {
	/**
	 * initializes a tree structure of components contained in a GEContainer
	 * from the given graph frame.
	 */
	public GETreeDialog(Frame frame, GECanvas canvas) {
		super(frame);
		_splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		// make left pane tree
		_gC = canvas;
		GraphicElement ge = _gC.getGraphicElement();
		GETree treePanel = null;
		if (ge instanceof GEContainer) {
			treePanel = new GETree((GEContainer) ge);
			tree = treePanel.getTree();
			_splitPane.setLeftComponent(treePanel);
			setCurrentPanel(ge.createDialogPanel());
		}
		// main layout
		Container cPane = getContentPane();
		cPane.setLayout(new BorderLayout());
		cPane.add(_splitPane, BorderLayout.CENTER);
		cPane.add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		// add listeners & open frame @ location
		addMouseListenerToTree();
		pack();
		Rectangle r = frame.getBounds();
		setLocation(r.x - getBounds().width, r.y);
		_splitPane.setDividerLocation(0.35);
		show();
	}

	/**
    *
    */
	public void setCurrentPanel(GEDialogPanel panel) {
		_currentPanel = panel;
		_splitPane.setRightComponent(_currentPanel);
		_splitPane.setDividerLocation(0.35);
	}

	/**
	 * apply changes done
	 */
	public void applyChanges() {
		_currentPanel.applyChanges();
		_gC.redoNextPaint();
		_gC.update(_gC.getGraphics());
	}

	/**
	 * done with changes, dispose of dialog
	 */
	public void doneChanges() {
		dispose();
	}

	/**
	 * path single clicked
	 */
	public void pathSingleClicked(int selRow, TreePath selPath) {
		pathSelected(selPath);
	}

	/**
   *
   */
	public void pathSelected(TreePath selPath) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) selPath
				.getLastPathComponent();
		GraphicElement ge = (GraphicElement) node.getUserObject();
		setCurrentPanel(ge.createDialogPanel());
	}

	/**
	 * path double clicked
	 */
	public void pathDoubleClicked(int selRow, TreePath selPath) {
		pathSelected(selPath);
	}

	/**
   *
   */
	private void addMouseListenerToTree() {
		MouseListener ml = new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				int selRow = tree.getRowForLocation(e.getX(), e.getY());
				TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
				if (selRow != -1) {
					if (e.getClickCount() == 1) {
						pathSingleClicked(selRow, selPath);
					} else if (e.getClickCount() == 2) {
						pathDoubleClicked(selRow, selPath);
					}
				}
			}
		};
		tree.addMouseListener(ml);
	}

	/**
    *
    */
	public static final boolean DEBUG = false;
	private GECanvas _gC;
	private JTree tree;
	private JSplitPane _splitPane;
	private GEDialogPanel _currentPanel;

	/**
    *
    *
    */
	private class GETreeListener implements TreeSelectionListener {
		public void valueChanged(TreeSelectionEvent evt) {
			pathSelected(evt.getPath());
		}
	}
}
