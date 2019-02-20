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
import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;

import javax.swing.JDialog;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;

/**
 * A dialog class for handling attribute change requests.
 */
public class GEDialog extends JDialog implements Changeable {
	/**
	 * constructor
	 */
	public GEDialog(Frame parent, GEDialogPanel panel) {
		super(parent, "GE Dialog");
		setModal(false);
		Container pane = getContentPane();
		pane.setLayout(new BorderLayout());
		setDialogPanel(panel);
		pane.add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		pack();
	}

	/**
   *
   */
	void setDialogPanel(GEDialogPanel panel) {
		Container pane = getContentPane();
		if (_panel != null)
			pane.remove(_panel);
		_panel = panel;
		pane.add(_panel, BorderLayout.CENTER);
	}

	/**
    *
    */
	GEDialogPanel getDialogPanel() {
		return _panel;
	}

	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	public void applyChanges() {
		_panel.applyChanges();
		Component comp = getParent();
		if (comp instanceof GraphFrameInterface) {
			GraphFrameInterface gf = (GraphFrameInterface) comp;
			GECanvas gc = gf.getCanvas();
			gc.redoNextPaint();
			gc.update(gc.getGraphics());
		}
	}

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	public void doneChanges() {
		setVisible(false);
		dispose();
	}

	/**
	 * the dialog panel
	 */
	private GEDialogPanel _panel;
}
