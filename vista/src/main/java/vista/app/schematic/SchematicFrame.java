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
package vista.app.schematic;

import java.awt.Color;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.ImageObserver;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import vista.app.GroupFrame;
import vista.app.MainGUI;
import vista.graph.GECanvas;
import vista.graph.GraphicElement;
import vista.graph.Schematic;
import vista.set.Group;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: SchematicFrame.java,v 1.5 1999/01/07 21:05:34 nsandhu Exp $
 */
public class SchematicFrame extends JFrame {
	private Schematic _sc;
	private Group _cg;
	public GECanvas _canvas;
	public ImageIcon ii;
	public Image img;
	public ImageObserver imgObs;
	public int xi, yi, wi, hi;
	public GroupFrame _gf;

	/**
   *
   */
	public SchematicFrame(Schematic sc) {
		super("Schematic");
		_cg = null;
		//
		// ii = new ImageIcon(VistaUtils.getImageAsBytes("/vista/delta1.gif"));
		// xi=32; yi=25; wi=460; hi=660;
		// create menu
		JMenuBar mbar = new JMenuBar();
		JMenu groupMenu = new JMenu("Group");
		JMenuItem tableItem = new JMenuItem("Show as table");
		tableItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				MainGUI.getContext().getCurrentSession().insertGroupAt(0, _cg);
				MainGUI.getContext().setCurrentGroup(_cg);
			}
		});
		groupMenu.add(tableItem);
		mbar.add(groupMenu);
		setJMenuBar(mbar);
		//
		_sc = sc;
		_canvas = new GECanvas(_sc);
		getContentPane().add(_canvas);
		_canvas.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				int x = e.getX();
				int y = e.getY();
				int cc = e.getClickCount();
				GraphicElement ge = _sc.getHitElement(x, y);
				Object obj = _sc.getHitElementObject(x, y);
				if (obj != null) {// callback method
					if (cc == 1) {
						Color fg = ge.getAttributes()._foregroundColor;
						if (!fg.equals(Color.red))
							ge.getAttributes()._foregroundColor = Color.red;
						else
							ge.getAttributes()._foregroundColor = Color.blue
									.darker().darker();
						_canvas.redoNextPaint();
						repaint();
						clickedOn(obj);
					} else {
						doubleClickedOn(obj);
					}
				}
			}
		});
		// c.addMouseMotionListener( new MouseMotionAdapter(){
		// public void mouseMoved(MouseEvent e){
		// int x = e.getX(); int y = e.getY();
		// ge.draw();
		// Object obj = _sc.getHitElementObject(x,y);
		// if ( obj != null ){
		// movedOver(obj);
		// }
		// }
		// });
	}

	/*
	 * public void paint(Graphics g){ img = ii.getImage(); imgObs =
	 * ii.getImageObserver(); g.drawImage(img,xi,yi,wi,hi,imgObs);
	 * _canvas.setDoubleBuffered(false); _canvas.paint(g); }
	 */
	/**
   *
   */
	public void clickedOn(Object obj) {
		if (obj instanceof Group) {
			if (_cg == null) {
				_cg = (Group) obj;
			} else {
				_cg = _cg.unionWith((Group) obj);
			}
		}
	}

	/**
   *
   */
	public void doubleClickedOn(Object obj) {
		// System.out.println("Double clicked on " + obj);
	}

	/**
   *
   */
	public void movedOver(Object obj) {
		// System.out.println("Moved over " + obj);
	}
}
