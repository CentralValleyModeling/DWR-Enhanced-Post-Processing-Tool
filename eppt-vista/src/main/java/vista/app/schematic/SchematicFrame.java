/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */
package vista.app.schematic;

import java.awt.Color;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.ImageObserver;
import javax.swing.*;

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
