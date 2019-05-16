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
package vista.graph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.PrintJob;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.IOException;
import java.util.Properties;
import java.util.StringTokenizer;
import javax.swing.*;

import vista.app.MainProperties;
import vista.gui.VistaUtils;

//import sun.awt.motif.*;

/**
 * A print previewer
 *
 * @author Nicky Sandhu
 * @version $Id: PrintPreviewer.java,v 1.1 2003/10/02 20:49:07 redwood Exp $
 */
public class PrintPreviewer extends JFrame implements MouseListener,
													  MouseMotionListener
{
	public static int PORTRAIT = 1;
	public static int LANDSCAPE = 2;
	public static Rectangle _lastRectangle = new Rectangle(0, 0, 10, 10);
	public static int _lastOrientation = PORTRAIT;
	public static String[] _printerNames;

	static
	{
		String prop = MainProperties.getProperties().getProperty("pp.rect");
		StringTokenizer st = new StringTokenizer(prop, ",");
		_lastRectangle.x = new Integer(st.nextToken()).intValue();
		_lastRectangle.y = new Integer(st.nextToken()).intValue();
		_lastRectangle.width = new Integer(st.nextToken()).intValue();
		_lastRectangle.height = new Integer(st.nextToken()).intValue();
		prop = MainProperties.getProperties().getProperty("printer.names");
		st = new StringTokenizer(prop, ",");
		_printerNames = new String[st.countTokens()];
		int i = 0;
		while(st.hasMoreTokens())
		{
			_printerNames[i++] = st.nextToken();
		}
	}

	public GECanvas canvas;
	public GraphicElement _ge;
	public Rectangle _prevRectangle;
	private int _pp = 72;
	private int _ox = 0, _oy = 0;
	private int _pw = 8 * 72, _ph = 11 * 72;
	private Insets _margins;
	private JRadioButtonMenuItem _lo, _po;
	private boolean _drawRect = false;
	private int _initialX = 0, _initialY = 0;
	private boolean _doMoving = false, _doResizing = false;
	private Cursor NORMAL_CURSOR = Cursor.getPredefinedCursor(0),
			MOVE_CURSOR = Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR);
	private int zoneSize = 8;
	private javax.swing.Timer _timer;
	private int _orientation;
	private boolean nr, sr, er, wr;

	/**
	 *
	 */
	public PrintPreviewer(GraphicElement ge)
	{
		JMenuBar mbar = new JMenuBar();
		JMenu printMenu = new JMenu("Print");
		JMenu printItem = new JMenu("Print to");
		JMenuItem printFileItem = new JMenuItem("File...");
		JMenuItem printPrinterItem = new JMenuItem("Printer...");
		JMenu orItem = new JMenu("Orientation");
		_lo = new JRadioButtonMenuItem("Landscape");
		_po = new JRadioButtonMenuItem("Portrait");
		orItem.add(_lo);
		orItem.add(_po);
		ButtonGroup bg = new ButtonGroup();
		bg.add(_lo);
		bg.add(_po);
		if(_lastOrientation == PORTRAIT)
		{
			_po.setSelected(true);
		}
		else
		{
			_lo.setSelected(true);
		}
		JMenuItem doneItem = new JMenuItem("Done Previewing");
		printItem.add(printFileItem);
		printItem.add(printPrinterItem);
		printMenu.add(printItem);
		printMenu.add(orItem);
		printMenu.addSeparator();
		printMenu.add(doneItem);
		mbar.add(printMenu);
		setJMenuBar(mbar);

		printFileItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				printFile();
			}
		});
		printPrinterItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				printPrinter();
			}
		});
		doneItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				// save pprect to properties
				Rectangle r = _lastRectangle;
				String rectProp = "" + r.x + "," + r.y + "," + r.width + ","
						+ r.height;
				MainProperties.getProperties().put("pp.rect", rectProp);
				_lastOrientation = _orientation;
				//
				setVisible(false);
				dispose();
			}
		});
		ActionListener orlistener = new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				Object obj = evt.getSource();
				if(!(obj instanceof JRadioButtonMenuItem))
				{
					return;
				}
				JRadioButtonMenuItem ji = (JRadioButtonMenuItem) obj;
				if(ji.getText().indexOf("Portrait") >= 0)
				{
					setOrientation(PORTRAIT);
				}
				else
				{
					setOrientation(LANDSCAPE);
				}
			}
		};
		_lo.addActionListener(orlistener);
		_po.addActionListener(orlistener);
		//
		_ge = ge;
		canvas = new GECanvas(ge);
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(canvas, BorderLayout.CENTER);
		setBackground(Color.white);
		canvas.redoNextPaint();
		canvas.setBounds(_lastRectangle);
		//
		addMouseListener(this);
		addMouseMotionListener(this);
		_timer = new javax.swing.Timer(400, new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				paint(getGraphics());
			}
		});
		//
		_orientation = _lastOrientation;
		if(_orientation == PORTRAIT)
		{
			setSize(_pw, _ph);
		}
		else
		{
			setSize(_ph, _pw);
		}
		this.setResizable(false);
		setVisible(true);
	}

	/**
	 *
	 */
	public void setOrientation(int or)
	{
		float dpi = 72.0f;
		if(or == _orientation)
		{
			return;
		}
		_orientation = or;
		setParams(_lastRectangle.x / dpi, _lastRectangle.y / dpi,
				_lastRectangle.height / dpi, _lastRectangle.width / dpi);
		if(_orientation == PORTRAIT)
		{
			setSize(_pw, _ph);
		}
		else
		{
			setSize(_ph, _pw);
		}
	}

	/**
	 * prints to file after quering by filename dialog for filename
	 */
	public void printFile()
	{
		String filename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "ps", "Post script file");
		if(filename == null || filename.length() == 0)
		{
			return;
		}
		printToFile(filename);
	}

	/**
	 * prints to printer after querying for printer name. Actual mechanism only
	 * works on Solaris where the result is output to a tmp file, then printed
	 * to printer
	 */
	public void printPrinter()
	{
		String osname = System.getProperty("os.name");
		if(osname.indexOf("olaris") >= 0)
		{
			// all this is pretty much for Solaris
			String printerName = (String) JOptionPane.showInputDialog(this,
					"Printer Name", "Printer chooser",
					JOptionPane.PLAIN_MESSAGE, null, _printerNames,
					_printerNames[0]);
			if(printerName == null || printerName.length() == 0)
			{
				return;
			}
			String filename = "/tmp/vista" + new java.util.Random().nextInt()
					+ ".ps";
			printToFile(filename);
			try
			{
				Runtime.getRuntime().exec(
						"lp -d " + printerName.trim() + " " + filename
								+ "; rm -rf " + filename);
			}
			catch(IOException ioe)
			{
				VistaUtils.displayException(this, ioe);
			}
		}
		else
		{
			GraphicElement ge = canvas.getGraphicElement();
			Dimension d = ge.getSize();
			Dimension d2 = getSize();
			// set up printing properties
			Properties printProps = new Properties();
			printProps.put("awt.print.destination", "printer");
			if(_po.isSelected())
			{
				printProps.put("awt.print.orientation", "portrait");
			}
			else
			{
				printProps.put("awt.print.orientation", "landscape");
			}
			// get print job
			PrintJob pjob = null;
			if(pjob == null)
			{
				pjob = Toolkit.getDefaultToolkit().getPrintJob(this, "",
						printProps);
			}
			Graphics pg = pjob.getGraphics();
			pg.setClip(canvas.getBounds());
			ge.setBounds(canvas.getBounds());
			ge.setGraphics(pg);
			if(ge instanceof GEContainer)
			{
				((GEContainer) ge).doLayout();
			}
			ge.draw(pg);
			pg.dispose();
			pjob.end();
		}
	}

	/**
	 * prints to given filename
	 */
	public void printToFile(String filename)
	{
		try
		{
			GraphicElement ge = canvas.getGraphicElement();
			Dimension d = ge.getSize();
			Dimension d2 = getSize();
			PrintJob pjob = null;
			String osname = System.getProperty("os.name");
			if(osname.indexOf("olaris") >= 0)
			{
				// uncomment and compile on solaris
				// pjob = new sun.awt.motif.PSPrintJob(ps,"Graph",d2,72);
			}
			else if(osname.indexOf("indows") >= 0)
			{
			}
			else
			{
			}
			if(pjob == null)
			{
				// set up printing properties
				Properties printProps = new Properties();
				printProps.put("awt.print.destination", "file");
				printProps.put("awt.print.fileName", filename);
				if(_po.isSelected())
				{
					printProps.put("awt.print.orientation", "portrait");
				}
				else
				{
					printProps.put("awt.print.orientation", "landscape");
				}
				pjob = Toolkit.getDefaultToolkit().getPrintJob(this, "",
						printProps);
			}
			Graphics pg = pjob.getGraphics();
			pg.setClip(canvas.getBounds());
			ge.setBounds(canvas.getBounds());
			ge.setGraphics(pg);
			if(ge instanceof GEContainer)
			{
				((GEContainer) ge).doLayout();
			}
			ge.draw(pg);
			pg.dispose();
			pjob.end();
		}
		catch(Exception e)
		{
			VistaUtils.displayException(this, e);
		}
	}

	/**
	 * set the margins
	 *
	 * @param tm     distance from top side of page
	 * @param lm     distance from left side of page
	 * @param width  width of graph
	 * @param height height of graph All dimension are in inches
	 */
	public void setParams(float tm, float lm, float w, float h)
	{
		int t = Math.round(tm * 72);
		int l = Math.round(lm * 72);
		int width = Math.round(w * 72);
		int height = Math.round(h * 72);
		if(height <= 0 || width <= 0)
		{
		}
		else
		{
			_lastRectangle.x = l;
			_lastRectangle.y = t;
			_lastRectangle.width = width;
			_lastRectangle.height = height;
			canvas.setBounds(_lastRectangle);
			canvas.redoNextPaint();
		}
	}

	/**
	 *
	 */
	public void paint(Graphics g)
	{
		// if in draw rectangle mode watch for mouse motions
		if(_drawRect)
		{
			// get image of graph canvas
			// if ( _doResizing ) canvas.redoNextPaint();
			super.setBackground(Color.white);
			super.paint(g);
			Rectangle r = canvas.getBounds();
			Rectangle rr = getRootPane().getBounds();
			r.x = _lastRectangle.x - rr.x;
			r.y = _lastRectangle.y - rr.y;
			rr = getContentPane().getBounds();
			r.x = r.x - rr.x;
			r.y = r.y - rr.y;
			r.width = _lastRectangle.width;
			r.height = _lastRectangle.height;
			if(_doResizing && !(nr || sr || er || wr))
			{ // saves time for huge
				// plots
				canvas.redoNextPaint();
				canvas.setBounds(r);
			}
			if(_doMoving)
			{
				canvas.setBounds(r);
			}
			// canvas.setBounds(_lastRectangle);
			g.drawRect(_lastRectangle.x, _lastRectangle.y,
					_lastRectangle.width, _lastRectangle.height);
			// draw rectangle on top of canvas
		}
		else
		{
			super.setBackground(Color.white);
			super.paint(g);
			Rectangle r = canvas.getBounds();
			Rectangle rr = getRootPane().getBounds();
			r.x = _lastRectangle.x - rr.x;
			r.y = _lastRectangle.y - rr.y;
			rr = getContentPane().getBounds();
			r.x = r.x - rr.x;
			r.y = r.y - rr.y;
			r.width = _lastRectangle.width;
			r.height = _lastRectangle.height;
			canvas.redoNextPaint();
			canvas.setBounds(r);
		}
	}

	/**
	 * If the middle mouse btn is pressed zoom into the current zoom rectangle
	 * and return. If the zoom rectangle is visible on the plot, check if user
	 * pressed mouse within the four hot spot regions. Two of these regions are
	 * for moving and two for resizing the zoom rectangle. Display tooltips for
	 * these hot spot regions. If the zoom rectangle is not visible mark the
	 * initial point and final point as the initial point and some resonable
	 * width/height.
	 */
	public void mousePressed(MouseEvent e)
	{
		// on pressing middle mouse btn
		if(SwingUtilities.isMiddleMouseButton(e))
		{
			if(!_drawRect)
			{
				_drawRect = true; // draw the rectangle
			}
			else
			{
				setCursor(NORMAL_CURSOR);
				_drawRect = false;
			}
		}
		else
		{
			if(_drawRect)
			{
				_prevRectangle = new Rectangle(_lastRectangle);
				_initialX = e.getX();
				_initialY = e.getY();
			}
		}

	}

	/**
	 * mouse moved
	 */
	public void mouseMoved(MouseEvent evt)
	{
		if(_drawRect)
		{ // if in resize/move mode
			_timer.stop();
			int x = evt.getX();
			int y = evt.getY();
			Rectangle r = _lastRectangle;
			// if mouse is within 2pixels of rectangle boundary -> resize
			// if mouse is inside boundary by inside of 2pixels -> move
			if(_lastRectangle.contains(x, y))
			{
				if(Math.abs(_lastRectangle.y - y) <= zoneSize)
				{
					nr = true;
				}
				else if(Math.abs(_lastRectangle.y + _lastRectangle.height
						- y) <= zoneSize)
				{
					sr = true;
				}
				else
				{
					nr = sr = false;
				}
				if(Math.abs(_lastRectangle.x - x) <= zoneSize)
				{
					wr = true;
				}
				else if(Math
						.abs(_lastRectangle.x + _lastRectangle.width - x) <= zoneSize)
				{
					er = true;
				}
				else
				{
					wr = er = false;
				}
				if(nr || sr || wr || er)
				{
					_doMoving = false;
					_doResizing = true;
					setCursor();
				}
				else
				{
					_doMoving = true;
					_doResizing = false;
					nr = sr = wr = er = false;
					setCursor(MOVE_CURSOR);
				}
			}
			else
			{
				nr = sr = wr = er = false;
				_doMoving = _doResizing = false;
				setCursor();
			}
		}
		else
		{
			nr = sr = wr = er = false;
			_doMoving = _doResizing = false;
		}
	}

	/**
	 *
	 */
	private void setCursor()
	{
		Cursor cursor = NORMAL_CURSOR;
		if(nr && er)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.NE_RESIZE_CURSOR);
		}
		else if(nr && wr)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR);
		}
		else if(sr && wr)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.SW_RESIZE_CURSOR);
		}
		else if(sr && er)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR);
		}
		else if(nr)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR);
		}
		else if(sr)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.S_RESIZE_CURSOR);
		}
		else if(wr)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR);
		}
		else if(er)
		{
			cursor = Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR);
		}
		else
		{
			cursor = NORMAL_CURSOR;
		}
		setCursor(cursor);
	}

	/**
	 * If mouse has not been dragged then don't draw zoom rectangle
	 */
	public void mouseReleased(MouseEvent e)
	{
		nr = sr = wr = er = false;
		if(_timer != null)
		{
			_timer.stop();
		}
		repaint();
	}

	/**
	 * If mouse has not been dragged then don't draw zoom rectangle
	 */
	public void mouseClicked(MouseEvent e)
	{
	}

	/**
	 * If mouse has not been dragged then don't draw zoom rectangle
	 */
	public void mouseEntered(MouseEvent e)
	{
	}

	/**
	 * If mouse has not been dragged then don't draw zoom rectangle
	 */
	public void mouseExited(MouseEvent e)
	{
	}

	/**
	 * As mouse is dragged a rectangle showing the currently selected zoom
	 * region is displayed as a rectangle. To achieve good performance double
	 * buffering was used. If resizing mode is on then rectangle is resized as
	 * its being dragged with the anchor being the opposite corner of the
	 * rectangle If the moving mode is on then the rectangle is moved with the
	 * rectangle shape being preserved.
	 */
	public void mouseDragged(MouseEvent e)
	{
		if(_drawRect)
		{
			_timer.start();
			if(_doMoving)
			{
				int x = e.getX();
				int y = e.getY();
				int delx = x - _initialX;
				int dely = y - _initialY;
				_lastRectangle.x += delx;
				_lastRectangle.y += dely;
				_initialX = x;
				_initialY = y;
			}
			if(_doResizing)
			{
				int x = e.getX();
				int y = e.getY();
				int delx = x - _initialX;
				int dely = y - _initialY;
				if(nr)
				{
					// near top
					_lastRectangle.y += dely;
					_lastRectangle.height -= dely;
				}
				else if(sr)
				{
					// near bottom
					_lastRectangle.height += dely;
				}
				if(wr)
				{
					// near left
					_lastRectangle.x += delx;
					_lastRectangle.width -= delx;
				}
				else if(er)
				{
					// near right
					_lastRectangle.width += delx;
				}
				_initialX = x;
				_initialY = y;
			}
		}
	}
}
