/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Graphics;
import java.awt.PrintJob;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.InputStream;
import javax.swing.*;

import vista.graph.AttributeSerializer;
import vista.graph.CoordinateDisplayInteractor;
import vista.graph.ElementInteractor;
import vista.graph.FontResizeInteractor;
import vista.graph.GECanvas;
import vista.graph.GETreeDialog;
import vista.graph.Graph;
import vista.graph.GraphProperties;
import vista.graph.GraphicElement;
import vista.graph.ImageSerializer;
import vista.graph.InfoDialog;
import vista.gui.VistaUtils;
import vista.set.DataReference;

/**
 * This class constructs a frame and provides the context with which to interact
 * with the Graph object. The Graph object itself is contained with the
 * GraphCanvas object
 *
 * @author Nicky Sandhu
 * @version $Id: MultiScatterGraph.java,v 1.1 2003/10/02 20:48:35 redwood Exp $
 * @see Graph
 * @see HEC.DSS.GraphCanvas
 */
public class MultiScatterGraph extends JFrame
{
	/**
	 * for debuggin'
	 */
	public boolean DEBUG = false;
	/**
	 * The component on which the graph is drawn.
	 */
	public GECanvas _gC = null;
	private ElementInteractor _ri;

	/**
	 * Constructor
	 */
	public MultiScatterGraph(DataReference[] ts)
	{
		this(new MultiScatterPlot(ts));
	}

	/**
	 * Constructor
	 */
	public MultiScatterGraph(MultiScatterPlot msp)
	{
		super("Multi Scatter Plot");
		setIconImage(Toolkit.getDefaultToolkit().createImage(
				VistaUtils.getImageAsBytes("/vista/planning.gif")));
		// add graph to canvas
		addGECanvas(msp);
		// set curve filters for the graph
		// AppUtils.setCurveFilter( graph, AppUtils.getCurrentCurveFilter());
		// set up menus and their listeners
		JMenuBar mb = new JMenuBar();
		JMenu mainMenu = getMainMenu();
		JMenu displayMenu = getDisplayMenu();
		mb.add(mainMenu);
		mb.add(displayMenu);
		setJMenuBar(mb);
		// get attribute properties from graph properties file.
		String propertiesFile = "graphPropertiesFile";
		InputStream is = VistaUtils.getPropertyFileAsStream(propertiesFile);
		if(is == null)
		{
			is = VistaUtils
					.getResourceAsStream("/vista/graph/demo1.properties");
		}
		new AttributeSerializer(msp).load(is);
		// position frame and show
		this.pack();
		this.setSize(1000, 1000);
		Toolkit tk = getToolkit();
		Dimension screenSize = tk.getScreenSize();
		Dimension frameSize = getSize();
		this.setLocation(screenSize.width - frameSize.width, screenSize.height
				- frameSize.height);
		this.show();
	}

	/**
	 * adds GraphicElement canvas
	 */
	private void addGECanvas(GraphicElement ge)
	{
		// create a graphic element canvas for the graph
		_gC = new GECanvas(ge);
		// add graph canvas to frame and set its listeners
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(_gC, BorderLayout.CENTER);
		//
		addInteractors(_gC);
	}

	/**
	 * adds interactors to graph
	 */
	private void addInteractors(GECanvas gC)
	{
		if(_ri != null)
		{
			gC.removeComponentListener(_ri);
		}
		// resize interactor
		_ri = new FontResizeInteractor(gC);
		gC.addComponentListener(_ri);
	}

	/**
	 * does printing to file or printer using java core classes.
	 */
	public void doPrint()
	{
		// set size to 8.5 X 11 inches == 21.25 cm X 27.5 cm
		Dimension pSize = getSize();
		int resolution = 72; // in pixels per inch
		// f.setSize((int) 8.5*resolution, 11*resolution);
		// landscape
		setSize(11 * resolution, (int) 8.5 * resolution);
		PrintJob pj = Toolkit.getDefaultToolkit().getPrintJob(this,
				"GraphCanvas Print Job", null);
		boolean bufferStatus = _gC.getDoubleBuffered();
		if(pj != null)
		{
			Graphics pg = pj.getGraphics();
			try
			{
				_gC.setDoubleBuffered(false);
				_gC.paintAll(pg);
			}
			finally
			{
				pg.dispose();
				_gC.setDoubleBuffered(bufferStatus);
			}
			pj.end();
		}
		this.setSize(pSize.width, pSize.height);
		this.repaint();
	}

	/**
	 * Outputs plot to gif file.
	 */

	public void outputGif()
	{

		FileDialog dialog = new FileDialog(this, "gifSelectionMsg",
				FileDialog.SAVE);
		dialog.setFile(GraphProperties.properties.getProperty("GIF_FILE"));
		dialog.pack();
		dialog.show();
		if(dialog.getFile() != null)
		{
			Thread serializerThread = new Thread(new ImageSerializer(dialog
					.getFile(), _gC, ImageSerializer.GIF), "Gif serializer");
			serializerThread.setPriority(Thread.MIN_PRIORITY);
			serializerThread.run();
		}
	}

	/**
	 * Outputs plot to ps file.
	 */
	public void outputPS()
	{

		FileDialog dialog = new FileDialog(this, "psSelectionMsg",
				FileDialog.SAVE);
		dialog.setFile(GraphProperties.properties.getProperty("PS_FILE"));
		dialog.pack();
		dialog.show();
		if(dialog.getFile() != null)
		{
			String filename = dialog.getFile();
			boolean bufferStatus = _gC.getDoubleBuffered();
			_gC.setDoubleBuffered(false);
			Thread serializerThread = new Thread(new ImageSerializer(filename,
					_gC, ImageSerializer.PS), "Post-script serializer");
			serializerThread.setPriority(Thread.MIN_PRIORITY);
			serializerThread.run();
			_gC.setDoubleBuffered(bufferStatus);
		}
	}

	/**
	 * Outputs plot to jpeg file.
	 */
	public void outputJpeg()
	{
		Dialog dialog = new InfoDialog(this, "Information Dialog", true,
				"Sorry no jpeg output available yet");

		// Thread serializerThread = new Thread
		// (new ImageSerializer("junk.jpg", _gC, ImageSerializer.JPEG),
		// "Jpeg serializer");
		// serializerThread.setPriority(Thread.MIN_PRIORITY);
		// serializerThread.run();
	}

	/**
	 * gets the reference to the graph canvas
	 */
	public GECanvas getCanvas()
	{
		return _gC;
	}

	/**
	 *
	 */
	private JMenu getMainMenu()
	{
		// !! start main menu
		JMenu mainMenu = new JMenu("Graph");
		// !! start print menu...
		JMenuItem printItem = new JMenuItem("Print...");
		printItem.addActionListener(new PrintListener());
		// @ end print menu...
		// !! start save as menu....
		JMenuItem save2gif = new JMenuItem("gif");
		JMenuItem save2ps = new JMenuItem("postscript");
		JMenuItem save2ppm = new JMenuItem("ppm");
		JMenuItem save2jpeg = new JMenuItem("jpeg");

		ActionListener sal = new SaveAsListener();
		save2jpeg.addActionListener(sal);
		save2ppm.addActionListener(sal);
		save2gif.addActionListener(sal);
		save2ps.addActionListener(sal);

		JMenu saveAsMenu = new JMenu("Save As ...");
		saveAsMenu.add(save2gif);
		saveAsMenu.add(save2ps);
		saveAsMenu.add(save2ppm);
		saveAsMenu.add(save2jpeg);
		// @ end save as menu
		// !!
		JMenuItem quitItem = new JMenuItem("Quit");
		quitItem.addActionListener(new QuitListener());
		// @
		JMenuItem loadAttrItem = new JMenuItem("Load Attributes");
		loadAttrItem.addActionListener(new AttrListener());
		JMenuItem saveAttrItem = new JMenuItem("Save Attributes");
		saveAttrItem.addActionListener(new AttrListener());

		mainMenu.add(printItem);
		mainMenu.addSeparator();
		mainMenu.add(saveAsMenu);
		mainMenu.addSeparator();
		mainMenu.add(loadAttrItem);
		mainMenu.add(saveAttrItem);
		mainMenu.addSeparator();
		mainMenu.add(quitItem);
		return mainMenu;
	}

	/**
	 *
	 */
	private JMenu getDisplayMenu()
	{
		JMenu displayMenu = new JMenu("Display");
		JMenuItem graphEdit = new JMenuItem("Edit Graph Display...");
		graphEdit.addActionListener(new EditListener());
		//
		JMenuItem displayLocationItem = new JMenuItem("Display Co-ordinates");
		displayLocationItem.addActionListener(new DisplayCoordinateListener());
		//
		displayMenu.add(graphEdit);
		displayMenu.add(displayLocationItem);
		return displayMenu;
	}

	/**
	 * creates a graph attribute editing dialog
	 */
	private class EditListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			new GETreeDialog(MultiScatterGraph.this, getCanvas());
		}
	}

	/**
	 * prints current graph
	 */
	private class PrintListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			doPrint();
		}
	}

	/**
	 * Loads/saves attributes
	 */
	private class AttrListener implements ActionListener
	{
		/**
		 *
		 */
		private AttributeSerializer _attrS = null;

		public void actionPerformed(ActionEvent e)
		{
			Object s = e.getSource();
			if(s instanceof JMenuItem)
			{
				JMenuItem mi = (JMenuItem) s;
				String label = mi.getText();
				if(label.indexOf("Load") >= 0)
				{
					loadAttributes();
				}
				else if(label.indexOf("Save") >= 0)
				{
					saveAttributes();
				}
				else
				{
					throw new RuntimeException(
							"Unknown menu tried to save/load attributes!");
				}
			}
		}

		/**
		 * loads attributes
		 */
		private void loadAttributes()
		{
			if(_attrS == null)
			{
				_attrS = new AttributeSerializer(_gC
						.getGraphicElement());
			}
			if(_attrS.loadAttributes())
			{
				_gC.redoNextPaint();
				_gC.repaint();
			}
		}

		/**
		 *
		 */
		private void saveAttributes()
		{
			if(_attrS == null)
			{
				_attrS = new AttributeSerializer(_gC.getGraphicElement());
			}
			_attrS.saveAttributes();
		}
	}

	/**
	 * quits on quit command
	 */
	protected class QuitListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			dispose();
		}
	}

	/**
	 * saves this graph to various file formats
	 */
	protected class SaveAsListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			Object obj = e.getSource();
			if(!(obj instanceof JMenuItem))
			{
				return;
			}
			JMenuItem mItem = (JMenuItem) e.getSource();
			String label = mItem.getText();

			if(label.indexOf("gif") >= 0)
			{
				outputGif();
			}
			else if(label.indexOf("post") >= 0)
			{
				outputPS();
			}
			else if(label.indexOf("jpeg") >= 0)
			{
				outputJpeg();
			}
			else
			{
				throw new RuntimeException("Unknown kind of format?: " + mItem);
			}
		}

	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: MultiScatterGraph.java,v 1.1 2003/10/02 20:48:35 redwood
	 * Exp $
	 */
	private class DisplayCoordinateListener implements ActionListener
	{
		private CoordinateDisplayInteractor _cdi;

		public void actionPerformed(ActionEvent evt)
		{
			JMenuItem mi = (JMenuItem) evt.getSource();
			String label = mi.getText();
			if(label.indexOf("Don't") >= 0)
			{
				if(_cdi != null)
				{
					getCanvas().removeMouseMotionListener(_cdi);
				}
				_cdi.doneDisplaying();
				mi.setText("Display Co-ordinates");
			}
			else
			{
				_cdi = new CoordinateDisplayInteractor(getCanvas());
				getCanvas().addMouseMotionListener(_cdi);
				mi.setText("Don't Display Co-ordinates");
			}
		}
	} // end of Displa....
}
