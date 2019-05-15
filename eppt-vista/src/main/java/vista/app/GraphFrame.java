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
package vista.app;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.PrintJob;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.InputStream;
import javax.swing.*;

import vista.graph.AttributeSerializer;
import vista.graph.CoordinateDisplayInteractor;
import vista.graph.Curve;
import vista.graph.ElementInteractor;
import vista.graph.FontResizeInteractor;
import vista.graph.GECanvas;
import vista.graph.GEContainer;
import vista.graph.GETreeDialog;
import vista.graph.Graph;
import vista.graph.GraphFrameInterface;
import vista.graph.GraphProperties;
import vista.graph.GraphicElement;
import vista.graph.ImageSerializer;
import vista.graph.InfoDialog;
import vista.graph.ZoomInteractor;
import vista.gui.VistaUtils;

/**
 * This class constructs a frame and provides the context with which to interact
 * with the Graph object. The Graph object itself is contained with the
 * GraphCanvas object
 *
 * @author Nicky Sandhu
 * @version $Id: GraphFrame.java,v 1.5 2001/03/05 21:48:08 eli2 Exp $
 * @see Graph
 * @see HEC.DSS.GraphCanvas
 */
public class GraphFrame extends JFrame implements GraphFrameInterface
{
	/**
	 * for debuggin'
	 */
	public boolean DEBUG = false;
	/**
	 * The component on which the graph is drawn.
	 */
	public GECanvas _gC = null;
	private ElementInteractor _zi, _ri;

	/**
	 * Constructor
	 */
	public GraphFrame(Graph graph, String frameTitle)
	{
		super(frameTitle);
		setIconImage(Toolkit.getDefaultToolkit().createImage(
				VistaUtils.getImageAsBytes("/vista/planning.gif")));

		addGECanvas(graph);

		ActionListener bl = new ButtonListener(this);

		JMenuBar mb = new JMenuBar();

		JMenu mainMenu = new JMenu(GraphProperties.properties
				.getProperty("GraphFrame.MAIN_MENU"));
		JMenuItem printItem = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.PRINT"));
		printItem.addActionListener(bl);
		// JMenuItem save2GifItem = new
		// JMenuItem(GraphProperties.properties.getProperty("GraphFrame.SAVE2GIF"));
		// save2GifItem.addActionListener(bl);
		JMenuItem save2PSItem = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.SAVE2PS"));
		save2PSItem.addActionListener(bl);
		JMenuItem save2PPMItem = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.SAVE2PPM"));
		save2PPMItem.addActionListener(bl);
		JMenuItem save2JpegItem = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.SAVE2JPEG"));
		save2JpegItem.addActionListener(bl);
		JMenuItem quitItem = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.QUIT"));
		quitItem.addActionListener(new QuitListener());

		JMenuItem loadAttrItem = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.LOAD_ATTR"));
		loadAttrItem.addActionListener(new AttrListener());
		JMenuItem saveAttrItem = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.SAVE_ATTR"));
		saveAttrItem.addActionListener(new AttrListener());

		mainMenu.add(printItem);
		mainMenu.addSeparator();
		// mainMenu.add(save2GifItem);
		mainMenu.add(save2PSItem);
		mainMenu.add(save2PPMItem);
		mainMenu.add(save2JpegItem);
		mainMenu.addSeparator();
		mainMenu.add(loadAttrItem);
		mainMenu.add(saveAttrItem);
		mainMenu.addSeparator();
		mainMenu.add(quitItem);

		JMenu editorMenu = new JMenu(GraphProperties.properties
				.getProperty("GraphFrame.EDIT_MENU"));
		JMenuItem graphEdit = new JMenuItem(GraphProperties.properties
				.getProperty("GraphFrame.EDIT_GRAPH"));
		graphEdit.addActionListener(new EditListener(this));
		JMenuItem flagEdit = new JMenuItem("Edit Flags");
		flagEdit.addActionListener(new FlagEditorListener());
		//
		JMenuItem displayLocationItem = new JMenuItem("Display Co-ordinates");
		displayLocationItem.addActionListener(new DisplayCoordinateListener());
		//
		JMenuItem pagingModeItem = new JMenuItem("Set Paging On");
		pagingModeItem.addActionListener(new PagingModeListener());
		//
		editorMenu.add(graphEdit);
		editorMenu.add(flagEdit);
		editorMenu.add(displayLocationItem);
		editorMenu.add(pagingModeItem);
		//
		DisplayFlagListener fdl = new DisplayFlagListener();
		JCheckBoxMenuItem goodItem = new JCheckBoxMenuItem("Display Good");
		JCheckBoxMenuItem questionableItem = new JCheckBoxMenuItem(
				"Display Questionable");
		JCheckBoxMenuItem rejectItem = new JCheckBoxMenuItem("Display Reject");
		JCheckBoxMenuItem unscreenedItem = new JCheckBoxMenuItem(
				"Display Unscreened");
		if(GraphProperties.properties.get("displayGood").equals("true"))
		{
			goodItem.setSelected(true);
		}
		if(GraphProperties.properties.get("displayQuestionable")
									 .equals("true"))
		{
			questionableItem.setSelected(true);
		}
		if(GraphProperties.properties.get("displayReject").equals("true"))
		{
			rejectItem.setSelected(true);
		}
		if(GraphProperties.properties.get("displayUnscreened").equals("true"))
		{
			unscreenedItem.setSelected(true);
		}
		//
		AppUtils.setCurveFilter(graph, AppUtils.getCurrentCurveFilter());
		//
		fdl.addGoodMenuItem(goodItem);
		fdl.addQuestionableMenuItem(questionableItem);
		fdl.addRejectMenuItem(rejectItem);
		fdl.addUnscreenedMenuItem(unscreenedItem);
		JMenu displayMenu = new JMenu("Display Options");
		displayMenu.add(goodItem);
		displayMenu.add(questionableItem);
		displayMenu.add(rejectItem);
		displayMenu.add(unscreenedItem);
		//
		mb.add(mainMenu);
		mb.add(editorMenu);
		mb.add(displayMenu);
		this.setJMenuBar(mb);

		String propertiesFile = GraphProperties.properties
				.getProperty("GraphFrame.graphPropertiesFile");
		InputStream is = VistaUtils.getFileAsStream(propertiesFile);
		if(is == null)
		{
			is = VistaUtils.getResourceAsStream(propertiesFile);
		}
		if(is == null)
		{
			is = VistaUtils.getPropertyFileAsStream("demo1.properties");
		}
		if(is == null)
		{
			is = VistaUtils
					.getResourceAsStream("/vista/graph/demo1.properties");
		}
		new AttributeSerializer(graph).load(is);

		this.pack();
		Toolkit tk = getToolkit();
		Dimension screenSize = tk.getScreenSize();
		Dimension frameSize = getSize();
		this.setLocation(screenSize.width - frameSize.width, screenSize.height
				- frameSize.height);
		this.setVisible(true);

	}

	/**
	 * adds GraphicElement canvas
	 */
	private void addGECanvas(Graph graph)
	{
		_gC = new GECanvas(graph);

		// add graph canvas to frame and set its listeners
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(_gC, BorderLayout.CENTER);
		//
		addInteractors(graph);
	}

	public void addToolBar(JToolBar tb)
	{
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(_gC, BorderLayout.CENTER);
		this.getContentPane().removeAll();
		this.getContentPane().add(tb, BorderLayout.NORTH);
		this.getContentPane().add(mainPanel);
	}

	/**
	 * sets graph in canvas
	 */
	public void setGraph(Graph graph)
	{
		new AttributeSerializer(graph).load(GraphProperties.properties
				.getProperty("GraphFrame.graphPropertiesFile"));
		_gC.setGraphicElement(graph);
		addInteractors(graph);
		_gC.redoNextPaint();
		_gC.paint(_gC.getGraphics());
	}

	/**
	 *
	 */
	private void addInteractors(Graph graph)
	{
		if(_zi != null)
		{
			_gC.removeMouseListener(_zi);
			_gC.removeMouseMotionListener(_zi);
			_gC.removeKeyListener(_zi);
		}
		if(_ri != null)
		{
			_gC.removeComponentListener(_ri);
		}
		//
		_zi = new ZoomInteractor(_gC);
		if(graph.getAttributes()._backgroundColor == Color.black)
		{
			((ZoomInteractor) _zi).setZoomRectangleColor(Color.white);
		}
		_gC.addMouseListener(_zi);
		_gC.addMouseMotionListener(_zi);
		_ri = new FontResizeInteractor(_gC);
		_gC.addComponentListener(_ri);
	}

	/**
	 * does printing to file or printer using java core classes.
	 */
	public void doPrint(Frame f)
	{
		// set size to 8.5 X 11 inches == 21.25 cm X 27.5 cm
		Dimension pSize = f.getSize();
		int resolution = 72; // in pixels per inch
		// f.setSize((int) 8.5*resolution, 11*resolution);
		// landscape
		f.setSize(11 * resolution, (int) 8.5 * resolution);
		PrintJob pj = Toolkit.getDefaultToolkit().getPrintJob(f,
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
	 * Outputs plot to ps file.
	 */
	public void outputPS()
	{

		FileDialog dialog = new FileDialog(this, GraphProperties.properties
				.getProperty("GraphFrame.psSelectionMsg"), FileDialog.SAVE);
		dialog.setFile(GraphProperties.properties
				.getProperty("GraphFrame.PS_FILE"));
		dialog.pack();
		dialog.setVisible(true);
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
	protected class EditListener implements ActionListener
	{
		/**
		 *
		 */
		protected GraphFrame _tg;

		/**
		 *
		 */
		public EditListener(GraphFrame tg)
		{
			_tg = tg;
		}

		/**
		 *
		 */
		public void actionPerformed(ActionEvent e)
		{
			new GETreeDialog(GraphFrame.this, getCanvas());
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

		/**
		 *
		 */
		public void actionPerformed(ActionEvent e)
		{
			Object s = e.getSource();
			if(s instanceof JMenuItem)
			{
				JMenuItem mi = (JMenuItem) s;
				String label = mi.getText();
				if(label.equals(GraphProperties.properties
						.getProperty("GraphFrame.LOAD_ATTR")))
				{
					loadAttributes();
				}
				else if(label.equals(GraphProperties.properties
						.getProperty("GraphFrame.SAVE_ATTR")))
				{
					saveAttributes();
				}
				else
				{

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
				_attrS = new AttributeSerializer(_gC
						.getGraphicElement());
			}
			_attrS.saveAttributes();
		}
	}

	/**
	 *
	 */
	protected class QuitListener implements ActionListener
	{
		/**
		 *
		 */
		public void actionPerformed(ActionEvent e)
		{
			// System.exit(0);
			dispose();
		}
	}

	/**
	 *
	 */
	protected class ButtonListener implements ActionListener
	{
		/**
		 *
		 */
		protected GraphFrame _tg;

		/**
		 *
		 */
		public ButtonListener(GraphFrame tg)
		{
			_tg = tg;
		}

		/**
		 * Handles button events for
		 */
		public void actionPerformed(ActionEvent e)
		{
			if(e.getSource() instanceof JMenuItem)
			{
				JMenuItem mItem = (JMenuItem) e.getSource();
				String label = mItem.getText();
				if(label.equals(GraphProperties.properties
						.getProperty("GraphFrame.PRINT")))
				{
					doPrint(_tg);
				}
				else if(label.equals(GraphProperties.properties
						.getProperty("GraphFrame.QUIT")))
				{
					System.exit(0);
					/*
					 * }else if
					 * (label.equals(GraphProperties.properties.getProperty
					 * ("GraphFrame.SAVE2GIF"))){ _tg.outputGif();
					 */
				}
				else if(label.equals(GraphProperties.properties
						.getProperty("GraphFrame.SAVE2PS")))
				{
					_tg.outputPS();
				}
				else if(label.equals(GraphProperties.properties
						.getProperty("GraphFrame.SAVE2JPEG")))
				{
					_tg.outputJpeg();
				}
			}
		}
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: GraphFrame.java,v 1.5 2001/03/05 21:48:08 eli2 Exp $
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

	/**
	 * @author Nicky Sandhu
	 * @version $Id: GraphFrame.java,v 1.5 2001/03/05 21:48:08 eli2 Exp $
	 */
	private class PagingModeListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			JMenuItem mi = (JMenuItem) evt.getSource();
			String label = mi.getText();
			ZoomInteractor zi = (ZoomInteractor) _zi;
			if(label.indexOf("On") >= 0)
			{
				if(zi != null)
				{
					zi.setPagingMode(true);
				}
				mi.setText("Set Paging Off");
			}
			else
			{
				if(zi != null)
				{
					zi.setPagingMode(false);
				}
				mi.setText("Set Paging On");
			}
		}
	} // end of Paging..

	/**
	 * @author Nicky Sandhu
	 * @version $Id: GraphFrame.java,v 1.5 2001/03/05 21:48:08 eli2 Exp $
	 */
	class DisplayFlagListener implements ActionListener
	{
		JMenuItem qItem, gItem, rItem, sItem, uItem;

		/**
		 *
		 */
		public void addQuestionableMenuItem(JMenuItem q)
		{
			qItem = q;
			qItem.addActionListener(this);
		}

		public void addGoodMenuItem(JMenuItem g)
		{
			gItem = g;
			gItem.addActionListener(this);
		}

		public void addRejectMenuItem(JMenuItem r)
		{
			rItem = r;
			rItem.addActionListener(this);
		}

		public void addUnscreenedMenuItem(JMenuItem us)
		{
			uItem = us;
			uItem.addActionListener(this);
		}

		/**
		 *
		 */
		public void actionPerformed(ActionEvent evt)
		{
			GraphicElement ge = _gC.getGraphicElement();
			if(!(ge instanceof Graph))
			{
				return;
			}
			Graph graph = (Graph) ge;
			if(qItem.isSelected())
			{
				GraphProperties.properties.put("displayQuestionable", "true");
			}
			else
			{
				GraphProperties.properties.put("displayQuestionable", "false");
			}
			if(gItem.isSelected())
			{
				GraphProperties.properties.put("displayGood", "true");
			}
			else
			{
				GraphProperties.properties.put("displayGood", "false");
			}
			if(rItem.isSelected())
			{
				GraphProperties.properties.put("displayReject", "true");
			}
			else
			{
				GraphProperties.properties.put("displayReject", "false");
			}
			if(uItem.isSelected())
			{
				GraphProperties.properties.put("displayUnscreened", "true");
			}
			else
			{
				GraphProperties.properties.put("displayUnscreened", "false");
			}
			AppUtils.setCurveFilter(graph, AppUtils.getCurrentCurveFilter());
			_gC.redoNextPaint();
			_gC.paint(_gC.getGraphics());
		}
	}

	/**
	 *
	 */
	class FlagEditorListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Graph graph = (Graph) _gC.getGraphicElement();
			GEContainer curveContainer = graph.getPlot().getCurveContainer();
			GraphicElement[] curves = curveContainer.getElements(Curve.class);
			Curve curve = (Curve) curves[0];
			new vista.app.FlagEditor(_gC, curve, true);
		}
	}
}
