/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.app;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.PrintJob;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.Properties;
import javax.swing.*;

import vista.graph.AttributeSerializer;
import vista.graph.CoordinateDisplayInteractor;
import vista.graph.Curve;
import vista.graph.CurveDataModel;
import vista.graph.FontResizeInteractor;
import vista.graph.GECanvas;
import vista.graph.GEContainer;
import vista.graph.GETreeDialog;
import vista.graph.Graph;
import vista.graph.GraphProperties;
import vista.graph.GraphUtils;
import vista.graph.GraphicElement;
import vista.graph.ImageSerializer;
import vista.graph.Plot;
import vista.graph.PrintPreviewer;
import vista.graph.ZoomInteractor;
import vista.gui.VistaUtils;
import vista.set.DataReference;
import vista.set.DefaultReference;
import vista.time.TimeWindow;

@SuppressWarnings("serial")
public class DataSetGraph extends JPanel
{
	public static boolean LANDSCAPE_PRINTING = false;
	public static String PRINTER_NAME = "";
	/**
	 * The component on which the graph is drawn.
	 */
	public GECanvas canvas = null;
	protected TimeSeriesMerger merger;
	private JPanel mainPanel;
	private ZoomInteractor _zi;
	private FontResizeInteractor _ri;

	public DataSetGraph(Graph graph)
	{
		this.setLayout(new BorderLayout());
		addGECanvas(graph);
		// get attribute properties from graph properties file.
		String propertiesFile = "graphPropertiesFile";
		InputStream is = VistaUtils.getPropertyFileAsStream(propertiesFile);
		if(is == null)
		{
			is = VistaUtils
					.getResourceAsStream("/vista/graph/demo1.properties");
		}
		new AttributeSerializer(graph).load(is);

	}

	public void cleanUp()
	{
		if(canvas != null)
		{
			if(mainPanel != null)
			{
				mainPanel.removeAll();
				mainPanel = null;
			}
			if(_zi != null)
			{
				_zi.releaseResources();
				canvas.removeMouseListener(_zi);
				canvas.removeMouseMotionListener(_zi);
				canvas.removeKeyListener(_zi);
				_zi = null;
			}
			if(_ri != null)
			{
				_ri.releaseResources();
				canvas.removeComponentListener(_ri);
				_ri = null;
			}
			try
			{
				canvas.finalize();
				canvas = null;
			}
			catch(Throwable exc)
			{
				exc.printStackTrace(System.err);
				throw new RuntimeException(exc.getMessage());
			}
		}

	}

	/**
	 * adds GraphicElement canvas
	 */
	private void addGECanvas(Graph graph)
	{
		// create a graphic element canvas for the graph
		canvas = new GECanvas(graph);
		this.add(canvas);
		//
		addInteractors(graph);
	}

	/**
	 * adds interactors to graph
	 */
	private void addInteractors(Graph graph)
	{
		if(_zi != null)
		{
			canvas.removeMouseListener(_zi);
			canvas.removeMouseMotionListener(_zi);
			canvas.removeKeyListener(_zi);
		}
		if(_ri != null)
		{
			canvas.removeComponentListener(_ri);
		}
		// zoom interactor
		_zi = new ZoomInteractor(canvas);
		if(graph.getAttributes()._backgroundColor == Color.black)
		{
			_zi.setZoomRectangleColor(Color.white);
		}
		canvas.addMouseListener(_zi);
		canvas.addMouseMotionListener(_zi);
		// resize interactor
		_ri = new FontResizeInteractor(canvas);
		canvas.addComponentListener(_ri);
	}

	/**
	 *
	 */
	public void doPrint()
	{
		if(GraphUtils.isJDK2())
		{
			try
			{
				String methodName = "print2d";
				Class[] params = {Class.forName("java.lang.String"),
						Boolean.TYPE, Class.forName("vista.graph.GECanvas")};
				Class cl2d = Class.forName("vista.graph.Print2D");
				Method m = cl2d.getDeclaredMethod(methodName, params);
				m.invoke(null, PRINTER_NAME,
						new Boolean(LANDSCAPE_PRINTING), canvas);
			}
			catch(Exception exc)
			{
				exc.printStackTrace(System.err);
				throw new RuntimeException("Nested Exception: "
						+ exc.getMessage());
			}

		}
		else
		{
			doPrint(PRINTER_NAME, LANDSCAPE_PRINTING);
		}
	}

	/**
	 * does printing to file or printer using java core classes.
	 */
	public void doPrint(String printer, boolean landscape)
	{
		// set size to 8.5 X 11 inches == 21.25 cm X 27.5 cm
		// Dimension pSize = getSize();
		int resolution = 72; // in pixels per inch
		// f.setSize((int) 8.5*resolution, 11*resolution);
		// landscape
		// setSize( 11*resolution, (int) 8.5*resolution);
		Properties props = new Properties();
		props.put("awt.print.printer", printer);
		if(landscape)
		{
			props.put("awt.print.orientation", "landscape");
		}
		PrintJob pj = Toolkit.getDefaultToolkit().getPrintJob(
				(Frame) SwingUtilities.getAncestorOfClass(Frame.class, this),
				"GraphCanvas Print Job", props);
		boolean bufferStatus = canvas.getDoubleBuffered();
		if(pj != null)
		{
			Graphics pg = pj.getGraphics();
			pg.translate(10, 10);
			try
			{
				canvas.setDoubleBuffered(false);
				canvas.paintAll(pg);
			}
			finally
			{
				pg.dispose();
				canvas.setDoubleBuffered(bufferStatus);
			}
			pj.end();
		}
		// this.setSize(pSize.width, pSize.height);
		this.repaint();
	}

	/**
	 * does printing to file or printer using java core classes.
	 */
	public void doPrintPreview()
	{
		new PrintPreviewer(canvas.getGraphicElement());
	}

	/**
	 * Outputs plot to gif file.
	 */

	public void outputGif()
	{
		String filename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, ".gif", "GIF Files");
		if(filename != null)
		{
			Thread serializerThread = new Thread(new ImageSerializer(filename,
					canvas, ImageSerializer.GIF), "Gif serializer");
			serializerThread.setPriority(Thread.MIN_PRIORITY);
			serializerThread.run();
		}
	}

	/**
	 * Outputs plot to ps file.
	 */
	public void outputPS()
	{
		String filename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, ".ps", "Postscript Files");
		if(filename != null)
		{
			boolean bufferStatus = canvas.getDoubleBuffered();
			canvas.setDoubleBuffered(false);
			Thread serializerThread = new Thread(new ImageSerializer(filename,
					canvas, ImageSerializer.PS), "Post-script serializer");
			serializerThread.setPriority(Thread.MIN_PRIORITY);
			serializerThread.run();
			canvas.setDoubleBuffered(bufferStatus);
		}
	}

	/**
	 * Outputs plot to jpeg file.
	 */
	public void outputJpeg()
	{
		String filename = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, ".jpg", "Jpeg Files");
		if(filename != null)
		{
			Thread serializerThread = new Thread(new ImageSerializer(filename,
					canvas, ImageSerializer.JPEG), "Jpeg serializer");
			serializerThread.setPriority(Thread.MIN_PRIORITY);
			serializerThread.run();
		}
	}

	/**
	 * gets the reference to the graph canvas
	 */
	public GECanvas getCanvas()
	{
		return canvas;
	}

	/**
	 *
	 */
	public JMenu getMainMenu()
	{
		// !! start main menu
		JMenu mainMenu = new JMenu("Graph");
		//
		JMenuItem reloadItem = new JMenuItem("Reload data");
		JMenuItem showAsTableItem = new JMenuItem("Show as Table");
		reloadItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				reloadData(evt);
			}
		});
		showAsTableItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				showAsTable(evt);
			}
		});
		// !! start print menu...
		JMenuItem printItem = new JMenuItem("Print...");
		printItem.addActionListener(new PrintListener());
		JMenuItem ppItem = new JMenuItem("Print Preview");
		ppItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				doPrintPreview();
			}
		});
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
		// @
		JMenuItem loadAttrItem = new JMenuItem("Load Attributes");
		loadAttrItem.addActionListener(new AttrListener());
		JMenuItem saveAttrItem = new JMenuItem("Save Attributes");
		saveAttrItem.addActionListener(new AttrListener());

		mainMenu.add(showAsTableItem);
		mainMenu.add(reloadItem);
		mainMenu.addSeparator();
		mainMenu.add(ppItem);
		mainMenu.addSeparator();
		mainMenu.add(printItem);
		mainMenu.addSeparator();
		mainMenu.add(saveAsMenu);
		mainMenu.addSeparator();
		mainMenu.add(loadAttrItem);
		mainMenu.add(saveAttrItem);
		mainMenu.addSeparator();
		return mainMenu;
	}

	/**
	 *
	 */
	public JMenu getDisplayMenu()
	{
		JMenu displayMenu = new JMenu("Display");
		JMenuItem graphEdit = new JMenuItem("Edit Graph Display...");
		graphEdit.addActionListener(new EditListener());
		JMenuItem flagEdit = new JMenuItem("Edit...");
		flagEdit.addActionListener(new FlagEditorListener());
		//
		JMenuItem displayLocationItem = new JCheckBoxMenuItem(
				"Display Co-ordinates");
		displayLocationItem.addActionListener(new DisplayCoordinateListener());
		JMenuItem fontResizeItem = new JCheckBoxMenuItem(
				"Set Font Resize By Ratio");
		fontResizeItem.addActionListener(new FontResizeListener());
		fontResizeItem.setSelected(true);
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
		fdl.addGoodMenuItem(goodItem);
		fdl.addQuestionableMenuItem(questionableItem);
		fdl.addRejectMenuItem(rejectItem);
		fdl.addUnscreenedMenuItem(unscreenedItem);
		JMenu flagDisplayMenu = new JMenu("Display...");
		flagDisplayMenu.add(goodItem);
		flagDisplayMenu.add(questionableItem);
		flagDisplayMenu.add(rejectItem);
		flagDisplayMenu.add(unscreenedItem);
		//
		JMenu flagMenu = new JMenu("Flag");
		flagMenu.add(flagEdit);
		flagMenu.add(flagDisplayMenu);
		//
		JMenu mergeMenu = new JMenu("Merge");
		final JMenuItem startMergeMenu = new JMenuItem("Start");
		mergeMenu.add(startMergeMenu);
		final JMenuItem cancelMergeMenu = new JMenuItem("Cancel");
		cancelMergeMenu.setEnabled(false);
		mergeMenu.add(cancelMergeMenu);
		final JMenuItem finishMergeMenu = new JMenuItem("Finish");
		finishMergeMenu.setEnabled(false);
		mergeMenu.add(finishMergeMenu);
		JMenuItem rangeOverrideMenu = new JMenuItem(
				"Override for Time range...");
		mergeMenu.add(rangeOverrideMenu);

		startMergeMenu.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				startMergeMenu.setEnabled(false);
				cancelMergeMenu.setEnabled(true);
				finishMergeMenu.setEnabled(true);
				try
				{
					merger = new TimeSeriesMerger(canvas);
				}
				catch(Exception ex)
				{
					JOptionPane.showMessageDialog(DataSetGraph.this, ex
							.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
					ex.printStackTrace();
					startMergeMenu.setEnabled(true);
					cancelMergeMenu.setEnabled(false);
					finishMergeMenu.setEnabled(false);
				}
			}
		});
		//
		finishMergeMenu.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				DataTableFrame table = new DataTableFrame(new DefaultReference(
						merger.getMergedData()));
				table.toFront();
				startMergeMenu.setEnabled(true);
				cancelMergeMenu.setEnabled(false);
				finishMergeMenu.setEnabled(false);
				merger.removeDataFromGraph();
				merger = null;
			}
		});
		//
		cancelMergeMenu.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e)
			{
				startMergeMenu.setEnabled(true);
				cancelMergeMenu.setEnabled(false);
				finishMergeMenu.setEnabled(false);
				merger.removeDataFromGraph();
				merger = null;
			}
		});
		//
		rangeOverrideMenu.addActionListener(new ActionListener()
		{

			@Override
			public void actionPerformed(ActionEvent evt)
			{
				if(merger != null)
				{
					merger.selectRange();
				}
			}
		});
		//
		displayMenu.add(flagMenu);
		displayMenu.addSeparator();
		displayMenu.add(displayLocationItem);
		displayMenu.add(fontResizeItem);
		displayMenu.addSeparator();
		displayMenu.add(mergeMenu);
		displayMenu.addSeparator();
		displayMenu.add(graphEdit);
		return displayMenu;
	}

	public void mergeAndReplace(Curve[] curves, TimeWindow timeWindow)
	{
		if(merger != null)
		{
			merger.doMerge(curves, timeWindow);
		}
	}

	/**
	 * shows the data in the graph as a table
	 */
	public void showAsTable(ActionEvent evt)
	{
		Graph graph = (Graph) canvas.getGraphicElement();
		GraphicElement[] plots = GraphUtils.getElements(graph, Plot.class);
		if(plots == null)
		{
			return;
		}
		DataReference[] refArray = null;
		for(int i = 0; i < plots.length; i++)
		{
			Plot plot = (Plot) plots[i];
			GraphicElement[] curves = GraphUtils.getElements(plot, Curve.class);
			if(curves == null)
			{
				continue;
			}
			refArray = new DataReference[curves.length];
			for(int k = 0; k < curves.length; k++)
			{
				refArray[k] = (DataReference) ((Curve) curves[k]).getModel()
																 .getReferenceObject();
			}
		}
		if(refArray.length == 1)
		{
			new DataTableFrame(refArray[0]);
		}
		else
		{
			new MultiDataTableFrame(refArray);
		}
	}

	/**
	 *
	 */
	public void reloadData(ActionEvent evt)
	{
		Graph graph = (Graph) canvas.getGraphicElement();
		GraphicElement[] plots = GraphUtils.getElements(graph, Plot.class);
		if(plots == null)
		{
			return;
		}
		for(int i = 0; i < plots.length; i++)
		{
			Plot plot = (Plot) plots[i];
			GraphicElement[] curves = GraphUtils.getElements(plot, Curve.class);
			if(curves == null)
			{
				continue;
			}
			for(int k = 0; k < curves.length; k++)
			{
				Curve crv = (Curve) curves[k];
				CurveDataModel cdm = crv.getModel();
				DataReference ref = (DataReference) cdm.getReferenceObject();
				ref.reloadData();
				crv.setModel(cdm);
			}
		}
		canvas.redoNextPaint();
		repaint();
	}

	/**
	 * sets graph in canvas
	 */
	public void setGraph(Graph graph)
	{
		new AttributeSerializer(graph).load("graphPropertiesFile");
		canvas.setGraphicElement(graph);
		addInteractors(graph);
		canvas.redoNextPaint();
		canvas.paint(canvas.getGraphics());
	}

	public void addZoomControlBar(JFrame fr)
	{
		_zi.addZoomControlToolbar(fr);
	}

	/**
	 * creates a graph attribute editing dialog
	 */
	private class EditListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			new GETreeDialog((Frame) SwingUtilities.getAncestorOfClass(
					Frame.class, DataSetGraph.this), getCanvas());
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
				_attrS = new AttributeSerializer(canvas
						.getGraphicElement());
			}
			if(_attrS.loadAttributes())
			{
				canvas.redoNextPaint();
				canvas.repaint();
			}
		}

		/**
		 *
		 */
		private void saveAttributes()
		{
			if(_attrS == null)
			{
				_attrS = new AttributeSerializer(canvas
						.getGraphicElement());
			}
			_attrS.saveAttributes();
		}
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: DataGraph.java,v 1.1 2003/10/02 20:48:25 redwood Exp $
	 */
	private class FontResizeListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			JCheckBoxMenuItem mi = (JCheckBoxMenuItem) evt.getSource();
			if(_ri != null)
			{
				_ri.setDoResize(mi.isSelected());
			}
		}
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: DataGraph.java,v 1.1 2003/10/02 20:48:25 redwood Exp $
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
			GraphicElement ge = canvas.getGraphicElement();
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
			canvas.redoNextPaint();
			canvas.paint(canvas.getGraphics());
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
	 *
	 */
	class FlagEditorListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Graph graph = (Graph) canvas.getGraphicElement();
			GEContainer curveContainer = graph.getPlot().getCurveContainer();
			GraphicElement[] curves = curveContainer.getElements(Curve.class);
			if(curves == null || curves[0] == null)
			{
				throw new RuntimeException("No curves in selection");
			}
			Curve curve = (Curve) curves[0];
			new vista.app.FlagEditor(canvas, curve, true);
		}
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: DataGraph.java,v 1.1 2003/10/02 20:48:25 redwood Exp $
	 */
	private class DisplayCoordinateListener implements ActionListener
	{
		private CoordinateDisplayInteractor _cdi;

		public void actionPerformed(ActionEvent evt)
		{
			JCheckBoxMenuItem mi = (JCheckBoxMenuItem) evt.getSource();
			if(mi.isSelected())
			{
				_cdi = new CoordinateDisplayInteractor(getCanvas());
				getCanvas().addMouseMotionListener(_cdi);
			}
			else
			{
				if(_cdi != null)
				{
					getCanvas().removeMouseMotionListener(_cdi);
				}
				_cdi.doneDisplaying();
			}
		}
	} // end of Displa....

}
