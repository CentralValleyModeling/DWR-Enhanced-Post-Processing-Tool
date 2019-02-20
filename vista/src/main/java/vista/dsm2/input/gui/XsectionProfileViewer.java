package vista.dsm2.input.gui;

import java.awt.Color;
import java.awt.Font;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.JFrame;

import gov.ca.dsm2.input.csdp.CDNReader;
import gov.ca.dsm2.input.model.Channel;
import gov.ca.dsm2.input.model.Channels;
import gov.ca.dsm2.input.model.DSM2Model;
import gov.ca.dsm2.input.model.XSection;
import gov.ca.dsm2.input.model.XSectionProfile;
import gov.ca.dsm2.input.model.calculator.ModelUtils;
import gov.ca.dsm2.input.parser.Parser;
import gov.ca.dsm2.input.parser.Tables;
import vista.app.CurveFactory;
import vista.app.DataGraphFrame;
import vista.app.GraphBuilderInfo;
import vista.app.MainProperties;
import vista.graph.AxisAttr;
import vista.graph.ColorChoice;
import vista.graph.Curve;
import vista.graph.CurveAttr;
import vista.graph.DefaultGraphFactory;
import vista.graph.GECanvas;
import vista.graph.Graph;
import vista.graph.GraphAttr;
import vista.graph.Legend;
import vista.graph.LegendItem;
import vista.graph.LegendItemAttr;
import vista.graph.MultiPlot;
import vista.graph.Plot;
import vista.graph.PlotAttr;
import vista.graph.SimpleTickGenerator;
import vista.graph.Symbol;
import vista.graph.SymbolAttr;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;
import vista.graph.TickGenerator;
import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.DefaultDataSet;
import vista.set.DefaultReference;

public class XsectionProfileViewer {
	public static void main(String[] args) throws Exception {
		String xsectionFile = args[0];
		Parser p = new Parser();
		Tables tables = p.parseModel(xsectionFile);
		DSM2Model dsm2Model = tables.toDSM2Model();
		// update model with CDN information from CSDP
		String cdnFile = args[1];
		CDNReader cdnReader = new CDNReader(cdnFile);
		cdnReader.readAndUpdateModel(dsm2Model);
		//
		Channels channels = dsm2Model.getChannels();
		for (Channel c : channels.getChannels()) {
			if (!c.getId().equals("126")){
				continue;
			}
			ArrayList<XSection> xsections = c.getXsections();
			plot(xsections);
		}
	}
	
	public static DataReference createDataReference(XSectionProfile xp, boolean calculated){
		List<double[]> profilePoints = xp.getProfilePoints();

		double[] x = new double[profilePoints.size()];
		double[] y = new double[profilePoints.size()];
		for (int i = 0; i < profilePoints.size(); i++) {
			x[i] = profilePoints.get(i)[0];
			y[i] = profilePoints.get(i)[1];
		}
		DefaultDataSet set = new DefaultDataSet("XSection Profile-" + (calculated ? "calculated": "from-csdp") + xp.getChannelId() + ":" + xp.getDistance(), x, y);
		return new DefaultReference(set);
	}
	

	public static void plot(ArrayList<XSection> xsections) {
		DefaultGraphFactory factory = new DefaultGraphFactory();
		Graph graph = factory.createGraph();
		graph.setBackgroundColor(Color.WHITE);
		MultiPlot multiPlot = factory.createMultiPlot();
		graph.addPlot(multiPlot);
		multiPlot.add(factory.createPlot());
		multiPlot.setCurrentPlot(0);
		TextLineAttr tla = new TextLineAttr();
		tla._font = new Font("Times Roman", Font.PLAIN, 16);
		tla._foregroundColor = Color.blue;
		((GraphAttr) (graph.getAttributes())).setTitleAttributes(tla);

		graph.setInsets(new java.awt.Insets(5, 20, 10, 25));
		Font legendFont = new Font("Arial", Font.PLAIN, 14);
		Legend legend = factory.createLegend();
		graph.setLegend(legend);

		Plot plot = graph.getPlot();
		plot.setInsets(new java.awt.Insets(20, 5, 20, 50));

		// now add dataReferences
		int ncurves = xsections.size();
		DataReference[] refs = new DataReference[ncurves*2];
		for(int i=0; i < ncurves; i++){
			XSectionProfile xpc = ModelUtils.calculateProfileFrom(xsections.get(i), true);
			XSectionProfile xp = xsections.get(i).getProfile();
			refs[2*i] = createDataReference(xp,false);
			refs[2*i+1] = createDataReference(xpc,true);
		}

		GraphBuilderInfo info = new GraphBuilderInfo(refs, MainProperties.getProperties());
		Color[] colors = new Color[]{Color.BLUE, Color.CYAN, Color.GRAY, Color.GREEN, Color.ORANGE, Color.YELLOW, Color.RED};
		for (int i = 0; i < refs.length; i++) {
			int xPos = info.getXAxisPosition(refs[i]);
			int yPos = info.getYAxisPosition(refs[i]);
			// DataSet ds = refs[i].getData();
			Curve crv = CurveFactory.createCurve(refs[i], xPos, yPos, info.getLegendLabel(refs[i]));
			plot.addCurve(crv);
			plot.setBackgroundColor(Color.WHITE);
			CurveAttr c = (CurveAttr) crv.getAttributes();
			c.setDrawSymbol(true);
			c.setDrawLines(true);
			c.setForegroundColor(colors[(i/2)%colors.length]);
			crv.setAttributes(c);
			SymbolAttr networkSymbolAttr = new SymbolAttr();
			int ps = 4;
			int[] networkSymbolX = { -ps, ps, ps, -ps };
			int[] networkSymbolY = { ps, ps, -ps, -ps };
			networkSymbolAttr.setIsFilled(true);
			networkSymbolAttr.setSymbol(networkSymbolX, networkSymbolY, 4);
			Symbol networkSymbol = new Symbol(networkSymbolAttr);
			networkSymbol.setForegroundColor(c.getForegroundColor());
			crv.setSymbol(networkSymbol);
			//
			LegendItem li = factory.createLegendItem();
			li.setLegendName(refs[i].getName());
			li.setCurve(crv);
			LegendItemAttr lia = (LegendItemAttr) li.getAttributes();
			legend.add(li);
			lia._foregroundColor = Color.black;
			lia.setFont(legendFont);
		}

		// Scale components in layout resizing: Template file
		// GEBorderLayout plotLayout = (GEBorderLayout) plot.getLayout();
		// plotLayout.setScaleComponents(true);
		TextLineAttr dateAttr = new TextLineAttr();
		dateAttr._font = new java.awt.Font("Times Roman", java.awt.Font.PLAIN, 10);
		dateAttr._foregroundColor = java.awt.Color.red;
		dateAttr._resizeProportionally = true;
		dateAttr._justification = TextLineAttr.RIGHT;
		graph.getLegend().add(new TextLine(dateAttr, new Date().toString()));

		// _gC = new GraphCanvas();
		GECanvas gC = new GECanvas(graph);

		graph.setInsets(new Insets(5, 5, 5, 5));

		PlotAttr pattr = (PlotAttr) plot.getAttributes();

		TickGenerator tg = new SimpleTickGenerator();

		plot.getAxis(AxisAttr.BOTTOM).setTickGenerator(tg);
		plot.getAxis(AxisAttr.LEFT).setTickGenerator(tg);

		AxisAttr aa = (AxisAttr) plot.getAxis(AxisAttr.BOTTOM).getAttributes();
		aa._tickLocation = AxisAttr.INSIDE;
		aa = (AxisAttr) plot.getAxis(AxisAttr.LEFT).getAttributes();
		aa._tickLocation = AxisAttr.BOTH;

		graph.addGrid();

		// use this for Frame
		// add("Center", _gC);
		// use instead for JFrame
		DataGraphFrame fr = new DataGraphFrame(graph, true);
	}

}
