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

package gov.ca.water.scenario.presentation;

import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import javax.swing.*;

import gov.ca.water.businessservice.impl.XMLParsingSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import org.apache.batik.script.Window;
import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.JSVGScrollPane;
import org.apache.batik.swing.gvt.GVTTreeRendererAdapter;
import org.apache.batik.swing.gvt.GVTTreeRendererEvent;
import org.apache.batik.swing.svg.SVGLoadEventDispatcherAdapter;
import org.apache.batik.swing.svg.SVGLoadEventDispatcherEvent;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;

/**
 * SchematicMain: Class to handle display of SVG-formatted schematic view.
 *
 * @author tslawecki
 */
public class SchematicMain
{

	private static final Logger LOG = Logger.getLogger(SchematicMain.class.getName());
	private final DisplayHelper _displayHelper;
	JSVGCanvas canvas;
	AffineTransform theAT;
	Document document;
	Window window;
	SwingEngine swix;
	JSVGScrollPane scrollPane;
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	private SwingEngine swingEngine = XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine();

	/**
	 *
	 *
	 */
	/**
	 * @param p    Housing panel
	 * @param url  URL for SVG file
	 * @param swix Handle to UI
	 * @param m0   -m5 Affine Transformation values
	 * @param m0
	 * @param m1
	 * @param m2
	 * @param m3
	 * @param m4
	 * @param m5
	 */
	public SchematicMain(JPanel p, String url, SwingEngine swix, double m0, double m1, double m2, double m3, double m4,
						 double m5)
	{
		_displayHelper = new DisplayHelper(p);
		this.swix = swix;
		try
		{
			theAT = new AffineTransform(m0, m1, m2, m3, m4, m5);
			canvas = new JSVGCanvas();
			// Forces the canvas to always be dynamic even if the current
			// document does not contain scripting or animation.
			canvas.setDocumentState(JSVGCanvas.ALWAYS_DYNAMIC);
			canvas.setEnablePanInteractor(true);
			canvas.setEnableZoomInteractor(true);
			canvas.setURI(url);
			canvas.addSVGLoadEventDispatcherListener(new SVGLoadEventDispatcherAdapter()
			{
				@Override
				public void svgLoadEventDispatchStarted(SVGLoadEventDispatcherEvent e)
				{
					// At this time the document is available...
					document = canvas.getSVGDocument();
					// ...and the window object too.
					window = canvas.getUpdateManager().getScriptingEnvironment().createWindow();
					// Registers the listeners on the document
					// just before the SVGLoad event is
					// dispatched.
					registerListeners();
					// It is time to pack the frame.
				}
			});
			canvas.addGVTTreeRendererListener(new GVTTreeRendererAdapter()
			{
				@Override
				public void gvtRenderingCompleted(GVTTreeRendererEvent e)
				{
					super.gvtRenderingCompleted(e);
					canvas.setRenderingTransform(theAT, true);
				}
			});
			scrollPane = new JSVGScrollPane(canvas);
			scrollPane.setSize(400, 400);
			scrollPane.setAutoscrolls(true);
			p.add(scrollPane);
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display schematic.";
			errorHandlingSvc.businessErrorHandler(messageText, e);
		}

	}

	public void registerListeners()
	{
		// Gets an element from the loaded document.
		NodeList elementsByTagName = document.getElementsByTagName("g");
		for(int i = 0; i < elementsByTagName.getLength(); i++)
		{
			Element elt = (Element) elementsByTagName.item(i);
			EventTarget t = (EventTarget) elt;
			// Adds a 'onclick' listener
			t.addEventListener("click", new OnClickAction(), false);
		}
	}

	public class OnClickAction implements EventListener
	{
		@Override
		public void handleEvent(Event evt)
		{
			// Perform some actions here...
			// theAT = new AffineTransform(4.0, 0, 0.0, 4.0, -1400.0, -200.0);
			evt.stopPropagation();
			// ...for example schedule an action for later:
			window.setTimeout(new DisplayClickedLabelTask(evt), 500);
		}
	}

	public class DisplayClickedLabelTask implements Runnable
	{
		private final Event evt;

		public DisplayClickedLabelTask(Event evt)
		{
			this.evt = evt;
		}

		@Override
		public void run()
		{
			String label = null;
			Element el = ((Element) evt.getTarget());
			String tag = el.getTagName();
			LOG.debug("Clicked on: " + evt.getTarget() + " " + tag);
			Element pel = el;
			// Get first text element in containing group
			while(label == null && pel.getParentNode() != null && pel.getParentNode() instanceof Element)
			{
				pel = (Element) pel.getParentNode();
				String ptag = pel.getTagName();
				if(ptag.equals("g"))
				{
					// When first group is found, look for first text element
					NodeList childNodes = pel.getChildNodes();
					for(int i = 0; (label == null) && (i < childNodes.getLength()); i++)
					{
						Node item = childNodes.item(i);
						if(item instanceof Element)
						{
							Element ce = (Element) item;
							LOG.debug("ce = tag:" + ce.getTagName() + " content: " + ce.getTextContent());
							if(ce.getTagName().startsWith("text"))
							{
								label = ce.getTextContent();
							}
						}
					}
				}
			}
			if(label == null)
			{
				Toolkit.getDefaultToolkit().beep();
			}
			else
			{
				JList lstScenarios = (JList) swix.find("SelectedList");
				if(lstScenarios.getModel().getSize() == 0)
				{
					ImageIcon icon = new ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
					Object[] options = {"OK"};
					JOptionPane optionPane = new JOptionPane("No scenarios loaded", JOptionPane.ERROR_MESSAGE,
							JOptionPane.OK_OPTION, null, options, options[0]);
					JDialog dialog = optionPane.createDialog(swingEngine.find(Constant.MAIN_FRAME_NAME), "CalLite");
					dialog.setIconImage(icon.getImage());
					dialog.setResizable(false);
					dialog.setVisible(true);
				}
				else
				{
					ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
					EpptScenarioRun baseScenario = projectConfigurationPanel.getBaseScenario();
					if(baseScenario != null)
					{
						List<EpptScenarioRun> alternatives = projectConfigurationPanel.getEpptScenarioAlternatives();
						PlotConfigurationState plotConfigurationState = projectConfigurationPanel.plotConfigurationState();
						LocalDate startMonth = projectConfigurationPanel.getStartMonth();
						LocalDate endMonth = projectConfigurationPanel.getEndMonth();
						_displayHelper.showDisplayFrames(plotConfigurationState, Collections.singletonList(label), baseScenario, alternatives,
								startMonth, endMonth);
					}
				}
			}
		}
	}
}
