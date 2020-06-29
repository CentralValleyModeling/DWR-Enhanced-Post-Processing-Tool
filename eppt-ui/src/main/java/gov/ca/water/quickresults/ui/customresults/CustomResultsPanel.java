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

package gov.ca.water.quickresults.ui.customresults;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionListener;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;
import javax.swing.*;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import calsim.gui.GeneralRetrievePanel;
import calsim.gui.GuiUtils;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.calgui.presentation.WRIMSGUILinks;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IDialogSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.calgui.presentation.plotly.EpptPlotException;
import gov.ca.water.quickresults.ui.quickresults.PlotConfigurationStateBuilder;
import gov.ca.water.quickresults.ui.quickresults.QuickResultsPanel;
import org.apache.log4j.Logger;
import vista.set.DataReference;
import vista.set.Group;
import vista.set.Pathname;
import vista.time.TimeWindow;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-25-2019
 */
public class CustomResultsPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanel.class.getName());
	private static final String CUSTOM_RESULTS_XML_FILE = "Custom_Results.xml";
	private final IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();
	private final DisplayHelper _displayHelper;
	private final EpptConfigurationController _epptConfigurationController;

	public CustomResultsPanel(EpptConfigurationController epptConfigurationController)
	{
		try
		{
			_epptConfigurationController = epptConfigurationController;
			_displayHelper = new DisplayHelper(this, _epptConfigurationController);
			super.setLayout(new BorderLayout());
			Container swixmlCustomResultsPanel = renderSwixml(CUSTOM_RESULTS_XML_FILE);
			super.add(swixmlCustomResultsPanel);
			WRIMSGUILinks.buildWRIMSGUI((JPanel) getSwingEngine().find("WRIMS"));
			JButton retrieveBtn = GuiUtils.getCLGPanel().getRetrievePanel().getRetrieveBtn();
			for(ActionListener al : retrieveBtn.getActionListeners())
			{
				retrieveBtn.removeActionListener(al);
			}
			retrieveBtn.addActionListener(arg0 -> retrieve());
			JButton filterBtn = GuiUtils.getCLGPanel().getRetrievePanel().getFilterBtn();
			for(ActionListener al : filterBtn.getActionListeners())
			{
				filterBtn.removeActionListener(al);
			}
			filterBtn.addActionListener(arg0 -> filter());
			Component secondTab = ((JTabbedPane) GuiUtils.getCLGPanel().getComponent(0)).getComponentAt(1);
			Component openButtonComponent = findFirstButtonMatchingText(secondTab, "Display");
			if(openButtonComponent != null)
			{
				JButton openButton = (JButton) openButtonComponent;
				for(ActionListener al : openButton.getActionListeners())
				{
					openButton.removeActionListener(al);
				}
				openButton.addActionListener(arg0 -> retrieve2());
			}
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + CUSTOM_RESULTS_XML_FILE, e);
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Helper function that scans GUI for a button with the indicated label
	 * starting with a given component. Used in CalLite GUI to find the "Open"
	 * button on the CLG panel and replace the associated action.
	 *
	 * @param comp Starting component. Function recurses through childre.n
	 * @param text Text to match on JButton
	 * @return JButton component with specified label
	 */
	private static Component findFirstButtonMatchingText(Component comp, String text)
	{

		if((comp instanceof JButton) && ((JButton) comp).getText().equals(text))
		{
			return comp;
		}

		if(comp instanceof Container)
		{
			Container container = (Container) comp;
			for(int i = 0; i < container.getComponentCount(); i++)
			{
				Component comp2 = findFirstButtonMatchingText(container.getComponent(i), text);
				if(comp2 != null)
				{
					return comp2;
				}
			}
		}
		return null;
	}

	private void filter()
	{
		List<EpptScenarioRun> epptScenarioAlternatives = _epptConfigurationController.getEpptScenarioAlternatives();
		Optional<EpptScenarioRun> baseScenario = _epptConfigurationController.getEpptScenarioBase();
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>(epptScenarioAlternatives);
		baseScenario.ifPresent(scenarioRuns::add);
		if(!scenarioRuns.isEmpty())
		{
			List<DataReference> allrefs = scenarioRuns.stream().map(this::loadRecordsForScenario).flatMap(Collection::stream).collect(toList());
			GeneralRetrievePanel retrievePanel = GuiUtils.getCLGPanel().getRetrievePanel();
			if(allrefs.isEmpty())
			{
				_dialogSvc.getOK("No records found using filter", JOptionPane.WARNING_MESSAGE);
			}
			else
			{
				retrievePanel.updateTable(allrefs.toArray(new DataReference[0]));
			}
		}
		else
		{
			_dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
		}

	}

	private List<DataReference> loadRecordsForScenario(EpptScenarioRun epptScenarioRun)
	{
		List<Path> allDssFiles = epptScenarioRun.getDssContainer().getAllDssFiles()
												.stream()
												.filter(Objects::nonNull)
												.map(NamedDssPath::getDssPath)
												.filter(Objects::nonNull)
												.collect(toList());
		List<DataReference> allrefs = new ArrayList<>();
		GeneralRetrievePanel retrievePanel = GuiUtils.getCLGPanel().getRetrievePanel();
		TimeWindow timeWindow = AppUtils.getCurrentProject().getTimeWindow();
		for(Path path : allDssFiles)
		{
			String[] stringParts = retrievePanel.getStringParts();
			Group dssGroup = AppUtils.openDSSFile(path.toString());
			if(dssGroup != null)
			{
				Group gc = Group.createGroup(dssGroup);
				DataReference[] refs = AppUtils.createRefs(stringParts, timeWindow, gc);
				if(refs != null)
				{
					allrefs.addAll(Arrays.asList(refs));
				}
			}
		}
		return allrefs;
	}

	@Override
	public String getJavaHelpId()
	{
		return "3.3_CustomResults.htm";
	}

	/**
	 * Data retrieval for single DSS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */
	private void retrieve()
	{
		try
		{
			Optional<EpptScenarioRun> epptScenarioBase = _epptConfigurationController.getEpptScenarioBase();
			if(!epptScenarioBase.isPresent())
			{
				_dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
				return;
			}
			String noRowsString = "";
			JTable table = GuiUtils.getCLGPanel().getRetrievePanel().getTable();
			if(table.getRowCount() == 0)
			{
				noRowsString = " after using \"Filter\" to load variables";
			}
			Group group = GuiUtils.getCLGPanel().getRetrievePanel().getGroup();
			if(group == null || table.getSelectedRowCount() == 0)
			{
				_dialogSvc.getOK("Variables not selected! Select one or more variables" + noRowsString,
						JOptionPane.INFORMATION_MESSAGE);
				return;
			}
			// checked if count > 0 above
			List<GUILinksAllModelsBO> collect = IntStream.of(table.getSelectedRows())
														 .mapToObj(row -> createPathnameForRow(table, row))
														 .map(this::mapToGuiLink)
														 .collect(toList());
			PlotConfigurationState plotConfigurationState = new PlotConfigurationStateBuilder(getSwingEngine()).createPlotConfigurationState();
			_displayHelper.showDisplayFramesGuiLink(plotConfigurationState, collect);
		}
		catch(RuntimeException | EpptPlotException e)
		{
			LOGGER.debug("Error in retrieve() -", e);
		}
	}

	private Pathname createPathnameForRow(JTable table, int row)
	{
		return Pathname.createPathname(new String[]
				{
						table.getModel().getValueAt(row, 1).toString(),
						table.getModel().getValueAt(row, 2).toString(),
						table.getModel().getValueAt(row, 3).toString(),
						table.getModel().getValueAt(row, 4).toString(),
						table.getModel().getValueAt(row, 5).toString(),
						table.getModel().getValueAt(row, 6).toString()
				});
	}

	private GUILinksAllModelsBO mapToGuiLink(Pathname path)
	{
		String bAndCPart = path.getPart(Pathname.B_PART) + "/" + path.getPart(Pathname.C_PART);
		GUILinksAllModelsBO guiLinksAllModelsBO = new GUILinksAllModelsBO(path.getFullPath(), path.getFullPath(),
				path.getPart(Pathname.B_PART), bAndCPart, "");
		GUILinksAllModelsBO.Model.values().forEach(
				m -> guiLinksAllModelsBO.addModelMapping(m.toString(), bAndCPart, ""));
		return guiLinksAllModelsBO;
	}

	/**
	 * Data retrieval for DTS/MTS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */

	private void retrieve2()
	{

		DerivedTimeSeries dts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getDTS();
		MultipleTimeSeries mts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getMTS();

		if(((mts == null) && (dts == null)) || ((dts != null) && (dts.getBParts().isEmpty()))
				|| ((mts != null) && (mts.getNumberOfDataReferences() < 1)))
		{
			_dialogSvc.getOK("Nothing to display! Specify DTS or MTS data reference", JOptionPane.WARNING_MESSAGE);

			return;
		}

		try
		{
			Optional<EpptScenarioRun> baseScenario = _epptConfigurationController.getEpptScenarioBase();
			if(baseScenario.isPresent())
			{
				PlotConfigurationState plotConfigurationState = new PlotConfigurationStateBuilder(getSwingEngine()).createPlotConfigurationState();
				_displayHelper.showDisplayFramesWRIMS(plotConfigurationState, dts, mts);
			}
			else
			{
				_dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
			}

		}
		catch(RuntimeException | EpptPlotException e)
		{
			LOGGER.debug("Error in retrieve2() -", e);
		}
	}
}
