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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;
import javax.swing.*;

import gov.ca.water.businessservice.IAllButtonsDele;
import gov.ca.water.businessservice.IDynamicControlSvc;
import gov.ca.water.businessservice.IScenarioSvc;
import gov.ca.water.businessservice.ITableSvc;
import gov.ca.water.businessservice.impl.AllButtonsDeleImp;
import gov.ca.water.businessservice.impl.DynamicControlSvcImpl;
import gov.ca.water.businessservice.impl.ScenarioSvcImpl;
import gov.ca.water.businessservice.impl.TableSvcImpl;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.ProgressFrameSwingWorker;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-27-2019
 */
public class ProgressFrameSwingWorkerWsidi extends ProgressFrameSwingWorker
{
	private static final Logger LOG = Logger.getLogger(ProgressFrameSwingWorkerWsidi.class.getName());
	private final SwingEngine _swingEngine;
	private final IAllButtonsDele _allButtonsDeleImp = new AllButtonsDeleImp();
	private final ITableSvc _tableSvc = TableSvcImpl.getTableSvcImplInstance();
	private final IScenarioSvc _scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();
	private final IDynamicControlSvc _dynamicControlSvc = DynamicControlSvcImpl.getDynamicControlSvcImplInstance();

	public ProgressFrameSwingWorkerWsidi(SwingEngine swingEngine, Consumer<String> statusUpdater,
										 Consumer<String[]> listUpdater)
	{
		super(statusUpdater, listUpdater);
		_swingEngine = swingEngine;
	}

	protected boolean processScenario(List<String> data, List<Path> scenariosToDrop,
									  String scenarioName)
	{
		boolean sleepAfterDisplay;
		if(Constant.BATCH_RUN_WSIDI.equalsIgnoreCase(getScenarioNamesAndAction().get(scenarioName)))
		{
			sleepAfterDisplay = processBatchWsidiRun(data, scenariosToDrop,
					scenarioName);
		}
		else
		{
			sleepAfterDisplay = super.processScenario(data, scenariosToDrop, Paths.get(scenarioName));
		}
		return sleepAfterDisplay;
	}

	private boolean processBatchWsidiRun(List<String> data, List<Path> scenariosToDrop,
										 String scenarioName)
	{
		boolean sleepAfterDisplay = false;
		final String text;
		text = getMonitorSvc().runWSIDI(scenarioName);
		data.add(text);
		if(text.toLowerCase()
			   .endsWith("(wsidi iteration " + getProperties().getProperty(
					   "wsidi.iterations") + "/"
					   + getProperties().getProperty("wsidi.iterations")
					   + ")  - DONE - run completed".toLowerCase()))
		{
			sleepAfterDisplay = true;
			loadGeneratedWSIDI(scenarioName);
			scenariosToDrop.add(Paths.get(scenarioName));
		}
		return sleepAfterDisplay;
	}

	/**
	 * After the WSIDI batch runs this method is called to load the tables in
	 * the "Operations" tab.
	 *
	 * @param scenarioName The scenario file name which the batch program is run. the
	 *                     name should not have any extension.
	 */
	private void loadGeneratedWSIDI(String scenarioName)
	{
		String wsiDiSwpPath = Paths.get(Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR + Constant.LOOKUP_DIR
				+ Constant.SWP_START_FILENAME + Constant.TABLE_EXT).toString();
		String wsiDiCvpSwpPath = Paths.get(Constant.RUN_DETAILS_DIR + scenarioName + Constant.RUN_DIR
				+ Constant.LOOKUP_DIR + Constant.CVP_START_FILENAME + Constant.TABLE_EXT).toString();
		try
		{
			DataTableModel swpDtm = _tableSvc.getWsiDiTable(wsiDiSwpPath);
			swpDtm.setCellEditable(true);
			swpDtm.setTableName(Constant.SWP_START_FILENAME);
			DataTableModel cvpDtm = _tableSvc.getWsiDiTable(wsiDiCvpSwpPath);
			cvpDtm.setCellEditable(true);
			cvpDtm.setTableName(Constant.CVP_START_FILENAME);
			_scenarioSvc.addUserDefinedTable(Constant.SWP_START_FILENAME, swpDtm);
			_scenarioSvc.addUserDefinedTable(Constant.CVP_START_FILENAME, cvpDtm);
			/*
			 * The following code we are sedtting the SWP and CVP file names as
			 * user defined because the table values we are getting after the
			 * batch run we consider them as user defined.
			 */
			_tableSvc.setWsidiForSWPFullFileName(Constant.USER_DEFINED);
			_tableSvc.setWsidiForCVPFullFileName(Constant.USER_DEFINED);
			JComponent component = (JComponent) _swingEngine.find("op_btn1");
			_allButtonsDeleImp.editButtonOnOperations(component);
			List<String> value = _dynamicControlSvc.getLabelAndGuiLinks4BOBasedOnTheRadioButtons(_swingEngine);
			JLabel jLabel = (JLabel) _swingEngine.find("op_WSIDI_Status");
			jLabel.setText(value.get(1) + " (Generated via "
					+ Integer.parseInt(getProperties().getProperty("wsidi.iterations")) + " iterations)");
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex);
		}
	}
}
