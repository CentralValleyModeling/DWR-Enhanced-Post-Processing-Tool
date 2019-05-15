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

package gov.ca.water.calgui.presentation;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.function.Consumer;
import javax.swing.*;

import gov.ca.water.calgui.busservice.IMonitorSvc;
import gov.ca.water.calgui.busservice.impl.MonitorSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-27-2019
 */
public class ProgressFrameSwingWorker extends SwingWorker<Void, String>
{
	private static final Logger LOG = Logger.getLogger(ProgressFrameSwingWorker.class.getName());
	private final Map<Path, String> _scenarioNamesAndAction = new HashMap<>();
	private final Properties _properties = new Properties();
	private final Consumer<String> _statusUpdater;
	private final Consumer<String[]> _listUpdater;
	private IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();
	private IMonitorSvc _monitorSvc = new MonitorSvcImpl();

	private String[] _oldValue;

	public ProgressFrameSwingWorker(Consumer<String> statusUpdater, Consumer<String[]> listUpdater)
	{
		_statusUpdater = statusUpdater;
		_listUpdater = listUpdater;
		try
		{
			_properties.load(
					ProgressFrameSwingWorker.class.getClassLoader().getResourceAsStream("callite-gui.properties"));
		}
		catch(IOException e)
		{
			LOG.debug("Problem loading properties. " + e.getMessage(), e);
		}
	}

	protected Properties getProperties()
	{
		return _properties;
	}

	protected IMonitorSvc getMonitorSvc()
	{
		return _monitorSvc;
	}

	protected Map<Path, String> getScenarioNamesAndAction()
	{
		return _scenarioNamesAndAction;
	}

	@Override
	protected Void doInBackground()
	{
		try
		{
			while(true)
			{
				if(isCancelled())
				{
					return null;
				}
				Thread.sleep(2000);
				boolean sleepAfterDisplay = false;
				String[] listData;
				List<String> data = new ArrayList<>();
				List<Path> scenariosToDrop = new ArrayList<>();
				if(_scenarioNamesAndAction.isEmpty())
				{
					listData = new String[1];
					listData[0] = "No active scenarios";
					_statusUpdater.accept(Constant.STATUS_BTN_TEXT_CLOSE);
				}
				else
				{
					for(Path scenarioName : _scenarioNamesAndAction.keySet())
					{
						sleepAfterDisplay = processScenario(data, scenariosToDrop, scenarioName);
					}
					for(Path s : scenariosToDrop)
					{
						_scenarioNamesAndAction.remove(s);
					}

					listData = new String[data.size()];
					for(int i = 0; i < data.size(); i++)
					{
						listData[i] = data.get(i);
					}
				}
				if(!Arrays.equals(_oldValue, listData))
				{
					_listUpdater.accept(listData);
					_oldValue = listData;
				}
				if(sleepAfterDisplay)
				{
					Thread.sleep(2000);
				}
			}
		}
		catch(Exception e)
		{
			if(!(e instanceof InterruptedException))
			{
				LOG.error(e.getMessage());
				String messageText = "Unable to display progress frame.";
				_errorHandlingSvc.businessErrorHandler(messageText, e);
			}
		}
		return null;
	}

	protected boolean processScenario(List<String> data, List<Path> scenariosToDrop,
									  Path scenarioName)
	{
		boolean sleepAfterDisplay = false;
		if(Constant.SAVE.equalsIgnoreCase(_scenarioNamesAndAction.get(scenarioName)))
		{
			sleepAfterDisplay = save(data, scenariosToDrop, scenarioName);
		}
		else if(Constant.BATCH_RUN.equalsIgnoreCase(_scenarioNamesAndAction.get(scenarioName)))
		{

			sleepAfterDisplay = processBatchRun(data, scenariosToDrop,
					scenarioName);
		}
		return sleepAfterDisplay;
	}

	private boolean save(List<String> data, List<Path> scenariosToDrop,
						 Path scenarioName)
	{
		boolean sleepAfterDisplay = false;
		String text;
		text = _monitorSvc.save(scenarioName);
		data.add(text);
		if(text.endsWith("Save is completed."))
		{
			sleepAfterDisplay = true;
			scenariosToDrop.add(scenarioName);
		}
		return sleepAfterDisplay;
	}

	private boolean processBatchRun(List<String> data, List<Path> scenariosToDrop,
									Path scenarioName)
	{
		boolean sleepAfterDisplay = false;
		final String text;
		text = _monitorSvc.runModel(scenarioName);
		data.add(text);
		if(text.toLowerCase().contains("done - run c".toLowerCase()))
		{
			LOG.info(text);
			sleepAfterDisplay = true;
			scenariosToDrop.add(scenarioName);
		}
		return sleepAfterDisplay;
	}

	void addScenario(Path key, String type)
	{
		_scenarioNamesAndAction.put(key, type);
	}

	void clearScenarios()
	{
		_scenarioNamesAndAction.clear();
	}
}
