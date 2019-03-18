/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice.impl;

import java.awt.Component;
import java.awt.Container;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import gov.ca.water.bo.GUILinks2BO;
import gov.ca.water.bo.ScenarioDisplayBO;
import gov.ca.water.businessservice.IScenarioDele;
import gov.ca.water.businessservice.IScenarioSvc;
import gov.ca.water.businessservice.ISeedDataSvc;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.constant.Constant;
import org.apache.commons.io.FilenameUtils;
import org.swixml.SwingEngine;

/**
 * This class is used to show the Scenario state of the application and also
 * compare to the saved files if provided.
 *
 * @author Mohan
 */
public class ScenarioDeleImp implements IScenarioDele
{
	private final IScenarioSvc _scenarioSvc = ScenarioSvcImpl.getScenarioSvcImplInstance();
	private final ISeedDataSvc _seedDataSvc = SeedDataSvcImpl.getSeedDataSvcImplInstance();

	@Override
	public List<DataTableModel> getScenarioTableData(List<String> fileNames, SwingEngine swingEngine)
			throws EpptInitializationException
	{
		if(fileNames == null)
		{
			fileNames = new ArrayList<>();
		}
		List<DataTableModel> dtmList = new ArrayList<>();
		if(fileNames.size() > 1)
		{
			DataTableModel comparisonDTM = buildScenarioTables(fileNames, swingEngine);
			DataTableModel baseDTM = builsBaseScenarioTables(comparisonDTM);
			DataTableModel differenceDTM = differenceScenarioTables(comparisonDTM);
			dtmList.add(baseDTM);
			dtmList.add(comparisonDTM);
			dtmList.add(differenceDTM);
		}
		else
		{
			dtmList.add(buildScenarioTables(fileNames, swingEngine));
		}
		return dtmList;
	}

	/**
	 * This method will build the Map's for all the files and then convert them
	 * into the {@link DataTableModel}.
	 *
	 * @param fileNames Names of the cls file.
	 * @return The Object of {@link DataTableModel} which has the comparison for
	 * all the files which are passed in from the @param fileName. If
	 * there is only one file name then it will give the base one.
	 */
	private DataTableModel buildScenarioTables(List<String> fileNames, SwingEngine swingEngine)
			throws EpptInitializationException
	{
		List<Map<String, String>> clsMapList = new ArrayList<>();
		String[] columnNames = new String[fileNames.size() + 1];
		columnNames[0] = "Dashboard Options";
		int index = 1;
		// This will take the file names and get the Map of each cls file.
		for(String fileName : fileNames)
		{
			clsMapList.add(buildMapOfCLSFile(fileName, swingEngine, _seedDataSvc.getTableIdMap()));
			columnNames[index] = FilenameUtils.removeExtension(fileName);
			index++;
		}
		/*
		 * This will take all map's of the cls file and make a linear structure
		 * of the file. we are using List because we don't know the no of rows.
		 * It may vary based on file.
		 */
		List<Object[]> data = new ArrayList<>();
		Map<String, String> firstMap = clsMapList.get(0);
		for(String key : firstMap.keySet())
		{
			Object[] temp = new Object[columnNames.length];
			if(key.startsWith(Constant.EMPTY))
			{
				temp[0] = key.replace(Constant.EMPTY + Constant.DASH, " ");
			}
			else
			{
				temp[0] = key;
			}
			index = 1;
			for(Map<String, String> map : clsMapList)
			{
				temp[index] = map.get(key);
				index++;
			}
			data.add(temp);
		}
		// This will change the data into the structure which the DataTableModle
		// accepts.
		Object[][] tableData = new Object[data.size()][];
		index = 0;
		for(Object[] objects : data)
		{
			tableData[index] = objects;
			index++;
		}
		return new DataTableModel("", columnNames, tableData, false);
	}

	/**
	 * This method will give the Object of the current Scenario.
	 *
	 * @param dataTableModel The whole comparison data Object.
	 * @return Will return the Object of the current Scenario.
	 */
	private DataTableModel builsBaseScenarioTables(DataTableModel dataTableModel)
	{
		Object[][] oldData = dataTableModel.getData();
		Object[][] newData = new Object[oldData.length][2];
		String[] oldColName = dataTableModel.getColumnNames();
		String[] colName = new String[2];
		colName[0] = oldColName[0];
		colName[1] = oldColName[1];
		for(int i = 0; i < newData.length; i++)
		{
			for(int j = 0; j < newData[0].length; j++)
			{
				newData[i][j] = oldData[i][j];
			}
		}
		return new DataTableModel("", colName, newData, false);
	}

	/**
	 * This will loop on the data and find the difference between the rows.
	 *
	 * @param dataTableModel The whole comparison data Object.
	 * @return The Object has the rows which holds different values.
	 */
	private DataTableModel differenceScenarioTables(DataTableModel dataTableModel)
	{
		Object[][] data = dataTableModel.getData();
		Object[][] compData = new Object[data.length][data[0].length];
		int index = 0;
		for(final Object[] datum : data)
		{
			boolean isDifferent = false;
			Object[] temp = datum;
			Object value = temp[1];
			for(int j = 2; j < temp.length; j++)
			{
				isDifferent = !value.equals(temp[j]);
				if(isDifferent)
				{
					break;
				}
			}
			if(isDifferent)
			{
				compData[index] = datum;
				index++;
			}
		}
		Object[][] newData = new Object[index][data[0].length];
		System.arraycopy(compData, 0, newData, 0, index);
		return new DataTableModel("", dataTableModel.getColumnNames(), newData, false);
	}

	/**
	 * This will read in the cls file and from that it will convert the cls file
	 * data into the Map.
	 *
	 * @param fileName    Names of the cls file.
	 * @param swingEngine The instance of Swing Engine.
	 * @param tableMap    Map of the table data and the key is the table Id.
	 * @return Will return the map of whole cls file.
	 */
	private Map<String, String> buildMapOfCLSFile(String fileName, SwingEngine swingEngine,
												  Map<String, GUILinks2BO> tableMap) throws EpptInitializationException
	{
		List<String> controlStrList = new ArrayList<>();
		List<String> dataTableModelStrList = new ArrayList<>();
		List<String> regulationoptionsStr = new ArrayList<>();
		List<String> wsidiStatusStr = new ArrayList<>();
		// Read in the cls file data.
		_scenarioSvc.getCLSData(Paths.get(Constant.SCENARIOS_DIR + fileName), controlStrList, dataTableModelStrList,
				regulationoptionsStr, wsidiStatusStr);
		// This will build all the component data into list of
		// ScenarioDisplayBO.
		List<ScenarioDisplayBO> componentData = buildCompDataIntoScenarioDisplayBOs(controlStrList, swingEngine);
		// This will convert the table Data in the cls file to the map.
		Map<String, String> tableData = buildTableDataInCLSIntoMap(dataTableModelStrList, tableMap);
		return buildDisplayBOsAndTableDataIntoMap(componentData, tableData);
	}

	/**
	 * This will convert the Table Data in the cls file into the map. the table
	 * rows have the key as the following format "Empty-dataTable name-RowNo".
	 *
	 * @param dataTableModelStrList The table Data list from the cls file.
	 * @param tableMap              Map of the table data and the key is the table Id.
	 * @return This contain all the table Data as a map.
	 */
	private Map<String, String> buildTableDataInCLSIntoMap(List<String> dataTableModelStrList,
														   Map<String, GUILinks2BO> tableMap)
	{
		Map<String, String> allTableData = new LinkedHashMap<>();
		String tableName = "";
		for(String dataTableStr : dataTableModelStrList)
		{
			String[] arr = dataTableStr.split(Constant.PIPELINE_DELIMITER);
			if(_scenarioSvc.isDouble(arr[0]))
			{
				GUILinks2BO gUILinks2BO = tableMap.get(arr[0]);
				tableName = gUILinks2BO.getDataTables();
			}
			else
			{
				tableName = arr[0];
			}
			allTableData.put(tableName.toUpperCase(), Constant.SPACE);
			String[] tableData = arr[1].split(Constant.SEMICOLON);
			int index = 1;
			for(final String tableDatum : tableData)
			{
				allTableData.put(Constant.EMPTY + Constant.DASH + tableName.toUpperCase() + Constant.DASH + index,
						tableDatum);
				index++;
			}
		}
		return allTableData;
	}

	/**
	 * This method will convert the component Data into the map and add the
	 * table Data map into the generated map.
	 *
	 * @param componentData The list of {@link ScenarioDisplayBO} Objects.
	 * @param tableData     Map of the Table Data.
	 * @return This will return the map of all the values in the cls file.
	 */
	private Map<String, String> buildDisplayBOsAndTableDataIntoMap(List<ScenarioDisplayBO> componentData,
																   Map<String, String> tableData)
	{
		String parent1 = "";
		String parent2 = "";
		String currentParent = "";
		String currentParent2 = "";
		String space = Constant.SPACE + Constant.SPACE;
		Map<String, String> data = new LinkedHashMap<>();
		// This will convert the component Data into the map.
		for(ScenarioDisplayBO scenarioDisplayBO : componentData)
		{
			if(scenarioDisplayBO.getComponentParents().contains(Constant.PIPELINE))
			{
				String[] parentArr = scenarioDisplayBO.getComponentParents().split(Constant.PIPELINE_DELIMITER);
				if(parent1.isEmpty())
				{
					parent1 = parentArr[0];
					parent2 = parentArr[1];
					data.put(parent1, Constant.SPACE);
					data.put(space + parent2, Constant.SPACE);
				}
				currentParent = parentArr[0];
				currentParent2 = parentArr[1];
				if(currentParent.equals(parent1))
				{
					if(!currentParent2.equals(parent2))
					{
						parent2 = parentArr[1];
						data.put(space + parentArr[1], Constant.SPACE);
					}
				}
				else
				{
					parent1 = currentParent;
					parent2 = currentParent2;
					data.put(currentParent, Constant.SPACE);
					data.put(space + currentParent2, Constant.SPACE);
				}
				data.put(space + space + scenarioDisplayBO.getComponentText(), scenarioDisplayBO.getComponentValue());
			}
			else
			{
				if(parent1.isEmpty())
				{
					parent1 = scenarioDisplayBO.getComponentParents();
					data.put(parent1, Constant.SPACE);
				}
				currentParent = scenarioDisplayBO.getComponentParents();
				if(!currentParent.equals(parent1))
				{
					parent1 = currentParent;
					data.put(parent1, Constant.SPACE);
				}
				data.put(space + scenarioDisplayBO.getComponentText(), scenarioDisplayBO.getComponentValue());
			}
		}
		data.putAll(tableData);
		return data;
	}

	/**
	 * This method will take the control strings from the cls file and convert
	 * them into the {@link ScenarioDisplayBO} Object. This method will also
	 * remove all the unwanted control from the cls file for the scenario
	 * display.
	 *
	 * @param controlStrList The control Data list from the cls file.
	 * @param swingEngine    The instance of Swing Engine.
	 * @return This will return the list of {@link ScenarioDisplayBO}.
	 */
	private List<ScenarioDisplayBO> buildCompDataIntoScenarioDisplayBOs(List<String> controlStrList,
																		SwingEngine swingEngine)
	{
		List<ScenarioDisplayBO> componentData = new ArrayList<>();
		for(String controlStr : controlStrList)
		{
			String[] arr = controlStr.split(Constant.PIPELINE_DELIMITER);
			String componentName = arr[0];
			String value = arr[1];
			String componentText = "";
			JComponent component = (JComponent) swingEngine.find(componentName);
			String sbparents = "";
			if(component != null)
			{
				sbparents = getControlParents(component);
			}
			// This check is to remove the unwanted data for the display.
			if(sbparents.contains("facilities"))
			{
				continue;
			}
			if(component instanceof JCheckBox)
			{
				JCheckBox jcb = (JCheckBox) component;
				if(jcb.getText().isEmpty())
				{
					componentText = componentName;
				}
				else
				{
					componentText = jcb.getText();
				}
				if("false".equals(value))
				{
					value = "Turned off";
				}
				else if(sbparents.contains("D-1641") && jcb.getText().contains("Default"))
				{
					value = "Per D-1641";
				}
				else
				{
					value = "Turned on";
				}
			}
			else if(component instanceof JRadioButton)
			{
				JRadioButton jrb = (JRadioButton) component;
				componentText = componentName;
				if("true".equals(value))
				{
					Container cont = jrb;
					while(cont.getParent() != null)
					{
						cont = cont.getParent();
						if(cont instanceof JPanel)
						{
							TitledBorder tb = (TitledBorder) ((JPanel) cont).getBorder();
							if(tb != null)
							{
								componentText = tb.getTitle();
								break;
							}
						}
					}
					value = jrb.getText();
				}
				else
				{
					continue;
				}
			}
			else
			{
				if(component instanceof JTextField)
				{
					JLabel label = (JLabel) swingEngine.find(componentName + "_t");
					if(label != null)
					{
						componentText = label.getText();
					}
					else
					{
						componentText = componentName;
					}
				}
				else
				{
					componentText = componentName;
				}
			}
			switch(componentText)
			{
				case "run_txaScenDesc":
					componentText = "Scenario Description";
					componentData.add(new ScenarioDisplayBO(componentText, sbparents, value));
					break;
				case "spnRunStartMonth":
					componentText = "Run Start";
					componentData.add(new ScenarioDisplayBO(componentText, sbparents, value));
					break;
				case "spnRunStartYear":
					for(ScenarioDisplayBO sdBO : componentData)
					{
						if("Run Start".equals(sdBO.getComponentText()))
						{
							sdBO.setComponentValue(sdBO.getComponentValue() + Constant.SPACE + value);
						}
					}
					break;
				case "spnRunEndMonth":
					componentText = "Run End";
					componentData.add(new ScenarioDisplayBO(componentText, sbparents, value));
					break;
				case "spnRunEndYear":
					for(ScenarioDisplayBO sdBO : componentData)
					{
						if("Run End".equals(sdBO.getComponentText()))
						{
							sdBO.setComponentValue(sdBO.getComponentValue() + Constant.SPACE + value);
						}
					}
					break;
				default:
					componentData.add(new ScenarioDisplayBO(componentText, sbparents, value));
					break;
			}
		}
		return componentData;
	}

	/**
	 * This method will go to the root parent and list them as a string with
	 * separated as "|".
	 *
	 * @param component The component in the SwingEngine
	 * @return The string of the parents which is separated with "|".
	 */
	private String getControlParents(Component component)
	{
		StringBuilder sb = new StringBuilder();
		Container cont = (Container) component;
		while(cont.getParent() != null)
		{
			cont = cont.getParent();
			if(cont instanceof JFrame)
			{
				break;
			}
			if(cont instanceof JPanel)
			{
				JPanel jp = (JPanel) cont;
				String name = jp.getName();
				if(name != null && !name.isEmpty() && !sb.toString().contains(name.toLowerCase()))
				{
					sb.append(name).append(Constant.PIPELINE);
				}
			}
		}
		sb.deleteCharAt(sb.length() - 1);
		// Reverse the string
		String[] strArray = sb.toString().split(Constant.PIPELINE_DELIMITER);
		List<String> result = new LinkedList<>();
		for(final String s : strArray)
		{
			if(!"null.contentPane".equals(s))
			{
				result.add(s);
			}
		}
		Collections.reverse(result);
		StringBuilder sb1 = new StringBuilder();
		for(String s : result)
		{
			sb1.append(s)
			   .append(Constant.PIPELINE);
		}
		sb = sb1.deleteCharAt(sb1.length() - 1);
		return sb.toString();
	}
}
