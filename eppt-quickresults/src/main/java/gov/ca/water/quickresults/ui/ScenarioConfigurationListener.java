/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.bo.FileDialogBO;
import gov.ca.water.calgui.bus_delegate.IScenarioDele;
import gov.ca.water.calgui.bus_delegate.impl.ScenarioDeleImp;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.ScenarioFrame;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
class ScenarioConfigurationListener implements ActionListener
{
    private static final Logger LOGGER = Logger.getLogger(ScenarioConfigurationListener.class.getName());
    private final ScenarioConfigurationPanel _scenarioConfigurationPanel;

    ScenarioConfigurationListener(ScenarioConfigurationPanel scenarioConfigurationPanel)
    {
        _scenarioConfigurationPanel = scenarioConfigurationPanel;
    }

    @Override
    public void actionPerformed(ActionEvent e)
    {
        switch (e.getActionCommand())
        {
            case "Rep_ClearMonths":
                setQRMonthCheckBoxesSelected(false);
                break;
            case "Rep_All":
                setQRMonthCheckBoxesSelected(true);
                break;
            case "AC_CompScen":
                compareScenarios();
                break;
            case "btnAddScenario":
                launchFileDialogToAddScenarios(e);
                break;
            case "btnDelScenario":
                launchFileDialogToAddScenarios(e);
                break;
            case "btnClearScenario":
                launchFileDialogToAddScenarios(e);
                break;
            default:
                LOGGER.info("Testing action events " + e.getActionCommand());
        }
    }

    private void launchFileDialogToAddScenarios(ActionEvent e)
    {
        SwingEngine swingEngine = _scenarioConfigurationPanel.getSwingEngine();
        JList<?> lstScenarios = (JList<?>) swingEngine.find("SelectedList");
        JRadioButton rdb1 = (JRadioButton) swingEngine.find("rdbp001");
        JRadioButton rdb2 = (JRadioButton) swingEngine.find("rdbp002");
        FileDialogBO fileDialogBO = new FileDialogBO(lstScenarios, (JLabel) swingEngine.find("lblBase"), rdb1, rdb2,
                (JButton) swingEngine.find("btnPower"), true, (JFrame) SwingUtilities.windowForComponent(_scenarioConfigurationPanel));
        fileDialogBO.actionPerformed(e);
    }

	private JFrame getMainWindow()
	{
		Window window = SwingUtilities.windowForComponent(_scenarioConfigurationPanel);
		return (JFrame) window;
	}

    private void compareScenarios()
    {
        IScenarioDele scenarioDele = new ScenarioDeleImp();
        List<String> fileNames = new ArrayList<>();
        for (int i = 0; i < _scenarioConfigurationPanel.getScenarios().size(); i++)
        {
            String name = Paths.get(_scenarioConfigurationPanel.getScenarios().get(i).toString())
                    .getFileName().toString();
            fileNames.add(name.substring(0, name.length() - 7) + Constant.CLS_EXT);
        }
        try
        {
			List<DataTableModel> dtmList = scenarioDele.getScenarioTableData(fileNames,
					_scenarioConfigurationPanel.getSwingEngine());
			ScenarioFrame scenarioFrame = new ScenarioFrame(dtmList, getMainWindow());
            scenarioFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            scenarioFrame.setVisible(true);
        }
        catch (EpptInitializationException ex)
        {
            //do something? Error finding or reading file in fileNames
        }
        try
        {
            Files.delete(
                    Paths.get(Constant.SCENARIOS_DIR + Constant.CURRENT_SCENARIO + Constant.CLS_EXT));
        }
        catch (IOException ex)
        {
            LOGGER.debug(ex);
        }
        return;
    }

    /**
     * Selects/deselects all monthly checkboxes on Quick Result control panel
     *
     * @param b
     */
    private void setQRMonthCheckBoxesSelected(boolean b)
    {
        JPanel controls2 = _scenarioConfigurationPanel.getControls2();
        Component[] components = controls2.getComponents();
        for (int i = 0; i < components.length; i++)
        {
            if (components[i] instanceof JCheckBox)
            {
                JCheckBox c = (JCheckBox) components[i];
                String cName = c.getName();
                if (cName != null)
                {
                    if (cName.startsWith("RepchkMon"))
                    {
                        c.setSelected(b);
                    }
                }
            }
        }
    }
}
