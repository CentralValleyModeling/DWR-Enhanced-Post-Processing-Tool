/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.presentation.plotly;

import java.util.Map;
import java.util.SortedMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableView;
import javafx.scene.layout.BorderPane;

import com.rma.javafx.treetable.RmaTreeTableView;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
public class SummaryPane extends JFXPanel
{
	private final String _plotTitle;
	private final WaterYearDefinition _waterYearDefinition;
	private final SortedMap<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> _data;

	public SummaryPane(String plotTitle, WaterYearDefinition waterYearDefinition,
					   SortedMap<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> data)
	{
		_data = data;
		_plotTitle = plotTitle;
		_waterYearDefinition = waterYearDefinition;
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		BorderPane borderPane = new BorderPane();
		RmaTreeTableView<SummaryTableModel, SummaryPaneRow> treeView = new RmaTreeTableView<>();
		SummaryTableModel monthlyTableModel = new SummaryTableModel(_plotTitle, _waterYearDefinition, _data);
		treeView.setModel(monthlyTableModel);
		treeView.setCopyEnabled(true);
		treeView.setColumnResizePolicy(TreeTableView.CONSTRAINED_RESIZE_POLICY);
		for(int i = 0; i < monthlyTableModel.getRows().size(); i++)
		{
			TreeItem<SummaryPaneRow> treeItem = treeView.getTreeItem(i);
			if(treeItem != null)
			{
				treeItem.setExpanded(true);
			}

		}
		treeView.resizeAllColumnsToFitContent();
		borderPane.setCenter(treeView);
		setScene(new Scene(borderPane));
	}
}
