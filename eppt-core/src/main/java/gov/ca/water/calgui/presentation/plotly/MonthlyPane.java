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

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableView;
import javafx.scene.control.TreeView;
import javafx.scene.layout.BorderPane;

import com.rma.javafx.treetable.RmaTreeTableView;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class MonthlyPane extends JFXPanel
{
	private final String _plotTitle;
	private final WaterYearDefinition _waterYearDefinition;
	private final EpptReportingComputedSet _epptReportingComputedSet;
	private RmaTreeTableView<MonthlyTableModel, MonthlyPaneRow> _treeView;

	MonthlyPane(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet)
	{
		_plotTitle = plotTitle;
		_waterYearDefinition = waterYearDefinition;
		_epptReportingComputedSet = epptReportingComputedSet;
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		BorderPane borderPane = new BorderPane();
		_treeView = new RmaTreeTableView<>();
		MonthlyTableModel monthlyTableModel = new MonthlyTableModel(_plotTitle, _waterYearDefinition, _epptReportingComputedSet);
		_treeView.setModel(monthlyTableModel);
		_treeView.setCopyEnabled(true);
		for(int i = 0; i < monthlyTableModel.getRows().size(); i++)
		{
			TreeItem<MonthlyPaneRow> treeItem = _treeView.getTreeItem(i);
			if(treeItem != null)
			{
				treeItem.setExpanded(true);
			}
		}
		Platform.runLater(() ->
		{
			_treeView.setColumnResizePolicy(TreeTableView.CONSTRAINED_RESIZE_POLICY);
			_treeView.resizeAllColumnsToFitContent();
			_treeView.setColumnResizePolicy(TreeTableView.CONSTRAINED_RESIZE_POLICY);
		});
//		_treeView.resizeAllColumnsToFitContent();
		borderPane.setCenter(_treeView);
		setScene(new Scene(borderPane));
	}
}
