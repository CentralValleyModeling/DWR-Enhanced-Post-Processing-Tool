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

import java.util.List;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.geometry.Orientation;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;

import com.rma.javafx.treetable.RmaTreeTableView;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class SummaryTablePane extends JFXPanel
{
	private final String _plotTitle;
	private final WaterYearDefinition _waterYearDefinition;
	private final List<MonthPeriod> _selectedMonthlyPeriods;
	private final EpptReportingComputedSet _data;

	SummaryTablePane(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet data,
					 List<MonthPeriod> selectedMonthlyPeriods)
	{
		_data = data;
		_plotTitle = plotTitle;
		_waterYearDefinition = waterYearDefinition;
		_selectedMonthlyPeriods = selectedMonthlyPeriods;
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		BorderPane borderPane = new BorderPane();
		RmaTreeTableView<SummaryTableModel, SummaryPaneRow> treeView = new RmaTreeTableView<>();
		SummaryTableModel tableModel = new SummaryTableModel(_plotTitle, _waterYearDefinition, _data, _selectedMonthlyPeriods);
		treeView.setModel(tableModel);
		treeView.setCopyEnabled(true);
		ObservableList<TreeItem<SummaryPaneRow>> treeItems = treeView.getRoot().getChildren();
		expandTreeItems(treeItems);
		treeView.setColumnResizePolicy(TreeTableView.CONSTRAINED_RESIZE_POLICY);
		borderPane.setCenter(treeView);
		treeView.getColumnFromSpec(tableModel.getColumnSpecs().get(0)).setMinWidth(250);
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL);
		Button copyDataButton = new Button("Copy Data");
		copyDataButton.setOnAction(evt -> TreeTableUtil.copyValuesToClipboard(treeView));
		flowPane.getChildren().add(copyDataButton);
		borderPane.setBottom(flowPane);
		setScene(new Scene(borderPane));
	}

	private void expandTreeItems(ObservableList<TreeItem<SummaryPaneRow>> treeItems)
	{
		for(TreeItem<SummaryPaneRow> treeItem : treeItems)
		{
			if(treeItem != null)
			{
				treeItem.setExpanded(true);
				expandTreeItems(treeItem.getChildren());
			}
		}
	}
}
