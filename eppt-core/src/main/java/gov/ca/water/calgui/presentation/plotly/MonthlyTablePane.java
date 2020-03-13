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
class MonthlyTablePane extends JFXPanel
{
	private final String _plotTitle;
	private final WaterYearDefinition _waterYearDefinition;
	private final EpptReportingComputedSet _epptReportingComputedSet;

	MonthlyTablePane(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet)
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
		RmaTreeTableView<MonthlyTableModel, MonthlyPaneRow> treeView = new RmaTreeTableView<>();
		MonthlyTableModel monthlyTableModel = new MonthlyTableModel(_plotTitle, _waterYearDefinition, _epptReportingComputedSet);
		treeView.setModel(monthlyTableModel);
		treeView.setCopyEnabled(true);
		ObservableList<TreeItem<MonthlyPaneRow>> treeItems = treeView.getRoot().getChildren();
		for(TreeItem<MonthlyPaneRow> treeItem : treeItems)
		{
			if(treeItem != null)
			{
				treeItem.setExpanded(true);
			}
		}
		treeView.setColumnResizePolicy(TreeTableView.CONSTRAINED_RESIZE_POLICY);
		treeView.getColumnFromSpec(monthlyTableModel.getColumnSpecs().get(0)).setMinWidth(250);
		borderPane.setCenter(treeView);
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL);
		Button copyDataButton = new Button("Copy Data");
		copyDataButton.setOnAction(evt->TreeTableUtil.copyValuesToClipboard(treeView));
		flowPane.getChildren().add(copyDataButton);
		borderPane.setBottom(flowPane);
		setScene(new Scene(borderPane));
	}
}
