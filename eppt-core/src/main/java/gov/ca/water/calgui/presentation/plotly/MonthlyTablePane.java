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
import javafx.event.ActionEvent;
import javafx.geometry.Orientation;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
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
	private final List<MonthPeriod> _selectedMonthlyPeriods;
	private RmaTreeTableView<MonthlyTableTableModel, MonthlyPaneRow> _treeView;
	private CheckBox _comparisonModeCheckbox;

	MonthlyTablePane(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet, List<MonthPeriod> selectedMonthlyPeriods)
	{
		_plotTitle = plotTitle;
		_waterYearDefinition = waterYearDefinition;
		_epptReportingComputedSet = epptReportingComputedSet;
		_selectedMonthlyPeriods = selectedMonthlyPeriods;
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		BorderPane borderPane = new BorderPane();
		_treeView = new RmaTreeTableView<>();
		_treeView.setCopyEnabled(true);
		setSequentialTableModel();
		borderPane.setCenter(_treeView);
		Button copyDataButton = new Button("Copy Data");
		_comparisonModeCheckbox = new CheckBox("Comparison Mode");
		_comparisonModeCheckbox.setOnAction(this::toggleTableModel);
		copyDataButton.setOnAction(evt -> TreeTableUtil.copyValuesToClipboard(_treeView));

		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL, 5.0, 10.0, copyDataButton, _comparisonModeCheckbox);
		borderPane.setBottom(flowPane);
		setScene(new Scene(borderPane));
	}

	private void toggleTableModel(ActionEvent evt)
	{
		if(_comparisonModeCheckbox.isSelected())
		{
			setSideBySideTableModel();
		}
		else
		{
			setSequentialTableModel();
		}
	}

	private void setSequentialTableModel()
	{
		MonthlySequentialTableModel monthlySequentialTableModel = new MonthlySequentialTableModel(_plotTitle, _waterYearDefinition, _epptReportingComputedSet,
				_selectedMonthlyPeriods);
		_treeView.setModel(monthlySequentialTableModel);
		_treeView.setColumnResizePolicy(TreeTableView.CONSTRAINED_RESIZE_POLICY);
		ObservableList<TreeItem<MonthlyPaneRow>> treeItems = _treeView.getRoot().getChildren();
		for(TreeItem<MonthlyPaneRow> treeItem : treeItems)
		{
			if(treeItem != null)
			{
				treeItem.setExpanded(true);
			}
		}
		_treeView.getColumnFromSpec(monthlySequentialTableModel.getColumnSpecs().get(0)).setPrefWidth(250);
	}

	private void setSideBySideTableModel()
	{
		_treeView.setColumnResizePolicy(TreeTableView.UNCONSTRAINED_RESIZE_POLICY);
		MonthlySideBySideTableModel monthlySequentialTableModel = new MonthlySideBySideTableModel(_plotTitle, _waterYearDefinition, _epptReportingComputedSet,
				_selectedMonthlyPeriods);
		_treeView.setModel(monthlySequentialTableModel);
		ObservableList<TreeItem<MonthlyPaneRow>> treeItems = _treeView.getRoot().getChildren();
		for(TreeItem<MonthlyPaneRow> treeItem : treeItems)
		{
			if(treeItem != null)
			{
				treeItem.setExpanded(true);
			}
		}
		_treeView.getColumnFromSpec(monthlySequentialTableModel.getColumnSpecs().get(0)).setPrefWidth(250);
	}
}
