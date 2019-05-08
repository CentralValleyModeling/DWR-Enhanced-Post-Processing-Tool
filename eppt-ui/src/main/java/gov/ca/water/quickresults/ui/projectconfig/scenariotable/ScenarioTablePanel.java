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

package gov.ca.water.quickresults.ui.projectconfig.scenariotable;

import java.util.List;
import java.util.Optional;

import gov.ca.water.calgui.project.EpptScenarioRun;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;

import com.rma.javafx.treetable.RmaTreeTableView;
import com.rma.javafx.treetable.TreeTableViewSelectionUtilities;
import com.rma.javafx.treetable.cells.views.RmaCheckBoxCellView;

import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.ALTERNATIVE_COL_SPEC;
import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.BASE_COL_SPEC;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-07-2019
 */
public class ScenarioTablePanel extends JFXPanel
{
	private RmaTreeTableView<ScenarioTableModel, ParentRowModel> _treeTable;
	private ScenarioTableModel _scnearioTableModel;

	public ScenarioTablePanel()
	{
		Platform.setImplicitExit(false);
		Platform.runLater(this::initFX);
	}

	private void initFX()
	{
		_scnearioTableModel = new ScenarioTableModel();
		_treeTable = new RmaTreeTableView<>();
		_treeTable.setModel(_scnearioTableModel);
		_treeTable.setPlaceholder(new Label("No Scenario Runs"));
		_treeTable.setCellView(BASE_COL_SPEC, new RmaCheckBoxCellView());
		_treeTable.setCellView(ALTERNATIVE_COL_SPEC, new RmaCheckBoxCellView());
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_treeTable);
		setScene(new Scene(borderPane));
		Platform.runLater(_treeTable::resizeAllColumnsToFitContent);
	}

	public void clearScenarios()
	{
		Platform.runLater(() ->
		{
			if(_scnearioTableModel != null)
			{
				_scnearioTableModel.getRows().clear();
			}
		});
	}

	public EpptScenarioRun getBaseScenarioRun()
	{
		return _scnearioTableModel.getBaseScenarioRun();
	}

	public List<EpptScenarioRun> getAlternativeScenarioRuns()
	{
		return _scnearioTableModel.getAlternativeScenarioRuns();
	}

	public void deleteSelectedScenarioRun()
	{
		Platform.runLater(() -> TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable)
															   .forEach(_scnearioTableModel.getRows()::remove));
	}

	public void addScenarioRun(EpptScenarioRun scenarioRun)
	{
		Platform.runLater(() ->
		{
			_scnearioTableModel.getRows().add(new ScenarioRowModel(_treeTable, _scnearioTableModel, scenarioRun,
					_scnearioTableModel.getRows().isEmpty(), false));
		});
	}

	public void updateScenario(EpptScenarioRun oldScenarioRun, EpptScenarioRun newScenarioRun)
	{
		Platform.runLater(() ->
		{
			Optional<ScenarioRowModel> scenarioRowModel = _scnearioTableModel.getRowForScenarioRun(oldScenarioRun);
			if(scenarioRowModel.isPresent())
			{
				ScenarioRowModel oldModel = scenarioRowModel.get();
				int i = _scnearioTableModel.getRows().indexOf(oldModel);
				_scnearioTableModel.getRows().remove(i);
				_scnearioTableModel.getRows().add(i,
						new ScenarioRowModel(_treeTable, _scnearioTableModel, newScenarioRun, oldModel.isBase(),
								oldModel.isAlternative()));
			}
		});
	}

	public EpptScenarioRun getSelectedScenario()
	{
		EpptScenarioRun retval = null;
		List<ParentRowModel> selectedRowModels = TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable);
		if(selectedRowModels.size() == 1)
		{
			ParentRowModel parentRowModel = selectedRowModels.get(0);
			if(parentRowModel instanceof ScenarioRowModel)
			{
				retval = ((ScenarioRowModel) parentRowModel).getScenarioRun();
			}
		}
		return retval;
	}
}
