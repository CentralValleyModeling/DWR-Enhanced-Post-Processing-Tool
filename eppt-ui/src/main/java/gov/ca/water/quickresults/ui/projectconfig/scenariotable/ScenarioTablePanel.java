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

import java.awt.Container;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import gov.ca.water.calgui.project.EpptScenarioRun;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;

import rma.lang.Modifiable;
import rma.util.RMAUtil;
import com.rma.javafx.treetable.RmaTreeTableView;
import com.rma.javafx.treetable.TreeTableViewSelectionUtilities;
import com.rma.javafx.treetable.cells.views.RmaCheckBoxCellView;

import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.ALTERNATIVE_COL_SPEC;
import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.BASE_COL_SPEC;
import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.NAME_COL_SPEC;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-07-2019
 */
public class ScenarioTablePanel extends JFXPanel
{
	private static final Logger LOGGER = Logger.getLogger(ScenarioTablePanel.class.getName());
	private RmaTreeTableView<ScenarioTableModel, ParentRowModel> _treeTable;
	private ScenarioTableModel _scenarioTableModel;

	public ScenarioTablePanel()
	{
		Platform.setImplicitExit(false);
		Platform.runLater(this::initFX);
	}

	private void initFX()
	{
		_scenarioTableModel = new ScenarioTableModel();
		_treeTable = new RmaTreeTableView<>();
		_treeTable.setModel(_scenarioTableModel);
		_treeTable.setPlaceholder(new Label("No Scenario Runs"));
		_treeTable.setCellView(BASE_COL_SPEC, new RmaCheckBoxCellView());
		_treeTable.setCellView(ALTERNATIVE_COL_SPEC, new RmaCheckBoxCellView());
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_treeTable);
		try
		{
			setScene(new Scene(borderPane));
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to set JavaFX scene for Scenario Table", ex);
		}
		Platform.runLater(() -> _treeTable.getColumnFromSpec(NAME_COL_SPEC).setPrefWidth(200));
	}

	public void clearScenarios()
	{
		Platform.runLater(() ->
		{
			if(_scenarioTableModel != null)
			{
				_scenarioTableModel.getRows().clear();
			}
		});
	}

	public EpptScenarioRun getBaseScenarioRun()
	{
		return _scenarioTableModel.getBaseScenarioRun();
	}

	public List<EpptScenarioRun> getAlternativeScenarioRuns()
	{
		return _scenarioTableModel.getAlternativeScenarioRuns();
	}

	public void deleteSelectedScenarioRun()
	{
		Platform.runLater(() -> TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable)
															   .forEach(_scenarioTableModel.getRows()::remove));
	}

	public void addScenarioRun(EpptScenarioRun scenarioRun)
	{
		Platform.runLater(() -> _scenarioTableModel.getRows().add(
				new ScenarioRowModel(this::setModified, _scenarioTableModel, scenarioRun,
						getBaseScenarioRun() == null, getBaseScenarioRun() != null && getAlternativeScenarioRuns().isEmpty())));
	}

	private void setModified()
	{
		_treeTable.refresh();
		SwingUtilities.invokeLater(()->RMAUtil.setParentModified(this));
	}

	public void updateScenario(EpptScenarioRun oldScenarioRun, EpptScenarioRun newScenarioRun)
	{
		Platform.runLater(() ->
		{
			Optional<ScenarioRowModel> scenarioRowModel = _scenarioTableModel.getRowForScenarioRun(oldScenarioRun);
			if(scenarioRowModel.isPresent())
			{
				ScenarioRowModel oldModel = scenarioRowModel.get();
				int i = _scenarioTableModel.getRows().indexOf(oldModel);
				_scenarioTableModel.getRows().remove(i);
				_scenarioTableModel.getRows().add(i,
						new ScenarioRowModel(this::setModified, _scenarioTableModel, newScenarioRun, oldModel.isBase(),
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

	public Collection<? extends EpptScenarioRun> getAllScenarioRuns()
	{
		return _scenarioTableModel.getAllScenarioRuns();
	}

	public void moveSelectedScenarioUp()
	{
		List<ParentRowModel> selectedRowModels = TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable);
		ObservableList<ParentRowModel> rows = _scenarioTableModel.getRows();
		if(!selectedRowModels.isEmpty())
		{
			for(ParentRowModel rowModel : selectedRowModels)
			{
				int i = rows.indexOf(rowModel);
				if(i > 0)
				{
					rows.remove(i);
					rows.add(i-1, rowModel);
				}
			}
		}
	}

	public void moveSelectedScenarioDown()
	{
		List<ParentRowModel> selectedRowModels = TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable);
		ObservableList<ParentRowModel> rows = _scenarioTableModel.getRows();
		if(!selectedRowModels.isEmpty())
		{
			for(ParentRowModel rowModel : selectedRowModels)
			{
				int i = rows.indexOf(rowModel);
				if(i >= 0 && i < rows.size() -1)
				{
					rows.remove(i);
					rows.add(i+1, rowModel);
				}
			}
		}
	}
}
