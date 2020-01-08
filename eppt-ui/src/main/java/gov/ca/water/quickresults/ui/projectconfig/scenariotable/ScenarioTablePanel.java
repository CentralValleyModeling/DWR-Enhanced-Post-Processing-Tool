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

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;

import rma.util.RMAUtil;
import com.rma.javafx.iface.ColumnSpec;
import com.rma.javafx.iface.RowModel;
import com.rma.javafx.treetable.RmaTreeTableView;
import com.rma.javafx.treetable.TreeTableViewSelectionUtilities;
import com.rma.javafx.treetable.cells.views.RmaCheckBoxCellView;

import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.ALTERNATIVE_COL_SPEC;
import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.BASE_COL_SPEC;
import static gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTableModel.NAME_COL_SPEC;
import static java.util.stream.Collectors.toList;

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
		_treeTable.setCellView(BASE_COL_SPEC, new ScenarioCheckboxView());
		_treeTable.setCellView(ALTERNATIVE_COL_SPEC, new ScenarioCheckboxView());
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
		if(_scenarioTableModel != null)
		{
			_scenarioTableModel.getRows().clear();
		}
	}

	public EpptScenarioRun getBaseScenarioRun()
	{
		if(_scenarioTableModel != null)
		{
			return _scenarioTableModel.getBaseScenarioRun();
		}
		else
		{
			return null;
		}
	}

	public List<EpptScenarioRun> getAlternativeScenarioRuns()
	{
		if(_scenarioTableModel != null)
		{
			return _scenarioTableModel.getAlternativeScenarioRuns();
		}
		else
		{
			return new ArrayList<>();
		}
	}

	public void deleteSelectedScenarioRun()
	{
		Platform.runLater(() -> TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable)
															   .forEach(_scenarioTableModel.getRows()::remove));
	}

	public void addScenarioRun(EpptScenarioRun scenarioRun)
	{
		Platform.runLater(() ->
				_scenarioTableModel.getRows().add(new ScenarioRowModel(this::setModified, _scenarioTableModel, scenarioRun)));
	}

	private void setModified()
	{
		_treeTable.refresh();
		SwingUtilities.invokeLater(() -> RMAUtil.setParentModified(this));
	}

	public CompletableFuture<Void> updateScenario(EpptScenarioRun oldScenarioRun, EpptScenarioRun newScenarioRun)
	{
		return CompletableFuture.runAsync(()->
		{
			Optional<ScenarioRowModel> scenarioRowModel = _scenarioTableModel.getRowForScenarioRun(oldScenarioRun);
			if(scenarioRowModel.isPresent())
			{
				ScenarioRowModel oldModel = scenarioRowModel.get();
				int i = _scenarioTableModel.getRows().indexOf(oldModel);
				_scenarioTableModel.getRows().remove(i);
				newScenarioRun.setBaseSelected(oldModel.isBase());
				newScenarioRun.setAltSelected(oldModel.isAlternative());
				_scenarioTableModel.getRows().add(i, new ScenarioRowModel(this::setModified, _scenarioTableModel, newScenarioRun));
			}
		}, Platform::runLater);
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

	public Path getSelectedDssFile()
	{
		Path retval = null;
		List<ParentRowModel> selectedRowModels = TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable);
		if(selectedRowModels.size() == 1)
		{
			ParentRowModel parentRowModel = selectedRowModels.get(0);
			if(parentRowModel instanceof DssPathRow)
			{
				retval = ((DssPathRow) parentRowModel).getDssPath();
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
					rows.add(i - 1, rowModel);
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
				if(i >= 0 && i < rows.size() - 1)
				{
					rows.remove(i);
					rows.add(i + 1, rowModel);
				}
			}
		}
	}

	public void relativizeScenariosToNewProject(Path newProjectPath, Path oldProjectPath)
	{
		List<EpptScenarioRun> allScenarioRuns = _scenarioTableModel.getAllScenarioRuns();
		for(EpptScenarioRun epptScenarioRun : allScenarioRuns)
		{
			EpptScenarioRun newRun = relativizeScenarioToNewProject(epptScenarioRun, newProjectPath, oldProjectPath);
			try
			{
				updateScenario(epptScenarioRun, newRun).get();
			}
			catch(InterruptedException e)
			{
				Thread.currentThread().interrupt();
				LOGGER.log(Level.FINE, "Error relativizing project files", e);
			}
			catch(ExecutionException e)
			{
				LOGGER.log(Level.SEVERE, "Error relativizing project files", e);
			}
		}
	}

	private EpptScenarioRun relativizeScenarioToNewProject(EpptScenarioRun scenarioRun, Path newProjectPath, Path oldProjectPath)
	{
		EpptDssContainer oldDssContainer = scenarioRun.getDssContainer();
		List<NamedDssPath> extra = oldDssContainer.getExtraDssFiles()
												  .stream()
												  .map(e -> copyDssToNewProjectFolder(newProjectPath, oldProjectPath, e))
												  .collect(toList());
		NamedDssPath ivDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getIvDssFile());
		NamedDssPath dvDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getDvDssFile());
		NamedDssPath svDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getSvDssFile());
		NamedDssPath dtsDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getDtsDssFile());
		EpptDssContainer newDssContainer = new EpptDssContainer(dvDssPath,
				svDssPath,
				ivDssPath,
				dtsDssPath,
				extra);
		return new EpptScenarioRun(scenarioRun.getName(),
				scenarioRun.getDescription(),
				scenarioRun.getModel(),
				makeRelativeToNewProject(scenarioRun.getOutputPath(), newProjectPath, oldProjectPath),
				makeRelativeToNewProject(scenarioRun.getWreslMain(), newProjectPath, oldProjectPath),
				makeRelativeToNewProject(scenarioRun.getWaterYearTable(), newProjectPath, oldProjectPath),
				newDssContainer,
				scenarioRun.getColor());
	}

	private NamedDssPath copyDssToNewProjectFolder(Path newProjectPath, Path oldProjectPath, NamedDssPath dvDssFile)
	{
		return new NamedDssPath(makeRelativeToNewProject(dvDssFile.getDssPath(), newProjectPath, oldProjectPath),
				dvDssFile.getAliasName(),
				dvDssFile.getAPart(),
				dvDssFile.getEPart(),
				dvDssFile.getFPart());
	}

	private Path makeRelativeToNewProject(Path outputPath, Path newProjectPath, Path oldProjectPath)
	{
		if(outputPath.startsWith(oldProjectPath))
		{
			Path relativize = oldProjectPath.relativize(outputPath);
			return newProjectPath.resolve(relativize);
		}
		return outputPath;
	}

	private static class ScenarioCheckboxView extends RmaCheckBoxCellView
	{

		@Override
		public CheckBox getCellEditorNode(Object value, RowModel<?> row, ColumnSpec column)
		{
			if(row instanceof DssPathRow)
			{
				return null;
			}
			else
			{
				return super.getCellEditorNode(value, row, column);
			}
		}

		@Override
		public CheckBox getCellRendererNode(Object value, Node graphic, RowModel<?> row, ColumnSpec column, boolean isCellEditable)
		{
			if(row instanceof DssPathRow)
			{
				return null;
			}
			else
			{
				return super.getCellRendererNode(value, graphic, row, column, isCellEditable);
			}
		}
	}
}
