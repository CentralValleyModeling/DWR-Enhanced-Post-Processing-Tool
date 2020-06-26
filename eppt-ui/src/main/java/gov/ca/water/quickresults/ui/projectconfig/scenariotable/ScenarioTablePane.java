/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.quickresults.ui.projectconfig.scenariotable;

import java.awt.Frame;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioRunEditor;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.TilePane;
import javafx.scene.paint.Color;

import hec.gui.NameDialog;
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
public class ScenarioTablePane extends TitledPane
{
	private static final Logger LOGGER = Logger.getLogger(ScenarioTablePane.class.getName());
	private final Button _addButton = new Button("Add");
	private final Button _editButton = new Button("Edit");
	private final Button _copyButton = new Button("Copy");
	private final Button _clearButton = new Button("Clear");
	private final Button _clearAllButton = new Button("Clear All");
	private final Button _upButton = new Button("\u25B2");
	private final Button _downButton = new Button("\u25BC");
	private final CheckBox _differenceCheckbox = new CheckBox("Difference");
	private final ScenarioTableModel _scenarioTableModel;
	private final RmaTreeTableView<ScenarioTableModel, ScenarioTableRowModel> _treeTable;
	private final EpptConfigurationController _controller;
	private boolean _modifiedListenerEnabled = false;

	public ScenarioTablePane(EpptConfigurationController controller)
	{
		_controller = controller;
		_scenarioTableModel = new ScenarioTableModel();
		_treeTable = new RmaTreeTableView<>();
		initComponents();
		_controller.getScenarioRuns().forEach(this::addScenarioRun);
		initListeners();
	}

	private void initListeners()
	{
		_treeTable.getSelectionModel().selectedItemProperty().addListener(this::tableSelected);
		_addButton.setOnAction(e -> launchFileDialogToAddScenario());
		_editButton.setOnAction(e -> launchFileDialogToEditScenario());
		_copyButton.setOnAction(e -> launchFileDialogToCopyScenario());
		_clearButton.setOnAction(e -> deleteSelectedScenarioRun());
		_clearAllButton.setOnAction(e -> clearScenarios());
		_upButton.setOnAction(e -> moveSelectedScenarioUp());
		_downButton.setOnAction(e -> moveSelectedScenarioDown());
		_differenceCheckbox.selectedProperty().addListener((e, o, n) ->
		{
			_controller.setDifference(n);
			if(_modifiedListenerEnabled)
			{
				_controller.setModified();
			}
		});
	}

	private void initComponents()
	{
		BorderPane titleBorderPane = new BorderPane();
		Label statisticLabel = new Label("Scenarios");
		titleBorderPane.setLeft(statisticLabel);
		BorderPane.setMargin(statisticLabel, new Insets(2));
		setGraphic(titleBorderPane);
		BorderPane borderPane = new BorderPane();
		_addButton.setMaxWidth(Double.MAX_VALUE);
		_editButton.setMaxWidth(Double.MAX_VALUE);
		_copyButton.setMaxWidth(Double.MAX_VALUE);
		_clearButton.setMaxWidth(Double.MAX_VALUE);
		_clearAllButton.setMaxWidth(Double.MAX_VALUE);
		_editButton.setDisable(true);
		_copyButton.setDisable(true);
		_clearButton.setDisable(true);
		_upButton.setDisable(true);
		_downButton.setDisable(true);
		TilePane buttonPane = new TilePane(Orientation.HORIZONTAL, 5.0, 5.0, _addButton, _editButton, _copyButton, _clearButton, _clearAllButton);
		borderPane.setTop(buttonPane);
		_treeTable.setModel(_scenarioTableModel);
		_treeTable.setPlaceholder(new Label("No Scenario Runs"));
		_treeTable.setCellView(BASE_COL_SPEC, new ScenarioCheckboxView());
		_treeTable.setCellView(ALTERNATIVE_COL_SPEC, new ScenarioCheckboxView());
		borderPane.setCenter(_treeTable);
		BorderPane.setMargin(_treeTable, new Insets(5.0, 0.0, 5.0, 0.0));
		BorderPane southernRegion = new BorderPane();
		TilePane upDownPane = new TilePane(Orientation.HORIZONTAL, 5.0, 5.0, _upButton, _downButton);
		upDownPane.setAlignment(Pos.CENTER_RIGHT);
		HBox checkboxPane = new HBox(5.0, _differenceCheckbox);
		southernRegion.setLeft(checkboxPane);
		southernRegion.setRight(upDownPane);
		borderPane.setBottom(southernRegion);
		setContent(borderPane);
		Platform.runLater(() -> _treeTable.getColumnFromSpec(NAME_COL_SPEC).setPrefWidth(200));
		setPrefWidth(200);
	}

	private void clearScenarios()
	{
		if(JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(Frame.getFrames()[0],
				"Are you sure you want to delete all Scenario Runs?\nThis operation cannot be undone.", "Clear All", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE))
		{
			_scenarioTableModel.getRows().clear();
			setModified();
		}
	}

	private void deleteSelectedScenarioRun()
	{
		EpptScenarioRun selectedScenario = getSelectedScenario();
		if(selectedScenario != null)
		{
			int clear = JOptionPane.showConfirmDialog(Frame.getFrames()[0],
					"Are you sure you want to delete Scenario Runs: " + selectedScenario + "?\nThis operation cannot be undone.", "Clear", JOptionPane.YES_NO_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if(JOptionPane.YES_OPTION == clear)
			{
				TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable).forEach(_scenarioTableModel.getRows()::remove);
				setModified();
			}
		}
	}

	private void addScenarioRun(EpptScenarioRun scenarioRun)
	{
		try
		{
			new WaterYearTableReader(scenarioRun).forceRead();
			_scenarioTableModel.getRows().add(new ScenarioRowModel(this::setModified, _scenarioTableModel, scenarioRun));
		}
		catch(EpptInitializationException e)
		{
			LOGGER.log(Level.SEVERE, "Error reading water year table files for scenario: " + scenarioRun, e);
		}
	}

	private void setModified()
	{
		if(_modifiedListenerEnabled)
		{
			_treeTable.refresh();
			_controller.setScenarioRuns(_scenarioTableModel.getAllScenarioRuns());
			_controller.setModified();
		}
	}

	private void updateScenario(EpptScenarioRun oldScenarioRun, EpptScenarioRun newScenarioRun)
	{
		try
		{
			Optional<ScenarioRowModel> rowForScenarioRun = _scenarioTableModel.getRowForScenarioRun(oldScenarioRun);
			if(rowForScenarioRun.isPresent())
			{
				new WaterYearTableReader(newScenarioRun).forceRead();
				ScenarioRowModel oldModel = rowForScenarioRun.get();
				int i = _scenarioTableModel.getRows().indexOf(oldModel);
				_scenarioTableModel.getRows().remove(i);
				newScenarioRun.setBaseSelected(oldModel.isBase());
				newScenarioRun.setAltSelected(oldModel.isAlternative());
				_scenarioTableModel.getRows().add(i, new ScenarioRowModel(this::setModified, _scenarioTableModel, newScenarioRun));
				setModified();
			}
		}
		catch(EpptInitializationException e)
		{
			LOGGER.log(Level.SEVERE, "Error reading water year table files for scenario: " + newScenarioRun, e);
		}
	}

	private EpptScenarioRun getSelectedScenario()
	{
		EpptScenarioRun retval = null;
		List<ScenarioTableRowModel> selectedRowModels = TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable);
		if(selectedRowModels.size() == 1)
		{
			ScenarioTableRowModel scenarioTableRowModel = selectedRowModels.get(0);
			if(scenarioTableRowModel instanceof ScenarioRowModel)
			{
				retval = ((ScenarioRowModel) scenarioTableRowModel).getScenarioRun();
			}
		}
		return retval;
	}

	private void moveSelectedScenarioUp()
	{
		List<ScenarioTableRowModel> selectedRowModels = TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable);
		ObservableList<ScenarioTableRowModel> rows = _scenarioTableModel.getRows();
		if(!selectedRowModels.isEmpty())
		{
			for(ScenarioTableRowModel rowModel : selectedRowModels)
			{
				int i = rows.indexOf(rowModel);
				if(i > 0)
				{
					rows.remove(i);
					rows.add(i - 1, rowModel);
					_treeTable.getSelectionModel().select(i - 1);
					_controller.setModified();
				}
			}
		}
	}

	private void moveSelectedScenarioDown()
	{
		List<ScenarioTableRowModel> selectedRowModels = TreeTableViewSelectionUtilities.getSelectedRowModels(_treeTable);
		ObservableList<ScenarioTableRowModel> rows = _scenarioTableModel.getRows();
		if(!selectedRowModels.isEmpty())
		{
			for(ScenarioTableRowModel rowModel : selectedRowModels)
			{
				int i = rows.indexOf(rowModel);
				if(i >= 0 && i < rows.size() - 1)
				{
					rows.remove(i);
					rows.add(i + 1, rowModel);
					_treeTable.getSelectionModel().select(i + 1);
					_controller.setModified();
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
			updateScenario(epptScenarioRun, newRun);
		}
	}

	private EpptScenarioRun relativizeScenarioToNewProject(EpptScenarioRun scenarioRun, Path newProjectPath, Path oldProjectPath)
	{
		EpptDssContainer oldDssContainer = scenarioRun.getDssContainer();
		List<NamedDssPath> extra = oldDssContainer.getExtraDssFiles().stream().map(e -> copyDssToNewProjectFolder(newProjectPath, oldProjectPath, e)).collect(toList());
		NamedDssPath ivDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getIvDssFile());
		NamedDssPath dvDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getDvDssFile());
		NamedDssPath svDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getSvDssFile());
		NamedDssPath dtsDssPath = copyDssToNewProjectFolder(newProjectPath, oldProjectPath, oldDssContainer.getDtsDssFile());
		EpptDssContainer newDssContainer = new EpptDssContainer(dvDssPath, svDssPath, ivDssPath, dtsDssPath, extra);
		return new EpptScenarioRun(scenarioRun.getName(), scenarioRun.getDescription(), scenarioRun.getModel(),
				makeRelativeToNewProject(scenarioRun.getOutputPath(), newProjectPath, oldProjectPath),
				makeRelativeToNewProject(scenarioRun.getWreslDirectory(), newProjectPath, oldProjectPath),
				makeRelativeToNewProject(scenarioRun.getLookupDirectory(), newProjectPath, oldProjectPath), newDssContainer, scenarioRun.getColor());
	}

	private NamedDssPath copyDssToNewProjectFolder(Path newProjectPath, Path oldProjectPath, NamedDssPath dvDssFile)
	{
		return new NamedDssPath(makeRelativeToNewProject(dvDssFile.getDssPath(), newProjectPath, oldProjectPath), dvDssFile.getAliasName(), dvDssFile.getAPart(),
				dvDssFile.getEPart(), dvDssFile.getFPart());
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

	private void launchFileDialogToAddScenario()
	{
		ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(Frame.getFrames()[0], _controller.getScenarioRuns());
		scenarioRunEditor.setVisible(true);
		scenarioRunEditor.dispose();
		EpptScenarioRun scenarioRun = scenarioRunEditor.createRun();
		if(scenarioRun != null && !scenarioRunEditor.isCanceled())
		{
			addScenarioRun(scenarioRun);
			setModified();
		}
	}

	private void launchFileDialogToCopyScenario()
	{
		EpptScenarioRun selectedScenarioRun = getSelectedScenario();
		if(selectedScenarioRun != null)
		{
			SwingUtilities.invokeLater(() ->
			{
				Color plotlyDefaultColor = Constant.getColorNotInList(_scenarioTableModel.getAllScenarioRuns().stream().map(EpptScenarioRun::getColor).collect(toList()));
				ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(Frame.getFrames()[0], _controller.getScenarioRuns());
				scenarioRunEditor.fillPanelForCopy(selectedScenarioRun, plotlyDefaultColor);
				scenarioRunEditor.setVisible(true);
				EpptScenarioRun newScenarioRun = scenarioRunEditor.createRun();
				scenarioRunEditor.setVisible(false);
				if(newScenarioRun != null && !scenarioRunEditor.isCanceled())
				{
					scenarioRunEditor.dispose();
					Platform.runLater(() ->
					{
						addScenarioRun(newScenarioRun);
						setModified();
					});
				}
			});
		}
	}

	private void launchFileDialogToEditScenario()
	{
		EpptScenarioRun oldScenarioRun = getSelectedScenario();
		if(oldScenarioRun != null)
		{
			editScenarioRun(oldScenarioRun);
		}
	}

	private void editScenarioRun(EpptScenarioRun oldScenarioRun)
	{
		SwingUtilities.invokeLater(() ->
		{
			ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(Frame.getFrames()[0], _controller.getScenarioRuns());
			scenarioRunEditor.fillPanel(oldScenarioRun);
			scenarioRunEditor.setVisible(true);
			EpptScenarioRun newScenarioRun = scenarioRunEditor.createRun();
			scenarioRunEditor.setVisible(false);
			scenarioRunEditor.dispose();

			if(newScenarioRun != null && !scenarioRunEditor.isCanceled())
			{
				Platform.runLater(() -> updateScenario(oldScenarioRun, newScenarioRun));
			}
		});
	}

	private void tableSelected(ObservableValue<? extends TreeItem<ScenarioTableRowModel>> e, TreeItem<ScenarioTableRowModel> o, TreeItem<ScenarioTableRowModel> n)
	{
		if(n != null && n.getValue() != null)
		{
			ScenarioTableRowModel selectedRowModel = n.getValue();
			_editButton.setDisable(!(selectedRowModel instanceof ScenarioRowModel));
			_copyButton.setDisable(!(selectedRowModel instanceof ScenarioRowModel));
			_clearButton.setDisable(!(selectedRowModel instanceof ScenarioRowModel));
			_upButton.setDisable(!(selectedRowModel instanceof ScenarioRowModel));
			_downButton.setDisable(!(selectedRowModel instanceof ScenarioRowModel));
			if(selectedRowModel instanceof DssPathRow)
			{
				_controller.setSelectedDssPath(((DssPathRow) selectedRowModel).getDssPath());
			}
			else
			{
				_controller.setSelectedDssPath(null);
			}
		}
		else
		{
			_editButton.setDisable(true);
			_copyButton.setDisable(true);
			_clearButton.setDisable(true);
			_upButton.setDisable(true);
			_downButton.setDisable(true);
			_controller.setSelectedDssPath(null);
		}
	}

	public void reloadProject()
	{
		try
		{
			_modifiedListenerEnabled = false;
			_scenarioTableModel.getRows().clear();
			_controller.getScenarioRuns().forEach(this::addScenarioRun);
			_differenceCheckbox.setSelected(_controller.isDifference());
		}
		finally
		{
			_modifiedListenerEnabled = true;
		}
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
