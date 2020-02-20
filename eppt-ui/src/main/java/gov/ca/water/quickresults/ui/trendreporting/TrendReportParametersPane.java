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

package gov.ca.water.quickresults.ui.trendreporting;

import java.awt.Frame;
import java.lang.reflect.InvocationTargetException;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.TrendReportingParameters;
import gov.ca.water.calgui.busservice.impl.TrendType;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.TilePane;
import javafx.scene.layout.VBox;

import static java.util.stream.Collectors.toCollection;
import static java.util.stream.Collectors.toSet;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-02-2020
 */
public class TrendReportParametersPane extends TitledPane
{
	private static final Logger LOGGER = Logger.getLogger(TrendReportParametersPane.class.getName());
	private final ListView<TrendReportingParameters.TrendParameter> _parameterListView = new ListView<>();
	private final ListView<TrendType> _typeListView = new ListView<>();
	private final Set<TrendType> _trendTypes = getTrendTypes();
	private FilteredList<TrendReportingParameters.TrendParameter> _filteredParameters;
	private ObservableList<TrendReportingParameters.TrendParameter> _backingParameters;
	private final AutoCompleteTextField<TrendReportingParameters.TrendParameter> _textField = new AutoCompleteTextField<>();
	private Predicate<TrendReportingParameters.TrendParameter> _searchPredicate = s -> true;
	private Predicate<TrendReportingParameters.TrendParameter> _typePredicate = s -> true;

	public TrendReportParametersPane()
	{
		initComponents();
		initListeners();
	}

	private void initComponents()
	{
		_typeListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		_parameterListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		TilePane innerPane = new TilePane(Orientation.VERTICAL, 5, 5, buildTypeListView(), buildParameterListView());
		setContent(innerPane);
		BorderPane borderPane = new BorderPane();
		setGraphicTextGap(0);
		Label parametersLabel = new Label("Parameters");
		borderPane.setLeft(parametersLabel);
		_textField.setPrefWidth(420);
		_textField.setPromptText(_backingParameters.get(0).toString());
		_textField.getEntries().addAll(_backingParameters);
		borderPane.setCenter(_textField);
		BorderPane.setMargin(parametersLabel, new Insets(5));
		setGraphic(borderPane);
		setPrefWidth(525);
		setPrefHeight(200);
		_trendTypes.removeIf(t -> _backingParameters.stream().noneMatch(n -> t.matchesGuiLink(n.getGuiLink())));
		updateTrendTypes();
		_typeListView.getSelectionModel().select(0);
	}

	private Pane buildParameterListView()
	{
		TrendReportingParameters trendReportingParameters = TrendReportingParameters.getTrendReportingParameters();
		_backingParameters = FXCollections.observableArrayList(trendReportingParameters.getTrendParameters());
		_filteredParameters = new FilteredList<>(_backingParameters, s -> true);
		_parameterListView.setItems(_filteredParameters);
		_parameterListView.getSelectionModel().select(0);
		BorderPane borderPane = new BorderPane();
		Label parameterLabel = new Label("Parameter Name");
		parameterLabel.setPrefHeight(20);
		borderPane.setLeft(parameterLabel);
		Button addButton = new Button("Add");
		addButton.setOnAction(this::addParameter);
		borderPane.setRight(addButton);
		VBox vBox = new VBox(5, borderPane, _parameterListView);
		vBox.setPrefHeight(200);
		return vBox;
	}

	private void initListeners()
	{
		_textField.textProperty().addListener((observable, oldValue, newValue) -> textChanged());
		_textField.setEntryPicked(obj ->
		{
			_parameterListView.getSelectionModel().select(obj);
			_parameterListView.getSelectionModel().getSelectedItem();
		});
	}

	private void textChanged()
	{
		String text = _textField.getText();
		if(text.trim().isEmpty())
		{
			_searchPredicate = s -> true;
		}
		else
		{
			_searchPredicate = s -> s.toString().toLowerCase().contains(text.toLowerCase());
		}
		TrendReportingParameters.TrendParameter selectedItem = _parameterListView.getSelectionModel().getSelectedItem();
		_filteredParameters.setPredicate(_typePredicate.and(_searchPredicate));
		if(selectedItem != null && _filteredParameters.contains(selectedItem))
		{
			_parameterListView.getSelectionModel().select(selectedItem);
		}
	}

	private void updateTrendTypes()
	{
		TrendType selectedItem = _typeListView.getSelectionModel().getSelectedItem();
		_trendTypes.removeIf(t -> t.getTitle() == null || t.getTitle().isEmpty());
		_typeListView.getItems().clear();
		_typeListView.getItems().add(TrendType.ALL_TREND_TYPE);

		if(_backingParameters.stream().anyMatch(s -> TrendType.MISC_TREND_TYPE.matchesGuiLink(s.getGuiLink())))
		{
			_typeListView.getItems().add(TrendType.MISC_TREND_TYPE);
		}
		if(_backingParameters.stream().anyMatch(s -> TrendType.USER_DEFINED_TREND_TYPE.matchesGuiLink(s.getGuiLink())))
		{
			_typeListView.getItems().add(TrendType.USER_DEFINED_TREND_TYPE);
		}
		_typeListView.getItems().addAll(_trendTypes);
		if(selectedItem != null)
		{
			_typeListView.getSelectionModel().select(selectedItem);
		}
	}

	private void addParameter(ActionEvent e)
	{
		try
		{
			GUILinksAllModelsBO guiLink = createGuiLink();
			if(guiLink != null)
			{
				//Both elements should be added to the list
				updateTrendTypes();
				int size = _backingParameters.size();
				_backingParameters.add(TrendReportingParameters.TrendParameter.create(size, guiLink, ""));
			}
		}
		catch(InterruptedException ex)
		{
			LOGGER.log(Level.FINE, "Swing thread interrupted. Unable to create new parameter", ex);
			Thread.currentThread().interrupt();
		}
		catch(InvocationTargetException | RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Error creating new parameter", ex);
		}
	}

	private GUILinksAllModelsBO createGuiLink() throws InvocationTargetException, InterruptedException
	{
		AddParameterDialog addParameterDialog = new AddParameterDialog(Frame.getFrames()[0]);
		SwingUtilities.invokeAndWait(() -> addParameterDialog.setVisible(true));
		if(!addParameterDialog.isCanceled())
		{
			String type = addParameterDialog.getDataType();
			String parameter = addParameterDialog.getParameter();
			Map<GUILinksAllModelsBO.Model, String> bAndCParts = addParameterDialog.getBAndCParts();
			GUILinksAllModelsBO guiLink = new GUILinksAllModelsBO("", "", parameter, "", type);
			bAndCParts.forEach((key, value) -> guiLink.addModelMapping(key.toString(), value, ""));
			return guiLink;
		}
		else
		{
			return null;
		}
	}

	private void typeSelected(ObservableValue<? extends TrendType> e, TrendType o, TrendType n)
	{
		_typePredicate = trendParameter ->
		{
			boolean retval = false;
			if(n != null)
			{
				retval = n.matchesGuiLink(trendParameter.getGuiLink());
			}
			return retval;
		};
		_filteredParameters.setPredicate(_typePredicate.and(_searchPredicate));
	}

	private Pane buildTypeListView()
	{
		_typeListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		_typeListView.getSelectionModel().select(0);
		_typeListView.getSelectionModel().selectedItemProperty().addListener(this::typeSelected);
		BorderPane borderPane = new BorderPane();
		Label typeLabel = new Label("Type Name");
		typeLabel.setPrefHeight(20);
		borderPane.setLeft(typeLabel);
		//Invisible spacer button
		Button addButton = new Button("");
		borderPane.setRight(addButton);
		addButton.setVisible(false);
		VBox vBox = new VBox(5, borderPane, _typeListView);
		vBox.setPrefHeight(200);
		return vBox;
	}

	public void addListener(ChangeListener<? super TrendReportingParameters.TrendParameter> inputsChanged)
	{
		_parameterListView.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
		{
			inputsChanged.changed(observable, oldValue, newValue);
			if(newValue != null)
			{
				_textField.setPromptText(newValue.toString());
			}
		});
	}

	private static Set<TrendType> getTrendTypes()
	{
		return GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance()
									  .getAllGuiLinks()
									  .stream()
									  .map(TrendType::new)
									  .collect(toCollection(() -> new TreeSet<>(Comparator.comparing(TrendType::getTitle))));
	}

	public ObservableList<TrendReportingParameters.TrendParameter> getSelectedItems()
	{
		return _parameterListView.getSelectionModel().getSelectedItems();
	}
}
