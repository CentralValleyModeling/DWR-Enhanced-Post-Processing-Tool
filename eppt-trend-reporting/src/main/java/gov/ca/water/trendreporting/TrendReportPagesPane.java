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

package gov.ca.water.trendreporting;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.xml.parsers.ParserConfigurationException;

import gov.ca.water.calgui.constant.Constant;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.HBox;
import org.xml.sax.SAXException;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-01-2020
 */
class TrendReportPagesPane extends HBox
{
	private static final Logger LOGGER = Logger.getLogger(TrendReportPagesPane.class.getName());
	private final Runnable _runnable;
	private final ComboBox<TrendReportTabConfig> _comboBox = new ComboBox<>();
	private final Button _leftButton = new Button("<");
	private final Button _rightButton = new Button(">");

	TrendReportPagesPane(Runnable updateTrigger)
	{
		_runnable = updateTrigger;
		initComponents();
		initListeners();
	}

	private void initComponents()
	{
		try(Stream<Path> paths = Files.walk(Paths.get(Constant.TREND_REPORTING_DIR), 1))
		{
			paths.filter(path -> path.toString().endsWith(".htm") || path.toString().endsWith(".html"))
				 .map(this::buildTrendReportTabConfig)
				 .filter(Objects::nonNull)
				 .forEach(_comboBox.getItems()::add);
			_comboBox.getSelectionModel().select(0);
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to build javascript panels", ex);
		}
		_comboBox.setPrefHeight(32);
		_leftButton.setPrefHeight(32);
		_rightButton.setPrefHeight(32);
		getChildren().addAll(_comboBox, _leftButton, _rightButton);
		HBox.setMargin(_comboBox, new Insets(0, 5, 0, 10));
	}

	private TrendReportTabConfig buildTrendReportTabConfig(Path path)
	{
		try
		{
			return new TrendReportTabConfig(path);
		}
		catch(IOException | RuntimeException | ParserConfigurationException | SAXException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to extract toggle button text from: " + path, ex);
			return null;
		}
	}

	TrendReportTabConfig getTrendReportTabConfig()
	{
		return _comboBox.getValue();
	}

	private void initListeners()
	{
		_comboBox.setOnAction(e -> _runnable.run());
		_rightButton.setOnAction(e ->
		{
			int selectedIndex = _comboBox.getSelectionModel().getSelectedIndex() + 1;
			if(selectedIndex >= _comboBox.getItems().size())
			{
				selectedIndex = 0;
			}
			_comboBox.getSelectionModel().select(selectedIndex);
		});
		_leftButton.setOnAction(e ->
		{
			int selectedIndex = _comboBox.getSelectionModel().getSelectedIndex() - 1;
			if(selectedIndex < 0)
			{
				selectedIndex = _comboBox.getItems().size() - 1;
			}
			_comboBox.getSelectionModel().select(selectedIndex);
		});
	}
}
