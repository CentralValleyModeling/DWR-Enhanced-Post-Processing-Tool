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

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javafx.geometry.Side;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.CustomMenuItem;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-02-2020
 */
class AutoCompleteTextField<T> extends TextField
{
	//Local variables
	//entries to autocomplete
	private final SortedSet<T> _entries = new TreeSet<>(Comparator.comparing(Object::toString));
	//popup GUI
	private final ContextMenu _entriesPopup = new ContextMenu();
	private final BiPredicate<T, String> _predicate;
	private Consumer<T> _entryPicked;


	AutoCompleteTextField()
	{
		this((e, s) -> e.toString().toLowerCase().contains(s.toLowerCase()));
	}

	AutoCompleteTextField(BiPredicate<T, String> predicate)
	{
		_predicate = predicate;
		initListeners();
	}

	void setEntryPicked(Consumer<T> entryPicked)
	{
		_entryPicked = entryPicked;
	}

	private void initListeners()
	{
		//Add "suggestions" by changing text
		textProperty().addListener((observable, oldValue, newValue) -> textChanged());
		//Hide always by focus-in (optional) and out
		focusedProperty().addListener((observableValue, oldValue, newValue) -> _entriesPopup.hide());
	}

	private void textChanged()
	{
		String enteredText = getText();
		if(enteredText == null || enteredText.isEmpty())
		{
			_entriesPopup.hide();
		}
		else
		{
			List<T> filteredEntries = _entries.stream()
											  .filter(e -> _predicate.test(e, enteredText))
											  .collect(toList());
			if(!filteredEntries.isEmpty())
			{
				populatePopup(filteredEntries, enteredText);
				if(!_entriesPopup.isShowing())
				{
					_entriesPopup.show(AutoCompleteTextField.this, Side.BOTTOM, 0, 0); //position of popup
				}
				filteredEntries.stream()
							   .filter(s -> s.toString().equalsIgnoreCase(enteredText))
							   .findAny()
							   .ifPresent(this::entryPicked);
			}
			else
			{
				_entriesPopup.hide();
			}
		}
	}


	/**
	 * Populate the entry set with the given search results. Display is limited to 10 entries, for performance.
	 *
	 * @param searchResult The set of matching strings.
	 */
	private void populatePopup(List<T> searchResult, String enteredText)
	{
		//List of "suggestions"
		List<CustomMenuItem> menuItems = searchResult.stream()
													 .limit(10)
													 .map(entry -> buildMenuItems(enteredText, entry))
													 .collect(toList());
		//"Refresh" context menu
		_entriesPopup.getItems().clear();
		_entriesPopup.getItems().addAll(menuItems);
	}

	private CustomMenuItem buildMenuItems(String enteredText, T entry)
	{
		String result = entry.toString();
		Label entryLabel = new Label();
		entryLabel.setGraphic(buildTextFlow(result, enteredText));
		entryLabel.setPrefHeight(12);  //don't sure why it's changed with "graphic"
		CustomMenuItem item = new CustomMenuItem(entryLabel, true);
		item.setOnAction(actionEvent ->
		{
			setText(result);
			positionCaret(result.length());
			_entriesPopup.hide();
			entryPicked(entry);
		});
		return item;
	}

	private void entryPicked(T entry)
	{
		_entriesPopup.hide();
		_entryPicked.accept(entry);
	}


	/**
	 * Get the existing set of autocomplete entries.
	 *
	 * @return The existing autocomplete entries.
	 */
	public SortedSet<T> getEntries()
	{
		return _entries;
	}

	/**
	 * Build TextFlow with selected text. Return "case" dependent.
	 *
	 * @param text   - string with text
	 * @param filter - string to select in text
	 * @return - TextFlow
	 */
	private static TextFlow buildTextFlow(String text, String filter)
	{
		int filterIndex = text.toLowerCase().indexOf(filter.toLowerCase());
		Text textBefore = new Text(text.substring(0, filterIndex));
		Text textAfter = new Text(text.substring(filterIndex + filter.length()));
		Text textFilter = new Text(text.substring(filterIndex, filterIndex + filter.length())); //instead of "filter" to keep all "case sensitive"
		textFilter.setFill(Color.ORANGE);
		return new TextFlow(textBefore, textFilter, textAfter);
	}
}
