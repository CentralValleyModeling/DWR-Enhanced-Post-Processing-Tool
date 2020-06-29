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

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;

import com.rma.javafx.iface.ColumnSpec;
import com.rma.javafx.treetable.RmaTreeTableView;
import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;
import com.rma.javafx.treetable.rows.TreeTableRowModel;
import com.rma.javafx.utils.JavaFxTreeTableViewUtils;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-05-2020
 */
final class TreeTableUtil
{

	private TreeTableUtil()
	{
		throw new AssertionError("utility class");
	}

	private static <R extends TreeTableRowModel> String copyValuesToString(RmaTreeTableView<?, R> treeTableView)
	{
		ObservableList<TreeTableColumnSpec> columnSpecs = treeTableView.getModel().getColumnSpecs();
		List<Object[]> objectArray = new ArrayList<>();
		List<String> headerRow = new ArrayList<>();
		List<String> subHeaderRow = new ArrayList<>();
		//This only handles a single row of child columns, this should be updated to use recursion in order to handle an infinite level of headers
		for(TreeTableColumnSpec spec : columnSpecs)
		{
			ObservableList<TreeTableColumnSpec> children = spec.getChildren();
			//Using do-while in order to ensure that if there are no children, the header column is printed
			int i = 0;
			if(treeTableView.getColumnFromSpec(spec).isVisible())
			{
				do
				{
					headerRow.add(spec.getColumnName());
					if(i < children.size())
					{
						TreeTableColumnSpec childSpec = children.get(i);
						if(treeTableView.getColumnFromSpec(childSpec).isVisible())
						{
							subHeaderRow.add(childSpec.getColumnName());
						}
						else
						{
							headerRow.remove(headerRow.size() - 1);
						}
					}
					if(i ==0 && children.isEmpty())
					{
						subHeaderRow.add("");
					}
					i++;
				} while(i < children.size());
			}
		}
		objectArray.add(headerRow.toArray());
		if(!subHeaderRow.isEmpty() && !subHeaderRow.stream().allMatch(String::isEmpty))
		{
			objectArray.add(subHeaderRow.toArray());
		}
		TreeItem<R> root = treeTableView.getRoot();
		ObservableList<TreeItem<R>> children = root.getChildren();
		//This only handles a single row of child columns, this should be updated to use recursion in order to handle an infinite level of sub columns
		List<TreeTableColumnSpec> childSpecs = columnSpecs.stream().map(spec ->
		{
			ObservableList<TreeTableColumnSpec> childColumns = spec.getChildren();
			if(childColumns.isEmpty())
			{
				childColumns = FXCollections.observableArrayList(spec);
			}
			return childColumns;
		}).flatMap(Collection::stream).collect(toList());
		addRowData(treeTableView, childSpecs, objectArray, children);
		Object[][] array = new Object[objectArray.size()][];
		for(int i = 0; i < array.length; i++)
		{
			array[i] = objectArray.get(i);
		}
		return JavaFxTreeTableViewUtils.convertToString(array, new String[0]);
	}

	private static <R extends TreeTableRowModel> void addRowData(RmaTreeTableView<?, R> treeTableView, List<TreeTableColumnSpec> columnSpecs,
																 List<Object[]> objectArray, ObservableList<TreeItem<R>> children)
	{
		for(TreeItem<R> item : children)
		{
			if(item != null && item.getValue() != null)
			{
				Object[] rowData = columnSpecs.stream()
											  .filter(spec -> treeTableView.getColumnFromSpec(spec).isVisible())
											  .map((Function<TreeTableColumnSpec, ObservableValue>) item.getValue()::getObservableValue)
											  .map(observableValue -> (observableValue == null) ? "" : observableValue.getValue())
											  .toArray();
				objectArray.add(rowData);
				addRowData(treeTableView, columnSpecs, objectArray, item.getChildren());
			}
		}
	}

	public static <R extends TreeTableRowModel> void copyValuesToClipboard(RmaTreeTableView<?, R> treeTableView)
	{
		String text = copyValuesToString(treeTableView);
		StringSelection selection = new StringSelection(text);
		Toolkit.getDefaultToolkit().getSystemClipboard().setContents(selection, (clipboard, data) ->
		{
		});
	}
}
