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
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTablePosition;

import com.rma.javafx.iface.ColumnSpec;
import com.rma.javafx.treetable.RmaTreeTableView;
import com.rma.javafx.treetable.TreeTableViewSelectionUtilities;
import com.rma.javafx.treetable.columns.RmaTreeTableColumn;
import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;
import com.rma.javafx.treetable.rows.TreeTableRowModel;
import com.rma.javafx.utils.JavaFxTreeTableViewUtils;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-05-2020
 */
class TreeTableUtil
{


	private static <R extends TreeTableRowModel> String copyValuesToString(RmaTreeTableView<?, R> treeTableView)
	{
		ObservableList<TreeTableColumnSpec> columnSpecs = treeTableView.getModel().getColumnSpecs();
		List<Object[]> objectArray = new ArrayList<>();
		objectArray.add(columnSpecs.stream().map(ColumnSpec::getColumnName).toArray());
		TreeItem<R> root = treeTableView.getRoot();
		ObservableList<TreeItem<R>> children = root.getChildren();
		addRowData(columnSpecs, objectArray, children);
		Object[][] array = new Object[objectArray.size()][];
		for(int i = 0; i < array.length; i++)
		{
			array[i] = objectArray.get(i);
		}
		return JavaFxTreeTableViewUtils.convertToString(array, new String[0]);
	}

	private static <R extends TreeTableRowModel> void addRowData(ObservableList<TreeTableColumnSpec> columnSpecs, List<Object[]> objectArray,
																 ObservableList<TreeItem<R>> children)
	{
		for(TreeItem<R> item : children)
		{
			if(item != null && item.getValue() != null)
			{
				Object[] rowData = columnSpecs.stream().map(
						(Function<TreeTableColumnSpec, ObservableValue>) item.getValue()::getObservableValue)
											  .map(observableValue -> (observableValue == null)? "" : observableValue.getValue()).toArray();
				objectArray.add(rowData);
				addRowData(columnSpecs, objectArray, item.getChildren());
			}
		}
	}

	public static <R extends TreeTableRowModel> void copyValuesToClipboard(RmaTreeTableView<?, R> treeTableView)
	{
		String text = copyValuesToString(treeTableView);
		StringSelection selection = new StringSelection(text);
		Toolkit.getDefaultToolkit().getSystemClipboard().setContents(selection, (clipboard, data) -> {});
	}
}
