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

import com.rma.javafx.treetable.RmaTreeTableModel;
import com.rma.javafx.treetable.columns.specs.RmaTreeTableColumnSpec;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-07-2019
 */
class ScenarioTableModel extends RmaTreeTableModel<ScenarioTableRowModel>
{
	static final RmaTreeTableColumnSpec NAME_COL_SPEC;
	static final RmaTreeTableColumnSpec MODEL_COL_SPEC;
	static final RmaTreeTableColumnSpec DESCRIPTION_COL_SPEC;
	static final RmaTreeTableColumnSpec BASE_COL_SPEC;
	static final RmaTreeTableColumnSpec ALTERNATIVE_COL_SPEC;
	static final RmaTreeTableColumnSpec OUTPUT_PATH_COL_SPEC;
	static final RmaTreeTableColumnSpec WRESL_MAIN_COL_SPEC;
	static final RmaTreeTableColumnSpec DSS_PATH_COL_SPEC;
	static final RmaTreeTableColumnSpec TYPE_COL_SPEC;
	static final RmaTreeTableColumnSpec A_PART_COL_SPEC;
	static final RmaTreeTableColumnSpec F_PART_COL_SPEC;
	static final RmaTreeTableColumnSpec WATER_YEAR_PATH_COL_SPEC;

	static
	{
		NAME_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Name")
				.withCanBeHidden(false)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		MODEL_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Model")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		TYPE_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Type")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
		DESCRIPTION_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Description")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
		BASE_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Base")
				.withCanBeHidden(false)
				.withEditable(true)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		ALTERNATIVE_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Alternative")
				.withCanBeHidden(false)
				.withEditable(true)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		OUTPUT_PATH_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Study Main Directory")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
		WRESL_MAIN_COL_SPEC = new RmaTreeTableColumnSpec.Builder("EPPT WRESL Directory")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
		DSS_PATH_COL_SPEC = new RmaTreeTableColumnSpec.Builder("DSS Path")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
		A_PART_COL_SPEC = new RmaTreeTableColumnSpec.Builder("A Part")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
		F_PART_COL_SPEC = new RmaTreeTableColumnSpec.Builder("F Part")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
		WATER_YEAR_PATH_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Lookup Directory")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(false)
				.build();
	}

	ScenarioTableModel()
	{
		getColumnSpecs().add(NAME_COL_SPEC);
		getColumnSpecs().add(BASE_COL_SPEC);
		getColumnSpecs().add(ALTERNATIVE_COL_SPEC);
		getColumnSpecs().add(MODEL_COL_SPEC);
		getColumnSpecs().add(TYPE_COL_SPEC);
		getColumnSpecs().add(DESCRIPTION_COL_SPEC);
		getColumnSpecs().add(OUTPUT_PATH_COL_SPEC);
		getColumnSpecs().add(WRESL_MAIN_COL_SPEC);
		getColumnSpecs().add(WATER_YEAR_PATH_COL_SPEC);
		getColumnSpecs().add(DSS_PATH_COL_SPEC);
		getColumnSpecs().add(A_PART_COL_SPEC);
		getColumnSpecs().add(F_PART_COL_SPEC);
	}

	synchronized Optional<ScenarioRowModel> getRowForScenarioRun(EpptScenarioRun scenarioRun)
	{
		return getRows()
				.stream()
				.filter(r -> r instanceof ScenarioRowModel)
				.map(r -> (ScenarioRowModel) r)
				.filter(r -> r.getScenarioRun().equals(scenarioRun))
				.findAny();
	}

	synchronized EpptScenarioRun getBaseScenarioRun()
	{
		return getRows()
				.stream()
				.filter(r -> r instanceof ScenarioRowModel)
				.map(r -> (ScenarioRowModel) r)
				.filter(ScenarioRowModel::isBase)
				.findAny()
				.map(ScenarioRowModel::getScenarioRun)
				.orElse(null);
	}

	synchronized List<EpptScenarioRun> getAlternativeScenarioRuns()
	{
		return getRows()
				.stream()
				.filter(r -> r instanceof ScenarioRowModel)
				.map(r -> (ScenarioRowModel) r)
				.filter(ScenarioRowModel::isAlternative)
				.map(ScenarioRowModel::getScenarioRun)
				.collect(toList());
	}

	synchronized List<EpptScenarioRun> getAllScenarioRuns()
	{
		return getRows()
				.stream()
				.filter(r -> r instanceof ScenarioRowModel)
				.map(r -> (ScenarioRowModel) r)
				.map(ScenarioRowModel::getScenarioRun)
				.collect(toList());
	}
}
