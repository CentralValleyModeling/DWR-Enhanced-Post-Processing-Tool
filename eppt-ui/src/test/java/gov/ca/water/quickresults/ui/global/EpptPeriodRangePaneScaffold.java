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

package gov.ca.water.quickresults.ui.global;

import java.time.Month;
import java.util.Collections;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.stage.Stage;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 06-18-2020
 */
public class EpptPeriodRangePaneScaffold extends Application
{
	@Override
	public void start(Stage primaryStage) throws Exception
	{
		WaterYearPeriod waterYearPeriod = new WaterYearPeriod("");
		EpptPeriodRangePane epptPeriodRangePane = new EpptPeriodRangePane(new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER, 1, 1));
		Scene scene = new Scene(epptPeriodRangePane);
		primaryStage.setScene(scene);
		primaryStage.show();
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(1921, waterYearPeriod),
				new WaterYearType(2003, waterYearPeriod));
		WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter("", "", Collections.singletonList(waterYearPeriodRange),
				new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER, 1, 1));
		Platform.runLater(()->epptPeriodRangePane.fill(waterYearPeriodRangesFilter));
	}
}
