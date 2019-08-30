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

package gov.ca.water.reportengine.jython;

import java.time.Month;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;
import java.util.stream.IntStream;

import gov.ca.water.calgui.project.EpptScenarioRun;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class JythonValueGenerator
{
	private final PeriodFilter _periodFilter;
	private final EpptScenarioRun _scenarioRun;
	private final String _function;

	public JythonValueGenerator(EpptScenarioRun scenarioRun, String function)
	{
		this(input -> input, scenarioRun, function);
	}

	public JythonValueGenerator(PeriodFilter periodFilter,EpptScenarioRun base, String function)
	{
		_scenarioRun = base;
		_function = function;
		_periodFilter = periodFilter;
	}

	public Object generateObjectValue()
	{
		return Math.random() * 1000;
	}

	public Double generateValue()
	{
		return (double) ThreadLocalRandom.current().nextInt(-4, 4);
	}

	public List<Double> generateValues()
	{
		return IntStream.generate(() -> ThreadLocalRandom.current().nextInt(1, 3 + 1))
						.limit(100)
						.map(i -> i * 100)
						.mapToObj(i -> (double) i)
						.collect(toList());
	}

	public NavigableMap<Double, Double> generateExceedanceValues()
	{
		List<Double> collect = IntStream.generate(() -> ThreadLocalRandom.current().nextInt(1, 3 + 1))
										.limit(100)
										.map(i -> i * 100)
										.mapToObj(i -> (double) i).sorted()
										.collect(toList());
		return new TreeMap<>(IntStream.iterate(0, i -> i + 1)
									  .limit(100)
									  .mapToObj(i -> (double) i)
									  .collect(toMap(Function.identity(), i -> collect.get(((Double) i).intValue()))));
	}

	public Map<Integer, Double> generateAnnualValues()
	{
		List<Double> collect = IntStream.generate(() -> ThreadLocalRandom.current().nextInt(1, 3 + 1))
										.limit(100)
										.map(i -> i * 100)
										.mapToObj(i -> (double) i).sorted()
										.collect(toList());
		return new HashMap<>(IntStream.iterate(0, i -> i + 1)
									  .limit(75)
									  .boxed()
									  .collect(toMap(i -> i + 1921, collect::get)));
	}

	public Map<Month, Double> generateMonthlyValues()
	{
		List<Double> collect = Arrays.stream(Month.values())
									 .map(i -> (double)ThreadLocalRandom.current().nextInt(1, 6) * 200)
									 .collect(toList());
		return IntStream.iterate(0, i -> i + 1)
									  .limit(Month.values().length)
									  .boxed()
									  .collect(toMap(i->Month.values()[i], collect::get));
	}
}
