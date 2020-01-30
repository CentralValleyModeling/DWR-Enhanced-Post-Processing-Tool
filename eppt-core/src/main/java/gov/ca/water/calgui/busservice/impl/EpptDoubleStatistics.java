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

package gov.ca.water.calgui.busservice.impl;

import java.time.LocalDateTime;
import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collector;

import gov.ca.water.calgui.constant.Constant;

import hec.lang.annotation.Scriptable;

import static java.util.stream.Collectors.toList;

/**
 * Code taken from GitHub answer:
 * https://stackoverflow.com/questions/36263352/java-streams-standard-deviation
 * <p>
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-30-2020
 */
public class EpptDoubleStatistics extends DoubleSummaryStatistics
{
	private double _sumOfSquare = 0.0D;
	private double _sumOfSquareCompensation; // Low order bits of sum
	private double _simpleSumOfSquare; // Used to compute right sum for non-finite inputs

	@Scriptable
	public static Double calculateStandardDeviation(Map<LocalDateTime, Double> values)
	{
		return values.values().stream().filter(Objects::nonNull).filter(Constant::isValidValue)
					 .collect(Collector.of(EpptDoubleStatistics::new, EpptDoubleStatistics::accept,
							 EpptDoubleStatistics::combine, EpptDoubleStatistics::getStandardDeviation));
	}

	@Scriptable
	public static Double calculateMedian(Map<LocalDateTime, Double> values)
	{
		Double retval = null;
		List<Double> list = values.values().stream().filter(Objects::nonNull).filter(Constant::isValidValue).collect(toList());
		if(!list.isEmpty())
		{
			boolean even = list.size() % 2 == 0;
			if(even)
			{
				retval = list.stream().mapToDouble(d -> d).sorted().skip(list.size() / 2 - 1).limit(2).average().orElse(Double.NaN);
			}
			else
			{
				retval = list.stream().mapToDouble(d -> d).sorted().skip(list.size() / 2).findFirst().orElse(Double.NaN);
			}
		}
		return retval;
	}

	@Override
	public void accept(double value)
	{
		super.accept(value);
		double squareValue = value * value;
		_simpleSumOfSquare += squareValue;
		sumOfSquareWithCompensation(squareValue);
	}

	public EpptDoubleStatistics combine(EpptDoubleStatistics other)
	{
		super.combine(other);
		_simpleSumOfSquare += other._simpleSumOfSquare;
		sumOfSquareWithCompensation(other._sumOfSquare);
		sumOfSquareWithCompensation(other._sumOfSquareCompensation);
		return this;
	}

	private void sumOfSquareWithCompensation(double value)
	{
		double tmp = value - _sumOfSquareCompensation;
		double velvel = _sumOfSquare + tmp; // Little wolf of rounding error
		_sumOfSquareCompensation = (velvel - _sumOfSquare) - tmp;
		_sumOfSquare = velvel;
	}

	private double getSumOfSquare()
	{
		double tmp = _sumOfSquare + _sumOfSquareCompensation;
		if(Double.isNaN(tmp) && Double.isInfinite(_simpleSumOfSquare))
		{
			return _simpleSumOfSquare;
		}
		return tmp;
	}

	private final double getStandardDeviation()
	{
		return getCount() > 0 ? Math.sqrt((getSumOfSquare() / getCount()) - Math.pow(getAverage(), 2)) : 0.0d;
	}
}
