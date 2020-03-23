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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IFileSystemSvc;
import gov.ca.water.calgui.techservice.impl.FilePredicates;
import gov.ca.water.calgui.techservice.impl.FileSystemSvcImpl;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-13-2020
 */
public final class DssPatternUpdater
{
	private static final List<DssPatternMatcher> PATTERN_MATCHERS = new ArrayList<>();
	private static final Pattern CSV_PATTERN = Pattern.compile(",");
	private static final int PATTERN_TYPE_COL = 0;
	private static final int DSS_PATH_PATTERN_COL = 1;
	private static final int DSS_RECORD_TYPE_COL = 2;
	private static final int DSS_RECORD_UNITS_COL = 3;
	private static final int TYPE_REPLACEMENT_COL = 4;
	private static final int UNIT_REPLACEMENT_COL = 5;

	private DssPatternUpdater()
	{
		throw new AssertionError("Utility class");
	}

	public static void initPatterns() throws EpptInitializationException
	{
		PATTERN_MATCHERS.clear();
		PATTERN_MATCHERS.addAll(readPatterns());
	}

	private static List<DssPatternMatcher> readPatterns() throws EpptInitializationException
	{
		List<DssPatternMatcher> retval = new ArrayList<>();
		Path file = Paths.get(Constant.CONFIG_DIR).resolve("DssPatternReplacer.csv");
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		try
		{
			List<String> rows = fileSystemSvc.getFileData(file, true, FilePredicates.commentFilter());
			for(String row : rows)
			{
				row = row.replace("%2C", ",");
				String[] list = CSV_PATTERN.split(row);
				Pattern dssPath;
				Pattern dssType;
				Pattern dssUnits;
				if("glob".equalsIgnoreCase(list[PATTERN_TYPE_COL]))
				{
					dssPath = Pattern.compile(convertGlobToRegEx(list[DSS_PATH_PATTERN_COL]));
					dssType = Pattern.compile(convertGlobToRegEx(list[DSS_RECORD_TYPE_COL]));
					dssUnits = Pattern.compile(convertGlobToRegEx(list[DSS_RECORD_UNITS_COL]));
				}
				else
				{
					dssPath = Pattern.compile(list[DSS_PATH_PATTERN_COL]);
					dssType = Pattern.compile(list[DSS_RECORD_TYPE_COL]);
					dssUnits = Pattern.compile(list[DSS_RECORD_UNITS_COL]);
				}
				String typeReplacement = null;
				if(TYPE_REPLACEMENT_COL < list.length)
				{
					typeReplacement = list[TYPE_REPLACEMENT_COL];
				}
				String unitReplacement = null;
				if(UNIT_REPLACEMENT_COL < list.length)
				{
					unitReplacement = list[UNIT_REPLACEMENT_COL];
				}
				retval.add(new DssPatternMatcher(dssPath, dssType, dssUnits, typeReplacement, unitReplacement));
			}
		}
		catch(CalLiteGUIException | RuntimeException e)
		{
			throw new EpptInitializationException("Error reading DSS Pattern file: " + file, e);
		}
		return retval;
	}

	public static void updateTimeSeriesContainer(TimeSeriesContainer tsc)
	{
		Optional<DssPatternMatcher> first = PATTERN_MATCHERS.stream().filter(s -> s.matches(tsc)).findFirst();
		if(first.isPresent())
		{
			DssPatternMatcher dssPatternMatcher = first.get();
			if(dssPatternMatcher._typeReplacement != null && !dssPatternMatcher._typeReplacement.isEmpty())
			{
				tsc.setType(dssPatternMatcher._typeReplacement);
			}
			if(dssPatternMatcher._unitReplacement != null && !dssPatternMatcher._unitReplacement.isEmpty())
			{
				tsc.setUnits(dssPatternMatcher._unitReplacement);
			}
		}
	}

	//Code reused from StackOverflow user Paul Tomblin https://stackoverflow.com/users/3333/paul-tomblin
	//https://stackoverflow.com/questions/1247772/is-there-an-equivalent-of-java-util-regex-for-glob-type-patterns
	private static String convertGlobToRegEx(String pattern)
	{
		StringBuilder sb = new StringBuilder(pattern.length());
		int inGroup = 0;
		int inClass = 0;
		int firstIndexInClass = -1;
		char[] arr = pattern.toCharArray();
		for(int i = 0; i < arr.length; i++)
		{
			char ch = arr[i];
			switch(ch)
			{
				case '\\':
					if(++i >= arr.length)
					{
						sb.append('\\');
					}
					else
					{
						char next = arr[i];
						switch(next)
						{
							case ',':
								// escape not needed
								break;
							case 'Q':
							case 'E':
								// extra escape needed
								sb.append('\\');
							default:
								sb.append('\\');
						}
						sb.append(next);
					}
					break;
				case '*':
					if(inClass == 0)
					{
						sb.append(".*");
					}
					else
					{
						sb.append('*');
					}
					break;
				case '?':
					if(inClass == 0)
					{
						sb.append('.');
					}
					else
					{
						sb.append('?');
					}
					break;
				case '[':
					inClass++;
					firstIndexInClass = i + 1;
					sb.append('[');
					break;
				case ']':
					inClass--;
					sb.append(']');
					break;
				case '.':
				case '(':
				case ')':
				case '+':
				case '|':
				case '^':
				case '$':
				case '@':
				case '%':
					if(inClass == 0 || (firstIndexInClass == i && ch == '^'))
					{
						sb.append('\\');
					}
					sb.append(ch);
					break;
				case '!':
					if(firstIndexInClass == i)
					{
						sb.append('^');
					}
					else
					{
						sb.append('!');
					}
					break;
				case '{':
					inGroup++;
					sb.append('(');
					break;
				case '}':
					inGroup--;
					sb.append(')');
					break;
				case ',':
					if(inGroup > 0)
					{
						sb.append('|');
					}
					else
					{
						sb.append(',');
					}
					break;
				default:
					sb.append(ch);
			}
		}
		return sb.toString();
	}

	private static final class DssPatternMatcher
	{
		private final Pattern _dssPath;
		private final Pattern _dssType;
		private final Pattern _dssUnits;
		private final String _typeReplacement;
		private final String _unitReplacement;

		private DssPatternMatcher(Pattern dssPath, Pattern dssType, Pattern dssUnits, String typeReplacement, String unitReplacement)
		{

			_dssPath = dssPath;
			_dssType = dssType;
			_dssUnits = dssUnits;
			_typeReplacement = typeReplacement;
			_unitReplacement = unitReplacement;
		}

		public boolean matches(TimeSeriesContainer tsc)
		{
			boolean retval = false;
			if(tsc != null)
			{
				retval = _dssPath.matcher(tsc.getFullName()).matches();
				if(retval)
				{
					retval = _dssType.matcher(tsc.getType()).matches();
					if(retval)
					{
						retval = _dssUnits.matcher(tsc.getUnits()).matches();
					}
				}
			}
			return retval;
		}
	}
}
