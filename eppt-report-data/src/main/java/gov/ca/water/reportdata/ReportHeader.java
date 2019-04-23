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

package gov.ca.water.reportdata;

import sun.util.resources.cldr.en.CalendarData_en_AU;

import java.util.List;

public class ReportHeader
{
    private final String _author;
    private final String _baseFile;
    private final List<String> _alternatives;
    private final String _subTitle = "EPPT Prototype";

    public ReportHeader(String author, String baseFile, List<String> alternatives)
    {
        _author = author;
        _baseFile = baseFile;
        _alternatives = alternatives;
    }

    public String getAuthor()
    {
        return _author;
    }

    public String getBaseFile()
    {
        return _baseFile;
    }

    public List<String> getAlternativeNames()
    {
        return _alternatives;
    }

    public String getSubTitle()
    {
        return _subTitle;
    }
}
