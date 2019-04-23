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

package gov.ca.water.reportdata.executivereport;

public class ModelInputs
{

    private final int _timeSeriesUpdatedInSV;
    private final int _filesDeleted;
    private final int _fileChanges;
    private final int _newFilesAdded;

    public ModelInputs(int timeSeriesUpdatedInSV, int fileChanges, int newFilesAdded, int filesDeleted)
    {
        _timeSeriesUpdatedInSV = timeSeriesUpdatedInSV;
        _fileChanges = fileChanges;
        _newFilesAdded = newFilesAdded;
        _filesDeleted = filesDeleted;
    }


    public int getTimeSeriesUpdatedInSV()
    {
        return _timeSeriesUpdatedInSV;
    }

    public int getFilesDeleted()
    {
        return _filesDeleted;
    }

    public int getFileChanges()
    {
        return _fileChanges;
    }
    public int getNewFilesAdded()
    {
        return _newFilesAdded;
    }

}
