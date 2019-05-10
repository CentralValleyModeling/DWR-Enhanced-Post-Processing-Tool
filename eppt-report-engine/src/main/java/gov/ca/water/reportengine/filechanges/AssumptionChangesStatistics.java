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

package gov.ca.water.reportengine.filechanges;

import java.nio.file.Path;
import java.util.Set;

public class AssumptionChangesStatistics
{

    private final Set<String> _recordsOnlyInAlt;
    private final Set<String> _recordsOnlyInBase;
    private final Set<String> _changedFiles;

    AssumptionChangesStatistics(Set<String> recordsOnlyInBase, Set<String> recordsOnlyInAlt, Set<String> filesUpdates)
    {
        _recordsOnlyInAlt = recordsOnlyInAlt;
        _recordsOnlyInBase = recordsOnlyInBase;
        _changedFiles = filesUpdates;
    }

    public Set<String> getChangedFiles()
    {
        return _changedFiles;
    }

    public Set<String> getRecordsOnlyInAlt()
    {
        return _recordsOnlyInAlt;
    }

    public Set<String> getRecordsOnlyInBase()
    {
        return _recordsOnlyInBase;
    }

}
