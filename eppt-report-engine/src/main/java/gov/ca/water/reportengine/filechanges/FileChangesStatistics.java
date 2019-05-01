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

package gov.ca.water.reportengine.assumptionchanges;

import java.util.HashSet;
import java.util.Set;

public class FileChangesStatistics
{

    private Set<String> _recordsOnlyInAlt;
    private Set<String> _recordsOnlyInBase;
    private Set<String> _changes;

    private FileChangesStatistics(Set<String> recordsOnlyInBase, Set<String> recordsOnlyInAlt, Set<String> changes)
    {
        _recordsOnlyInAlt = recordsOnlyInAlt;
        _recordsOnlyInBase = recordsOnlyInBase;
        _changes = changes;
    }

    public Set<String> getChanges()
    {
        return _changes;
    }

    public Set<String> getRecordsOnlyInAlt()
    {
        return _recordsOnlyInAlt;
    }

    public Set<String> getRecordsOnlyInBase()
    {
        return _recordsOnlyInBase;
    }


    public static class Builder
    {
        private Set<String> _recordsOnlyInAlt;
        private Set<String> _recordsOnlyInBase;
        private Set<String> _changes;

        public Builder()
        {
            _recordsOnlyInAlt = new HashSet<>();
            _recordsOnlyInBase = new HashSet<>();
            _changes = new HashSet<>();
        }

        public FileChangesStatistics build()
        {
            return new FileChangesStatistics(_recordsOnlyInBase, _recordsOnlyInAlt, _changes);
        }

        public Builder withRecordsOnlyInBase(Set<String> recsOnlyInBase)
        {
            _recordsOnlyInBase = recsOnlyInBase;
            return this;
        }

        public Builder withRecordsOnlyInAlt(Set<String> recsOnlyInAlt)
        {
            _recordsOnlyInAlt = recsOnlyInAlt;
            return this;
        }

        public Builder withFileChanges(Set<String> fileChanges)
        {
            _changes = fileChanges;
            return this;
        }
    }

}
