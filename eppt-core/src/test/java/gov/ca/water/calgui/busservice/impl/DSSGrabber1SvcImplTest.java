/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2026.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */
package gov.ca.water.calgui.busservice.impl;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.project.NamedDssPath;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers how {@link DSSGrabber1SvcImpl#getOneSeries} reports DSS files and
 * paths that cannot be used (issue #247).
 */
public class DSSGrabber1SvcImplTest {

    private static final GUILinksAllModelsBO.Model MODEL = GUILinksAllModelsBO.Model.findModel("CalSim2");

    /**
     * A file that exists but is not a valid DSS file must not crash the
     * grabber; the user should be told which file could not be accessed.
     */
    @Test
    void unreadableDssFileIsReportedInsteadOfFailingSilently() throws IOException {
        // A directory named *.dss exists (so the grabber tries to open it) but cannot be opened as a DSS file.
        Path badDss = Files.createTempDirectory("eppt-bad-").resolve("unopenable.dss");
        Files.createDirectory(badDss);
        try {
            NamedDssPath dssPath = new NamedDssPath(badDss, "alias", "A", "1MON", "F");

            DSSGrabber1SvcImpl grabber = new DSSGrabber1SvcImpl();
            assertNull(grabber.getOneSeries(dssPath, "B/C", MODEL));

            List<String> messages = grabber.getMissingList().get(MODEL);
            assertTrue(messages != null && messages.size() == 1, "Expected one reported problem for the model");
            assertTrue(messages.get(0).startsWith("Unable to access HEC-DSS file " + badDss),
                    "Message should name the DSS file: " + messages.get(0));
        } finally {
            // HEC may still hold the file open; don't let cleanup mask the test result.
            try {
                Files.deleteIfExists(badDss);
                Files.deleteIfExists(badDss.getParent());
            } catch (IOException cleanupFailure) {
                badDss.toFile().deleteOnExit();
                badDss.getParent().toFile().deleteOnExit();
            }
        }
    }

    /**
     * A path that is absent from one DSS file is not an error on its own,
     * because other files for the scenario may hold it. It must not be
     * reported.
     */
    @Test
    void pathMissingFromDssFileIsNotReported() throws URISyntaxException {
        Path validDss = Paths.get(getClass().getResource("/SampleDV_Base.dss").toURI());
        NamedDssPath dssPath = new NamedDssPath(validDss, "alias", "NO_SUCH_B", "1MON", "F");

        DSSGrabber1SvcImpl grabber = new DSSGrabber1SvcImpl();
        assertNull(grabber.getOneSeries(dssPath, "NO_SUCH_B/NO_SUCH_C", MODEL));

        Map<GUILinksAllModelsBO.Model, List<String>> missing = grabber.getMissingList();
        assertFalse(missing.containsKey(MODEL) && !missing.get(MODEL).isEmpty(),
                "A missing path in one file should not be reported: " + missing);
    }
}
