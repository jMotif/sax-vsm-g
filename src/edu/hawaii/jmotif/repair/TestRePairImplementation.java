package edu.hawaii.jmotif.repair;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import edu.hawaii.jmotif.sax.NumerosityReductionStrategy;
import edu.hawaii.jmotif.sax.TSProcessor;
import edu.hawaii.jmotif.sax.datastructures.SAXRecords;
import edu.hawaii.jmotif.sax.parallel.ParallelSAXImplementation;

public class TestRePairImplementation {

  private static final String TEST_DATASET_NAME = "test/data/ecg0606_1.csv";

  private static final Integer WINDOW_SIZE = 220;
  private static final Integer PAA_SIZE = 8;
  private static final Integer ALPHABET_SIZE = 6;

  private double[] ts1;

  /**
   * Test the simple SAX conversion.
   * 
   * @throws Exception if error occurs.
   */
  @Test
  public void testRePairImplementation() throws Exception {

    ts1 = TSProcessor.readFileColumn(TEST_DATASET_NAME, 0, 0);

    ParallelSAXImplementation ps = new ParallelSAXImplementation();
    SAXRecords saxData = ps.process(ts1, 3, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE,
        NumerosityReductionStrategy.EXACT, 0.05);

    String inputString = saxData.getSAXString(" ");
    // System.out.println("Input string:\n" + inputString);
    saxData.buildIndex();

    // Date start = new Date();

    RePairRule grammar = RePairFactory.buildGrammar(saxData);
    // Date grammarEnd = new Date();

    RePairRule.expandRules();
    // Date expandEnd = new Date();

    String recoveredString = RePairRule.recoverString();
//    System.out.println("RePair grammar:\n" + RePairRule.toGrammarRules());
//    System.out.println("Recovered string:\n" + recoveredString);
//    System.out.println("Original SAX string:\n" + saxData.getSAXString(" "));

    assertNotNull(grammar);
    assertTrue(inputString.trim().equalsIgnoreCase(recoveredString.trim()));

  }

}
