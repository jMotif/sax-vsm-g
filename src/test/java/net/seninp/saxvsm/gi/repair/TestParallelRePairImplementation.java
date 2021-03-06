package net.seninp.saxvsm.gi.repair;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import org.junit.Test;
import net.seninp.jmotif.sax.NumerosityReductionStrategy;
import net.seninp.jmotif.sax.TSProcessor;
import net.seninp.jmotif.sax.datastructure.SAXRecord;
import net.seninp.jmotif.sax.datastructure.SAXRecords;
import net.seninp.jmotif.sax.parallel.ParallelSAXImplementation;

public class TestParallelRePairImplementation {

  private static final String TEST_DATASET_NAME = "test/data/ecg0606_1.csv";

  private static final Integer WINDOW_SIZE = 120;
  private static final Integer PAA_SIZE = 8;
  private static final Integer ALPHABET_SIZE = 6;

  private static final int THREADS_NUM = 3;

  private double[] ts1;

  /**
   * Test the simple SAX conversion.
   * 
   * @throws Exception if error occurs.
   */
  @Test
  public void testParallelRePairFullRun() throws Exception {

    ts1 = TSProcessor.readFileColumn(TEST_DATASET_NAME, 0, 0);

    ParallelSAXImplementation ps = new ParallelSAXImplementation();
    SAXRecords saxData = ps.process(ts1, 3, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE,
        NumerosityReductionStrategy.EXACT, 0.05);

    String inputString = saxData.getSAXString(" ");
    // System.out.println("Input string:\n" + inputString);
    saxData.buildIndex();

    // Date start = new Date();
    ParallelGrammarKeeper grammar = toGrammarKeeper(saxData);
    ParallelRePairImplementation pr = new ParallelRePairImplementation();
    ParallelGrammarKeeper res = pr.buildGrammar(grammar, THREADS_NUM);
    // System.out.println("RePair grammar:\n" + res.toGrammarRules());
    // System.out.println("Recovered string:\n" + res.r0ExpandedString);
    // System.out.println("Original SAX string:\n" + saxData.getSAXString(" "));

    assertNotNull(res);
    res.expandR0();
    assertTrue(inputString.trim().equalsIgnoreCase(res.r0ExpandedString.trim()));

  }

  private ParallelGrammarKeeper toGrammarKeeper(SAXRecords saxData) {
    ArrayList<Symbol> string = new ArrayList<Symbol>();
    for (int i = 0; i < saxData.size(); i++) {
      SAXRecord r = saxData.getByIndex(saxData.mapStringIndexToTSPosition(i));
      Symbol symbol = new Symbol(r, i);
      string.add(symbol);
    }
    // System.out.println("Converted str: " + stringToDisplay(string));

    ParallelGrammarKeeper gk = new ParallelGrammarKeeper(0);
    gk.setWorkString(string);
    return gk;
  }

  @SuppressWarnings("unused")
  private static String stringToDisplay(ArrayList<Symbol> string) {
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < string.size(); i++) {
      sb.append(string.get(i).toString()).append(" ");
    }
    return sb.toString();
  }
}
