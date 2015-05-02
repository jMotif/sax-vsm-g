package edu.hawaii.jmotif.gi.repair;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.junit.Test;
import edu.hawaii.jmotif.gi.GrammarRuleRecord;
import edu.hawaii.jmotif.gi.GrammarRules;
import edu.hawaii.jmotif.logic.RuleInterval;
import edu.hawaii.jmotif.sax.NumerosityReductionStrategy;
import edu.hawaii.jmotif.sax.SAXProcessor;
import edu.hawaii.jmotif.sax.alphabet.Alphabet;
import edu.hawaii.jmotif.sax.alphabet.NormalAlphabet;
import edu.hawaii.jmotif.sax.datastructures.SAXRecords;
import edu.hawaii.jmotif.util.UCRUtils;

public class TestRePairConcatenation {

  private static final String TEST_DATASET_NAME = "data/cbf/CBF_TRAIN";

  private static final Integer WINDOW_SIZE = 40;
  private static final Integer PAA_SIZE = 6;
  private static final Integer ALPHABET_SIZE = 6;

  private static final Alphabet na = new NormalAlphabet();

  private static final NumerosityReductionStrategy STRATEGY = NumerosityReductionStrategy.NONE;

  private static final double NORMALIZATION_THRESHOLD = 0.001;

  private static final SAXProcessor sp = new SAXProcessor();

  /**
   * Test the simple SAX conversion.
   * 
   * @throws Exception if error occurs.
   */
  @Test
  public void testRePairImplementation() throws Exception {

    // [0.0] - read the data in
    Map<String, List<double[]>> data = UCRUtils.readUCRData(TEST_DATASET_NAME);

    // [0.1] - get a data to work with
    for (Entry<String, List<double[]>> testSet : data.entrySet()) {

      String classLabel = testSet.getKey();
      System.out.println("Processing the class " + classLabel);

      // compute the length of the new time-series and identify the break points
      ArrayList<Integer> skips = new ArrayList<Integer>();
      int tl = 0;
      for (double[] arr : testSet.getValue()) {
        tl = tl + arr.length;
        for (int i = tl - WINDOW_SIZE; i < tl; i++) {
          skips.add(i);
        }
      }

      // compose the time-series
      double[] ts = new double[tl];
      int globalI = 0;
      for (double[] arr : testSet.getValue()) {
        for (int i = 0; i < arr.length; i++) {
          ts[globalI] = arr[i];
          globalI++;
        }
      }

      // convert the ts to list of SAX words
      SAXRecords saxData = sp.ts2saxViaWindowSkipping(ts, WINDOW_SIZE, PAA_SIZE,
          na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD, skips);

      // no clean-up those that overlap
      saxData.buildIndex();

      // build the grammar
      @SuppressWarnings("unused")
      RePairRule rePairGrammar = RePairFactory.buildGrammarWithSkips(saxData, skips);
      RePairRule.expandRules();
      RePairRule.buildIntervals(saxData, ts, WINDOW_SIZE);
      GrammarRules rules = RePairRule.toGrammarRulesData();

      // get the coverage array
      //
      int[] coverageArray = new int[ts.length];
      for (GrammarRuleRecord rule : rules) {
        if (0 == rule.ruleNumber()) {
          continue;
        }
        ArrayList<RuleInterval> arrPos = rule.getRuleIntervals();
        for (RuleInterval saxPos : arrPos) {
          int startPos = saxPos.getStartPos();
          int endPos = saxPos.getEndPos();
          for (int j = startPos; j < endPos; j++) {
            coverageArray[j] = coverageArray[j] + 1;
          }
        }
      }

      // saving the coverage
      //
      String currentPath = new File(".").getCanonicalPath();
      BufferedWriter bw = new BufferedWriter(new FileWriter(new File(currentPath + File.separator
          + "coverage" + classLabel + ".txt")));
      for (int i : coverageArray) {
        bw.write(i + "\n");
      }
      bw.close();
    }
  }

}
