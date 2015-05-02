package edu.hawaii.jmotif.gi;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import net.seninp.jmotif.distance.EuclideanDistance;
import net.seninp.jmotif.sax.NumerosityReductionStrategy;
import net.seninp.jmotif.sax.SAXException;
import net.seninp.jmotif.sax.SAXProcessor;
import net.seninp.jmotif.sax.alphabet.Alphabet;
import net.seninp.jmotif.sax.alphabet.NormalAlphabet;
import net.seninp.jmotif.sax.datastructures.SAXRecords;
import edu.hawaii.jmotif.direct.TfIdfEntryComparator;
import edu.hawaii.jmotif.gi.repair.BagConstructionStrategy;
import edu.hawaii.jmotif.gi.repair.RePairFactory;
import edu.hawaii.jmotif.gi.repair.RePairRule;
import edu.hawaii.jmotif.gi.sequitur.SequiturFactory;
import edu.hawaii.jmotif.logic.Interval;
import edu.hawaii.jmotif.logic.RuleInterval;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.util.UCRUtils;

public class RePairConcatenationTinker {

  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat df = new DecimalFormat("0.00###", otherSymbols);
  private static final String CR = "\n";

//  private static final String TRAIN_DATASET_NAME = "data/cbf/CBF_TRAIN";
//  private static final String TEST_DATASET_NAME = "data/cbf/CBF_TEST";
   private static final String TRAIN_DATASET_NAME = "data/synthetic_control/synthetic_control_TRAIN";
   private static final String TEST_DATASET_NAME = "data/synthetic_control/synthetic_control_TEST";

  private static final double NORMALIZATION_THRESHOLD = 0.001;
  private static final NumerosityReductionStrategy STRATEGY = NumerosityReductionStrategy.NONE;
  private static final BagConstructionStrategy BAG_STRATEGY = BagConstructionStrategy.COMPRESSED;

  private static final Alphabet na = new NormalAlphabet();
  private static final TextUtils tu = new TextUtils();
  private static final SAXProcessor sp = new SAXProcessor();

  private static final int MAX_SERIES_2PRINT = 12;
  private static final int MAX_PATTERNS_2PRINT = 1;

  /**
   * Test the simple SAX conversion.
   * 
   * @throws Exception if error occurs.
   */
  public static void main(String[] args) throws Exception {

    // [0.0] - read the data in
    Map<String, List<double[]>> trainData = UCRUtils.readUCRData(TRAIN_DATASET_NAME);
    Map<String, List<double[]>> testData = UCRUtils.readUCRData(TEST_DATASET_NAME);

    Map<String, List<double[]>> reducedData = new HashMap<String, List<double[]>>();

    int tsLength = trainData.values().iterator().next().get(0).length;

    // [1.0] - make arrays of params
    int[] windows = new int[3];
    for (int i = 0; i < 3; i++) {
      windows[i] = (int) (tsLength * (0.2 * (i + 1)));
    }
    // int[] windows = {10, 50, 100 };
    int[] paas = { 3, 7 };
    int[] alphabets = { 3, 7 };

    // [2.0] - get a data to work with
    for (Entry<String, List<double[]>> testSet : trainData.entrySet()) {

      String classLabel = testSet.getKey();
      List<double[]> series = testSet.getValue();

      // compute the length of the new time-series and identify the break points
      int tl = 0;
      for (double[] arr : series) {
        tl = tl + arr.length;
      }

      // compose the time-series
      double[] ts = new double[tl];
      int globalI = 0;
      for (double[] arr : series) {
        for (int i = 0; i < arr.length; i++) {
          ts[globalI] = arr[i];
          globalI++;
        }
      }

      // global coverage
      double[] globalCoverage = new double[ts.length];

      for (int w : windows) {
        for (int p : paas) {
          for (int a : alphabets) {

            // skips
            ArrayList<Integer> skips = new ArrayList<Integer>();
            tl = 0;
            for (double[] arr : series) {
              tl = tl + arr.length;
              for (int i = tl - w; i < tl; i++) {
                skips.add(i);
              }
            }

            // grammar
            System.out.println(w + ", " + p + ", " + a);
            SAXRecords saxData = sp.ts2saxViaWindowSkipping(ts, w, p, na.getCuts(a), STRATEGY,
                NORMALIZATION_THRESHOLD, skips);
            saxData.buildIndex();
            RePairRule rePairGrammar = RePairFactory.buildGrammarWithSkips(saxData, skips);
            RePairRule.expandRules();
            RePairRule.buildIntervals(saxData, ts, w);
            GrammarRules rules = RePairRule.toGrammarRulesData();

            // the coverage array
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

            // normalize
            int mm = tu.max(coverageArray);
            double[] coverage = new double[coverageArray.length];
            for (int i = 0; i < coverageArray.length; i++) {
              coverage[i] = (double) coverageArray[i] / (double) mm;
            }

            // update global coverage
            for (int i = 0; i < globalCoverage.length; i++) {
              globalCoverage[i] = globalCoverage[i] + coverage[i];
            }

          }
        }
      }

      // saving the coverage
      //
      String currentPath = new File(".").getCanonicalPath();
      BufferedWriter bw = new BufferedWriter(new FileWriter(new File(currentPath + File.separator
          + "repair_coverage" + classLabel + ".txt")));
      for (double i : globalCoverage) {
        bw.write(i + "\n");
      }
      bw.close();

      // get the median
      //
      double[] tmp = Arrays.copyOf(globalCoverage, globalCoverage.length);
      Arrays.sort(tmp);
      double median;
      if (tmp.length % 2 == 0)
        median = ((double) tmp[tmp.length / 2] + (double) tmp[tmp.length / 2 - 1]) / 2;
      else
        median = (double) tmp[tmp.length / 2];

      // get a set of intervals above the median
      //
      // time series concatenation disassembly and intervals extraction starts here
      //
      int currPos = 0;
      for (double[] arr : series) {
        int start = currPos;
        int end = currPos + arr.length;
        // find the longest interval
        //
        Interval longest = getLongestInterval(globalCoverage, start, end, median);
        if (null != longest) {
          if (!reducedData.containsKey(classLabel)) {
            reducedData.put(classLabel, new ArrayList<double[]>());
          }
          List<double[]> collection = reducedData.get(classLabel);
          collection.add(Arrays.copyOfRange(ts, longest.getStart(), longest.getEnd()));
        }
        currPos = end;
      }

    }

    int totalPositiveTests = 0;
    int totalTestSample = 0;
    // #### here we iterate over all TEST series, class by class, series by series
    //
    for (Entry<String, List<double[]>> querySet : testData.entrySet()) {
      // for (Entry<String, List<double[]>> querySet : reducedData.entrySet()) {
      for (double[] querySeries : querySet.getValue()) {

        // this holds the closest neighbor out of all training data with its class
        //
        double bestDistance = Double.MAX_VALUE;
        String bestClass = "";

        // #### here we iterate over all TRAIN series, class by class, series by series
        //
        // for (Entry<String, List<double[]>> referenceSet : trainData.entrySet()) {
        for (Entry<String, List<double[]>> referenceSet : reducedData.entrySet()) {
          for (double[] referenceSeries : referenceSet.getValue()) {
            // this computes the Euclidean distance.
            // earlyAbandonedDistance implementation abandons full distance computation
            // if current value is above the best known
            //
            Double distance = EuclideanDistance.earlyAbandonedAnyPlaceDistance(referenceSeries,
                querySeries, bestDistance);
            // Double distance = EuclideanDistance.earlyAbandonedDistance(
            // TSUtils.zNormalize(querySeries), TSUtils.zNormalize(referenceSeries), bestDistance);

            if (null != distance && distance.doubleValue() < bestDistance) {
              bestDistance = distance.doubleValue();
              bestClass = referenceSet.getKey();
              // consoleLogger.fine(" + closest class: " + bestClass + " distance: " +
              // bestDistance);
            }
            else {
              // consoleLogger.fine(" - abandoned search for class: " + referenceSet.getKey()
              // + ", distance: " + EuclideanDistance.distance(querySeries, referenceSeries));

              // consoleLogger.fine(" - abandoned search for class: "
              // + referenceSet.getKey()
              // + ", distance: "
              // + EuclideanDistance.distance(TSUtils.zNormalize(querySeries),
              // TSUtils.zNormalize(referenceSeries)));

            }

          }
        }

        if (bestClass.equalsIgnoreCase(querySet.getKey())) {
          totalPositiveTests++;
        }

        totalTestSample++;
      }
    }

    double accuracy = (double) totalPositiveTests / (double) totalTestSample;
    double error = 1.0d - accuracy;

    System.out.println(accuracy + "," + error + "\n");

  }

  private static Interval getLongestInterval(double[] ts, int start, int end, double median) {

    ArrayList<Interval> intervals = new ArrayList<Interval>();

    int cstart = 0;
    int cend = 0;
    boolean aboveThreshold = false;

    if (ts[start] > median) {
      aboveThreshold = true;
    }

    for (int i = start + 1; i < end; i++) {
      if (!(aboveThreshold) && ts[i] >= median) {
        aboveThreshold = true;
        cstart = i;
      }
      if (aboveThreshold && ts[i] < median) {
        aboveThreshold = false;
        cend = i;
        intervals.add(new Interval(cstart, cend, 0));
      }
    }
    if (intervals.isEmpty()) {
      return null;
    }
    Collections.sort(intervals, new Comparator<Interval>() {
      @Override
      public int compare(final Interval a, Interval b) {
        // TODO return 1 if a should be before b
        if (a.getLength() < b.getLength()) {
          return 1;
        }
        // return -1 if a should be before b
        else if (a.getLength() > b.getLength()) {
          return -1;
        }
        // return 0 otherwise
        return 0;
      }
    });
    return intervals.get(0);
  }
}
