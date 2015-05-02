package edu.hawaii.jmotif.gi;

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
import edu.hawaii.jmotif.logic.RuleInterval;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.util.UCRUtils;

public class ConcatenationTinker {

  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat df = new DecimalFormat("0.00###", otherSymbols);
  private static final String CR = "\n";

  private static final String TEST_DATASET_NAME = "data/cbf/CBF_TRAIN";

  private static final Integer WINDOW_SIZE = 40;
  private static final Integer PAA_SIZE = 6;
  private static final Integer ALPHABET_SIZE = 6;

  private static final Alphabet na = new NormalAlphabet();

  private static final NumerosityReductionStrategy STRATEGY = NumerosityReductionStrategy.NONE;

  private static final double NORMALIZATION_THRESHOLD = 0.001;

  private static final SAXProcessor sp = new SAXProcessor();

  private static final TextUtils tu = new TextUtils();

  private static final BagConstructionStrategy BAG_STRATEGY = BagConstructionStrategy.COMPRESSED;

  private static final int MAX_SERIES_2PRINT = 12;
  private static final int MAX_PATTERNS_2PRINT = 1;

  /**
   * Test the simple SAX conversion.
   * 
   * @throws Exception if error occurs.
   */
  public static void main(String[] args) throws Exception {

    // [0.0] - read the data in
    Map<String, List<double[]>> data = UCRUtils.readUCRData(TEST_DATASET_NAME);

    // make a map of resulting bags
    Map<String, WordBag> res = new HashMap<String, WordBag>();

    // [0.1] - get a data to work with
    for (Entry<String, List<double[]>> testSet : data.entrySet()) {
      WordBag bag = toRePairWordBag(testSet.getValue());
      bag.setLabel(testSet.getKey());
      res.put(testSet.getKey(), bag);
    }

    HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDFInstrumented(res.values());
    // HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDF(res.values());

    // get best patterns for each class printed
    //

    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      // sort the class' patterns
      //
      String className = e.getKey();
      ArrayList<Entry<String, Double>> values = new ArrayList<Entry<String, Double>>();
      values.addAll(e.getValue().entrySet());
      Collections.sort(values, new TfIdfEntryComparator());

      // print the class' key
      //
      System.out.print("Class key: " + className + CR);

      // while stopping criterion isn't met, iterate
      //
      for (int i = 0; i < MAX_PATTERNS_2PRINT; i++) {

        // the pattern we working with
        String pattern = values.get(i).getKey();
        Double weight = values.get(i).getValue();
        System.out.println("pattern=\"" + pattern + "\"; weight=" + df.format(weight));

        // init buffers
        StringBuffer seriesBuff = new StringBuffer("series = c(");
        StringBuffer startsBuff = new StringBuffer("starts = c(");
        StringBuffer stopsBuff = new StringBuffer("stops = c(");

        Map<Integer, Map<String, Integer[]>> hits = getPatternLocationsForTheClass(className, data,
            pattern, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE);

        int k = 0;
        int printedK = 0;
        do {
          if (hits.get(k).get("starts").length > 0) {
            System.out.println(k + ": " + Arrays.toString(hits.get(k).get("starts")));
            for (int j = 0; j < hits.get(k).get("starts").length; j++) {
              seriesBuff.append(String.valueOf(k + 1) + ",");
              startsBuff.append(String.valueOf(hits.get(k).get("starts")[j] + 1) + ",");
              stopsBuff.append(String.valueOf(hits.get(k).get("stops")[j] + 1) + ",");
            }
            printedK++;
          }
          k++;
        }
        while (k < hits.size() && printedK < MAX_SERIES_2PRINT);

        System.out.print(seriesBuff.delete(seriesBuff.length() - 1, seriesBuff.length()).toString()
            + ")" + CR);
        System.out.print(startsBuff.delete(startsBuff.length() - 1, startsBuff.length()).toString()
            + ")" + CR);
        System.out.print(stopsBuff.delete(stopsBuff.length() - 1, stopsBuff.length()).toString()
            + ")" + CR + "#" + CR);

      }
    }

    // classifying
    int testSampleSize = 0;
    int positiveTestCounter = 0;
    for (String label : tfidf.keySet()) {

      List<double[]> testD = data.get(label);

      for (double[] series : testD) {
        WordBag test = RePairFactory.seriesToGrammarWordBag("tmp", series, WINDOW_SIZE, PAA_SIZE,
            na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD,
            BagConstructionStrategy.REDUCED);
        String testLabel = tu.classify(test, tfidf);

        if (label.equalsIgnoreCase(testLabel)) {
          positiveTestCounter++;

          HashMap<String, Double> ref = tfidf.get(testLabel);

        }
        testSampleSize++;

      }
    }

    // accuracy and error
    // double accuracy = (double) positiveTestCounter / (double) testSampleSize;
    // double error = 1.0d - accuracy;

  }

  private static WordBag toRePairWordBag(List<double[]> series) throws SAXException {

    WordBag bag = new WordBag("res");

    // compute the length of the new time-series and identify the break points
    ArrayList<Integer> skips = new ArrayList<Integer>();
    int tl = 0;
    for (double[] arr : series) {
      tl = tl + arr.length;
      for (int i = tl - WINDOW_SIZE; i < tl; i++) {
        skips.add(i);
      }
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

    // infer the grammar
    SAXRecords saxData = sp.ts2saxViaWindowSkipping(ts, WINDOW_SIZE, PAA_SIZE,
        na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD, skips);
    saxData.buildIndex();

    RePairRule rePairGrammar = RePairFactory.buildGrammarWithSkips(saxData, skips);
    RePairRule.expandRules();
    RePairRule.buildIntervals(saxData, ts, WINDOW_SIZE);

    GrammarRules rules = RePairRule.toGrammarRulesData();

    // get words out
    for (GrammarRuleRecord r : rules) {
      // if the strategy is REDUCED we shall be adding basic tokens here
      //
      if (0 == r.getRuleNumber()) {
        if (BagConstructionStrategy.REDUCED == BAG_STRATEGY) {
          // extracting all basic tokens
          //
          GrammarRuleRecord r0 = rules.get(0);
          String[] split = r0.getRuleString().trim().split("\\s");
          for (String s : split) {
            if (s.startsWith("R")) {
              continue;
            }
            bag.addWord(s);
          }
        }
      }
      else {
        // adding the expanded rule
        //
        String str = r.getExpandedRuleString();
        bag.addWord(str, r.getOccurrences().size());
      }
    }

    return bag;
  }

  private int[] getCoverage(List<double[]> series) throws Exception {

    // String classLabel = testSet.getKey();
    // System.out.println("Processing the class " + classLabel);

    // compute the length of the new time-series and identify the break points
    ArrayList<Integer> skips = new ArrayList<Integer>();
    int tl = 0;
    for (double[] arr : series) {
      tl = tl + arr.length;
      for (int i = tl - WINDOW_SIZE; i < tl; i++) {
        skips.add(i);
      }
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

    GrammarRules rules = SequiturFactory.series2SequiturRulesWithSkip(ts, WINDOW_SIZE, PAA_SIZE,
        ALPHABET_SIZE, STRATEGY, NORMALIZATION_THRESHOLD, skips);

    // SAXRecords saxData = sp.ts2saxViaWindowSkipping(ts, WINDOW_SIZE, PAA_SIZE,
    // na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD, skips);
    // saxData.buildIndex();
    // RePairRule rePairGrammar = RePairFactory.buildGrammarWithSkips(saxData, skips);
    // RePairRule.expandRules();
    // RePairRule.buildIntervals(saxData, ts, WINDOW_SIZE);
    // GrammarRules rules = RePairRule.toGrammarRulesData();

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

    ArrayList<GrammarRuleRecord> recs = new ArrayList<GrammarRuleRecord>(rules.getRecords());

    Collections.sort(recs, new Comparator<GrammarRuleRecord>() {
      @Override
      public int compare(final GrammarRuleRecord a, GrammarRuleRecord b) {
        // TODO return 1 if a should be before b
        if (a.getOccurrences().size() < b.getOccurrences().size()) {
          return 1;
        }
        // return -1 if a should be before b
        else if (a.getOccurrences().size() > b.getOccurrences().size()) {
          return -1;
        }
        // return 0 otherwise
        return 0;
      }
    });

    GrammarRuleRecord rr = recs.get(0);
    ArrayList<RuleInterval> ints = rr.getRuleIntervals();
    int[] ocs = new int[ints.size()];
    for (int k = 0; k < ints.size(); k++) {
      ocs[k] = ints.get(k).getStartPos();
    }
    System.out.println("Most frequent rule " + rr.getRuleName() + " \'" + rr.getRuleString()
        + "\', occurences: " + Arrays.toString(ocs));

    // saving the coverage
    //
    // String currentPath = new File(".").getCanonicalPath();
    // BufferedWriter bw = new BufferedWriter(new FileWriter(new File(currentPath + File.separator
    // + "sequitur_coverage" + classLabel + ".txt")));
    // for (int i : coverageArray) {
    // bw.write(i + "\n");
    // }
    // bw.close();

    return coverageArray;

  }

  private static Map<Integer, Map<String, Integer[]>> getPatternLocationsForTheClass(
      String className, Map<String, List<double[]>> trainData, String pattern, int windowSize,
      int paaSize, int alphabetSize) throws Exception {

    // the resulting map
    Map<Integer, Map<String, Integer[]>> res = new HashMap<Integer, Map<String, Integer[]>>();

    // series counter
    int seriesCounter = 0;
    // iterating over class' data
    for (double[] ts : trainData.get(className)) {

      // get the SAX conversion out
      SAXRecords saxData = sp.ts2saxViaWindow(ts, windowSize, paaSize, na.getCuts(alphabetSize),
          STRATEGY, NORMALIZATION_THRESHOLD);
      saxData.buildIndex();
      String str = saxData.getSAXString(" ");

      // search for pattern's hits on the raw string
      List<Integer> starts = new ArrayList<Integer>();
      List<Integer> stops = new ArrayList<Integer>();
      int idx = str.indexOf(pattern, 0);
      while (idx > -1) {
        // if there is a hit
        // [1] extract the string position
        int spaceCount = idx - str.substring(0, idx + 1).replaceAll(" ", "").length();
        // [2] convert this into the real time series index
        starts.add(saxData.mapStringIndexToTSPosition(spaceCount + 1));
        // [3] the length of this pattern
        int patternSpaceCount = pattern.length() - pattern.replaceAll(" ", "").length();
        stops.add(saxData.mapStringIndexToTSPosition(spaceCount + 1 + patternSpaceCount)
            + windowSize);
        // update the current hit index
        idx = str.indexOf(pattern, idx + 1);
      }

      Map<String, Integer[]> entry = new HashMap<String, Integer[]>();
      entry.put("starts", starts.toArray(new Integer[starts.size()]));
      entry.put("stops", stops.toArray(new Integer[stops.size()]));
      res.put(seriesCounter, entry);
      seriesCounter++;
    }

    return res;
  }

}
