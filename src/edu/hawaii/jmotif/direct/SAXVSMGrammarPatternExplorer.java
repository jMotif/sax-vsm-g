package edu.hawaii.jmotif.direct;

import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import edu.hawaii.jmotif.repair.GrammarRuleRecord;
import edu.hawaii.jmotif.repair.GrammarRules;
import edu.hawaii.jmotif.repair.RePairFactory;
import edu.hawaii.jmotif.repair.RePairRule;
import edu.hawaii.jmotif.sax.NumerosityReductionStrategy;
import edu.hawaii.jmotif.sax.alphabet.Alphabet;
import edu.hawaii.jmotif.sax.alphabet.NormalAlphabet;
import edu.hawaii.jmotif.sax.datastructures.SAXRecords;
import edu.hawaii.jmotif.sax.datastructures.SaxRecord;
import edu.hawaii.jmotif.sax.parallel.ParallelSAXImplementation;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.util.UCRUtils;

/**
 * Hunts for best scoring patterns for the class and prints them out.
 * 
 * @author psenin
 * 
 */
public class SAXVSMGrammarPatternExplorer {

  // defines the amount of words from each class's vector to print
  //
  private static final int MAX_WORDS_2_PRINT = 10;

  // how many patterns and how many series to output
  //
  private static final int MAX_SERIES_2PRINT = 5;
  private static final int MAX_PATTERNS_2PRINT = 10;

  private static String TRAINING_DATA;
  private static String TEST_DATA;

  private static Integer WINDOW_SIZE;
  private static Integer PAA_SIZE;
  private static Integer ALPHABET_SIZE;
  private static Map<String, List<double[]>> trainData;
  private static Map<String, List<double[]>> testData;
  private static SAXNumerosityReductionStrategy STRATEGY;

  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat df = new DecimalFormat("0.00###", otherSymbols);

  private static int[] params;
  private static final String COMMA = ", ";
  private static final String CR = "\n";

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMGrammarPatternExplorer.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  /**
   * @param args
   * @throws TSException
   * @throws IndexOutOfBoundsException
   * @throws IOException
   */
  public static void main(String[] args) throws IndexOutOfBoundsException, TSException, IOException {

    // parsing the parameters
    //
    try {
      // args: <train dataset>, <test dataset>, Wsize , Psize, Asize, Startegy
      consoleLogger.info("processing parameters: " + Arrays.toString(args));

      TRAINING_DATA = args[0];
      TEST_DATA = args[1];
      trainData = UCRUtils.readUCRData(TRAINING_DATA);
      consoleLogger.info("trainData classes: " + trainData.size() + ", series length: "
          + trainData.entrySet().iterator().next().getValue().get(0).length);
      for (Entry<String, List<double[]>> e : trainData.entrySet()) {
        consoleLogger.info(" training class: " + e.getKey() + " series: " + e.getValue().size());
      }

      testData = UCRUtils.readUCRData(TEST_DATA);
      consoleLogger.info("testData classes: " + testData.size() + ", series length: "
          + testData.entrySet().iterator().next().getValue().get(0).length);
      for (Entry<String, List<double[]>> e : testData.entrySet()) {
        consoleLogger.info(" test class: " + e.getKey() + " series: " + e.getValue().size());
      }

      WINDOW_SIZE = Integer.valueOf(args[2]);
      PAA_SIZE = Integer.valueOf(args[3]);
      ALPHABET_SIZE = Integer.valueOf(args[4]);

      STRATEGY = SAXNumerosityReductionStrategy.valueOf(args[5].toUpperCase());

      params = new int[] { WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE, STRATEGY.index() };

    }
    catch (Exception e) {
      System.err.println("There was parameters error....");
      System.exit(-10);
    }

    int[] params = new int[] { WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE, STRATEGY.index() };

    List<WordBag> bags = labeledSeries2GrammarWordBags(trainData, params);

    // get tfidf statistics
    HashMap<String, HashMap<String, Double>> tfidf = TextUtils.computeTFIDF(bags);

    // normalize all vectors to a unit - so it will be fair comparison
    tfidf = TextUtils.normalizeToUnitVectors(tfidf);

    // sort words by their weight and print top 10 of these for each class
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {
      String className = e.getKey();
      ArrayList<Entry<String, Double>> values = new ArrayList<Entry<String, Double>>();
      values.addAll(e.getValue().entrySet());

      Collections.sort(values, new TfIdfEntryComparator());

      System.out.print("Class key: " + className + CR);
      for (int i = 0; i < MAX_WORDS_2_PRINT; i++) {
        String pattern = values.get(i).getKey();
        Double weight = values.get(i).getValue();
        System.out.println("\"" + pattern + "\", " + weight);
      }

    }

    // get best patterns for each class
    //
    // iterating over each of classes
    //
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      // class name and weights vector
      String className = e.getKey();
      ArrayList<Entry<String, Double>> values = new ArrayList<Entry<String, Double>>();
      values.addAll(e.getValue().entrySet());

      // form the output
      Collections.sort(values, new TfIdfEntryComparator());
      System.out.print("Class key: " + className + CR);

      for (int i = 0; i < MAX_PATTERNS_2PRINT; i++) {

        String pattern = values.get(i).getKey();
        Double weight = values.get(i).getValue();
        System.out.println("pattern=\"" + pattern + "\"; weight=" + df.format(weight));

        StringBuffer seriesBuff = new StringBuffer("series = c(");
        StringBuffer offsetBuff = new StringBuffer("offsets = c(");
        Map<Integer, Integer[]> hits = getPatternLocationsForTheClass(className, trainData,
            pattern, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE);

        int k = 0;
        int printedK = 0;
        do {
          if (hits.get(k).length > 0) {
            System.out.print(k + ": " + Arrays.toString(hits.get(k)) + ", ");
            System.out.println(ArraytoString(trainData.get(className).get(k)));
            System.out.println(ArraytoString(seriesValuesAsHeat(trainData.get(className).get(k),
                className, tfidf, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE)));
            for (int offset : hits.get(k)) {
              seriesBuff.append(String.valueOf(k + 1) + ",");
              offsetBuff.append(String.valueOf(offset + 1) + ",");
            }
            printedK++;
          }
          k++;
        }
        while (k < hits.size() && printedK < MAX_SERIES_2PRINT);

        System.out.print(seriesBuff.delete(seriesBuff.length() - 1, seriesBuff.length()).toString()
            + ")" + CR);
        System.out.print(offsetBuff.delete(offsetBuff.length() - 1, offsetBuff.length()).toString()
            + ")" + CR + "#" + CR);

      }

      System.out.print("Missclassified for Class key: " + className + CR);
      List<double[]> testD = testData.get(className);
      int seriesIdx = 0;
      for (double[] series : testD) {
        int classificationResult = classifyGrammar(className, series, tfidf, params);
        if (0 == classificationResult) {
          System.out.println(seriesIdx + 1);
        }
        seriesIdx++;
      }
      System.out.println(" ============== ");
    }

  }

  private static String ArraytoString(double[] ds) {
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    for (double d : ds) {
      sb.append(df.format(d)).append(COMMA);
    }
    sb.delete(sb.length() - 2, sb.length()-1);
    sb.append("]");
    return sb.toString();
  }

  private static double[] seriesValuesAsHeat(double[] series, String className,
      HashMap<String, HashMap<String, Double>> tfidf, int window_size, int paa_size,
      int alphabet_size) throws TSException {

    Alphabet a = new NormalAlphabet();

    double[] weights = new double[series.length];
    HashMap<String, Integer> words = new HashMap<String, Integer>();

    for (int i = 0; i <= series.length - window_size; i++) {
      double[] subseries = TSUtils.subseries(series, i, window_size);
      double[] paa = TSUtils.paa(TSUtils.zNormalize(subseries), paa_size);
      char[] sax = TSUtils.ts2String(paa, a.getCuts(alphabet_size));
      words.put(String.valueOf(sax), i);
    }

    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {
      for (Entry<String, Double> e1 : e.getValue().entrySet()) {
        if (words.containsKey(e1.getKey())) {
          int idx = words.get(e1.getKey());
          double increment = 0.0;
          if (className.equalsIgnoreCase(e.getKey())) {
            increment = e1.getValue();
          }
          else {
            // increment = -e1.getValue();
          }

          for (int i = 0; i < window_size; i++) {
            weights[idx + i] = weights[idx + i] + increment;
          }
        }
      }
    }

    return weights;

  }

  private static Map<Integer, Integer[]> getPatternLocationsForTheClass(String className,
      Map<String, List<double[]>> trainData, String pattern, int windowSize, int paaSize,
      int alphabetSize) throws IndexOutOfBoundsException, TSException {

    Alphabet a = new NormalAlphabet();

    Map<Integer, Integer[]> res = new HashMap<Integer, Integer[]>();

    int seriesCounter = 0;
    for (double[] series : trainData.get(className)) {

      List<Integer> arr = new ArrayList<Integer>();

      ParallelSAXImplementation ps = new ParallelSAXImplementation();
      SAXRecords saxData = ps.process(series, 1, windowSize, paaSize, alphabetSize,
          NumerosityReductionStrategy.fromValue(STRATEGY.index()), 0.05);
      saxData.buildIndex();

      @SuppressWarnings("unused")
      RePairRule rePairGrammar = RePairFactory.buildGrammar(saxData);
      RePairRule.expandRules();
      GrammarRules rules = RePairRule.toGrammarRulesData();

      for (GrammarRuleRecord r : rules) {
        if (0 == r.getRuleNumber()) {
          // extracting all basic tokens
          for (SaxRecord sr : saxData) {
            if (pattern.equalsIgnoreCase(String.valueOf(sr.getPayload()))) {
              arr.addAll(sr.getIndexes());
            }
          }
        }
        else {
          // extracting all longer tokens
          String str = r.getExpandedRuleString();
          if (pattern.equalsIgnoreCase(String.valueOf(str))) {
            arr.addAll(r.getOccurrences());
          }
        }
      }

      for (int i = 0; i <= series.length - windowSize; i++) {
        double[] paa = TSUtils.paa(TSUtils.zNormalize(TSUtils.subseries(series, i, windowSize)),
            paaSize);
        char[] sax = TSUtils.ts2String(paa, a.getCuts(alphabetSize));
        if (pattern.equalsIgnoreCase(String.valueOf(sax))) {
          arr.add(i);
        }
      }

      res.put(seriesCounter, arr.toArray(new Integer[0]));
      seriesCounter++;
    }

    return res;
  }

  private static int classifyGrammar(String classKey, double[] series,
      HashMap<String, HashMap<String, Double>> tfidf, int[] params)
      throws IndexOutOfBoundsException, TSException {

    WordBag test = seriesToGrammarWordBag("test", series, params);

    // it is Cosine similarity,
    //
    // which ranges from 0.0 for the angle of 90 to 1.0 for the angle of 0
    // i.e. LARGES value is a SMALLEST distance
    double minDist = Double.MIN_VALUE;
    String className = "";
    double[] cosines = new double[tfidf.entrySet().size()];

    int index = 0;
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      double dist = TextUtils.cosineSimilarity(test, e.getValue());
      cosines[index] = dist;
      index++;

      if (dist > minDist) {
        className = e.getKey();
        minDist = dist;
      }

    }

    // sometimes, due to the VECTORs specific layout, all values are the same, NEED to take care
    boolean allEqual = true;
    double cosine = cosines[0];
    for (int i = 1; i < cosines.length; i++) {
      if (!(cosines[i] == cosine)) {
        allEqual = false;
      }
    }

    // report our findings
    if (!(allEqual) && className.equalsIgnoreCase(classKey)) {
      return 1;
    }

    // System.out.println("all equal " + allEqual + ", assigned to " + className + " instead of " +
    // classKey);

    return 0;
  }

  private static List<WordBag> labeledSeries2GrammarWordBags(Map<String, List<double[]>> data,
      int[] params) throws IndexOutOfBoundsException, TSException {
    // make a map of resulting bags
    Map<String, WordBag> preRes = new HashMap<String, WordBag>();

    // process series one by one building word bags
    for (Entry<String, List<double[]>> e : data.entrySet()) {

      String classLabel = e.getKey();
      WordBag bag = new WordBag(classLabel);

      for (double[] series : e.getValue()) {
        WordBag cb = seriesToGrammarWordBag("tmp", series, params);
        bag.mergeWith(cb);
      }

      preRes.put(classLabel, bag);
    }

    List<WordBag> res = new ArrayList<WordBag>();
    res.addAll(preRes.values());
    return res;
  }

  private static WordBag seriesToGrammarWordBag(String label, double[] series, int[] params)
      throws IndexOutOfBoundsException, TSException {

    int windowSize = params[0];
    int paaSize = params[1];
    int alphabetSize = params[2];
    NumerosityReductionStrategy strategy = NumerosityReductionStrategy.fromValue(params[3]);

    WordBag resultBag = new WordBag(label);

    ParallelSAXImplementation ps = new ParallelSAXImplementation();
    SAXRecords saxData = ps.process(series, 1, windowSize, paaSize, alphabetSize, strategy, 0.05);
    saxData.buildIndex();

    @SuppressWarnings("unused")
    RePairRule rePairGrammar = RePairFactory.buildGrammar(saxData);
    RePairRule.expandRules();
    GrammarRules rules = RePairRule.toGrammarRulesData();

    for (GrammarRuleRecord r : rules) {
      if (0 == r.getRuleNumber()) {
        // extracting all basic tokens
        for (SaxRecord sr : saxData) {
          resultBag.addWord(String.valueOf(sr.getPayload()), sr.getIndexes().size());
        }
      }
      else {
        // extracting all longer tokens
        String str = r.getExpandedRuleString();
        resultBag.addWord(str);
      }
    }

    // System.out.println("Strategy: " + strategy.index());

    return resultBag;
  }

}
