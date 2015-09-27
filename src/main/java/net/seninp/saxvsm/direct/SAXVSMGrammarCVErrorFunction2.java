package net.seninp.saxvsm.direct;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import net.seninp.jmotif.sax.NumerosityReductionStrategy;
import net.seninp.jmotif.sax.alphabet.Alphabet;
import net.seninp.jmotif.sax.alphabet.NormalAlphabet;
import net.seninp.saxvsm.gi.repair.BagConstructionStrategy;
import net.seninp.saxvsm.text.TextUtils;
import net.seninp.saxvsm.text.WordBag;
import net.seninp.util.StackTrace;

/**
 * This computes the cross-validation classification error given the set of parameters.
 * 
 * @author psenin
 */
public class SAXVSMGrammarCVErrorFunction2 implements AbstractErrorFunction {

  private static final double DEFAULT_NORMALIZATION_THRESHOLD = 0.05;

  public static final Character DELIMITER = '~';

  private Alphabet na = new NormalAlphabet();

  // the default normalization threshold
  private double NORMALIZATION_THRESHOLD = DEFAULT_NORMALIZATION_THRESHOLD;

  // the numerosity reduction strategy
  private NumerosityReductionStrategy numerosityReductionStrategy;

  // the bag construction strategy
  private BagConstructionStrategy bagStrategy;

  // the data
  private Map<String, double[]> tsData;

  // the hold out sample size
  private int holdOutSampleSize;

  private TextUtils tu;

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMGrammarCVErrorFunction2.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  /**
   * Constructor.
   * 
   * @param data
   * @param holdOutSampleSize
   */
  public SAXVSMGrammarCVErrorFunction2(Map<String, List<double[]>> data, int holdOutSampleSize,
      NumerosityReductionStrategy strategy, double normalizationThresholdValue,
      BagConstructionStrategy bagStrategyValue) {

    this.tsData = new HashMap<String, double[]>();
    this.tu = new TextUtils();

    for (Entry<String, List<double[]>> e : data.entrySet()) {
      String classLabel = e.getKey();
      int classCounter = 0;
      for (double[] series : e.getValue()) {
        this.tsData.put(classLabel + DELIMITER + classCounter, series);
        classCounter++;
      }
    }

    this.holdOutSampleSize = holdOutSampleSize;
    this.numerosityReductionStrategy = strategy;
    this.NORMALIZATION_THRESHOLD = normalizationThresholdValue;
    this.bagStrategy = bagStrategyValue;
  }

  /**
   * Computes the value at point.
   * 
   * @param point
   * @return
   */
  public double valueAt(Point point) {

    // point is in fact a aset of parameters - window, paa, and the alphabet
    //
    double[] coords = point.toArray();
    int windowSize = Long.valueOf(Math.round(coords[0])).intValue();
    int paaSize = Long.valueOf(Math.round(coords[1])).intValue();
    int alphabetSize = Long.valueOf(Math.round(coords[2])).intValue();

    // if we stepped above window length with PAA size - for some reason - return the max possible
    // error value
    if (paaSize > windowSize) {
      return 1.0d;
    }

    // the whole thing begins here
    //
    try {

      consoleLogger.debug("parameters: " + windowSize + ", " + paaSize + ", " + alphabetSize + ", "
          + this.numerosityReductionStrategy.toString());

      // cache for word bags
      HashMap<String, WordBag> seriesBags = new HashMap<String, WordBag>();

      // the class series bags
      HashMap<String, WordBag> bags = new HashMap<String, WordBag>();

      // push into stack all the samples we are going to validate for
      Stack<String> samples2go = new Stack<String>();
      for (Entry<String, double[]> e : this.tsData.entrySet()) {
        String seriesKey = e.getKey();
        samples2go.push(seriesKey);
      }
      Collections.shuffle(samples2go);
      consoleLogger.debug("series: " + seriesBags.keySet().toString());
      consoleLogger.debug("samples2go: " + samples2go.toString());

      // total counter
      int totalSamples = samples2go.size();

      // missclassified counter
      int missclassifiedSamples = 0;

      // while something is in the stack
      while (!samples2go.isEmpty()) {

        consoleLogger
            .debug("cross valiadtion iteration, in stack " + samples2go.size() + " series");

        // extracting validation samples batch and building to remove collection
        //
        HashMap<String, WordBag> wordsToRemove = new HashMap<String, WordBag>();
        List<String> currentValidationSample = new ArrayList<String>();
        for (int i = 0; i < this.holdOutSampleSize && !samples2go.isEmpty(); i++) {

          String seriesKey = samples2go.pop();
          String classLabel = seriesKey.substring(0, seriesKey.indexOf(DELIMITER));
          currentValidationSample.add(seriesKey);

          WordBag classBag = wordsToRemove.get(classLabel);
          if (null == classBag) {
            classBag = new WordBag(classLabel);
            wordsToRemove.put(classLabel, classBag);
          }
          classBag.mergeWith(seriesBags.get(seriesKey));

        }

        consoleLogger.debug("cross valiadtion sample: " + currentValidationSample.toString());

        // adjust word bags
        //
        HashMap<String, WordBag> basisBags = adjustWordBags(bags, wordsToRemove);

        // validation phase
        //
        // all stuff from the cache will build a classifier vectors
        //
        // compute TFIDF statistics for training set
        HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDFInstrumented(basisBags
            .values());

        // Classifying...
        // is this sample correctly classified?
        for (String e : currentValidationSample) {
          String trueClassLabel = e.substring(0, e.indexOf(DELIMITER));
          int res = classifyGrammar(trueClassLabel, seriesBags.get(e), tfidf, windowSize, paaSize,
              alphabetSize, this.numerosityReductionStrategy);
          if (0 == res) {
            missclassifiedSamples = missclassifiedSamples + 1;
          }
        }

      }

      double error = Integer.valueOf(missclassifiedSamples).doubleValue()
          / Integer.valueOf(totalSamples).doubleValue();

      consoleLogger.debug("## " + Arrays.toString(point.toArray()) + ", " + error);
      return error;

    }
    catch (Exception e) {
      System.err.println("Exception caught: " + StackTrace.toString(e));
      return Double.MAX_VALUE;
    }

  }

  private HashMap<String, WordBag> adjustWordBags(HashMap<String, WordBag> bags,
      HashMap<String, WordBag> wordsToRemove) {

    HashMap<String, WordBag> res = new HashMap<String, WordBag>();
    for (Entry<String, WordBag> e : bags.entrySet()) {
      res.put(e.getKey(), e.getValue().clone());
    }

    for (Entry<String, WordBag> e : wordsToRemove.entrySet()) {
      String classKey = e.getKey();
      for (Entry<String, AtomicInteger> eBag : e.getValue().getInternalWords().entrySet()) {
        res.get(classKey).addWord(eBag.getKey(), -eBag.getValue().intValue());
      }
    }

    return res;
  }

  private int classifyGrammar(String classKey, WordBag testBag,
      HashMap<String, HashMap<String, Double>> tfidf, int windowSize, int paaSize,
      int alphabetSize, NumerosityReductionStrategy strategy) throws Exception {

    // it is Cosine similarity,
    //
    // which ranges from 0.0 for the angle of 90 to 1.0 for the angle of 0
    // i.e. LARGES value is a SMALLEST distance
    double minDist = Double.MIN_VALUE;
    String className = "";
    double[] cosines = new double[tfidf.entrySet().size()];

    int index = 0;
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      double dist = tu.cosineSimilarity(testBag, e.getValue());
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

}
