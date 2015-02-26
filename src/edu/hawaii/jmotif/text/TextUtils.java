package edu.hawaii.jmotif.text;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import edu.hawaii.jmotif.sax.NumerosityReductionStrategy;
import edu.hawaii.jmotif.sax.SAXProcessor;
import edu.hawaii.jmotif.sax.TSProcessor;
import edu.hawaii.jmotif.sax.alphabet.Alphabet;
import edu.hawaii.jmotif.sax.alphabet.NormalAlphabet;

/**
 * Implements text statistics and mining utilities.
 * 
 * @author psenin
 * 
 */
public class TextUtils {

  @SuppressWarnings("unused")
  private static final String COMMA = ",";
  private static final String CR = "\n";
  private static final DecimalFormat df = new DecimalFormat("#0.00000");

  private static final Alphabet a = new NormalAlphabet();
  private TSProcessor tsp;
  private SAXProcessor sp;

  public TextUtils() {
    super();
    tsp = new TSProcessor();
    sp = new SAXProcessor();

  }

  /**
   * Computes TF*IDF values.
   * 
   * @param texts The collection of text documents for which the statistics need to be computed.
   * @return The map of source documents names to the word - tf*idf weight collections.
   */
  public HashMap<String, HashMap<String, Double>> computeTFIDF(Collection<WordBag> texts) {

    // the number of docs
    int totalDocs = texts.size();

    // the result. map of document names to the pairs word - tfidf weight
    HashMap<String, HashMap<String, Double>> res = new HashMap<String, HashMap<String, Double>>();

    // build a collection of all observed words and their frequency in corpus
    HashMap<String, AtomicInteger> allWords = new HashMap<String, AtomicInteger>();
    for (WordBag bag : texts) {

      // here populate result map with empty entries
      res.put(bag.getLabel(), new HashMap<String, Double>());

      // and get those words
      for (Entry<String, AtomicInteger> e : bag.getInternalWords().entrySet()) {

        if (allWords.containsKey(e.getKey())) {
          allWords.get(e.getKey()).incrementAndGet();
        }
        else {
          allWords.put(e.getKey(), new AtomicInteger(1));
        }
      }

    }

    // outer loop - iterating over documents
    for (WordBag bag : texts) {

      // fix the doc name
      String bagName = bag.getLabel();
      HashMap<String, AtomicInteger> bagWords = bag.getInternalWords(); // these are words of
                                                                        // documents

      // what we want to do for TF*IDF is to compute it for all WORDS ever seen in set
      //
      for (Entry<String, AtomicInteger> word : allWords.entrySet()) {

        // by default it is zero
        //
        double tfidf = 0;

        // if (totalDocs == word.getValue().intValue()) {
        // System.out.println("excluded: " + word.getKey());
        // }
        // if this document contains the word - here we go
        if (bagWords.containsKey(word.getKey()) & (totalDocs != word.getValue().intValue())) {

          int wordInBagFrequency = bagWords.get(word.getKey()).intValue();

          // compute TF: we take a log and correct for 0 by adding 1

          // OSULeaf: 0.09091
          // double tfValue = Math.log(1.0D + Integer.valueOf(wordInBagFrequency).doubleValue());

          // OSULeaf: 0.08678
          double tfValue = 1.0D + Math.log(Integer.valueOf(wordInBagFrequency).doubleValue());

          // OSULeaf: 0.1405
          // double tfValue = normalizedTF(bag, word.getKey());

          // OSULeaf: 0.08678
          // double tfValue = augmentedTF(bag, word.getKey());

          // OSULeaf: 0.08678
          // double tfValue = logAveTF(bag, word.getKey());

          // compute the IDF
          //
          double idfLOGValue = Math.log10(Integer.valueOf(totalDocs).doubleValue()
              / word.getValue().doubleValue());

          // and the TF-IDF
          //
          tfidf = tfValue * idfLOGValue;

        }

        res.get(bagName).put(word.getKey(), tfidf);

      }
    }
    return res;
  }

  /**
   * Compute TF (term frequency) metrics. This is logarithmically scaled TF.
   * 
   * @param bag The words bag.
   * @param term The term.
   * @return The term frequency value.
   */
  public double logTF(WordBag bag, String term) {
    if (bag.contains(term)) {
      return 1.0d + Math.log(bag.getWordFrequency(term).doubleValue());
    }
    return 0d;
  }

  /**
   * Compute TF (term frequency) metrics. This is normalized TF without bias towards longer
   * documents.
   * 
   * @param bag The words bag.
   * @param term The term.
   * @return The term frequency value.
   */
  public double normalizedTF(WordBag bag, String term) {
    if (bag.contains(term)) {
      return Integer.valueOf(bag.getWordFrequency(term)).doubleValue()
          / Integer.valueOf(bag.getMaxFrequency()).doubleValue();
    }
    return 0;
  }

  /**
   * Compute TF (term frequency) metrics. This is normalized TF without bias towards longer
   * documents.
   * 
   * @param bag The words bag.
   * @param term The term.
   * @return The term frequency value.
   */
  public double augmentedTF(WordBag bag, String term) {
    if (bag.contains(term)) {
      return 0.5D + (Integer.valueOf(bag.getWordFrequency(term)).doubleValue())
          / (2.0D * Integer.valueOf(bag.getMaxFrequency()).doubleValue());
    }
    return 0;
  }

  /**
   * Compute TF (term frequency) metrics. This is normalized TF without bias towards longer
   * documents.
   * 
   * @param bag The words bag.
   * @param term The term.
   * @return The term frequency value.
   */
  public double logAveTF(WordBag bag, String term) {
    if (bag.contains(term)) {
      return (1D + Math.log(Integer.valueOf(bag.getWordFrequency(term)).doubleValue()))
          / (1D + Math.log(bag.getAverageFrequency()));
    }
    return 0;
  }

  /**
   * Compute document frequency, DF, metrics.
   * 
   * @param bags The word bags collection.
   * @param string The string term.
   * @return The DF value.
   */
  public int df(HashMap<String, WordBag> bags, String string) {
    int res = 0;
    for (WordBag b : bags.values()) {
      if (b.contains(string)) {
        res += 1;
      }
    }
    return res;
  }

  /**
   * Compute idf (inverse document frequency) metrics.
   * 
   * @param bags The bags of words collection.
   * @param string The string (term).
   * @return The idf value.
   */
  public double idf(HashMap<String, WordBag> bags, String string) {
    return Integer.valueOf(bags.size()).doubleValue()
        / Integer.valueOf(df(bags, string)).doubleValue();
  }

  public String tfidfToTable(HashMap<String, HashMap<String, Double>> tfidf) {

    // melt together sets of keys
    //
    TreeSet<String> words = new TreeSet<String>();
    for (HashMap<String, Double> t : tfidf.values()) {
      words.addAll(t.keySet());
    }

    // print keys - the dictionaries names
    //
    StringBuilder sb = new StringBuilder("\"\",");
    for (String key : tfidf.keySet()) {
      sb.append("\"").append(key).append("\",");
    }
    sb.delete(sb.length() - 1, sb.length()).append(CR);

    // print rows, one by one
    //
    for (String w : words) {

      int zeroCounter = 0;
      StringBuffer rowSB = new StringBuffer();
      rowSB.append("\"").append(w).append("\",");

      for (String key : tfidf.keySet()) {
        HashMap<String, Double> data = tfidf.get(key);

        if (data.keySet().contains(w)) {
          rowSB.append(data.get(w)).append(",");
          if (data.get(w).equals(0D)) {
            zeroCounter++;
          }
        }
        else {
          rowSB.append(df.format(0.0d)).append(",");
          zeroCounter++;
        }

      }
      rowSB.delete(rowSB.length() - 1, rowSB.length()).append("\n");
      if (zeroCounter == tfidf.keySet().size()) {
        continue;
      }
      else {
        sb.append(rowSB.toString());
      }
    }
    return sb.toString();
  }

  /**
   * Normalize the vector to the norm of 1.
   * 
   * @param vector the vector.
   * @return normalized vector.
   */
  public HashMap<String, Double> normalizeToUnitVector(HashMap<String, Double> vector) {
    double sum = 0d;
    for (double value : vector.values()) {
      sum = sum + value * value;
    }
    sum = Math.sqrt(sum);
    HashMap<String, Double> res = new HashMap<String, Double>();
    for (Entry<String, Double> e : vector.entrySet()) {
      res.put(e.getKey(), e.getValue() / sum);
    }
    return res;
  }

  /**
   * Computes a cosine normalization of TFIDF statistics.
   * 
   * @param data The data.
   * @return The normalized tfidf statistics.
   */
  public HashMap<String, HashMap<String, Double>> normalizeToUnitVectors(
      HashMap<String, HashMap<String, Double>> data) {
    HashMap<String, HashMap<String, Double>> res = new HashMap<String, HashMap<String, Double>>();
    for (Entry<String, HashMap<String, Double>> e : data.entrySet()) {
      HashMap<String, Double> normalE = normalizeToUnitVector(e.getValue());
      res.put(e.getKey(), normalE);
    }
    return res;
  }

  /**
   * Computes a cosine similarity.
   * 
   * @param mapA The data vector 1.
   * @param mapB The data vector 2.
   * @return The cosine distance.
   */
  public double cosineSimilarity(HashMap<String, Double> mapA, HashMap<String, Double> mapB) {
    double res = 0;
    for (Entry<String, Double> entry : mapA.entrySet()) {
      if (mapB.containsKey(entry.getKey())) {
        res = res + entry.getValue().doubleValue() * mapB.get(entry.getKey()).doubleValue();
      }
    }
    double m1 = magnitude(mapA.values());
    double m2 = magnitude(mapB.values());
    return res / (m1 * m2);
  }

  public double cosineSimilarity(WordBag testSample, HashMap<String, Double> weightVector) {
    double res = 0;
    for (Entry<String, Integer> entry : testSample.getWords().entrySet()) {
      if (weightVector.containsKey(entry.getKey())) {
        res = res + entry.getValue().doubleValue() * weightVector.get(entry.getKey()).doubleValue();
      }
    }
    double m1 = magnitude(testSample.getWordsAsDoubles().values());
    double m2 = magnitude(weightVector.values());
    return res / (m1 * m2);
  }

  public CosineDistanceMatrix getCosineDistanceMatrix(HashMap<String, HashMap<String, Double>> tfidf) {
    CosineDistanceMatrix res = new CosineDistanceMatrix(tfidf);
    return res;
  }

  /**
   * Compute the magnitude of the vector.
   * 
   * @param vector The vector.
   * @return The magnitude.
   */
  public double magnitude(double[] vector) {
    return Math.sqrt(dotProduct(vector, vector));
  }

  /**
   * Compute the magnitude of the vector.
   * 
   * @param vector The vector.
   * @return The magnitude.
   */
  public double magnitude(Double[] vector) {
    return Math.sqrt(dotProduct(vector, vector));
  }

  private double magnitude(Collection<Double> values) {
    Double res = 0.0D;
    for (Double v : values) {
      res = res + v * v;
    }
    return Math.sqrt(res.doubleValue());
  }

  /**
   * Compute the dot product of two vectors.
   * 
   * @param vector1 The vector 1.
   * @param vector2 The vector 2.
   * @return The dot product.
   */
  public double dotProduct(double[] vector1, double[] vector2) {
    double res = 0.0D;
    for (int i = 0; i < vector1.length; i++) {
      res = res + vector1[i] * vector2[i];
    }
    return res;
  }

  /**
   * Compute the dot product of two vectors.
   * 
   * @param vector1 The vector 1.
   * @param vector2 The vector 2.
   * @return The dot product.
   */
  public double dotProduct(Double[] vector1, Double[] vector2) {
    double res = 0.0D;
    for (int i = 0; i < vector1.length; i++) {
      res = res + vector1[i] * vector2[i];
    }
    return res;
  }

  public WordBag seriesToWordBag(String label, double[] ts, int windowSize, int paaSize,
      double[] cuts, NumerosityReductionStrategy strategy, double nThreshold) throws Exception {

    WordBag resultBag = new WordBag(label);

    // scan across the time series extract sub sequences, and convert them to strings
    char[] previousString = null;

    for (int i = 0; i < ts.length - (windowSize - 1); i++) {

      // fix the current subsection
      double[] subSection = Arrays.copyOfRange(ts, i, i + windowSize);

      // Z normalize it
      subSection = tsp.znorm(subSection, nThreshold);

      // perform PAA conversion if needed
      double[] paa = tsp.paa(subSection, paaSize);

      // Convert the PAA to a string.
      char[] currentString = tsp.ts2String(paa, cuts);

      if (null != previousString) {

        if (NumerosityReductionStrategy.EXACT.equals(strategy)
            && Arrays.equals(previousString, currentString)) {
          // NumerosityReduction
          continue;
        }
        else if (NumerosityReductionStrategy.MINDIST.equals(strategy)
            && sp.checkMinDistIsZero(previousString, currentString)) {
          continue;
        }

      }

      previousString = currentString;

      resultBag.addWord(String.valueOf(currentString));
    }

    return resultBag;
  }

  public List<WordBag> labeledSeries2WordBags(Map<String, List<double[]>> data, int windowSize,
      int paaSize, double[] cuts, NumerosityReductionStrategy nrStrategy, double nrThreshold)
      throws Exception {

    // make a map of resulting bags
    Map<String, WordBag> preRes = new HashMap<String, WordBag>();

    // process series one by one building word bags
    for (Entry<String, List<double[]>> e : data.entrySet()) {

      String classLabel = e.getKey();
      WordBag bag = new WordBag(classLabel);

      for (double[] series : e.getValue()) {
        WordBag cb = seriesToWordBag("tmp", series, windowSize, paaSize, cuts, nrStrategy,
            nrThreshold);
        bag.mergeWith(cb);
      }

      preRes.put(classLabel, bag);
    }

    List<WordBag> res = new ArrayList<WordBag>();
    res.addAll(preRes.values());
    return res;

  }

  public int classify(String classKey, double[] series,
      HashMap<String, HashMap<String, Double>> tfidf, int windowSize, int paaSize, double[] cuts,
      NumerosityReductionStrategy strategy, double nt) throws Exception {

    WordBag test = seriesToWordBag("test", series, windowSize, paaSize, cuts, strategy, nt);

    // it is Cosine similarity,
    //
    // which ranges from 0.0 for the angle of 90 to 1.0 for the angle of 0
    // i.e. LARGER value is a SMALLEST distance
    double minDist = Double.MIN_VALUE;
    String className = "";
    double[] cosines = new double[tfidf.entrySet().size()];

    int index = 0;
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      double dist = this.cosineSimilarity(test, e.getValue());
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

  public String classify(WordBag test, HashMap<String, HashMap<String, Double>> tfidf) {

    // it is Cosine similarity,
    //
    // which ranges from 0.0 for the angle of 90 to 1.0 for the angle of 0
    // i.e. LARGES value is a SMALLEST distance
    double minDist = Double.MIN_VALUE;
    String className = "";
    double[] cosines = new double[tfidf.entrySet().size()];

    int index = 0;
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      double dist = this.cosineSimilarity(test, e.getValue());
      cosines[index] = dist;
      index++;

      if (dist > minDist) {
        className = e.getKey();
        minDist = dist;
      }

    }

    return className;
  }

  public String wordBagToTable(WordBag bag) {

    TreeSet<String> words = new TreeSet<String>();
    words.addAll(bag.getWordSet());

    // name
    //
    StringBuilder sb = new StringBuilder("\"" + bag.getLabel() + "\"").append(CR);

    // print rows, one by one
    //
    for (String w : words) {

      Integer count = bag.getWordFrequency(w);

      if (count == 0) {
        continue;
      }

      sb.append("\"").append(w).append("\",");
      sb.append(count).append(CR);
    }
    return sb.toString();
  }

  public String bagsToTable(List<WordBag> bags) {

    // melt together sets of keys
    //
    TreeSet<String> words = new TreeSet<String>();
    for (WordBag bag : bags) {
      words.addAll(bag.getWordSet());
    }

    // print keys - the dictionaries names
    //
    LinkedHashMap<String, Integer> bagKeys = new LinkedHashMap<String, Integer>();
    StringBuilder sb = new StringBuilder("\"\",");
    int index = 0;
    for (WordBag bag : bags) {
      bagKeys.put(bag.getLabel(), index);
      index++;
      sb.append("\"").append(bag.getLabel()).append("\",");
    }
    sb.delete(sb.length() - 1, sb.length()).append(CR);

    // print rows, one by one
    //
    for (String w : words) {

      int zeroCounter = 0;
      StringBuffer rowSB = new StringBuffer();
      rowSB.append("\"").append(w).append("\",");

      for (Entry<String, Integer> bagKey : bagKeys.entrySet()) {
        WordBag bag = bags.get(bagKey.getValue());
        HashMap<String, Integer> data = bag.getWords();

        if (data.keySet().contains(w)) {
          rowSB.append(data.get(w)).append(",");
          if (data.get(w).equals(0)) {
            zeroCounter++;
          }
        }
        else {
          rowSB.append(0).append(",");
          zeroCounter++;
        }

      }
      rowSB.delete(rowSB.length() - 1, rowSB.length()).append("\n");
      if (zeroCounter == bags.size()) {
        continue;
      }
      else {
        sb.append(rowSB.toString());
      }
    }
    return sb.toString();
  }
}
