package edu.hawaii.jmotif.text;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import edu.hawaii.jmotif.sax.SAXFactory;
import edu.hawaii.jmotif.sax.alphabet.Alphabet;
import edu.hawaii.jmotif.sax.alphabet.NormalAlphabet;
import edu.hawaii.jmotif.timeseries.TSException;
import edu.hawaii.jmotif.timeseries.TSUtils;

/**
 * Implements text statistics and mining utilities.
 * 
 * @author psenin
 * 
 */
public final class TextUtils {

  @SuppressWarnings("unused")
  private static final String COMMA = ",";
  private static final String CR = "\n";
  private static final DecimalFormat df = new DecimalFormat("#0.00000");

  private static final Alphabet a = new NormalAlphabet();

  private TextUtils() {
    assert true;
  }

  /**
   * Computes TF*IDF values.
   * 
   * @param texts The collection of text documents for which the statistics need to be computed.
   * @return The map of source documents names to the word - tf*idf weight collections.
   */
  public static synchronized HashMap<String, HashMap<String, Double>> computeTFIDF(
      Collection<WordBag> texts) {

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

  public static synchronized HashMap<String, HashMap<Bigram, Double>> computeTFIDF(
      List<BigramBag> bags) {

    // the result. map of document names to the pairs word - tfidf weight
    HashMap<String, HashMap<Bigram, Double>> res = new HashMap<String, HashMap<Bigram, Double>>();

    // build a collection of all observed words
    TreeSet<Bigram> allWords = new TreeSet<Bigram>();
    for (BigramBag bag : bags) {
      allWords.addAll(bag.getBigramSet());
    }

    // outer loop - iterating over documents
    for (BigramBag bag : bags) {

      // fix the doc name
      String bagName = bag.getLabel();
      HashMap<Bigram, Integer> bagWords = bag.getBigrams(); // these are words of documents

      // what we want to do for TF*IDF is to compute it for all WORDS ever seen in set
      //
      for (Bigram word : allWords) {

        // get the TF first
        //
        int wordFrequency = 0;

        if (bagWords.containsKey(word)) {
          wordFrequency = bagWords.get(word);
        }

        // TF = we take a log and correct for 0 by adding 1
        double tfLOGValue = Math.log(Integer.valueOf(wordFrequency).doubleValue()
            / Integer.valueOf(bag.getTotalWordCount()).doubleValue() + 1.0D);
        // double tfLOGValue = Math.log(Integer.valueOf(wordFrequency).doubleValue()) + 1.0D;

        // compute the IDF
        //
        int totalDocs = bags.size();
        int docsWithWord = 0;
        for (BigramBag wb : bags) {
          if (wb.contains(word)) {
            docsWithWord = docsWithWord + 1;
          }
        }
        double idfLOGValue = Math.log(Integer.valueOf(totalDocs).doubleValue()
            / Integer.valueOf(docsWithWord).doubleValue());

        // and the TF-IDF
        //
        double tfIdf = tfLOGValue * idfLOGValue;

        if (null == res.get(bagName)) {
          res.put(bagName, new HashMap<Bigram, Double>());
        }
        res.get(bagName).put(word, tfIdf);

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
  public static synchronized double logTF(WordBag bag, String term) {
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
  public static synchronized double normalizedTF(WordBag bag, String term) {
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
  public static synchronized double augmentedTF(WordBag bag, String term) {
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
  public static synchronized double logAveTF(WordBag bag, String term) {
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
  public static synchronized int df(HashMap<String, WordBag> bags, String string) {
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
  public static synchronized double idf(HashMap<String, WordBag> bags, String string) {
    return Integer.valueOf(bags.size()).doubleValue()
        / Integer.valueOf(df(bags, string)).doubleValue();
  }

  public static synchronized String tfidfToTable(HashMap<String, HashMap<String, Double>> tfidf) {

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
  public static synchronized HashMap<String, Double> normalizeToUnitVector(
      HashMap<String, Double> vector) {
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
  public static synchronized HashMap<String, HashMap<String, Double>> normalizeToUnitVectors(
      HashMap<String, HashMap<String, Double>> data) {
    // result
    HashMap<String, HashMap<String, Double>> res = new HashMap<String, HashMap<String, Double>>();
    // cosine normalize these rows corresponding to docs TFIDF
    //
    for (Entry<String, HashMap<String, Double>> e : data.entrySet()) {

      // normalization coefficient calculation
      //
      double sum = 0D;
      for (double el : e.getValue().values()) {
        if (!(0. == el)) {
          sum = sum + el * el;
        }
      }
      double sqRoot = Math.sqrt(sum);

      // now all the elements must be divided by its value
      HashMap<String, Double> newEntry = new HashMap<String, Double>(e.getValue().size());
      for (Entry<String, Double> val : e.getValue().entrySet()) {
        if (val.getValue().equals(0D)) {
          newEntry.put(val.getKey(), 0D);
        }
        else {
          newEntry.put(val.getKey(), val.getValue() / sqRoot);
        }
      }

      // place it to map
      res.put(e.getKey(), newEntry);

    }
    return res;
  }

  public static HashMap<String, HashMap<Bigram, Double>> normalizeBigramsToUnitVectors(
      HashMap<String, HashMap<Bigram, Double>> data) {
    // result
    HashMap<String, HashMap<Bigram, Double>> res = new HashMap<String, HashMap<Bigram, Double>>();
    // cosine normalize these rows corresponding to docs TFIDF
    //
    for (Entry<String, HashMap<Bigram, Double>> e : data.entrySet()) {
      double sum = 0D;
      for (Double el : e.getValue().values()) {
        sum = sum + el * el;
      }
      double sqRoot = Math.sqrt(sum);
      HashMap<Bigram, Double> newEntry = new HashMap<Bigram, Double>();
      for (Entry<Bigram, Double> val : e.getValue().entrySet()) {
        double newValue = val.getValue() / sqRoot;
        newEntry.put(val.getKey(), newValue);
      }
      res.put(e.getKey(), newEntry);
    }
    return res;
  }

  /**
   * Computes a cosine similarity.
   * 
   * @param data1 The data vector 1.
   * @param data2 The data vector 2.
   * @return The cosine distance.
   */
  public static synchronized double cosineDistance(HashMap<String, Double> data1,
      HashMap<String, Double> data2) {
    // sanity word order check
    if (!(data2.keySet().containsAll(data1.keySet()))
        || !(data2.keySet().size() == data1.keySet().size())) {
      throw new RuntimeException("COSINE SIMILARITY ERROR: word sets are different in length!");
    }

    double[] vector1 = new double[data1.size()];
    double[] vector2 = new double[data2.size()];

    int i = 0;
    for (String s : data1.keySet()) {
      vector1[i] = data1.get(s);
      vector2[i] = data2.get(s);
      i++;
    }

    double numerator = dotProduct(vector1, vector2);
    double denominator = magnitude(vector1) * magnitude(vector2);

    return numerator / denominator;
  }

  public static synchronized double cosineSimilarity(WordBag testSample,
      HashMap<String, Double> weightVector) {
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

  public static synchronized double cosineSimilarityInstrumented(WordBag testSample,
      HashMap<String, Double> weightVector, HashMap<String, Double> insight) {
    double res = 0;
    for (Entry<String, Integer> entry : testSample.getWords().entrySet()) {
      if (weightVector.containsKey(entry.getKey())) {
        res = res + entry.getValue().doubleValue() * weightVector.get(entry.getKey()).doubleValue();
        insight.put(entry.getKey(),
            entry.getValue().doubleValue() * weightVector.get(entry.getKey()).doubleValue());
      }
    }
    double m1 = magnitude(testSample.getWordsAsDoubles().values());
    double m2 = magnitude(weightVector.values());
    return res / (m1 * m2);
  }

  private static double cosineSimilarity(BigramBag testSample, HashMap<Bigram, Double> weightVector) {
    double res = 0;
    for (Entry<Bigram, Integer> entry : testSample.getBigrams().entrySet()) {
      if (weightVector.containsKey(entry.getKey())) {
        res = res + entry.getValue().doubleValue() * weightVector.get(entry.getKey()).doubleValue();
      }
    }
    double m1 = magnitude(testSample.getBigramsAsDoubles().values());
    double m2 = magnitude(weightVector.values());
    return res / (m1 * m2);
  }

  public static synchronized CosineDistanceMatrix getCosineDistanceMatrix(
      HashMap<String, HashMap<String, Double>> tfidf) {
    CosineDistanceMatrix res = new CosineDistanceMatrix(tfidf);
    return res;
  }

  /**
   * Compute the magnitude of the vector.
   * 
   * @param vector The vector.
   * @return The magnitude.
   */
  public static synchronized double magnitude(double[] vector) {
    return Math.sqrt(dotProduct(vector, vector));
  }

  /**
   * Compute the magnitude of the vector.
   * 
   * @param vector The vector.
   * @return The magnitude.
   */
  public static synchronized double magnitude(Double[] vector) {
    return Math.sqrt(dotProduct(vector, vector));
  }

  private static synchronized double magnitude(Collection<Double> values) {
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
  public static synchronized double dotProduct(double[] vector1, double[] vector2) {
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
  public static synchronized double dotProduct(Double[] vector1, Double[] vector2) {
    double res = 0.0D;
    for (int i = 0; i < vector1.length; i++) {
      res = res + vector1[i] * vector2[i];
    }
    return res;
  }

  public static synchronized WordBag seriesToWordBag(String label, double[] series, int[] params,
      double normalizationThresholdValue) throws IndexOutOfBoundsException, TSException {

    WordBag resultBag = new WordBag(label);

    int windowSize = params[0];
    int paaSize = params[1];
    int alphabetSize = params[2];
    SAXNumerosityReductionStrategy strategy = SAXNumerosityReductionStrategy.fromValue(params[3]);

    // System.out.println("Strategy: " + strategy.index());

    String oldStr = "";
    for (int i = 0; i <= series.length - windowSize; i++) {

      double[] paa = TSUtils.optimizedPaa(TSUtils.optimizedZNorm(
          TSUtils.subseries(series, i, windowSize), normalizationThresholdValue), paaSize);

      char[] sax = TSUtils.ts2String(paa, a.getCuts(alphabetSize));

      if (SAXNumerosityReductionStrategy.CLASSIC.equals(strategy)) {
        if (oldStr.length() > 0 && SAXFactory.strDistance(sax, oldStr.toCharArray()) == 0) {
          continue;
        }
      }
      else if (SAXNumerosityReductionStrategy.EXACT.equals(strategy)) {
        if (oldStr.equalsIgnoreCase(String.valueOf(sax))) {
          continue;
        }
      }

      oldStr = String.valueOf(sax);

      resultBag.addWord(String.valueOf(sax));
    }

    return resultBag;
  }

  protected static synchronized BigramBag seriesToBigramBag(String label, double[] series,
      int[][] params) throws TSException {

    BigramBag resultBag = new BigramBag(label);

    for (int[] p : params) {

      ArrayList<String> text = new ArrayList<String>();

      int windowSize = p[0];
      int paaSize = p[1];
      int alphabetSize = p[2];
      SAXNumerosityReductionStrategy strategy = SAXNumerosityReductionStrategy.fromValue(p[3]);

      String oldStr = "";
      for (int i = 0; i <= series.length - windowSize; i++) {

        double[] paa = TSUtils.optimizedPaa(
            TSUtils.zNormalize(TSUtils.subseries(series, i, windowSize)), paaSize);

        char[] sax = TSUtils.ts2String(paa, a.getCuts(alphabetSize));

        // System.out.println(Arrays.toString(TSUtils.subseries(series, i, windowSize)) + "->"
        // + Arrays.toString(paa));

        if (SAXNumerosityReductionStrategy.CLASSIC.equals(strategy)) {
          if (oldStr.length() > 0 && SAXFactory.strDistance(sax, oldStr.toCharArray()) == 0) {
            continue;
          }
        }
        else if (SAXNumerosityReductionStrategy.EXACT.equals(strategy)) {
          if (oldStr.equalsIgnoreCase(String.valueOf(sax))) {
            continue;
          }
        }

        oldStr = String.valueOf(sax);
        text.add(String.valueOf(sax));
      }

      // need to text into bigrams
      //
      Bigram cBigram = new Bigram();
      for (String str : text) {
        cBigram.setNext(str);
        if (cBigram.isComplete()) {
          resultBag.add(cBigram);
          cBigram = new Bigram();
          cBigram.setNext(str);
        }
      }

    }

    return resultBag;
  }

  public static synchronized List<WordBag> labeledSeries2WordBags(Map<String, List<double[]>> data,
      int paaSize, int alphabetSize, int windowSize, SAXNumerosityReductionStrategy strategy)
      throws IndexOutOfBoundsException, TSException {
    int[] params = new int[4];
    params[0] = windowSize;
    params[1] = paaSize;
    params[2] = alphabetSize;
    params[3] = strategy.index();
    return labeledSeries2WordBags(data, params, TSUtils.GLOBAL_NORMALIZATION_THRESHOLD);
  }

  /**
   * Converts timeseries datastructure into the bag of words. It is assumed that every key in the
   * parameters map is the timeseries class label. The corresponding list of double arrays, is a set
   * of representatives of this class.
   * 
   * @param data The map of class labels and representatives.
   * @param params The set of SAX parameters to use, index 0 - sliding window size, index 1 - PAA
   * size, index 2 - alphabet size.
   * @param normalizationThresholdValue
   * @return The words bag.
   * @throws IndexOutOfBoundsException If error occurs.
   * @throws TSException If error occurs.
   */
  public static synchronized List<WordBag> labeledSeries2WordBags(Map<String, List<double[]>> data,
      int[] params, double normalizationThresholdValue) throws IndexOutOfBoundsException,
      TSException {

    // make a map of resulting bags
    Map<String, WordBag> preRes = new HashMap<String, WordBag>();

    // process series one by one building word bags
    for (Entry<String, List<double[]>> e : data.entrySet()) {

      String classLabel = e.getKey();
      WordBag bag = new WordBag(classLabel);

      for (double[] series : e.getValue()) {
        WordBag cb = seriesToWordBag("tmp", series, params, normalizationThresholdValue);
        bag.mergeWith(cb);
      }

      preRes.put(classLabel, bag);
    }

    List<WordBag> res = new ArrayList<WordBag>();
    res.addAll(preRes.values());
    return res;
  }

  public static synchronized List<WordBag> labeledMultivariateSeries2WordBags(
      Map<String, List<double[][]>> data, int[] params) throws IndexOutOfBoundsException,
      TSException {

    // make a summary map
    Map<String, WordBag> preRes = new HashMap<String, WordBag>();
    for (String tag : data.keySet()) {
      preRes.put(tag, new WordBag(tag));
    }

    // process series one by one building word bags
    for (Entry<String, List<double[][]>> e : data.entrySet()) {

      String seriesLabel = e.getKey();
      WordBag bag = preRes.get(seriesLabel);

      for (double[][] series : e.getValue()) {

        for (double[] currSeries : series) {

          WordBag cb = seriesToWordBag("tmp", currSeries, params, TSUtils.GLOBAL_NORMALIZATION_THRESHOLD);
          bag.mergeWith(cb);

        }

      }
    }
    List<WordBag> res = new ArrayList<WordBag>();
    res.addAll(preRes.values());
    return res;
  }

  public static synchronized List<BigramBag> labeledSeries2BigramBags(
      Map<String, List<double[]>> data, int[][] params) throws IndexOutOfBoundsException,
      TSException {
    // make a map of resulting bags
    Map<String, BigramBag> preRes = new HashMap<String, BigramBag>();
    for (String tag : data.keySet()) {
      preRes.put(tag, new BigramBag(tag));
    }

    // process series one by one building word bags
    for (Entry<String, List<double[]>> e : data.entrySet()) {

      String seriesLabel = e.getKey();
      BigramBag bag = preRes.get(seriesLabel);

      for (double[] series : e.getValue()) {

        BigramBag cb = seriesToBigramBag("tmp", series, params);
        bag.mergeWith(cb);

      }
    }

    List<BigramBag> res = new ArrayList<BigramBag>();
    res.addAll(preRes.values());
    return res;
  }

  public static synchronized int classify(String classKey, double[] series,
      HashMap<String, HashMap<String, Double>> tfidf, int paaSize, int alphabetSize,
      int windowSize, SAXNumerosityReductionStrategy strategy, double nt) throws IndexOutOfBoundsException,
      TSException {
    int[] params = new int[4];
    params[0] = windowSize;
    params[1] = paaSize;
    params[2] = alphabetSize;
    params[3] = strategy.index();
    return classify(classKey, series, tfidf, params, nt);
  }

  /**
   * Performs classification.
   * 
   * @param classKey The target class key, if series will appear of this class, returns 1.
   * @param series The series to test.
   * @param tfidf The TF*IDF weights data structure.
   * @param params SAX parameters to use.
   * @param nt 
   * @return 1 if the series vector aligns with a class vector, or 0 otherwise.
   * 
   * @throws IndexOutOfBoundsException
   * @throws TSException
   */
  public static synchronized int classify(String classKey, double[] series,
      HashMap<String, HashMap<String, Double>> tfidf, int[] params, Double nt)
      throws IndexOutOfBoundsException, TSException {

    WordBag test = seriesToWordBag("test", series, params, nt);

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

  public static synchronized String classify(WordBag test,
      HashMap<String, HashMap<String, Double>> tfidf) {

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

    return className;
  }

  public static synchronized int classifyBigrams(String classKey, double[] series,
      HashMap<String, HashMap<Bigram, Double>> tfidf, int[][] params) throws TSException {

    BigramBag test = seriesToBigramBag("test", series, params);

    double minDist = -1.0d;
    String className = "";
    double[] cosines = new double[tfidf.entrySet().size()];
    int index = 0;
    for (Entry<String, HashMap<Bigram, Double>> e : tfidf.entrySet()) {
      double dist = TextUtils.cosineSimilarity(test, e.getValue());
      cosines[index] = dist;
      index++;
      if (dist > minDist) {
        className = e.getKey();
        minDist = dist;
      }
    }

    boolean allEqual = true;
    double cosine = cosines[0];
    for (int i = 1; i < cosines.length; i++) {
      if (!(cosines[i] == cosine)) {
        allEqual = false;
      }
    }

    if (!(allEqual) && className.equalsIgnoreCase(classKey)) {
      return 1;
    }
    return 0;
  }

  public static synchronized int classify(String classKey, double[][] data,
      HashMap<String, HashMap<String, Double>> tfidf, int[][] params)
      throws IndexOutOfBoundsException, TSException {

    WordBag test = new WordBag("test");

    for (int[] p : params) {
      int windowSize = p[0];
      int paaSize = p[1];
      int alphabetSize = p[2];
      SAXNumerosityReductionStrategy strategy = SAXNumerosityReductionStrategy.fromValue(p[3]);
      String oldStr = "";

      for (double[] series : data) {

        for (int j = 0; j <= series.length - windowSize; j++) {
          double[] paa = TSUtils.optimizedPaa(
              TSUtils.zNormalize(TSUtils.subseries(series, j, windowSize)), paaSize);
          char[] sax = TSUtils.ts2String(paa, a.getCuts(alphabetSize));
          if (SAXNumerosityReductionStrategy.CLASSIC.equals(strategy)) {
            if (oldStr.length() > 0 && SAXFactory.strDistance(sax, oldStr.toCharArray()) == 0) {
              continue;
            }
          }
          else if (SAXNumerosityReductionStrategy.EXACT.equals(strategy)) {
            if (oldStr.equalsIgnoreCase(String.valueOf(sax))) {
              continue;
            }
          }
          oldStr = String.valueOf(sax);
          test.addWord(String.valueOf(sax));
        }

      }
    }

    double minDist = -1.0d;
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

    boolean allEqual = true;
    double cosine = cosines[0];
    for (int i = 1; i < cosines.length; i++) {
      if (!(cosines[i] == cosine)) {
        allEqual = false;
      }
    }

    if (!(allEqual) && className.equalsIgnoreCase(classKey)) {
      return 1;
    }
    return 0;
  }

  public static synchronized String wordBagToTable(WordBag bag) {

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

  public static synchronized String bagsToTable(List<WordBag> bags) {

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
