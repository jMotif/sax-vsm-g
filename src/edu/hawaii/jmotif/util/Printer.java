package edu.hawaii.jmotif.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class Printer {

  public static void main(String[] args) throws IOException {
    String fileName = "/home/psenin/Downloads/time.txt";
    BufferedReader br = new BufferedReader(new FileReader(new File(fileName)));
    String line = "";
    while (null != (line = br.readLine())) {
      if (line.indexOf("cmd = FastShapelet.exe") > 0) {
        String[] split = line.split("\\s+");
        System.out.print(split[4].substring(split[4].lastIndexOf("\\") + 1, split[4].length()));
        line = br.readLine();
        split = line.split("\\s+");
        System.out.print(" " + split[3]);
        line = br.readLine();
        line = br.readLine();
        split = line.split("\\s+");
        System.out.println(" " + split[2]);

      }
    }
  }

}
