task inItalian {
  String x = "ciao"
  command {
    echo "${x}"
  }
}

task inSpanish {
  String x = "ohla"
  command {
    echo "${x}"
  }
}

task inFrench {
  String x = "bonjour"
  command {
    echo "${x}" 
  }
}

workflow echoHello {}
