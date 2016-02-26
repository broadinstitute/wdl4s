import "echoHello.wdl"

task ls {
  command {
    ls -l
  }
  output {
    String fileList = read_string(stdout())
  }
}

task pwd {
   command {
     pwd
   }
   output {
     String current = read_string(stdout())
   }
}

workflow basic {
  call ls
  call pwd
  output {
       ls.fileList
   }
}
