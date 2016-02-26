task print1 {
  Int x = 10
  command {
    for i in `seq 1 ${x}`
      do
        echo $i
      done  
  }
}

task print2 {
  Int x = 20
  command {
    for i in `seq 1 ${x}` 
      do
        echo $i
      done  
  }
}

task print3 {
  Int x = 30
  command {
    for i in `seq 1 ${x}`
      do
        echo $i
      done
  }
}
