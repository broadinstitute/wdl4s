task x {
    File i
    command { ./script ${f} }
    output { File o = "somefile" }
}

workflow w {
    Array[File] f = [......] # 50 elements
    File f2
    scatter(y in f) {
        call x {input:i=y, i2=f2}
    }
    call x as a { input: i=f2 }
    call x as b { input: i=f2 }
}