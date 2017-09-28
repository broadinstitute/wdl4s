cwlVersion: v1.0
class: Workflow
inputs: []
outputs:
- id: cgrep-count
  outputSource: "#cgrep/cgrep-count"
  type: int
- id: wc-count
  outputSource: "#wc/wc-count"
  type: int
steps:
- id: ps
  in: []
  out:
  - ps-stdOut
  run:
    inputs: []
    outputs:
    - id: ps-stdOut
      outputBinding:
        glob: stdout
      type: File
    class: CommandLineTool
    baseCommand: ps
    stdout: stdout
- id: cgrep
  in:
  - id: file
    source: "#ps/ps-stdOut"
  out:
  - id: cgrep-count
  run:
    inputs:
    - id: file
      type: File
    outputs:
    - id: cgrep-count
      outputBinding:
        glob: stdout
        loadContents: true
        outputEval: $(self[0].contents.toInt)
      type: int
    class: CommandLineTool
    requirements:
    - class: ShellCommandRequirement
    - class: InlineJavascriptRequirement
    arguments:
    - valueFrom: grep
      shellQuote: false
    - valueFrom: '*'
      shellQuote: false
    - valueFrom: $(inputs.file)
      shellQuote: false
    - valueFrom: '|'
      shellQuote: false
    - valueFrom: wc
      shellQuote: false
    - valueFrom: -l
      shellQuote: false
    stdout: stdout
- id: wc
  in:
  - id: file
    source: "#ps/ps-stdOut"
  out:
  - id: wc-count
  run:
    inputs:
    - id: file
      type: File
    outputs:
    - id: wc-count
      outputBinding:
        glob: stdout
        loadContents: true
        outputEval: $(self[0].contents.toInt)
      type: int
    class: CommandLineTool
    requirements:
    - class: ShellCommandRequirement
    - class: InlineJavascriptRequirement
    arguments:
    - valueFrom: cat
      shellQuote: false
    - valueFrom: $(inputs.file)
      shellQuote: false
    - valueFrom: '|'
      shellQuote: false
    - valueFrom: wc
      shellQuote: false
    - valueFrom: -l
      shellQuote: false
    stdout: stdout
